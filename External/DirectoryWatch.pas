unit DirectoryWatch;

(*============================================================================================================
 This software is distributed under BSD license.
 Copyright (c) 2009 Iztok Kacin, Cromis (iztok.kacin@gmail.com). All rights reserved.
 Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. Neither the name of the Iztok Kacin nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 NOTICE OF CODE ORIGIN
 This code was derived from the original code of author "Gleb Yourchenko". The original code "FnugryDirWatch" can still be found at Torry Components. The URL is: http://www.torry.net/pages.php?id=252. The code was taken as a starting point and then mainly written from scratch. keeping some of the healthy code parts. So I am not in any way an author of. the original idea. But I am the author of all the changes and new code parts.

 Source:
    http://www.cromis.net/blog/downloads/directory-watch
============================================================================================================

 2026.02.21 (Gabriel Moraru)
   FIX: compatibility with Win64 compiler
   FIX: Added terminate in TDirWatchThread.Execute - Without this I enter an infinite loop when I delete all files from folder then the folder itself  !!!!!!!!!!!!
   FIX: 64-bit pointer truncation in Execute (Cardinal/DWORD casts -> NativeUInt)
   FIX: Memory leak when PostMessage fails (FileName/ErrorMsg buffers)
   FIX: Range check on Action parameter in Notify to prevent invalid enum cast
   FIX: Missing directory existence check in Start
   FIX: Removed redundant FILE_NOTIFY_CHANGE_* constants (already in Winapi.Windows)
   FIX: Updated comment accuracy

 Tester:
     c:\Projects\Project Testers\DirectoryWatch tester\
============================================================================================================*)

INTERFACE

USES
   Windows, SysUtils, Classes, Messages, DateUtils;

CONST
  cShutdownTimeout = 3000;
  cFileWaitTimeout = 0;       // 0 = wait indefinitely until file is ready (no timeout)

TYPE
  TWatchOption  = (woFileName, woDirName, woAttributes, woSize, woLastWrite, woLastAccess, woCreation, woSecurity);   // the filters that control when the watch is triggered
  TWatchOptions = set of TWatchOption;

  TWatchAction = (waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew);    // the actions that are the result of the watch being triggered

  TWatchActions = set of TWatchAction;

  TFileChangeNotifyEvent = procedure(const Sender: TObject;
                                     const Action: TWatchAction;
                                     const FileName: string) of object;

  TOnError = procedure(const Sender: TObject; const ErrorCode: Integer; const ErrorMessage: string ) of Object;

  TDirectoryWatch = class
  private
    FWatchOptions : TWatchOptions;
    FWatchActions : TWatchActions;
    FWatchSubTree : Boolean;
    FWatchThread  : TThread;
    FBufferSize   : Integer;
    FWndHandle    : HWND;
    FDirectory    : string;
    FAbortEvent   : THandle;
    FOnError      : TOnError;
    FOnChange     : TNotifyEvent;
    FOnNotify     : TFileChangeNotifyEvent;
    procedure WatchWndProc(var Msg: TMessage);
    procedure SetDirectory(const Value: string);
    procedure SetWatchOptions(const Value: TWatchOptions);
    procedure SetWatchActions(const Value: TWatchActions);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure DeallocateHWnd(Wnd: HWND);
    function  MakeFilter: Integer;
  protected
    procedure Change; virtual;
    procedure AllocWatchThread;
    procedure ReleaseWatchThread;
    procedure RestartWatchThread;
    procedure Notify(const Action: Integer; const FileName: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Running: Boolean;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property WatchOptions: TWatchOptions read FWatchOptions write SetWatchOptions;
    property WatchActions: TWatchActions read FWatchActions write SetWatchActions;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property Directory: string read FDirectory write SetDirectory;              { old name was Path }
    // notification properties. Notify about internal and exernal changes
    property OnNotify: TFileChangeNotifyEvent read FOnNotify write FOnNotify;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnError: TOnError read FOnError write FOnError;
  end;

  // Waits for the file to be ready (not in use anymore) or until timeout expires.
  // When Timeout = 0 (default), waits indefinitely.
  procedure WaitForFileReady(const FileName: string; const Timeout: Cardinal = cFileWaitTimeout);


IMPLEMENTATION

TYPE
  PFILE_NOTIFY_INFORMATION = ^TFILE_NOTIFY_INFORMATION;
  TFILE_NOTIFY_INFORMATION = record
    NextEntryOffset : Cardinal;
    Action          : Cardinal;
    FileNameLength  : Cardinal;
    FileName        : array[0..MAX_PATH - 1] of WideChar;
  end;

CONST
  WM_DIRWATCH_ERROR    = WM_USER + 137;
  WM_DIRWATCH_NOTIFY   = WM_USER + 138;
  FILE_LIST_DIRECTORY  = $0001;
  // error messages
  cErrorInWatchThread = 'Error "%s" in watch thread. Error code: %d';
  cErrorCreateWatchError = 'Error trying to create file handle for "%s". Error code: %d';

TYPE
  TDirWatchThread = class(TThread)
  private
    FWatchSubTree : Boolean;
    FAbortEvent   : THandle;
    FChangeEvent  : THandle;
    FBufferSize   : Integer;
    FWndHandle    : HWND;
    FDirHandle    : THandle;
    FDirectory    : string;
    FIOResult     : Pointer;
    FFilter       : Integer;
    procedure SignalError(const ErrorMessage: string; ErrorCode: Cardinal = 0);
  protected
    procedure Execute; override;
  public
    constructor Create(const Directory: string; const WndHandle: HWND; const BufferSize: Integer; const AbortEvent: THandle; const TypeFilter: Cardinal; const aWatchSubTree: Boolean);
    destructor Destroy; override;
  end;


procedure WaitForFileReady(const FileName: string; const Timeout: Cardinal);
VAR
  hFile: THandle;
  StartTime: TDateTime;
begin
 StartTime := Now;

 // Wait until file can be opened exclusively, or until timeout expires.
 // When Timeout = 0, loops indefinitely until the file becomes available.
 while (Timeout = 0) or (MilliSecondsBetween(Now, StartTime) < Timeout) do
 begin
   hFile := CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

   if hFile <> INVALID_HANDLE_VALUE then
    begin
     CloseHandle(hFile);
     Break;
    end;

   Sleep(50);
 end;
end;


procedure TDirWatchThread.Execute;
var
 NotifyData: PFILE_NOTIFY_INFORMATION;
 Events: array [0..1] of THandle;
 ErrorMessage: string;
 WaitResult: DWORD;
 NextEntry: Cardinal;
 FileName: PWideChar;
 Overlap: TOverlapped;
 ResSize: Cardinal;
begin
 FillChar(Overlap, SizeOf(TOverlapped), 0);
 Overlap.hEvent := FChangeEvent;

 // set the array of events
 Events[0] := FChangeEvent;
 Events[1] := FAbortEvent;

 while not Terminated DO
 try
   if ReadDirectoryChangesW(FDirHandle, FIOResult, FBufferSize, FWatchSubtree, FFilter, @ResSize, @Overlap, nil) then
   begin
     WaitResult := WaitForMultipleObjects(Length(Events), @Events, FALSE, INFINITE);

     // check if we have terminated the thread
     if WaitResult <> WAIT_OBJECT_0 then
      begin
       Terminate;
       Exit;
      end;

     if WaitResult = WAIT_OBJECT_0
     then
      begin
      if GetOverlappedResult(FDirHandle, Overlap, ResSize, False) then
       begin
         NotifyData := FIOResult;

         // check overflow
         if ResSize = 0 then
          begin
           ErrorMessage := SysErrorMessage(ERROR_NOTIFY_ENUM_DIR);
           SignalError(ErrorMessage, ERROR_NOTIFY_ENUM_DIR);
          end;

         REPEAT
           NextEntry := NotifyData^.NextEntryOffset;

           // Allocate buffer for filename and null-terminate it
           GetMem(FileName, NotifyData^.FileNameLength + SizeOf(WideChar));
           Move(NotifyData^.FileName, Pointer(FileName)^, NotifyData^.FileNameLength);
           PWord(NativeUInt(FileName) + NotifyData^.FileNameLength)^ := 0;   // FIX: was Cardinal - truncates pointers on Win64

           // Send the message about the filename. Receiver (WatchWndProc) frees FileName.
           if NOT PostMessage(FWndHandle, WM_DIRWATCH_NOTIFY, NotifyData^.Action, LParam(FileName))
           then FreeMem(FileName);  // FIX: Free on PostMessage failure to prevent leak

           // Advance to next entry in the notification buffer
           PByte(NotifyData) := PByte(NativeUInt(NotifyData) + NextEntry);   // FIX: was DWORD - truncates pointers on Win64
         UNTIL (NextEntry = 0);
       end
      else
       begin
         ErrorMessage := SysErrorMessage(GetLastError);
         SignalError(ErrorMessage);
         Terminate;     //MY FIX: Without this I enter an infinite loop when I delete all files from folder then the folder itself. The program will go into an infinite loop via  WatchWndProc.WM_DIRWATCH_ERROR !!!
         //Steps to reproduce this bug: create folder, monitor folder, put files in folder, delete ALL files from folder, delete folder, app freezes
       end;
     end;
    end
   else
    begin
     ErrorMessage := SysErrorMessage(GetLastError);
     SignalError(ErrorMessage);
    end;

 except
   on E :Exception do
   begin
     ErrorMessage := E.Message;
     SignalError(ErrorMessage);
   end;
 end;
end;


procedure TDirWatchThread.SignalError(const ErrorMessage: string; ErrorCode: Cardinal);
var
 ErrorMsg: PChar;
 MessageSize: Integer;
begin
 if ErrorCode = 0
 then ErrorCode := GetLastError;

 // Allocate buffer for error message string + null terminator
 MessageSize := (Length(ErrorMessage) + 1) * SizeOf(Char);

 GetMem(ErrorMsg, MessageSize);
 StrPCopy(ErrorMsg, ErrorMessage);

 // Receiver (WatchWndProc) frees ErrorMsg
 if NOT PostMessage(FWndHandle, WM_DIRWATCH_ERROR, ErrorCode, LPARAM(ErrorMsg))
 then FreeMem(ErrorMsg);  // FIX: Free on PostMessage failure to prevent leak
end;


constructor TDirWatchThread.Create(const Directory: string; const WndHandle: HWND;
                                  const BufferSize: Integer;
                                  const AbortEvent: THandle;
                                  const TypeFilter: Cardinal;
                                  const aWatchSubTree: Boolean);
begin
  //
  // Retrieve proc pointer, open directory to watch and allocate buffer for notification data. (note, it is done before calling inherited
  // create (that calls BeginThread) so any exception will be still raised in caller's thread)
  FDirHandle := CreateFile(PChar(Directory),
                           FILE_LIST_DIRECTORY,
                           FILE_SHARE_READ OR
                           FILE_SHARE_DELETE OR
                           FILE_SHARE_WRITE,
                           nil, OPEN_EXISTING,
                           FILE_FLAG_BACKUP_SEMANTICS OR
                           FILE_FLAG_OVERLAPPED,
                           0);

  if FDirHandle = INVALID_HANDLE_VALUE
  then raise Exception.CreateFmt(cErrorCreateWatchError, [Directory, GetLastError]);

  FChangeEvent := CreateEvent(nil, FALSE, FALSE, nil);
  FAbortEvent := AbortEvent;

  // allocate the buffer memory
  FBufferSize := BufferSize * SizeOf(TFILE_NOTIFY_INFORMATION);
  GetMem(FIOResult, FBufferSize);

  FWatchSubTree := aWatchSubtree;
  FWndHandle := WndHandle;
  FDirectory := Directory;
  FFilter := TypeFilter;

  inherited Create(False);
end;


destructor TDirWatchThread.Destroy;
begin
  CloseHandle(FChangeEvent);

  if FDirHandle <> INVALID_HANDLE_VALUE  then
    CloseHandle(FDirHandle);
  if Assigned(FIOResult) then
    FreeMem(FIOResult);

  inherited Destroy;
end;


{ TDirectoryWatch }

destructor TDirectoryWatch.Destroy;
begin
 Stop;
 DeallocateHWnd(FWndHandle);

 inherited Destroy;
end;



constructor TDirectoryWatch.Create;
begin
  FWndHandle := AllocateHWnd(WatchWndProc);
  FWatchSubtree := True;
  FBufferSize := 32;

  // construct the default watch actions and options
  FWatchActions := [waAdded, waRemoved, waModified, waRenamedOld, waRenamedNew];
  FWatchOptions := [woFileName, woDirName, woAttributes, woSize, woLastWrite, woLastAccess, woCreation, woSecurity];
end;


procedure TDirectoryWatch.AllocWatchThread;
begin
 if FWatchThread = nil then
 begin
   FAbortEvent := CreateEvent(nil, FALSE, FALSE, nil);
   FWatchThread := TDirWatchThread.Create(Directory, FWndHandle, FBufferSize, FAbortEvent, MakeFilter, WatchSubtree);
 end;
end;



procedure TDirectoryWatch.ReleaseWatchThread;
var
 AResult: Cardinal;
 ThreadHandle: THandle;
begin
 if FWatchThread <> nil then
 begin
   ThreadHandle := FWatchThread.Handle;
   // Signal the abort event so the watch thread exits its wait loop
   SetEvent(FAbortEvent);

   // wait and block until thread is finished
   AResult := WaitForSingleObject(ThreadHandle, cShutdownTimeout);

   // check if we timed out
   if AResult = WAIT_TIMEOUT then
     TerminateThread(ThreadHandle, 0);

   FreeAndNil(FWatchThread);
   CloseHandle(FAbortEvent);
 end;
end;



procedure TDirectoryWatch.RestartWatchThread;
begin
 Stop;
 Start;
end;



function TDirectoryWatch.Running: Boolean;
begin
 Result := FWatchThread <> nil;
end;



procedure TDirectoryWatch.DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
 Instance := Pointer(GetWindowLongPtr(Wnd, GWL_WNDPROC));     { GetWindowLong was replaced with GetWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }

 if Instance <> @DefWindowProc then
 begin
   { make sure we restore the default windows procedure before freeing memory }
   SetWindowLongPtr(Wnd, GWL_WNDPROC, LONG_PTR(@DefWindowProc));   { SetWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility.  Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
   FreeObjectInstance(Instance);
 end;

 DestroyWindow(Wnd);
end;



procedure TDirectoryWatch.SetWatchActions(const Value: TWatchActions);
begin
 if FWatchActions <> Value then
 begin
   FWatchActions := Value;

   if Running then
     RestartWatchThread;

   Change;
 end;
end;



procedure TDirectoryWatch.SetWatchOptions(const Value: TWatchOptions);
begin
 if FWatchOptions <> Value then
 begin
   FWatchOptions := Value;

   if Running then
     RestartWatchThread;

   Change;
 end;
end;



procedure TDirectoryWatch.WatchWndProc(var Msg: TMessage);
var
 ErrorCode: Cardinal;
 ErrorMessage: string;
begin
  case Msg.msg of
    // Retrieve notify data and forward the event to TDirectoryWatch's notify handler. Free filename string (allocated in WatchThread).
    WM_DIRWATCH_NOTIFY:
     TRY
       Notify(Msg.wParam, WideCharToString(PWideChar(Msg.lParam)));
     FINALLY
       if Msg.lParam <> 0
       then FreeMem(Pointer(Msg.lParam));
     END;

    // Report error via OnError event
    WM_DIRWATCH_ERROR:
     TRY
       ErrorMessage := StrPas(PChar(Msg.lParam));
       ErrorCode    := Msg.WParam;

       if Assigned(FOnError)
       then FOnError(Self, ErrorCode, ErrorMessage);
     FINALLY
       if Msg.lParam <> 0 then FreeMem(Pointer(Msg.lParam));
     END;

  // pass all other messages down the line
  else
     begin
      Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.wParam, Msg.lParam);
      EXIT;
     end;
  end;
end;



function TDirectoryWatch.MakeFilter: Integer;
CONST
   FilterFlags: array [TWatchOption] of Integer = (
     FILE_NOTIFY_CHANGE_FILE_NAME,
     FILE_NOTIFY_CHANGE_DIR_NAME,
     FILE_NOTIFY_CHANGE_ATTRIBUTES,
     FILE_NOTIFY_CHANGE_SIZE,
     FILE_NOTIFY_CHANGE_LAST_WRITE,
     FILE_NOTIFY_CHANGE_LAST_ACCESS,
     FILE_NOTIFY_CHANGE_CREATION,
     FILE_NOTIFY_CHANGE_SECURITY);
VAR
   Flag: TWatchOption;
begin
 Result := 0;

 for Flag in FWatchOptions do
   Result := Result or FilterFlags[Flag];
end;



procedure TDirectoryWatch.SetWatchSubTree(const Value :Boolean);
begin
 if Value <> FWatchSubtree then
 begin
   FWatchSubtree := Value;

   if Running then
     RestartWatchThread;

   Change;
 end;
end;



procedure TDirectoryWatch.Start;
begin
 if FDirectory = ''
 then raise Exception.Create('Please specify a directory to watch');

 if NOT DirectoryExists(FDirectory)
 then raise Exception.CreateFmt('Directory does not exist: "%s"', [FDirectory]);

 if NOT Running then
  begin
   AllocWatchThread;
   Change;
  end;
end;



procedure TDirectoryWatch.Stop;
begin
 if Running then
 begin
   ReleaseWatchThread;
   Change;
 end;
end;



procedure TDirectoryWatch.SetDirectory(const Value: string);
begin
 if StrIComp(PChar(Trim(Value)), PChar(FDirectory)) <> 0 then
 begin
   FDirectory := Trim(Value);

   if Running
   then RestartWatchThread;

   Change;
 end;
end;



procedure TDirectoryWatch.Change;
begin
 if Assigned(FOnChange)
 then FOnChange(Self);
end;



procedure TDirectoryWatch.Notify(const Action: Integer; const FileName: string);
begin
 // FILE_ACTION_* values from Windows are 1-5, mapping to waAdded(0)..waRenamedNew(4)
 if (Action < 1) OR (Action > Ord(High(TWatchAction)) + 1)
 then EXIT;  // Ignore unknown action values

 if Assigned(FOnNotify) then
   if TWatchAction(Action - 1) in FWatchActions
   then FOnNotify(Self, TWatchAction(Action - 1), FileName);
end;


end.
