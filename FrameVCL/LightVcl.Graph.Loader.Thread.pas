UNIT LightVcl.Graph.Loader.Thread;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Loads multiple images in separate threads.
  Images are automatically resized to Width, Height.
  When the thumbnail of an image is ready, it is put in a Queue by PushPicture which
    also informs the caller that the thumbnail is ready (via a WM_THUMBNAIL_NOTIFY signal).
  The caller gets the picture from queue using PopPicture (the image is deleted from queue).
  The caller is responsible for freeing the thumbnail.

  HOW TO USE IT:

     BkgThread:= TBkgImgLoader.Create(Handle);
     BkgThread.Priority:= tpLower;
     BkgThread.FileList:= GetFolderContent("c:\images\");  // Thread takes ownership of FileList!
     BkgThread.Height:= 800;
     BkgThread.Width := 600;
     BkgThread.Start;

  The result is sent back to the application via a WM_THUMBNAIL_NOTIFY message/notification:

     procedure TForm1.WMThumbnailReady(VAR AMessage: TMessage);
     begin
       BMP:= BkgThread.PopPicture;
       if BMP <> NIL then
        begin
         Show(BMP);
         FreeAndNil(BMP);
        end;
     end;

  IMPORTANT:
    - FileList: The thread takes ownership of the TStringList and will free it when done.
                Do NOT free FileList after assigning it to the thread!
    - PopPicture: Returns NIL if the queue is empty. Always check the result before using.
    - The caller is responsible for freeing the bitmaps returned by PopPicture.
    - Free MUST be called from the thread that owns the window handle (typically the main
      UI thread). The destructor drains pending WM_THUMBNAIL_NOTIFY messages from the
      caller's queue; that drain only works on the calling thread's own queue.

  We can put a single file in FileList and process only that file.

  Used by: LightVcl.Visual.ThumbViewerM
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, Generics.Collections, System.SyncObjs,
  Vcl.Forms, Vcl.Graphics;

CONST
   { LIMITATION: hardcoded magic number — collides with any other component in the same
     process that also posts WM_APP+1 to the same window. Proper fix would be
     RegisterWindowMessage('TBkgImgLoader.WM_THUMBNAIL_NOTIFY') at unit init.
     Not done because: existing callers reference this constant directly.
     Ref: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-registerwindowmessagew }
   WM_THUMBNAIL_NOTIFY = WM_APP + 1;

TYPE
  TBkgImgLoader = class(TThread)
  private
    FWndHandle : HWND;
    ReadyThumbs: TQueue<TBitmap>;
    FQueueLock : TCriticalSection;
    Fexception : exception;
    procedure DoHandleexception;    
    procedure PushPicture(APicture: TBitmap);
    procedure ProcessFile(const AFileName: string);
  protected
    procedure Execute; override;
    procedure Handleexception; virtual;
  public
    SilentErrors: Boolean;                        { Silent: don't show errors in case the image cannot be loaded }
    Width: Integer;
    Height: Integer;
    FileList: TStringList;
    constructor Create(CONST AWndHandle: HWND);   { Handler to the caller window. The thread will inform the caller that a new thumbnail is ready via this handle }
    destructor Destroy; override;

    function  PopPicture: TBitmap;
  end;



IMPLEMENTATION
USES LightVcl.Graph.Resize;




{--------------------------------------------------------------------------------------------------
  THREAD CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

{ Creates the background image loader thread (suspended).
  AWndHandle: Handle to the caller window that will receive WM_THUMBNAIL_NOTIFY messages.
              The caller must have a valid window handle to receive notifications. }
constructor TBkgImgLoader.Create(CONST AWndHandle: HWND);
begin
 inherited Create(TRUE);                                                                           { Create suspended }
 Assert(IsWindow(AWndHandle), 'TBkgImgLoader.Create: AWndHandle must be a valid window handle');
 FWndHandle      := AWndHandle;
 ReadyThumbs     := TQueue<TBitmap>.Create;
 FQueueLock      := TCriticalSection.Create;
 SilentErrors    := False;    { If True, don't show errors when images fail to load }
 Width           := 256;
 Height          := 128;
 FileList        := NIL;      { Caller must assign FileList before Start. Thread takes ownership! }
 Priority        := tpLower;
end;


destructor TBkgImgLoader.Destroy;
VAR
  BMP: TBitmap;
  Msg: TMsg;
begin
 { Order matters: signal Terminate, then wait for Execute to finish (via inherited Destroy)
   BEFORE touching ReadyThumbs/FQueueLock — otherwise a still-running PushPicture
   would access a freed lock and crash. }
 Terminate;
 inherited Destroy;   { Waits for the thread to exit Execute }

 { Drain any WM_THUMBNAIL_NOTIFY messages the worker posted before Terminate was observed.
   Without this, the caller's handler fires later and calls PopPicture on this freed
   instance → AV.

   MSDN-verified facts (do NOT "optimize" without re-reading these):
     Ref: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-peekmessagew

   • Cross-thread requirement — confirmed verbatim in MSDN:
       "The window must belong to the current thread."
     → Free MUST run on the thread that owns FWndHandle (typically main UI). See
       IMPORTANT block in unit header.

   • WM_QUIT is NOT consumed by this drain — confirmed by MSDN + multiple sources.
     MSDN warns: "PeekMessage always retrieves WM_QUIT messages, no matter which values
     you specify for wMsgFilterMin and wMsgFilterMax" — but that caveat applies ONLY
     when hwnd=NULL. With a specific hwnd (as here), WM_QUIT (whose MSG.hwnd is NULL)
     is filtered out by the hwnd parameter and stays in the queue.
     → DO NOT change FWndHandle to NULL here — that would eat the app's shutdown signal.

   • Filter range is inclusive; PM_REMOVE removes only messages matching the filter. }
 if IsWindow(FWndHandle) then
   while PeekMessage(Msg, FWndHandle, WM_THUMBNAIL_NOTIFY, WM_THUMBNAIL_NOTIFY, PM_REMOVE) do
     ; { intentionally empty - just drain }

 { Now safe — no other thread is touching the queue or the lock }
 if ReadyThumbs <> NIL then
   while ReadyThumbs.Count > 0 DO
    begin
     BMP:= ReadyThumbs.Dequeue;
     FreeAndNil(BMP);
    end;
 FreeAndNil(FileList);   { No-op for Started threads (Execute's FINALLY freed+niled it). REQUIRED for never-Started threads: TThread.ShutdownThread calls Terminate BEFORE Resume, and ThreadProc then skips Execute entirely ('if not Thread.Terminated' - System.Classes), so Execute's FINALLY never runs for them. }
 FreeAndNil(FQueueLock);
 FreeAndNil(ReadyThumbs);
end;





{--------------------------------------------------------------------------------------------------
   THREAD EXECUTE
   Main thread loop: iterates through FileList and loads each image.
   FileList is freed when done (thread owns it).
   All exceptions are caught to prevent thread crashes - see Delphi documentation on TThread.Execute.
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.Execute;
VAR CurFile: string;
begin
 Assert(FileList <> NIL, 'TBkgImgLoader.Execute: FileList must be assigned before starting the thread!');

 TRY
   for CurFile in FileList DO
    TRY
      if Terminated then EXIT;
      ProcessFile(CurFile);
    EXCEPT
      //todo 1: trap only specific exceptions (EFOpenError, EInvalidGraphic, etc.)
      Handleexception;                                                  { Catch all exceptions to prevent thread crash. Delphi TThread requires this. }
    END;
 FINALLY
   FreeAndNil(FileList);   { Thread owns FileList - free it when done }
 END;
end;


{ Loads a single image file, resizes it to Width x Height, and pushes it to the queue.
  Handles exceptions gracefully based on SilentErrors setting. }
procedure TBkgImgLoader.ProcessFile(CONST AFileName: string);
VAR BMP: TBitmap;
begin
 if Terminated then EXIT;
 if NOT FileExists(AFileName) then EXIT;

 BMP:= NIL;
 TRY
  BMP:= LoadAndStretch(AFileName, Width, Height);

  { Push thumbnail in queue }
  if BMP <> NIL then
    if Terminated
    then FreeAndNil(BMP)                                               { Thread terminated - free bitmap since caller won't receive it }
    else PushPicture(BMP);

 EXCEPT
   //todo 1: trap only specific exceptions (EFOpenError, EInvalidGraphic, etc.)
   FreeAndNil(BMP);                                                    { Release the image in case of an error }
   if NOT SilentErrors then RAISE;
 END;
end;





{--------------------------------------------------------------------------------------------------
   exception HANDLING
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.DoHandleexception;
begin
 if GetCapture <> 0
 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);                    // Cancel the mouse capture

 { Now actually show the exception }
 if Fexception is exception
 then Application.Showexception(Fexception)
 else System.SysUtils.Showexception(Fexception, nil);
end;


procedure TBkgImgLoader.Handleexception;                               // This function is virtual so you can override it and add your own functionality.
begin
 Fexception := exception(exceptObject);
 TRY
   if NOT (Fexception is EAbort)                                       // Don't show EAbort messages
   then Synchronize(DoHandleexception);
 FINALLY
   Fexception := NIL;
 end;
end;





{--------------------------------------------------------------------------------------------------
   PUSH RESULTS TO QUEUE
--------------------------------------------------------------------------------------------------}

procedure TBkgImgLoader.PushPicture(APicture: TBitmap);     { Push thumbnail in queue }
begin
 FQueueLock.Enter;
 TRY
   ReadyThumbs.Enqueue(APicture);
 FINALLY
   FQueueLock.Leave;
 END;

 { Send a message to the main program to let it know a new thumb is ready/available.
   Skip if Terminate was already signaled — the destructor will free the bitmap from
   the queue and drain any in-flight messages anyway. }
 if NOT Terminated then
   PostMessage(FWndHandle, WM_THUMBNAIL_NOTIFY, 0, 0);
end;


{ Retrieves the next ready thumbnail from the queue.
  Returns NIL if the queue is empty. Caller is responsible for freeing the returned bitmap. }
function TBkgImgLoader.PopPicture: TBitmap;
begin
 FQueueLock.Enter;
 TRY
   if ReadyThumbs.Count > 0
   then Result:= ReadyThumbs.Dequeue
   else Result:= NIL;
 FINALLY
   FQueueLock.Leave;
 END;
end;





end.
