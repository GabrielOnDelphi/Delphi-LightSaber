UNIT LightCore.Win.Window;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.Window is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Locate windows (anywhere in the OS) by
     * Handle
     * Class name

   Change visibility for windows (Minimize, Restore, SetFront, SetBack)

   See also:
     LightVcl.Common.Window.pas
     LightVcl.Common.VclUtils.pas

   Windows-only unit, package LightCore.Win. Every routine takes a Win32 window handle (HWND), a Win32 window class name, or a THandle that is a window handle, or drives the Windows shell (MinAllWnd_ByShell creates the Shell.Application COM object). None of these exists off Windows, so a build for Android, macOS or iOS stops at the top of this unit with a fatal compiler message. The routines that need a VCL form or the VCL Application object - KeepOnTop, MaximizeForm, FindWindowByTitle, FindChildForm - are in LightVcl.Common.Window, and so is the HWND overload of KeepOnTop, kept beside its TForm twin.
=============================================================================================================}

INTERFACE  
USES
   Winapi.Windows, System.Win.ComObj, Winapi.Messages, System.SysUtils;

CONST
   UM_ENSURERESTORED = WM_USER+ 65;       { For 'Run Single Instance' }

 { By class name }
 function IsApplicationRunning   (CONST ClassName: string): Boolean;

 { Restore - By handle }
 function  ForceForegroundWindow (WndHandle: HWND): Boolean;                         { Brings window on top of other windows. You need to call ForceRestoreWindow first }
 function  ForceRestoreWindow    (WndHandle: HWND; Immediate: Boolean): Boolean;

 { Find window }
 function  FindTopWindowByClass  (CONST ClassName: string): THandle;

 function  FindChildWindowByClass(Parent: HWnd;  CONST ClassName: string): THandle;  { http://www.delphipages.com/forum/showthread.php?t=6119 }

 function  GetTextFromHandle(hWND: THandle): string;

 { Window visibility }
 procedure SetWindowPosToFront   (WndHandle: HWND);                                  { Set the specified windows in top of all other windows in the system }
 procedure SetWindowPosToBack    (WndHandle: HWND);

 { Minimize / Restore }
 procedure MinimizeAllExcept(CONST ExceptApp: HWND);
 procedure MinAllWnd_ByShell;
 procedure MinAllWnd_ByShell2;                                                       { Minimize All Windows by sending a message to Shelltray }
 procedure MinAllWnd_ByHandle(ApplicationWindow: HWnd);                              { Minimizes by iterating window handles }
 procedure MinAllWnd_ByWinMKey;                                                      { Simulate Win + M }

 function  RestoreWindowByName   (CONST ClassName: string): Boolean;
 procedure RestoreWindow         (WndHandle: HWND);

 { Hacks }
 procedure Remove_X_Button       (FormHandle: THandle);




IMPLEMENTATION







{--------------------------------------------------------------------------------------------------
   WINDOW POS
--------------------------------------------------------------------------------------------------}
{ Set the specified windows in top of all other windows in the system. Does not activate the window (does not take the keyboard focus). }
procedure SetWindowPosToFront(WndHandle: HWND);
begin
  SetWindowPos(WndHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;


procedure SetWindowPosToBack(WndHandle: HWND);
begin
  SetWindowPos(WndHandle, HWND_NOTOPMOST,0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
end;


{ Restore app -
  Restore window if it was minimized to taskbar or systray }
procedure RestoreWindow(WndHandle: HWND);
begin
 if WndHandle = 0
 then raise Exception.Create('RestoreWindow: WndHandle parameter cannot be 0');

 Winapi.Windows.SendMessage(WndHandle, UM_ENSURERESTORED, 0, 0);  { Send Restore signal to running instance. The application will respond to this ONLY if I implemented code for it }
 ForceRestoreWindow(WndHandle, TRUE);
 ForceForegroundWindow(WndHandle);                                { Brings window on top of other windows. You need to call ForceRestoreWindow first }
end;


{ Restore window if it was minimized to taskbar or systray. You need to call ForceForegroundWindow after this.
  From: http://stackoverflow.com/questions/14440717/delphi-how-to-use-showwindow-properly-on-external-application }
function ForceRestoreWindow(WndHandle: HWnd; Immediate: Boolean): Boolean;
VAR WindowPlacement: TWindowPlacement;
begin
 Result := FALSE;
 if Immediate then
  begin
   WindowPlacement.Length := SizeOf(WindowPlacement);
   if GetWindowPlacement(WndHandle, @WindowPlacement) then
    begin
      if (WindowPlacement.flags AND WPF_RESTORETOMAXIMIZED) <> 0
      then WindowPlacement.showCmd := SW_MAXIMIZE
      else WindowPlacement.showCmd := SW_RESTORE;
      Result := SetWindowPlacement(WndHandle, @WindowPlacement);
    end;
  end
 else
  Result := SendMessage(WndHandle, WM_SYSCOMMAND, SC_RESTORE, 0) = 0;
end;


{ Forces a window to the foreground, working around Windows' SetForegroundWindow restrictions.
  Windows only allows SetForegroundWindow if the calling thread owns the foreground.
  This function attaches to the foreground thread's input queue to gain permission.
  Call ForceRestoreWindow first if the window might be minimized.
  Returns TRUE if the window was successfully brought to foreground. }
function ForceForegroundWindow(WndHandle: HWnd): Boolean;
VAR
   CurrThreadID: DWord;
   ForeThreadID: DWord;
begin
 Result:= TRUE;
 if GetForegroundWindow <> WndHandle then
  begin
   CurrThreadID:= GetWindowThreadProcessId(WndHandle, nil);
   ForeThreadID:= GetWindowThreadProcessId(GetForegroundWindow, nil);
   if ForeThreadID <> CurrThreadID then
     begin
       { Attach to foreground thread's input queue to bypass SetForegroundWindow restrictions }
       AttachThreadInput(ForeThreadID, CurrThreadID, TRUE);
       Result:= SetForegroundWindow(WndHandle);
       AttachThreadInput(ForeThreadID, CurrThreadID, FALSE);
       { Call again after detaching for reliability }
       if Result
       then Result:= SetForegroundWindow(WndHandle);
     end
   else
    Result:= SetForegroundWindow(WndHandle);
  end;
end;


{--------------------------------------------------------------------------------------------------
   FIND WINDOW
--------------------------------------------------------------------------------------------------}

{ Retrieves a handle to the TOP-LEVEL window that matches this class }
function FindTopWindowByClass(CONST ClassName: string): THandle;
begin
 Result:= WinApi.Windows.FindWindow(PWideChar(ClassName), NIL);
end;


{ Use it like this:  Found:= FindChildWindowByClass(MainForm.ClientHandle, 'TClientForm')<> 0 } { http://stackoverflow.com/questions/6844988/how-to-check-if-a-child-window-exists } {Old name: FindChildWindowByClass }
function FindChildWindowByClass(Parent: HWnd; CONST ClassName: string): THandle;
begin
  Result:= FindWindowEx(Parent, 0, PChar(ClassName), NIL);
end;


{-------------------------------------------------------------------------------------------------------------
  How to use a mutex? (Allow only one single application instance)

  Window := FindTopWindowByClass(ctAppWinClassName);
  if Window = 0
  then Application.Initialize + etc
  else (Send ParamStr to the other running instance);

  procedure TMainFrorm.CreateParams(var Params: TCreateParams);
  begin
   inherited;
   StrLCopy(PChar(@Params.WinClassName[0]), PChar(ctAppWinClassName), High(Params.WinClassName));  Give a unique name to our form
  end;

  For details see the BioniX project.
-------------------------------------------------------------------------------------------------------------}

{ Returns TRUE if a top-level window with the specified class name exists.
  Typically used for single-instance application detection via custom window class names. }
function IsApplicationRunning(CONST ClassName: string): Boolean;
begin
 if ClassName = ''
 then raise Exception.Create('IsApplicationRunning: ClassName parameter cannot be empty');

 Result:= FindTopWindowByClass(ClassName) <> 0;
end;












{--------------------------------------------------------------------------------------------------
                              MinimizeAllWindows
--------------------------------------------------------------------------------------------------}
procedure MinimizeAllExcept(const ExceptApp : HWND);
VAR h: HWND;
begin
  { Minimize all visible top-level windows except ExceptApp.
    Uses WM_SYSCOMMAND+SC_MINIMIZE per window instead of Shell.Application.MinimizeAll,
    which minimizes the calling app too (race condition with the restore call). }
  h:= GetTopWindow(0);  { Topmost Z-order window }
  while h <> 0 do
  begin
    if (h <> ExceptApp) AND IsWindowVisible(h) AND NOT IsIconic(h)
    then PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    h:= GetNextWindow(h, GW_HWNDNEXT);
  end;
end;


{ Recommended }
procedure MinAllWnd_ByShell;
VAR Shell : OleVariant;
begin
 Shell:= System.Win.ComObj.CreateOleObject('Shell.Application') ;
 Shell.MinimizeAll;
end;


{ Minimize all windows by sending a command message to the shell tray.
  Command 419 = Minimize All, Command 416 = Undo Minimize All }
procedure MinAllWnd_ByShell2;
VAR TrayWnd: HWND;
begin
 TrayWnd:= FindWindow('Shell_TrayWnd', nil);
 if TrayWnd <> 0
 then PostMessage(TrayWnd, WM_COMMAND, 419, 0);
end;


{ Minimizes all visible windows except the specified ApplicationWindow.
  Iterates through all windows in Z-order starting from ApplicationWindow.
  Uses PostMessage so minimization happens asynchronously.
  See also: ForceRestoreWindow for restoring minimized windows. }
procedure MinAllWnd_ByHandle(ApplicationWindow: HWnd);
VAR h: HWnd;
begin
  h:= ApplicationWindow;
  while h > 0 do
  begin
    if IsWindowVisible(h) AND (h <> ApplicationWindow)
    then PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    h:= GetNextWindow(h, GW_HWNDNEXT);
  end;
end;

{ Simulates Win+M keyboard shortcut to minimize all windows.
  Note: Uses legacy Keybd_event API. Consider SendInput for new code. }
procedure MinAllWnd_ByWinMKey;
begin
  Keybd_event(VK_LWIN, 0, 0, 0);
  Keybd_event(Byte('M'), 0, 0, 0);
  Keybd_event(Byte('M'), 0, KEYEVENTF_KEYUP, 0);
  Keybd_event(VK_LWIN, 0, KEYEVENTF_KEYUP, 0);
end;


{ Locate the window and bring it to front. Return false if cannot find window. }
function RestoreWindowByName(CONST ClassName: string): Boolean;
VAR Wnd: HWND;
begin
 if ClassName = ''
 then raise Exception.Create('RestoreWindowByName: ClassName parameter cannot be empty');

 Wnd:= FindTopWindowByClass(ClassName);                                 { Check if window exists }
 Result:= Wnd > 0;
 if Result
 then RestoreWindow(Wnd);      { Restore app }
end;






function GetTextFromHandle(hWND: THandle): string;
VAR
  pText: PChar;
  TextLen: integer;
begin
 TextLen:= GetWindowTextLength(hWND);        { Get the length of the text }
 if TextLen = 0
 then EXIT('');

 GetMem(pText, (TextLen + 1) * SizeOf(Char)); { Allocate memory including null terminator }
 TRY
   GetWindowText(hWND, pText, TextLen + 1);   { Get the control's text }
   Result:= String(pText);
 FINALLY
   FreeMem(pText);
 END;
end;



{--------------------------------------------------------------------------------------------------
   SYSTEM HACK
--------------------------------------------------------------------------------------------------}

{ Removes the Close (X) button from a window's system menu.
  Example: Remove_X_Button(MainForm.Handle) }
procedure Remove_X_Button(FormHandle: THandle);
VAR SysMenu: HMENU;
begin
  SysMenu:= GetSystemMenu(FormHandle, False);
  if SysMenu <> 0
  then DeleteMenu(SysMenu, SC_CLOSE, MF_BYCOMMAND);
end;





end.
