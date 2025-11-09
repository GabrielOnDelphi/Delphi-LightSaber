UNIT LightVcl.Common.Window;

{=============================================================================================================
   SYSTEM - Window
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Locate windows (anywhere in the OS) by
     * Handle
     * Caption
     * Class name

   Cahnge visibility for windows (Minimize, Maximize, Restore, SetBack)

   See also:
     cmWindowMetrics.pas
     LightVcl.Common.VclUtils.pas

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE  
USES
   Winapi.Windows, System.Win.ComObj, Winapi.Messages, System.SysUtils, Vcl.Forms;

CONST
   UM_ENSURERESTORED = WM_USER+ 65;       { For 'Run Single Instance' }

 { By class name }
 function IsApplicationRunning   (CONST ClassName: string): Boolean;

 { Restore - By handle }
 function  ForceForegroundWindow (WndHandle: HWND): Boolean;                         { Brings window on top of other windows. You need to call ForceRestoreWindow first }
 function  ForceRestoreWindow    (WndHandle: HWND; Immediate: Boolean): Boolean;

 { Find window }
 function  FindWindowByTitle     (CONST WindowTitle: string; PartialSearch: Boolean= TRUE; CaseSens: Boolean= FALSE): Hwnd;
 function  FindTopWindowByClass  (CONST ClassName: string): THandle;

 function  FindChildWindowByClass(Parent: HWnd;  CONST ClassName: string): THandle;  { http://www.delphipages.com/forum/showthread.php?t=6119 }
 function  FindChildForm         (Parent: TForm; CONST ClassName: string): THandle;

 function  GetTextFromHandle(hWND: THandle): string;

 { Window visibility }
 procedure SetWindowPosToFront   (WndHandle: HWND);                                  { Set the specified windows in top of all other windows in the system }
 procedure SetWindowPosToBack    (WndHandle: HWND);

 { On top }
 procedure KeepOnTop             (Form: TForm; TopStyle: HWnd);       overload;
 procedure KeepOnTop             (Handle: HWND; StayOnTop: Boolean);  overload;      { Not tested }
 procedure MaximizeForm          (Form: TForm; Maximize: Boolean);                   { Make form normal or maximized/ontop }

 { Minimize / Restore }
 procedure MinimizeAllExcept(CONST ExceptApp: HWND);
 procedure MinAllWnd_ByShell;
 procedure MinAllWnd_ByShell2;                                                       { Minimize All Windows by sending a message to Shelltray }
 procedure MinAllWnd_ByHandle(ApplicationWindow: HWnd);                              { Merge prost}
 procedure MinAllWnd_ByWinMKey;                                                      { Simulate Win + M }

 function  RestoreWindowByName   (CONST ClassName: string): Boolean;
 procedure RestoreWindow         (WndHandle: HWND);

 { Hacks }
 procedure Remove_X_Button       (FormHandle: THandle);




IMPLEMENTATION
USES LightCore, LightCore.Time;







{--------------------------------------------------------------------------------------------------
   WINDOW POS
--------------------------------------------------------------------------------------------------}
{ Set the specified windows in top of all other windows in the system }
procedure SetWindowPosToFront(WndHandle: HWND);
begin
  SetWindowPos(WndHandle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE);
end;


procedure SetWindowPosToBack(WndHandle: HWND);
begin
  SetWindowPos(WndHandle, HWND_NOTOPMOST,0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
end;


{ Use it like this:  KeepOnTop(Self, HWND_TOPMOST)
  Documentation: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setwindowpos }
procedure KeepOnTop(Form: TForm; TopStyle: HWnd);
{  The TopStyle parameter can be:
     HWND_TOPMOST   - Places the window above all non-topmost windows. The window maintains its topmost position even when it is deactivated.
     HWND_NOTOPMOST - Places the window above all non-topmost windows
     HWND_TOP       - Places the window at the top of the Z order.
     HWND_BOTTOM    - Places the window at the bottom of the Z order.   }
begin
  SetWindowPos(Form.Handle, TopStyle, Form.Left, Form.Top, Form.Width, Form.Height, SWP_NOACTIVATE OR SWP_NOMOVE OR SWP_NOSIZE {or SWP_NOOWNERZORDER});
end;


{ Not tested }
procedure KeepOnTop(Handle: HWND; StayOnTop: Boolean);
CONST HWND_STYLE: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, HWND_STYLE[StayOnTop], 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
end;


{ Make form normal or maximized/ontop
  Delphi help says: It is not advisable to change FormStyle at runtime !!! }
VAR
   LastBounds: TRect = ();
procedure MaximizeForm(Form: TForm; Maximize: Boolean);
//del
//const
{$J+}    { Writeable typed constants }
  //LastBounds: TRect = ();
{$J-}
begin
 if Maximize then
  begin
    LastBounds       := Form.BoundsRect;
    Form.FormStyle   := fsStayOnTop;  { Delphi help: Note: It is not advisable to change FormStyle at runtime. }
    Form.BorderStyle := bsNone;
    Form.BoundsRect  := Screen.MonitorFromWindow(Form.Handle).BoundsRect;
  end
 else
  begin
    Form.FormStyle   := fsNormal;
    Form.BorderStyle := bsSizeable;
    Form.BoundsRect  := LastBounds;
  end;
end;








{ Restore app -
  Restore window if it was minimized to taskbar or systray }
procedure RestoreWindow(WndHandle: HWND);
begin
 Assert(WndHandle > 0);
 Winapi.Windows.SendMessage(WndHandle, UM_ENSURERESTORED, 0, 0);{ Send Restore signal to running instance. The application will respond to this ONLY if I implemented code for it }
 ForceRestoreWindow(WndHandle, TRUE);
 ForceForegroundWindow(WndHandle);                              { Brings window on top of other windows. You need to call ForceRestoreWindow first }
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


{ Brings window on top of other windows. You need to call ForceRestoreWindow first }
function ForceForegroundWindow(WndHandle: HWnd): Boolean;
VAR
   CurrThreadID: DWord;
   ForeThreadID: DWord;
begin
 Result := TRUE;
 if (GetForegroundWindow <> WndHandle) then
  begin
   CurrThreadID := GetWindowThreadProcessId(WndHandle, nil);
   ForeThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
   if (ForeThreadID <> CurrThreadID) then
     begin
       AttachThreadInput(ForeThreadID, CurrThreadID, TRUE);
       Result := SetForegroundWindow(WndHandle);
       AttachThreadInput(ForeThreadID, CurrThreadID, FALSE);
       if Result
       then Result := SetForegroundWindow(WndHandle);
     end
   else
    Result := SetForegroundWindow(WndHandle);
  end;
end;


{--------------------------------------------------------------------------------------------------
   FIND WINDOW
--------------------------------------------------------------------------------------------------}

{ Returns window's handle if found, else zero.
  See also: FindComponent }
function FindWindowByTitle(CONST WindowTitle: string; PartialSearch: Boolean= TRUE; CaseSens: Boolean= FALSE): Hwnd;
var
  NextHandle: Hwnd;
  NextTitle: array[0..260] of char;
  s: string;
begin
  NextHandle := GetWindow(Application.Handle, GW_HWNDFIRST);                  // Get the first window
  WHILE NextHandle > 0 DO
   begin
     GetWindowText(NextHandle, NextTitle, 255);                               // retrieve its text
     s:= StrPas(NextTitle);
     if LightCore.find(WindowTitle, s, PartialSearch, CaseSens)
     then
      begin
       Result:= NextHandle;
       Exit;
      end
    else
     NextHandle := GetWindow(NextHandle, GW_HWNDNEXT);                        // Get the next window
  end;
  Result := 0;
end;


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

  Window := csWindow.FindTopWindowByClass(ctAppWinClassName);
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

function IsApplicationRunning(CONST ClassName: string): Boolean;
VAR Wnd: HWND;
begin
 Assert(ClassName > '', 'IsApplicationRunning - ClassName is empty!');

 Result:= FALSE;
 REPEAT
  Wnd:= FindTopWindowByClass(ClassName);                                 { Check if mutex exists }
  if Wnd > 0
  then EXIT(TRUE);
 UNTIL Wnd <= 0;
end;












{--------------------------------------------------------------------------------------------------
                              MinimizeAllWindows
--------------------------------------------------------------------------------------------------}
procedure MinimizeAllExcept(const ExceptApp : HWND);
var
  Ole : OleVariant;
begin
  //This is like pressing WIN + M it makes use of the Shell. It's much safer for the user. Parameter excpt saves an window from beeing minimised
  ShowWindow(ExceptApp, SW_HIDE);
  Ole := CreateOleObject('Shell.Application');
  Ole.MinimizeAll;
  {
  tmp : DWORD;
  tmp := GetTickCount;          // GetTickCount accuracy= 15ms+  https://blogs.msdn.microsoft.com/oldnewthing/20050902-00/?p=34333
  WHILE GetTickCount - tmp < 300
   DO Application.ProcessMessages;  //Wait for minimization to complete    }
  Sleep(300);
  ShowWindow(ExceptApp, SW_SHOW);
end;


{ Recomended }
procedure MinAllWnd_ByShell;
VAR Shell : OleVariant;
begin
 Shell:= System.Win.ComObj.CreateOleObject('Shell.Application') ;
 Shell.MinimizeAll;
end;


procedure MinAllWnd_ByShell2;
VAR IntHwnd: Integer;
begin
 IntHwnd:= FindWindow('Shell_TrayWnd', nil);
 PostMessage(IntHwnd, WM_COMMAND, 419, 0);  { You can change 419 with 416 to restore the window }
end;


 // Also exists: ForceRestoreWindow - Restore window if it was minimized to taskbar or systray
procedure MinAllWnd_ByHandle(ApplicationWindow: HWnd);    { Use 'Handle' as parameter }
VAR h: HWnd;
begin
  h:= ApplicationWindow;
  while h> 0 do
  begin
    if IsWindowVisible(h) AND (h <> ApplicationWindow)
    then PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    h:= GetNextWindow(h, GW_HWNDNEXT);
  end;
end;

procedure MinAllWnd_ByWinMKey;  // Or Simulate Win + M:
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
 Assert(ClassName > '', 'ClassName was not defined!');

 Wnd:= FindTopWindowByClass(ClassName);                                 { Check if mutex exists }
 Result:= Wnd > 0;
 if Result
 then RestoreWindow(Wnd);      { Restore app }
end;






{--------------------------------------------------------------------------------------------------
   FIND FORM
--------------------------------------------------------------------------------------------------}
function FindChildForm(Parent: TForm; CONST ClassName: string): THandle;     {Old name: FormFindChildWnd }
VAR i: integer;
begin
 Result:= 0;
 for i:= 0 to Parent.MDIChildCount-1 DO
   if SameText(Parent.MDIChildren[i].ClassName, ClassName)
   then EXIT((Parent.MDIChildren[i] as TForm).Handle);
end;


function GetTextFromHandle(hWND: THandle): string;
VAR pText : PChar;
    TextLen : integer;
begin
 TextLen:=GetWindowTextLength(hWND);         { get the length of the text}
 GetMem(pText,TextLen);                      { alocate memory}   // takes a pointer
 TRY
   GetWindowText(hWND, pText, TextLen + 1);  { get the control's text}
   Result:= String(pText);                   { display the text}
 FINALLY
   FreeMem(pText);                           { free the memory}
 END;
end;



{--------------------------------------------------------------------------------------------------
   SYSTEM HACK
--------------------------------------------------------------------------------------------------}

{ Example: Remove_X_Button(MainForm.Handle) }
procedure Remove_X_Button(FormHandle: THandle);
VAR hMenuHandle: Integer;
begin
  hMenuHandle := GetSystemMenu(FormHandle, False);
  if (hMenuHandle <> 0)
  then DeleteMenu(hMenuHandle, SC_CLOSE, MF_BYCOMMAND);
end;





end.
