UNIT LightVcl.Common.Window;

{=============================================================================================================
   2026.09.30
   www.GabrielMoraru.com

==============================================================================================================

   Locate windows (anywhere in the OS) by
     * Caption

   Locate the MDI child form of a given class

   Change visibility for windows (Maximize, keep on top)

   See also:
     LightCore.Win.Window.pas - locate windows by handle or class name; minimize, restore, bring to front
     LightVcl.Common.VclUtils.pas

   In this group:
     * LightVcl.Common.Shell.pas
     * LightVcl.Common.System.pas
     * LightVcl.Common.Window.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE  
USES
   Winapi.Windows, System.SysUtils, Vcl.Forms;

 { Find window }
 function  FindWindowByTitle     (CONST WindowTitle: string; PartialSearch: Boolean= TRUE; CaseSens: Boolean= FALSE): Hwnd;
 function  FindChildForm         (Parent: TForm; CONST ClassName: string): THandle;

 { On top }
 procedure KeepOnTop             (Form: TForm; TopStyle: HWnd);       overload;
 procedure KeepOnTop             (Handle: HWND; StayOnTop: Boolean);  overload;      { Not tested }
 procedure MaximizeForm          (Form: TForm; Maximize: Boolean);                   { Make form normal or maximized/ontop }





IMPLEMENTATION
USES LightCore;







{--------------------------------------------------------------------------------------------------
   WINDOW POS
--------------------------------------------------------------------------------------------------}
{ Use it like this:  KeepOnTop(Self, HWND_TOPMOST)
  Documentation: https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setwindowpos }
procedure KeepOnTop(Form: TForm; TopStyle: HWnd);
{  The TopStyle parameter can be:
     HWND_TOPMOST   - Places the window above all non-topmost windows. The window maintains its topmost position even when it is deactivated.
     HWND_NOTOPMOST - Places the window above all non-topmost windows
     HWND_TOP       - Places the window at the top of the Z order.
     HWND_BOTTOM    - Places the window at the bottom of the Z order.   }
begin
  if Form = NIL
  then raise Exception.Create('KeepOnTop: Form parameter cannot be nil');

  SetWindowPos(Form.Handle, TopStyle, Form.Left, Form.Top, Form.Width, Form.Height, SWP_NOACTIVATE OR SWP_NOMOVE OR SWP_NOSIZE {or SWP_NOOWNERZORDER});
end;


{ Not tested }
procedure KeepOnTop(Handle: HWND; StayOnTop: Boolean);
CONST HWND_STYLE: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, HWND_STYLE[StayOnTop], 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
end;


{ Make form normal or maximized/ontop.
  Delphi help says: It is not advisable to change FormStyle at runtime!
  WARNING: Uses a module-level variable to store bounds. If called for multiple forms
  before restoring any, only the last form's bounds are preserved. Consider storing
  bounds per-form if this becomes an issue. }
VAR
   LastBounds: TRect = ();
procedure MaximizeForm(Form: TForm; Maximize: Boolean);
begin
 if Form = NIL
 then raise Exception.Create('MaximizeForm: Form parameter cannot be nil');

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








{--------------------------------------------------------------------------------------------------
   FIND WINDOW
--------------------------------------------------------------------------------------------------}

{ Searches all top-level windows for one matching the specified title.
  Returns window handle if found, else 0.
  PartialSearch: if TRUE, matches if WindowTitle is contained anywhere in the window title.
  CaseSens: if TRUE, comparison is case-sensitive.
  Note: Uses GetWindow enumeration which iterates through all windows in Z-order. }
function FindWindowByTitle(CONST WindowTitle: string; PartialSearch: Boolean= TRUE; CaseSens: Boolean= FALSE): Hwnd;
var
  NextHandle: Hwnd;
  NextTitle: array[0..260] of char;
  s: string;
begin
  if Application.Handle <> 0
  then NextHandle:= GetWindow(Application.Handle, GW_HWNDFIRST)               // Get the first window
  else NextHandle:= GetWindow(GetDesktopWindow, GW_CHILD);                    // A console program has no Application window: TApplication.CreateHandle skips it when IsConsole (Vcl.Forms.pas). Start at the top-level window highest in the Z order.
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


{--------------------------------------------------------------------------------------------------
   FIND FORM
--------------------------------------------------------------------------------------------------}
function FindChildForm(Parent: TForm; CONST ClassName: string): THandle;     { Old name: FormFindChildWnd }
VAR i: integer;
begin
 if Parent = NIL
 then raise Exception.Create('FindChildForm: Parent parameter cannot be nil');

 if ClassName = ''
 then raise Exception.Create('FindChildForm: ClassName parameter cannot be empty');

 Result:= 0;
 for i:= 0 to Parent.MDIChildCount-1 DO
   if SameText(Parent.MDIChildren[i].ClassName, ClassName)
   then EXIT((Parent.MDIChildren[i] as TForm).Handle);
end;







end.
