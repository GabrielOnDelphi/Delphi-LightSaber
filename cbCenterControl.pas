UNIT cbCenterControl;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

   Center control on form
   Center form on screen
=============================================================================================================}

INTERFACE
{$I Frameworks.inc}

USES
   Vcl.Controls, Vcl.Forms;

{=============================================================================================================
   FORM POSITION
=============================================================================================================}
 Procedure CorrectMDIFormPosition    (ParentForm: TForm);
 Procedure CorrectFormPositionScreen (Form: TForm);                                              { Make sure that the child window is not outside the Screen }
 Procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);  deprecated 'Use Form.MakeFullyVisible(Monitor)';
 procedure CenterForm                (Form: TForm);                     overload;                { You can also use Form.Position:= poMainFormCenter or poScreenCenter' }
 procedure CenterForm                (Form, Parent: TForm);             overload;

{=============================================================================================================
   CTRL POSITION
=============================================================================================================}
 Procedure CorrectCtrlPosition       (Ctrl, Parent: TControl);                                     overload;
 Procedure CorrectCtrlPosition       (Ctrl: TControl; CONST aParentWidth, aParentHeight: Integer); overload;
 procedure CenterInvalidChild        (Ctrl, Parent: TControl);                                   { Center Chiald in Parent window but only if Child has 'bad' coordinates }
 procedure CenterChild               (Ctrl, Parent: TControl);                                   { Center Chiald in Parent window }
 procedure CenterChildX              (Ctrl, Parent: TControl);


IMPLEMENTATION


{=============================================================================================================
   FORM POSITION
=============================================================================================================}

{ Brings the form back into the screen, IF it was outside the screen.
  Usage:  CorrectFormScreenPosition(Self).
  It is automaticalled by cv_IniFile.LoadForm.

  Screen.WorkArea -> Specifies the work area on the Primary monitor.
  DesktopWidth    -> Determines the width of the desktop. The desktop is defined as the entire virtual desktop, which includes all monitors in the system. On a single-monitor system, DesktopWidth corresponds to Width. }
Procedure CorrectFormPositionScreen(Form: TForm);                                                             { Old name: RepairPositionOnScreen }
begin
 CorrectCtrlPosition(Form, Screen.DesktopWidth, Screen.DesktopHeight);
end;


{ Make sure that the specified form fits into the specified monitor. }
Procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);
begin
 Form.MakeFullyVisible(Monitor);
end;


{ Center form in screen }
procedure CenterForm(Form: TForm);
VAR
   XPos, YPos : Integer;
Begin
  Xpos := (Screen.Width - Form.Width)  DIV 2;
  YPos := (Screen.Height- Form.Height) DIV 2;
  Form.Left:= Xpos;
  Form.Top := YPos;
End;


procedure CenterForm(Form, Parent: TForm);
VAR Left, Top: Integer;
begin
 Left:= Parent.Left+ (Parent.ClientWidth - Form.Width) div 2;

 if Left < 0                                { Happens when the child is bigger than the parent }
 then Left:= Parent.Left - Left;
 Form.Left:= Left;

 Top:= Parent.Top+ (Parent.ClientHeight- Form.Height) div 2;

 if Top < 0                                 { Happens when the child is bigger than the parent }
 then Top:= Parent.Top - Top;
 Form.Top:= Top;
end;


{ Enumerate through all children form of ParentFrom and ensure their position/size }
Procedure CorrectMDIFormPosition(ParentForm: TForm);
VAR i: Integer;
begin
  for I := ParentForm.MDIChildCount-1 downto 0 do
    begin
     { Vertical }
     if ParentForm.MDIChildren[I].Top < 0
     then ParentForm.MDIChildren[I].Top := 0;
     if ParentForm.MDIChildren[I].Height > ParentForm.ClientHeight
     then ParentForm.MDIChildren[I].Height:= ParentForm.ClientHeight-10;

     { Horizontal }
     if ParentForm.MDIChildren[I].Left < 0
     then ParentForm.MDIChildren[I].Left:= 0;
     if ParentForm.MDIChildren[I].Width > ParentForm.ClientWidth
     then ParentForm.MDIChildren[I].Width := ParentForm.ClientWidth;
    end;
end;






{=============================================================================================================
   CENTER CTRL
=============================================================================================================}

{ Centers the child into the parent, IF the child was outside the screen.

  Note! The Top is relative to parent's client area.
  In other words, if the Parent has a toolbar/panel (height = 200) aligned to its top and the Child form is
  imediatelly under that panel, Child's top will be 0, not 500!
  Usage: CorrectFormScreenPosition(Self, ParentForm)  }
Procedure CorrectCtrlPosition(Ctrl, Parent: TControl);
begin
 CorrectCtrlPosition(Ctrl, Parent.ClientWidth, Parent.ClientHeight);
end;


Procedure CorrectCtrlPosition(Ctrl: TControl; CONST aParentWidth, aParentHeight: Integer);  {TODO 2: this won't be aligned correctly when it is under the parent bottom and the parent height is big (over 1000 pixels) }
begin
 if Ctrl.Top < 0
 then Ctrl.Top := 0;               { It was too high, show it imediatelly under the top }
 if Ctrl.Top  > aParentHeight-10
 then Ctrl.Top := aParentHeight- (Ctrl.Height DIV 2);  { It was too low, put_its top in the middle of the parent (to be clear, we don't center the whole child into the parent; we only put child's top in the midle of the parent) }

 if Ctrl.Left < 0
 then Ctrl.Left:= 0;
 if Ctrl.Left > aParentWidth-10
 then Ctrl.Left:= aParentWidth- (Ctrl.Width DIV 2);

 if Ctrl.Height > aParentHeight
 then Ctrl.Height:= aParentHeight;
 if Ctrl.Width  > aParentWidth
 then Ctrl.Width:= aParentWidth;
end;


{ Center Chiald in Parent window but only if Child has 'bad' coordinates }
procedure CenterInvalidChild(Ctrl, Parent: TControl);
VAR iTop: Integer;
begin
 if (Ctrl.Top < -10)
 OR (Ctrl.Left< -10)
 OR (Ctrl.Left> Parent.Width) then   { But only if goes out of screen so the user can't find it }
  begin
   Ctrl.Left:= (Parent.ClientWidth - Ctrl.Width)  div 2;
   iTop:= Parent.ClientHeight- Ctrl.Height;
   //if Parent.ToolBar.Visible then iTop:= iTop- Parent.ToolBar.Height;
   Ctrl.Top:= iTop div 2;
  end;
end;


{ Center Child ctrl in Parent window }
{ToDo: We should take into consideration the controls that are aligned (alleft, alright) }
procedure CenterChild(Ctrl, Parent: TControl);
VAR Left, Top: Integer;
begin
 Left:= (Parent.ClientWidth - Ctrl.Width) div 2;

 if Left < 0                                { Happens when the child is bigger than the parent }
 then Left:= Parent.Left - Left;
 Ctrl.Left:= Left;

 Top:= (Parent.ClientHeight- Ctrl.Height) div 2;

 if Top < 0                                 { Happens when the child is bigger than the parent }
 then Top:= Parent.Top - Top;
 Ctrl.Top:= Top;
end;


{ Center Child ctrl in Parent window, bot only on the X axis }
procedure CenterChildX(Ctrl, Parent: TControl);
begin
 Ctrl.Left:= (Parent.ClientWidth - Ctrl.Width) div 2;  {todo: We should take into consideration the controls that are aligned (alleft, alright) }
end;


end.
