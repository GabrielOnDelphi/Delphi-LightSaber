UNIT LightVcl.Common.CenterControl;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================
   Center control on form
   Center form on screen


  Explanations:
    Screen.WorkArea      ->  Specifies the work area on the Primary monitor.
    Screen.DesktopWidth  ->  Determines the width of the desktop. The desktop is defined as the entire virtual desktop, which includes all monitors in the system. On a single-monitor system, DesktopWidth corresponds to Width.
=============================================================================================================}

INTERFACE

USES
   System.Types, System.Math,
   Vcl.Controls, Vcl.Forms;

{=============================================================================================================
   FORM POSITIONING
=============================================================================================================}

{ Ensures the form is fully visible on on the whole desktop }
procedure CorrectFormPositionDesktop(Form: TForm);

{ Ensures the form is fully visible on its primary monitor }
procedure CorrectFormPositionMainMonitor(Form: TForm);                deprecated 'Use Form.MakeFullyVisible(Screen.PrimaryMonitor)';

{ Ensures the form is fully visible on the specified monitor }
procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor); deprecated 'Use Form.MakeFullyVisible(Monitor)';

{ Center form on primary monitor (client work area) }
procedure CenterForm(Form: TForm); overload;

{ Center form within a parent form }
procedure CenterForm(Form, Parent: TForm); overload;

{ Ensures that the MDI child windows stay within the parent client area }
procedure CorrectMDIFormPosition(ParentForm: TForm);

{=============================================================================================================
   CONTROL POSITIONING
=============================================================================================================}

{ Adjusts the control's position/size so it stays within parent client area }
 Procedure CorrectCtrlPosition(Ctrl: TControl; CONST ParentWidth, ParentHeight: Integer); overload;

{ Adjusts the control's position/size so it stays within the specified bounds }
procedure CorrectCtrlPosition(Ctrl, Parent: TControl); overload;

{ Center child in parent if it has invalid/out-of-bounds coordinates }
procedure CenterInvalidChild(Ctrl, Parent: TControl);

{ Center child in parent client area }
procedure CenterChild(Ctrl, Parent: TControl);

{ Center child horizontally in parent client area }
procedure CenterChildX(Ctrl, Parent: TControl);


IMPLEMENTATION


{=============================================================================================================
   FORM POSITIONING
=============================================================================================================}

{ Brings the form back into the screen, IF it was outside the screen.
  Usage:  CorrectFormScreenPosition(Self).
  It is automaticalled by cv_IniFile.LoadForm.

  Screen.WorkArea -> Specifies the work area on the Primary monitor.
  DesktopWidth    -> Determines the width of the desktop. The desktop is defined as the entire virtual desktop, which includes all monitors in the system. On a single-monitor system, DesktopWidth corresponds to Width. }
procedure CorrectFormPositionDesktop(Form: TForm);    //Old name: CorrectFormPositionScreen
begin
 CorrectCtrlPosition(Form, Screen.DesktopWidth, Screen.DesktopHeight);
end;


{ Brings the form back into the screen, if it was outside the screen.
  It is automaticalled by LoadForm() }
procedure CorrectFormPositionMainMonitor(Form: TForm);
begin
  Form.MakeFullyVisible(Screen.PrimaryMonitor);
end;


{ Make sure that the specified form fits into the specified monitor. }
procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);
begin
  Form.MakeFullyVisible(Monitor);
end;


{  Center within primary monitor work area (excludes taskbar) }
procedure CenterForm(Form: TForm);
var
  WorkArea: TRect;
begin
  {old code
  Xpos := (Screen.Width - Form.Width)  DIV 2;
  YPos := (Screen.Height- Form.Height) DIV 2;
  Form.Left:= Xpos;
  Form.Top := YPos;}
  
  WorkArea := Screen.PrimaryMonitor.WorkareaRect;
  Form.Left := WorkArea.Left + ((WorkArea.Width  - Form.Width) div 2);
  Form.Top  := WorkArea.Top  + ((WorkArea.Height - Form.Height) div 2);
end;


procedure CenterForm(Form, Parent: TForm);
begin
  // Center relative to parent client area
  Form.Left := Parent.Left + ((Parent.ClientWidth  - Form.Width) div 2);
  Form.Top  := Parent.Top  + ((Parent.ClientHeight - Form.Height) div 2);
end;




{=============================================================================================================
   MDI
=============================================================================================================}
{ Enumerate through all children form of ParentForm and ensure their position/size }
procedure CorrectMDIFormPosition(ParentForm: TForm);
var
  i: Integer;
  Child: TForm;
begin
  for i := 0 to ParentForm.MDIChildCount - 1 do
  begin
    Child := ParentForm.MDIChildren[i];
    // Clamp Top and Height
    Child.Height := Min(Child.Height, ParentForm.ClientHeight);
    Child.Top    := EnsureRange(Child.Top, 0, ParentForm.ClientHeight - Child.Height);
    // Clamp Left and Width
    Child.Width  := Min(Child.Width, ParentForm.ClientWidth);
    Child.Left   := EnsureRange(Child.Left, 0, ParentForm.ClientWidth - Child.Width);
  end;
end;





{=============================================================================================================
   CENTER CTRL
=============================================================================================================}

{ Centers the child into the parent, IF the child was outside the screen.

  Note! The Top is relative to parent's client area.
  In other words, if the Parent has a toolbar/panel (height = 200) aligned to its top and the Child form is
  immediately under that panel, Child's top will be 0, not 500!
  Usage: CorrectFormScreenPosition(Self, ParentForm)  }
procedure CorrectCtrlPosition(Ctrl, Parent: TControl);
begin
  CorrectCtrlPosition(Ctrl, Parent.ClientWidth, Parent.ClientHeight);
end;


procedure CorrectCtrlPosition(Ctrl: TControl; const ParentWidth, ParentHeight: Integer);
begin
  // Ensure control does not exceed parent size
  Ctrl.Width  := Min(Ctrl.Width, ParentWidth);
  Ctrl.Height := Min(Ctrl.Height, ParentHeight);

  // Clamp within parent client area
  Ctrl.Left := EnsureRange(Ctrl.Left, 0, ParentWidth  - Ctrl.Width);
  Ctrl.Top  := EnsureRange(Ctrl.Top,  0, ParentHeight - Ctrl.Height);
end;


{ Center Child in Parent window but ONLY if Child has 'bad' coordinates }
procedure CenterInvalidChild(Ctrl, Parent: TControl);
begin
 if (Ctrl.Top < -5)
 OR (Ctrl.Left< -5)
 OR (Ctrl.Left> Parent.Width) 
 OR (Ctrl.Top > Parent.ClientHeight) then   { But only if goes out of screen so the user can't find it }
  begin
    Ctrl.Left := (Parent.ClientWidth  - Ctrl.Width) div 2;
    Ctrl.Top  := (Parent.ClientHeight - Ctrl.Height) div 2;
  end;
end;


{ Center Child ctrl in Parent window }
{ToDo: We should take into consideration the controls that are aligned (alLeft, alRight) }
procedure CenterChild(Ctrl, Parent: TControl);
VAR Left, Top: Integer;
begin
 Left:= (Parent.ClientWidth - Ctrl.Width) div 2;

 if Left < 0                                { Happens when the child is bigger than the parent }
 then Ctrl.Left:= 0
 else Ctrl.Left:= Left;

 Top:= (Parent.ClientHeight- Ctrl.Height) div 2;

 if Top < 0                                 { Happens when the child is bigger than the parent }
 then Ctrl.Top:= 0
 else Ctrl.Top:= Top;
end;


{ Center Child ctrl in Parent window, but only on the X axis }
procedure CenterChildX(Ctrl, Parent: TControl);
begin
 Ctrl.Left:= (Parent.ClientWidth - Ctrl.Width) div 2;  {todo: We should take into consideration the controls that are aligned (alleft, alright) }
end;


end.
