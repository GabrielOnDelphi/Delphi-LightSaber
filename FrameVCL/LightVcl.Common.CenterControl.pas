UNIT LightVcl.Common.CenterControl;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Functions for centering and positioning controls and forms.

   Includes:
     - Form positioning (within desktop, monitor, or parent)
     - Control positioning (within parent bounds)
     - Centering controls/forms (horizontally, vertically, or both)
     - MDI child window positioning

   Key concepts:
     Screen.WorkArea      -> Specifies the work area on the Primary monitor (excludes taskbar).
     Screen.DesktopWidth  -> Width of the entire virtual desktop (all monitors combined).
                             On a single-monitor system, DesktopWidth equals Width.
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Types, System.Math,
   Vcl.Controls, Vcl.Forms;

{=============================================================================================================
   FORM POSITIONING
=============================================================================================================}

{ Ensures the form is fully visible on the whole desktop }
procedure CorrectFormPositionDesktop(Form: TForm);

{ Ensures the form is fully visible on its primary monitor }
procedure CorrectFormPositionMainMonitor(Form: TForm);                deprecated 'Use Form.MakeFullyVisible(Screen.PrimaryMonitor)';

{ Ensures the form is fully visible on the specified monitor }
procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor); deprecated 'Use Form.MakeFullyVisible(Monitor)';

{ Center form on primary monitor (client work area) }
procedure CenterForm(Form: TForm); overload;

{ Center form within a parent form }
procedure CenterForm(Form, Parent: TForm); overload;

{ Center form on the monitor where the main form is located }
procedure CenterFormOnMainFormMonitor(Form: TForm);

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
procedure CorrectFormPositionDesktop(Form: TForm);
begin
 if Form = NIL
 then raise Exception.Create('CorrectFormPositionDesktop: Form parameter cannot be nil');

 CorrectCtrlPosition(Form, Screen.DesktopWidth, Screen.DesktopHeight);
end;


{ Brings the form back into the screen, if it was outside the screen.
  Called automatically by LoadForm(). }
procedure CorrectFormPositionMainMonitor(Form: TForm);
begin
  if Form = NIL
  then raise Exception.Create('CorrectFormPositionMainMonitor: Form parameter cannot be nil');

  Form.MakeFullyVisible(Screen.PrimaryMonitor);
end;


{ Ensures the specified form fits within the specified monitor. }
procedure CorrectFormPositionMonitor(Form: TForm; Monitor: TMonitor);
begin
  if Form = NIL
  then raise Exception.Create('CorrectFormPositionMonitor: Form parameter cannot be nil');
  if Monitor = NIL
  then raise Exception.Create('CorrectFormPositionMonitor: Monitor parameter cannot be nil');

  Form.MakeFullyVisible(Monitor);
end;


{ Centers the form within the primary monitor work area (excludes taskbar). }
procedure CenterForm(Form: TForm);
var
  WorkArea: TRect;
begin
  if Form = NIL
  then raise Exception.Create('CenterForm: Form parameter cannot be nil');

  WorkArea := Screen.PrimaryMonitor.WorkareaRect;
  Form.Left := WorkArea.Left + ((WorkArea.Width  - Form.Width) div 2);
  Form.Top  := WorkArea.Top  + ((WorkArea.Height - Form.Height) div 2);
end;


{ Centers the form within the parent form's client area. }
procedure CenterForm(Form, Parent: TForm);
begin
  if Form = NIL
  then raise Exception.Create('CenterForm: Form parameter cannot be nil');
  if Parent = NIL
  then raise Exception.Create('CenterForm: Parent parameter cannot be nil');

  Form.Left := Parent.Left + ((Parent.ClientWidth  - Form.Width) div 2);
  Form.Top  := Parent.Top  + ((Parent.ClientHeight - Form.Height) div 2);
end;


{ Centers the form on the monitor where Application.MainForm is located.
  Falls back to primary monitor if MainForm is not available. }
procedure CenterFormOnMainFormMonitor(Form: TForm);
VAR
  Monitor: TMonitor;
  WorkArea: TRect;
begin
  if Form = NIL
  then raise Exception.Create('CenterFormOnMainFormMonitor: Form parameter cannot be nil');

  if Application.MainForm <> NIL
  then Monitor:= Application.MainForm.Monitor
  else Monitor:= Screen.PrimaryMonitor;

  WorkArea:= Monitor.WorkareaRect;
  Form.Left:= WorkArea.Left + ((WorkArea.Width  - Form.Width) div 2);
  Form.Top := WorkArea.Top  + ((WorkArea.Height - Form.Height) div 2);
end;




{=============================================================================================================
   MDI
=============================================================================================================}

{ Iterates through all MDI child forms of ParentForm and ensures their position/size
  stays within the parent's client area. }
procedure CorrectMDIFormPosition(ParentForm: TForm);
var
  i: Integer;
  Child: TForm;
begin
  if ParentForm = NIL
  then raise Exception.Create('CorrectMDIFormPosition: ParentForm parameter cannot be nil');

  for i := 0 to ParentForm.MDIChildCount - 1 do
  begin
    Child := ParentForm.MDIChildren[i];
    { Clamp Height and Top }
    Child.Height := Min(Child.Height, ParentForm.ClientHeight);
    Child.Top    := EnsureRange(Child.Top, 0, ParentForm.ClientHeight - Child.Height);
    { Clamp Width and Left }
    Child.Width  := Min(Child.Width, ParentForm.ClientWidth);
    Child.Left   := EnsureRange(Child.Left, 0, ParentForm.ClientWidth - Child.Width);
  end;
end;





{=============================================================================================================
   CENTER CTRL
=============================================================================================================}

{ Adjusts the control's position/size so it stays within the parent's client area.

  Note: The Top/Left are relative to parent's client area.
  If the Parent has a toolbar/panel aligned to its top, the Child's top will be 0
  when positioned immediately below that panel, not the panel's height. }
procedure CorrectCtrlPosition(Ctrl, Parent: TControl);
begin
  if Ctrl = NIL
  then raise Exception.Create('CorrectCtrlPosition: Ctrl parameter cannot be nil');
  if Parent = NIL
  then raise Exception.Create('CorrectCtrlPosition: Parent parameter cannot be nil');

  CorrectCtrlPosition(Ctrl, Parent.ClientWidth, Parent.ClientHeight);
end;


{ Adjusts the control's position/size so it stays within the specified bounds. }
procedure CorrectCtrlPosition(Ctrl: TControl; const ParentWidth, ParentHeight: Integer);
begin
  if Ctrl = NIL
  then raise Exception.Create('CorrectCtrlPosition: Ctrl parameter cannot be nil');

  { Ensure control does not exceed parent size }
  Ctrl.Width  := Min(Ctrl.Width, ParentWidth);
  Ctrl.Height := Min(Ctrl.Height, ParentHeight);

  { Clamp within parent client area }
  Ctrl.Left := EnsureRange(Ctrl.Left, 0, ParentWidth  - Ctrl.Width);
  Ctrl.Top  := EnsureRange(Ctrl.Top,  0, ParentHeight - Ctrl.Height);
end;


{ Centers the child in the parent window, but ONLY if the child has invalid/out-of-bounds coordinates.
  A small tolerance of -5 pixels is allowed before triggering centering. }
procedure CenterInvalidChild(Ctrl, Parent: TControl);
begin
 if Ctrl = NIL
 then raise Exception.Create('CenterInvalidChild: Ctrl parameter cannot be nil');
 if Parent = NIL
 then raise Exception.Create('CenterInvalidChild: Parent parameter cannot be nil');

 if (Ctrl.Top < -5)
 OR (Ctrl.Left < -5)
 OR (Ctrl.Left > Parent.Width)
 OR (Ctrl.Top > Parent.ClientHeight) then
  begin
    Ctrl.Left := (Parent.ClientWidth  - Ctrl.Width) div 2;
    Ctrl.Top  := (Parent.ClientHeight - Ctrl.Height) div 2;
  end;
end;


{ Centers the child control within the parent's client area.
  If the child is larger than the parent, positions it at 0.
  Note: Does not account for controls aligned with alLeft, alRight, etc. }
procedure CenterChild(Ctrl, Parent: TControl);
VAR NewLeft, NewTop: Integer;
begin
 if Ctrl = NIL
 then raise Exception.Create('CenterChild: Ctrl parameter cannot be nil');
 if Parent = NIL
 then raise Exception.Create('CenterChild: Parent parameter cannot be nil');

 NewLeft:= (Parent.ClientWidth - Ctrl.Width) div 2;
 if NewLeft < 0                             { Child is wider than parent }
 then Ctrl.Left:= 0
 else Ctrl.Left:= NewLeft;

 NewTop:= (Parent.ClientHeight - Ctrl.Height) div 2;
 if NewTop < 0                              { Child is taller than parent }
 then Ctrl.Top:= 0
 else Ctrl.Top:= NewTop;
end;


{ Centers the child control horizontally within the parent's client area.
  If the child is wider than the parent, positions it at 0.
  Note: Does not account for controls aligned with alLeft, alRight, etc. }
procedure CenterChildX(Ctrl, Parent: TControl);
VAR NewLeft: Integer;
begin
 if Ctrl = NIL
 then raise Exception.Create('CenterChildX: Ctrl parameter cannot be nil');
 if Parent = NIL
 then raise Exception.Create('CenterChildX: Parent parameter cannot be nil');

 NewLeft:= (Parent.ClientWidth - Ctrl.Width) div 2;
 if NewLeft < 0                             { Child is wider than parent }
 then Ctrl.Left:= 0
 else Ctrl.Left:= NewLeft;
end;


end.
