UNIT LightFMX.lbCenterControl;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================
   Center a control on form
   Center a form on screen
=============================================================================================================}

INTERFACE

USES
  System.Math,
  System.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Types,
  FMX.Platform;


procedure CenterFormOnDesktop(Form: TForm);

// Ensures the given form remains entirely within the visible work area of the primary monitor.
procedure EnsureFormVisibleOnScreen(Form: TForm);

// Centers a child form relative to a parent form's current position and size. Ideal for dialog-style forms: it calculates offsets based on the parent form's absolute Position and dimensions.
procedure CenterFormOnParent(Form: TForm; AParent: TForm);

// Centers a control within its immediate parent. If the control has no parent, this call does nothing. Internally calls the overload passing width/height.
procedure CenterControl(Ctrl: TControl); overload;

// Centers a control within a rectangle of given dimensions.
procedure CenterControl(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;

// Adjusts a control's Position so that it remains fully visible within its parent container.
procedure EnsureControlVisible(Ctrl: TControl); overload;

// Clamps a control's position ensuring the control never extends outside the given rectangle.
procedure EnsureControlVisible(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;


IMPLEMENTATION


{ Centers the given form on the primary screen, so that the form appears in the exact center of the desktop.}
procedure CenterFormOnDesktop(Form: TForm);
var
  ScreenSvc: IFMXScreenService;
  ScreenSize: TPointF;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  begin
    ScreenSize := ScreenSvc.GetScreenSize;
    Form.Left := round((ScreenSize.X - Form.Width)  / 2);
    Form.Top  := round((ScreenSize.Y - Form.Height) / 2);
  end;
end;



{------------------------------------------------------------------------------}
procedure EnsureFormVisibleOnScreen(Form: TForm);
var
  WorkArea: TRectF;
begin
  // Use Screen.WorkAreaRect to get the usable desktop area
  WorkArea  := Screen.WorkAreaRect;
  Form.Left := round(EnsureRange(Form.Left, WorkArea.Left,  WorkArea.Right  - Form.Width));
  Form.Top  := round(EnsureRange(Form.Top , WorkArea.Top,   WorkArea.Bottom - Form.Height));
end;


{------------------------------------------------------------------------------}
procedure CenterFormOnParent(Form: TForm; AParent: TForm);
begin
  Form.Left:= round(AParent.Left + (AParent.Width  - Form.Width)  / 2);
  Form.Top := round(AParent.Top  + (AParent.Height - Form.Height) / 2);
end;


{------------------------------------------------------------------------------}
procedure CenterControl(Ctrl: TControl);
var
  ParentControl: TControl;
begin
  Assert(Assigned(Ctrl.Parent));
  Assert(Ctrl.Parent is TControl);

  ParentControl := TControl(Ctrl.Parent);
  CenterControl(Ctrl, ParentControl.Width, ParentControl.Height);
end;


{------------------------------------------------------------------------------}
procedure CenterControl(Ctrl: TControl; const AParentWidth, AParentHeight: Single);
begin
  Ctrl.Position.X := (AParentWidth  - Ctrl.Width) / 2;
  Ctrl.Position.Y := (AParentHeight - Ctrl.Height) / 2;
end;



{------------------------------------------------------------------------------}
procedure EnsureControlVisible(Ctrl: TControl);
begin
  Assert(Assigned(Ctrl.Parent));
  Assert(Ctrl.Parent is TControl);

  EnsureControlVisible(Ctrl, TControl(Ctrl.Parent).Width, TControl(Ctrl.Parent).Height);
end;



{------------------------------------------------------------------------------}
procedure EnsureControlVisible(Ctrl: TControl; const AParentWidth, AParentHeight: Single);
begin
  Ctrl.Position.X := EnsureRange(Ctrl.Position.X, 0, AParentWidth  - Ctrl.Width);
  Ctrl.Position.Y := EnsureRange(Ctrl.Position.Y, 0, AParentHeight - Ctrl.Height);
end;


end.
