UNIT LightFmx.Common.CenterControl;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Functions for centering and positioning controls and forms in FMX.

   Includes:
     - Form positioning (within primary screen or parent form)
     - Control positioning (within parent bounds)

   Note: FMX uses Single (floating-point) for coordinates, unlike VCL's Integer.
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

// Centers a control within its immediate parent. Asserts if the control has no parent.
procedure CenterControl(Ctrl: TControl); overload;

// Centers a control within a rectangle of given dimensions.
procedure CenterControl(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;

// Adjusts a control's Position so that it remains fully visible within its parent container.
procedure EnsureControlVisible(Ctrl: TControl); overload;

// Clamps a control's position ensuring the control never extends outside the given rectangle.
procedure EnsureControlVisible(Ctrl: TControl; const AParentWidth, AParentHeight: Single); overload;


IMPLEMENTATION


{ Centers the given form on the primary screen, so that the form appears in the exact center of the desktop.
  Requires IFMXScreenService to be available. }
procedure CenterFormOnDesktop(Form: TForm);
var
  ScreenSvc: IFMXScreenService;
  ScreenSize: TPointF;
begin
  Assert(Assigned(Form), 'CenterFormOnDesktop: Form parameter cannot be nil');

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  begin
    ScreenSize:= ScreenSvc.GetScreenSize;
    Form.Left:= Round((ScreenSize.X - Form.Width)  / 2);
    Form.Top := Round((ScreenSize.Y - Form.Height) / 2);
  end;
end;



{ Ensures the given form remains entirely within the visible work area of the primary monitor.
  Uses Screen.WorkAreaRect to get the usable desktop area (excludes taskbar). }
procedure EnsureFormVisibleOnScreen(Form: TForm);
var
  WorkArea: TRectF;
begin
  Assert(Assigned(Form), 'EnsureFormVisibleOnScreen: Form parameter cannot be nil');

  WorkArea  := Screen.WorkAreaRect;
  Form.Left:= Round(EnsureRange(Form.Left, WorkArea.Left, WorkArea.Right  - Form.Width));
  Form.Top := Round(EnsureRange(Form.Top,  WorkArea.Top,  WorkArea.Bottom - Form.Height));
end;


{ Centers a child form relative to a parent form's current position and size.
  Ideal for dialog-style forms: calculates offsets based on the parent form's absolute position. }
procedure CenterFormOnParent(Form: TForm; AParent: TForm);
begin
  Assert(Assigned(Form), 'CenterFormOnParent: Form parameter cannot be nil');
  Assert(Assigned(AParent), 'CenterFormOnParent: AParent parameter cannot be nil');

  Form.Left:= Round(AParent.Left + (AParent.Width  - Form.Width)  / 2);
  Form.Top := Round(AParent.Top  + (AParent.Height - Form.Height) / 2);
end;


{ Centers a control within its immediate parent.
  The control must have a parent assigned (asserts if not). }
procedure CenterControl(Ctrl: TControl);
var
  ParentControl: TControl;
begin
  Assert(Assigned(Ctrl), 'CenterControl: Ctrl parameter cannot be nil');
  Assert(Assigned(Ctrl.Parent), 'CenterControl: Control has no parent');
  Assert(Ctrl.Parent is TControl, 'CenterControl: Parent must be a TControl descendant');

  ParentControl:= TControl(Ctrl.Parent);
  CenterControl(Ctrl, ParentControl.Width, ParentControl.Height);
end;


{ Centers a control within a rectangle of given dimensions.
  If the control is larger than the given dimensions, the position may become negative. }
procedure CenterControl(Ctrl: TControl; const AParentWidth, AParentHeight: Single);
begin
  Assert(Assigned(Ctrl), 'CenterControl: Ctrl parameter cannot be nil');

  Ctrl.Position.X:= (AParentWidth  - Ctrl.Width) / 2;
  Ctrl.Position.Y:= (AParentHeight - Ctrl.Height) / 2;
end;



{ Adjusts a control's position so that it remains fully visible within its parent container.
  The control must have a parent assigned (asserts if not). }
procedure EnsureControlVisible(Ctrl: TControl);
begin
  Assert(Assigned(Ctrl), 'EnsureControlVisible: Ctrl parameter cannot be nil');
  Assert(Assigned(Ctrl.Parent), 'EnsureControlVisible: Control has no parent');
  Assert(Ctrl.Parent is TControl, 'EnsureControlVisible: Parent must be a TControl descendant');

  EnsureControlVisible(Ctrl, TControl(Ctrl.Parent).Width, TControl(Ctrl.Parent).Height);
end;



{ Clamps a control's position ensuring the control never extends outside the given rectangle.
  Note: If the control is larger than the parent dimensions, position will be clamped to 0. }
procedure EnsureControlVisible(Ctrl: TControl; const AParentWidth, AParentHeight: Single);
begin
  Assert(Assigned(Ctrl), 'EnsureControlVisible: Ctrl parameter cannot be nil');

  Ctrl.Position.X:= EnsureRange(Ctrl.Position.X, 0, Max(0, AParentWidth  - Ctrl.Width));
  Ctrl.Position.Y:= EnsureRange(Ctrl.Position.Y, 0, Max(0, AParentHeight - Ctrl.Height));
end;


end.
