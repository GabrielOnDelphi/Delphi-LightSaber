unit LightFmx.Visual.Layout;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
==============================================================================================================

   In FMX, unlike VCL, the Visible property affects both design time and runtime when set in the Object Inspector.
   That's why setting Visible := False in FMX hides components in the Form Designer, which is inconvenient.

   Solution:
     In the Form Designer, the component is always visible (Visible := True) so you can edit it,
     regardless of the VisibleAtRuntime value.
     Set VisibleAtRuntime to False to hide the component at runtime (Visible will be set to False).

=============================================================================================================}

INTERFACE

USES
  System.Classes,
  FMX.Controls, FMX.Layouts, FMX.Types;

TYPE
  { Invisible layout that fires a callback when its parent resizes.
    Needed because embedded forms don't receive OnResize — the Container is reparented away from the form.
    Usage: TResizeSensor.CreateSensor(Container, MyResizeHandler); }
  TResizeSensor = class(TLayout)
  private
    FOnResized: TNotifyEvent;
  protected
    procedure Resize; override;
  public
    constructor CreateSensor(aParent: TFmxObject; aOnResized: TNotifyEvent);
  end;


  TLightLayout = class(TLayout)
  private
    FVisibleAtRuntime: Boolean;
    procedure SetVisibleAtRuntime(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadVisibleAtRuntime(Reader: TReader); // Custom streaming method
  public
    constructor Create(AOwner: TComponent); override;
  published
    property VisibleAtRuntime: Boolean read FVisibleAtRuntime write SetVisibleAtRuntime default True;
  end;

procedure Register;

IMPLEMENTATION


constructor TLightLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisibleAtRuntime:= True;  // Default: visible at runtime

  // Ensure visibility in designer
  if csDesigning in ComponentState then Visible:= True;
end;


procedure TLightLayout.SetVisibleAtRuntime(const Value: Boolean);
begin
  if FVisibleAtRuntime <> Value
  then FVisibleAtRuntime:= Value;  // No immediate visibility change; applied in Loaded
end;


{ Legacy streaming support - may be called when reading older FMX files }
procedure TLightLayout.ReadVisibleAtRuntime(Reader: TReader);
begin
  FVisibleAtRuntime:= Reader.ReadBoolean;
end;


{ Note: The published property handles standard streaming. This custom reader
  provides backwards compatibility for older FMX files that may have used
  a different streaming mechanism. WriteProc is nil because writing is
  handled by the published property's default streaming. }
procedure TLightLayout.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('VisibleAtRuntime', ReadVisibleAtRuntime, NIL, not FVisibleAtRuntime);
end;


procedure TLightLayout.Loaded;
begin
  inherited;
  // Apply VisibleAtRuntime only at runtime, not in designer
  if not (csDesigning in ComponentState)
  then Visible:= FVisibleAtRuntime;
end;







{-------------------------------------------------------------------------------------------------------------
   TResizeSensor
-------------------------------------------------------------------------------------------------------------}
constructor TResizeSensor.CreateSensor(aParent: TFmxObject; aOnResized: TNotifyEvent);
begin
  inherited Create(aParent as TComponent);
  FOnResized:= aOnResized;
  Parent:= aParent;
  Align:= TAlignLayout.Contents;  // Matches parent bounds without affecting siblings
  HitTest:= FALSE;
end;

procedure TResizeSensor.Resize;
begin
  inherited;
  if Assigned(FOnResized)
  then FOnResized(Self);
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightLayout]);
end;


end.
