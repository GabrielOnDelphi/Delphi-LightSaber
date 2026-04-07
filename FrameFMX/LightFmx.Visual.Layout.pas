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
    destructor Destroy; override;
  end;


  TLightLayout = class(TLayout)
  private
    FVisibleAtRuntime: Boolean;
    procedure SetVisibleAtRuntime(const Value: Boolean);
  protected
    procedure Loaded; override;
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
  if FVisibleAtRuntime = Value then Exit;
  FVisibleAtRuntime:= Value;
  if not (csLoading in ComponentState) AND not (csDesigning in ComponentState)
  then Visible:= Value;
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

destructor TResizeSensor.Destroy;
begin
  FOnResized:= nil;  // Clear callback to prevent stale invocation during partial teardown
  inherited;
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
