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


  { TFlowLayout subclass that auto-sizes its height to fit wrapped children.
    TFlowLayout does not auto-resize height when children wrap to new rows (FMX limitation).
    This override computes the needed height after each layout pass and sets it.
    Only active when Align is Top/Bottom/MostTop/MostBottom (the layout must be free to change height).
    Usage: drop in FMX file exactly like TFlowLayout. }
  TAutoHeightFlowLayout = class(TFlowLayout)
  private
    FRealigning: Boolean;
  protected
    procedure DoRealign; override;
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


{-------------------------------------------------------------------------------------------------------------
   TAutoHeightFlowLayout
-------------------------------------------------------------------------------------------------------------}
procedure TAutoHeightFlowLayout.DoRealign;
var
  NewH, ChildBottom: Single;
  i: Integer;
begin
  if FRealigning then Exit;
  FRealigning:= True;
  try
    inherited;  { Let TFlowLayout position and wrap all children }

    { Only auto-size when we're in a position where height can change freely.
      If Align=Client or Align=None, changing Height conflicts with the parent layout. }
    if Align in [TAlignLayout.Top, TAlignLayout.Bottom, TAlignLayout.MostTop, TAlignLayout.MostBottom] then
    begin
      NewH:= 0;
      for i:= 0 to ControlsCount - 1 do
        if Controls[i].Visible then
        begin
          ChildBottom:= Controls[i].Position.Y + Controls[i].Height + Controls[i].Margins.Bottom;
          if ChildBottom > NewH
          then NewH:= ChildBottom;
        end;
      NewH:= NewH + Padding.Bottom;

      if (NewH > 0) and (Abs(Height - NewH) > 0.1)
      then Height:= NewH;
    end;
  finally
    FRealigning:= False;
  end;
end;


procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightLayout, TAutoHeightFlowLayout]);
end;


end.
