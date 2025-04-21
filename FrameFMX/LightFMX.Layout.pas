unit LightFMX.Layout;

{=============================================================================================================
   2025.04
   www.GabrielMoraru.com
==============================================================================================================

   In FMX, unlike the VCL, the Visible property affects both design time and runtime when set in the Object Inspector.
   That�s why setting Visible:= False in FMX hides components in the Form Designer, which is a big inconvenience.

   Solution:
     In the Form Designer, the component is always visible (Visible := True) so you can edit it,
     regardless of the VisibleAtRuntime value.
     Set VisibleAtRuntime to false to set the Visible to False at runtime (making the component invisible).

=============================================================================================================}

INTERFACE

USES
  System.Classes,
  FMX.Controls, FMX.Layouts, FMX.StdCtrls;

TYPE
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
  FVisibleAtRuntime := True; // Default: visible at runtime
  // Ensure visibility in designer
  if csDesigning in ComponentState
  then Visible := True;
end;


procedure TLightLayout.SetVisibleAtRuntime(const Value: Boolean);
begin
  if FVisibleAtRuntime <> Value
  then FVisibleAtRuntime := Value; // No immediate visibility change here; handled in Loaded
end;


procedure TLightLayout.ReadVisibleAtRuntime(Reader: TReader);
begin
  FVisibleAtRuntime := Reader.ReadBoolean;
end;


procedure TLightLayout.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('VisibleAtRuntime', ReadVisibleAtRuntime, nil, not FVisibleAtRuntime);
end;


procedure TLightLayout.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState)
  then Visible := FVisibleAtRuntime; // Set visibility based on VisibleAtRuntime at runtime
end;







procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightLayout]);
end;


end.