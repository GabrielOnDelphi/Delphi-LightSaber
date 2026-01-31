unit LightFmx.Visual.Panel;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
==============================================================================================================

   In FMX, unlike the VCL, the Visible property affects both design time and runtime when set in the Object Inspector.
   That�s why setting Visible:= False in FMX hides components in the Form Designer, which is a big inconvenience.

   Solution:
     In the Form Designer, the component is always visible (Visible := True) so you can edit it,
     regardless of the VisibleAtRuntime value.
     Set VisibleAtRuntime to false to set the Visible to False at runtime (making the component invisible).

   Demo:
     LightSaber\Demo\FMX\Demo LightPanel\FMX_Demo_LightPanel.dpr
=============================================================================================================}

INTERFACE

USES
  System.Classes,
  FMX.Controls, FMX.StdCtrls;

TYPE
  TLightPanel = class(TPanel)
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


constructor TLightPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisibleAtRuntime:= True; // Default: visible at runtime
  // Ensure visibility in designer
  if csDesigning in ComponentState
  then Visible:= True;
end;


procedure TLightPanel.SetVisibleAtRuntime(const Value: Boolean);
begin
  if FVisibleAtRuntime <> Value
  then FVisibleAtRuntime:= Value; // No immediate visibility change here; handled in Loaded
end;


procedure TLightPanel.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState)
  then Visible:= FVisibleAtRuntime; // Apply VisibleAtRuntime setting at runtime only
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLightPanel]);
end;


end.
