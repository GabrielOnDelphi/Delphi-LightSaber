UNIT LightVcl.Graph.RainDropParamEditorForm;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Editor dialog for RainDrop animation parameters.
   Allows users to configure water drop effect settings: damping, wave amplitude, travel distance, etc.
   Use TfrmRainEditor.ShowEditor(aParams) to display the modal dialog.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  LightVcl.Graph.RainDropParams;
  {Don't add dependencies to LightVcl.Visual.* because that package is not available yet}

TYPE
  TfrmRainEditor = class(TForm)
    btnAdvanced   : TButton;
    Container     : TPanel;
    GroupBox2     : TGroupBox;
    grpAdvanced   : TGroupBox;
    Label1        : TLabel;
    lblDropInterv : TLabel;
    lblDamping    : TLabel;
    lblFPS        : TLabel;
    lblWaveAmp    : TLabel;
    Panel1        : TPanel;
    Panel2        : TPanel;
    Panel3        : TPanel;
    Panel4        : TPanel;
    Panel5        : TPanel;
    pnlMouseDrops : TPanel;
    chkMouseDrops : TCheckBox;
    lblMouseInterv: TLabel;
    spnMouseInterv: TSpinEdit;
    spnTargetFPS  : TSpinEdit;
    trkDamping    : TTrackBar;
    trkDropInterv : TTrackBar;
    trkRain       : TTrackBar;
    trkTravelDist : TTrackBar;
    trkWaveAmp    : TTrackBar;
    pnlBtm        : TPanel;
    btnClose      : TButton;
    procedure trkRainChange   (Sender: TObject);
    procedure btnAdvancedClick(Sender: TObject);
    procedure FormDestroy     (Sender: TObject);
    procedure FormKeyPress    (Sender: TObject; var Key: Char);
    procedure FormClose       (Sender: TObject; var Action: TCloseAction);
    procedure SettingChanged  (Sender: TObject);
  private
    Params: PRaindropParams;
    Initializing: Boolean;     { Prevent OnChange events during initialization }
    procedure ObjectFromGUI;
  public
    procedure GuiFromObject;
    class procedure ShowEditorModal(aParams: PRaindropParams); static;
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Common.Translate;



{-------------------------------------------------------------------------------------------------------------
   CTOR/DTOR
-------------------------------------------------------------------------------------------------------------}
class procedure TfrmRainEditor.ShowEditorModal(aParams: PRaindropParams);
VAR Editor: TfrmRainEditor;
begin
  Assert(aParams <> NIL, 'TfrmRainEditor.ShowEditor: aParams cannot be nil');
  Application.CreateForm(TfrmRainEditor, Editor);  { Freed by caFree in FormClose }

  Editor.Params:= aParams;
  Editor.Initializing:= TRUE;
  TRY
    Editor.GuiFromObject;
  FINALLY
    Editor.Initializing:= FALSE;
  END;
  Editor.ShowModal;
end;


procedure TfrmRainEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;


procedure TfrmRainEditor.FormDestroy(Sender: TObject);
begin
  assert(Params <> NIL);
  Container.Parent:= Self;    { We need to move the container back on its original form, in order to let that form to correctly save its children }
end;





{-------------------------------------------------------------------------------------------------------------
   GuiFromObject / ObjectFromGUI
   Transfer data between the Params record and the GUI controls.
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRainEditor.GuiFromObject;
begin
  grpAdvanced.Visible    := Params.AdvancedMode;
  spnTargetFPS.Value     := Params.TargetFPS;
  trkWaveAmp.Position    := Params.WaveAplitude;
  trkTravelDist.Position := Params.WaveTravelDist;
  trkDropInterv.Position := Params.DropInterval;
  trkDamping.Position    := Params.Damping;
  chkMouseDrops.Checked  := Params.MouseDrops;
  spnMouseInterv.Value   := Params.MouseDropInterv;
end;


procedure TfrmRainEditor.ObjectFromGUI;
begin
  Params.AdvancedMode    := grpAdvanced.Visible;
  Params.TargetFPS       := spnTargetFPS.Value;
  Params.WaveAplitude    := trkWaveAmp.Position;
  Params.WaveTravelDist  := trkTravelDist.Position;
  Params.DropInterval    := trkDropInterv.Position;
  Params.Damping         := trkDamping.Position;
  Params.MouseDrops      := chkMouseDrops.Checked;
  Params.MouseDropInterv := spnMouseInterv.Value;
end;



{-------------------------------------------------------------------------------------------------------------
   GUI
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRainEditor.SettingChanged(Sender: TObject);  { Apply settings in real-time }
begin
  if NOT Initializing then ObjectFromGUI;
end;


procedure TfrmRainEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then Close;
  if Ord(Key) = VK_ESCAPE then Close;
end;


procedure TfrmRainEditor.btnAdvancedClick(Sender: TObject);
begin
  grpAdvanced.Visible:= NOT grpAdvanced.Visible;
end;


{ Rain intensity presets: adjusts wave amplitude, travel distance, drop interval, and damping based on the selected intensity level (0=Light rain to 4=Flood). }
CONST
  RAIN_LIGHT  = 0;    { Rain intensity presets }    //ToDo: make this a set
  RAIN_NORMAL = 1;
  RAIN_POUR   = 2;
  RAIN_STORM  = 3;
  RAIN_FLOOD  = 4;

procedure TfrmRainEditor.trkRainChange(Sender: TObject);
begin
  if Initializing OR (NOT Visible) then EXIT;  { Prevent preset application during form creation }

  case trkRain.Position of
  RAIN_LIGHT:  { Light rain }
   begin
     trkWaveAmp.Position    := 1;
     trkTravelDist.Position := 100;
     trkDropInterv.Position := 200;
     trkDamping.Position    := 18;
   end;
  RAIN_NORMAL: { Normal rain }
   begin
     trkWaveAmp.Position    := 2;
     trkTravelDist.Position := 400;
     trkDropInterv.Position := 150;
     trkDamping.Position    := 14;
   end;
  RAIN_POUR:   { Heavy rain }
   begin
     trkWaveAmp.Position    := 3;
     trkTravelDist.Position := 700;
     trkDropInterv.Position := 50;
     trkDamping.Position    := 13;
   end;
  RAIN_STORM:  { Storm }
   begin
     trkWaveAmp.Position    := 4;
     trkTravelDist.Position := 1000;
     trkDropInterv.Position := 45;
     trkDamping.Position    := 12;
   end;
  RAIN_FLOOD:  { Flood }
   begin
     trkWaveAmp.Position    := 5;
     trkTravelDist.Position := 1300;
     trkDropInterv.Position := 40;
     trkDamping.Position    := 11;
   end;
  else
    RAISE Exception.Create('Invalid RainDrop intensity: ' + IntToStr(trkRain.Position));
 end;

 SettingChanged(Sender);  { Apply preset immediately }
end;



end.
