UNIT LightVcl.Graph.Loader.RainEditorForm;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Editor dialog for RainDrop animation parameters.
   Allows users to configure water drop effect settings: damping, wave amplitude, travel distance, etc.
   Use TfrmRainEditor.ShowEditor(aParams) to display the modal dialog.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  LightVcl.Graph.Loader.RainDrop;

TYPE
  TfrmRainEditor = class(TLightForm)
    btnAdvanced: TButton;
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
    spnTargetFPS  : TSpinEdit;
    trkDamping    : TTrackBar;
    trkDropInterv : TTrackBar;
    trkRain       : TTrackBar;
    trkTravelDist : TTrackBar;
    trkWaveAmp    : TTrackBar;
    pnlBtm        : TPanel;
    btnOK         : TButton;
    btnCancel     : TButton;
    btnApply: TButton;
    procedure trkRainChange   (Sender: TObject);
    procedure btnOKClick      (Sender: TObject);
    procedure FormDestroy     (Sender: TObject);
    procedure FormKeyPress    (Sender: TObject; var Key: Char);
    procedure btnAdvancedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Params: PRaindropParams;
    procedure ObjectFromGUI;
  public
    procedure GuiFromObject;
    class procedure ShowEditor(aParams: PRaindropParams); static;
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Common.Translate, LightVcl.Common.IniFile, LightCore.AppData, LightVcl.Visual.AppData;
   {Don't add dependencies to LightVcl.Visual.INIFile because that package is not available yet}



{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

{---------------------------------------------------------------------------------------------------------------
   ShowEditor
   Displays the RainDrop parameter editor as a modal dialog.
   aParams: Pointer to the RRaindropParams record to edit. Must not be NIL.
   The dialog modifies the parameters in-place when OK is clicked.
---------------------------------------------------------------------------------------------------------------}
class procedure TfrmRainEditor.ShowEditor(aParams: PRaindropParams);
VAR Form: TfrmRainEditor;
begin
 TAppData.RaiseIfStillInitializing;
 Assert(aParams <> NIL, 'TfrmRainEditor.ShowEditor: aParams cannot be nil');

 AppData.CreateFormHidden(TfrmRainEditor, Form);  { Freed by caFree in FormClose }

 if Translator <> NIL
 then Translator.LoadTranslation(Form);

 Form.Params:= aParams;
 Form.GuiFromObject;
 Form.ShowModal;    { Closed by mrOk/mrCancel }
end;


procedure TfrmRainEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmRainEditor.FormDestroy(Sender: TObject);
begin
  Container.Parent:= Self;    { We need to move the container back on its original form, in order to let that form to correctly save its children }
  //done automatically: SaveFormBase(Self);         { Don't add dependencies to LightVcl.Visual.INIFile because that package is not available yet! }
end;


procedure TfrmRainEditor.btnOKClick(Sender: TObject);
begin
 Assert(Params <> NIL);
 ObjectFromGUI;
end;


procedure TfrmRainEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Ord(Key) = VK_RETURN then btnOkClick(Sender);
 if Ord(Key) = VK_ESCAPE then Close;
end;










{---------------------------------------------------------------------------------------------------------------
   GuiFromObject / ObjectFromGUI
   Transfer data between the Params record and the GUI controls.
---------------------------------------------------------------------------------------------------------------}
procedure TfrmRainEditor.GuiFromObject;
begin
 Assert(Params <> NIL, 'TfrmRainEditor.GuiFromObject: Params not initialized');
 spnTargetFPS.Value     := Params.TargetFPS;
 trkWaveAmp.Position    := Params.WaveAplitude;
 trkTravelDist.Position := Params.WaveTravelDist;
 trkDropInterv.Position := Params.DropInterval;
 trkDamping.Position    := Params.Damping;
end;


procedure TfrmRainEditor.ObjectFromGUI;
begin
 Assert(Params <> NIL, 'TfrmRainEditor.ObjectFromGUI: Params not initialized');
 Params.TargetFPS      := spnTargetFPS.Value;
 Params.WaveAplitude   := trkWaveAmp.Position;
 Params.WaveTravelDist := trkTravelDist.Position;
 Params.DropInterval   := trkDropInterv.Position;
 Params.Damping        := trkDamping.Position;
end;










procedure TfrmRainEditor.btnAdvancedClick(Sender: TObject);
begin
 grpAdvanced.Visible:= NOT grpAdvanced.Visible;
end;


{---------------------------------------------------------------------------------------------------------------
   trkRainChange
   Rain intensity presets: adjusts wave amplitude, travel distance, drop interval, and damping
   based on the selected intensity level (0=Light rain to 4=Flood).
---------------------------------------------------------------------------------------------------------------}
CONST
  { Rain intensity preset indices }
  RAIN_LIGHT  = 0;
  RAIN_NORMAL = 1;
  RAIN_POUR   = 2;
  RAIN_STORM  = 3;
  RAIN_FLOOD  = 4;

procedure TfrmRainEditor.trkRainChange(Sender: TObject);
begin
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
end;


end.
