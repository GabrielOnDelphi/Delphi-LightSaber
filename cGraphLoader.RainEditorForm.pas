UNIT cGraphLoader.RainEditorForm;

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  cGraphLoader.RainDrop;

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
  cTranslate, cbINIFile, cbAppData;
   {Don't add dependencies to cvIniFile because that package is not available yet}



{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}

{ Show the editor }
class procedure TfrmRainEditor.ShowEditor(aParams: PRaindropParams);
VAR Form: TfrmRainEditor;
begin
 TAppData.RaiseIfStillInitializing;
 Assert(aParams <> NIL);

 AppData.CreateFormHidden(TfrmRainEditor, Form);  { Freed by ShowModal }
 WITH Form DO
 begin
   if Translator <> NIL
   then Translator.LoadFormTranlation(Form);
   Params:= aParams;
   GuiFromObject;
 end;

 Form.ShowModal;    { Closed by mrOk/mrCancel }
end;


procedure TfrmRainEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmRainEditor.FormDestroy(Sender: TObject);
begin
  Container.Parent:= Self;    { We need to move the container back on its original form, in order to let that form to correctly save its children }
  SaveFormBase(Self);         { Don't add dependencies to cvIniFile because that package is not available yet! }
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










procedure TfrmRainEditor.GuiFromObject;
begin
 spnTargetFPS.Value     := Params.TargetFPS;
 trkWaveAmp.Position    := Params.WaveAplitude;
 trkTravelDist.Position := Params.WaveTravelDist;
 trkDropInterv.Position := Params.DropInterval;
 trkDamping.Position    := Params.Damping;
end;


procedure TfrmRainEditor.ObjectFromGUI;
begin
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


procedure TfrmRainEditor.trkRainChange(Sender: TObject);
begin
 case trkRain.Position of
  0:
   begin
     //Light rain'
     trkWaveAmp.Position    := 1;
     trkTravelDist.Position := 100;
     trkDropInterv.Position := 200;
     trkDamping.Position    := 18;
   end;
  1:
   begin
     //Hmm... It's raining
     trkWaveAmp.Position    := 2;
     trkTravelDist.Position := 400;
     trkDropInterv.Position := 150;
     trkDamping.Position    := 14;
   end;
  2:
   begin
     //Let it pour
     trkWaveAmp.Position    := 3;
     trkTravelDist.Position := 700;
     trkDropInterv.Position := 50;
     trkDamping.Position    := 13;
   end;
  3:
   begin
     //Big storm
     trkWaveAmp.Position    := 4;
     trkTravelDist.Position := 1000;
     trkDropInterv.Position := 45;
     trkDamping.Position    := 12;
   end;
  4:
   begin
     //The flood
     trkWaveAmp.Position    := 5;
     trkTravelDist.Position := 1300;
     trkDropInterv.Position := 40;
     trkDamping.Position    := 11;
   end;
  else
    RAISE Exception.Create('Invalid RainDrop force parameter!');
 end;
end;


end.
