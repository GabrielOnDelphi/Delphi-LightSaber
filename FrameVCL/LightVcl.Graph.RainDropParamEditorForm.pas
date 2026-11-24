UNIT LightVcl.Graph.RainDropParamEditorForm;

{=============================================================================================================
   2026.11.22
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Editor dialog for RainDrop animation parameters.
   Allows users to configure water drop effect settings: damping, wave amplitude, travel distance, etc.
   Use TfrmRainEditor.ShowEditorModal(aParams) to display the modal dialog.

   Every control writes into aParams^ at once, so a running effect shows the change while the form is open.
   The bottom bar: OK keeps the changes (ShowEditorModal returns TRUE: the caller saves them).
   Reset puts the RRaindropParams defaults into the controls and into aParams^.
   Cancel, Escape and the X button put back the values aParams^ had when the form opened (ShowEditorModal returns FALSE).
   The buttons have the names and the size of the bottom bar of the BioniX effect editors (btnOk, btnReset, btnCancel, 90 x 28).

   Effect header: the overload ShowEditorModal(aParams, aPicture, aText) shows a picture and a few words that
   explain the effect at the top of the form, which grows by their height. imgEffect and lblEffectInfo are
   hidden in the DFM, so the first overload shows the form without a header.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls,
  LightCore.Graph.RainDropParams;
  {Don't add dependencies to LightVcl.Visual.* because that package is not available yet}

TYPE
  TfrmRainEditor = class(TForm)
    imgEffect     : TImage;
    lblEffectInfo : TLabel;
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
    btnOk         : TButton;
    btnReset      : TButton;
    btnCancel     : TButton;
    procedure trkRainChange   (Sender: TObject);
    procedure btnAdvancedClick(Sender: TObject);
    procedure btnOkClick      (Sender: TObject);
    procedure btnResetClick   (Sender: TObject);
    procedure FormDestroy     (Sender: TObject);
    procedure FormKeyPress    (Sender: TObject; var Key: Char);
    procedure FormClose       (Sender: TObject; var Action: TCloseAction);
    procedure SettingChanged  (Sender: TObject);
  private
    Params: PRaindropParams;
    Initializing: Boolean;     { Prevent OnChange events during initialization }
    procedure ObjectFromGUI;
    procedure showHeader(aPicture: TGraphic; const aText: string);
  public
    procedure GuiFromObject;
    { TRUE after OK. FALSE after Cancel, Escape or the X button: aParams^ then holds again the values it had at open }
    class function ShowEditorModal(aParams: PRaindropParams): Boolean; overload; static;
    { aPicture (may be NIL) and aText (may be '') form the effect header. The form copies the picture; the caller still owns it. }
    class function ShowEditorModal(aParams: PRaindropParams; aPicture: TGraphic; const aText: string): Boolean; overload; static;
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Graph.ResizeWinBlt;



{-------------------------------------------------------------------------------------------------------------
   CTOR/DTOR
-------------------------------------------------------------------------------------------------------------}
class function TfrmRainEditor.ShowEditorModal(aParams: PRaindropParams): Boolean;
begin
  Result:= ShowEditorModal(aParams, NIL, '');
end;


{ The copy is written back here, after ShowModal, and not in the form: the form only reports how it was closed }
class function TfrmRainEditor.ShowEditorModal(aParams: PRaindropParams; aPicture: TGraphic; const aText: string): Boolean;
VAR
  Editor: TfrmRainEditor;
  Saved: RRaindropParams;
begin
  Assert(aParams <> NIL, 'TfrmRainEditor.ShowEditor: aParams cannot be nil');
  Saved:= aParams^;
  Application.CreateForm(TfrmRainEditor, Editor);  { Freed by caFree in FormClose }
  Editor.showHeader(aPicture, aText);

  Editor.Params:= aParams;
  Editor.Initializing:= TRUE;
  TRY
    Editor.GuiFromObject;
  FINALLY
    Editor.Initializing:= FALSE;
  END;

  Result:= Editor.ShowModal = mrOk;
  if NOT Result
  then aParams^:= Saved;
end;


{ Shows the header controls that the DFM hides, and makes the form taller by their height.
  The bounds are set here, because the VCL aligns a control only once the form has a window handle. }
procedure TfrmRainEditor.showHeader(aPicture: TGraphic; const aText: string);
CONST
  MaxPictureHeight = 120;
VAR
  Scale: Double;
  Full, Small: TBitmap;
  Y, Added: Integer;
begin
  Y:= 0;
  Added:= 0;

  { # Picture: scaled down proportionally (HALFTONE), never up }
  if (aPicture <> NIL) AND NOT aPicture.Empty then
   begin
    Scale:= Min(Min((ClientWidth - 20) / aPicture.Width, MaxPictureHeight / aPicture.Height), 1.0);
    Full:= TBitmap.Create;
    TRY
      Full.PixelFormat:= pf24bit;
      Full.SetSize(aPicture.Width, aPicture.Height);
      Full.Canvas.Draw(0, 0, aPicture);
      Small:= StretchF(Full, Max(1, Round(aPicture.Width * Scale)), Max(1, Round(aPicture.Height * Scale)));
      TRY
        imgEffect.Picture.Assign(Small);
        imgEffect.SetBounds(10, 10, ClientWidth - 20, Small.Height);
      FINALLY
        FreeAndNil(Small);
      END;
    FINALLY
      FreeAndNil(Full);
    END;
    imgEffect.Visible:= TRUE;
    Y:= imgEffect.Top + imgEffect.Height;
    Inc(Added, imgEffect.Margins.ControlHeight);
   end;

  { # Text: AutoSize + WordWrap, so the height follows the text }
  if aText <> '' then
   begin
    lblEffectInfo.SetBounds(12, Y + 6, ClientWidth - 24, 15);
    lblEffectInfo.Caption:= aText;
    lblEffectInfo.Visible:= TRUE;
    Inc(Added, lblEffectInfo.Margins.ControlHeight);
   end;

  ClientHeight:= ClientHeight + Added;
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


{ Enter is OK. Escape is Cancel: Close sets ModalResult to mrCancel on a modal form }
procedure TfrmRainEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
   begin
    btnOk.Click;   { TCustomButton.Click sets ModalResult (mrOk in the DFM) before OnClick, so the form closes }
    Key:= #0;
   end;
  if Ord(Key) = VK_ESCAPE then Close;
end;


procedure TfrmRainEditor.btnOkClick(Sender: TObject);
begin
  ObjectFromGUI;
end;


{ The defaults go into the controls and into Params at once, so a running effect shows them. Cancel undoes it.
  The Advanced group stays as the user has it: AdvancedMode only says whether that group is shown }
procedure TfrmRainEditor.btnResetClick(Sender: TObject);
VAR Defaults: RRaindropParams;
begin
  Defaults:= Params^;
  Defaults.Reset;
  Defaults.AdvancedMode:= grpAdvanced.Visible;
  Params^:= Defaults;

  Initializing:= TRUE;
  TRY
    GuiFromObject;
  FINALLY
    Initializing:= FALSE;
  END;
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
