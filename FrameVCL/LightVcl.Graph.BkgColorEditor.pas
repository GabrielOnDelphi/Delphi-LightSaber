UNIT LightVcl.Graph.BkgColorEditor;

{=============================================================================================================
   Gabriel Moraru
   2026.01.31
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Visual properties editor for RBkgColorParams (background color/fill effects).
   Provides UI controls for configuring fill type, effect shape, color detection mode, and fade parameters.

   DON'T ADD IT TO ANY DPK or Don't add dependencies to VisualControls.bpl because that lib is compiled after this one.

   How to use it:
      Auto create form:
        frmBorderEditor.GuiFromRec(BorderSettings);
        frmBorderEditor.Show;

      Or use CreateParented for embedding in another control:
        Editor:= TfrmBorderEditor.CreateParented(ParentPanel, @BkgSettings);

   Related files:
      LightVcl.Graph.BkgColorParams.pas - The parameter record this editor modifies
      LightVcl.Graph.BkgColor.pas       - Applies the border fading effects to images
--------------------------------------------------------------------------------------------------}
//todo 1: "Background shape": automatically use "rectangle" or "tringle" based on image size. If the image touches with two edges the border of the monitor then use "rectangle". if the image does not touch the border then use "tringle".
//todo 4: measure time in LightVcl.Graph.FX.Border.pas. FadeBorder
//todo 5: an alternative to bkg color is to stretch the whole image to fit the monitor, blur it heavily and then use it as bkg color.
//todo 5: why it takes so long? marmari 022.jpg.      make sure I apply it AFTER I downsize the image! I do it:  Monitor.pas -> FadeBorderAuto(Result, OutputRegion, BorderSettings^);

//done: Make "automatic bkg color settings" window non-modal.
//done: perform ALL expensive op after I resize the image:   { Rotate Manually  FLIP COLOR ENHANCEMENT }

INTERFACE
USES
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  LightVcl.Graph.BkgColorParams;

TYPE
  TfrmBorderEditor = class(TForm)
    btnApply          : TButton;
    btnBackgroundClr  : TButton;
    btnCancel         : TButton;
    btnOk             : TButton;
    btnReset          : TButton;
    btnAdvanced       : TButton;
    ColorDialog       : TColorDialog;
    Container         : TPanel;
    grpBkgFill        : TGroupBox;
    grpFadeParams     : TGroupBox;
    grpFillFade       : TGroupBox;
    grpShape          : TGroupBox;
    lblHint           : TLabel;
    lblEdgeSmear      : TLabel;
    lblExplain        : TLabel;
    lblFadeSpeed      : TLabel;
    lblFuzzy          : TLabel;
    lblNeighborWeight : TLabel;
    lblPixTolerance   : TLabel;
    lblWarning        : TLabel;
    pnlBottom         : TPanel;
    pnlExplain        : TPanel;
    radAutoDetBorder  : TRadioButton;
    radFade           : TRadioButton;
    radFill           : TRadioButton;
    radImageAverage   : TRadioButton;
    radShapeRect      : TRadioButton;
    radShapeSolid     : TRadioButton;
    radShapeTraing    : TRadioButton;
    radUserColor      : TRadioButton;
    spnEdgeSmear      : TSpinEdit;
    spnFallSpeed      : TSpinEdit;
    spnNeighborDistance: TSpinEdit;
    spnNeighborWeight : TSpinEdit;
    spnPixTolerance   : TSpinEdit;
    procedure FormDestroy           (Sender: TObject);
    procedure btnBackgroundClrClick (Sender: TObject);
    procedure SettingsChanged       (Sender: TObject);
    procedure btnResetClick         (Sender: TObject);
    procedure lblExplainClick       (Sender: TObject);
    procedure btnApplyClick         (Sender: TObject);
    procedure FormCreate            (Sender: TObject);
    procedure btnOkClick            (Sender: TObject);
    procedure btnCancelClick        (Sender: TObject);
    procedure btnAdvancedClick      (Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    BkgClrParams: PBkgColorParams;
    FApplySettings: TNotifyEvent;
  public
    function GetEffectColor: TEffectColor;
    function GetFillType: TFillType;
    function GetEffectShape: TEffectShape;
    class function CreateParented(Parent: TWinControl; aBkgClrParams: PBkgColorParams): TfrmBorderEditor;

    procedure ObjectFromGUI;
    procedure GuiFromObject;

    property OnApplySettings: TNotifyEvent read FApplySettings write FApplySettings;    // let user apply the wallpaper so he can see how it looks like with the new settings right away!
  end;




IMPLEMENTATION  {$R *.dfm}

USES
  LightVcl.Graph.Util, LightVcl.Common.VclUtils, LightCore.INIFileQuick, LightVcl.Common.CenterControl, LightVcl.Common.Dialogs;
  { Don't use LightVcl.Visual.INIFile because it belongs to LightVisControls pkg which is after this (LightVclGraphics) package }



{ Creates the editor form and embeds its Container panel into the specified Parent control.
  The form itself is not visible - only the Container panel is shown.
  Call GuiFromObject after creation to populate controls from aBkgClrParams.
  The form is freed when closed (caFree in FormClose). }
class function TfrmBorderEditor.CreateParented(Parent: TWinControl; aBkgClrParams: PBkgColorParams): TfrmBorderEditor;
begin
 if Parent = NIL
 then raise Exception.Create('TfrmBorderEditor.CreateParented: Parent parameter cannot be nil');
 if aBkgClrParams = NIL
 then raise Exception.Create('TfrmBorderEditor.CreateParented: aBkgClrParams parameter cannot be nil');

 Application.CreateForm(TfrmBorderEditor, Result);
 Result.Container.Align:= alNone;
 Result.Container.Parent:= Parent;
 CenterChild(Result.Container, Parent);
 Result.BkgClrParams:= aBkgClrParams;
 Result.Container.Show;
end;






procedure TfrmBorderEditor.FormCreate(Sender: TObject);
begin
  CenterChild(pnlExplain, Self);
  pnlExplain.Visible:= ReadBoolean('AutoBkg.ShowInfo', TRUE);  { Show the 'info' label to the user only once }
  lblExplain.Transparent:= VclStylesEnabled;
  lblHint   .Transparent:= VclStylesEnabled;
  //LoadForm(Self, TRUE);   { Don't use LightVcl.Visual.INIFile because it belongs to LightVisControls pkg which is after this (LightVclGraphics) package }
end;


procedure TfrmBorderEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;


procedure TfrmBorderEditor.FormDestroy(Sender: TObject);
begin
  Container.Parent:= Self;
  WriteBool('AutoBkg.ShowInfo', pnlExplain.Visible);
  //SaveForm(Self, TRUE);  { Don't use LightVcl.Visual.INIFile because it belongs to LightVisControls pkg which is after this (LightVclGraphics) package }
end;










{--------------------------------------------------------------------------------------------------
  GET/SET SETTINGS
--------------------------------------------------------------------------------------------------}

{ Copies all GUI control values to the BkgClrParams record.
  Call this to save user changes before closing the form. }
procedure TfrmBorderEditor.ObjectFromGUI;
begin
 if BkgClrParams = NIL
 then raise Exception.Create('TfrmBorderEditor.ObjectFromGUI: BkgClrParams is not initialized');

 BkgClrParams.FillType      := GetFillType;
 BkgClrParams.EffectShape   := GetEffectShape;
 BkgClrParams.EffectColor   := GetEffectColor;
 BkgClrParams.Color         := ColorDialog.Color;
 BkgClrParams.FadeSpeed     := spnFallSpeed.Value;
 BkgClrParams.EdgeSmear     := spnEdgeSmear.Value;
 BkgClrParams.NeighborWeight:= spnNeighborWeight.Value;
 BkgClrParams.NeighborDist  := spnNeighborDistance.Value;
 BkgClrParams.Tolerance     := spnPixTolerance.Value;
end;


{ Populates all GUI controls from the BkgClrParams record.
  Call this after creating the form to display current settings. }
procedure TfrmBorderEditor.GuiFromObject;
begin
 if BkgClrParams = NIL
 then raise Exception.Create('TfrmBorderEditor.GuiFromObject: BkgClrParams is not initialized');

 case BkgClrParams.EffectShape of
   esOneColor  : radShapeSolid.Checked := TRUE;
   esRectangles: radShapeRect.Checked  := TRUE;
   esTriangles : radShapeTraing.Checked:= TRUE;
 end;

 case BkgClrParams.EffectColor of
   ecAutoDetBorder: radAutoDetBorder.Checked := TRUE;
   ecImageAverage : radImageAverage.Checked  := TRUE;
   ecUserColor    : radUserColor.Checked     := TRUE;
 end;

 ColorDialog.Color         := BkgClrParams.Color;
 radFill.Checked           := BkgClrParams.FillType = ftSolid;
 radFade.Checked           := BkgClrParams.FillType = ftFade;
 spnFallSpeed.Value        := BkgClrParams.FadeSpeed;
 spnEdgeSmear.Value        := BkgClrParams.EdgeSmear;
 spnNeighborWeight.Value   := BkgClrParams.NeighborWeight;
 spnNeighborDistance.Value := BkgClrParams.NeighborDist;
 spnPixTolerance.Value     := BkgClrParams.Tolerance;
end;




{ Apply current settings and notify listeners.
  Note: ObjectFromGUI is commented out - the caller's OnApplySettings handler
  should call ObjectFromGUI to retrieve the current GUI settings. }
procedure TfrmBorderEditor.btnApplyClick(Sender: TObject);
begin
  //ObjectFromGUI;  { Caller should do this in OnApplySettings handler }
  if Assigned(FApplySettings)
  then FApplySettings(Self);
end;

{ Close the editor.
  Note: ObjectFromGUI is not called - caller should handle saving if needed. }
procedure TfrmBorderEditor.btnOkClick(Sender: TObject);
begin
  //ObjectFromGUI;  { Caller should handle saving if needed }
  Close;
end;

procedure TfrmBorderEditor.btnCancelClick(Sender: TObject);
begin
 Close;
end;








function TfrmBorderEditor.GetEffectShape: TEffectShape;
begin
 if radShapeSolid.Checked
 then Result:= esOneColor
 else
   if radShapeRect.Checked
   then Result:= esRectangles
   else
     if radShapeTraing.Checked
     then Result:= esTriangles
     else
      begin
        MessageError('Invalid shape');
        Result:= esOneColor;
      end;
end;


function TfrmBorderEditor.GetEffectColor: TEffectColor;
begin
 if radAutoDetBorder.Checked     { Detect border color }
 then Result:= ecAutoDetBorder
 else
  if radImageAverage.Checked
  then Result:= ecImageAverage
  else
   if radUserColor.Checked
   then Result:= ecUserColor
   else
    begin
     MessageError('Invalid color effect');
     Result:= ecImageAverage;
    end;
end;


function TfrmBorderEditor.GetFillType: TFillType;
begin
 if radFill.Checked
 then Result:= ftSolid
 else Result:= ftFade
end;











{--------------------------------------------------------------------------------------------------
  GUI
--------------------------------------------------------------------------------------------------}
procedure TfrmBorderEditor.SettingsChanged(Sender: TObject);
begin
 EnableDisable(grpFadeParams, radFade.Checked);
 lblWarning.Visible      := radFade.Checked;
 spnPixTolerance.Enabled := radAutoDetBorder.Checked;
 lblPixTolerance.Enabled := radAutoDetBorder.Checked;
 radShapeSolid.Enabled   := NOT radAutoDetBorder.Checked; { I disable this because the program will determine 4 color but I can only use one as solid background }
 radShapeRect.Enabled    := radAutoDetBorder.Checked;
 radShapeTraing.Enabled  := radAutoDetBorder.Checked;

 { Autodetect border clr }
 if radAutoDetBorder.Checked
 AND radShapeSolid.Checked
 then radShapeRect.Checked:= TRUE;

 { Average color }
 if radImageAverage.Checked OR radUserColor.Checked
 then radShapeSolid.Checked:= TRUE;

 {$IFDEF TESTER}
 frmTester.PreviewEffects(Sender); {$ENDIF}
end;


procedure TfrmBorderEditor.btnBackgroundClrClick(Sender: TObject);
begin
 if BkgClrParams = NIL
 then raise Exception.Create('TfrmBorderEditor.btnBackgroundClrClick: BkgClrParams is not initialized');

 if ColorDialog.Execute { The color is memorized in ColorDialog.Color }
 then BkgClrParams.Color:= ColorDialog.Color;
end;


procedure TfrmBorderEditor.btnResetClick(Sender: TObject);
begin
 if BkgClrParams = NIL
 then raise Exception.Create('TfrmBorderEditor.btnResetClick: BkgClrParams is not initialized');

 BkgClrParams.Reset;
 GuiFromObject;
end;


procedure TfrmBorderEditor.btnAdvancedClick(Sender: TObject);
begin
 grpFadeParams.Visible:= TRUE;
 btnAdvanced.Visible:= FALSE;
end;


procedure TfrmBorderEditor.lblExplainClick(Sender: TObject);
begin
 pnlExplain.Visible:= FALSE;
end;



end.
