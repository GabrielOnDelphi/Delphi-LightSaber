UNIT FormStylesMain;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Demo/test program for FormSkinsDisk

   In Deployment Manager deploy your skins to "assets\internal\System\Skins\Android\"
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Dialogs, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  LightFmx.Common.AppData.Form, FMX.Memo.Types;

TYPE
  TfrmSimpleDemo = class(TLightForm)
    chkSome      : TCheckBox;
    GroupBox1    : TGroupBox;
    Layout1      : TLayout;
    Memo1        : TMemo;
    ProgressBar  : TProgressBar;
    RadioButton1 : TRadioButton;
    RadioButton2 : TRadioButton;
    StatusBar1   : TStatusBar;
    Timer        : TTimer;
    TrackBar1    : TTrackBar;
    edtPath: TEdit;
    lblFolder: TLabel;
    btnSkins: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure btnSkinsClick(Sender: TObject);
  private
  public
    procedure AfterConstruction; override;
  end;


VAR frmSimpleDemo: TfrmSimpleDemo;

IMPLEMENTATION
{$R *.fmx}

USES
   LightFmx.Common.AppData, FormSkinsDisk;



procedure TfrmSimpleDemo.AfterConstruction;
begin
  inherited;
end;


procedure TfrmSimpleDemo.TimerTimer(Sender: TObject);
begin
  ProgressBar.Value:= ProgressBar.Value+ 1;
  if ProgressBar.Value > 99
  then ProgressBar.Value:= 0;
end;


procedure TfrmSimpleDemo.btnSkinsClick(Sender: TObject);
VAR Dummy: TfrmStyleDisk;
begin
  AppData.CreateForm(TfrmStyleDisk, Dummy);
  Dummy.show;
end;

end.
