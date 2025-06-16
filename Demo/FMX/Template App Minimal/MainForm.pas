unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService,
  FMX.Grid.Style, FMX.Presentation.Factory, FMX.Presentation.Style,
  LightFmx.Common.AppData, LightFmx.Common.AppData.Form,
  LightCore.INIFile,  LightCore.LogRam, FMX.ScrollBox, FMX.Grid,
  LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private
  public
   procedure FormPostInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES LightCore.AppData;



procedure TForm1.FormCreate(Sender: TObject);
begin
 VAR LogViewer:= TLogViewer.Create(Self);
 LogViewer.Parent:= Self;
end;

procedure TForm1.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited;           // This will load the form's state from disk
end;


end.
