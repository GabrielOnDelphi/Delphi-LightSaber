unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService,
  ccAppDataFmxForm, cbDialogsFMX, ccIniFile;

TYPE
  TForm1 = class(TLightForm)
    CheckBox1: TCheckBox;
  private
  public
   procedure FormPostInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}



procedure TForm1.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited;           // This will load the form's state from disk
end;


end.
