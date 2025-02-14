UNIT MainForm;

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  cbAppDataForm;

type
  TfrmMain = class(TLightForm)
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
  private
  public
  end;

VAR
  frmMain: TfrmMain;

IMPLEMENTATION {$R *.dfm}

end.
