UNIT SecondForm;

INTERFACE

USES
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LightVcl.Visual.CheckBox, Vcl.StdCtrls, LightVcl.Visual.RadioButton, LightVcl.Common.AppDataForm;

TYPE
  TfrmContainer = class(TLightForm)
    Label1           : TLabel;
    CheckBox1        : TCheckBox;
    grpContainer     : TGroupBox;
    CubicCheckBox1   : TCubicCheckBox;
    GroupBox1        : TGroupBox;
    CubicCheckBox2   : TCubicCheckBox;
    RadioButton1     : TRadioButton;
    CubicRadioButton1: TCubicRadioButton;
  end;



IMPLEMENTATION {$R *.dfm}

USES LightVcl.Visual.INIFile;


end.
