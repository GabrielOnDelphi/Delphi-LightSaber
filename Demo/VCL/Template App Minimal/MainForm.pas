UNIT MainForm;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Use this as a template when you start a new mini application (usually for code-testing purposes)
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  LightVcl.Common.AppDataForm, Vcl.ExtCtrls;

type
  TfrmMain = class(TLightForm)
    CheckBox: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Button1: TButton;
  private
  public
  end;


IMPLEMENTATION {$R *.dfm}

end.
