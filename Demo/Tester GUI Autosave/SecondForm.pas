UNIT SecondForm;

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cvCheckBox, Vcl.StdCtrls, cvRadioButton;

TYPE
  TfrmContainer = class(TForm)
    Label1         : TLabel;
    CheckBox1      : TCheckBox;
    grpContainer   : TGroupBox;
    CubicCheckBox1 : TCubicCheckBox;
    GroupBox1: TGroupBox;
    CubicCheckBox2: TCubicCheckBox;
    RadioButton1: TRadioButton;
    CubicRadioButton1: TCubicRadioButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;



IMPLEMENTATION {$R *.dfm}

USES cvIniFile;


procedure TfrmContainer.FormCreate(Sender: TObject);
begin
 LoadForm(Self);
end;

procedure TfrmContainer.FormDestroy(Sender: TObject);
begin
 SaveForm(Self);
end;

end.
