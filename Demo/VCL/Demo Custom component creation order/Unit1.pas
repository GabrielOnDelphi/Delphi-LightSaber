unit Unit1;

INTERFACE
USES
  System.SysUtils, System.Classes, Vcl.Forms, LightVcl.Visual.AppDataForm, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, LightVcl.Visual.CreationOrderTester,
  LightVcl.Visual.SpinEdit;

TYPE
  TForm1 = class(TLightForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION {$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
var obj: TCreationOrderTest;
begin
 obj:= TCreationOrderTest.Create(Self);
 obj.Parent:= Self;
end;


procedure TForm1.Button2Click(Sender: TObject);
var obj: LightVcl.Visual.SpinEdit.TCubicSpinEditSplit;
begin
 obj:= TCubicSpinEditSplit.Create(Self);
 obj.Parent:= Self;
 obj.Top:= 60;
 obj.Left:= 2;
end;

end.









