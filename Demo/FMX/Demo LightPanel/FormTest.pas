UNIT FormTest;

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Visual.Panel;

TYPE
  TForm2 = class(TForm)
    LightPanel1: TLightPanel;
    LightPanel2: TLightPanel;
    lblVisible: TLabel;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form2: TForm2;

IMPLEMENTATION
{$R *.fmx}


procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption:= 'LightPanel2.Visible is '+ BoolToStr(LightPanel2.Visible, TRUE);
end;


procedure TForm2.Button1Click(Sender: TObject);
begin
  LightPanel2.Visible:= TRUE;
end;


end.
