unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  cbAppDataFmxForm, ccIniFile, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TLightForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
   procedure FormInitialize; override;
  end;

var
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}


procedure TForm1.FormInitialize;
begin
  AutoState:= asFull;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 sleep(1);
 Left:= 0;
end;

end.
