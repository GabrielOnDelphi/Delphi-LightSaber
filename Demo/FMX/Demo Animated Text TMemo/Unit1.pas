unit Unit1;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Memo, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, LightFmx.Visual.AnimatedMemo;

type
  TForm1 = class(TForm)
    mmoAni: TAnimatedMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
  end;

var
  Form1: TForm1;

implementation
{$R *.fmx}


procedure TForm1.Button2Click(Sender: TObject);
begin
  mmoAni.AnimatedText:='Initialize systems'+ #13#10;
  mmoAni.AnimatedText:='Loading components'+ #13#10;
  mmoAni.AnimatedText:='Reading config data'+ #13#10;
  mmoAni.AnimatedText:='Ready to start'+ #13#10;
end;

end.
