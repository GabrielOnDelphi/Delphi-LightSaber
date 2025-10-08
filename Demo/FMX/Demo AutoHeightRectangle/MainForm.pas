unit MainForm;

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  LightFmx.Visual.AutoHeightRectangle;

TYPE
  TForm1 = class(TForm)
    Bubble: TAutoHeightRectangle;
    boxConversation: TFramedVertScrollBox;
    btnSendAnswer: TButton;
    laySend: TLayout;
    mmoUserResponse: TMemo;
    procedure btnSendAnswerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure laySendClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  mmoUserResponse.Text :=
    'WhatsApp-like text bubble. ' + #13#10+
    'Cool and green!';
  btnSendAnswerClick(Sender);
end;


procedure TForm1.btnSendAnswerClick(Sender: TObject);
begin
  Bubble.BoxType:= bxUser;
  Bubble.Text:= mmoUserResponse.text;
end;


procedure TForm1.laySendClick(Sender: TObject);
begin
  Bubble.BoxType:= bxModel;
  Bubble.Text:= 'Aha... That''s great!';
end;

end.