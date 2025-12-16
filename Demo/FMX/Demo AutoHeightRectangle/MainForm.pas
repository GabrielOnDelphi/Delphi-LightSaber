unit MainForm;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to use TAutoSizeBoxTxt.

   TAutoSizeBoxTxt looks like a WhatsApp text buuble.
   It automatically adjusts its height to fit its internal component (text or image).
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Types, System.Classes,
  FMX.Types, FMX.Forms, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo,
  LightFmx.Visual.AutoSizeBoxTxt, LightFmx.Visual.AutoSizeBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Effects,
  FMX.Controls;

TYPE
  TForm1 = class(TForm)
    Bubble: TAutoSizeBoxTxt;
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