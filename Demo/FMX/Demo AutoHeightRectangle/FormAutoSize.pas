unit FormAutoSize;

{-------------------------------------------------------------------------------------------------------------
   GabrielMoraru.com
   2025.07
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to use TAutoSizeBoxTxt.

   TAutoSizeBoxTxt looks like a WhatsApp text bubble.
   It automatically adjusts its height to fit its internal component (text or image).
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Types, System.Classes, System.SysUtils, System.Math,
  FMX.Types, FMX.Forms, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo, FMX.Memo.Types, FMX.ScrollBox, FMX.Effects, FMX.Controls,
  LightFmx.Common.AppData.Form;

TYPE
  TForm1 = class(TLightForm)
    boxConversation: TFramedVertScrollBox;
    btnSendAnswer: TButton;
    laySend: TLayout;
    mmoUserResponse: TMemo;
    btnImage: TButton;
    Timer: TTimer;
    procedure btnSendAnswerClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    Shown: Boolean;
    procedure ScrollDown;
    procedure MakeDemoTextBubble;
    procedure MakeDemoImageBubble;
  public
    procedure FixFmxBug;
    procedure AfterConstruction; override;
  end;

var
  Form1: TForm1;

IMPLEMENTATION {$R *.fmx}

USES LightFmx.Visual.AutoSizeBoxImg, LightFmx.Visual.AutosizeBoxText, LightFmx.Visual.AutoSizeBox;




{-------------------------------------------------------------------------------------------------------------
   STARTUP
-------------------------------------------------------------------------------------------------------------}
procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
  MakeDemoImageBubble;
end;


procedure TForm1.TimerTimer(Sender: TObject);
begin
  Timer.Enabled:= FALSE;
  MakeDemoTextBubble;
end;


procedure TForm1.FormActivate(Sender: TObject);
begin
  if Shown then EXIT;
  Shown:= TRUE;
end;



{-------------------------------------------------------------------------------------------------------------
   FMX BUG
   stackoverflow.com/questions/79851998/fmx-form-has-incorrect-size-after-startup
-------------------------------------------------------------------------------------------------------------}
procedure TForm1.FixFmxBug;
begin
  mmoUserResponse.Text:= IntToStr(Round(width)) + '/'+ IntToStr(Round(height)); // This magically fixes the bug
end;




{-------------------------------------------------------------------------------------------------------------
   DEMO BUBBLES
-------------------------------------------------------------------------------------------------------------}
procedure TForm1.MakeDemoTextBubble;
begin
  mmoUserResponse.Text :=
      'WhatsApp-like text bubble. ' + #13#10+
      'Cool and green!' + #13#10+
      'And fresh';

  //FixFmxBug;
  btnSendAnswerClick(Self);
end;


procedure TForm1.MakeDemoImageBubble;
begin
  // The Image Bubble component
  VAR Img     := TAutosizeBoxImg.Create(Self);
  Img.Parent  := boxConversation.Content;
  Img.Stored  := FALSE;
  Img.Position.Y:= 99999999;
  Img.LoadImage('..\..\Demo image - Geek queen.jpg', TRect.Create(0, 0, 1344, 768));    // This triggers image loading and self-sizing!

  ScrollDown;
end;





{-------------------------------------------------------------------------------------------------------------
   GUI
-------------------------------------------------------------------------------------------------------------}
procedure TForm1.btnSendAnswerClick(Sender: TObject);
begin
  MakeTextBubble(boxConversation.Content, mmoUserResponse.text, bxUser);
  MakeTextBubble(boxConversation.Content, 'You are right!', bxModel);
  ScrollDown;
end;


procedure TForm1.btnImageClick(Sender: TObject);
begin
  MakeDemoImageBubble;
end;


procedure TForm1.ScrollDown;
var BottomY: Single;
begin
  BottomY := Max(0, boxConversation.ContentBounds.Height - boxConversation.Height);
  boxConversation.ViewportPosition := PointF(boxConversation.ViewportPosition.X, BottomY);  // Scroll to true bottom
end;



end.
