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
    procedure btnSendAnswerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Shown: Boolean;
    procedure ScrollDown;
  public
    procedure AfterConstruction; override;
  end;

var
  Form1: TForm1;

IMPLEMENTATION {$R *.fmx}

USES LightFmx.Visual.AutoSizeBoxImg, LightFmx.Visual.AutoSizeBoxTxt, LightFmx.Visual.AutoSizeBox;


procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;


procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
  { Not a good place to initialize code here. The form is not ready yet. }

  // Weird bug: reading the Width/Height of the form, sets its correct size.
  mmoUserResponse.Text :=
      'WhatsApp-like text bubble. ' + #13#10+
      'Cool and green!';

  //mmoUserResponse.Text:= IntToStr(Round(width)) + '/'+ IntToStr(Round(height));

  btnSendAnswerClick(Self);
  btnImageClick(Self);
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  //
end;


procedure TForm1.FormActivate(Sender: TObject);
begin
  if Shown then EXIT;
  Shown:= TRUE;
end;


procedure TForm1.btnSendAnswerClick(Sender: TObject);
begin
  //mmoUserResponse.Text:= IntToStr(Round(width)) + '/'+ IntToStr(Round(height));

  MakeBubbleText(boxConversation.Content, mmoUserResponse.text, bxUser);
  MakeBubbleText(boxConversation.Content, 'You are right!', bxModel);

  ScrollDown;
end;


// I have this weird bug in Delphi 13 fmx: when I click this button, the TAutosizeBubble does not have the correct size. Looks like Width/Height of the form or of the boxConversation.Content is not correctly read.
// However, if I click the button the second time or if I resize the window, the next TAutosizeBubble has the correct size!
// Even more weird, I realised that if I touch (read) Form.Width or Form.Hieght at program startup, the height of the TAutosizeBubble is correct (or much better).
// If I read (in the first run) the width of the Parent - see the line ParentContentWidth:= GetParentContentWidth in TAutosizeBubble.UpdateSize, I get ParentContentWidth = 35 pixels, instead of 500, the correct width on the screen, later after the forms appears on the screen.
procedure TForm1.btnImageClick(Sender: TObject);
begin
  // The Image Bubble component
  VAR Img     := TAutosizeBubble.Create(Self);
  Img.Parent  := boxConversation.Content;
  Img.Stored  := FALSE;
  Img.Position.Y:= 99999999;
  Img.LoadImage('..\..\Demo image - Geek queen.jpg', TRect.Create(0, 0, 1344, 768));    // This triggers image loading and self-sizing!

  ScrollDown;
end;


procedure TForm1.ScrollDown;
var BottomY: Single;
begin
  BottomY := Max(0, boxConversation.ContentBounds.Height - boxConversation.Height);
  boxConversation.ViewportPosition := PointF(boxConversation.ViewportPosition.X, BottomY);  // Scroll to true bottom
end;



end.
