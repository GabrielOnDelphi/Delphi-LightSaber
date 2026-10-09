unit Test.LightVcl.Common.EllipsisText;

{=============================================================================================================
   Unit tests for LightVcl.Common.EllipsisText.pas
   Tests text shortening and ellipsis functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Vcl.Graphics;

type
  [TestFixture]
  TTestEllipsisText = class
  private
    FCanvas: TCanvas;
    FBitmap: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ShortenString Tests }
    [Test]
    procedure TestShortenString_ShortText_NoChange;

    [Test]
    procedure TestShortenString_ExactLength_NoChange;

    [Test]
    procedure TestShortenString_LongText_Shortened;

    [Test]
    procedure TestShortenString_ContainsEllipsis;

    [Test]
    procedure TestShortenString_PreservesStartAndEnd;

    [Test]
    procedure TestShortenString_EmptyString;

    [Test]
    procedure TestShortenString_MaxLengthZero;

    [Test]
    procedure TestShortenString_MaxLengthOne;

    [Test]
    procedure TestShortenString_VeryLongText;

    { GetAverageCharSize Tests }
    [Test]
    procedure TestGetAverageCharSize_ReturnsPositiveX;

    [Test]
    procedure TestGetAverageCharSize_ReturnsPositiveY;

    [Test]
    procedure TestGetAverageCharSize_NoException;

    { GetEllipsisText Tests - Canvas overload }
    [Test]
    procedure TestGetEllipsisText_Canvas_ShortText_NoChange;

    [Test]
    procedure TestGetEllipsisText_Canvas_NoException;

    [Test]
    procedure TestGetEllipsisText_Canvas_EmptyString;

    { GetEllipsisText Tests - MaxWidth overload }
    [Test]
    procedure TestGetEllipsisText_MaxWidth_NoException;

    [Test]
    procedure TestGetEllipsisText_MaxWidth_EmptyString;

    { DrawStringEllipsis Tests }
    [Test]
    procedure TestDrawStringEllipsis_Rect_NoException;

    [Test]
    procedure TestDrawStringEllipsis_Rect_ReturnsNonZero;

    [Test]
    procedure TestDrawStringEllipsis_NoRect_NoException;

    [Test]
    procedure TestDrawStringEllipsis_EmptyString;
  end;

implementation

uses
  LightVcl.Common.EllipsisText;


procedure TTestEllipsisText.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 800;
  FBitmap.Height:= 600;
  FCanvas:= FBitmap.Canvas;
  FCanvas.Font.Name:= 'Arial';
  FCanvas.Font.Size:= 10;
end;


procedure TTestEllipsisText.TearDown;
begin
  FreeAndNil(FBitmap);
  FCanvas:= NIL;
end;


{ ShortenString Tests }

procedure TTestEllipsisText.TestShortenString_ShortText_NoChange;
var
  Result: string;
begin
  Result:= ShortenString('Hello', 20);

  Assert.AreEqual('Hello', Result, 'Short text should not be modified');
end;


procedure TTestEllipsisText.TestShortenString_ExactLength_NoChange;
var
  Result: string;
begin
  Result:= ShortenString('Hello', 5);

  Assert.AreEqual('Hello', Result, 'Text at exact length should not be modified');
end;


{ ShortenString keeps the first (MaxLength div 2 - 2) characters, adds '..', and keeps the last (MaxLength div 2 - 1) characters.
  The input below has 51 characters; MaxLength 20 keeps 8 + '..' + 9 = 19 characters. }
procedure TTestEllipsisText.TestShortenString_LongText_Shortened;
var
  Input, Result: string;
begin
  Input:= 'This is a very long text that needs to be shortened';
  Result:= ShortenString(Input, 20);

  Assert.AreEqual('This is ..shortened', Result, 'The first 8 characters, the ellipsis, the last 9 characters');
  Assert.IsTrue(Length(Result) <= 20,
    'Result should be at most MaxLength characters');
end;


procedure TTestEllipsisText.TestShortenString_ContainsEllipsis;
var
  Input, Result: string;
begin
  Input:= 'This is a very long text that needs to be shortened';
  Result:= ShortenString(Input, 20);

  Assert.AreEqual(9, Pos('..', Result), 'The ellipsis must follow the first 8 characters');
  Assert.AreEqual(0, Pos('.', Result, 11), 'No other dot after the ellipsis');
  Assert.AreEqual(19, Length(Result), '8 + 2 + 9 characters');
end;


procedure TTestEllipsisText.TestShortenString_PreservesStartAndEnd;
var
  Input, Result: string;
begin
  Input:= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result:= ShortenString(Input, 15);

  { 26 characters, MaxLength 15: the first 5 and the last 6 }
  Assert.AreEqual('ABCDE..UVWXYZ', Result, 'Start and end of the alphabet around the ellipsis');
end;


procedure TTestEllipsisText.TestShortenString_EmptyString;
var
  Result: string;
begin
  Result:= ShortenString('', 20);

  Assert.AreEqual('', Result, 'Empty string should return empty');
end;


procedure TTestEllipsisText.TestShortenString_MaxLengthZero;
var
  Result: string;
begin
  Result:= ShortenString('Hello', 0);

  { No room for anything: not even the 2-char ellipsis may be returned }
  Assert.AreEqual('', Result, 'MaxLength = 0 must return an empty string');
end;


procedure TTestEllipsisText.TestShortenString_MaxLengthOne;
var
  Result: string;
begin
  Result:= ShortenString('Hello World', 1);

  { No room for the 2-char ellipsis: the result is the first MaxLength characters, never longer }
  Assert.AreEqual('H', Result, 'MaxLength = 1 must return 1 character');
end;


procedure TTestEllipsisText.TestShortenString_VeryLongText;
var
  Input, Result: string;
begin
  Input:= StringOfChar('X', 10000);
  Result:= ShortenString(Input, 50);

  { MaxLength 50: the first 23 and the last 24 characters }
  Assert.AreEqual(StringOfChar('X', 23) + '..' + StringOfChar('X', 24), Result, 'The first 23 characters, the ellipsis, the last 24 characters');
  Assert.IsTrue(Length(Result) <= 50,
    'Very long text should be shortened to MaxLength');
end;


{ GetAverageCharSize Tests }

procedure TTestEllipsisText.TestGetAverageCharSize_ReturnsPositiveX;
var
  Size: TPoint;
begin
  Size:= GetAverageCharSize(FCanvas);

  Assert.IsTrue(Size.X > 0, 'Average char width should be positive');
end;


procedure TTestEllipsisText.TestGetAverageCharSize_ReturnsPositiveY;
var
  Size: TPoint;
begin
  Size:= GetAverageCharSize(FCanvas);

  Assert.IsTrue(Size.Y > 0, 'Average char height should be positive');
end;


{ The average is measured over the 52 letters A..Z, a..z. The reference measures them with TCanvas.TextExtent. }
procedure TTestEllipsisText.TestGetAverageCharSize_NoException;
CONST
  Letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
var
  Size: TPoint;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Size:= GetAverageCharSize(FCanvas);
    end);

  Assert.AreEqual(FCanvas.TextWidth(Letters) DIV 52, Size.X, 'X = width of the 52 letters div 52');
  Assert.AreEqual(FCanvas.TextHeight(Letters), Size.Y, 'Y = height of the letters');
end;


{ GetEllipsisText Tests - Canvas overload }

procedure TTestEllipsisText.TestGetEllipsisText_Canvas_ShortText_NoChange;
var
  Result: string;
begin
  { Short text with large max width should not be modified }
  Result:= GetEllipsisText('Hello', FCanvas, 500, 100);

  Assert.AreEqual('Hello', Result, 'Short text should not be modified');
end;


{ The rectangle is Rect(1, 1, 100, 50): 99 pixels wide. The text does not fit, so the END is replaced by '...' }
procedure TTestEllipsisText.TestGetEllipsisText_Canvas_NoException;
CONST
  Input = 'This is a test string';
var
  Result, Kept: string;
begin
  Assert.IsTrue(FCanvas.TextWidth(Input) > 99, 'Precondition: the input is wider than the rectangle');

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result:= GetEllipsisText(Input, FCanvas, 100, 50);
    end);

  Assert.AreNotEqual(Input, Result, 'A text wider than the rectangle must be shortened');
  Assert.AreEqual('...', Copy(Result, Length(Result) - 2, 3), 'End ellipsis expected. Got: ' + Result);
  Kept:= Copy(Result, 1, Length(Result) - 3);
  Assert.AreEqual(Copy(Input, 1, Length(Kept)), Kept, 'The text before the ellipsis is the start of the input. Got: ' + Result);
  Assert.IsTrue(FCanvas.TextWidth(Result) <= 99, 'The shortened text must fit in 99 pixels. Got ' + IntToStr(FCanvas.TextWidth(Result)) + ' for ' + Result);
end;


procedure TTestEllipsisText.TestGetEllipsisText_Canvas_EmptyString;
var
  Result: string;
begin
  Result:= GetEllipsisText('', FCanvas, 100, 50);

  Assert.AreEqual('', Result, 'Empty string should return empty');
end;


{ GetEllipsisText Tests - MaxWidth overload }

{ The first I and the last I characters of S around '...' - the shape the MaxWidth overload returns }
function MiddleEllipsis(CONST S: string; I: Integer): string;
begin
  Result:= Copy(S, 1, I) + '...' + Copy(S, Length(S) - I + 1, I);
end;


{ The MaxWidth overload keeps the most characters from both ends whose width stays within MaxWidth minus the width of '...'.
  The test checks the shape, the fit, and that one more character on each side would not fit. }
procedure TTestEllipsisText.TestGetEllipsisText_MaxWidth_NoException;
CONST
  Input = 'This is a test string';
  MaxWidth = 50;
var
  Result: string;
  I, Kept, Limit: Integer;
begin
  Assert.IsTrue(FCanvas.TextWidth(Input) > MaxWidth, 'Precondition: the input is wider than MaxWidth');

  Assert.WillNotRaiseAny(
    procedure
    begin
      Result:= GetEllipsisText(Input, FCanvas, MaxWidth);
    end);

  Kept:= -1;
  for I:= 0 to Length(Input) DIV 2 do
    if Result = MiddleEllipsis(Input, I)
    then Kept:= I;
  Assert.IsTrue(Kept >= 0, 'The result must be "start...end" of the input. Got: ' + Result);

  Limit:= MaxWidth - FCanvas.TextWidth('...');
  if Kept > 0
  then Assert.IsTrue(FCanvas.TextWidth(Result) <= Limit, 'The result must fit in ' + IntToStr(Limit) + ' pixels. Got: ' + Result);
  Assert.IsTrue(FCanvas.TextWidth(MiddleEllipsis(Input, Kept + 1)) > Limit, 'One more character on each side would still fit, so the result is too short: ' + Result);
end;


procedure TTestEllipsisText.TestGetEllipsisText_MaxWidth_EmptyString;
var
  Result: string;
begin
  Result:= GetEllipsisText('', FCanvas, 100);

  Assert.AreEqual('', Result, 'Empty string should return empty');

  { The '' above comes from the "it fits" branch, which returns the input unchanged }
  Assert.IsTrue(FCanvas.TextWidth('Hi') <= 100, 'Precondition: "Hi" fits in 100 pixels');
  Assert.AreEqual('Hi', GetEllipsisText('Hi', FCanvas, 100), 'A text that fits must come back unchanged');
end;


{ DrawStringEllipsis Tests }

{ Number of pixels in R that are not white }
function CountInk(Canvas: TCanvas; CONST R: TRect): Integer;
var
  X, Y: Integer;
begin
  Result:= 0;
  for Y:= R.Top to R.Bottom - 1 do
    for X:= R.Left to R.Right - 1 do
      if Canvas.Pixels[X, Y] <> clWhite
      then Inc(Result);
end;


{ Paints the whole test bitmap white, so that whatever DrawText adds shows up as non-white pixels }
procedure ClearToWhite(Canvas: TCanvas; Width, Height: Integer);
begin
  Canvas.Brush.Color:= clWhite;
  Canvas.Brush.Style:= bsSolid;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ DrawText returns the height of the text it drew: one line of the canvas font }
procedure TTestEllipsisText.TestDrawStringEllipsis_Rect_NoException;
var
  R: TRect;
  Height: Integer;
begin
  R:= Rect(100, 100, 300, 150);
  ClearToWhite(FCanvas, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Height:= DrawStringEllipsis('This is a test string', FCanvas, R);
    end);

  Assert.AreEqual(FCanvas.TextHeight('This is a test string'), Height, 'One line of text');
  Assert.IsTrue(CountInk(FCanvas, R) > 0, 'The text must be drawn inside the rectangle');
  Assert.AreEqual(0, CountInk(FCanvas, Rect(0, 0, 100, 100)), 'Nothing may be drawn above and left of the rectangle');
end;


procedure TTestEllipsisText.TestDrawStringEllipsis_Rect_ReturnsNonZero;
var
  R: TRect;
  Result: Integer;
begin
  R:= Rect(0, 0, 200, 50);
  Result:= DrawStringEllipsis('Test', FCanvas, R);

  Assert.IsTrue(Result > 0, 'DrawStringEllipsis should return non-zero height');
end;


{ Without a rectangle the text goes to the canvas ClipRect, which for a bitmap is the whole bitmap: drawn at the top left }
procedure TTestEllipsisText.TestDrawStringEllipsis_NoRect_NoException;
var
  Height, LineHeight: Integer;
begin
  ClearToWhite(FCanvas, FBitmap.Width, FBitmap.Height);
  LineHeight:= FCanvas.TextHeight('This is a test string');

  Assert.WillNotRaiseAny(
    procedure
    begin
      Height:= DrawStringEllipsis('This is a test string', FCanvas);
    end);

  Assert.AreEqual(LineHeight, Height, 'One line of text');
  Assert.IsTrue(CountInk(FCanvas, Rect(0, 0, FCanvas.TextWidth('This is a test string'), LineHeight)) > 0, 'The text must be drawn at the top left');
  Assert.AreEqual(0, CountInk(FCanvas, Rect(0, LineHeight, 200, LineHeight + 20)), 'Nothing may be drawn under the first line');
end;


procedure TTestEllipsisText.TestDrawStringEllipsis_EmptyString;
var
  Height: Integer;
begin
  ClearToWhite(FCanvas, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaiseAny(
    procedure
    begin
      Height:= DrawStringEllipsis('', FCanvas);
    end);

  Assert.AreEqual(0, CountInk(FCanvas, Rect(0, 0, 200, 40)), 'An empty string draws nothing');

  { DrawText returns 1 for an empty string on Windows 11, a value its documentation does not give, so the
    test asserts only that no line of text was laid out }
  Assert.IsTrue(Height < FCanvas.TextHeight('Wg'), 'An empty string lays out no line of text. Height: ' + IntToStr(Height));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEllipsisText);

end.
