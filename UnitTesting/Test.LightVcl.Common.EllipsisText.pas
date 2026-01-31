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


procedure TTestEllipsisText.TestShortenString_LongText_Shortened;
var
  Input, Result: string;
begin
  Input:= 'This is a very long text that needs to be shortened';
  Result:= ShortenString(Input, 20);

  Assert.IsTrue(Length(Result) <= 20,
    'Result should be at most MaxLength characters');
end;


procedure TTestEllipsisText.TestShortenString_ContainsEllipsis;
var
  Input, Result: string;
begin
  Input:= 'This is a very long text that needs to be shortened';
  Result:= ShortenString(Input, 20);

  Assert.IsTrue(Pos('..', Result) > 0,
    'Shortened text should contain ellipsis');
end;


procedure TTestEllipsisText.TestShortenString_PreservesStartAndEnd;
var
  Input, Result: string;
begin
  Input:= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result:= ShortenString(Input, 15);

  { Should preserve start }
  Assert.IsTrue(Result.StartsWith('A'),
    'Should preserve start of string');

  { Should preserve end }
  Assert.IsTrue(Result.EndsWith('Z'),
    'Should preserve end of string');
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
  { With MaxLength 0, the div 2 gives 0, minus 2 gives -2, so Copy returns empty }
  Result:= ShortenString('Hello', 0);

  { The function behavior with 0 max length }
  Assert.IsTrue(Length(Result) <= Length('Hello'),
    'Result should not exceed original length');
end;


procedure TTestEllipsisText.TestShortenString_MaxLengthOne;
var
  Result: string;
begin
  Result:= ShortenString('Hello World', 1);

  { Function will still try to shorten }
  Assert.Pass('Function handles MaxLength=1 without exception');
end;


procedure TTestEllipsisText.TestShortenString_VeryLongText;
var
  Input, Result: string;
begin
  Input:= StringOfChar('X', 10000);
  Result:= ShortenString(Input, 50);

  Assert.IsTrue(Length(Result) <= 50,
    'Very long text should be shortened to MaxLength');
  Assert.IsTrue(Pos('..', Result) > 0,
    'Should contain ellipsis');
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


procedure TTestEllipsisText.TestGetAverageCharSize_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      GetAverageCharSize(FCanvas);
    end);
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


procedure TTestEllipsisText.TestGetEllipsisText_Canvas_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      GetEllipsisText('This is a test string', FCanvas, 100, 50);
    end);
end;


procedure TTestEllipsisText.TestGetEllipsisText_Canvas_EmptyString;
var
  Result: string;
begin
  Result:= GetEllipsisText('', FCanvas, 100, 50);

  Assert.AreEqual('', Result, 'Empty string should return empty');
end;


{ GetEllipsisText Tests - MaxWidth overload }

procedure TTestEllipsisText.TestGetEllipsisText_MaxWidth_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      GetEllipsisText('This is a test string', FCanvas, 50);
    end);
end;


procedure TTestEllipsisText.TestGetEllipsisText_MaxWidth_EmptyString;
var
  Result: string;
begin
  Result:= GetEllipsisText('', FCanvas, 100);

  Assert.AreEqual('', Result, 'Empty string should return empty');
end;


{ DrawStringEllipsis Tests }

procedure TTestEllipsisText.TestDrawStringEllipsis_Rect_NoException;
var
  R: TRect;
begin
  R:= Rect(0, 0, 200, 50);

  Assert.WillNotRaise(
    procedure
    begin
      DrawStringEllipsis('This is a test string', FCanvas, R);
    end);
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


procedure TTestEllipsisText.TestDrawStringEllipsis_NoRect_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawStringEllipsis('This is a test string', FCanvas);
    end);
end;


procedure TTestEllipsisText.TestDrawStringEllipsis_EmptyString;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawStringEllipsis('', FCanvas);
    end);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEllipsisText);

end.
