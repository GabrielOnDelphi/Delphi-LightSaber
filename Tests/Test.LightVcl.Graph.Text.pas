unit Test.LightVcl.Graph.Text;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Text.pas
   Tests text rendering, centering, shadows, and font measurement functions.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  Winapi.Windows,
  Vcl.Graphics;

type
  [TestFixture]
  TTestGraphText = class
  private
    FBitmap: TBitmap;
    FCanvas: TCanvas;
    procedure FillBitmapWithColor(BMP: TBitmap; Color: TColor);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetFontHeight Tests }
    [Test]
    procedure TestGetFontHeight_BasicCall;

    [Test]
    procedure TestGetFontHeight_NilFont;

    [Test]
    procedure TestGetFontHeight_ReturnsPositive;

    [Test]
    procedure TestGetFontHeight_DifferentSizes;

    { DrawTextCentered Tests }
    [Test]
    procedure TestDrawTextCentered_BasicCall;

    [Test]
    procedure TestDrawTextCentered_NilCanvas;

    [Test]
    procedure TestDrawTextCentered_EmptyText;

    { CenterTextX (Bitmap overload) Tests }
    [Test]
    procedure TestCenterTextX_Bitmap_BasicCall;

    [Test]
    procedure TestCenterTextX_Bitmap_NilBitmap;

    [Test]
    procedure TestCenterTextX_Bitmap_EmptyText;

    [Test]
    procedure TestCenterTextX_Bitmap_CentersCorrectly;

    { CenterTextY (Bitmap overload) Tests }
    [Test]
    procedure TestCenterTextY_Bitmap_BasicCall;

    [Test]
    procedure TestCenterTextY_Bitmap_NilBitmap;

    [Test]
    procedure TestCenterTextY_Bitmap_EmptyText;

    { CenterTextX (Canvas overload) Tests }
    [Test]
    procedure TestCenterTextX_Canvas_BasicCall;

    [Test]
    procedure TestCenterTextX_Canvas_NilCanvas;

    { CenterTextY (Canvas overload) Tests }
    [Test]
    procedure TestCenterTextY_Canvas_BasicCall;

    [Test]
    procedure TestCenterTextY_Canvas_NilCanvas;

    { DrawTextShadowBox (Bitmap overload) Tests }
    [Test]
    procedure TestDrawTextShadowBox_Bitmap_BasicCall;

    [Test]
    procedure TestDrawTextShadowBox_Bitmap_NilBitmap;

    [Test]
    procedure TestDrawTextShadowBox_Bitmap_WrongPixelFormat;

    [Test]
    procedure TestDrawTextShadowBox_Bitmap_AlignTop;

    [Test]
    procedure TestDrawTextShadowBox_Bitmap_AlignBottom;

    { DrawTextShadowBox (Canvas overload) Tests }
    [Test]
    procedure TestDrawTextShadowBox_Canvas_BasicCall;

    [Test]
    procedure TestDrawTextShadowBox_Canvas_NilCanvas;

    { DrawTextShadow3DSoft Tests }
    [Test]
    procedure TestDrawTextShadow3DSoft_BasicCall;

    [Test]
    procedure TestDrawTextShadow3DSoft_NilCanvas;

    [Test]
    procedure TestDrawTextShadow3DSoft_EmptyText;

    [Test]
    procedure TestDrawTextShadow3DSoft_CustomShadowColor;

    { DrawTextShadow3DHard Tests }
    [Test]
    procedure TestDrawTextShadow3DHard_BasicCall;

    [Test]
    procedure TestDrawTextShadow3DHard_NilCanvas;

    [Test]
    procedure TestDrawTextShadow3DHard_WithTopShadow;

    [Test]
    procedure TestDrawTextShadow3DHard_WithoutTopShadow;

    { DrawTextOutline Tests }
    [Test]
    procedure TestDrawTextOutline_BasicCall;

    [Test]
    procedure TestDrawTextOutline_NilCanvas;

    [Test]
    procedure TestDrawTextOutline_SolidMiddle;

    [Test]
    procedure TestDrawTextOutline_TransparentMiddle;

    { DrawTextXOR Tests }
    [Test]
    procedure TestDrawTextXOR_BasicCall;

    [Test]
    procedure TestDrawTextXOR_NilFont;

    [Test]
    procedure TestDrawTextXOR_EmptyText;

    { ShadowDownLeft Tests (deprecated) }
    [Test]
    procedure TestShadowDownLeft_BasicCall;

    [Test]
    procedure TestShadowDownLeft_NilBitmap;

    { ShadowDownRight Tests (deprecated) }
    [Test]
    procedure TestShadowDownRight_BasicCall;

    [Test]
    procedure TestShadowDownRight_NilBitmap;

    { clTextShadow constant test }
    [Test]
    procedure TestClTextShadow_Value;
  end;

implementation

uses
  LightVcl.Graph.Text;


procedure TTestGraphText.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.PixelFormat:= pf24bit;
  FillBitmapWithColor(FBitmap, clWhite);
  FCanvas:= FBitmap.Canvas;
end;


procedure TTestGraphText.TearDown;
begin
  FreeAndNil(FBitmap);
  FCanvas:= NIL;
end;


procedure TTestGraphText.FillBitmapWithColor(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.FillRect(Rect(0, 0, BMP.Width, BMP.Height));
end;


{ GetFontHeight Tests }

procedure TTestGraphText.TestGetFontHeight_BasicCall;
var
  Font: TFont;
  Height: Integer;
begin
  Font:= TFont.Create;
  TRY
    Font.Size:= 12;
    Height:= GetFontHeight(Font);
    Assert.IsTrue(Height > 0, 'Font height should be positive');
  FINALLY
    FreeAndNil(Font);
  END;
end;


procedure TTestGraphText.TestGetFontHeight_NilFont;
begin
  Assert.WillRaise(
    procedure
    begin
      GetFontHeight(NIL);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestGetFontHeight_ReturnsPositive;
var
  Font: TFont;
  Height: Integer;
begin
  Font:= TFont.Create;
  TRY
    Font.Name:= 'Arial';
    Font.Size:= 10;
    Height:= GetFontHeight(Font);
    Assert.IsTrue(Height > 0, 'Height should be positive for any valid font');
  FINALLY
    FreeAndNil(Font);
  END;
end;


procedure TTestGraphText.TestGetFontHeight_DifferentSizes;
var
  Font: TFont;
  Height10, Height20: Integer;
begin
  Font:= TFont.Create;
  TRY
    Font.Name:= 'Arial';

    Font.Size:= 10;
    Height10:= GetFontHeight(Font);

    Font.Size:= 20;
    Height20:= GetFontHeight(Font);

    Assert.IsTrue(Height20 > Height10, 'Larger font size should have greater height');
  FINALLY
    FreeAndNil(Font);
  END;
end;


{ DrawTextCentered Tests }

procedure TTestGraphText.TestDrawTextCentered_BasicCall;
var
  R: TRect;
begin
  R:= Rect(0, 0, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaise(
    procedure
    begin
      DrawTextCentered(FCanvas, 'Test', R);
    end);
end;


procedure TTestGraphText.TestDrawTextCentered_NilCanvas;
var
  R: TRect;
begin
  R:= Rect(0, 0, 100, 100);

  Assert.WillRaise(
    procedure
    begin
      DrawTextCentered(NIL, 'Test', R);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextCentered_EmptyText;
var
  R: TRect;
begin
  R:= Rect(0, 0, FBitmap.Width, FBitmap.Height);

  Assert.WillNotRaise(
    procedure
    begin
      DrawTextCentered(FCanvas, '', R);
    end);
end;


{ CenterTextX (Bitmap overload) Tests }

procedure TTestGraphText.TestCenterTextX_Bitmap_BasicCall;
begin
  CenterTextX(FBitmap, 'Test');
  Assert.Pass('CenterTextX should not raise exception');
end;


procedure TTestGraphText.TestCenterTextX_Bitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterTextX(TBitmap(NIL), 'Test');
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestCenterTextX_Bitmap_EmptyText;
var
  X: Integer;
begin
  X:= CenterTextX(FBitmap, '');
  { Empty text has zero width, so X should be half the bitmap width }
  Assert.AreEqual(FBitmap.Width DIV 2, X, 'Empty text should center at bitmap midpoint');
end;


procedure TTestGraphText.TestCenterTextX_Bitmap_CentersCorrectly;
var
  X: Integer;
  TextWidth: Integer;
begin
  TextWidth:= FBitmap.Canvas.TextWidth('Test');
  X:= CenterTextX(FBitmap, 'Test');

  { Verify the calculated X is correct: (BitmapWidth - TextWidth) / 2 }
  Assert.AreEqual((FBitmap.Width - TextWidth) DIV 2, X, 'X should center the text');
end;


{ CenterTextY (Bitmap overload) Tests }

procedure TTestGraphText.TestCenterTextY_Bitmap_BasicCall;
begin
  CenterTextY(FBitmap, 'Test');
  Assert.Pass('CenterTextY should not raise exception');
end;


procedure TTestGraphText.TestCenterTextY_Bitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      CenterTextY(TBitmap(NIL), 'Test');
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestCenterTextY_Bitmap_EmptyText;
var
  Y: Integer;
  TextHeight: Integer;
begin
  TextHeight:= FBitmap.Canvas.TextHeight('');
  Y:= CenterTextY(FBitmap, '');
  Assert.AreEqual((FBitmap.Height - TextHeight) DIV 2, Y, 'Empty text should still calculate Y based on font height');
end;


{ CenterTextX (Canvas overload) Tests }

procedure TTestGraphText.TestCenterTextX_Canvas_BasicCall;
var
  R: TRect;
begin
  R:= Rect(0, 0, 200, 100);
  CenterTextX(FCanvas, 'Test', R);
  Assert.Pass('CenterTextX (Canvas) should not raise exception');
end;


procedure TTestGraphText.TestCenterTextX_Canvas_NilCanvas;
var
  R: TRect;
begin
  R:= Rect(0, 0, 200, 100);

  Assert.WillRaise(
    procedure
    begin
      CenterTextX(TCanvas(NIL), 'Test', R);
    end,
    EAssertionFailed);
end;


{ CenterTextY (Canvas overload) Tests }

procedure TTestGraphText.TestCenterTextY_Canvas_BasicCall;
var
  R: TRect;
begin
  R:= Rect(0, 0, 200, 100);
  CenterTextY(FCanvas, 'Test', R);
  Assert.Pass('CenterTextY (Canvas) should not raise exception');
end;


procedure TTestGraphText.TestCenterTextY_Canvas_NilCanvas;
var
  R: TRect;
begin
  R:= Rect(0, 0, 200, 100);

  Assert.WillRaise(
    procedure
    begin
      CenterTextY(TCanvas(NIL), 'Test', R);
    end,
    EAssertionFailed);
end;


{ DrawTextShadowBox (Bitmap overload) Tests }

procedure TTestGraphText.TestDrawTextShadowBox_Bitmap_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadowBox(FBitmap, 'Test', TRUE);
    end);
end;


procedure TTestGraphText.TestDrawTextShadowBox_Bitmap_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextShadowBox(TBitmap(NIL), 'Test', TRUE);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextShadowBox_Bitmap_WrongPixelFormat;
var
  BMP32: TBitmap;
begin
  BMP32:= TBitmap.Create;
  TRY
    BMP32.Width:= 100;
    BMP32.Height:= 50;
    BMP32.PixelFormat:= pf32bit;  { Wrong format - should be pf24bit }

    Assert.WillRaise(
      procedure
      begin
        DrawTextShadowBox(BMP32, 'Test', TRUE);
      end,
      EAssertionFailed);
  FINALLY
    FreeAndNil(BMP32);
  END;
end;


procedure TTestGraphText.TestDrawTextShadowBox_Bitmap_AlignTop;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadowBox(FBitmap, 'Test', TRUE, clTextShadow, 20, 2);
    end);
end;


procedure TTestGraphText.TestDrawTextShadowBox_Bitmap_AlignBottom;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadowBox(FBitmap, 'Test', FALSE, clTextShadow, 20, 2);
    end);
end;


{ DrawTextShadowBox (Canvas overload) Tests }

procedure TTestGraphText.TestDrawTextShadowBox_Canvas_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadowBox(FCanvas, 'Test', 10, 10);
    end);
end;


procedure TTestGraphText.TestDrawTextShadowBox_Canvas_NilCanvas;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextShadowBox(TCanvas(NIL), 'Test', 10, 10);
    end,
    EAssertionFailed);
end;


{ DrawTextShadow3DSoft Tests }

procedure TTestGraphText.TestDrawTextShadow3DSoft_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DSoft(FCanvas, 'Test', 10, 10);
    end);
end;


procedure TTestGraphText.TestDrawTextShadow3DSoft_NilCanvas;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextShadow3DSoft(NIL, 'Test', 10, 10);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextShadow3DSoft_EmptyText;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DSoft(FCanvas, '', 10, 10);
    end);
end;


procedure TTestGraphText.TestDrawTextShadow3DSoft_CustomShadowColor;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DSoft(FCanvas, 'Test', 10, 10, clGray);
    end);
end;


{ DrawTextShadow3DHard Tests }

procedure TTestGraphText.TestDrawTextShadow3DHard_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DHard(FCanvas, 'Test', 10, 10);
    end);
end;


procedure TTestGraphText.TestDrawTextShadow3DHard_NilCanvas;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextShadow3DHard(NIL, 'Test', 10, 10);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextShadow3DHard_WithTopShadow;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DHard(FCanvas, 'Test', 20, 20, clTextShadow, TRUE);
    end);
end;


procedure TTestGraphText.TestDrawTextShadow3DHard_WithoutTopShadow;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextShadow3DHard(FCanvas, 'Test', 20, 20, clTextShadow, FALSE);
    end);
end;


{ DrawTextOutline Tests }

procedure TTestGraphText.TestDrawTextOutline_BasicCall;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextOutline(FCanvas, 'Test', 10, 10);
    end);
end;


procedure TTestGraphText.TestDrawTextOutline_NilCanvas;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextOutline(NIL, 'Test', 10, 10);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextOutline_SolidMiddle;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextOutline(FCanvas, 'Test', 10, 10, TRUE);
    end);
end;


procedure TTestGraphText.TestDrawTextOutline_TransparentMiddle;
begin
  Assert.WillNotRaise(
    procedure
    begin
      DrawTextOutline(FCanvas, 'Test', 10, 10, FALSE);
    end);
end;


{ DrawTextXOR Tests }

procedure TTestGraphText.TestDrawTextXOR_BasicCall;
var
  Font: TFont;
begin
  Font:= TFont.Create;
  TRY
    Font.Size:= 12;

    Assert.WillNotRaise(
      procedure
      begin
        DrawTextXOR(FCanvas.Handle, Font, 'Test', 10, 10);
      end);
  FINALLY
    FreeAndNil(Font);
  END;
end;


procedure TTestGraphText.TestDrawTextXOR_NilFont;
begin
  Assert.WillRaise(
    procedure
    begin
      DrawTextXOR(FCanvas.Handle, NIL, 'Test', 10, 10);
    end,
    EAssertionFailed);
end;


procedure TTestGraphText.TestDrawTextXOR_EmptyText;
var
  Font: TFont;
begin
  Font:= TFont.Create;
  TRY
    Font.Size:= 12;

    { Empty text should not crash, just create an empty bitmap }
    Assert.WillNotRaise(
      procedure
      begin
        DrawTextXOR(FCanvas.Handle, Font, '', 10, 10);
      end);
  FINALLY
    FreeAndNil(Font);
  END;
end;


{ ShadowDownLeft Tests (deprecated) }

procedure TTestGraphText.TestShadowDownLeft_BasicCall;
begin
  {$WARNINGS OFF}  { Suppress deprecation warning }
  Assert.WillNotRaise(
    procedure
    begin
      ShadowDownLeft(FBitmap);
    end);
  {$WARNINGS ON}
end;


procedure TTestGraphText.TestShadowDownLeft_NilBitmap;
begin
  {$WARNINGS OFF}  { Suppress deprecation warning }
  Assert.WillRaise(
    procedure
    begin
      ShadowDownLeft(NIL);
    end,
    EAssertionFailed);
  {$WARNINGS ON}
end;


{ ShadowDownRight Tests (deprecated) }

procedure TTestGraphText.TestShadowDownRight_BasicCall;
begin
  {$WARNINGS OFF}  { Suppress deprecation warning }
  Assert.WillNotRaise(
    procedure
    begin
      ShadowDownRight(FBitmap);
    end);
  {$WARNINGS ON}
end;


procedure TTestGraphText.TestShadowDownRight_NilBitmap;
begin
  {$WARNINGS OFF}  { Suppress deprecation warning }
  Assert.WillRaise(
    procedure
    begin
      ShadowDownRight(NIL);
    end,
    EAssertionFailed);
  {$WARNINGS ON}
end;


{ clTextShadow constant test }

procedure TTestGraphText.TestClTextShadow_Value;
begin
  { Verify the constant has the expected value }
  Assert.AreEqual(TColor($303035), clTextShadow, 'clTextShadow should be $303035');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestGraphText);

end.
