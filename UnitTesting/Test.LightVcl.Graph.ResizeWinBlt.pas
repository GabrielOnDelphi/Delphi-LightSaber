unit Test.LightVcl.Graph.ResizeWinBlt;

{=============================================================================================================
   2026.03.10
   Unit tests for LightVcl.Graph.ResizeWinBlt.pas
   Tests Windows StretchBlt-based image resizing (StretchF and Stretch).

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
  TTestResizeWinBlt = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { StretchF - Parameter Validation }
    [Test]
    procedure TestStretchF_NilBitmap;

    [Test]
    procedure TestStretchF_ZeroWidth;

    [Test]
    procedure TestStretchF_ZeroHeight;

    [Test]
    procedure TestStretchF_TooSmallWidth;

    [Test]
    procedure TestStretchF_TooSmallHeight;

    [Test]
    procedure TestStretchF_BothDimensionsTooSmall;

    { StretchF - Normal Operation }
    [Test]
    procedure TestStretchF_ResizeDown;

    [Test]
    procedure TestStretchF_ResizeUp;

    [Test]
    procedure TestStretchF_ReturnsNewBitmap;

    [Test]
    procedure TestStretchF_OriginalUnchanged;

    [Test]
    procedure TestStretchF_ResultHasCorrectDimensions;

    [Test]
    procedure TestStretchF_PreservesPixelFormat;

    [Test]
    procedure TestStretchF_MinValidSize;

    { Stretch - Procedure Version }
    [Test]
    procedure TestStretch_NilBitmap;

    [Test]
    procedure TestStretch_ResizeDown;

    [Test]
    procedure TestStretch_ResizeUp;
  end;


IMPLEMENTATION

USES
  LightVcl.Graph.ResizeWinBlt;


procedure TTestResizeWinBlt.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 200;
  FBitmap.Height:= 100;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
end;


procedure TTestResizeWinBlt.TearDown;
begin
  FreeAndNil(FBitmap);
end;


procedure TTestResizeWinBlt.CreateTestBitmap(Width, Height: Integer);
begin
  FreeAndNil(FBitmap);
  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= Width;
  FBitmap.Height:= Height;
  FBitmap.Canvas.Brush.Color:= clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
end;


{ StretchF - Parameter Validation }

procedure TTestResizeWinBlt.TestStretchF_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchF(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretchF_ZeroWidth;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 0, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretchF_ZeroHeight;
begin
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 100, 0);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretchF_TooSmallWidth;
begin
  { StretchF asserts for source images under 12 pixels — caller must validate. }
  CreateTestBitmap(10, 100);
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 50, 50);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretchF_TooSmallHeight;
begin
  CreateTestBitmap(100, 5);
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 50, 50);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretchF_BothDimensionsTooSmall;
begin
  CreateTestBitmap(8, 8);
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 50, 50);
    end,
    EAssertionFailed);
end;


{ StretchF - Normal Operation }

procedure TTestResizeWinBlt.TestStretchF_ResizeDown;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(400, 300);

  Result:= StretchF(FBitmap, 200, 150);
  TRY
    Assert.AreEqual(200, Result.Width, 'Width should be 200');
    Assert.AreEqual(150, Result.Height, 'Height should be 150');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_ResizeUp;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(100, 100);

  Result:= StretchF(FBitmap, 300, 300);
  TRY
    Assert.AreEqual(300, Result.Width, 'Width should be 300');
    Assert.AreEqual(300, Result.Height, 'Height should be 300');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_ReturnsNewBitmap;
VAR
  Result: TBitmap;
begin
  Result:= StretchF(FBitmap, 100, 50);
  TRY
    Assert.IsNotNull(Result, 'Should return a new bitmap');
    Assert.AreNotEqual(Pointer(FBitmap), Pointer(Result), 'Should be a different object');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_OriginalUnchanged;
VAR
  OrigWidth, OrigHeight: Integer;
  Result: TBitmap;
begin
  OrigWidth:= FBitmap.Width;
  OrigHeight:= FBitmap.Height;

  Result:= StretchF(FBitmap, 50, 50);
  TRY
    Assert.AreEqual(OrigWidth, FBitmap.Width, 'Original width should be unchanged');
    Assert.AreEqual(OrigHeight, FBitmap.Height, 'Original height should be unchanged');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_ResultHasCorrectDimensions;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(500, 400);

  Result:= StretchF(FBitmap, 250, 200);
  TRY
    Assert.AreEqual(250, Result.Width, 'Result should have exact target width');
    Assert.AreEqual(200, Result.Height, 'Result should have exact target height');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_PreservesPixelFormat;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(200, 200);
  FBitmap.PixelFormat:= pf32bit;

  Result:= StretchF(FBitmap, 100, 100);
  TRY
    Assert.AreEqual(pf32bit, Result.PixelFormat, 'Pixel format should be preserved');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_MinValidSize;
VAR
  Result: TBitmap;
begin
  { 12x12 is the minimum valid size }
  CreateTestBitmap(12, 12);

  Result:= StretchF(FBitmap, 50, 50);
  TRY
    Assert.IsNotNull(Result, 'Should succeed with 12x12 source');
    Assert.AreEqual(50, Result.Width);
    Assert.AreEqual(50, Result.Height);
  FINALLY
    FreeAndNil(Result);
  END;
end;


{ Stretch - Procedure Version }

procedure TTestResizeWinBlt.TestStretch_NilBitmap;
begin
  Assert.WillRaise(
    procedure
    begin
      Stretch(NIL, 100, 100);
    end,
    EAssertionFailed);
end;


procedure TTestResizeWinBlt.TestStretch_ResizeDown;
begin
  CreateTestBitmap(400, 300);

  Stretch(FBitmap, 200, 150);

  Assert.AreEqual(200, FBitmap.Width, 'Width should be 200');
  Assert.AreEqual(150, FBitmap.Height, 'Height should be 150');
end;


procedure TTestResizeWinBlt.TestStretch_ResizeUp;
begin
  CreateTestBitmap(100, 100);

  Stretch(FBitmap, 300, 300);

  Assert.AreEqual(300, FBitmap.Width, 'Width should be 300');
  Assert.AreEqual(300, FBitmap.Height, 'Height should be 300');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeWinBlt);

end.
