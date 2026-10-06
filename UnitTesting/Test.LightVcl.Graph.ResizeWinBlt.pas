unit Test.LightVcl.Graph.ResizeWinBlt;

{=============================================================================================================
   2026.10.05
   Unit tests for LightVcl.Graph.ResizeWinBlt.pas
   Tests Windows StretchBlt-based image resizing (StretchF and Stretch).

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics;

type
  [TestFixture]
  TTestResizeWinBlt = class
  private
    FBitmap: TBitmap;
    procedure CreateTestBitmap(Width, Height: Integer);
    procedure CheckStretchesRed(SrcWidth, SrcHeight: Integer);
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
    procedure TestStretchF_EmptySource;

    { StretchF - Sources under 12 px (COLORONCOLOR mode) }
    [Test]
    procedure TestStretchF_TinyWidth;

    [Test]
    procedure TestStretchF_TinyHeight;

    [Test]
    procedure TestStretchF_TinyBoth;

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

    { Pixel content }
    [Test]
    procedure TestStretchF_CopiesPixels;

    { Palette }
    [Test]
    procedure TestStretchF_KeepsSourcePalette;

    { Thread safety: no TBitmapCanvas DC may be left for the main thread's FreeMemoryContexts }
    [Test]
    procedure TestStretchF_ReleasesSourceCanvasDC;

    [Test]
    procedure TestStretchF_ResultHasNoCanvasDC;

    [Test]
    procedure TestStretch_OnWorkerWhileMainFreesContexts;
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


{ An empty source raises in every build (not an Assert, which the Release build strips) }
procedure TTestResizeWinBlt.TestStretchF_EmptySource;
begin
  CreateTestBitmap(0, 0);
  Assert.WillRaise(
    procedure
    begin
      StretchF(FBitmap, 50, 50);
    end,
    Exception);
end;


{ StretchF - Sources under 12 px
  A user image can be that small, so StretchF must stretch it, not refuse it. }

procedure TTestResizeWinBlt.CheckStretchesRed(SrcWidth, SrcHeight: Integer);
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(SrcWidth, SrcHeight);
  FBitmap.Canvas.Brush.Color:= clRed;
  FBitmap.Canvas.FillRect(Rect(0, 0, SrcWidth, SrcHeight));

  Result:= StretchF(FBitmap, 50, 40);
  TRY
    Assert.AreEqual(50, Result.Width,  'Result width');
    Assert.AreEqual(40, Result.Height, 'Result height');
    Assert.AreEqual(Integer(clRed), Integer(Result.Canvas.Pixels[25, 20]), 'Center pixel must keep the source color');
    Assert.AreEqual(Integer(clRed), Integer(Result.Canvas.Pixels[49, 39]), 'Bottom-right pixel must keep the source color');
  FINALLY
    FreeAndNil(Result);
  END;
end;


procedure TTestResizeWinBlt.TestStretchF_TinyWidth;
begin
  CheckStretchesRed(10, 100);
end;


{ 192x11 is the size of a real BioniX thumbnail of a very wide picture }
procedure TTestResizeWinBlt.TestStretchF_TinyHeight;
begin
  CheckStretchesRed(192, 11);
end;


procedure TTestResizeWinBlt.TestStretchF_TinyBoth;
begin
  CheckStretchesRed(1, 1);
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
  { 12x12 is the smallest source stretched in HALFTONE mode }
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


{ Pixel content }

procedure TTestResizeWinBlt.TestStretchF_CopiesPixels;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(100, 80);
  FBitmap.Canvas.Brush.Color:= clRed;
  FBitmap.Canvas.FillRect(Rect(0, 0, 100, 80));

  Result:= StretchF(FBitmap, 40, 30);
  TRY
    Assert.AreEqual(Integer(clRed), Integer(Result.Canvas.Pixels[20, 15]), 'The stretched image must keep the source color');
    Assert.AreEqual(Integer(clRed), Integer(Result.Canvas.Pixels[0, 0]),   'Top-left pixel must keep the source color');
  FINALLY
    FreeAndNil(Result);
  END;
end;


{ Palette
  A pf1bit bitmap starts with no palette handle, so the first read of its Palette property runs TBitmap.PaletteNeeded, which selects the DIB into a DC of its own to read the color table.
  StretchF must read the palette BEFORE it selects the bitmap into its source DC (a bitmap sits in one DC at a time).
  In the other order that select fails and GetDIBColorTable reads the black and white table of the DC's default 1x1 bitmap (measured on Windows 11), so the source keeps a black and white palette.
  The source here gets a red and green color table, so the wrong palette cannot pass for the right one. }
procedure TTestResizeWinBlt.TestStretchF_KeepsSourcePalette;
VAR
  Source, Result: TBitmap;
  DC: HDC;
  OldBitmap: HGDIOBJ;
  Colors: array[0..1] of TRGBQuad;
  Entries: array[0..255] of TPaletteEntry;
  Count: UINT;
begin
  Source:= TBitmap.Create;
  TRY
    Source.PixelFormat:= pf1bit;
    Source.SetSize(64, 64);

    { Red and green color table, written through a private DC. Neither Source.Handle nor this DC builds the palette, so StretchF is the first to read it. }
    FillChar(Colors, SizeOf(Colors), 0);
    Colors[0].rgbRed  := 255;
    Colors[1].rgbGreen:= 255;
    DC:= CreateCompatibleDC(0);
    TRY
      OldBitmap:= SelectObject(DC, Source.Handle);
      Assert.IsTrue(OldBitmap <> 0, 'Precondition: the source DIB must go into the test DC');
      TRY
        Assert.AreEqual(2, Integer(SetDIBColorTable(DC, 0, 2, Colors)), 'Precondition: SetDIBColorTable must set 2 entries');
      FINALLY
        SelectObject(DC, OldBitmap);
      END;
    FINALLY
      DeleteDC(DC);
    END;

    Result:= StretchF(Source, 32, 32);
    TRY
      Assert.IsFalse(Source.IgnorePalette, 'StretchF must not make the source ignore its palette');
      Count:= GetPaletteEntries(Source.Palette, 0, 256, Entries);
      Assert.AreEqual(2, Integer(Count), 'The source palette must hold the 2 entries of its color table');
      Assert.AreEqual(255, Integer(Entries[0].peRed),   'Palette entry 0 must be the red of the color table, not black');
      Assert.AreEqual(0,   Integer(Entries[0].peGreen), 'Palette entry 0 must be the red of the color table');
      Assert.AreEqual(0,   Integer(Entries[1].peRed),   'Palette entry 1 must be the green of the color table, not white');
      Assert.AreEqual(255, Integer(Entries[1].peGreen), 'Palette entry 1 must be the green of the color table');
    FINALLY
      FreeAndNil(Result);
    END;
  FINALLY
    FreeAndNil(Source);
  END;
end;


{ Thread safety
  The main thread calls Vcl.Graphics.FreeMemoryContexts after every message; it frees the DC of every TBitmapCanvas
  in its list. A worker that frees a bitmap whose canvas still owns a DC races with it: FastMM caught the main thread
  writing into a freed TBitmapCanvas on 2026-10-05 (thumbnail thread: ExtractThumbnailJpg -> StretchProport -> Stretch -> StretchF).
  So StretchF must leave no canvas DC behind, neither on the source nor on the result. }

{ Fills a pf24bit bitmap through ScanLine, so no TBitmapCanvas is created }
procedure FillRed24(BMP: TBitmap);
VAR
  x, y: Integer;
  Pixel: PRGBTriple;
begin
  for y:= 0 to BMP.Height-1 do
    begin
      Pixel:= BMP.ScanLine[y];
      for x:= 0 to BMP.Width-1 do
        begin
          Pixel.rgbtRed  := 255;
          Pixel.rgbtGreen:= 0;
          Pixel.rgbtBlue := 0;
          Inc(Pixel);
        end;
    end;
end;


function GdiObjectCount: Integer;
begin
  Result:= GetGuiResources(GetCurrentProcess, GR_GDIOBJECTS);
end;


{ The caller drew on the source, so its canvas owns a DC. StretchF must release that DC (TBitmap.GetHandle does it under the canvas lock). }
procedure TTestResizeWinBlt.TestStretchF_ReleasesSourceCanvasDC;
VAR
  Result: TBitmap;
begin
  CreateTestBitmap(200, 100);
  Assert.IsTrue(FBitmap.Canvas.HandleAllocated, 'Precondition: FillRect gave the source canvas a DC');

  Result:= StretchF(FBitmap, 50, 25);
  TRY
    Assert.IsFalse(FBitmap.Canvas.HandleAllocated, 'StretchF must not leave a DC on the source canvas');
    Assert.AreEqual(50, Result.Width);
  FINALLY
    FreeAndNil(Result);
  END;
end;


{ StretchF may add exactly one GDI object: the bitmap of the result. A second one would be a canvas DC. }
procedure TTestResizeWinBlt.TestStretchF_ResultHasNoCanvasDC;
VAR
  Source, Result: TBitmap;
  Before, After: Integer;
begin
  Source:= TBitmap.Create;
  TRY
    Source.PixelFormat:= pf24bit;
    Source.SetSize(200, 100);
    FillRed24(Source);

    Result:= StretchF(Source, 50, 25);   { Warm-up: GDI may create objects of its own on the first HALFTONE StretchBlt }
    FreeAndNil(Result);

    Before:= GdiObjectCount;
    Result:= StretchF(Source, 50, 25);
    TRY
      After:= GdiObjectCount;
      Assert.AreEqual(1, After - Before, 'StretchF must create only the result bitmap, no device context');
    FINALLY
      FreeAndNil(Result);
    END;
    Assert.AreEqual(Before, GdiObjectCount, 'StretchF must not leak GDI objects');
  FINALLY
    FreeAndNil(Source);
  END;
end;


{ Smoke test: the main thread calls FreeMemoryContexts in a loop (as TWinControl.MainWndProc does after every message)
  while a worker stretches and frees its own bitmaps. The worker never creates a canvas (it fills through ScanLine),
  so the only canvases that could exist are the ones Stretch/StretchF create.
  What it proves: Stretch gives the right pixels on a worker thread and raises nothing while the main thread runs
  FreeMemoryContexts. What it cannot prove: that the use-after-free is gone. The race window is a few instructions
  wide, and this test project links no FastMM FullDebugMode, so a write into a freed canvas goes unreported.
  The deterministic guards against a StretchF that creates canvas DCs again are TestStretchF_ReleasesSourceCanvasDC and TestStretchF_ResultHasNoCanvasDC. }
procedure TTestResizeWinBlt.TestStretch_OnWorkerWhileMainFreesContexts;
CONST
  Rounds = 300;
VAR
  Worker: TThread;
  BadPixels: Integer;
  WorkerError: string;
begin
  BadPixels:= 0;
  WorkerError:= '';

  Worker:= TThread.CreateAnonymousThread(
    procedure
    VAR
      i: Integer;
      BMP: TBitmap;
      Pixel: PRGBTriple;
    begin
      TRY
        for i:= 1 to Rounds do
          begin
            BMP:= TBitmap.Create;
            TRY
              BMP.PixelFormat:= pf24bit;
              BMP.SetSize(64, 64);
              FillRed24(BMP);

              Stretch(BMP, 20, 20);

              Pixel:= BMP.ScanLine[10];
              Inc(Pixel, 10);
              if (Pixel.rgbtRed <> 255) OR (Pixel.rgbtGreen <> 0) OR (Pixel.rgbtBlue <> 0)
              then Inc(BadPixels);
            FINALLY
              FreeAndNil(BMP);
            END;
          end;
      EXCEPT
        on E: Exception do
          WorkerError:= E.ClassName + ': ' + E.Message;   { Reported by the Assert below }
      END;
    end);

  Worker.FreeOnTerminate:= FALSE;
  TRY
    Worker.Start;
    while NOT Worker.Finished do
      FreeMemoryContexts;
    Worker.WaitFor;
  FINALLY
    FreeAndNil(Worker);
  END;

  Assert.AreEqual('', WorkerError, 'The worker raised');
  Assert.AreEqual(0, BadPixels, 'Stretched pixels on the worker must keep the source color');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestResizeWinBlt);

end.
