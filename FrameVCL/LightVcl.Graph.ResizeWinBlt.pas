UNIT LightVcl.Graph.ResizeWinBlt;

{=============================================================================================================
   Gabriel Moraru
   2026.10.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Image resizer using Windows StretchBlt API

   TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr

-------------------------------------------------------------------------------------------------------------}
INTERFACE

USES
   Winapi.Windows, System.SysUtils, Vcl.Graphics,
   LightVcl.Graph.Bitmap;

 { Not proportional }
 function  StretchF         (BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;                            { Best of all algorithms. 2019.08 }
 procedure Stretch          (BMP: TBitmap; OutWidth, OutHeight: Integer);


IMPLEMENTATION




{-------------------------------------------------------------------------------------------------------------
   Based on MS Windows StretchBlt
   BEST quality resampler (see tester)

   Zoom: In/Out
   Keep aspect ratio: No
   Stretch provided in: pixels

   Resize down: VERY smooth. Better than JanFX.SmoothResize.
   Resize up: better (sharper) than JanFX.SmoothResize
   Time: similar to JanFx

   Note: BitBlt only does copy, NO STRETCH.

   Returns: A NEW bitmap that caller must free.
   Alpha is lost: HALFTONE writes 0 into the alpha byte.
   Raises if the source is empty (0 pixels wide or high). A source under 12 pixels is stretched in COLORONCOLOR
   mode instead of HALFTONE (see MinHalftoneSize).

   Thread safety (safe to call from a worker thread):
     StretchF copies through two private memory DCs and never creates a TBitmapCanvas DC, neither for BMP nor
     for Result. Source: c:\Delphi\Delphi 13\source\vcl\Vcl.Graphics.pas (Delphi 13.1).
     - TWinControl.MainWndProc (Vcl.Controls.pas) calls "FreeMemoryContexts;" after every message.
       FreeMemoryContexts runs "if TryLock then try FreeContext; finally Unlock; end" on every canvas that owns a DC.
       TBitmapCanvas.FreeContext sets "Handle := 0", then DeleteDC, then calls Unlock, so the main thread unlocks twice.
     - A worker that frees its bitmap in that window finds "if FHandle <> 0 then" FALSE in TBitmapCanvas.FreeContext,
       takes no lock, and frees the canvas. The main thread's two Unlock calls then write into freed memory
       (FastMM, 2026-10-05: "TBitmapCanvas modified after free", one byte each at offsets 36 and 52).
     - BMP.Handle is used, not BMP.Canvas.Handle: TBitmap.GetHandle starts with "FreeContext;", which releases
       the DC of an existing BMP.Canvas under the canvas lock and takes it out of the list FreeMemoryContexts walks.
       Cost: BMP.Handle also calls TBitmap.Changing on BMP, as every ScanLine read does. No pixel changes, but a shared
       image is copied, a DDB drops its DIB section, biClrUsed/biClrImportant become 0 and the saved stream is freed.

   https://msdn.microsoft.com/en-us/library/windows/desktop/dd162950(v=vs.85).aspx
-------------------------------------------------------------------------------------------------------------}
function StretchF(BMP: TBitmap; OutWidth, OutHeight: Integer): TBitmap;
CONST
   { An older version of this unit recorded that HALFTONE StretchBlt crashes or draws artifacts on sources under
     10-12 pixels [UNVERIFIED - a solid-color test on Windows 11, Win32 and Win64, did not reproduce it].
     So a smaller source is stretched in COLORONCOLOR mode. A user image can be that small (a 192x11 thumbnail of
     a very wide picture), so this is not an Assert. }
   MinHalftoneSize = 12;
VAR
   SrcDC, DestDC: HDC;
   SrcBitmap: HBITMAP;
   OldSrcBitmap, OldDestBitmap: HGDIOBJ;
   SrcPalette, OldSrcPalette: HPALETTE;
begin
 Assert(BMP <> NIL, 'StretchF: BMP parameter cannot be nil');
 Assert(OutWidth > 0, 'StretchF: OutWidth must be > 0');
 Assert(OutHeight > 0, 'StretchF: OutHeight must be > 0');
 if (BMP.Width < 1) OR (BMP.Height < 1)
 then raise Exception.Create('StretchF: the source bitmap is empty ('+ IntToStr(BMP.Width)+ 'x'+ IntToStr(BMP.Height)+ ')');

  Result:= TBitmap.Create;
 TRY
  { Preserve the pixel format only for direct-color formats. A FRESH pf1/4/8bit target gets a stock
    color table (pf8bit: the halftone palette, pf4bit: the 16-color system palette, pf1bit: black and white),
    NOT the source's palette, so StretchBlt would remap every color through the wrong color table -> posterized
    output. Same whitelist as FlipRight (LightVcl.Graph.FX). }
  if BMP.PixelFormat in [pf15bit, pf16bit, pf24bit, pf32bit]
  then Result.PixelFormat:= BMP.PixelFormat
  else Result.PixelFormat:= pf24bit;
  SetLargeSize(Result, OutWidth, OutHeight);

  { Same palette handling as TBitmapCanvas.CreateHandle, in the same order: the palette is read BEFORE the bitmap
    goes into SrcDC. BMP.Palette may run TBitmap.PaletteNeeded, which selects the DIB into a DC of its own to read
    its color table, and a bitmap can be selected into only one DC at a time (SelectObject docs).
    In the other order that select fails, GetDIBColorTable reads the black and white table of the DC's default 1x1 bitmap instead (measured on Windows 11), and the caller's bitmap keeps that wrong palette. }
  SrcBitmap := BMP.Handle;
  SrcPalette:= BMP.Palette;

  { # Source DC }
  SrcDC:= CreateCompatibleDC(0);
  if SrcDC = 0 then RaiseLastOSError;
  TRY
    OldSrcBitmap:= SelectObject(SrcDC, SrcBitmap);
    if OldSrcBitmap = 0 then RaiseLastOSError;
    TRY
      OldSrcPalette:= 0;
      if SrcPalette <> 0 then
       begin
        OldSrcPalette:= SelectPalette(SrcDC, SrcPalette, TRUE);
        RealizePalette(SrcDC);
       end;
      TRY
        { # Destination DC }
        DestDC:= CreateCompatibleDC(0);
        if DestDC = 0 then RaiseLastOSError;
        TRY
          OldDestBitmap:= SelectObject(DestDC, Result.Handle);
          if OldDestBitmap = 0 then RaiseLastOSError;
          TRY
            if (BMP.Width >= MinHalftoneSize) AND (BMP.Height >= MinHalftoneSize) then
             begin
              SetStretchBltMode(DestDC, HALFTONE);
              SetBrushOrgEx(DestDC, 0, 0, NIL);
             end
            else
              SetStretchBltMode(DestDC, COLORONCOLOR);

            if NOT StretchBlt(DestDC, 0, 0, Result.Width, Result.Height,
                              SrcDC,  0, 0, BMP.Width, BMP.Height, SRCCOPY)
            then RaiseLastOSError;   { Else the caller gets the all-white bitmap that SetLargeSize made, and it looks like a valid result }

            { Result is a DIB section: GDI must finish drawing before anybody reads its bits (CreateDIBSection docs).
              GDI may hold a call that returns BOOL, as StretchBlt does, in a batch; the failure of a batched call is reported only by GdiFlush (GdiFlush docs). }
            if NOT GdiFlush then RaiseLastOSError;
          FINALLY
            SelectObject(DestDC, OldDestBitmap);
          END;
        FINALLY
          DeleteDC(DestDC);
        END;
      FINALLY
        if OldSrcPalette <> 0
        then SelectPalette(SrcDC, OldSrcPalette, TRUE);
      END;
    FINALLY
      SelectObject(SrcDC, OldSrcBitmap);
    END;
  FINALLY
    DeleteDC(SrcDC);
  END;
 EXCEPT
  FreeAndNil(Result);
  RAISE;
 END;
end;


{ Uses MS Windows StretchBlt. See StretchF for the empty and small source cases. }
procedure Stretch(BMP: TBitmap; OutWidth, OutHeight: Integer);
VAR Temp: TBitmap;
begin
  Assert(BMP <> NIL, 'Stretch: BMP parameter cannot be nil');

  Temp:= StretchF(BMP, OutWidth, OutHeight);
  TRY
    BMP.Assign(Temp);
  FINALLY
    FreeAndNil(Temp);
  END;
end;


end.
