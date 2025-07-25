UNIT janFxStretch;

{--------------------------------------------------------------------------------------------------
  JanFX Resamplers

  TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr


  Speed results:
    Orig res: 3700x2800
    Time to resize up the image 2.5x times:
                      6.62 seconds in debug    (8.8 with FastMM)
                      5.94 seconds in release

  Time per resampler:
    Resize down 5x
       Box         Time 343 ms
       Triangle    Time 561 ms
       Hermite     Time 561 ms
       Bell        Time 764 ms
       B-Spline    Time 967 ms
       Mitchell    Time 983 ms
       Lanczos3    Time 1404 ms

    Resize up 2.5x
       Box         Time 281ms
       Triangle    Time 375ms
       Hermite     Time 375ms
       Bell        Time 453ms
       B-Spline    Time 562ms
       Mitchell    Time 546ms
       Lanczos3    Time 780ms


  The best resamplers (up/down) are:
    rsHermite - good enough
    rsLanczos - about 2 times better but 2.5 times slower


  Conclusion: SmoothResize is almost as good as Stretch (a bit too soft) but is WAY much faster and takes less memory.

--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Windows, SysUtils, Vcl.Graphics, Math;

TYPE
  TFilterProc = function(Value: Single): Single;              { Type of a filter for use with Stretch }
  FilterRecord = record
   Name: string;                                              { Filter name }
   Filter: TFilterProc;                                       { Filter implementation }
   Width: Single;                                             { Suggested sampling width/radius }
  end;

 { RESIZE/RESAMPLE }
 procedure SmoothResize(Src, Dst: TBitmap);                   { Good for resize up }
 procedure Stretch_    (Src, Dst: TBitmap; Filter: TFilterProc; AWidth: Single);  deprecated 'Use SmoothResize instead.'
 procedure Grow        (src1, src2, dst: TBitmap; amount: extended; x, y: Integer);   // Expand dest to match the size of src1. then resizes the src2 and puts the output in dst Doesn't seem to work!

{ Sample filters for use with Stretch }
function BellFilter     (Value: Single): Single;
function TriangleFilter (Value: Single): Single;        // a.k.a. "Linear" or "Bilinear" filter
function BoxFilter      (Value: Single): Single;
function HermiteFilter  (Value: Single): Single;
function Lanczos3Filter (Value: Single): Single;
function MitchellFilter (Value: Single): Single;
function SplineFilter   (Value: Single): Single;

CONST
 rsBox        = 0;                       { QUALITY: 0 - DON'T USE to resize down! Quality is Ok but it has some strange artefacts (checker board) }
 rsTriangle   = 1;                       { QUALITY: 1 }
 rsBell       = 2;                       { QUALITY: 2 }
 rsBSpline    = 3;                       { QUALITY: 3 - Too smooth }
 rsMitchell   = 4;                       { QUALITY: 4 - Too smooth }
 rsHermite    = 5;                       { QUALITY: 5 - Good for shrinking (almost as good as Lanczos but 2.5x faster }
 rsLanczos    = 6;                       { QUALITY: 6 - Best when shrinking and enlarging images. February 2012 }

 rsGoodFilter = rsHermite;
 rsBestResampleFilter = rsLanczos;

 ResampleFilters: array[0..6] of FilterRecord= (
       (Name: 'Box'     ; Filter: BoxFilter;      Width: 0.5),
       (Name: 'Triangle'; Filter: TriangleFilter; Width: 1.0),
       (Name: 'Bell'    ; Filter: BellFilter;     Width: 1.5),
       (Name: 'B-Spline'; Filter: SplineFilter;   Width: 2.0),
       (Name: 'Mitchell'; Filter: MitchellFilter; Width: 2.0),
       (Name: 'Hermite' ; Filter: HermiteFilter;  Width: 1.0),
       (Name: 'Lanczos' ; Filter: Lanczos3Filter; Width: 3.0));


IMPLEMENTATION


TYPE
 TJvRGBTriple = packed record    { Used in SmoothResize }
  rgbBlue: Byte;
  rgbGreen: Byte;
  rgbRed: Byte;
 end;

CONST
 bpp = SizeOf(TJvRGBTriple);


{ Interpolator.  Used in Stretch }
TYPE
 TContributor = record               // Contributor for a pixel
  Pixel: Integer;                    // Source pixel
  Weight: Single;                    // Pixel Weight
 end;

 TContributorList = array [0 .. 0] of TContributor;
 PContributorList = ^TContributorList;

 TCList = record      // List of source pixels contributing to a destination pixel
  N: Integer;
  P: PContributorList;
 end;

 TCListList = array [0 .. 0] of TCList;
 PCListList = ^TCListList;

 TRGB = packed record
  R: Single;
  G: Single;
  B: Single;
 end;

 TColorRGB = packed record    // Physical bitmap pixel
  R: Byte;
  G: Byte;
  B: Byte;
 end;

 PColorRGB = ^TColorRGB;

 TRGBList = packed array [0 .. 0] of TColorRGB;    // Physical bitmap ScanLine (row)
 PRGBList = ^TRGBList;









procedure SmoothResize(Src, Dst: TBitmap);
{-------------+------------------------------------------------------------------------------------
              |    SmoothResize           Stretch(Hermite)      Its quality, compared to Hermite
              |------------------------------------------------------------------------------------
Resize down   |       40ms                    655ms                too sharp (barelly usable)
Resize up     |       85ms                    375ms                quite good (a bit to soft)
--------------+-----------------------------------------------------------------------------------}
VAR
  X, Y, xP, yP, yP2, xP2: Integer;
  Read, Read2: PByteArray;
  T, z, z2, iz2: Integer;
  pc: PByteArray;
  w1, w2, w3, w4: Integer;
  Col1r, Col1g, Col1b, Col2r, Col2g, Col2b: Byte;
begin
 Src.PixelFormat:= pf24bit;

 xP2 := ((Src.Width  - 1) shl 15) div Dst.Width;
 yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
 yP := 0;

 for Y := 0 to Dst.Height - 1 do
  begin
   xP := 0;
   Read := Src.ScanLine[yP shr 15];

   if yP shr 16 < Src.Height - 1
   then Read2 := Src.ScanLine[yP shr 15 + 1]
   else Read2 := Src.ScanLine[yP shr 15];
   pc := Dst.ScanLine[Y];
   z2 := yP and $7FFF;
   iz2 := $8000 - z2;

   for X := 0 to Dst.Width - 1 do
    begin
     T := xP shr 15;
     Col1r := Read[T * bpp];
     Col1g := Read[T * bpp + 1];
     Col1b := Read[T * bpp + 2];
     Col2r := Read2[T * bpp];
     Col2g := Read2[T * bpp + 1];
     Col2b := Read2[T * bpp + 2];
     z  := xP and $7FFF;
     w2 := (z * iz2) SHR 15;
     w1 := iz2 - w2;
     w4 := (z * z2) SHR 15;
     w3 := z2 - w4;
     pc[X * bpp + 2] := (Col1b * w1 + Read [(T + 1) * bpp + 2] * w2 + Col2b * w3 + Read2[(T + 1) * bpp + 2] * w4) shr 15;
     pc[X * bpp + 1] := (Col1g * w1 + Read [(T + 1) * bpp + 1] * w2 + Col2g * w3 + Read2[(T + 1) * bpp + 1] * w4) shr 15;
     pc[X * bpp    ] := (Col1r * w1 + Read2[(T + 1) * bpp]     * w2 + Col2r * w3 + Read2[(T + 1) * bpp] * w4) shr 15;
     Inc(xP, xP2);
    end;

   Inc(yP, yP2);
  end;
end;


// Expand dest to match the size of src1. then resizes the src2 and puts the output in dst
// Doesn't seem to work!
procedure Grow(src1, src2, dst: TBitmap; amount: extended; x, y: Integer);
VAR
   Bm: TBitmap;
   hr, wr: Integer;
begin
 dst.Width  := src1.Width;
 dst.Height := src1.Height;
 dst.canvas.Draw(0, 0, src1);

 wr := Round(amount * src1.Width);
 hr := Round(amount * src1.Height);

 Bm := TBitmap.create;
 TRY
  Bm.SetSize(wr, hr);
  SmoothResize(src2, Bm);
  dst.canvas.Draw(x, y, Bm);
 FINALLY
  FreeAndNil(Bm);
 END;
end;



procedure Stretch_(Src, Dst: TBitmap; Filter: TFilterProc; AWidth: Single);
 var
  xscale, yscale: Single;                  // Zoom Scale factors
  I, J, k: Integer;                        // Loop variables
  Center: Single;                          // Filter calculation variables
  Width, fscale, Weight: Single;           // Filter calculation variables
  Left, Right: Integer;                    // Filter calculation variables
  N: Integer;                              // Pixel number
  Work: TBitmap;
  Contrib: PCListList;
  RGB: TRGB;
  Color: TColorRGB;
  SourceLine, DestLine: PRGBList;
  DestPixel: PColorRGB;
  Delta, DestDelta: Integer;
  SrcWidth, SrcHeight, DstWidth, DstHeight: Integer;
 begin
  DstWidth  := Dst.Width;
  DstHeight := Dst.Height;
  SrcWidth  := Src.Width;
  SrcHeight := Src.Height;

  if (SrcWidth < 1) or (SrcHeight < 1)
  then raise Exception.Create('Source Bitmap Too Small');

  // Create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  TRY
   Work.SetSize(DstWidth, SrcHeight);

   // Improvement suggested by David Ullrich:
   if (SrcWidth = 1)
   then xscale := DstWidth / SrcWidth
   else xscale := (DstWidth - 1) / (SrcWidth - 1);

   if (SrcHeight = 1)
   then yscale := DstHeight / SrcHeight
   else yscale := (DstHeight - 1) / (SrcHeight - 1);

   // This implementation only works on 24-bit images because it uses TBitmap.ScanLine
   Src.PixelFormat := pf24bit;
   Dst.PixelFormat := Src.PixelFormat;
   Work.PixelFormat := Src.PixelFormat;

   // --------------------------------------------
   // Pre-calculate filter contributions for a row
   // -----------------------------------------------
   GetMem(Contrib, DstWidth * SizeOf(TCList));

   // Horizontal sub-sampling
   // Scales from bigger to smaller Width
   {$R-}                                             { $R-   Postul asta (Rob) explica de ce e asta aici: http://stackoverflow.com/questions/628965/delphi-accessing-data-from-dynamic-array-that-is-populated-from-an-untyped-poi }
   if xscale < 1 then
    begin
     Width := AWidth / xscale;
     fscale := 1 / xscale;
     for I := 0 to DstWidth - 1 do
      begin
       Contrib^[I].N := 0;
       GetMem(Contrib^[I].P, Trunc(Width * 2 + 1) * SizeOf(TContributor));
       Center := I / xscale;
       // Original code:
       // Left  := Ceil(Center - Width);
       // Right := Floor(Center + Width);
       Left := Floor(Center - Width);
       Right:= Ceil(Center + Width);

       for J := Left to Right do
        begin
         Weight := Filter((Center - J) / fscale) / fscale;
         if (Weight = 0)
         then Continue;
         if (J < 0)
         then N := -J
         else
          if (J >= SrcWidth)
          then N := SrcWidth - J + SrcWidth - 1
          else N := J;

         k := Contrib^[I].N;
         Contrib^[I].N := Contrib^[I].N + 1;

         Contrib^[I].P^[k].Pixel  := N;
         Contrib^[I].P^[k].Weight := Weight;
        end;
      end;
    end
   else
    // Horizontal super-sampling. Scales from smaller to bigger Width
    begin
     for I := 0 to DstWidth - 1 do
      begin
       Contrib^[I].N := 0;
       GetMem(Contrib^[I].P, Trunc(AWidth * 2 + 1) * SizeOf(TContributor));
       Center := I / xscale;

       // Original code:
       // Left := Ceil(Center - AWidth);
       // Right := Floor(Center + AWidth);
       Left := Floor(Center - AWidth);
       Right := Ceil(Center + AWidth);
       for J := Left to Right DO
        begin
         Weight := Filter(Center - J);
         if (Weight = 0)
         then Continue;
         if J < 0
         then N := -J
         else
          if J >= SrcWidth
          then N := SrcWidth - J + SrcWidth - 1
          else N := J;

         if N < 0
         then N := -N;

         k := Contrib^[I].N;
         Contrib^[I].N := Contrib^[I].N + 1;
         Contrib^[I].P^[k].Pixel := N;
         Contrib^[I].P^[k].Weight := Weight;
        end;
      end;
    end;

   // ----------------------------------------------------
   // Apply filter to sample horizontally from Src to Work
   // ----------------------------------------------------
   for k := 0 to SrcHeight - 1 do
    begin
     SourceLine := Src.ScanLine[k];
     DestPixel := Work.ScanLine[k];
     for I := 0 to DstWidth - 1 do
      begin
       RGB.R := 0;
       RGB.G := 0;
       RGB.B := 0;
       for J := 0 to Contrib^[I].N - 1 do
        begin
         Color := SourceLine^[Contrib^[I].P^[J].Pixel];
         Weight := Contrib^[I].P^[J].Weight;
         if (Weight = 0) then
          Continue;
         RGB.R := RGB.R + Color.R * Weight;
         RGB.G := RGB.G + Color.G * Weight;
         RGB.B := RGB.B + Color.B * Weight;
        end;
       if RGB.R > 255 then
        Color.R := 255
       else
        if RGB.R < 0 then
         Color.R := 0
        else
         Color.R := Round(RGB.R);
       if RGB.G > 255 then
        Color.G := 255
       else
        if RGB.G < 0 then
         Color.G := 0
        else
         Color.G := Round(RGB.G);
       if RGB.B > 255 then
        Color.B := 255
       else
        if RGB.B < 0
        then Color.B := 0
        else Color.B := Round(RGB.B);

       DestPixel^ := Color;       // Set new Pixel value
       Inc(DestPixel);  // Move on to next column
      end;
    end;

   // Free the memory allocated for horizontal filter weights
   for I := 0 to DstWidth - 1 do
    FreeMem(Contrib^[I].P);
   FreeMem(Contrib);

   // -----------------------------------------------
   // Pre-calculate filter contributions for a column
   // -----------------------------------------------
   GetMem(Contrib, DstHeight * SizeOf(TCList));
   // Vertical sub-sampling. Scales from bigger to smaller Height
   if (yscale < 1) then
    begin
     Width := AWidth / yscale;
     fscale := 1 / yscale;
     for I := 0 to DstHeight - 1 do
      begin
       Contrib^[I].N := 0;
       GetMem(Contrib^[I].P, Trunc(Width * 2 + 1) * SizeOf(TContributor));
       Center := I / yscale;
       // Original code:
       // Left := Ceil(Center - Width);
       // Right := Floor(Center + Width);
       Left := Floor(Center - Width);
       Right := Ceil(Center + Width);
       for J := Left to Right do
        begin
         Weight := Filter((Center - J) / fscale) / fscale;
         if Weight = 0 then
          Continue;
         if J < 0 then
          N := -J
         else
          if J >= SrcHeight then
           N := SrcHeight - J + SrcHeight - 1
          else
           N := J;
         k := Contrib^[I].N;
         Contrib^[I].N := Contrib^[I].N + 1;
         Contrib^[I].P^[k].Pixel := N;
         Contrib^[I].P^[k].Weight := Weight;
        end;
      end
    end
   else
    // Vertical super-sampling
    // Scales from smaller to bigger Height
    begin
     for I := 0 to DstHeight - 1 do
      begin
       Contrib^[I].N := 0;
       GetMem(Contrib^[I].P, Trunc(AWidth * 2 + 1) * SizeOf(TContributor));
       Center := I / yscale;
       // Original code:
       // Left := Ceil(Center - AWidth);
       // Right := Floor(Center + AWidth);
       Left  := Floor(Center - AWidth);
       Right := Ceil(Center + AWidth);
       for J := Left to Right do
        begin
         Weight := Filter(Center - J);
         if Weight = 0 
         then Continue;
         if J < 0 
         then N := -J
         else
          if J >= SrcHeight 
          then N := SrcHeight - J + SrcHeight - 1
          else N := J;
         k := Contrib^[I].N;
         Contrib^[I].N := Contrib^[I].N + 1;
         Contrib^[I].P^[k].Pixel := N;
         Contrib^[I].P^[k].Weight := Weight;
        end;
      end;
    end;

   // --------------------------------------------------
   // Apply filter to sample vertically from Work to Dst
   // --------------------------------------------------
   SourceLine := Work.ScanLine[0];
   if Work.Height > 1
   then Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine)
   else Delta := 0;
   DestLine := Dst.ScanLine[0];
   if Dst.Height > 1
   then DestDelta := Integer(Dst.ScanLine[1]) - Integer(DestLine)
   else DestDelta := 0;

   for k := 0 to DstWidth - 1 do
    begin
     DestPixel := pointer(DestLine);
     for I := 0 to DstHeight - 1 do
      begin
       RGB.R := 0;
       RGB.G := 0;
       RGB.B := 0;
       // Weight := 0;
       for J := 0 to Contrib^[I].N - 1 do
        begin
         Color := PColorRGB(Integer(SourceLine) + Contrib^[I].P^[J].Pixel * Delta)^;
         Weight := Contrib^[I].P^[J].Weight;
         if (Weight = 0)
         then Continue;
         RGB.R := RGB.R + Color.R * Weight;
         RGB.G := RGB.G + Color.G * Weight;
         RGB.B := RGB.B + Color.B * Weight;
        end;

       if RGB.R > 255 then
        Color.R := 255
       else
        if RGB.R < 0 then
         Color.R := 0
        else
         Color.R := Round(RGB.R);
       if RGB.G > 255 then
        Color.G := 255
       else
        if RGB.G < 0 then
         Color.G := 0
        else
         Color.G := Round(RGB.G);
       if RGB.B > 255 then
        Color.B := 255
       else
        if RGB.B < 0 then
         Color.B := 0
        else
         Color.B := Round(RGB.B);

       DestPixel^ := Color;

       Inc(PByte(DestPixel), DestDelta);      {Details: http://stackoverflow.com/questions/9151190/integer-type-cast-doesnt-work-on-delphi-64-bit }

       (*
       This is the original code but it does not complie on Win64
       {$IFDEF RTL230_UP}
       Inc(INT_PTR(DestPixel), DestDelta);
       {$ELSE}
       Inc(Integer(DestPixel), DestDelta);
       {$ENDIF}
       *)
      end;
     Inc(SourceLine, 1);
     Inc(DestLine, 1);
    end;

   // Free the memory allocated for vertical filter weights
   for I := 0 to DstHeight - 1
    DO FreeMem(Contrib^[I].P);
    {$R+}
   FreeMem(Contrib);
  FINALLY
   FreeAndNil(Work);
  END;
 end;








{--------------------------------------------------------------------------------------------------
  Filters
--------------------------------------------------------------------------------------------------}

function BellFilter(Value: Single): Single;
begin
 if Value < 0
 then Value := -Value;

 if Value < 0.5
 then Result := 0.75 - Sqr(Value)
 else
  if Value < 1.5
  then
   begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
   end
  else Result := 0;
end;


// Box filter a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable results with this filter for subsampling.
function BoxFilter(Value: Single): Single;
begin
 if (Value > -0.5) and (Value <= 0.5)
 then Result := 1
 else Result := 0;
end;


function HermiteFilter(Value: Single): Single; {inline;     Time without inline: 6693   Time with inline: 6630 }
begin
 // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
 if Value < 0
 then Value := -Value;

 if Value < 1
 then Result := (2 * Value - 3) * Sqr(Value) + 1
 else Result := 0;
end;


function Lanczos3Filter(Value: Single): Single;

 function SinC(Value: Single): Single;
  begin
   if Value <> 0 then
    begin
     Value := Value * Pi;
     Result := Sin(Value) / Value;
    end
   else
    Result := 1;
  end;

begin
 if Value < 0 then Value := -Value;

 if Value < 3
 then Result := SinC(Value) * SinC(Value / 3)
 else Result := 0;
end;


function MitchellFilter(Value: Single): Single;
CONST
   B = 1 / 3;
   C = 1 / 3;
VAR T: Single;
begin
 if Value < 0 then
  Value := -Value;
 T := Sqr(Value);
 if Value < 1 then
  begin
   Value := (((12 - 9 * B - 6 * C) * (Value * T)) + ((-18 + 12 * B + 6 * C) * T) + (6 - 2 * B));
   Result := Value / 6;
  end
 else
  if Value < 2 then
   begin
    Value := (((-1 * B - 6 * C) * (Value * T)) + ((6*B + 30*C) * T) + ((-12 * B - 48 * C) * Value) + (8 * B + 24 * C));
    Result := Value / 6;
   end
  else
   Result := 0;
end;


// B-spline filter
function SplineFilter(Value: Single): Single;
VAR T: Single;
begin
 if Value < 0 then
  Value := -Value;
 if Value < 1 then
  begin
   T := Sqr(Value);
   Result := 0.5 * T * Value - T + 2 / 3;
  end
 else
  if Value < 2 then
   begin
    Value := 2 - Value;
    Result := 1 / 6 * Sqr(Value) * Value;
   end
  else
   Result := 0;
end;


// Triangle filter (a.k.a. "Linear" or "Bilinear" filter)
function TriangleFilter(Value: Single): Single;
begin
 if Value < 0 then Value := -Value;
 if Value < 1
 then Result := 1 - Value
 else Result := 0;
end;


end.
