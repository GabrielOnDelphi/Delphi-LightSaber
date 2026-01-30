UNIT LightVcl.Graph.Bitmap;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Basic BMP functions:
     - Bitmap creation and clearing
     - Text centering on bitmaps
     - Canvas manipulation (enlarge, center)
     - RAM size estimation
     - Image orientation/aspect ratio detection
     - Scale classification (tiny/small/large)

   External dependencies: None
----------------------------------------------------------------------------------------------------
   Also see:
     Must try this library: http://www.soft-gems.net/index.php/libs/graphicex-library
     SIMMD INSTRUCTIONS: https://blog.grijjy.com/2017/07/10/simd-assembly-optimization/
     Vcl.GraphUtil.pas (Embarcadero)
---------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.Types, Vcl.ExtCtrls, System.SysUtils, Vcl.Graphics, System.Classes, LightVcl.Graph.FX;


TYPE
  RFont= record
     Name  : string;
     Size  : Integer;
     Color : TColor;
     procedure Clear;                                                       overload;
     procedure Clear(CONST aName: string; aSize: Integer; aColor: TColor);  overload;
     procedure AssignTo(Font: TFont);
  end;


{-------------------------------------------------------------------------------------------------------------
   CREATE BITMAP
-------------------------------------------------------------------------------------------------------------}
 function  CreateBitmap      (Width, Height: Integer; PixelFormat: TPixelFormat= pf24bit): TBitmap;
 function  CreateBlankBitmap (Width, Height: Integer; BkgClr: TColor= clBlack; PixelFormat: TPixelFormat= pf24bit): TBitmap; // old name: GetBlankImage
 procedure SetLargeSize      (BMP: TBitmap; CONST Width, Height: Integer);


{-------------------------------------------------------------------------------------------------------------
   CLEAR BITMAP
-------------------------------------------------------------------------------------------------------------}
 procedure ClearImage        (Img: TImage);
 procedure ClearBitmap       (BMP: TBitmap);
 procedure FillBitmap        (BMP: TBitmap; Color: TColor);


{-------------------------------------------------------------------------------------------------------------
   MANIPULATION
-------------------------------------------------------------------------------------------------------------}
 procedure CenterBitmap      (Source, InDest: TBitmap);                                                { Center Source into Dest, without resizing any of them. The source can be bigger than destination or other way around. }
 procedure EnlargeCanvas     (BMP: TBitmap; NewWidth, NewHeight: Integer; CanvasFillColor: TColor);    { Enlarge the canvas of the specified image and put the original image in the center of the new canvas }


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}
 procedure CenterText        (BMP: TBitmap; CONST Text: string);                                                                overload;
 procedure CenterText        (BMP: TBitmap; CONST Text: string; aFont: RFont);                                                  overload;
 procedure CenterText        (BMP: TBitmap; CONST Text: string; CONST FontName: string; FontSize: Integer; FontColor: TColor);  overload;


{-------------------------------------------------------------------------------------------------------------
   GET RAM SIZE
-------------------------------------------------------------------------------------------------------------}
 function  PredictBitmapRamSize(NewWidth, NewHeight: Integer): Cardinal;                     overload;
 function  PredictBitmapRamSize(BMP: TBitmap; CONST NewWidth, NewHeight: Integer): Cardinal; overload;   { Get the size of the bitmap without actually increasing its size }
 function  GetBitmapRamSize    (BMP: TBitmap): Int64;


{-------------------------------------------------------------------------------------------------------------
   Orientation
-------------------------------------------------------------------------------------------------------------}
 TYPE
   TImgOrientation= (orLandscape, orPortrait, orSquare);

   TImageScale = (isTiny,     { Images with area  < TinyThreshold% of the orig image }
                  isSmall,    { Images with area  < 100% of the orig image}
                  isLarge);   { Images with area >= 100% of the orig image }

 function IsPanoramic       (BMP: TBitmap): Boolean;              overload;
 function IsPanoramic       (Width, Height: Integer): Boolean;    overload;

 function GetImageScale     (InputImage, DesktopSize: TBitmap;  Tile: RTileParams): TImageScale;  overload;
 function GetImageScale     (InputImage: TBitmap; DesktopWidth, DesktopHeight: Integer; Tile: RTileParams): TImageScale;  overload;

 function AspectIsSmaller   (SrcBMP: TBitmap; CONST Width, Height: Integer): Boolean;     overload; { Compare the aspect ratio of the specified image with the AR of the monitor (for example) }
 function AspectIsSmaller   (Width, Height, SrcWidth, SrcHeight: Integer): Boolean;       overload;
 function AspectOrientation (BMP: TBitmap): TImgOrientation;                              overload;
 function AspectOrientation (Width, Height: Integer): TImgOrientation;                    overload;
 function IsLandscape       (Width, Height: Integer): Boolean;



IMPLEMENTATION

USES
   LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs, LightCore.Math, LightVcl.Graph.Loader.Resolution;



{--------------------------------------------------------------------------------------------------
   BMP UTILS
--------------------------------------------------------------------------------------------------}

procedure SetLargeSize(BMP: TBitmap; CONST Width, Height: Integer);
begin
 { Protect against empty images }
 if BMP = NIL
 then raise Exception.Create('SetLargeSize: BMP parameter cannot be nil');

 if Width <= 0
 then raise Exception.Create('SetLargeSize: Width parameter is invalid: ' + IntToStr(Width));

 if Height <= 0
 then raise Exception.Create('SetLargeSize: Height parameter is invalid: ' + IntToStr(Height));

 VAR RequiredRam:= PredictBitmapRamSize(BMP, Width, Height);

 TRY
   BMP.SetSize(Width, Height);
 EXCEPT
   On EOutOfMemory DO
      { With size over 1GB it might start to fail in some computers because of 32 bit mem limit/fragmentation limit }
      MessageWarning('Cannot set bitmap size to: '+ IntToStr(Width)+ ' x '+ IntToStr(Height)+ 'pixels.'
             +CRLFw+'Required RAM: '+ FormatBytes(RequiredRam, 2));
 END;
end;



function CreateBitmap(Width, Height: Integer; PixelFormat: TPixelFormat= pf24bit): TBitmap;
begin
 Result:= TBitmap.Create;
 Result.PixelFormat:= PixelFormat;
 TRY
   SetLargeSize(Result, Width, Height);   { Set size }
 EXCEPT
   FreeAndNil(Result); { Free result ONLY in case of error }
   RAISE;
 END;
end;



function CreateBlankBitmap(Width, Height: Integer; BkgClr: TColor= clBlack; PixelFormat: TPixelFormat= pf24bit): TBitmap;
begin
  Result:= CreateBitmap(Width, Height, PixelFormat);
  FillBitmap(Result, BkgClr);   { Make it black }
end;





procedure ClearImage(Img: TImage);
begin
  if Img = NIL
  then raise Exception.Create('ClearImage: Img parameter cannot be nil');

  Img.Picture := NIL;
end;

procedure ClearBitmap(BMP: TBitmap);
begin
  if BMP = NIL
  then raise Exception.Create('ClearBitmap: BMP parameter cannot be nil');

  BMP.Assign(NIL);
end;



{ Fill bitmap with the specified color }
procedure FillBitmap(BMP: TBitmap; Color: TColor);
begin
  if BMP = NIL
  then raise Exception.Create('FillBitmap: BMP parameter cannot be nil');

  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.Brush.Style:= bsSolid;
  BMP.Canvas.FillRect(BMP.Canvas.ClipRect);
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}

{ Uses whatever font was already set for the canvas. Text is drawn with transparent background. }
procedure CenterText(BMP: TBitmap; CONST Text: string);
begin
 if BMP = NIL
 then raise Exception.Create('CenterText: BMP parameter cannot be nil');

 if Text > '' then
  begin
   BMP.Canvas.Brush.Style:= bsClear;  { Transparent background }
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


{ Draws centered text using the specified RFont settings. Text is drawn with transparent background. }
procedure CenterText(BMP: TBitmap; CONST Text: string; aFont: RFont);
begin
 if BMP = NIL
 then raise Exception.Create('CenterText: BMP parameter cannot be nil');

 if Text > '' then
  begin
   BMP.Canvas.Brush.Style:= bsClear;  { Transparent background }
   aFont.AssignTo(BMP.Canvas.Font);
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


{ Draws centered text using specified font parameters. Text is drawn with transparent background. }
procedure CenterText(BMP: TBitmap; CONST Text: string; CONST FontName: string; FontSize: Integer; FontColor: TColor);
begin
 if BMP = NIL
 then raise Exception.Create('CenterText: BMP parameter cannot be nil');

 if Text > '' then
  begin
   BMP.Canvas.Brush.Style:= bsClear;  { Transparent background }
   BMP.Canvas.Font.Name:= FontName;
   BMP.Canvas.Font.Size:= FontSize;
   BMP.Canvas.Font.Color:= FontColor;
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   GET RAM SIZE
-------------------------------------------------------------------------------------------------------------}

{ Returns the serialized BMP file size (not raw pixel data size).
  Note: This includes BMP headers and may differ from PredictBitmapRamSize which calculates raw pixel memory. }
function GetBitmapRamSize(BMP: TBitmap): Int64;
VAR Stream: TMemoryStream;
begin
 if BMP = NIL
 then raise Exception.Create('GetBitmapRamSize: BMP parameter cannot be nil');

 Stream:= TMemoryStream.Create;
 TRY
  BMP.SaveToStream(Stream);            { Save bitmap to stream }
  Result:= Stream.Size;                { Measure stream size }
 FINALLY
  FreeAndNil(Stream);
 END;
end;


{ Returns how much RAM will that BMP required after setting its size to the new values. The result is in bytes }
function PredictBitmapRamSize(BMP: TBitmap; CONST NewWidth, NewHeight: Integer): Cardinal;
begin
 if BMP = NIL
 then raise Exception.Create('PredictBitmapRamSize: BMP parameter cannot be nil');

 Result:= (NewWidth * NewHeight) * (LightVcl.Graph.Loader.Resolution.GetBitsPerPixel(BMP) DIV 8);
end;



{ Returns how much RAM a BMP of this resolution will require.
  It supposes that the BMP will be pf24.
  The result is in bytes } //old name: BmpRequiredRAM
function PredictBitmapRamSize(NewWidth, NewHeight: Integer): Cardinal;
begin
 Result:= (NewWidth * NewHeight) * 3;  { pf24 = 3 bytes}
end;











{--------------------------------------------------------------------------------------------------
   IMAGE ORIENTATION / ASPECT
--------------------------------------------------------------------------------------------------}

{ Panoramic images have width much bigger than height (aspect ratio > 4:1). }
function IsPanoramic(Width, Height: Integer): Boolean;
begin
 if Height <= 0
 then EXIT(FALSE);  { Invalid height cannot be panoramic }

 Result:= (Width > 6000) AND             { Cannot be panoramic unless the width is really big }
         ((Width / Height) > 4);         { Panoramic images have width much much bigger than height }
end;


{ Panoramic images have width much much bigger than height }
function IsPanoramic(BMP: TBitmap): Boolean;
begin
 if BMP = NIL
 then raise Exception.Create('IsPanoramic: BMP parameter cannot be nil');

 Result:= IsPanoramic(BMP.Width, BMP.Height);
end;


{ Compare the aspect ratio of the specified image with the AR of the monitor (for example).
  Returns True if source aspect ratio is narrower (taller) than the target aspect ratio. }
function AspectIsSmaller(SrcBMP: TBitmap; CONST Width, Height: Integer): Boolean;
begin
 if SrcBMP = NIL
 then raise Exception.Create('AspectIsSmaller: SrcBMP parameter cannot be nil');

 if (SrcBMP.Height <= 0) OR (Height <= 0)
 then raise Exception.Create('AspectIsSmaller: Height parameters must be greater than zero');

 Result:= (SrcBMP.Width / SrcBMP.Height) < (Width / Height);
end;



{ Compare the aspect ratio of the specified image with the AR of the monitor (for example).
  Returns True if source aspect ratio is narrower (taller) than the target aspect ratio. }
function AspectIsSmaller(Width, Height, SrcWidth, SrcHeight: Integer): Boolean;
begin
 if (SrcHeight <= 0) OR (Height <= 0)
 then raise Exception.Create('AspectIsSmaller: Height parameters must be greater than zero');

 Result:= (SrcWidth / SrcHeight) < (Width / Height);
end;



{ Returns image orientation (portrait/landscape) based on aspect ratio }
function AspectOrientation(BMP: TBitmap): TImgOrientation;              { Old name: 'Orientation' }
begin
 if BMP = NIL
 then raise Exception.Create('AspectOrientation: BMP parameter cannot be nil');

 if BMP.Width > BMP.Height
 then Result:= orLandscape
 else
   if BMP.Width < BMP.Height
   then Result:= orPortrait
   else Result:= orSquare;
end;


{ Returns image orientation (portrait/landscape) based on aspect ratio }
function AspectOrientation(Width, Height: Integer): TImgOrientation;
begin
 if Width > Height
 then Result:= orLandscape
 else
   if Width < Height
   then Result:= orPortrait
   else Result:= orSquare;
end;


{ Returns True if the image is Landscape or Square (wider than or equal to tall).
  Returns False only for Portrait orientation. }
function IsLandscape(Width, Height: Integer): Boolean;
begin
 var Orientation:= AspectOrientation(Width, Height);
 Result:= (Orientation = orLandscape) OR (Orientation = orSquare);
end;


{ Classifies an image's scale relative to a target size (typically desktop).

  Scale classifications:
    - isLarge: Image area >= 100% of desktop area (normal size, no stretching needed)
    - isSmall: Image area between TileThreshold% and 100% (can be stretched to fill)
    - isTiny:  Image area < TileThreshold% (too small to stretch without quality loss)

  Special case: Images with one dimension >= desktop dimension are never classified
  as isTiny, even if their total area is small (e.g., tall slim wallpapers). }
function GetImageScale(InputImage, DesktopSize: TBitmap; Tile: RTileParams): TImageScale;
begin
 if InputImage = NIL
 then raise Exception.Create('GetImageScale: InputImage parameter cannot be nil');

 if DesktopSize = NIL
 then raise Exception.Create('GetImageScale: DesktopSize parameter cannot be nil');

 Result:= GetImageScale(InputImage, DesktopSize.Width, DesktopSize.Height, Tile);
end;


function GetImageScale(InputImage: TBitmap; DesktopWidth, DesktopHeight: Integer; Tile: RTileParams): TImageScale;
VAR Percent: Extended;
    ImageArea, DesktopArea: Int64;
begin
 if InputImage = NIL
 then raise Exception.Create('GetImageScale: InputImage parameter cannot be nil');

 if Tile.TileThreshold >= 100
 then raise Exception.Create('GetImageScale: TileThreshold cannot be >= 100%');

 { Use Int64 to prevent overflow with large image dimensions }
 ImageArea:= Int64(InputImage.Width) * InputImage.Height;
 DesktopArea:= Int64(DesktopWidth) * DesktopHeight;
 Percent:= ProcentRepresent(ImageArea, DesktopArea);    { Percent of desktop area }

 if Percent < Tile.TileThreshold
 then Result:= isTiny
 else
   if Percent < 100{%}          { under 100% the image is small }
   then Result:= isSmall
   else Result:= isLarge;       { over 100% it is large }

 { Extra condition:
     Images that are taller/wider than the desktop cannot be declared tiny!

   Explanations:
     I have a situation when the wallpaper is super tall and slim (W=300, H=1300).
     This is classified as "tiny" because its area is very small.
     This results in the "Auto tile small images" algorithm to kick in. But when this kicks in, the "Resize" algorithm does not kick in anymore (mutually exclusive) so the image remains taller than the desktop. This result in an assertion failure in "FadeBorder":  Assert(iTop  >= 0, 'iTop.  The wallpaper is larger than the desktop!'. }
 if (Result = isTiny)
 AND ((InputImage.Width >= DesktopWidth) OR (InputImage.Height >= DesktopHeight))
 then Result:= isSmall;
end;














{-------------------------------------------------------------------------------------------------------------
   CANVAS ENLARGE
-------------------------------------------------------------------------------------------------------------}

procedure EnlargeCanvas(BMP: TBitmap; NewWidth, NewHeight: Integer; CanvasFillColor: TColor);    { Enlarge the canvas of the specified image and put the original image in the center of the new canvas }
VAR NewCanvas: TBitmap;
    iTop, iLeft: Integer;
begin
 if BMP = NIL
 then raise Exception.Create('EnlargeCanvas: BMP parameter cannot be nil');

 NewCanvas:= CreateBitmap(NewWidth, NewHeight);
 TRY
  FillBitmap(NewCanvas, CanvasFillColor);                       { Fill with CanvasFillColor }

  { Compute where I center the image  }
  iTop := (NewHeight- BMP.Height) div 2;
  iLeft:= (NewWidth - BMP.Width)  div 2;

  NewCanvas.Canvas.Draw(iLeft, iTop, BMP);
  BMP.Assign(NewCanvas);
 FINALLY
  FreeAndNil(NewCanvas);
 END;
end;



{ Centers Source bitmap into InDest bitmap without resizing either.
  Handles all size combinations:
    - If Source is smaller than InDest: Source is centered within InDest
    - If Source is larger than InDest: The center portion of Source is copied to fill InDest
    - If sizes are equal: Simple assignment

  The InDest bitmap is modified in-place with the centered content.
  Does not clear/fill the destination first - caller should prefill if needed. }
procedure CenterBitmap(Source, InDest: TBitmap);
VAR
   SrcTop, SrcLeft, SrcRight, SrcBottom: Integer;
   DestLeft, DestRight, DestTop, DestBottom: Integer;
begin
 if Source = NIL
 then raise Exception.Create('CenterBitmap: Source parameter cannot be nil');

 if InDest = NIL
 then raise Exception.Create('CenterBitmap: InDest parameter cannot be nil');

 { Fast path: same size - direct copy }
 if  (Source.Width = InDest.Width)
 AND (Source.Height= InDest.Height)
 then InDest.Assign(Source)
 else
  begin
    { Calculate vertical (Y) coordinates }
    if Source.Height < InDest.Height
    then
     begin
      { Source is shorter - center it vertically in destination }
      SrcTop:= 0;
      SrcBottom:= Source.Height;

      DestTop:= (InDest.Height- Source.Height) DIV 2;
      DestBottom:= DestTop+ Source.Height
     end
    else
     begin
      { Source is taller - take center portion of source }
      SrcTop:= (Source.Height- InDest.Height) DIV 2;
      SrcBottom:= SrcTop+ InDest.Height;

      DestTop:= 0;
      DestBottom:= InDest.Height;
     end;

    { Calculate horizontal (X) coordinates }
    if Source.Width < InDest.Width
    then
     begin
      { Source is narrower - center it horizontally in destination }
      SrcLeft := 0;
      SrcRight:= Source.Width;

      DestLeft:= (InDest.Width- Source.Width) DIV 2;
      DestRight:= DestLeft+ Source.Width;
     end
    else
     begin
      { Source is wider - take center portion of source }
      SrcLeft := (Source.Width - InDest.Width) DIV 2;
      SrcRight:= SrcLeft+ InDest.Width;

      DestLeft:= 0;
      DestRight:= InDest.Width;
     end;

   InDest.Canvas.CopyRect(
        Rect(DestLeft, DestTop, DestRight, DestBottom),                                            {Dest}
        Source.Canvas,                                                                             {Source canvas}
        Rect(SrcLeft, SrcTop, SrcRight, SrcBottom));                                               {Source}
  end;
end;






{ RFont - Lightweight font record for basic text rendering settings }

{ Resets font to default values (Verdana, 10pt, lime green) }
procedure RFont.Clear;
begin
  Name  := 'Verdana';
  Size  := 10;
  Color := clLime;
end;


{ Initializes font with specified values }
procedure RFont.Clear(CONST aName: string; aSize: Integer; aColor: TColor);
begin
  Name  := aName;
  Size  := aSize;
  Color := aColor;
end;


{ Applies this record's settings to a TFont object }
procedure RFont.AssignTo(Font: TFont);
begin
   if Font = NIL
   then raise Exception.Create('RFont.AssignTo: Font parameter cannot be nil');

   Font.Name  := Name;
   Font.Size  := Size;
   Font.Color := Color;
end;



end.






