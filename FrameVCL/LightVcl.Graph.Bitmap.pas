UNIT LightVcl.Graph.Bitmap;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Basic BMP functions
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
 procedure CenterBitmap      (Source, InDest: TBitmap);                                                { Center Source into Dest, withour resizing any of them. The source can be bigger than destination or other way arround. }
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
 Assert(BMP <> NIL, 'SetLargeSize');
 Assert(Width  > 0, 'SetLargeSize -  Width parameter is invalid: '+ IntToStr(Width));
 Assert(Height > 0, 'SetLargeSize - Height parameter is invalid: '+ IntToStr(Height));
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
  Img.Picture := NIL;
end;

procedure ClearBitmap(BMP: TBitmap);
begin
  BMP.Assign( NIL );
end;



{ Fill bitmap with the specified color }
procedure FillBitmap(BMP: TBitmap; Color: TColor);
begin
  BMP.Canvas.Brush.Color:= Color;
  BMP.Canvas.Brush.Style:= bsSolid;
  BMP.Canvas.FillRect(BMP.Canvas.ClipRect);
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}

{ Uses whatever font was already set for the canvas }
procedure CenterText(BMP: TBitmap; CONST Text: string);
begin
 if Text > '' then
  begin
   //BMP.Canvas.Brush.Color:= Font.BkgClr;  del
   BMP.Canvas.Brush.Style:= bsClear;
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


{ Uses Font as font }
procedure CenterText(BMP: TBitmap; CONST Text: string; aFont: RFont);
begin
 if Text > '' then
  begin
   //BMP.Canvas.Brush.Color:= Font.BkgClr;  del
   BMP.Canvas.Brush.Style:= bsClear;
   aFont.AssignTo(BMP.Canvas.Font);
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


procedure CenterText(BMP: TBitmap; CONST Text: string; CONST FontName: string; FontSize: Integer; FontColor: TColor);
begin
 if Text > '' then
  begin
   //BMP.Canvas.Brush.Color:= Font.BkgClr;  del
   BMP.Canvas.Brush.Style:= bsClear;
   BMP.Canvas.Font.Name:= FontName;
   BMP.Canvas.Font.Size:= FontSize;
   BMP.Canvas.Font.Color:= FontColor;
   BMP.Canvas.TextOut((BMP.Width- BMP.Canvas.TextWidth(Text)) DIV 2, (BMP.Height- BMP.Canvas.TextHeight(Text)) DIV 2, Text);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
   GET RAM SIZE
-------------------------------------------------------------------------------------------------------------}

{ Returns how much RAM this BMP requires }
function GetBitmapRamSize(BMP: TBitmap): Int64;
VAR Stream: TMemoryStream;
begin
 Stream:= TMemoryStream.Create;
 TRY
  Stream.Seek(0, soFromBeginning);     { Do we really need to do this?????????? }
  BMP.SaveToStream(Stream);            { Save bitmap to stream }
  Result:= Stream.Size;                { Measure stream size }
 FINALLY
  FreeAndNil(Stream);
 END;
end;


{ Returns how much RAM will that BMP required after setting its size to the new values. The result is in bytes } //old name: BmpRequiredRAM
function PredictBitmapRamSize(BMP: TBitmap; CONST NewWidth, NewHeight: Integer): Cardinal;
begin
 Assert(BMP <> NIL);
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

function IsPanoramic(Width, Height: Integer): Boolean;
begin
 Result:= (Width > 6000) AND             { Cannot be panoramic unless the width is really big }
         ((Width / Height) > 4);         { Panoramic images have width much much bigger than height }
end;


{ Panoramic images have width much much bigger than height }
function IsPanoramic(BMP: TBitmap): Boolean;
begin
 Result:= IsPanoramic(BMP.Width, BMP.Height);
end;


{ Compare the aspect ratio of the specified image with the AR of the monitor (for example) }
function AspectIsSmaller(SrcBMP: TBitmap; CONST Width, Height: Integer): Boolean;
begin
 Result:= (SrcBMP.Width / SrcBMP.Height) < (Width / Height);
end;



{ Compare the aspect ratio of the specified image with the AR of the monitor (for example) }
function AspectIsSmaller(Width, Height, SrcWidth, SrcHeight: Integer): Boolean;
begin
 Result:= (SrcWidth / SrcHeight) < (Width / Height);
end;



{ Returns image orientation (portrait/landscape) based on aspect ratio }
function AspectOrientation(BMP: TBitmap): TImgOrientation;              { Old name: 'Orientation' }
begin
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


{ Returns False if the image is Landscape.
  Returns true if the aspect ratio is portrait OR square.  }
function IsLandscape(Width, Height: Integer): Boolean;
begin
 var Orientation:= AspectOrientation(Width, Height);
 Result:= (Orientation = orLandscape) OR (Orientation = orSquare);
end;


{ Returns the scale of an input image relative to requiered size.
   Normal: Images with area = or > desktop area
   Small : Images with area < desktop and > TinyThresh
   Tiny  : Images with area < TinyThresh.  Tiny images are considered too small to be stretched. }
function GetImageScale(InputImage, DesktopSize: TBitmap; Tile: RTileParams): TImageScale;
begin
 Result:= GetImageScale(InputImage, DesktopSize.Width, DesktopSize.Height, Tile);
end;


function GetImageScale(InputImage: TBitmap; DesktopWidth, DesktopHeight: Integer; Tile: RTileParams): TImageScale;
VAR Percent: Extended;
begin
 Assert(Tile.TileThreshold < 100, 'TinyThreshold cannot be 100% !');
 Percent:= ProcentRepresent(InputImage.Width* InputImage.Height, DesktopWidth* DesktopHeight);    { Percent of desktop area }

 if Percent < Tile.TileThreshold
 then Result:= isTiny
 else
   if Percent < 100{%}          { under 100% the image is small }
   then Result:= isSmall
   else Result:= isLarge;       { over 100% it is large }

 { Extra condition:
     Images that are taller/wider than the desktop cannot be declated tiny!

   Explanations:
     I have a situation when the wallpaper is super tall and slim (W=300, H=1300).
     This is classified as "tiny" because its area is very small.
     This results in the "Auto tile small images" algorithm to kick in. But when this kicks in, the "Resize" algorithm does not kick in anymore (mutually exclusive) so the image remains taller than the desktop. This result in an assertion failure in "FadeBorder":  Assert(iTop  >= 0, 'iTop.  The wallpaper is larger than the desktop!'. }
 if (Result= isTiny) AND
    (InputImage.Width>= DesktopWidth) OR (InputImage.Height>= DesktopHeight)
 then Result:= isSmall;
end;














{-------------------------------------------------------------------------------------------------------------
   CANVAS ENLARGE
-------------------------------------------------------------------------------------------------------------}

procedure EnlargeCanvas(BMP: TBitmap; NewWidth, NewHeight: Integer; CanvasFillColor: TColor);    { Enlarge the canvas of the specified image and put the original image in the center of the new canvas }
VAR NewCanvas: TBitmap;
    iTop, iLeft: Integer;
begin
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



{ Center Source into Dest, withour resizing any of them. The source can be bigger than destination or other way arround. }
procedure CenterBitmap(Source, InDest: TBitmap);
VAR
   SrcTop, SrcLeft, SrcRight, SrcBottom: Integer;
   DestLeft, DestRight, DestTop, DestBottom: Integer;
begin
 Assert(Source<> NIL, 'Source is NIL');
 Assert(InDest  <> NIL, 'Dest is NIL');

 if  (Source.Width = InDest.Width)
 AND (Source.Height= InDest.Height)
 then InDest.Assign(Source)
 else
  begin
    if Source.Height < InDest.Height
    then
     begin
      { SRC }
      SrcTop:= 0;
      SrcBottom:= Source.Height;

      { DEST }
      DestTop:= (InDest.Height- Source.Height) DIV 2;
      DestBottom:= DestTop+ Source.Height
     end
    else
     begin
      { SRC }
      SrcTop:= (Source.Height- InDest.Height) DIV 2;
      SrcBottom:= SrcTop+ InDest.Height;

      { DEST }
      DestTop:= 0;
      DestBottom:= InDest.Height;
     end;


    if Source.Width < InDest.Width
    then
     begin
      { SRC }
      SrcLeft := 0;
      SrcRight:= Source.Width;

      DestLeft:= (InDest.Width- Source.Width) DIV 2;
      DestRight:= DestLeft+ Source.Width;
     end
    else
     begin
      { SRC }
      SrcLeft := (Source.Width - InDest.Width) DIV 2;
      SrcRight:= SrcLeft+ InDest.Width;

      { DEST }
      DestLeft:= 0;
      DestRight:= InDest.Width;
     end;

   InDest.Canvas.CopyRect(
        Rect(DestLeft, DestTop, DestRight, DestBottom),                                            {Dest}
        Source.Canvas,                                                                             {Source canvas}
        Rect(SrcLeft, SrcTop, SrcRight, SrcBottom));                                               {Source}
  end;
end;






{ RFont }
procedure RFont.Clear;
begin
  Name  := 'Verdana';
  Size  := 10;
  Color := clLime;
  //BkgClr:= clBlack;
end;


procedure RFont.Clear(CONST aName: string; aSize: Integer; aColor: TColor);
begin
  Name  := aName;
  Size  := aSize;
  Color := aColor;
end;


procedure RFont.AssignTo(Font: TFont);
begin
   Font.Name  := Name;
   Font.Size  := Size;
   Font.Color := Color;
end;



end.






