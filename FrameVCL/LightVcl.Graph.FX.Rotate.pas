UNIT LightVcl.Graph.FX.Rotate;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Rotate an image at a specified angle.

  I M P O R T A N T
     This library contains various rotation functions collected for testing/comparison purposes.
     For production use, prefer LightVcl.Graph.FX.RotateGr32 (when GR32 is available) or RotateBitmapGDI.

--------------------------------------------------------------------------------------------------------------

  Source: http://stackoverflow.com/questions/10633400/rotate-bitmap-by-real-angle

  External dependencies:
      JanFX    - Required only when CCRExif is defined
      CCR.Exif - Optional, for EXIF-based auto-rotation (Github.com/exilon/ccr-exif)

  Also see:
     Fastest possible, but quality not so great: c:\MyProjects\Packages\Third party packages\Rotate Image VCL\RotImg.pas
     Fade image to white: https://stackoverflow.com/questions/13701685/fade-an-image-using-gdi-i-e-change-only-the-alpha-channel-of-a-tgpgraphic
     Alternative using WIC: https://www.delphipraxis.net/199843-gdi-bilddrehung-mit-transparenz.html

  AdjustSize parameter:
    True : The bitmap size will be adjusted to hold the entire rotated image
    False: The bitmap size remains the same; the rotated image will be cropped

  Tester:
      c:\MyProjects\Project Testers\gr Rotate, flip tester\RotateTester.dpr
==================================================================================================}

INTERFACE
USES
  winapi.Windows, System.Types, system.SysUtils, Vcl.Graphics, System.Math, LightCore.StreamBuff,
  GDIPAPI, GDIPOBJ //GdiPlus; >= Delphi2009
  {$IFDEF CCRExif},janFX{$ENDIF}
  {$IFDEF CCRExif},CCR.Exif{$ENDIF};   { CCR Exif library can be found here: Github.com/exilon/ccr-exif }

TYPE
 TRotateSense = (rtNone, rtLeft, rtRight, rtExif);

 PSpatialParams= ^RSpatialParams;
 RSpatialParams = record
    Flip  : Boolean;
    Mirror: Boolean;
    Rotation: TRotateSense;
    procedure Reset;
    procedure WriteToStream  (Stream: TLightStream);
    procedure ReadFromStream (Stream: TLightStream);
 end;

 {$IFDEF CCRExif}
 procedure RotateExif        (BMP: TBitmap; Exif: TExifData);
 {$ENDIF}
 procedure RotateBitmap      (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
 {}
 procedure RotateBitmapGDI   (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); { Uses GDI+ }
 procedure RotateBitmapSWT   (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { No antialiasing }
 procedure RotateBitmapJanFX (BMP: TBitmap; Degs: Single;                            BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { This is slow even if I rotate the image at right angles (90, 180, 270) }
 procedure RotateBitmapPLG   (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use LightVcl.Graph.FX.RotateBitmap'; { 39ms. No antialising }
 procedure RotateBitmapBLT   (BMP: TBitmap; Rads: Single; lWidth, lHeight: Longint);  // Worst!


IMPLEMENTATION
USES
   {$IFDEF GR32}
   LightVcl.Graph.FX.RotateGr32,   {$ENDIF}
   LightVcl.Graph.Bitmap;





{ Autorotate image based on its EXIF orientation tag }
{$IFDEF CCRExif}
procedure RotateExif(BMP: TBitmap; Exif: TExifData);
begin
 Assert(BMP <> NIL, 'RotateExif: BMP cannot be NIL');
 Assert(Exif <> NIL, 'RotateExif: Exif cannot be NIL');
 if  (Exif.Orientation<> toUndefined)                                    { The image has info about orientation }
 AND (Exif.Orientation<> toTopLeft)                                      { This is the 'normal' orientation }
 AND (Exif.Orientation<> toLeftTop) then                                 { 'Flipped' (left hand in the right side of the picture }
   case Exif.Orientation of                                              { The normal orientation is toTopLeft }
    toRightBottom, toBottomRight: RotateBitmap(BMP, 180);
    toRightTop   , toTopRight   : RotateBitmap(BMP, 90, TRUE);
    toLeftBottom , toBottomLeft : RotateBitmap(BMP, 270, TRUE);     { TTiffOrientation = (toUndefined, toTopLeft, toTopRight, toBottomRight,  toLeftTop (i.e., rotated), toRightBottom, toLeftBottom);}
   end;
end;
{$ENDIF}


{ Main rotation function. Selects the best available algorithm.
  Uses GR32 when available, otherwise falls back to GDI+. }
procedure RotateBitmap(BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
begin
 Assert(BMP <> NIL, 'RotateBitmap: BMP cannot be NIL');
 if Degs <> 0 then
   {$IFDEF GR32}
     RotateBitmapGR32(Bmp, Degs, AdjustSize, BkColor, TRUE, 13);
   {$ELSE}
     RotateBitmapGDI(Bmp, Degs, AdjustSize, BkColor);
   {$ENDIF}
end;





{ Based on GDI+ }
procedure RotateBitmapGDI(BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
var
  Tmp: TGPBitmap;
  Matrix: TGPMatrix;
  C, S: Extended;
  DiffW, DiffH: Integer;
  NewSize: TSize;
  Graphs: TGPGraphics;
begin
  Assert(Bmp <> NIL, 'RotateBitmapGDI: BMP cannot be NIL');
  Tmp := TGPBitmap.Create(Bmp.Handle, Bmp.Palette);
  Matrix := TGPMatrix.Create;
  TRY
    Matrix.RotateAt(Degs, MakePoint(0.5* Bmp.Width, 0.5* Bmp.Height));
    if AdjustSize then
    begin
      C := Cos(DegToRad(Degs));
      S := Sin(DegToRad(Degs));
      NewSize.cx := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      NewSize.cy := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      Bmp.Width  := NewSize.cx;
      Bmp.Height := NewSize.cy;
    end;
    Graphs:= TGPGraphics.Create(Bmp.Canvas.Handle);
    TRY
      Graphs.Clear(ColorRefToARGB(ColorToRGB(BkColor)));
      {
        https://stackoverflow.com/questions/41274112/how-to-draw-tgpgraphics-contents-onto-a-canvas
        Graphs.SetCompositingMode(CompositingModeSourceCopy);
        Graphs.SetInterpolationMode(InterpolationModeHighQualityBicubic);
        Graphs.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        Graphs.SetSmoothingMode(SmoothingModeHighQuality);  }
      Graphs.SetTransform(Matrix);

      { Calculate offset to center the rotated image }
      if AdjustSize
      then
       begin
         if Degs <= 90 then
          begin
           DiffW:= Integer(Tmp.GetWidth)- Bmp.Width;
           DiffH:= Bmp.Height - Integer(Tmp.GetHeight);
          end
         else
          begin
           DiffW:= Bmp.Width  - Integer(Tmp.GetWidth);
           DiffH:= Integer(Tmp.GetHeight)- Bmp.Height;
          end;
       end
      else
       begin
        DiffW:= Bmp.Width  - Integer(Tmp.GetWidth);
        DiffH:= Bmp.Height - Integer(Tmp.GetHeight);
       end;

      //tmp.Save(getappdir + '2post.bmp', nil, nil);
      Graphs.DrawImage(Tmp, DiffW / 2, DiffH / 2);
      //Graphs.DrawImage(Tmp, Top.recta);  //DiffW, DiffH);
    FINALLY
      FreeAndNil(Graphs);
    END;
  finally
    FreeAndNil(Matrix);
    FreeAndNil(Tmp);
  end;
end;

{      if AdjustSize
      then
       begin
        DiffW:= 0;
        DiffH:= 0;
       end
      else
       begin
        DiffW:= Bmp.Width  - Integer(Tmp.GetWidth);
        DiffH:= Bmp.Height - Integer(Tmp.GetHeight);
       end;}
 {
 Procedure TwndMS.FormClick(Sender: TObject);
  Var
     GP  : TGPImage;
     GPGR: TGPGraphics;
 Begin
  GP:= TGPImage.Create('C:\LazLogo Blue.png', True);
   Try
	GP.RotateFlip(Rotate180FlipNone);
	GPGR:= TGPGraphics.Create(wndMS.Canvas.Handle);
	 Try
	  GPGR.DrawImage(GP, 0, 0);
	 Finally
	  GPGR.Free;
	 End;
   Finally
	GP.Free;
   End;
 End;   }

(*
 if code above doesn't work, try this:
 graphics := TGPGraphics.Create(DC);
 Image:= TGPImage.Create('..\GrapeBunch.bmp');
 LMatrix:= TGPMatrix.Create(1, 0, 0, 1,100, 200); //Change the X and Y //coordinates with respect to <'le of rotation.
 graphics.SetTransform(LMatrix);
 graphics.RotateTransform(270); //Pass the angle of rotation
 graphics.DrawImage(Image,0,0,Image.GetWidth,Image.GetHeight);
 Image.Free;
 graphics.Free;

 or

 Image:= TGPImage.Create('Image1.bmp');
 ....
 gr.RotateTransform(30);//rotate 30 degrees
 gr.DrawImage(image, x, y);//draw image at x,y
 There are several overloaded version of DrawImage(), one of which will
 allow you to scale and/or skew the image
*)





{ This is slow even if we rotate the image at right angles (90, 180, 270) }
procedure RotateBitmapJanFX(BMP: TBitmap; Degs: Single; BkColor: TColor = clNone);
VAR BMPOut: TBitmap;
begin
 Assert(BMP <> NIL, 'RotateBitmapJanFX: BMP cannot be NIL');
 BMPOut:= TBitmap.Create;
 TRY
  BMP.PixelFormat   := pf24bit;                    { Otherwise SmoothRotate won't work }
  BMPOut.PixelFormat:= pf24bit;

  SetLargeSize(BMPOut, BMP.Height, BMP.Width);
  LightVcl.Graph.Bitmap.FillBitmap(BMPOut, BkColor);

  {$IFDEF CCRExif}
  janFX.SmoothRotate(BMP, BMPOut, BMPOut.Width div 2, BMPOut.Height div 2, Degs);
  {$ELSE}
    RAISE Exception.Create('CCREXIF NOT AVAILABLE');
  {$ENDIF}

  SetLargeSize(BMP, BMPOut.Width, BMPOut.Height);
  BMP.Assign(BMPout);
 FINALLY
  FreeAndNil(BMPOut);
 end;
end;



{
  SetWorldTransform
  SetWorldTransform doc: https://msdn.microsoft.com/en-us/library/dd145104(v=vs.85).aspx
  Note: This function uses radians, not degrees. No antialiasing. }
procedure RotateBitmapSWT(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
VAR
  C: Single;
  S: Single;
  XForm: TXForm;
  Tmp: TBitmap;
begin
  Assert(Bmp <> NIL, 'RotateBitmapSWT: BMP cannot be NIL');
  C := Cos(Rads);
  S := Sin(Rads);
  XForm.eM11 := C;
  XForm.eM12 := S;
  XForm.eM21 := -S;
  XForm.eM22 := C;
  Tmp := TBitmap.Create;
  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
     begin
      Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      Tmp.Height:= Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      XForm.eDx := (Tmp.Width  - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
     end
    else
     begin
      Tmp.Width := Bmp.Width;
      Tmp.Height:= Bmp.Height;
      XForm.eDx := (Bmp.Width  - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
     end;
    SetGraphicsMode(Tmp.Canvas.Handle, GM_ADVANCED);
    SetWorldTransform(Tmp.Canvas.Handle, XForm);

    BitBlt(Tmp.Canvas.Handle, 0, 0, Tmp.Width, Tmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY); // this was thw original code, but jramos said is buggy: the fix is below:
    //BitBlt(Tmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Assign(Tmp);
  finally
    FreeAndNil(Tmp);
  end;
end;



{ Uses PlgBlt for rotation.
  Note: This function uses radians, not degrees. No antialiasing. }
procedure RotateBitmapPLG(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
var
  C: Single;
  S: Single;
  Tmp: TBitmap;
  OffsetX: Single;
  OffsetY: Single;
  Points: array[0..2] of TPoint;
begin
  Assert(Bmp <> NIL, 'RotateBitmapPLG: BMP cannot be NIL');
  C := Cos(Rads);
  S := Sin(Rads);
  Tmp := TBitmap.Create;
  TRY
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
     begin
      Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      Tmp.Height := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      OffsetX := (Tmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      OffsetY := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
     end
    else
     begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      OffsetX := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      OffsetY := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
     end;
    Points[0].X := Round(OffsetX);
    Points[0].Y := Round(OffsetY);
    Points[1].X := Round(OffsetX + Bmp.Width * C);
    Points[1].Y := Round(OffsetY + Bmp.Width * S);
    Points[2].X := Round(OffsetX - Bmp.Height * S);
    Points[2].Y := Round(OffsetY + Bmp.Height * C);
    PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
    Bmp.Assign(Tmp);
  FINALLY
    FreeAndNil(Tmp);
  end;
end;



 {
//https://forum.lazarus.freepascal.org/index.php?topic=38426.0
  function RotateLazarus(FileName: string): TBitmap; // support png also
  Var
      BGRAout, BGRAin: TBGRABitmap;
 Begin
  Result:= TBitmap.Create;
  PIC:= TPicture.Create;
   Try
	PIC.LoadFromFile(FileName);
 
	BGRAout:= TBGRABitmap.Create(PIC.Width, PIC.Height, BGRAPixelTransparent);
	 Try
	  BGRAin:= TBGRABitmap.Create(PIC.Bitmap);
	   Try
		BGRAout.PutImageAngle(PIC.Width, PIC.Height, BGRAin, 180, rfBestQuality);
	   Finally
		BGRAin.Free;
	   End;
	  PIC.Assign(BGRAout);
	 Finally
	  BGRAout.Free;
	 End;
	Result.Assign(PIC);
   Finally
	PIC.Free;
   End;
 End;  }



{ Pixel-by-pixel rotation using BitBlt.
  Warning: Very slow and produces poor quality results.
  Note: This function uses radians. lWidth/lHeight are passed but ignored - bitmap dimensions are used.
  Source: http://www.delphi-central.com/tutorials/RotateBitmapBitBlt.aspx }
procedure RotateBitmapBLT(BMP: TBitmap; Rads: Single; lWidth, lHeight: Longint);
var
 I: Longint;
 J: Longint;
 hNewBitmapDC: HDC;
 hNewBitmap: HBITMAP;
 hOldBitmap: HBITMAP;
 lSine: Extended;
 lCosine: Extended;
 X1, X2, X3: Longint;
 Y1, Y2, Y3: Longint;
 lMinX, lMaxX: Longint;
 lMinY, lMaxY: Longint;
 lNewWidth: Longint;
 lNewHeight: Longint;
 lSourceX: Longint;
 lSourceY: Longint;
 hBitmapDC: HDC;
 SrcWidth, SrcHeight: Integer;
begin
 Assert(BMP <> NIL, 'RotateBitmapBLT: BMP cannot be NIL');

 { Use actual bitmap dimensions instead of parameters }
 SrcWidth:= BMP.Width;
 SrcHeight:= BMP.Height;

 hBitmapDC:= BMP.Canvas.Handle;
 hNewBitmapDC:= CreateCompatibleDC(hBitmapDC);
 TRY
   lSine:= Sin(Rads);
   lCosine:= Cos(Rads);

   { Compute the size of the new bitmap being created }
   X1:= Round(-SrcHeight * lSine);
   Y1:= Round(SrcHeight * lCosine);
   X2:= Round(SrcWidth * lCosine - SrcHeight * lSine);
   Y2:= Round(SrcHeight * lCosine + SrcWidth * lSine);
   X3:= Round(SrcWidth * lCosine);
   Y3:= Round(SrcWidth * lSine);

   { Figure out the max/min size of the new bitmap }
   lMinX:= Min(0, Min(X1, Min(X2, X3)));
   lMinY:= Min(0, Min(Y1, Min(Y2, Y3)));
   lMaxX:= Max(X1, Max(X2, X3));
   lMaxY:= Max(Y1, Max(Y2, Y3));

   lNewWidth:= lMaxX - lMinX;
   lNewHeight:= lMaxY - lMinY;

   hNewBitmap:= CreateCompatibleBitmap(hBitmapDC, lNewWidth, lNewHeight);
   TRY
     hOldBitmap:= SelectObject(hNewBitmapDC, hNewBitmap);

     { Loop through and translate each pixel to its new location }
     for I:= 0 to lNewHeight-1 do
       for J:= 0 to lNewWidth-1 do
        begin
         lSourceX:= Round((J + lMinX) * lCosine + (I + lMinY) * lSine);
         lSourceY:= Round((I + lMinY) * lCosine - (J + lMinX) * lSine);
         if (lSourceX >= 0) AND (lSourceX < SrcWidth) AND (lSourceY >= 0) AND (lSourceY < SrcHeight)
         then BitBlt(hNewBitmapDC, J, I, 1, 1, hBitmapDC, lSourceX, lSourceY, SRCCOPY);
        end;

     { Copy result back to input bitmap }
     BMP.Width:= lNewWidth;
     BMP.Height:= lNewHeight;
     BitBlt(BMP.Canvas.Handle, 0, 0, lNewWidth, lNewHeight, hNewBitmapDC, 0, 0, SRCCOPY);

     SelectObject(hNewBitmapDC, hOldBitmap);
   FINALLY
     DeleteObject(hNewBitmap);
   END;
 FINALLY
   DeleteDC(hNewBitmapDC);
 END;
end;






(*
{-------------------------------------------------------------------------------------------------------------
   ROTATE ClockWise
   Angles: 90, 180, 270
-------------------------------------------------------------------------------------------------------------}

{ 36 ms }
procedure Rotate90(Bmp: TBitmap);        { Source http://stackoverflow.com/questions/10546582/rotate-timage-in-delphi }
VAR
  x,y:integer;
  p: array[0..2] of TPoint;
  OutBmp: TBitmap;
begin
 x:= Bmp.Width;
 y:= Bmp.Height;

 OutBmp:= TBitmap.Create;
 TRY
  OutBmp.Width := y;   { Flip resolution }
  OutBmp.Height:= x;

  p[0].X:= y;
  p[0].Y:= 0;
  p[1].X:= y;
  p[1].Y:= x;
  p[2].X:= 0;
  p[2].Y:= 0;

  PlgBlt(OutBmp.Canvas.Handle, p, Bmp.Canvas.Handle, 0,0,x,y,0,0,0);
  Bmp.Assign(OutBmp);
 FINALLY
  FreeAndNil(OutBmp);
 END;
end;



{ 16ms }
procedure Rotate180(Bmp: TBitmap);     { Source: http://www.delphipraxis.net/111932-bild-um-angegebene-90%B0-180%B0-270%B0-drehen.html }
var dx, dy : integer;
begin
 dx:= Bmp.Width;
 dy:= Bmp.Height;
 Bmp.Canvas.CopyRect(Rect(0,0,dx,dy), Bmp.Canvas,Rect(dx,dy,0,0));
end;



{ 39ms }
procedure Rotate270(Bmp: TBitmap);  { I also tried this but it takes 350ms: http://www.efg2.com/Lab/ImageProcessing/FlipReverseRotate.htm }
begin
 RotateBitmapPLG(Bmp, DegToRad(270), TRUE, clPurple);
end;
*)








{ TSpatialParams }
procedure RSpatialParams.Reset;
begin
 Flip    := FALSE;     { Flip the wallpaper. Useful when the image is "concentrated" on the left side and hidden by desktop icons, while the right side of the desktop is free of icons. }
 Mirror  := FALSE;
 Rotation:= rtExif;    { Auto rotate }
end;


procedure RSpatialParams.WriteToStream(Stream: TLightStream);
begin
 Stream.WriteBoolean (Flip);
 Stream.WriteBoolean (Mirror);
 Stream.WriteByte    (Ord(Rotation));
 Stream.WritePaddingValidation;
end;


procedure RSpatialParams.ReadFromStream(Stream: TLightStream);
VAR RotByte: Byte;
begin
 Flip   := Stream.ReadBoolean;
 Mirror := Stream.ReadBoolean;
 RotByte:= Stream.ReadByte;
 if Integer(RotByte) > Ord(High(TRotateSense))
 then Rotation:= rtNone   { Invalid value - default to rtNone }
 else Rotation:= TRotateSense(RotByte);
 Stream.ReadPaddingValidation;
end;


end.

