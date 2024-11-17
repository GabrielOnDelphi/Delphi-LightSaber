UNIT cGraphFx.Rotate;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
  Rotate an image at a specified angle.

  I M P O R T A N T
     This library contains all rotation function I could find.
     It is for rather for testing than for real use.
     I decided to use cGraphFx.RotateGr32. However, it seems that RotateBitmapGDIP is even better but it doesn't work.

--------------------------------------------------------------------------------------------------------------

  Source: http://stackoverflow.com/questions/10633400/rotate-bitmap-by-real-angle

  External dependencies:
      JanFX
      CCR.Exif

  Also see:
     Fastest possible, but quality not so great: c:\MyProjects\Packages\Third party packages\Rotate Image VCL\RotImg.pas
     Fade image to white: https://stackoverflow.com/questions/13701685/fade-an-image-using-gdi-i-e-change-only-the-alpha-channel-of-a-tgpgraphic
     Hier mal eine Alternative zu GDI+, die ich persönlich übersichtlicher finde: WIC: https://www.delphipraxis.net/199843-gdi-bilddrehung-mit-transparenz.html

  If AdjustSize is:
    True : then the size of BMP will adjusted to hold the entire rotated image.
    False: then the size of BMP will remain the same, thereofre the rotated image will be cropped

  Tester:
      c:\MyProjects\Project Testers\gr Rotate, flip tester\RotateTester.dpr
      Put some conclusions here
==================================================================================================}

INTERFACE
USES
  winapi.Windows, System.Types, system.SysUtils, Vcl.Graphics, System.Math, ccStreamBuff,
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
    procedure WriteToStream  (Stream: TCubicBuffStream);
    procedure ReadFromStream (Stream: TCubicBuffStream);
 end;

 {$IFDEF CCRExif}
 procedure RotateExif        (BMP: TBitmap; Exif: TExifData);
 {$ENDIF}
 procedure RotateBitmap      (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
 {}
 procedure RotateBitmapGDI   (BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); { Uses GDI+. Doesn't work. It works now. }
 procedure RotateBitmapSWT   (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use cGraphFx.RotateBitmap'; { No antialiasing }
 procedure RotateBitmapJanFX (BMP: TBitmap; Degs: Single;                            BkColor: TColor = clNone); deprecated 'Use cGraphFx.RotateBitmap'; { This is slow even if I rotate the image at right angles (90, 180, 270) }
 procedure RotateBitmapPLG   (BMP: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone); deprecated 'Use cGraphFx.RotateBitmap'; { 39ms. No antialising }
 procedure RotateBitmapBLT   (BMP: TBitmap; Rads: Single; lWidth, lHeight: Longint);  // Worst!


IMPLEMENTATION
USES cGraphFx.RotateGr32, cGraphBitmap;




{ Autorotate image based on its EXIF }
{$IFDEF CCRExif}
procedure RotateExif(BMP: TBitmap; Exif: TExifData);
begin
 Assert(Exif <> NIL, 'RotateExif. Exif cannot be NIL!');
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


{ Global function.
  Best algorithm of all.

  The RotateBitmapGDI is the fastest but I don't know how to center the image in the middle of the view port.
  RotateBitmapGR32 is super slow but it works without issues. However, I WANT TO GET RID OF Graphics32 }
procedure RotateBitmap(BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
begin
 if Degs <> 0
 then RotateBitmapGR32(Bmp, Degs, AdjustSize, BkColor, TRUE, 13);
end;








{ based on GDI+

  I broke  it from accident. Can't find it in D: backup. need to dig into the external drive  }
procedure RotateBitmapGDI(BMP: TBitmap; Degs: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
var
  Tmp: TGPBitmap;
  Matrix: TGPMatrix;
  C, S: Extended;
  DiffW, DiffH: Integer;
  NewSize: TSize;
  Graphs: TGPGraphics;
begin
  Assert(Bmp <> NIL);
  // maybe I need to set pf24 or pf32?
  Tmp := TGPBitmap.Create(Bmp.Handle, Bmp.Palette);
  Matrix := TGPMatrix.Create;
  TRY
    Matrix.RotateAt(Degs, MakePoint(0.5 * Bmp.Width, 0.5 * Bmp.Height));
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
      {
      then DiffW:= Integer(Tmp.GetWidth) -  Bmp.Width
      then DiffH:= Integer(Tmp.GetHeight) - Bmp.Height
      }
      if AdjustSize
      then
       begin
        //DiffW:= round ((Tmp.GetWidth  - Bmp.Width * C + Bmp.Height * S) / 2);
        //DiffH:= round ((Tmp.GetHeight - Bmp.Width * S - Bmp.Height * C) / 2);
       end;

      {if AdjustSize
      then
       begin
        XForm.eDx := (Tmp.Width  - Bmp.Width * C + Bmp.Height * S) / 2;
        XForm.eDy := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
       end;  }

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
 BMPOut:= TBitmap.Create;
 TRY
  BMP.PixelFormat   := pf24bit;                    { Otherwise SmoothRotate won't work }
  BMPOut.PixelFormat:= pf24bit;

  SetLargeSize(BMPOut, BMP.Height, BMP.Width);
  cGraphBitmap.FillBitmap(BMPOut, BkColor);

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
  SetWorldTransform  doc: https://msdn.microsoft.com/en-us/library/dd145104(v=vs.85).aspx }
procedure RotateBitmapSWT(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);   { No antialising }
VAR
  C: Single;
  S: Single;
  XForm: TXForm;
  Tmp: TBitmap;
begin
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



{ 39ms
  No antialising }
procedure RotateBitmapPLG(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean= TRUE; BkColor: TColor = clNone);
var
  C: Single;
  S: Single;
  Tmp: TBitmap;
  OffsetX: Single;
  OffsetY: Single;
  Points: array[0..2] of TPoint;
begin
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



{ Slow.
  HORRIBLE quality.
  Source: http://www.delphi-central.com/tutorials/RotateBitmapBitBlt.aspx }
procedure RotateBitmapBLT(BMP: TBitmap; Rads: Single; lWidth, lHeight: Longint);
var
 I: Longint;             // loop counter
 J: Longint;             // loop counter
 hNewBitmapDC: Cardinal; // DC of the new bitmap
 hNewBitmap: Cardinal;   // handle to the new bitmap
 lSine: extended;        // sine used in rotation
 lCosine: extended;      // cosine used in rotation
 X1: Longint;            // used in calculating new
 X2: Longint;            // used in calculating new
 X3: Longint;            // used in calculating new
 Y1: Longint;            // used in calculating new
 Y2: Longint;            // used in calculating new
 Y3: Longint;            // used in calculating new
 lMinX: Longint;
 lMaxX: Longint;
 lMinY: Longint;
 lMaxY: Longint;
 lNewWidth: Longint;     // width of new bitmap
 lNewHeight: Longint;    // height of new bitmap
 lSourceX: Longint;      // x pixel coord we are blitting from the source  image
 lSourceY: Longint;      // y pixel coord we are blitting from the source image
 hBitmapDC: Cardinal;
begin
 hBitmapDC := BMP.Canvas.Handle;

 // create a compatible DC from the one just brought into this function
 hNewBitmapDC := CreateCompatibleDC(hBitmapDC);

 // compute the sine/cosinse of the radians used to rotate this image
 lSine := Sin(Rads);
 lCosine := Cos(Rads);

 // compute the size of the new bitmap being created
 X1 := Round(-lHeight * lSine);
 Y1 := Round(lHeight * lCosine);
 X2 := Round(lWidth * lCosine - lHeight * lSine);
 Y2 := Round(lHeight * lCosine + lWidth * lSine);
 X3 := Round(lWidth * lCosine);
 Y3 := Round(lWidth * lSine);

 // figure out the max/min size of the new bitmap
 lMinX := Min(0, Min(X1, Min(X2, X3)));
 lMinY := Min(0, Min(Y1, Min(Y2, Y3)));
 lMaxX := Max(X1, Max(X2, X3));
 lMaxY := Max(Y1, Max(Y2, Y3));

 // set the new bitmap width/height
 lNewWidth := lMaxX - lMinX;
 lNewHeight := lMaxY - lMinY;

 // create a new bitmap based upon the new width/height of the rotated bitmap
 hNewBitmap := CreateCompatibleBitmap(hBitmapDC, lNewWidth, lNewHeight);

 // attach the new bitmap to the new device context created above before constructing the rotated bitmap
 SelectObject(hNewBitmapDC, hNewBitmap);

 // loop through and translate each pixel to its new location. this is using a standard rotation algorithm
 For I := 0 To lNewHeight do
  begin
   For J := 0 To lNewWidth do
    begin
     lSourceX := Round((J + lMinX) * lCosine + (I + lMinY) * lSine);
     lSourceY := Round((I + lMinY) * lCosine - (J + lMinX) * lSine);
     If (lSourceX >= 0) And (lSourceX <= lWidth) And (lSourceY >= 0) And (lSourceY <= lHeight) Then
      BitBlt(hNewBitmapDC, J, I, 1, 1, hBitmapDC, lSourceX, lSourceY, SRCCOPY);
    end;
  end;

 // reset the new bitmap width and height
 lWidth  := lNewWidth;
 lHeight := lNewHeight;

 // return the DC to the new bitmap
 hBitmapDC := hNewBitmapDC;

 // destroy the bitmap created
 DeleteObject(hNewBitmap);

 BMP.Width  := lWidth;
 BMP.Height := lHeight;
 BitBlt(BMP.Canvas.Handle, 0, 0, lWidth, lHeight, hBitmapDC, 0, 0, SRCCOPY);
End;







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


procedure RSpatialParams.WriteToStream(Stream: TCubicBuffStream);
begin
 Stream.WriteBoolean (Flip);
 Stream.WriteBoolean (Mirror);
 Stream.WriteByte    (Ord(Rotation));
 Stream.WritePadding(32);
end;


procedure RSpatialParams.ReadFromStream(Stream: TCubicBuffStream);
begin
 Flip    := Stream.ReadBoolean;
 Mirror  := Stream.ReadBoolean;
 Rotation:= TRotateSense(Stream.ReadByte);
 Stream.ReadPadding(32);
end;


end.

