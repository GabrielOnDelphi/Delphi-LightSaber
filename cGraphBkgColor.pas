UNIT cGraphBkgColor;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Features:
     * Fades the borders of BMP to the specified color
     * Get border color
     * Get border average color
     * Get border dominant color

   Tester:
     c:\MyProjects\Project Testers\gr cGraphBorder.pas tester\TesterFadeBrd.dpr
--------------------------------------------------------------------------------------------------}


INTERFACE

USES
   System.Types, System.SysUtils, System.Math, Vcl.Graphics, cGraphBkgColorParams;


{ Fade border }
procedure FadeBorder            (InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams; Border: TBorderSet);  { Fades the borders of BMP to the specified color. }
procedure FadeBorderAuto        (InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams);

{ Border rectangle }
procedure ApplyBorderRectOut    (BMP: TBitmap; BorderSize: Integer; FrameColor: TColor; DarkenFrame: Boolean= FALSE);
procedure ApplyBorderRectIn     (BMP: TBitmap; BorderSize: Integer; FrameColor: TColor);

{ Border line }
function  HasBlackBorder        (BMP: TBitmap; CONST Tolerance: Byte= 5): Boolean;    { Returns True if the border was black }
procedure RemoveBorder          (BMP: TBitmap);                                       { Removes the black border that surronds an image. The returned image will have same size. The black border will be replaced with content from image (adiacent line). Only works if the border is 1 pixel wide! }

{ Utils }
function  LineIsBlack           (BMP: TBitmap; RowNo: Integer; Tolerance: Byte= 3): Boolean;
function  GetBorderDominantColor(BMP: TBitmap; Border: TBorderType; Tolerance: Integer= 8): TColor;


IMPLEMENTATION

USES
   cGraphUtil, ccBinary, ccCore, cGraphFX, cGraphBitmap;



{--------------------------------------------------------------------------------------------------
   Border rectangle

   Applies a colored border (see FrameColor) arroud the image.
   The resulted image size WILL increase with 2*BorderSize pixels. This way the border will not be drawn over the image.
   If DarkenFrame is true, a darkened FrameColor will be used.

   Tester: c:\MyProjects\Project Testers\gr Rotate, flip tester\RotateTester.dpr
--------------------------------------------------------------------------------------------------}
procedure ApplyBorderRectOut(BMP: TBitmap; BorderSize: Integer; FrameColor: TColor; DarkenFrame: Boolean= FALSE);
VAR Temp: TBitmap;
begin
 //todo: to be optimized
 if BorderSize = 0 then EXIT;

 Temp:= CreateBitmap(BMP.Width+ (BorderSize*2), BMP.Height+ (BorderSize*2));    { The output image will be larger than the input image }
 TRY
  FillBitmap(Temp, FrameColor);                                                 { Fill it with the frame color }

  { Draw outside border }  { One extra line drawn into the with frame }
  if DarkenFrame then
   begin
    Temp.Canvas.Brush.Color:= DarkenColor(FrameColor, 50);                      { 0 is total darkness, 100 no darkness (same color) }
    Temp.Canvas.FrameRect(Rect(1, 1, Temp.Width-1, Temp.Height-1));             { Draws a rectangle using the Brush of the canvas to draw the border. Use FrameRect to draw a 1 pixel wide border around a rectangular region. FrameRect does not fill the interior of the rectangle with the Brush pattern. To draw a boundary using the Pen instead, use the Polygon method. }
    Temp.Canvas.FrameRect(Rect(2, 2, Temp.Width-2, Temp.Height-2));
   end;

  Temp.Canvas.CopyRect(Rect(BorderSize, BorderSize, BMP.Width+BorderSize, BMP.Height+BorderSize), BMP.Canvas, Rect(0, 0, BMP.Width, BMP.Height));

  { Draw inside border }
  Temp.Canvas.Brush.Color:= DarkenColor(FrameColor, 40);                        { A line between the image and the white frame }
  Temp.Canvas.FrameRect(Rect(BorderSize, BorderSize, BMP.Width+BorderSize, BMP.Height+BorderSize));

  BMP.Assign(Temp);
 FINALLY
  FreeAndNil(Temp);
 END;
end;


{--------------------------------------------------------------------------------------------------
   Same as above, but the original image size will not change.
   The frame will be drawn over the image.
--------------------------------------------------------------------------------------------------}
procedure ApplyBorderRectIn(BMP: TBitmap; BorderSize: Integer; FrameColor: TColor);
begin
 if BorderSize = 0 then EXIT;
 //Assert(BMP.PixelFormat= pf24bit);
 BMP.Canvas.Brush.Color:= FrameColor;
 for var i:= 1 to BorderSize do
   BMP.Canvas.FrameRect(Rect(i-1, i-1, BMP.Width-i, BMP.Height-i));
end;







{ Returns True if the specified line contains only black pixels.
  Color Tolerance:
    In Jpeg images the colors are "smeared", so a line that was black in the original image, may not be perfect black ($0).
    So, we allow a tolerance from clBlack via Tolerance }
function LineIsBlack(Bmp: TBitmap; RowNo: Integer; Tolerance: Byte= 3): Boolean;
VAR
   X: Integer;
   r,g,b: Cardinal;
   Line: PRGB24Array;
begin
 r:= 0;
 g:= 0;
 b:= 0;

 Line := bmp.ScanLine[RowNo];
 for x := 0 to bmp.Width -1 do
  begin
   r := r + Line[x].R;
   g := g + Line[x].G;
   b := b + Line[x].B;
  end;

 r := r DIV Cardinal(bmp.Width);
 g := g div Cardinal(bmp.Width);
 b := b div Cardinal(bmp.Width);

 Result:= (r+g+b) DIV 3 <= Tolerance;
end;


{ Same as above but for columns }
function ColumnIsBlack(Bmp: TBitmap; ColNo: Integer; Tolerance: Byte= 3): Boolean;
VAR
   Rw: Integer;
   r,g,b: Cardinal;
   Line: PRGB24Array;
begin
 r:= 0;
 g:= 0;
 b:= 0;

 for Rw:= 0 to Bmp.Height-1 DO
  begin
   Line := bmp.ScanLine[Rw];

   r := r + Line[ColNo].R;
   g := g + Line[ColNo].G;
   b := b + Line[ColNo].B;
  end;

 r := r DIV Cardinal(bmp.Height);
 g := g div Cardinal(bmp.Height);
 b := b div Cardinal(bmp.Height);

 Result:= (r+g+b) DIV 3 <= Tolerance;
end;




{ Returns True if all 4 borders of the image are black.
  See above note about color tolerance. }
function HasBlackBorder(Bmp: TBitmap; CONST Tolerance: Byte= 5): Boolean;
begin
 Assert(bmp.pixelformat= pf24bit, 'HasBlackBorder - Wrong PixelFormat!');


 { Top line }
 Result:= LineIsBlack(Bmp, 0, Tolerance)
          AND NOT LineIsBlack(Bmp, 3, Tolerance);   { If lines inside of the image are still black it means we cannot determine border color so we abandon }
 if NOT Result then EXIT;


 { Bottom line }
 Result:= LineIsBlack(Bmp, Bmp.Height-IndexDiff, Tolerance)
          AND NOT LineIsBlack(Bmp, Bmp.Height-3-IndexDiff, Tolerance);   { If lines inside of the image are still black it means we cannot determine border color so we abandon }
 if NOT Result then EXIT;


 { Left }
 Result:= ColumnIsBlack(Bmp, 0, Tolerance);
 if NOT Result then EXIT;


 { Right }
 Result:= ColumnIsBlack(Bmp, Bmp.Width-IndexDiff, Tolerance);
 if NOT Result then EXIT;
end;




{ Removes the black border that surronds an image. The returned image will 1 pixel (per border) smaller than the original image.
  Only works if the border is 1 pixel wide!

  Returns True if the black border was fixed }
procedure RemoveBorder(Bmp: TBitmap);     // Old name: RemoveBlackBorder
begin
 cGraphFX.CropBitmap(Bmp, 1, 1, Bmp.Width-2, bmp.Height-2);
end;





{--------------------------------------------------------------------------------------------------
  FADE BORDER
  Fades the borders of BMP to the color specified via BkgClrParams.
--------------------------------------------------------------------------------------------------}

{todo 1: FillTriangles: choose correct triangle middle !! }
{todo 2: FillTriangles: make a transition between triangles }

procedure FadeBorder(InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams; Border: TBorderSet);
VAR
   iTop, iLeft, iRight, iBott: Integer;
   TopColor, BtmColor, LeftColor, RightColor: TColor;
   XCenter,  YCenter: Integer;

   R_BkgClr, G_BkgClr, B_BkgClr: Byte;
   R1, R2, G1, G2, B1, B2: Byte;
   LNeigbour, MainPixel, RNeigbour: TRGB24;
   Line, NextLine: PRGB24Array;

  procedure FillRectangles;
   begin
    OutBMP.Canvas.Pen.Width := 0;

    if (btTop in Border) OR (btBottom in Border) then
     begin
      YCenter:= OutBMP.Height DIV 2;

      { Paint rectangles }
      OutBMP.Canvas.Brush.Color := TopColor;
      OutBMP.Canvas.Rectangle(0,0, OutBMP.Width, YCenter);

      OutBMP.Canvas.Brush.Color := BtmColor;
      OutBMP.Canvas.Rectangle(0,YCenter, OutBMP.Width, OutBMP.Height);
     end
    else
     if (btLeft in Border) OR (btRight in Border) then
      begin
       { Paint rectangles }
       OutBMP.Canvas.Brush.Color := LeftColor;
       OutBMP.Canvas.Rectangle(0,0, OutBMP.Width DIV 2, OutBMP.Height);

       OutBMP.Canvas.Brush.Color := RightColor;
       OutBMP.Canvas.Rectangle(OutBMP.Width DIV 2, 0, OutBMP.Width, OutBMP.Height);
      end
   end;

  procedure FillTriangles;
  VAR
     X1: Integer;
     Y1: Integer;
     X2, Y2: Integer;
  begin
   X1 := Round((OutBMP.Width  - InpBmp.Width)  / 2)-1;
   X2 := X1+ InpBmp.Width-1;

   Y1 := Round((OutBMP.Height - InpBmp.Height) / 2)-1;
   Y2 := Y1+ InpBmp.Height-1;

   OutBMP.Canvas.Pen.Width := 1;

   { Paint triangles }
   XCenter:= OutBMP.Width DIV 2;

   { Top triangle  }
   OutBMP.Canvas.Brush.Color := TopColor;
   DrawPolygon(OutBMP.Canvas, TopColor,   [0,0,   X1,Y1,   X2,Y1,  outbmp.Width-1,0]);

   { Right triangle }
   OutBMP.Canvas.Brush.Color := RightColor;
   DrawPolygon(OutBMP.Canvas, RightColor,  [outbmp.Width-1,0,    OutBMP.Width-1,OutBMP.Height-1,   X2,Y2,   X2,Y1]);

   { Bottom triangle }
   OutBMP.Canvas.Brush.Color := BtmColor;
   DrawPolygon(OutBMP.Canvas, BtmColor, [OutBMP.Width-1, OutBMP.Height-1,    0, OutBMP.Height-1,   X1,Y2,   X2, Y2]);

   { Left triangle }
   OutBMP.Canvas.Brush.Color := LeftColor;
   DrawPolygon(OutBMP.Canvas, LeftColor, [0,OutBMP.Height-1,    0,0,    x1, y1,  x1,y2]);
  end;

  procedure ComputeColorRange(Clr: TColor);    { If the current processed pixel is in this range we stop the processing }
  begin
   { Split BkgClr to RGB  }
   SplitColor2RGB(Clr, R_BkgClr, G_BkgClr, B_BkgClr);

   { R range}
   R1:= EnsureByte(R_BkgClr - BkgClrParams.EdgeSmear);
   R2:= EnsureByte(R_BkgClr + BkgClrParams.EdgeSmear);
   { G range}
   G1:= EnsureByte(G_BkgClr - BkgClrParams.EdgeSmear);
   G2:= EnsureByte(G_BkgClr + BkgClrParams.EdgeSmear);
   { B range}
   B1:= EnsureByte(B_BkgClr - BkgClrParams.EdgeSmear);
   B2:= EnsureByte(B_BkgClr + BkgClrParams.EdgeSmear);
  end;


  function ProcessRow: Integer;                                                              { Returns total number of black pixels on this row }
  VAR
     x1: Integer;
     R, G, B: Byte;                                                                          { Current pixel }
     FadeSpeed, NeighborWeight: Real;
   begin
    Result:= 0;
    FadeSpeed     := BkgClrParams.FadeSpeed      / 100;
    NeighborWeight:= BkgClrParams.NeighborWeight / 100;

    for x1:= iLeft+BkgClrParams.NeighborDist to iRight-BkgClrParams.NeighborDist-IndexDiff DO
     begin
      { Get 3 neighbour pixels }
      LNeigbour:= Line[x1-BkgClrParams.NeighborDist];                                           { NeighborDist - gives the fuzzyness }
      MainPixel:= Line[x1];
      RNeigbour:= Line[x1+BkgClrParams.NeighborDist];

      { Make average between neighbours }
      R:= EnsureByte((NeighborWeight*LNeigbour.R+ MainPixel.R+ NeighborWeight*RNeigbour.R) / 3);
      G:= EnsureByte((NeighborWeight*LNeigbour.G+ MainPixel.G+ NeighborWeight*RNeigbour.G) / 3);
      B:= EnsureByte((NeighborWeight*LNeigbour.B+ MainPixel.B+ NeighborWeight*RNeigbour.B) / 3);

      { Fade RGB pixel towards the BkgClr and put result in 'NextLine' }
      if InRange(R, R1, R2)                                                                   { If the current processed pixel (G) is in this range we stop the processing }
      then NextLine[x1].R:= R
      else
        if (R> R_BkgClr)
        then NextLine[x1].R:= EnsureByte(R- FadeSpeed)
        else
          if (R< R_BkgClr)
          then NextLine[x1].R:= EnsureByte(R+ FadeSpeed);

      if InRange(G, G1, G2)
      then NextLine[x1].G:= G
      else
        if (G> G_BkgClr)
        then NextLine[x1].G:= EnsureByte(G- FadeSpeed)
        else
          if (G< G_BkgClr)
          then NextLine[x1].G:= EnsureByte(G+ FadeSpeed);

      if InRange(B, B1, B2)
      then NextLine[x1].B:= B
      else
        if (B> B_BkgClr)
        then NextLine[x1].B:= EnsureByte(B- FadeSpeed)
        else
          if (B< B_BkgClr)
          then NextLine[x1].B:= EnsureByte(B+ FadeSpeed);
     end;
   end;


  function ProcessColumn(CurColumn, AdiaCol: Integer): Integer;   { AdiaCol is the adiacent column that will store the modified pixles }
  VAR
     Ln: Integer;
     R, G, B: Byte;   { Current pixel }
     MiddleLine, L: PRGB24Array;
     FadeSpeed, NeighborWeight: Real;
   begin
    Result:= 0;
    FadeSpeed:= BkgClrParams.FadeSpeed / 100;
    NeighborWeight:= BkgClrParams.NeighborWeight / 100;

    for Ln:= iTop+BkgClrParams.NeighborDist to iBott-BkgClrParams.NeighborDist-IndexDiff DO
     begin
      { Get 3 neighbour pixels }
      L:= OutBMP.Scanline[Ln-BkgClrParams.NeighborDist];
      Assert(CurColumn >= 0,           'Invalid curColumn: ' + IntToStr(CurColumn));
      Assert(CurColumn < OutBMP.Width, 'Invalid CurColumn: ' + IntToStr(CurColumn)+ '. OutBMP.Width: '+ IntToStr(OutBMP.Width));
      LNeigbour:= l[CurColumn];

      MiddleLine:= OutBMP.Scanline[Ln];
      MainPixel:= MiddleLine[CurColumn];

      {if Ln+BkgClrRec.NeighborDist >= OutBMP.Height
      then EmptyDummy; }

      L:= OutBMP.Scanline[Ln+BkgClrParams.NeighborDist];
      RNeigbour:= l[CurColumn];

      { Make average between neighbours }
      R:= EnsureByte((NeighborWeight*LNeigbour.R+ MainPixel.R+ NeighborWeight*RNeigbour.R) / 3);
      G:= EnsureByte((NeighborWeight*LNeigbour.G+ MainPixel.G+ NeighborWeight*RNeigbour.G) / 3);
      B:= EnsureByte((NeighborWeight*LNeigbour.B+ MainPixel.B+ NeighborWeight*RNeigbour.B) / 3);

      { Fade RGB pixel towards the BkgClr and put result in 'NextLine' }
      if InRange(R, R1, R2)                                { If the current processed pixel (G) is in this range we stop the processing }
      then
       begin
        Inc(Result);
        MiddleLine[AdiaCol].R:= R;
       end
      else
        if (R> R_BkgClr)
        then MiddleLine[AdiaCol].R:= EnsureByte(R- FadeSpeed)
        else
          if (R< R_BkgClr)
          then MiddleLine[AdiaCol].R:= EnsureByte(R+ FadeSpeed);

      if InRange(G, G1, G2)
      then
       begin
        Inc(Result);
        MiddleLine[AdiaCol].G:= G;
       end
      else
        if (G> G_BkgClr)
        then MiddleLine[AdiaCol].G:= EnsureByte(G- FadeSpeed)
        else
          if (G< G_BkgClr)
          then MiddleLine[AdiaCol].G:= EnsureByte(G+ FadeSpeed);

      if InRange(B, B1, B2)
      then
       begin
        Inc(Result);
        MiddleLine[AdiaCol].B:= B;
       end
      else
        if (B> B_BkgClr)
        then MiddleLine[AdiaCol].B:= EnsureByte(B- FadeSpeed)
        else
          if (B< B_BkgClr)
          then MiddleLine[AdiaCol].B:= EnsureByte(B+ FadeSpeed);
     end;
   end;


  procedure FadeBorders;
  VAR r, c: Integer;
  begin

   { Top border }
   if (btTop in Border) AND (TopColor > -1) then
    begin
     ComputeColorRange(TopColor);
     for r:= iTop+1 downto 0+1 DO                                                { Go where the to where the actual picture starts in the output image (pixel 101). Get pixels on this line make the effect and put the result one linne up. +1 because I have 'NextLine:= InpBmp.Scanline[y-1]' }
      begin
       Line    := OutBMP.Scanline[r];
       NextLine:= OutBMP.Scanline[r-1];
       ProcessRow;                                                             { Number of black pixels in current line ('Line') }
      end;
     end;

   { Bottom border }
   if (btBottom in Border) AND (BtmColor > -1) then
    begin
     ComputeColorRange(BtmColor);
     for r:= iBott-1 to OutBMP.Height-IndexDiff-1 DO                                   { Go down towards the bottom. 'Height-1' decause the Height is indexed in 0 and another -1 because I have 'NextLine:= InpBmp.Scanline[y+1]' below }
      begin
       Line    := OutBMP.Scanline[r];
       NextLine:= OutBMP.Scanline[r+1];
       ProcessRow;
      end;
     end;

   { Left }
   if (btLeft in Border) AND (LeftColor > -1)  then
    begin
     ComputeColorRange(LeftColor);
     for c:= iLeft downto 1                                                  { go towards left edge of the screen }
       DO ProcessColumn(c, c-1);                                             { Read this column and put the modified pixels in the adiacentcolumn }

    { Right }
    if (btRight in Border) AND (RightColor > -1)  then
     begin
      ComputeColorRange(RightColor);
      for c:= iRight-1 to OutBMP.Width-IndexDiff-1
       DO ProcessColumn(c, c+1);                                             { Read this column and put the modified pixels in the adiacentcolumn }
     end;
    end;
  end;


begin
 { Init }
 Assert(InpBmp <> NIL);
 Assert(OutBMP <> NIL);
 if (InpBmp.Width< 12) OR (InpBmp.Height< 12) then EXIT;

 if (OutBMP.Width <= InpBmp.Width)                            { if input and output have same width and... }
 AND (NOT (btTop in Border) OR NOT (btBottom in Border))      { ... we don't have to process the top/bottom border }
 then EXIT;                                                   { Then there is nothing to do here }

 if (OutBMP.Height<= InpBmp.Height)                           { if input and output have same height and... }
 AND (NOT (btLeft in Border) OR NOT (btRight in Border))      { ... we don't have to process the left/right border }
 then EXIT;                                                   { Then there is nothing to do here }


 Assert(inpBMP.pixelformat= pf24bit, 'FadeBorder - inpBMP Wrong PixelFormat!');
 Assert(OutBMP.pixelformat= pf24bit, 'FadeBorder - Wrong PixelFormat!');

 { Compute from where I start the effect }
 iTop  := (OutBMP.Height - InpBmp.Height) div 2;
 iLeft := (OutBMP.Width  - InpBmp.Width)  div 2;

 Assert(iTop  >= 0, 'iTop.  The wallpaper is larger than the desktop! '+ IntToStr(iTop));
 Assert(iLeft >= 0, 'iLeft. The wallpaper is larger than the desktop! '+ IntToStr(iLeft));
 (* if iTop < 0then iTop:= 0;                 { This happens when the wallpaper is bigger than the desktop! }
 if iLeft < 0 then iLeft:= 0;  *)

 iRight:= InpBmp.Width + iLeft;
 iBott := InpBmp.Height+ iTop;

 if iRight > OutBMP.Width
 then iRight:= OutBMP.Width;     { This happens when the wallpaper is bigger than the desktop! }
 if iBott > OutBMP.Height
 then iBott:= OutBMP.Height;


 case BkgClrParams.EffectColor of
  ecAutoDetBorder:           { Detect border color }
    begin
     if (btTop    in Border) then TopColor  := GetBorderDominantColor (InpBmp, btTop,    BkgClrParams.Tolerance) else TopColor  := clBlack;
     if (btBottom in Border) then BtmColor  := GetBorderDominantColor (InpBmp, btBottom, BkgClrParams.Tolerance) else BtmColor  := clBlack;
     if (btLeft   in Border) then LeftColor := GetBorderDominantColor (InpBmp, btLeft,   BkgClrParams.Tolerance) else LeftColor := clBlack;
     if (btRight  in Border) then RightColor:= GetBorderDominantColor (InpBmp, btRight,  BkgClrParams.Tolerance) else RightColor:= clBlack;
    end;
  ecImageAverage:
    begin
     //ToDo 5: Calculate the bkg color and store it to the RWallpaper object. Problem: how do I know when the parameters were change and I need to recompute the color?
     TopColor  := GetAverageColor(InpBmp, FALSE);
     BtmColor  := TopColor;
     LeftColor := TopColor;
     RightColor:= TopColor;
    end;
  ecUserColor:
    begin
     TopColor  := BkgClrParams.Color;
     BtmColor  := BkgClrParams.Color;
     LeftColor := BkgClrParams.Color;
     RightColor:= BkgClrParams.Color;
    end;
 end;

 case BkgClrParams.EffectShape of

   esRectangles:
    begin
     FillRectangles;
     OutBMP.Canvas.Draw(iLeft, iTop, InpBmp);         { Copy input BMP in the middle of the output BMP }
     if BkgClrParams.FillType= ftFade then FadeBorders;            { Fade borders to color }
    end;

   esTriangles:
    begin
     FillTriangles;
     OutBMP.Canvas.Draw(iLeft, iTop, InpBmp);         { Copy input BMP in the middle of the output BMP }
     if BkgClrParams.FillType= ftFade then  FadeBorders;
    end;

   esOneColor:
    begin
     FillBitmap(OutBMP, TopColor);                    { Fill background with clr }
     OutBMP.Canvas.Draw(iLeft, iTop, InpBmp);         { Copy input BMP in the middle of the output BMP }
     if BkgClrParams.FillType= ftFade then  FadeBorders;
    end;

 end;
end;



{ Same as FadeBorder but automatically detects which borders to process based n the InpBMP/OutBMP size ratio }
procedure FadeBorderAuto (InpBmp, OutBMP: TBitmap; BkgClrParams: RBkgColorParams);
VAR Border: TBorderSet;
begin
 inpBMP.PixelFormat:= pf24bit;
 OutBMP.PixelFormat:= pf24bit;

 if  (OutBmp.Height > InpBmp.Height)
 AND (OutBmp.Width <= InpBmp.Width)
 then Border:=  [btTop, btBottom]
 else

 if  (OutBmp.Width  >  InpBmp.Width)
 AND (OutBmp.Height <= InpBmp.Height)
 then Border:= [btLeft, btRight]
 else

 if  (OutBmp.Width  > InpBmp.Width)
 AND (OutBmp.Height > InpBmp.Height)
 then Border:= [btTop, btBottom, btLeft, btRight]
 else

 if  (OutBmp.Width  <= InpBmp.Width)
 AND (OutBmp.Height <= InpBmp.Height)
 then Border:= [btTop, btBottom, btLeft, btRight]
 else
  begin
   Mesaj('OutBMP < InpBMP');
   //OutBMP.Canvas.Draw(iLeft, iTop, InpBmp);    { Copy input BMP in the middle of the output BMP }
   EXIT;
  end;

 if  HasBlackBorder(InpBmp, 6)
 then RemoveBorder(InpBmp);

 FadeBorder (InpBmp, OutBMP, BkgClrParams, Border)
end;






{--------------------------------------------------------------------------------------------------
  BORDER COLOR
  Returns the DOMINANT color in the specified border of the image
--------------------------------------------------------------------------------------------------}
function GetBorderDominantColor;
TYPE
   RDot= record
      Col  : TColor;
      Count: Integer;
     end;
VAR
   i, m, TotalUniqueColors, CommonColCount, MaxDistance: Integer;
   UniqueXM: array of RDot;                                                                     { matrix of unique colors }
   Unique: Boolean;
   CurColor: TColor;
begin
 Result:= -1;
 if (BMP.Width< 4) OR (BMP.Height< 4) then EXIT(-1);                                            { Check for invalid images }

 CurColor:= 0;
 TotalUniqueColors:= 0;                                                                         { daca toti pixelii sunt de acceasi culoare (negru de exemplu) atunci am cel putin o culoare (negru) }
 SetLength(UniqueXM, 100);                                                                { 0 indexed }

 if (Border= btTop)
 OR (Border= btBottom)
 then MaxDistance:= BMP.Width-1
 else MaxDistance:= BMP.Height-1;

 { Get current color }
 for i:= 0 to MaxDistance DO                                                                    {todo 3: this could be twice faster if I process only the odd/even pixels}
  begin                                                                                         {todo 2: this will much faster if load the line of pixels in an array (scanline - dirrect access to bitmap). it will only work for horizontal lines. http://stackoverflow.com/questions/13583451/how-to-use-scanline-property-for-24-bit-bitmaps }
   case Border of
     btTop    : CurColor:= BMP.Canvas.Pixels[i, 0];                             { Top of the wallpaper }
     btBottom : CurColor:= BMP.Canvas.Pixels[i, BMP.Height-1];                  { Bottom of the wallpaper }
     btLeft   : CurColor:= BMP.Canvas.Pixels[0, i];                             { Left of the wallpaper }
     btRight  : CurColor:= BMP.Canvas.Pixels[BMP.Width-1, i];                   { Right of the wallpaper }
   end;

   { Check if current color is unique }
   Unique:= TRUE;
   for m:= 0 to TotalUniqueColors-1 DO
    if SimilarColor(CurColor, UniqueXM[m].Col, Tolerance) then
     begin
      Inc(UniqueXM[m].Count);
      Unique:= FALSE;
      break;
     end;

   if NOT Unique then continue;

   { if unique, add it to the matrix of unique colors }
   Assert(TotalUniqueColors < Length(UniqueXM), 'TotalUniqueColors: '+ IntToStr(TotalUniqueColors)+ '   Length(UniqueXM)'+IntToStr(Length(UniqueXM)));

   UniqueXM[TotalUniqueColors].Col  := CurColor;
   UniqueXM[TotalUniqueColors].Count:= 1;

   Inc(TotalUniqueColors);

   { Expand array if necessary }
   if TotalUniqueColors> High(UniqueXM)
   then SetLength(UniqueXM, TotalUniqueColors+1);
  end;

 { Find most used color }
 CommonColCount:= -1;

 for m:= 0 to TotalUniqueColors-1 DO
  if UniqueXM[m].Count> CommonColCount
  then
   begin
    CommonColCount:= UniqueXM[m].Count;
    Result:= UniqueXM[m].Col;
   end;
end;



end.
