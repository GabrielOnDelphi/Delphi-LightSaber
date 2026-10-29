UNIT LightVcl.Visual.SvgSpeedButton;

{=============================================================================================================
   2026.10.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TLightSvgSpeedButton - a TSpeedButton that draws a solid one-colour SVG icon on top of its face.

   Paint calls inherited Paint first, so the VCL style (or Windows) still paints the button face and the
   hot and pressed frames; then the icon is blended on top, centred in the button.
   The icon is drawn with GDI+ (Winapi.GDIPOBJ, part of Windows since XP) from the shapes that
   LightCore.SvgShapes reads. No Skia, no DLL.
   Every shape is drawn in the colour of the current state, whatever colour the SVG text names:
   GlyphColor (normal), HotColor (mouse over), PressedColor (down). The disabled state uses GlyphColor at
   DisabledOpacity (0..255, default 102 = 40%).

   Cache: one bitmap per state, at the current glyph size. It is rebuilt when the size, a colour or the
   SVG text changes, so a repaint only blends a ready bitmap.

   Glyph size: GlyphSize pixels square; 0 = 64% of the smaller side of the button.

   Colours that stand out on the active VCL style: call PickColorsFromStyle once the button exists. It
   picks white or #202020 (BestContrastColor) against the style's panel colour, its hot toolbar-button
   colour and its pressed toolbar-button colour. Made for a flat button that sits on a TPanel.

   Usage:
     Btn:= TLightSvgSpeedButton.Create(Self);
     Btn.Parent:= Panel1;
     Btn.Flat:= TRUE;
     Btn.SvgText:= '<svg viewBox="0 0 24 24"><path d="M4 4.5 L15.5 12 L4 19.5 Z"/></svg>';
     Btn.PickColorsFromStyle;

   SvgText raises ESvgError (LightCore.SvgShapes) when the text cannot be read, when it is assigned.

   Tests: UnitTesting\Test.LightVcl.Visual.SvgSpeedButton.pas
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Math, System.Types,
  Winapi.Windows, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  Vcl.Graphics, Vcl.Controls, Vcl.Buttons, Vcl.Themes,
  LightCore.SvgShapes;

TYPE
  ESvgRenderError = class(Exception);

  TSvgGlyphState = (gsNormal, gsHot, gsPressed, gsDisabled);

  TLightSvgSpeedButton = class(TSpeedButton)
  private
    FSvgText: string;
    FImage: TSvgImage;
    FGlyphColor: TColor;
    FHotColor: TColor;
    FPressedColor: TColor;
    FGlyphSize: Integer;
    FDisabledOpacity: Byte;
    FCache: array[TSvgGlyphState] of TBitmap;
    FCacheSize: Integer;
    procedure SetSvgText(CONST Value: string);
    procedure SetGlyphColor(Value: TColor);
    procedure SetHotColor(Value: TColor);
    procedure SetPressedColor(Value: TColor);
    procedure SetGlyphSize(Value: Integer);
    procedure SetDisabledOpacity(Value: Byte);
    procedure ClearCache;
    function  CurrentState: TSvgGlyphState;
    function  StateColor(State: TSvgGlyphState): TColor;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PickColorsFromStyle;
    function  ActualGlyphSize: Integer;
    function  GlyphBitmap(State: TSvgGlyphState): TBitmap;   { The cached glyph of State, owned by the button. NIL when SvgText is empty or the size is 0. }
  published
    property SvgText: string read FSvgText write SetSvgText;
    property GlyphColor: TColor read FGlyphColor write SetGlyphColor default clBlack;
    property HotColor: TColor read FHotColor write SetHotColor default clBlack;
    property PressedColor: TColor read FPressedColor write SetPressedColor default clBlack;
    property GlyphSize: Integer read FGlyphSize write SetGlyphSize default 0;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 102;
  end;

{ Draws Img, fitted into a Size x Size square with its aspect ratio kept, every shape in Color.
  Returns a pf32bit bitmap with premultiplied alpha (AlphaFormat = afPremultiplied); the caller frees it. }
function RenderSvgGlyph(CONST Img: TSvgImage; Size: Integer; Color: TColor): TBitmap;

function ContrastRatio(C1, C2: TColor): Double;           { WCAG 2 contrast ratio, 1 to 21 }
function BestContrastColor(Background: TColor): TColor;    { White or #202020, whichever has the higher ContrastRatio on Background }

procedure Register;


IMPLEMENTATION

CONST
  NearBlack       = TColor($202020);
  AutoSizePercent = 0.64;      { GlyphSize 0: the glyph side as a part of the smaller button side }


{-------------------------------------------------------------------------------------------------------------
   DRAWING
-------------------------------------------------------------------------------------------------------------}

procedure GdiCheck(Status: Winapi.GDIPAPI.TStatus; CONST What: string);
begin
  if Status <> Winapi.GDIPAPI.Ok
  then raise ESvgRenderError.Create('RenderSvgGlyph: GDI+ ' + What + ' failed with status ' + IntToStr(Ord(Status)));
end;


{ The figures of one shape, in the shape's own coordinates }
function BuildPath(CONST Shape: TSvgShape): TGPGraphicsPath;
VAR
  Seg: TSvgSegment;
  Cur, Start: TSvgPoint;
  RX2, RY2: Single;
begin
  { The default of TGPGraphicsPath.Create is FillModeAlternate (even-odd); SVG's default is nonzero = FillModeWinding }
  if Shape.FillRule = sfrEvenOdd
  then Result:= TGPGraphicsPath.Create(FillModeAlternate)
  else Result:= TGPGraphicsPath.Create(FillModeWinding);
  try
    case Shape.Kind of
      sskPath:
        begin
          Cur.X:= 0;
          Cur.Y:= 0;
          Start:= Cur;
          for Seg in Shape.Segments do
            case Seg.Kind of
              sgMoveTo:
                begin
                  Result.StartFigure;
                  Cur:= Seg.Pt;
                  Start:= Seg.Pt;
                end;
              sgLineTo:
                begin
                  Result.AddLine(Cur.X, Cur.Y, Seg.Pt.X, Seg.Pt.Y);
                  Cur:= Seg.Pt;
                end;
              sgCubicTo:
                begin
                  Result.AddBezier(Cur.X, Cur.Y, Seg.Ctrl1.X, Seg.Ctrl1.Y, Seg.Ctrl2.X, Seg.Ctrl2.Y, Seg.Pt.X, Seg.Pt.Y);
                  Cur:= Seg.Pt;
                end;
              sgClose:
                begin
                  Result.CloseFigure;
                  Cur:= Start;
                end;
            end;
        end;

      sskRect:
        if (Shape.RX <= 0) OR (Shape.RY <= 0)
        then Result.AddRectangle(MakeRect(Shape.X, Shape.Y, Shape.Width, Shape.Height))
        else
          begin
            { Four quarter arcs; GDI+ joins consecutive arcs of a figure with straight lines }
            RX2:= 2 * Shape.RX;
            RY2:= 2 * Shape.RY;
            Result.StartFigure;
            Result.AddArc(Shape.X,                       Shape.Y,                        RX2, RY2, 180, 90);
            Result.AddArc(Shape.X + Shape.Width - RX2,   Shape.Y,                        RX2, RY2, 270, 90);
            Result.AddArc(Shape.X + Shape.Width - RX2,   Shape.Y + Shape.Height - RY2,   RX2, RY2,   0, 90);
            Result.AddArc(Shape.X,                       Shape.Y + Shape.Height - RY2,   RX2, RY2,  90, 90);
            Result.CloseFigure;
          end;

      sskEllipse:
        Result.AddEllipse(Shape.CX - Shape.RX, Shape.CY - Shape.RY, 2 * Shape.RX, 2 * Shape.RY);
    end;

    GdiCheck(Result.GetLastStatus, 'path');
  except
    FreeAndNil(Result);
    raise;
  end;
end;


procedure DrawShape(Canvas: TGPGraphics; Brush: TGPSolidBrush; GpColor: ARGB; CONST Shape: TSvgShape; Scale, OffX, OffY: Single);
VAR
  Matrix, ShapeMatrix: TGPMatrix;
  Path: TGPGraphicsPath;
  Pen: TGPPen;
begin
  { viewBox to pixels, applied after the shape's own transform }
  Matrix:= TGPMatrix.Create(Scale, 0, 0, Scale, OffX, OffY);
  try
    ShapeMatrix:= TGPMatrix.Create(Shape.Transform.A, Shape.Transform.B, Shape.Transform.C, Shape.Transform.D, Shape.Transform.E, Shape.Transform.F);
    try
      GdiCheck(Matrix.Multiply(ShapeMatrix, MatrixOrderPrepend), 'Matrix.Multiply');
    finally
      FreeAndNil(ShapeMatrix);
    end;
    GdiCheck(Canvas.SetTransform(Matrix), 'SetTransform');
  finally
    FreeAndNil(Matrix);
  end;

  Path:= BuildPath(Shape);
  try
    if Shape.Fill.Kind <> spkNone
    then GdiCheck(Canvas.FillPath(Brush, Path), 'FillPath');

    { The pen width is in shape units: the world transform scales it like the shape }
    if (Shape.Stroke.Kind <> spkNone) AND (Shape.StrokeWidth > 0) then
      begin
        Pen:= TGPPen.Create(GpColor, Shape.StrokeWidth);
        try
          GdiCheck(Canvas.DrawPath(Pen, Path), 'DrawPath');
        finally
          FreeAndNil(Pen);
        end;
      end;
  finally
    FreeAndNil(Path);
  end;
end;


function RenderSvgGlyph(CONST Img: TSvgImage; Size: Integer; Color: TColor): TBitmap;
VAR
  GpBmp: TGPBitmap;
  Canvas: TGPGraphics;
  Brush: TGPSolidBrush;
  Shape: TSvgShape;
  Scale, OffX, OffY: Single;
  RGB: Cardinal;
  GpColor: ARGB;
  Data: TBitmapData;
  y: Integer;
begin
  if Size < 1 then raise ESvgRenderError.Create('RenderSvgGlyph: Size must be at least 1, not ' + IntToStr(Size));
  if (Img.ViewBox.Width <= 0) OR (Img.ViewBox.Height <= 0) then raise ESvgRenderError.Create('RenderSvgGlyph: the image has no viewBox');

  RGB:= Cardinal(ColorToRGB(Color));
  GpColor:= MakeColor(255, GetRValue(RGB), GetGValue(RGB), GetBValue(RGB));

  { Fit the viewBox into the square, centred, aspect ratio kept (SVG "xMidYMid meet") }
  Scale:= Min(Size / Img.ViewBox.Width, Size / Img.ViewBox.Height);
  OffX:= (Size - Img.ViewBox.Width  * Scale) / 2 - Img.ViewBox.MinX * Scale;
  OffY:= (Size - Img.ViewBox.Height * Scale) / 2 - Img.ViewBox.MinY * Scale;

  Result:= TBitmap.Create;
  try
    Result.PixelFormat:= pf32bit;
    Result.SetSize(Size, Size);
    Result.AlphaFormat:= afPremultiplied;   { Canvas.Draw then blends it with AlphaBlend. Every pixel is overwritten below. }

    GpBmp:= TGPBitmap.Create(Size, Size, PixelFormat32bppPARGB);
    try
      GdiCheck(GpBmp.GetLastStatus, 'TGPBitmap.Create');

      { # Draw }
      Canvas:= TGPGraphics.Create(GpBmp);
      try
        GdiCheck(Canvas.GetLastStatus, 'TGPGraphics.Create');
        GdiCheck(Canvas.Clear(MakeColor(0, 0, 0, 0)), 'Clear');
        GdiCheck(Canvas.SetSmoothingMode(SmoothingModeAntiAlias8x8), 'SetSmoothingMode');
        GdiCheck(Canvas.SetPixelOffsetMode(PixelOffsetModeHalf), 'SetPixelOffsetMode');   { Pixel centres at +0.5, so an edge on a whole coordinate falls between two pixels }
        Brush:= TGPSolidBrush.Create(GpColor);
        try
          for Shape in Img.Shapes do
            DrawShape(Canvas, Brush, GpColor, Shape, Scale, OffX, OffY);
        finally
          FreeAndNil(Brush);
        end;
      finally
        FreeAndNil(Canvas);
      end;

      { # Copy the premultiplied BGRA rows into the VCL bitmap }
      GdiCheck(GpBmp.LockBits(MakeRect(0, 0, Size, Size), ImageLockModeRead, PixelFormat32bppPARGB, Data), 'LockBits');
      try
        for y:= 0 to Size - 1 do
          Move((PByte(Data.Scan0) + y * Data.Stride)^, Result.ScanLine[y]^, Size * 4);
      finally
        GpBmp.UnlockBits(Data);
      end;
    finally
      FreeAndNil(GpBmp);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   COLOURS
-------------------------------------------------------------------------------------------------------------}

{ WCAG 2 relative luminance }
function RelLuminance(Color: TColor): Double;

  function Channel(C: Byte): Double;
  VAR S: Double;
  begin
    S:= C / 255;
    if S <= 0.03928
    then Result:= S / 12.92
    else Result:= Power((S + 0.055) / 1.055, 2.4);
  end;

VAR RGB: Cardinal;
begin
  RGB:= Cardinal(ColorToRGB(Color));
  Result:= 0.2126 * Channel(GetRValue(RGB)) + 0.7152 * Channel(GetGValue(RGB)) + 0.0722 * Channel(GetBValue(RGB));
end;


function ContrastRatio(C1, C2: TColor): Double;
VAR L1, L2: Double;
begin
  L1:= RelLuminance(C1);
  L2:= RelLuminance(C2);
  Result:= (Max(L1, L2) + 0.05) / (Min(L1, L2) + 0.05);
end;


function BestContrastColor(Background: TColor): TColor;
begin
  if ContrastRatio(clWhite, Background) >= ContrastRatio(NearBlack, Background)
  then Result:= clWhite
  else Result:= NearBlack;
end;


{ The centre pixel of a style element drawn over Background. The hot and pressed toolbar buttons are pictures in most styles. }
function ElementCentreColor(Style: TCustomStyleServices; Element: TThemedToolBar; Background: TColor): TColor;
CONST Side = 24;
VAR Bmp: TBitmap;
begin
  Bmp:= TBitmap.Create;
  try
    Bmp.SetSize(Side, Side);
    Bmp.Canvas.Brush.Color:= Background;
    Bmp.Canvas.FillRect(Rect(0, 0, Side, Side));
    Style.DrawElement(Bmp.Canvas.Handle, Style.GetElementDetails(Element), Rect(0, 0, Side, Side));
    Result:= Bmp.Canvas.Pixels[Side DIV 2, Side DIV 2];
  finally
    FreeAndNil(Bmp);
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   TLightSvgSpeedButton
-------------------------------------------------------------------------------------------------------------}

constructor TLightSvgSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphColor     := clBlack;
  FHotColor       := clBlack;
  FPressedColor   := clBlack;
  FDisabledOpacity:= 102;
end;


destructor TLightSvgSpeedButton.Destroy;
begin
  ClearCache;
  inherited Destroy;
end;


procedure TLightSvgSpeedButton.ClearCache;
VAR State: TSvgGlyphState;
begin
  for State:= Low(TSvgGlyphState) to High(TSvgGlyphState) do
    FreeAndNil(FCache[State]);
end;


procedure TLightSvgSpeedButton.SetSvgText(CONST Value: string);
begin
  if Value = FSvgText then EXIT;
  if Value = ''
  then FImage:= Default(TSvgImage)
  else FImage:= ParseSvg(Value);   { Raises before anything changes }
  FSvgText:= Value;
  ClearCache;
  Invalidate;
end;


procedure TLightSvgSpeedButton.SetGlyphColor(Value: TColor);
begin
  if Value = FGlyphColor then EXIT;
  FGlyphColor:= Value;
  ClearCache;
  Invalidate;
end;


procedure TLightSvgSpeedButton.SetHotColor(Value: TColor);
begin
  if Value = FHotColor then EXIT;
  FHotColor:= Value;
  ClearCache;
  Invalidate;
end;


procedure TLightSvgSpeedButton.SetPressedColor(Value: TColor);
begin
  if Value = FPressedColor then EXIT;
  FPressedColor:= Value;
  ClearCache;
  Invalidate;
end;


procedure TLightSvgSpeedButton.SetGlyphSize(Value: Integer);
begin
  if Value = FGlyphSize then EXIT;
  FGlyphSize:= Max(0, Value);
  Invalidate;    { GlyphBitmap rebuilds the cache when the size differs }
end;


procedure TLightSvgSpeedButton.SetDisabledOpacity(Value: Byte);
begin
  if Value = FDisabledOpacity then EXIT;
  FDisabledOpacity:= Value;
  Invalidate;
end;


function TLightSvgSpeedButton.ActualGlyphSize: Integer;
begin
  if FGlyphSize > 0
  then Result:= FGlyphSize
  else Result:= Max(0, Round(Min(Width, Height) * AutoSizePercent));
end;


function TLightSvgSpeedButton.StateColor(State: TSvgGlyphState): TColor;
begin
  case State of
    gsHot    : Result:= FHotColor;
    gsPressed: Result:= FPressedColor;
  else
    Result:= FGlyphColor;     { gsNormal; gsDisabled is the same colour, blended at DisabledOpacity }
  end;
end;


function TLightSvgSpeedButton.GlyphBitmap(State: TSvgGlyphState): TBitmap;
VAR Size: Integer;
begin
  Size:= ActualGlyphSize;
  if Size <> FCacheSize then
    begin
      ClearCache;
      FCacheSize:= Size;
    end;
  if (FSvgText = '') OR (Size < 1) then EXIT(NIL);

  if FCache[State] = NIL
  then FCache[State]:= RenderSvgGlyph(FImage, Size, StateColor(State));
  Result:= FCache[State];
end;


function TLightSvgSpeedButton.CurrentState: TSvgGlyphState;
begin
  if NOT Enabled                      then EXIT(gsDisabled);
  if FState in [bsDown, bsExclusive]  then EXIT(gsPressed);
  if MouseInControl                   then EXIT(gsHot);
  Result:= gsNormal;
end;


procedure TLightSvgSpeedButton.Paint;
VAR
  State: TSvgGlyphState;
  Glyph: TBitmap;
  X, Y: Integer;
begin
  inherited Paint;   { The face, and the hot / pressed frame of the style }

  State:= CurrentState;
  Glyph:= GlyphBitmap(State);
  if Glyph = NIL then EXIT;

  X:= (Width  - Glyph.Width)  DIV 2;
  Y:= (Height - Glyph.Height) DIV 2;
  if State = gsDisabled
  then Canvas.Draw(X, Y, Glyph, FDisabledOpacity)
  else Canvas.Draw(X, Y, Glyph);
end;


procedure TLightSvgSpeedButton.PickColorsFromStyle;
VAR
  Style: TCustomStyleServices;
  Bar, C: TColor;
begin
  Style:= StyleServices(Self);

  { The colour TCustomPanel.Paint fills a panel with }
  Bar:= ColorToRGB(clBtnFace);
  if Style.Enabled
  AND Style.GetElementColor(Style.GetElementDetails(tpPanelBackground), ecFillColor, C)
  AND (C <> clNone)
  then Bar:= ColorToRGB(C);

  GlyphColor  := BestContrastColor(Bar);
  HotColor    := BestContrastColor(ElementCentreColor(Style, ttbButtonHot,     Bar));
  PressedColor:= BestContrastColor(ElementCentreColor(Style, ttbButtonPressed, Bar));
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightSvgSpeedButton]);
end;


end.
