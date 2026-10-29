UNIT LightCore.SvgShapes;

{=============================================================================================================
   2026.10.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A small SVG reader. It turns the text of a simple SVG icon into a neutral list of shapes (paths,
   rectangles, ellipses) in viewBox coordinates. It draws nothing: a drawing layer (GDI+ in a VCL control,
   for example) turns the shapes into its own figures.
   RTL only - no VCL, FMX or Winapi unit - so a console tool or an Android build can use it too.

   Supported:
     Elements   svg (viewBox, or width + height), path, rect (rx, ry), circle, ellipse, polygon.
                title, desc, metadata, comments and namespaced elements (inkscape:..., sodipodi:...) are skipped.
     Path data  M L H V C Z and their relative forms m l h v c z. Packed numbers ("M10-5.5.5") and exponents.
     Paint      fill and stroke = none | currentColor | #rgb | #rrggbb | black | white. stroke-width, fill-rule.
                Set on the <svg> element, they are inherited by every shape.
     Transform  matrix, translate, scale, rotate(a), rotate(a cx cy); a list of them is composed.

   Anything else that changes the drawing (arcs, quadratic and smooth curves, gradients, <g>, <use>,
   CSS style ...) raises ESvgUnsupported, whose message names the unsupported thing.
   Empty or malformed text raises ESvgError.
   Numbers are read digit by digit with "." as the decimal separator, so the locale plays no part.

   Usage:
     Img:= ParseSvg(SvgText);
     for Shape in Img.Shapes do ...   // Shape.Kind: sskPath (Segments), sskRect (X, Y, Width, Height, RX, RY), sskEllipse (CX, CY, RX, RY)

   Tests: UnitTesting\Test.LightCore.SvgShapes.pas
=============================================================================================================}

{$ZEROBASEDSTRINGS OFF}

INTERFACE

USES
  System.SysUtils, System.UITypes;

TYPE
  ESvgError       = class(Exception);     { The SVG text is empty or malformed }
  ESvgUnsupported = class(ESvgError);     { The SVG text is valid, but uses a feature this reader does not support }

  TSvgPoint = record
    X, Y: Single;
  end;

  TSvgSegmentKind = (sgMoveTo, sgLineTo, sgCubicTo, sgClose);

  { One step of a path, in absolute coordinates. Every figure starts with sgMoveTo. }
  TSvgSegment = record
    Kind : TSvgSegmentKind;
    Ctrl1: TSvgPoint;                     { sgCubicTo only }
    Ctrl2: TSvgPoint;                     { sgCubicTo only }
    Pt   : TSvgPoint;                     { End point. Unused by sgClose }
  end;
  TSvgSegments = array of TSvgSegment;

  TSvgShapeKind = (sskPath, sskRect, sskEllipse);       { <polygon> gives sskPath, <circle> gives sskEllipse }
  TSvgPaintKind = (spkNone, spkColor, spkCurrentColor);  { spkCurrentColor: the caller picks the colour }
  TSvgFillRule  = (sfrNonZero, sfrEvenOdd);

  TSvgPaint = record
    Kind : TSvgPaintKind;
    Color: TAlphaColor;                   { $AARRGGBB. Only for spkColor }
  end;

  { 2D affine matrix, in SVG order:  X' = A*X + C*Y + E,  Y' = B*X + D*Y + F }
  TSvgMatrix = record
    A, B, C, D, E, F: Single;
    class function Identity: TSvgMatrix; static;
    function IsIdentity: Boolean;
    function Transform(CONST P: TSvgPoint): TSvgPoint;
    function Multiply(CONST Right: TSvgMatrix): TSvgMatrix;   { Self * Right: Right is applied first }
  end;

  TSvgShape = record
    Kind       : TSvgShapeKind;
    Segments   : TSvgSegments;            { sskPath }
    X, Y       : Single;                  { sskRect: top-left corner }
    Width      : Single;                  { sskRect }
    Height     : Single;                  { sskRect }
    CX, CY     : Single;                  { sskEllipse: centre }
    RX, RY     : Single;                  { sskRect: corner radii, 0 = sharp corners. sskEllipse: radii }
    Fill       : TSvgPaint;
    Stroke     : TSvgPaint;
    StrokeWidth: Single;
    FillRule   : TSvgFillRule;
    Transform  : TSvgMatrix;              { Maps the shape's own coordinates to viewBox coordinates }
  end;
  TSvgShapes = array of TSvgShape;

  TSvgViewBox = record
    MinX, MinY, Width, Height: Single;
  end;

  TSvgImage = record
    ViewBox: TSvgViewBox;
    Shapes : TSvgShapes;                  { In drawing order }
  end;

function ParseSvg        (CONST SvgText : string): TSvgImage;
function ParseSvgPathData(CONST PathData: string): TSvgSegments;


IMPLEMENTATION


CONST
  ColorBlack    = TAlphaColor($FF000000);
  ColorWhite    = TAlphaColor($FFFFFFFF);
  MaxCoordinate = 1E30;                   { Larger numbers are rejected, so every value fits in a Single }
  MaxDigits     = 60;                     { Longer mantissas are rejected, so the Double accumulator cannot overflow }

  { Attributes that change the drawing and that this reader does not support }
  UnsupportedAttributes: array[0..18] of string = (
    'style', 'opacity', 'fill-opacity', 'stroke-opacity', 'clip-path', 'mask', 'filter',
    'stroke-dasharray', 'stroke-dashoffset', 'stroke-linecap', 'stroke-linejoin', 'stroke-miterlimit',
    'marker-start', 'marker-mid', 'marker-end', 'color', 'visibility', 'display', 'vector-effect');

  { Elements that are not drawn. They are skipped together with everything inside them. }
  SkippedElements: array[0..2] of string = ('title', 'desc', 'metadata');

  LengthUnits: array[0..10] of string = ('%', 'em', 'ex', 'rem', 'pt', 'pc', 'cm', 'mm', 'in', 'vw', 'vh');

TYPE
  TNumbers = array of Double;

  TAttribute = record
    Name, Value: string;
  end;
  TAttributes = array of TAttribute;

  { The paint attributes that a shape inherits from the <svg> element }
  TPresentation = record
    Fill, Stroke: TSvgPaint;
    StrokeWidth : Single;
    FillRule    : TSvgFillRule;
  end;

  TContentMode = (cmSvg,        { Children of <svg>: shapes }
                  cmShape,      { Children of a shape: only skipped elements are allowed }
                  cmSkip);      { Inside a skipped element: everything is skipped }

  TSvgTextReader = record
  private
    S: string;
    P, Len: Integer;
    Shapes: TSvgShapes;
    ShapeCount: Integer;
    Root: TPresentation;
    function  StartsHere(CONST Sub: string): Boolean;
    procedure SkipPast(CONST Terminator, What: string);
    function  SkipMarkup: Boolean;
    function  ReadName: string;
    procedure ReadStartTag(OUT Name: string; OUT Attributes: TAttributes; OUT SelfClosing: Boolean);
    procedure ReadEndTag(CONST Expected: string);
    procedure ReadContent(CONST Parent: string; Mode: TContentMode);
    procedure ReadRoot(CONST Attributes: TAttributes; OUT ViewBox: TSvgViewBox);
    procedure ReadShape(CONST Name: string; CONST Attributes: TAttributes);
    procedure AddShape(CONST Shape: TSvgShape);
  public
    function Parse(CONST SvgText: string): TSvgImage;
  end;

  TSegmentList = record
    Items: TSvgSegments;
    Count: Integer;
    procedure Add(Kind: TSvgSegmentKind; CONST Ctrl1, Ctrl2, Pt: TSvgPoint);
    function  ToArray: TSvgSegments;
  end;



{-------------------------------------------------------------------------------------------------------------
   ERRORS
-------------------------------------------------------------------------------------------------------------}

function SvgError(CONST Msg: string): ESvgError;
begin
  Result:= ESvgError.Create('SVG: ' + Msg);
end;


function SvgUnsupported(CONST What: string): ESvgUnsupported;
begin
  Result:= ESvgUnsupported.Create('SVG: not supported - ' + What);
end;



{-------------------------------------------------------------------------------------------------------------
   TSvgMatrix
-------------------------------------------------------------------------------------------------------------}

class function TSvgMatrix.Identity: TSvgMatrix;
begin
  Result.A:= 1;
  Result.B:= 0;
  Result.C:= 0;
  Result.D:= 1;
  Result.E:= 0;
  Result.F:= 0;
end;


function TSvgMatrix.IsIdentity: Boolean;
begin
  Result:= (A = 1) AND (B = 0) AND (C = 0) AND (D = 1) AND (E = 0) AND (F = 0);
end;


function TSvgMatrix.Transform(CONST P: TSvgPoint): TSvgPoint;
begin
  Result.X:= A * P.X + C * P.Y + E;
  Result.Y:= B * P.X + D * P.Y + F;
end;


function TSvgMatrix.Multiply(CONST Right: TSvgMatrix): TSvgMatrix;
begin
  Result.A:= A * Right.A + C * Right.B;
  Result.B:= B * Right.A + D * Right.B;
  Result.C:= A * Right.C + C * Right.D;
  Result.D:= B * Right.C + D * Right.D;
  Result.E:= A * Right.E + C * Right.F + E;
  Result.F:= B * Right.E + D * Right.F + F;
end;



{-------------------------------------------------------------------------------------------------------------
   SCANNING
-------------------------------------------------------------------------------------------------------------}

function MakePoint(X, Y: Double): TSvgPoint;
begin
  Result.X:= X;
  Result.Y:= Y;
end;


function IsWsp(Ch: Char): Boolean;
begin
  Result:= (Ch = ' ') OR (Ch = #9) OR (Ch = #10) OR (Ch = #13);
end;


function IsDigit(Ch: Char): Boolean;
begin
  Result:= (Ch >= '0') AND (Ch <= '9');
end;


function IsLetter(Ch: Char): Boolean;
begin
  Result:= ((Ch >= 'a') AND (Ch <= 'z')) OR ((Ch >= 'A') AND (Ch <= 'Z'));
end;


function IsNameChar(Ch: Char): Boolean;
begin
  Result:= IsLetter(Ch) OR IsDigit(Ch) OR (Ch = '-') OR (Ch = '_') OR (Ch = ':') OR (Ch = '.') OR (Ord(Ch) > 127);
end;


procedure SkipWsp(CONST S: string; VAR P: Integer);
begin
  while (P <= Length(S)) AND IsWsp(S[P]) do
    Inc(P);
end;


{ Skips the SVG "comma-wsp" separator: white space with at most one comma in it }
procedure SkipCommaWsp(CONST S: string; VAR P: Integer);
begin
  SkipWsp(S, P);
  if (P <= Length(S)) AND (S[P] = ',') then
    begin
      Inc(P);
      SkipWsp(S, P);
    end;
end;


function AtNumber(CONST S: string; P: Integer): Boolean;
begin
  Result:= (P <= Length(S)) AND (IsDigit(S[P]) OR (S[P] = '+') OR (S[P] = '-') OR (S[P] = '.'));
end;


function Pow10(N: Integer): Double;
VAR i: Integer;
begin
  Result:= 1;
  for i:= 1 to N do
    Result:= Result * 10;
end;


{ Reads one number at S[P]: [+-] digits [. digits] [(e|E) [+-] digits], with at least one mantissa digit.
  A second dot or a sign ends the number, so "-5.5.5" reads as -5.5 and leaves ".5" for the next call.
  Returns FALSE and leaves P unchanged when S[P] does not start a number. }
function TryScanNumber(CONST S: string; VAR P: Integer; OUT Value: Double): Boolean;
VAR
  Len, Q, R: Integer;
  Negative, ExpNegative: Boolean;
  Mantissa: Double;
  Digits, Decimals, Exponent: Integer;
begin
  Value:= 0;
  Len:= Length(S);
  Q:= P;

  Negative:= FALSE;
  if (Q <= Len) AND ((S[Q] = '+') OR (S[Q] = '-')) then
    begin
      Negative:= S[Q] = '-';
      Inc(Q);
    end;

  { # Mantissa }
  Mantissa:= 0;
  Digits:= 0;
  Decimals:= 0;
  while (Q <= Len) AND IsDigit(S[Q]) do
    begin
      Mantissa:= Mantissa * 10 + (Ord(S[Q]) - Ord('0'));
      Inc(Digits);
      Inc(Q);
      if Digits > MaxDigits then raise SvgError('number too long at character ' + IntToStr(P));
    end;
  if (Q <= Len) AND (S[Q] = '.') then
    begin
      Inc(Q);
      while (Q <= Len) AND IsDigit(S[Q]) do
        begin
          Mantissa:= Mantissa * 10 + (Ord(S[Q]) - Ord('0'));
          Inc(Digits);
          Inc(Decimals);
          Inc(Q);
          if Digits > MaxDigits then raise SvgError('number too long at character ' + IntToStr(P));
        end;
    end;
  if Digits = 0 then EXIT(FALSE);

  { # Exponent. An "e" that is not followed by digits is not part of the number. }
  Exponent:= 0;
  if (Q <= Len) AND ((S[Q] = 'e') OR (S[Q] = 'E')) then
    begin
      R:= Q + 1;
      ExpNegative:= FALSE;
      if (R <= Len) AND ((S[R] = '+') OR (S[R] = '-')) then
        begin
          ExpNegative:= S[R] = '-';
          Inc(R);
        end;
      if (R <= Len) AND IsDigit(S[R]) then
        begin
          while (R <= Len) AND IsDigit(S[R]) do
            begin
              Exponent:= Exponent * 10 + (Ord(S[R]) - Ord('0'));
              if Exponent > 1000 then raise SvgError('number out of range at character ' + IntToStr(P));
              Inc(R);
            end;
          if ExpNegative then Exponent:= -Exponent;
          Q:= R;
        end;
    end;

  { # Value }
  Exponent:= Exponent - Decimals;
  if Exponent > 0 then
    begin
      if Exponent > 60 then raise SvgError('number out of range at character ' + IntToStr(P));
      Value:= Mantissa * Pow10(Exponent);
    end
  else
    if Exponent < -300
    then Value:= 0
    else Value:= Mantissa / Pow10(-Exponent);

  if Value > MaxCoordinate then raise SvgError('number out of range at character ' + IntToStr(P));
  if Negative then Value:= -Value;
  P:= Q;
  Result:= TRUE;
end;


function ScanNumber(CONST S: string; VAR P: Integer; CONST Context: string): Double;
begin
  if NOT TryScanNumber(S, P, Result)
  then raise SvgError('expected a number at character ' + IntToStr(P) + ' of ' + Context + ' "' + S + '"');
end;


{ Numbers separated by white space and/or commas: viewBox, points, transform arguments }
function ParseNumberList(CONST S, Context: string): TNumbers;
VAR
  P, Count: Integer;
  Value: Double;
begin
  Result:= NIL;
  Count:= 0;
  P:= 1;
  SkipWsp(S, P);
  while P <= Length(S) do
    begin
      Value:= ScanNumber(S, P, Context);
      if Count = Length(Result)
      then SetLength(Result, Count * 2 + 8);
      Result[Count]:= Value;
      Inc(Count);
      SkipCommaWsp(S, P);
    end;
  SetLength(Result, Count);
end;


{ A length attribute: a number, optionally followed by "px" }
function ParseLength(CONST Value, AttrName: string): Double;
VAR
  P, i: Integer;
  Rest: string;
begin
  P:= 1;
  SkipWsp(Value, P);
  Result:= ScanNumber(Value, P, AttrName);
  Rest:= Trim(Copy(Value, P, MaxInt));
  if (Rest = '') OR (Rest = 'px') then EXIT;

  for i:= Low(LengthUnits) to High(LengthUnits) do
    if Rest = LengthUnits[i]
    then raise SvgUnsupported('unit "' + Rest + '" in ' + AttrName + '="' + Value + '"');
  raise SvgError('bad length ' + AttrName + '="' + Value + '"');
end;



{-------------------------------------------------------------------------------------------------------------
   PAINT AND TRANSFORM ATTRIBUTES
-------------------------------------------------------------------------------------------------------------}

function HexDigit(Ch: Char): Integer;
begin
  if IsDigit(Ch)                 then EXIT(Ord(Ch) - Ord('0'));
  if (Ch >= 'a') AND (Ch <= 'f') then EXIT(Ord(Ch) - Ord('a') + 10);
  if (Ch >= 'A') AND (Ch <= 'F') then EXIT(Ord(Ch) - Ord('A') + 10);
  Result:= -1;
end;


{ #rgb or #rrggbb, opaque }
function ParseHexColor(CONST V, AttrName: string): TAlphaColor;
VAR i, Digit: Integer;
begin
  if (Length(V) = 5) OR (Length(V) = 9)
  then raise SvgUnsupported('colour with alpha ' + AttrName + '="' + V + '"');
  if (Length(V) <> 4) AND (Length(V) <> 7)
  then raise SvgError('bad colour ' + AttrName + '="' + V + '"');

  Result:= 0;
  for i:= 2 to Length(V) do
    begin
      Digit:= HexDigit(V[i]);
      if Digit < 0 then raise SvgError('bad colour ' + AttrName + '="' + V + '"');
      if Length(V) = 4
      then Result:= (Result shl 8) OR Cardinal(Digit * 17)     { #f00 = #ff0000 }
      else Result:= (Result shl 4) OR Cardinal(Digit);
    end;
  Result:= Result OR $FF000000;
end;


function ParsePaint(CONST Value, AttrName: string): TSvgPaint;
VAR V: string;
begin
  V:= Trim(Value);
  Result.Kind:= spkColor;
  Result.Color:= ColorBlack;
  if      SameText(V, 'none')         then Result.Kind := spkNone
  else if SameText(V, 'currentColor') then Result.Kind := spkCurrentColor
  else if SameText(V, 'black')        then Result.Color:= ColorBlack
  else if SameText(V, 'white')        then Result.Color:= ColorWhite
  else if (V <> '') AND (V[1] = '#')  then Result.Color:= ParseHexColor(V, AttrName)
  else if SameText(Copy(V, 1, 4), 'url(')
  then raise SvgUnsupported('gradient or pattern paint ' + AttrName + '="' + Value + '"')
  else raise SvgUnsupported('colour ' + AttrName + '="' + Value + '"');
end;


function DefaultPresentation: TPresentation;
begin
  Result.Fill.Kind    := spkColor;          { SVG default: fill black, no stroke }
  Result.Fill.Color   := ColorBlack;
  Result.Stroke.Kind  := spkNone;
  Result.Stroke.Color := ColorBlack;
  Result.StrokeWidth  := 1;
  Result.FillRule     := sfrNonZero;
end;


{ Applies a paint attribute to Pres. Raises for an attribute that changes the drawing but is not supported.
  Ignores every other attribute (id, class, xmlns ...). }
procedure ApplyCommonAttribute(CONST Name, Value: string; VAR Pres: TPresentation);
VAR i: Integer;
begin
  if Name = 'fill' then Pres.Fill:= ParsePaint(Value, Name)
  else
  if Name = 'stroke' then Pres.Stroke:= ParsePaint(Value, Name)
  else
  if Name = 'stroke-width' then
    begin
      Pres.StrokeWidth:= ParseLength(Value, Name);
      if Pres.StrokeWidth < 0 then raise SvgError('negative stroke-width="' + Value + '"');
    end
  else
  if Name = 'fill-rule' then
    begin
      if Trim(Value) = 'nonzero' then Pres.FillRule:= sfrNonZero
      else
      if Trim(Value) = 'evenodd' then Pres.FillRule:= sfrEvenOdd
      else raise SvgError('bad fill-rule="' + Value + '"');
    end
  else
    for i:= Low(UnsupportedAttributes) to High(UnsupportedAttributes) do
      if Name = UnsupportedAttributes[i]
      then raise SvgUnsupported('attribute ' + Name + '="' + Value + '"');
end;


{ One function of a transform list, with its numbers N }
function TransformStep(CONST FnName: string; CONST N: TNumbers; CONST Value: string): TSvgMatrix;
VAR
  Count: Integer;
  Angle, SinA, CosA: Double;
begin
  Result:= TSvgMatrix.Identity;
  Count:= Length(N);

  if FnName = 'matrix' then
    begin
      if Count <> 6 then raise SvgError('matrix needs 6 numbers in transform="' + Value + '"');
      Result.A:= N[0];
      Result.B:= N[1];
      Result.C:= N[2];
      Result.D:= N[3];
      Result.E:= N[4];
      Result.F:= N[5];
    end
  else
  if FnName = 'translate' then
    begin
      if (Count < 1) OR (Count > 2) then raise SvgError('translate needs 1 or 2 numbers in transform="' + Value + '"');
      Result.E:= N[0];
      if Count = 2 then Result.F:= N[1];
    end
  else
  if FnName = 'scale' then
    begin
      if (Count < 1) OR (Count > 2) then raise SvgError('scale needs 1 or 2 numbers in transform="' + Value + '"');
      Result.A:= N[0];
      if Count = 2
      then Result.D:= N[1]
      else Result.D:= N[0];
    end
  else
  if FnName = 'rotate' then
    begin
      if (Count <> 1) AND (Count <> 3) then raise SvgError('rotate needs 1 or 3 numbers in transform="' + Value + '"');
      Angle:= N[0] * Pi / 180;
      SinA:= Sin(Angle);
      CosA:= Cos(Angle);
      Result.A:= CosA;
      Result.B:= SinA;
      Result.C:= -SinA;
      Result.D:= CosA;
      if Count = 3 then
        begin
          { rotate(a cx cy) = translate(cx cy) * rotate(a) * translate(-cx -cy) }
          Result.E:= N[1] - CosA * N[1] + SinA * N[2];
          Result.F:= N[2] - SinA * N[1] - CosA * N[2];
        end;
    end
  else
  if (FnName = 'skewX') OR (FnName = 'skewY')
  then raise SvgUnsupported('transform ' + FnName + ' in transform="' + Value + '"')
  else raise SvgError('unknown transform function "' + FnName + '" in transform="' + Value + '"');
end;


{ A transform list such as "translate(10 0) scale(2)": the rightmost function is applied first }
function ParseTransform(CONST Value: string): TSvgMatrix;
VAR
  P, Start, CloseAt: Integer;
  FnName: string;
  Numbers: TNumbers;
begin
  Result:= TSvgMatrix.Identity;
  P:= 1;
  SkipWsp(Value, P);
  while P <= Length(Value) do
    begin
      Start:= P;
      while (P <= Length(Value)) AND IsLetter(Value[P]) do
        Inc(P);
      FnName:= Copy(Value, Start, P - Start);
      if FnName = '' then raise SvgError('bad transform="' + Value + '"');

      SkipWsp(Value, P);
      if (P > Length(Value)) OR (Value[P] <> '(')
      then raise SvgError('expected "(" after ' + FnName + ' in transform="' + Value + '"');
      CloseAt:= Pos(')', Value, P);
      if CloseAt = 0 then raise SvgError('missing ")" in transform="' + Value + '"');

      Numbers:= ParseNumberList(Copy(Value, P + 1, CloseAt - P - 1), 'transform ' + FnName);
      Result:= Result.Multiply(TransformStep(FnName, Numbers, Value));
      P:= CloseAt + 1;
      SkipCommaWsp(Value, P);
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   PATH DATA
-------------------------------------------------------------------------------------------------------------}

procedure TSegmentList.Add(Kind: TSvgSegmentKind; CONST Ctrl1, Ctrl2, Pt: TSvgPoint);
begin
  if Count = Length(Items)
  then SetLength(Items, Count * 2 + 8);
  Items[Count].Kind := Kind;
  Items[Count].Ctrl1:= Ctrl1;
  Items[Count].Ctrl2:= Ctrl2;
  Items[Count].Pt   := Pt;
  Inc(Count);
end;


function TSegmentList.ToArray: TSvgSegments;
begin
  Result:= Copy(Items, 0, Count);
end;


function ParseSvgPathData(CONST PathData: string): TSvgSegments;
CONST
  Context = 'path data';
VAR
  List: TSegmentList;
  P: Integer;
  Cmd, UpCmd: Char;
  Relative, FirstArgs, FigureOpen: Boolean;
  Cur, Start, Ctrl1, Ctrl2, EndPt, Zero: TSvgPoint;
  Value: Double;

  function ReadPoint: TSvgPoint;
  begin
    Result.X:= ScanNumber(PathData, P, Context);
    SkipCommaWsp(PathData, P);
    Result.Y:= ScanNumber(PathData, P, Context);
    if Relative then
      begin
        Result.X:= Result.X + Cur.X;
        Result.Y:= Result.Y + Cur.Y;
      end;
  end;

  { After Z, a drawing command without M continues from the start of the closed figure }
  procedure OpenFigure;
  begin
    if NOT FigureOpen then
      begin
        List.Add(sgMoveTo, Zero, Zero, Start);
        FigureOpen:= TRUE;
      end;
  end;

  procedure LineTo(CONST Pt: TSvgPoint);
  begin
    OpenFigure;
    List.Add(sgLineTo, Zero, Zero, Pt);
    Cur:= Pt;
  end;

begin
  if Trim(PathData) = '' then raise SvgError('empty path data');

  List.Items:= NIL;
  List.Count:= 0;
  Zero:= MakePoint(0, 0);
  Cur:= Zero;
  Start:= Zero;
  FigureOpen:= FALSE;

  P:= 1;
  SkipWsp(PathData, P);
  while P <= Length(PathData) do
    begin
      Cmd:= PathData[P];
      UpCmd:= UpCase(Cmd);
      case UpCmd of
        'M', 'L', 'H', 'V', 'C', 'Z': ;
        'A'     : raise SvgUnsupported('path command "' + Cmd + '" (elliptical arc)');
        'Q', 'T': raise SvgUnsupported('path command "' + Cmd + '" (quadratic Bezier curve)');
        'S'     : raise SvgUnsupported('path command "' + Cmd + '" (smooth cubic Bezier curve)');
      else
        raise SvgError('expected a path command at character ' + IntToStr(P) + ' of path data "' + PathData + '"');
      end;
      if (List.Count = 0) AND (UpCmd <> 'M') then raise SvgError('path data must start with M: "' + PathData + '"');
      Inc(P);
      Relative:= Cmd <> UpCmd;

      if UpCmd = 'Z' then
        begin
          List.Add(sgClose, Zero, Zero, Zero);
          Cur:= Start;
          FigureOpen:= FALSE;
          SkipWsp(PathData, P);
          CONTINUE;
        end;

      { A command letter may be followed by several argument groups: "L1 1 2 2" = "L1 1 L2 2". Extra groups after M are lines. }
      SkipWsp(PathData, P);
      FirstArgs:= TRUE;
      repeat
        case UpCmd of
          'M': begin
                 EndPt:= ReadPoint;
                 if FirstArgs then
                   begin
                     List.Add(sgMoveTo, Zero, Zero, EndPt);
                     Start:= EndPt;
                     Cur:= EndPt;
                     FigureOpen:= TRUE;
                   end
                 else
                   LineTo(EndPt);
               end;
          'L': LineTo(ReadPoint);
          'H': begin
                 Value:= ScanNumber(PathData, P, Context);
                 if Relative then Value:= Value + Cur.X;
                 LineTo(MakePoint(Value, Cur.Y));
               end;
          'V': begin
                 Value:= ScanNumber(PathData, P, Context);
                 if Relative then Value:= Value + Cur.Y;
                 LineTo(MakePoint(Cur.X, Value));
               end;
          'C': begin
                 { All three points of a relative "c" are relative to the point where the curve starts }
                 Ctrl1:= ReadPoint;
                 SkipCommaWsp(PathData, P);
                 Ctrl2:= ReadPoint;
                 SkipCommaWsp(PathData, P);
                 EndPt:= ReadPoint;
                 OpenFigure;
                 List.Add(sgCubicTo, Ctrl1, Ctrl2, EndPt);
                 Cur:= EndPt;
               end;
        end;
        FirstArgs:= FALSE;
        SkipCommaWsp(PathData, P);
      until NOT AtNumber(PathData, P);
    end;

  Result:= List.ToArray;
end;



{-------------------------------------------------------------------------------------------------------------
   XML
-------------------------------------------------------------------------------------------------------------}

function IsSkippedElement(CONST Name: string): Boolean;
VAR i: Integer;
begin
  if Pos(':', Name) > 0 then EXIT(TRUE);     { Editor data: inkscape:..., sodipodi:... }
  for i:= Low(SkippedElements) to High(SkippedElements) do
    if Name = SkippedElements[i] then EXIT(TRUE);
  Result:= FALSE;
end;


function IsShapeElement(CONST Name: string): Boolean;
begin
  Result:= (Name = 'path') OR (Name = 'rect') OR (Name = 'circle') OR (Name = 'ellipse') OR (Name = 'polygon');
end;


function TSvgTextReader.StartsHere(CONST Sub: string): Boolean;
begin
  Result:= Copy(S, P, Length(Sub)) = Sub;
end;


procedure TSvgTextReader.SkipPast(CONST Terminator, What: string);
VAR Found: Integer;
begin
  Found:= Pos(Terminator, S, P);
  if Found = 0 then raise SvgError('unterminated ' + What + ' at character ' + IntToStr(P));
  P:= Found + Length(Terminator);
end;


{ Skips a comment, a CDATA section, a processing instruction (<?xml ...?>) or a declaration (<!DOCTYPE ...>).
  Returns FALSE when S[P] starts none of them. }
function TSvgTextReader.SkipMarkup: Boolean;
begin
  Result:= TRUE;
  if StartsHere('<!--')      then SkipPast('-->', 'comment')
  else
  if StartsHere('<![CDATA[') then SkipPast(']]>', 'CDATA section')
  else
  if StartsHere('<?')        then SkipPast('?>',  'processing instruction')
  else
  if StartsHere('<!')        then SkipPast('>',   'declaration')
  else
    Result:= FALSE;
end;


function TSvgTextReader.ReadName: string;
VAR Start: Integer;
begin
  Start:= P;
  while (P <= Len) AND IsNameChar(S[P]) do
    Inc(P);
  if P = Start then raise SvgError('expected a name at character ' + IntToStr(P));
  Result:= Copy(S, Start, P - Start);
end;


{ Reads <Name attr="value" ...> or <Name ... />. S[P] is the "<". }
procedure TSvgTextReader.ReadStartTag(OUT Name: string; OUT Attributes: TAttributes; OUT SelfClosing: Boolean);
VAR
  Count, ValueStart: Integer;
  Quote: Char;
  AttrName: string;
begin
  Inc(P);
  Name:= ReadName;
  Attributes:= NIL;
  Count:= 0;
  SelfClosing:= FALSE;
  while TRUE do
    begin
      SkipWsp(S, P);
      if P > Len then raise SvgError('unterminated tag <' + Name + '>');
      if S[P] = '>' then
        begin
          Inc(P);
          BREAK;
        end;
      if StartsHere('/>') then
        begin
          Inc(P, 2);
          SelfClosing:= TRUE;
          BREAK;
        end;

      AttrName:= ReadName;
      SkipWsp(S, P);
      if (P > Len) OR (S[P] <> '=') then raise SvgError('expected "=" after ' + AttrName + ' in <' + Name + '>');
      Inc(P);
      SkipWsp(S, P);
      if (P > Len) OR ((S[P] <> '"') AND (S[P] <> '''')) then raise SvgError('expected a quoted value for ' + AttrName + ' in <' + Name + '>');
      Quote:= S[P];
      Inc(P);
      ValueStart:= P;
      while (P <= Len) AND (S[P] <> Quote) do
        Inc(P);
      if P > Len then raise SvgError('unterminated value of ' + AttrName + ' in <' + Name + '>');

      SetLength(Attributes, Count + 1);
      Attributes[Count].Name := AttrName;
      Attributes[Count].Value:= Copy(S, ValueStart, P - ValueStart);
      Inc(Count);
      Inc(P);   { The closing quote }
    end;
end;


{ Reads </Name>. S[P] is the "<". }
procedure TSvgTextReader.ReadEndTag(CONST Expected: string);
VAR Name: string;
begin
  Inc(P, 2);
  Name:= ReadName;
  SkipWsp(S, P);
  if (P > Len) OR (S[P] <> '>') then raise SvgError('unterminated end tag </' + Name + '>');
  Inc(P);
  if Name <> Expected then raise SvgError('</' + Name + '> does not close <' + Expected + '>');
end;


{ Reads the children of the element Parent, up to and including its end tag }
procedure TSvgTextReader.ReadContent(CONST Parent: string; Mode: TContentMode);
VAR
  Name: string;
  Attributes: TAttributes;
  SelfClosing: Boolean;
begin
  while TRUE do
    begin
      { Text between tags is not drawn }
      while (P <= Len) AND (S[P] <> '<') do
        Inc(P);
      if P > Len then raise SvgError('missing </' + Parent + '>');

      if SkipMarkup then CONTINUE;
      if StartsHere('</') then
        begin
          ReadEndTag(Parent);
          EXIT;
        end;

      ReadStartTag(Name, Attributes, SelfClosing);
      if (Mode = cmSkip) OR IsSkippedElement(Name) then
        begin
          if NOT SelfClosing then ReadContent(Name, cmSkip);
        end
      else
      if (Mode = cmSvg) AND IsShapeElement(Name) then
        begin
          ReadShape(Name, Attributes);
          if NOT SelfClosing then ReadContent(Name, cmShape);
        end
      else
        raise SvgUnsupported('element <' + Name + '>');
    end;
end;


procedure TSvgTextReader.ReadRoot(CONST Attributes: TAttributes; OUT ViewBox: TSvgViewBox);
VAR
  i: Integer;
  Name, Value: string;
  Numbers: TNumbers;
  HaveViewBox: Boolean;
  Width, Height: Double;
begin
  Root:= DefaultPresentation;
  HaveViewBox:= FALSE;
  Width := 0;
  Height:= 0;
  for i:= 0 to High(Attributes) do
    begin
      Name := Attributes[i].Name;
      Value:= Attributes[i].Value;
      if Name = 'viewBox' then
        begin
          Numbers:= ParseNumberList(Value, Name);
          if Length(Numbers) <> 4 then raise SvgError('viewBox needs 4 numbers: "' + Value + '"');
          if (Numbers[2] <= 0) OR (Numbers[3] <= 0) then raise SvgError('viewBox width and height must be positive: "' + Value + '"');
          ViewBox.MinX  := Numbers[0];
          ViewBox.MinY  := Numbers[1];
          ViewBox.Width := Numbers[2];
          ViewBox.Height:= Numbers[3];
          HaveViewBox:= TRUE;
        end
      else
      if Name = 'width'  then Width := ParseLength(Value, Name)
      else
      if Name = 'height' then Height:= ParseLength(Value, Name)
      else
      if Name = 'transform' then raise SvgUnsupported('transform on <svg>')
      else
      if Name = 'preserveAspectRatio' then
        begin
          { The default, "xMidYMid meet", is what a caller gets by fitting the viewBox into its box with the aspect ratio kept }
          if (Trim(Value) <> 'xMidYMid') AND (Trim(Value) <> 'xMidYMid meet')
          then raise SvgUnsupported('preserveAspectRatio="' + Value + '"');
        end
      else
        ApplyCommonAttribute(Name, Value, Root);
    end;

  if NOT HaveViewBox then
    begin
      if (Width <= 0) OR (Height <= 0) then raise SvgError('<svg> has no viewBox and no width and height');
      ViewBox.MinX  := 0;
      ViewBox.MinY  := 0;
      ViewBox.Width := Width;
      ViewBox.Height:= Height;
    end;
end;


procedure TSvgTextReader.AddShape(CONST Shape: TSvgShape);
begin
  if ShapeCount = Length(Shapes)
  then SetLength(Shapes, ShapeCount * 2 + 8);
  Shapes[ShapeCount]:= Shape;
  Inc(ShapeCount);
end;


function PolygonSegments(CONST N: TNumbers): TSvgSegments;
VAR
  i, Count: Integer;
  Zero: TSvgPoint;
begin
  Zero:= MakePoint(0, 0);
  Count:= Length(N) DIV 2;
  SetLength(Result, Count + 1);
  for i:= 0 to Count - 1 do
    begin
      if i = 0
      then Result[i].Kind:= sgMoveTo
      else Result[i].Kind:= sgLineTo;
      Result[i].Ctrl1:= Zero;
      Result[i].Ctrl2:= Zero;
      Result[i].Pt   := MakePoint(N[2*i], N[2*i + 1]);
    end;
  Result[Count].Kind := sgClose;
  Result[Count].Ctrl1:= Zero;
  Result[Count].Ctrl2:= Zero;
  Result[Count].Pt   := Zero;
end;


{ Builds the shape of one element. An element with a zero size is not drawn (SVG rule), so it adds no shape. }
procedure TSvgTextReader.ReadShape(CONST Name: string; CONST Attributes: TAttributes);
VAR
  Shape: TSvgShape;
  Pres: TPresentation;
  i: Integer;
  AttrName, Value, PathData, Points: string;
  X, Y, W, H, CX, CY, R, RX, RY: Double;
  HasRX, HasRY: Boolean;
  Numbers: TNumbers;
begin
  Pres:= Root;
  Shape.Transform:= TSvgMatrix.Identity;
  X := 0;  Y := 0;  W := 0;  H := 0;
  CX:= 0;  CY:= 0;  R := 0;  RX:= 0;  RY:= 0;
  HasRX:= FALSE;
  HasRY:= FALSE;
  PathData:= '';
  Points:= '';

  { # Attributes }
  for i:= 0 to High(Attributes) do
    begin
      AttrName:= Attributes[i].Name;
      Value   := Attributes[i].Value;
      if AttrName = 'transform'                              then Shape.Transform:= ParseTransform(Value)
      else
      if (Name = 'path')    AND (AttrName = 'd')             then PathData:= Value
      else
      if (Name = 'polygon') AND (AttrName = 'points')        then Points:= Value
      else
      if (Name = 'rect') AND (AttrName = 'x')                then X:= ParseLength(Value, AttrName)
      else
      if (Name = 'rect') AND (AttrName = 'y')                then Y:= ParseLength(Value, AttrName)
      else
      if (Name = 'rect') AND (AttrName = 'width')            then W:= ParseLength(Value, AttrName)
      else
      if (Name = 'rect') AND (AttrName = 'height')           then H:= ParseLength(Value, AttrName)
      else
      if ((Name = 'circle') OR (Name = 'ellipse')) AND (AttrName = 'cx') then CX:= ParseLength(Value, AttrName)
      else
      if ((Name = 'circle') OR (Name = 'ellipse')) AND (AttrName = 'cy') then CY:= ParseLength(Value, AttrName)
      else
      if (Name = 'circle') AND (AttrName = 'r')              then R:= ParseLength(Value, AttrName)
      else
      if ((Name = 'rect') OR (Name = 'ellipse')) AND (AttrName = 'rx') then
        begin
          RX:= ParseLength(Value, AttrName);
          HasRX:= TRUE;
        end
      else
      if ((Name = 'rect') OR (Name = 'ellipse')) AND (AttrName = 'ry') then
        begin
          RY:= ParseLength(Value, AttrName);
          HasRY:= TRUE;
        end
      else
        ApplyCommonAttribute(AttrName, Value, Pres);
    end;

  { # Common fields }
  Shape.Segments   := NIL;
  Shape.X          := 0;
  Shape.Y          := 0;
  Shape.Width      := 0;
  Shape.Height     := 0;
  Shape.CX         := 0;
  Shape.CY         := 0;
  Shape.RX         := 0;
  Shape.RY         := 0;
  Shape.Fill       := Pres.Fill;
  Shape.Stroke     := Pres.Stroke;
  Shape.StrokeWidth:= Pres.StrokeWidth;
  Shape.FillRule   := Pres.FillRule;

  { # Geometry }
  if Name = 'path' then
    begin
      if Trim(PathData) = '' then EXIT;
      Shape.Kind:= sskPath;
      Shape.Segments:= ParseSvgPathData(PathData);
    end
  else
  if Name = 'polygon' then
    begin
      Numbers:= ParseNumberList(Points, 'points');
      if Odd(Length(Numbers)) then raise SvgError('odd count of numbers in polygon points="' + Points + '"');
      if Length(Numbers) = 0 then EXIT;
      Shape.Kind:= sskPath;
      Shape.Segments:= PolygonSegments(Numbers);
    end
  else
  if Name = 'rect' then
    begin
      if (W < 0) OR (H < 0) OR (RX < 0) OR (RY < 0) then raise SvgError('negative size in <rect>');
      if (W = 0) OR (H = 0) then EXIT;
      { One radius given: the other takes the same value. Each is limited to half the side. }
      if HasRX AND NOT HasRY then RY:= RX
      else
      if HasRY AND NOT HasRX then RX:= RY;
      if RX > W / 2 then RX:= W / 2;
      if RY > H / 2 then RY:= H / 2;
      Shape.Kind  := sskRect;
      Shape.X     := X;
      Shape.Y     := Y;
      Shape.Width := W;
      Shape.Height:= H;
      Shape.RX    := RX;
      Shape.RY    := RY;
    end
  else
  if Name = 'circle' then
    begin
      if R < 0 then raise SvgError('negative r in <circle>');
      if R = 0 then EXIT;
      Shape.Kind:= sskEllipse;
      Shape.CX:= CX;
      Shape.CY:= CY;
      Shape.RX:= R;
      Shape.RY:= R;
    end
  else
    begin
      Assert(Name = 'ellipse', 'ReadShape: unexpected element ' + Name);
      if (RX < 0) OR (RY < 0) then raise SvgError('negative radius in <ellipse>');
      if (RX = 0) OR (RY = 0) then EXIT;
      Shape.Kind:= sskEllipse;
      Shape.CX:= CX;
      Shape.CY:= CY;
      Shape.RX:= RX;
      Shape.RY:= RY;
    end;

  AddShape(Shape);
end;


function TSvgTextReader.Parse(CONST SvgText: string): TSvgImage;
VAR
  Name: string;
  Attributes: TAttributes;
  SelfClosing, HaveRoot: Boolean;
begin
  if Trim(SvgText) = '' then raise SvgError('empty text');
  S:= SvgText;
  Len:= Length(S);
  P:= 1;
  if S[1] = #$FEFF then P:= 2;   { Byte order mark }
  Shapes:= NIL;
  ShapeCount:= 0;
  HaveRoot:= FALSE;

  while TRUE do
    begin
      SkipWsp(S, P);
      if P > Len then BREAK;
      if S[P] <> '<' then raise SvgError('text outside the <svg> element at character ' + IntToStr(P));
      if SkipMarkup then CONTINUE;
      if HaveRoot then raise SvgError('content after </svg> at character ' + IntToStr(P));
      if StartsHere('</') then raise SvgError('end tag before <svg> at character ' + IntToStr(P));

      ReadStartTag(Name, Attributes, SelfClosing);
      if Name <> 'svg' then raise SvgError('the root element is <' + Name + '>, not <svg>');
      ReadRoot(Attributes, Result.ViewBox);
      if NOT SelfClosing then ReadContent('svg', cmSvg);
      HaveRoot:= TRUE;
    end;

  if NOT HaveRoot then raise SvgError('no <svg> element');
  Result.Shapes:= Copy(Shapes, 0, ShapeCount);
end;



{-------------------------------------------------------------------------------------------------------------
   PUBLIC
-------------------------------------------------------------------------------------------------------------}

function ParseSvg(CONST SvgText: string): TSvgImage;
VAR Reader: TSvgTextReader;
begin
  Result:= Reader.Parse(SvgText);
end;


end.
