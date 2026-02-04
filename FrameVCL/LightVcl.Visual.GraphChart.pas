UNIT LightVcl.Visual.GraphChart;

{--------------------------------------------------------------------------------------------------
   2026.01.31

   How to use it:
     Create serie (or more) with CreateSerie
     Assign data points to each serie. The matrix must be indexed in 0 (the first data point should be at index 0) no matter if the axis was indexed in 0 or 1.
     Set LabelSpacing

   Tester: c:\Projects\Project Testers\cChartFastQ\Project3.dpr
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Types, System.SysUtils, System.Classes, System.UITypes, Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls,
  LightVcl.Common.Colors, LightVcl.Graph.Util, LightCore.Types;

TYPE
  TPlotType= set of (plDotValues,                                { Show a label next to each data point to display its value }
                     plDots,                                     { Show a dot (small rectangle) for each data point }
                     plHorizLine,                                { Instead of drawing a small square for each data point. I draw a vertical line as wide as a Wisker bar. Obviously it will be used in conjunction with Wiskers. Needed by 'Per base sequence quality' graph }
                     plConnLines,                                { Show lines connecting the dots }
                     plBar,                                      { This Series is of type 'Yellow Bar'. It should not be used in conjunction with 'plConnLines' but with plWisker }
                     plWisker);                                  { This Series is of type 'Wiskers'. It should not be used in conjunction with 'plConnLines' }

  { X/Y axis }
  TAxis= record
   private
    LengthDp      : Integer;                                     { The length (in datapoints, not in pixels) of this axis. It will be autocomputed if set to zero! }
    LengthPx      : Integer;                                     { The length in pixels of this axis. }
    ScaleFactor   : Double;                                      { Multiplier for the distance between two points on X/Y axis. This will be higher than zero if I have 10 points to draw on a 1000 pixels area, and smaller than zero if I have to draw 1000 points in a 500 px area }
    FTickEvery    : Real;                                        { Same as TickEvery but it holds the ACTUAL data  }
   public
    Offset        : Integer;                                     { Offset for x axis. Can be zero or one. }
    Labels        : array of string;
    ShowTickValues: Boolean;                                     { Show user assigned labels. If this is set to false then the tick values will be shown }
    Color         : TColor;                                      { Text color }
    PixSize       : Integer;                                     { This behaves as Height for XLabel and Width for YAxis }
    TickEvery     : Real;                                        { In datapoints. Put a tick every x datapoints. Use 0 for autocompute ticks }
    Name          : string;                                      { What is represented on this axis. For example 'Population (%)'  }
  end;

TYPE
  { Series }
  TDataPoints= System.Types.TIntegerDynArray;                    { This must be big enough to hold all samples (6 billion) but it must also hold negative numbers because the QV of Solexa starts at -5. So I have to choose between integer and int64. }
  TSerie= class(TObject)                                         { old name: TCurve }
   public
    Legend     : string;                                         { Legend text to be shown in Legend box (one legend of each Series). Legend box is drawn be DoDrawLegend }
    Color      : TColor;
    ClrFillWisk: TColor;                                         { Color used to fill Wisker rectangles }
    DataPoints : TDataPoints;                                    { Datapoints (Y values). Indexed in zero }
    DataPoints2: TDataPoints;                                    { Datapoints bottom. Has meaning only if Bar is True }
    PlotType   : TPlotType;
    procedure Clear;                                             { Clear the data but does not resize the array }
  end;


  TFastQChart = class(TImage)                                    { It cannot be a TPaintBox because DrawAntialisedLine is 1000x slower when used directly on canvas }
  private
    FZebra: Boolean;
    FVertGuides,
    FLegendInside,
    FDrawLegend: Boolean;
    FScaleDownY: Boolean;                                        { Automatically scale down the Y axis: the axis may be very high but the datapoints may never have high values (may be all gathered down, close to 0) }
    FPointSize: Integer;
    FLabelSpacing: Integer;                                      { Distance between label area and axis }
    FVGuideClr, FBkgColor, FZebraColor: TColor;
    FIgnoreZeros: Boolean;
    procedure DoDrawLegend;
    function HighestDpX: Double;                                 { Find highest value among all axis }
  protected
    Ground: Integer;
    GraphRect: TRect;                                            { Area effectively used to draw the graph Series }
    TextHeight: Integer;
    procedure Loaded; override;
    procedure Resize; override;
    procedure FakeLinearDataPoints(var Serie: TSerie);
    procedure FakeDataPoints  (var Serie: TSerie);
    procedure FakeDataPointsWsk (var Serie, Serie2: TSerie);     { Wiskers }
    procedure FakeDataPointsB (var Serie: TSerie);
  public
    ShowLabelRect: Boolean;                                      { show a nice rectangle around the labelbar }
    LegendRect: TRect;                                           { Rectangle where I draw the legend }
    XAxis: TAxis;                                                { Text to display on X axis under the table, under each datapoint }
    YAxis: TAxis;
    ShiftHalfTick: Boolean;                                      { Shift all data points with a half of a unit/tick so it won't start right on zero. Looks good with most graphs }
    Series: TObjectList<TSerie>;                                 { List of Series }
    LegendSpacing: Integer;                                      { Spacing inside the legend box (ex: space between text rows) }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    function  CreateSerie(const LegendText: string; Color: TColor; PlotType: TPlotType= [plConnLines]; TotalDataPoints: Integer= 0): TSerie;

    procedure PrintXLabels;
    procedure PrintYLabels;
    procedure DrawChart;
    property  OnResize;

    procedure FakeData;

    property  IgnoreZeros    : Boolean read FIgnoreZeros  write FIgnoreZeros        default FALSE;    { Makes sense only when we use plConnLines. It makes the chart to skip over datapoints that contain a 0. Useful when the user only fills some data points with valid numbers, but not all }
    property  DrawLegend     : Boolean read FDrawLegend   write FDrawLegend          default TRUE;     { Draw legend or not? }
    property  LegendInside   : Boolean read FLegendInside write FLegendInside        default FALSE;    { If true, the legend is placed inside the graph (top-left corner), over data. Else is placed under the graph (no overlap) }
    property  PointSize      : Integer read FPointSize    write FPointSize           default 2;        { The size of the rectangle associated with a point }
    property  LabelSpacing   : Integer read FLabelSpacing write FLabelSpacing        default 5;        { Distance between label area and axis }{ Pixels }
    property  BkgColor       : TColor  read FBkgColor     write FBkgColor            default clWhite;
    property  ScaleDownYAxis : Boolean read FScaleDownY   write FScaleDownY          default TRUE;     { Automatically scale down the Y axis: the axis may be very high but the datapoints may never have high values (may be all gathered down, close to 0) }
    property  ZebraColor     : TColor  read FZebraColor   write FZebraColor          default clSilverLight;
    property  Zebra          : Boolean read FZebra        write FZebra               default FALSE;    { Draw zebra background }

    property  VerticalGuides : Boolean read FVertGuides   write FVertGuides          default TRUE;
    property  VertGuideClr   : TColor  read FVGuideClr    write FVGuideClr           default TColor($EDEDED);
  end;

procedure Register;


function FindNonZeroPoint(DataPoints: TDataPoints): Cardinal;                                      { Stop on the first non-empty cell }
function FindMax         (DataPoints: TDataPoints): Integer;                                       { Stop on the first non-empty cell }

IMPLEMENTATION

USES LightCore.Math, LightCore;




constructor TFastQChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Series:= TObjectList<TSerie>.Create(TRUE);

  FIgnoreZeros     := FALSE;
  ShiftHalfTick    := TRUE;
  FLegendInside    := FALSE;
  FVertGuides      := TRUE;
  FBkgColor        := clWhite;
  FZebraColor      := clSilverLight;
  FVGuideClr       := TColor($EDEDED);
  FZebra           := FALSE;
  FLabelSpacing    := 5;                            { Distance between label area and axis }
  LegendSpacing    := 6;                            { Spacing inside the legend box (ex: space between text rows) }
  FPointSize       := 2;                            { The size of the rectangle associated with a point }
  ShowLabelRect    := FALSE;
  FScaleDownY      := TRUE;
  FDrawLegend      := TRUE;

  XAxis.Color      := clBlack;
  XAxis.PixSize    := 12;                           { This behaves as Height for XAxis }
  XAxis.LengthDp   := 100;
  XAxis.TickEvery  := 30;                           { Tick every x datapoints }
  XAxis.ShowTickValues := TRUE;
  XAxis.Offset     := 0;                            { Offset for x axis. If set to 0 then the axis will start at 0 }
  XAxis.Name       := '';

  YAxis.Color      := clBlack;
  YAxis.PixSize    := 25;                           { This behaves as Width for YAxis }
  YAxis.LengthDp   := 50;
  YAxis.TickEvery  := 20;                           { Tick every x datapoints }
  YAxis.ShowTickValues := TRUE;
end;


destructor TFastQChart.Destroy;
begin
  FreeAndNil(Series);
  inherited;
end;


procedure TFastQChart.Clear;                       { This will delete all existing series }
begin
  inherited;
  Series.Clear;
end;


procedure TFastQChart.Loaded;
begin
  inherited;
  if Series.Count > 0
  then DrawChart;
end;


function TFastQChart.CreateSerie(CONST LegendText: string; Color: TColor; PlotType: TPlotType= [plConnLines]; TotalDataPoints: Integer= 0): TSerie;
begin
  Result:= TSerie.Create;
  Result.Legend:= LegendText;
  Result.Color:= Color;
  Result.ClrFillWisk:= clYellowLight;
  Result.PlotType:= PlotType;
  SetLength(Result.DataPoints, TotalDataPoints);
  Series.Add(Result);
end;


function TFastQChart.HighestDpX: Double;  { Find longest x axis among all x axis }
VAR
   Serie: TSerie;
   CurSeries: Integer;
begin
  Result:= 0;
  for CurSeries:= 0 to Series.Count-1 DO
   begin
    Serie:= Series[CurSeries];
    if Length(Serie.DataPoints) > Result
    then Result:= Length(Serie.DataPoints);
   end;
end;


{ Note: dx measures datapoints. px measures pixels }
procedure TFastQChart.DrawChart;
CONST
   RowDistance = 3;
VAR
   dx, dY: Double;
   pX, X2, X3, Y, Y2, Y3, OldX, OldY: Integer;
   sLabel: string;
   MaxPoint: Integer;
   CurSeries, Cl, ZebraWidth: Integer;
   HalfTick: Double;
   YLabRect, XLabRect: TRect;
   Serie: TSerie;
   bFirstTime: Boolean;
begin
  Assert(XAxis.TickEvery > -1, 'Invalid XAxis.TickEvery');
  Assert(YAxis.TickEvery > -1, 'Invalid YAxis.TickEvery');

  { Set canvas size }
  Picture.Bitmap.Width  := ClientWidth;
  Picture.Bitmap.Height := ClientHeight;

  Canvas.Font.Size:= 7;
  TextHeight:= Canvas.TextHeight('Tp')+ RowDistance;

  { Clear all }
  Canvas.Brush.Color:= BkgColor;
  Canvas.FillRect(Picture.Bitmap.Canvas.ClipRect);

  { Graph area }
  if LegendInside OR NOT DrawLegend
  then
   begin
    Ground:= ClientHeight-XAxis.PixSize- LabelSpacing- 15;                                          { This is the 'logic 0' of the graph (the Y coordinate where I draw the X axis) } { -15 is the height of the text under the axis ('XAxis.Name') }
    GraphRect:= Rect(YAxis.PixSize- LabelSpacing, LabelSpacing, ClientWidth-LabelSpacing, Ground);  { The area where I can draw the graph. The rest of the canvas is reserved for labels and legend. }
   end
  else
   begin
    Ground:= ClientHeight-XAxis.PixSize- LabelSpacing -(LegendSpacing+ TextHeight+ LegendSpacing);  { This is the 'logic 0' of the graph (the Y coordinate where I draw the X axis) }
    GraphRect:= Rect(YAxis.PixSize- LabelSpacing, LabelSpacing, ClientWidth-LabelSpacing, Ground);  { The area where I can draw the graph. The rest of the canvas is reserved for labels and legend. }
   end;

  { Length in DP of X axis }
  if Series.Count > 0
  then XAxis.LengthDp:= RoundUp(HighestDpX + (0.03 * HighestDpX));              { Make axis 3% bigger than highest value }

  if ScaleDownYAxis then
   begin
    MaxPoint:= 0;
    for CurSeries:= 0 to Series.Count-1 DO
     begin
      Serie:= Series[CurSeries];
      for Cl:= 0 to High(Serie.DataPoints) DO
       begin
        if Serie.DataPoints[Cl]> MaxPoint
        then MaxPoint:= Serie.DataPoints[Cl];
       end;
     end;
    if MaxPoint> 0                                                                                  { Prevent 'Division by 0' }
    then YAxis.LengthDp:= RoundEx(MaxPoint+ 0.03*MaxPoint);                                         { Make axis 3% bigger than highest value }
   end;


  { Axis scale }
  XAxis.LengthPx:= GraphRect.Right - GraphRect.Left;
  YAxis.LengthPx:= GraphRect.Bottom- GraphRect.Top;

  XAxis.ScaleFactor:= XAxis.LengthPx / XAxis.LengthDp;
  YAxis.ScaleFactor:= YAxis.LengthPx / YAxis.LengthDp;

  { Shift all data points with a half of a unit/tick so it won't start right on zero. Looks good with most graphs }
  if ShiftHalfTick
  then HalfTick:= 0.5*xAxis.ScaleFactor
  else HalfTick:= 0;


  { Autocompute ticks }
  if XAxis.TickEvery = 0
  then
    begin
     XAxis.FTickEvery:= XAxis.LengthDp DIV 50;
     LightCore.Math.NotSmallerThan(XAxis.FTickEvery, 10);      { Too few ticks? }
    end
  else
    XAxis.FTickEvery:= XAxis.TickEvery;

  if YAxis.TickEvery = 0
  then
    begin
     YAxis.FTickEvery:= YAxis.LengthDp DIV 10;
     LightCore.Math.NotSmallerThan(YAxis.FTickEvery, 10);   { Too few ticks? }
    end
  else
    YAxis.FTickEvery:= YAxis.TickEvery;

  { Fix invalid values }
  if XAxis.FTickEvery= 0
  then XAxis.FTickEvery:= 1;

  if YAxis.FTickEvery= 0
  then YAxis.FTickEvery:= 1;

  { Draw X axis XLabels rectangle }
  if ShowLabelRect then
   begin
    XLabRect       := GraphRect;
    XLabRect.Top   := GraphRect.Bottom+ LabelSpacing;
    XLabRect.Bottom:= XLabRect.Top+ XAxis.PixSize;
    Canvas.Brush.Color:= BkgColor;
    Canvas.Pen.Color  := clSilver;
    Canvas.Rectangle(XLabRect);
   end;

  { Draw X axis }
  Canvas.Pen.Color:= clBlack;
  Canvas.MoveTo(GraphRect.Left, Ground);
  Canvas.LineTo(GraphRect.Right, Ground);



  { Zebra background - Note: Does not work well for large numbers }
  if Zebra then
   begin
    if XAxis.Offset= 0                                                                               { Start }
    then dx:= XAxis.FTickEvery
    else dx:= 0;

    cl:= 1;
    ZebraWidth:= Roundex(xAxis.ScaleFactor / 2)+1;
    if ZebraWidth= 0                                                                                { Protection! Happens when I have lots of data points (like 1000) }
    then ZebraWidth:= 1;

    WHILE ZebraWidth<= 20 DO
     begin
       cl:= cl+ 1;
       ZebraWidth:= ZebraWidth*cl;
     end;

     REPEAT
      pX := RoundEx(dx*xAxis.ScaleFactor)+ GraphRect.Left- (ZebraWidth div 2);
      X2:= pX+ ZebraWidth;

      Canvas.Brush.Color:= ZebraColor;
      Canvas.FillRect(Rect(pX, ground, x2, LabelSpacing*3));

      dx:= dx+ XAxis.FTickEvery*cl;
     UNTIL pX>= XAxis.LengthPx;
   end;


  { X axis - Ticks }
  dx:= 0;
  Canvas.Font.Color:= clBlack;                                                                      { rx measures datapoints. px measures pixels }
  REPEAT
   pX:= RoundEx(dx * xAxis.ScaleFactor + HalfTick)+ GraphRect.Left;

   { Draw ticks }
   Canvas.Pen.Color:= clBlack;
   Canvas.MoveTo(pX, Ground+3);
   Canvas.LineTo(pX, Ground);

   { Draw vertical guides }
   if VerticalGuides then
    begin
     Canvas.Pen.Color:= VertGuideClr;
     Canvas.Pen.Style:= psDot;
     Canvas.MoveTo(pX, Ground-5);
     Canvas.LineTo(pX, LabelSpacing*3);
     Canvas.Pen.Style:= psSolid;
    end;

   { Draw tick values }
   if xAxis.ShowTickValues
   then Canvas.TextOut(pX, Ground+ XAxis.PixSize DIV 2, IntToStr(RoundEx(dx+ XAxis.Offset)))
   else
    if RoundEx(dx) < High(XAxis.Labels) then
     begin
      sLabel:= XAxis.labels[RoundEx(dx)];
      Canvas.TextOut(pX - Canvas.TextWidth(sLabel) DIV 2, Ground+ XAxis.PixSize DIV 2, sLabel);
     end;

   dx:= dx+ XAxis.FTickEvery;
  UNTIL pX>= XAxis.LengthPx;


  { Y axis }
  Canvas.Pen.Color:= clBlack;
  Canvas.MoveTo(GraphRect.Left, Ground);
  Canvas.LineTo(GraphRect.Left, LabelSpacing);

  { YLabels rectangle }
  YLabRect          := GraphRect;
  YLabRect.Left     := 1;
  YLabRect.Right    := GraphRect.Left- LabelSpacing;

  { Y ticks }
  Canvas.Font.Color:= clBlack;

  dY:= YAxis.LengthDp;
  REPEAT
   y:= RoundEx(dY*YAxis.ScaleFactor)+ LabelSpacing;

   { Ticks }
   Canvas.MoveTo(GraphRect.Left-3, Y);
   Canvas.LineTo(GraphRect.Left+1, Y);

   { Tick values }
   if YAxis.ShowTickValues
   then Canvas.TextOut(5, Y-6, Real2Str(YAxis.LengthDp-dY));

   dY:= dY- YAxis.FTickEvery;
  UNTIL dY<= 0;

  { YLabels text }
  if NOT YAxis.ShowTickValues then
   for Cl:= 0 to High(YAxis.Labels) DO
    if YAxis.Labels[cl] > '' then
     begin
      Y:= RoundEx(Ground - (cl* YAxis.ScaleFactor));
      Canvas.TextOut(5, Y-6, YAxis.Labels[cl]);
     end;


  { Draw legend }
  if (Series.Count> 0) AND DrawLegend
  then DoDrawLegend;


  { Draw axis name }
  if XAxis.Name> ''then
   begin
    Canvas.Font.Color:= clGray;
    Canvas.TextOut(ClientWidth - Canvas.TextWidth(XAxis.Name)- 4, Ground+ 18, XAxis.Name);
   end;
  if YAxis.Name> '' then
   begin
    Canvas.Font.Color:= clGray;
    Canvas.TextOut(1, 1, YAxis.Name);
   end;


  { DRAW SERIES }
  for CurSeries:= 0 to Series.Count-1 DO
   begin
    bFirstTime:= TRUE;  // don't draw the first line (the one that starts from 0,0)
    Serie:= Series[CurSeries];
    Canvas.Brush.Color:= Serie.ClrFillWisk;
    Canvas.Pen.Color:= Serie.Color;

    { Checks }
    Assert(Length(Serie.DataPoints) > 0, 'Empty data point for Serie '+ IntToStr(CurSeries+1));

    if (plBar in Serie.PlotType) then
     begin
      if Length(Serie.DataPoints2) = 0
      then raise Exception.Create('The graph is set to "plBar" but the second data point matrix is empty!');

      if ShiftHalfTick= FALSE
      then raise Exception.Create('The graph is set to "plBar" but ShiftHalfTick is set to false!');
     end;

    { Draw each data point }
    dx:= GraphRect.Left + HalfTick;                                                                 { This is in order to have all data shifted with a half of a measure so I can draw bars without going out of grid over the Y axis}
    Y := Ground-RoundEx(Serie.DataPoints[0]* YAxis.ScaleFactor);                                    { Init 'connect dots' }
    OldX:= RoundEx(dx);
    OldY:= Ground-RoundEx(Serie.DataPoints[0]* YAxis.ScaleFactor);
    Canvas.MoveTo(GraphRect.Left, Y);                                                               { Init 'connect dots' }

    for Cl:= 0 to High(Serie.DataPoints) DO
     begin
      pX:= RoundEx(dx);
      Y:= Ground-RoundEx(Serie.DataPoints[Cl]* YAxis.ScaleFactor);

      { Wiskers }
      if plWisker in Serie.PlotType  then
       begin
        Y2:= Y;
        Y3:= Ground-RoundEx(Serie.DataPoints2[Cl]* YAxis.ScaleFactor);

        { Vertical foot }
        Canvas.MoveTo(pX, Y2);
        Canvas.LineTo(pX, Y3);

        x2:= RoundEx(dx- HalfTick);
        x3:= RoundEx(dx+ HalfTick);

        { Top wisker }
        Canvas.MoveTo(x2, Y2);
        Canvas.LineTo(x3, Y2);

        { Btm wisker }
        Canvas.MoveTo(x2, Y3);
        Canvas.LineTo(x3, Y3);
       end;

      { Bars }
      if plBar in Serie.PlotType
      then
        { Wide enough? Draw as bar }
        if xAxis.ScaleFactor> 1.5
        then
         begin
          x2:= RoundEx(dx- HalfTick);
          x3:= RoundEx(dx+ HalfTick);

          Y2:= Ground-RoundEx(Serie.DataPoints2[Cl]* YAxis.ScaleFactor);
          if y= y2 then y2:= y2-1;                                                                  { Make sure that I represent even very very small data (everything that will not be drawn because it is under 1 pixel }

          Canvas.FillRect(Rect (x2, y, x3, y2));
          if xAxis.ScaleFactor> 9
          then Canvas.Rectangle(x2, y, x3, y2);                                                     { Draw rectangle highlight ONLY if the rectangle is big enough }
         end
        else
         { Not wide enough? Draw as vertical line }
         begin
          Y2:= Ground-RoundEx(Serie.DataPoints2[Cl]* YAxis.ScaleFactor);
          Canvas.MoveTo(pX, Y);
          Canvas.LineTo(pX, Y2);
         end;

      { Draw simple line between datapoints }
      if plConnLines in Serie.PlotType then
       begin
        if (NOT IgnoreZeros) OR (IgnoreZeros AND (Serie.DataPoints[Cl] > 0)) then
         begin
          if NOT bFirstTime  // don't draw the first line (the one that starts from 0,0)
          then AntialisedLine(Canvas, OldX, OldY, pX, y, Canvas.Pen.Color);
          OldX:= pX;   //x 88 , y = 213
          OldY:= Y;
          bFirstTime:= FALSE;
         end;
       end;

      { Draw datapoint as dot }
      if  (plDots in Serie.PlotType)
      AND (Serie.DataPoints[Cl] > 0) then                                                           { Don't draw this data point if it is zero! }
       begin
        Canvas.Brush.Color:= clYellowLight;
        pX:= RoundEx(dx);
        Canvas.Rectangle(pX-PointSize, Y-PointSize, pX+PointSize, Y+PointSize);
       end;

      { Draw datapoint as HorizontalLine }
      if plHorizLine in Serie.PlotType then
       begin
        if xAxis.ScaleFactor> 1.5
        then x2:= RoundEx(dx- HalfTick)
        else x2:= pX;
        x3:= RoundEx(x2+xAxis.ScaleFactor);

        Canvas.MoveTo(x2, Y);
        Canvas.LineTo(x3, Y);
       end;

      { Show datapoint values }
      if (plDotValues in Serie.PlotType)
      AND (Serie.DataPoints[Cl] > 0) then
       begin
        Canvas.Font.Size:= 5;
        Canvas.TextOut(pX+PointSize+2, Y-PointSize-9, IntToStr(Serie.DataPoints[Cl]));
       end;

      dx:= dx+ xAxis.ScaleFactor;
     end;
   end;
end;




{ Legend box }
procedure TFastQChart.DoDrawLegend;
VAR
   i, W, TextWidth: Integer;
   LegendText: string;
   Serie: TSerie;
begin
  { Legend text size }
  TextWidth:= 0;

  if LegendInside
  then
   for i:= 0 to Series.Count-1 DO                                                                   { Find the longest string (in pixels, not in chars) }
    begin
     Serie:= Series[i];
     W:= Canvas.TextWidth(Serie.Legend)+ 3;
     if W > TextWidth
     then TextWidth:= W;
    end
  else
    begin
     for i:= 0 to Series.Count-1
      DO LegendText:= LegendText+ Series[i].Legend+ '  ';
     TextWidth:= Canvas.TextWidth(LegendText);
    end;

  { Legend rectangle }
  Canvas.Pen.Color := clBlue;
  Canvas.Pen.Style := psDot;
  Canvas.Font.Size:= 8;
  { Legend text }
  Canvas.Brush.Color:= BkgColor;

  if LegendInside
  then
   begin
    LegendRect.Left  := GraphRect.Left + 20;
    LegendRect.Top   := 20;
    LegendRect.Bottom:= LegendSpacing+ LegendRect.Top + (TextHeight* Series.Count)+ LegendSpacing;
    LegendRect.Right := LegendSpacing+ LegendRect.Left+ TextWidth+                + LegendSpacing;  { Legend width }

    { Box }
    Canvas.Rectangle(LegendRect);

    { Legend text }
    for i:= 0 to Series.Count-1 DO
     begin
      Canvas.Font.Color:= Series[i].Color;
      Canvas.TextOut(LegendRect.Left+LegendSpacing, LegendRect.Top+ LegendSpacing+ (i* TextHeight), Series[i].Legend);
     end;
   end
  else
   begin
    W:= LegendSpacing+ TextWidth+ LegendSpacing+ LegendSpacing;
    LegendRect.Left:= (XAxis.LengthPx- W) DIV 2;
    LegendRect.Right := LegendRect.Left+ W;                                                         { Legend width }
    LegendRect.Top   := ClientHeight  - TextHeight- LegendSpacing;
    LegendRect.Bottom:= LegendRect.Top+ TextHeight+ LegendSpacing;

    { Box }
    Canvas.Rectangle(LegendRect);

    { Legend text }
    W:= LegendRect.Left+LegendSpacing;
    for i:= 0 to Series.Count-1 DO
     begin
      Canvas.Font.Color:= Series[i].Color;
      Canvas.TextOut(W, LegendRect.Top+ 3, Series[i].Legend+ '  ');

      TextWidth:= Canvas.TextWidth(Series[i].Legend+ '  ');
      W:= W+ TextWidth;
     end;
   end;

  Canvas.Pen.Style := psSolid;                                                                      { Restore to normal line }
end;







procedure TFastQChart.Resize;
begin
  inherited Resize;

  if (Series= NIL)
  OR (csCreating in ControlState)
  OR (Series.Count= 0)                                                                             { KEEP THIS ORDER }
  OR (Length(Series[0].DataPoints)= 0)
  then EXIT;

  DrawChart;
end;





{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}
function FindNonZeroPoint(DataPoints: TDataPoints): Cardinal;                                      { Stop on the first non-empty cell }
VAR
   i: Cardinal;
begin
  Result:= 0;
  for i:= High(DataPoints) downto 0 DO
    if DataPoints[i]<> 0
    then EXIT(i);
end;


function FindMax(DataPoints: TDataPoints): Integer;                                                { Stop on the first non-empty cell }
VAR i: Cardinal;
begin
  Result:= 0;
  for i:= 0 to High(DataPoints) DO
    if DataPoints[i] > Result
    then Result:= DataPoints[i];
end;









procedure TFastQChart.PrintXLabels;
VAR i: Integer;
begin
  SetLength(XAxis.Labels, XAxis.LengthDp);
  for i:= 0 to XAxis.LengthDp-1 DO
   XAxis.Labels[i]:= IntToStr(i);
end;


procedure TFastQChart.PrintYLabels;
VAR i: Integer;
begin
  SetLength(YAxis.Labels, YAxis.LengthDp);
  for i:= 0 to YAxis.LengthDp-1 DO
   if i mod 10 = 0
   then YAxis.Labels[i]:= 'Y:'+IntToStr(i)
   else YAxis.Labels[i]:= '';
end;




{-------------------------------------------------------------------------------------------------------------
   TESTER
-------------------------------------------------------------------------------------------------------------}
procedure TFastQChart.FakeData;                                                                    { Fill the Series with some random data, for testing }
VAR
   SeriesH, SeriesW, SeriesBar: TSerie;
begin
  Clear;
  LegendInside    := FALSE;

  Zebra           := FALSE;
  VerticalGuides  := TRUE;
  XAxis.Offset    := 1;
  XAxis.TickEvery := 0;
  YAxis.TickEvery := 0;
  YAxis.PixSize   := 30;

  { Label setup }
  PrintXLabels;
  PrintYLabels;
  XAxis.ShowTickValues:= TRUE;
  YAxis.ShowTickValues:= TRUE;

  SeriesW  := CreateSerie('Wiskers', clBlack,   [plWisker]);                                             { Wisker Series should ALWAYS be before the bar Series so the bars will cover the vertical stick of the wisker }
  SeriesBar:= CreateSerie('Bars'   , clMustard, [plBar]);
  SeriesBar.ClrFillWisk:= clYellow;
  FakeDataPointsB(SeriesBar);
  FakeDataPointsWsk(SeriesW, SeriesBar);

  SeriesH:= CreateSerie('Before', clGreen, [plDots]);
  FakeLinearDataPoints (SeriesH);
end;


CONST
   TotalDataPoints= 40;
   Randomness= 43;

procedure TFastQChart.FakeDataPoints(VAR Serie: TSerie);
VAR
   i: Integer;
begin
  SetLength(Serie.DataPoints, TotalDataPoints);

  for i:= 0 to High(Serie.DataPoints) DIV 2 DO
   Serie.DataPoints[i]:= (i*6)+ Random(Randomness);

  for i:= High(Serie.DataPoints) DIV 2 to High(Serie.DataPoints) DO
   Serie.DataPoints[i]:= (High(Serie.DataPoints)*6)- (i*6)+ Random(Randomness);
end;



procedure TFastQChart.FakeLinearDataPoints(VAR Serie: TSerie);
VAR
   i: Integer;
begin
  SetLength(Serie.DataPoints, TotalDataPoints);
  for i:= 0 to High(Serie.DataPoints)
   DO Serie.DataPoints[i]:= (i+IndexDiff);
end;



procedure TFastQChart.FakeDataPointsB(VAR Serie: TSerie);                                          { Bars }
VAR
   i: Integer;
begin
  SetLength(Serie.DataPoints , TotalDataPoints);
  SetLength(Serie.DataPoints2, TotalDataPoints);

  for i:= 0 to High(Serie.DataPoints) DIV 2 DO
   begin
    Serie.DataPoints [i]:= (i*6)+ Random(Randomness);
    Serie.DataPoints2[i]:= (i*6)+ Random(4);
   end;

  for i:= High(Serie.DataPoints) DIV 2 to High(Serie.DataPoints) DO
   begin
    Serie.DataPoints [i]:= (High(Serie.DataPoints)*6)- (i*6)+ Random(Randomness);
    Serie.DataPoints2[i]:= (High(Serie.DataPoints)*6)- (i*6)+ Random(4);
   end;
end;



procedure TFastQChart.FakeDataPointsWsk(VAR Serie, Serie2: TSerie);                                  { Wiskers. Copy data from Serie2 into Serie and increase it a bit  }
VAR
   i: Integer;
   r: Integer;
   i64: Int64;
begin
  SetLength(Serie.DataPoints , TotalDataPoints);
  SetLength(Serie.DataPoints2, TotalDataPoints);

  for i:= High(Serie.DataPoints) DIV 2 to High(Serie.DataPoints) DO
   begin
    r:= Random(7);
    Serie.DataPoints[i]:= Serie2.DataPoints[i]+ 2+ r;

    r:= Random(7);
    i64:= Serie2.DataPoints2[i];
    if (i64- r) >= 0
    then Serie.DataPoints2[i]:= Serie2.DataPoints2[i]- r;
   end;
end;





{ TSerie }
procedure TSerie.Clear;
begin
  FillZeros(DataPoints);
  FillZeros(DataPoints2);
end;






procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TFastQChart]);
end;



end.
