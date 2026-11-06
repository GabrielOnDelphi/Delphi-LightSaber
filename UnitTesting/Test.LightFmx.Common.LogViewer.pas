unit Test.LightFmx.Common.LogViewer;

{=============================================================================================================
   2026.11.06
   Unit tests for LightFmx.Common.LogViewer.pas
   Tests FMX log viewer component functionality, filtering, and clipboard operations

   Multi-line tests: the grid has no public way to read the text of a row, so they call the grid's own
   OnDrawColumnCell handler (the one FMX calls while painting) with TTextRecorderCanvas, a canvas that records
   every FillText instead of drawing it.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Forms,
  FMX.Types,
  LightCore.LogTypes,
  LightCore.LogRam;

type
  [TestFixture]
  TTestFmxLogViewer = class
  private
    FTestForm: TForm;
    procedure checkOneEntry(CONST Msg, ExpectedRowText: string);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_InitializesRamLog;

    [Test]
    procedure TestCreate_DefaultProperties;

    [Test]
    procedure TestCreate_OwnsRamLog;

    { AssignExternalRamLog Tests }
    [Test]
    procedure TestAssignExternalRamLog_NilParameter_ShouldRaise;

    [Test]
    procedure TestAssignExternalRamLog_ValidLog_Assigns;

    [Test]
    procedure TestAssignExternalRamLog_DoesNotOwnExternal;

    { ConstructInternalRamLog Tests }
    [Test]
    procedure TestConstructInternalRamLog_CreatesNew;

    { Verbosity2Color Tests }
    [Test]
    procedure TestVerbosity2Color_Debug;

    [Test]
    procedure TestVerbosity2Color_Errors;

    [Test]
    procedure TestVerbosity2Color_AllLevels;

    { Count Tests }
    [Test]
    procedure TestCount_EmptyLog;

    [Test]
    procedure TestCount_WithEntries;

    { Verbosity Filter Tests }
    [Test]
    procedure TestVerbosityFilter_DefaultValue;

    [Test]
    procedure TestVerbosityFilter_SetValue;

    { ShowTime/ShowDate Tests }
    [Test]
    procedure TestShowTime_DefaultFalse;

    [Test]
    procedure TestShowDate_DefaultFalse;

    [Test]
    procedure TestShowTime_SetTrue;

    [Test]
    procedure TestShowDate_SetTrue;

    { AutoScroll Tests }
    [Test]
    procedure TestAutoScroll_DefaultTrue;

    [Test]
    procedure TestAutoScroll_SetFalse;

    { Clear Tests }
    [Test]
    procedure TestClear_RemovesAllEntries;

    { Multi-line entries: one entry = one grid row, each line break drawn as one space }
    [Test]
    procedure TestMultiLine_CRLF_OneRowFlattened;

    [Test]
    procedure TestMultiLine_LoneLF_OneRowFlattened;

    [Test]
    procedure TestMultiLine_LoneCR_OneRowFlattened;

    [Test]
    procedure TestMultiLine_TrailingBreak_OneRowNoExtraRow;

    [Test]
    procedure TestMultiLine_BetweenSingleLines_RowsMatchEntries;

    [Test]
    procedure TestMultiLine_TwoMultiLineEntries_TwoRows;

    [Test]
    procedure TestMultiLine_ShowTime_MessageColumnFlattened;
  end;

implementation

uses
  System.Types,
  System.Rtti,
  FMX.Graphics,
  FMX.Grid,
  LightFmx.Common.LogViewer;


{-------------------------------------------------------------------------------------------------------------
   TTextRecorderCanvas - a canvas that records the text of every FillText call and draws nothing.
   Every abstract method of TCanvas gets an empty body, so constructing it gives no W1020 warning.
-------------------------------------------------------------------------------------------------------------}
type
  TTextRecorderCanvas = class(TCanvas)
  protected
    class function  DoInitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle; override;
    class procedure DoFinalizeBitmap(var Bitmap: THandle); override;
    class function  DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean); override;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
  public
    Texts: TStringList;
    constructor Create;
    destructor Destroy; override;
    function  PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    procedure FillText(const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
                       const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center); override;
  end;


constructor TTextRecorderCanvas.Create;
begin
  inherited CreateFromPrinter(NIL);   { No window, no bitmap: Initialize creates Fill, Stroke and Font, at scale 1 }
  Texts:= TStringList.Create;
end;

destructor TTextRecorderCanvas.Destroy;
begin
  FreeAndNil(Texts);
  inherited;
end;

procedure TTextRecorderCanvas.FillText(const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
                                       const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign);
begin
  Texts.Add(AText);
end;

function TTextRecorderCanvas.PtInPath(const APoint: TPointF; const APath: TPathData): Boolean;
begin
  Result:= FALSE;
end;

class function TTextRecorderCanvas.DoInitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle;
begin
  Result:= 0;
end;

class procedure TTextRecorderCanvas.DoFinalizeBitmap(var Bitmap: THandle);
begin
  Bitmap:= 0;
end;

class function TTextRecorderCanvas.DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  Result:= FALSE;
end;

class procedure TTextRecorderCanvas.DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
begin
end;

procedure TTextRecorderCanvas.DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
end;

procedure TTextRecorderCanvas.DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
begin
end;

procedure TTextRecorderCanvas.DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
end;

procedure TTextRecorderCanvas.DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean);
begin
end;

procedure TTextRecorderCanvas.DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
end;

procedure TTextRecorderCanvas.DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
end;

procedure TTextRecorderCanvas.DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
end;

procedure TTextRecorderCanvas.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
end;


{ Draws one cell through the grid's own OnDrawColumnCell handler and returns every text it drew }
function drawCell(Viewer: TLogViewer; ColIndex, Row: Integer): TArray<string>;
VAR
  Canvas: TTextRecorderCanvas;
  Handler: TDrawColumnCellEvent;
begin
  Handler:= Viewer.OnDrawColumnCell;
  Assert.IsTrue(Assigned(Handler), 'The viewer must have an OnDrawColumnCell handler');
  Assert.IsTrue(ColIndex < Viewer.ColumnCount, 'Column ' + IntToStr(ColIndex) + ' does not exist; ColumnCount = ' + IntToStr(Viewer.ColumnCount));

  Canvas:= TTextRecorderCanvas.Create;
  try
    Handler(Viewer, Canvas, Viewer.Columns[ColIndex], RectF(0, 0, 400, 22), Row, TValue.Empty, []);
    Result:= Canvas.Texts.ToStringArray;
  finally
    FreeAndNil(Canvas);
  end;
end;


{ Asserts that the cell draws exactly one text, equal to Expected }
procedure assertCellText(Viewer: TLogViewer; ColIndex, Row: Integer; CONST Expected, What: string);
VAR Texts: TArray<string>;
begin
  Texts:= drawCell(Viewer, ColIndex, Row);
  Assert.AreEqual(1, Length(Texts), What + ': the cell must draw exactly one text');
  Assert.AreEqual(Expected, Texts[0], What + ': drawn text');
end;


procedure TTestFmxLogViewer.Setup;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.ClientWidth:= 800;
  FTestForm.ClientHeight:= 600;
end;


procedure TTestFmxLogViewer.TearDown;
begin
  FreeAndNil(FTestForm);
end;


{ Constructor Tests }

procedure TTestFmxLogViewer.TestCreate_InitializesRamLog;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsNotNull(LogViewer.RamLog, 'RamLog should be initialized');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestCreate_DefaultProperties;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowTime, 'ShowTime should default to False');
    Assert.IsFalse(LogViewer.ShowDate, 'ShowDate should default to False');
    Assert.IsTrue(LogViewer.AutoScroll, 'AutoScroll should default to True');
    Assert.AreEqual(lvVerbose, LogViewer.Verbosity, 'Verbosity should default to lvVerbose');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestCreate_OwnsRamLog;
var
  LogViewer: TLogViewer;
  RamLogPtr: Pointer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    RamLogPtr:= LogViewer.RamLog;
    Assert.IsNotNull(RamLogPtr, 'RamLog should exist');
    { The destructor will free the owned RamLog - if it crashes, ownership is wrong }
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ AssignExternalRamLog Tests }

procedure TTestFmxLogViewer.TestAssignExternalRamLog_NilParameter_ShouldRaise;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.WillRaise(
      procedure
      begin
        LogViewer.AssignExternalRamLog(NIL);
      end,
      EAssertionFailed);
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestAssignExternalRamLog_ValidLog_Assigns;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;
    ExternalLog.AddInfo('Test message');

    LogViewer.AssignExternalRamLog(ExternalLog);

    Assert.AreSame(ExternalLog, LogViewer.RamLog, 'RamLog should be assigned to external log');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


procedure TTestFmxLogViewer.TestAssignExternalRamLog_DoesNotOwnExternal;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.AssignExternalRamLog(ExternalLog);

    { Free the LogViewer - it should NOT free the external log }
    FreeAndNil(LogViewer);

    { If we can still access ExternalLog, ownership was correctly NOT transferred }
    Assert.IsNotNull(ExternalLog, 'External log should still exist after LogViewer destruction');
    ExternalLog.AddInfo('Still accessible');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


{ ConstructInternalRamLog Tests }

procedure TTestFmxLogViewer.TestConstructInternalRamLog_CreatesNew;
var
  LogViewer: TLogViewer;
  ExternalLog: TRamLog;
  OriginalLog: TRamLog;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  ExternalLog:= TRamLog.Create(FALSE, NIL);
  try
    LogViewer.Parent:= FTestForm;

    { First assign external log }
    LogViewer.AssignExternalRamLog(ExternalLog);
    OriginalLog:= LogViewer.RamLog;
    Assert.AreSame(ExternalLog, OriginalLog, 'Should have external log');

    { Now construct internal log }
    LogViewer.ConstructInternalRamLog;

    Assert.IsNotNull(LogViewer.RamLog, 'Should have new RamLog');
    Assert.AreNotSame(ExternalLog, LogViewer.RamLog, 'Should be different from external log');
  finally
    FreeAndNil(LogViewer);
    FreeAndNil(ExternalLog);
  end;
end;


{ Verbosity2Color Tests }

procedure TTestFmxLogViewer.TestVerbosity2Color_Debug;
begin
  { Light theme: Debug is medium gray $FF909090. Dark theme returns the same. }
  Assert.AreEqual(TAlphaColor($FF909090), Verbosity2Color(lvDebug, FALSE), 'Light: Debug should be $FF909090');
  Assert.AreEqual(TAlphaColor($FF909090), Verbosity2Color(lvDebug, TRUE),  'Dark: Debug should be $FF909090');
end;


procedure TTestFmxLogViewer.TestVerbosity2Color_Errors;
begin
  Assert.AreEqual(TAlphaColors.Red, Verbosity2Color(lvErrors, FALSE), 'Light: Errors should be red');
  Assert.AreEqual(TAlphaColors.Red, Verbosity2Color(lvErrors, TRUE),  'Dark: Errors should be red');
end;


procedure TTestFmxLogViewer.TestVerbosity2Color_AllLevels;
var
  Level: TLogVerbLvl;
begin
  { Every verbosity level must return a fully-opaque color (alpha byte = $FF) for both themes.
    Verbosity2Color uses array lookup — exhaustive at compile time, so any future enum
    value forces the arrays to be extended. The runtime check here verifies the alpha
    channel is set (a transparent color would render invisible text). }
  for Level:= Low(TLogVerbLvl) to High(TLogVerbLvl) do
    begin
      Assert.IsTrue(Verbosity2Color(Level, FALSE) shr 24 = $FF,
        Verbosity2String(Level) + ' (light) must be fully opaque');
      Assert.IsTrue(Verbosity2Color(Level, TRUE) shr 24 = $FF,
        Verbosity2String(Level) + ' (dark) must be fully opaque');
    end;
end;


{ Count Tests }

procedure TTestFmxLogViewer.TestCount_EmptyLog;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.AreEqual(0, LogViewer.Count, 'Count should be 0 for empty log');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestCount_WithEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddInfo('Message 2');
    LogViewer.RamLog.AddError('Error 1');

    Assert.IsTrue(LogViewer.Count > 0, 'Count should be greater than 0 after adding entries');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Verbosity Filter Tests }

procedure TTestFmxLogViewer.TestVerbosityFilter_DefaultValue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.AreEqual(lvVerbose, LogViewer.Verbosity, 'Default verbosity should be lvVerbose');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestVerbosityFilter_SetValue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    LogViewer.Verbosity:= lvWarnings;
    Assert.AreEqual(lvWarnings, LogViewer.Verbosity, 'Verbosity should be set to lvWarnings');

    LogViewer.Verbosity:= lvErrors;
    Assert.AreEqual(lvErrors, LogViewer.Verbosity, 'Verbosity should be set to lvErrors');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ ShowTime/ShowDate Tests }

procedure TTestFmxLogViewer.TestShowTime_DefaultFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowTime, 'ShowTime should default to False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowDate_DefaultFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsFalse(LogViewer.ShowDate, 'ShowDate should default to False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowTime_SetTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.ShowTime:= TRUE;
    Assert.IsTrue(LogViewer.ShowTime, 'ShowTime should be True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestShowDate_SetTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.ShowDate:= TRUE;
    Assert.IsTrue(LogViewer.ShowDate, 'ShowDate should be True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ AutoScroll Tests }

procedure TTestFmxLogViewer.TestAutoScroll_DefaultTrue;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    Assert.IsTrue(LogViewer.AutoScroll, 'AutoScroll should default to True');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestAutoScroll_SetFalse;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.AutoScroll:= FALSE;
    Assert.IsFalse(LogViewer.AutoScroll, 'AutoScroll should be False');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ Clear Tests }

procedure TTestFmxLogViewer.TestClear_RemovesAllEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;

    { Add some entries }
    LogViewer.RamLog.AddInfo('Message 1');
    LogViewer.RamLog.AddError('Error 1');
    Assert.IsTrue(LogViewer.Count > 0, 'Should have entries before clear');

    { Clear }
    LogViewer.Clear;

    Assert.AreEqual(0, LogViewer.Count, 'Count should be 0 after clear');
  finally
    FreeAndNil(LogViewer);
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   MULTI-LINE ENTRIES
   The spec (LightCore.LogRam.pas, TRamLog.prepareString): TRamLog keeps the line breaks of a message, so one
   entry may hold several lines; a view that shows one row per entry (this grid) puts it on one line at draw time.
   The spec of the flattening (LightCore.pas, FlattenLineBreaks): one space for each CRLF, CR or LF.
   The expected texts below are literals written from those two sentences.
-------------------------------------------------------------------------------------------------------------}

{ Adds one entry holding Msg and checks: one row, the row draws ExpectedRowText, the row after it draws nothing }
procedure TTestFmxLogViewer.checkOneEntry(CONST Msg, ExpectedRowText: string);
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.RamLog.AddInfo(Msg);

    Assert.AreEqual(1, LogViewer.Count,      'One entry must give one row');
    Assert.AreEqual(1, LogViewer.RowCount,   'The grid must hold one row');
    Assert.AreEqual(1, LogViewer.ColumnCount,'No date/time: one column');

    assertCellText(LogViewer, 0, 0, ExpectedRowText, 'Row 0');
    Assert.AreEqual(0, Length(drawCell(LogViewer, 0, 1)), 'Row 1 does not exist and must draw nothing');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestMultiLine_CRLF_OneRowFlattened;
begin
  checkOneEntry('Line A'#13#10'Line B', 'Line A Line B');
end;


procedure TTestFmxLogViewer.TestMultiLine_LoneLF_OneRowFlattened;
begin
  checkOneEntry('Line A'#10'Line B', 'Line A Line B');
end;


procedure TTestFmxLogViewer.TestMultiLine_LoneCR_OneRowFlattened;
begin
  checkOneEntry('Line A'#13'Line B', 'Line A Line B');
end;


{ The trailing break stays in the entry (TRamLog keeps it), so the row ends in one space - and no empty second row appears }
procedure TTestFmxLogViewer.TestMultiLine_TrailingBreak_OneRowNoExtraRow;
begin
  checkOneEntry('Head'#13#10, 'Head ');
end;


procedure TTestFmxLogViewer.TestMultiLine_BetweenSingleLines_RowsMatchEntries;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.RamLog.AddInfo('First');
    LogViewer.RamLog.AddInfo('Mid A'#13#10'Mid B'#10'Mid C');
    LogViewer.RamLog.AddInfo('Last');

    Assert.AreEqual(3, LogViewer.Count,    'Three entries must give three rows');
    Assert.AreEqual(3, LogViewer.RowCount, 'The grid must hold three rows');

    assertCellText(LogViewer, 0, 0, 'First',             'Row 0');
    assertCellText(LogViewer, 0, 1, 'Mid A Mid B Mid C', 'Row 1');
    assertCellText(LogViewer, 0, 2, 'Last',              'Row 2');
    Assert.AreEqual(0, Length(drawCell(LogViewer, 0, 3)), 'Row 3 does not exist and must draw nothing');
  finally
    FreeAndNil(LogViewer);
  end;
end;


procedure TTestFmxLogViewer.TestMultiLine_TwoMultiLineEntries_TwoRows;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.RamLog.AddInfo('One A'#13#10'One B');
    LogViewer.RamLog.AddInfo('Two A'#13'Two B'#13#10);

    Assert.AreEqual(2, LogViewer.Count,    'Two entries must give two rows, whatever the number of lines');
    Assert.AreEqual(2, LogViewer.RowCount, 'The grid must hold two rows');

    assertCellText(LogViewer, 0, 0, 'One A One B',  'Row 0');
    assertCellText(LogViewer, 0, 1, 'Two A Two B ', 'Row 1');
  finally
    FreeAndNil(LogViewer);
  end;
end;


{ With ShowTime the grid has two columns; the message is in column 1 and is flattened there too }
procedure TTestFmxLogViewer.TestMultiLine_ShowTime_MessageColumnFlattened;
var
  LogViewer: TLogViewer;
begin
  LogViewer:= TLogViewer.Create(FTestForm);
  try
    LogViewer.Parent:= FTestForm;
    LogViewer.ShowTime:= TRUE;
    LogViewer.RamLog.AddInfo('Line A'#10'Line B');

    Assert.AreEqual(2, LogViewer.ColumnCount, 'ShowTime: a time column and a message column');
    Assert.AreEqual(1, LogViewer.RowCount,    'One entry must give one row');
    assertCellText(LogViewer, 1, 0, 'Line A Line B', 'Message column, row 0');
  finally
    FreeAndNil(LogViewer);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxLogViewer);

end.
