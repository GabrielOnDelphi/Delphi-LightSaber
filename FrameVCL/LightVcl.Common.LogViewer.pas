UNIT LightVcl.Common.LogViewer;

{=============================================================================================================
   2026.11.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A log viewer based on TStringGrid.
   It can easily show up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.
   A log with TRamLog.RingSize keeps only its newest lines instead.

   What the view shows
      The grid holds COPIES of the visible lines (TAbstractLogLines.ReadPage), never pointers into the log, so the log may drop or free lines at any time.
      AutoScroll = TRUE: the view follows the newest line while it shows the bottom. When the user scrolls up, it stays on the same lines, also while the log drops old ones; scrolling back to the bottom follows again.
      RefreshInterval > 0: a new line only marks the view as changed; a timer redraws at most once per interval. Use it for a log that gets many lines per second.

   How to use it
      Drop a TLogViewer on your form and use it to log messages like this:
         LogViewer.RamLog.AddError('Something bad happened!');

   Application wide logging
      This component can also be used to see messages logged at the application level (see TAppData).
      For this, just call
         LogViewer.AssignExternalRamLog(AppData.RamLog);

      Now on you can send your logging messages directly to AppData, instead of sending them to the log window:
         AppData.LogError('Something bad happened!');

      The log window will automatically pop-up when a error is received.

   Full demo in:
      c:\Projects\LightSaber\Demo\VCL\Demo LightLog\VCL_Demo_Log.dpr

=============================================================================================================}

{TODO 5: Let user sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 5: Let user show/hide grid lines}

INTERFACE

USES
   Winapi.Windows,
   System.Classes, System.SysUtils,
   Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Grids, Vcl.ExtCtrls, VCL.ComCtrls,
   LightCore.LogRam, LightCore.LogTypes, LightCore.LogLinesAbstract;

TYPE
  { Helper class to access protected CalcColWidth method of TStringGrid.
    Used to calculate appropriate column width based on text content. }
  THackGrid = class(TStringGrid)
  public
    function CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer;
  end;

  TLogViewer = class(TPanel, ILogObserver)
   private
     FGrid        : THackGrid;
     FVerbChanged : TNotifyEvent;
     FVerbosity   : TLogVerbLvl;
     FAutoScroll  : Boolean;           // Autoscroll to bottom
     FShowTime    : Boolean;
     FShowDate    : Boolean;
     FRamLog      : TRamLog;
     FVerbTrackBar: TPanel;            // TLogVerbFilter
     FOwnRamLog   : Boolean;           // Frees RamLog if owned
     FFilteredRowCount: Integer;       // Cached count of filtered rows (drives scrollbar.Max)
     FScrollBar: TScrollBar;
     FRows        : TArray<RLogLine>;  // Copies of the visible lines, row HeaderOverhead + i = FRows[i]
     FTopRow      : Integer;           // The filtered line shown in the first data row
     FFollowing   : Boolean;           // The view shows the bottom, so it follows the newest line (only when AutoScroll)
     FDroppedSeen : Int64;             // Lines.DroppedCount when the page was read; -1 = read afresh (see RLogPage)
     FTimeFormat  : string;
     FRefreshInterval: Integer;
     FRefreshTimer: TTimer;
     FDirty       : Boolean;           // A Populate came while RefreshInterval > 0; the timer redraws
     FFormDestroying: Boolean;         { Set TRUE in TfrmRamLog.FormDestroy so already-queued TThread.Queue closures (posted by background-thread log appends) bail out instead of touching a freed viewer.
                                         Distinct from TComponent.Destroying (which only flips once we are inside our own destructor — too late for a closure that has already entered Populate).
                                         Mirrors the same pattern in LightFmx.Common.LogViewer. }
     procedure setTimeFormat(const Value: string);
     function  isTimeFormatStored: Boolean;
     procedure setRefreshInterval(Value: Integer);
     procedure refreshTimerTimer(Sender: TObject);
     procedure refreshNow;
     procedure readPage;
     function  getVisibleText: string;
     function  getFollowing: Boolean;
     function  timeText(Time: TDateTime): string;
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
     procedure FixFixedRow;
     procedure resizeColumns;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  getLineFiltered(Row: Integer): PLogLine;
     procedure scrollBarChange(Sender: TObject);
   protected
     procedure CreateWnd; override;
     procedure Resize; override;
     procedure setUpRows;
     procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure AssignExternalRamLog(ExternalLog: TRamLog);
     procedure Clear;

     procedure RegisterVerbFilter(TrackBar: TPanel{TLogVerbFilter});
     procedure Populate;
     procedure PopUpWindow;
     procedure ChangeScrollBarVisibility(aVisible: boolean);
     procedure SaveAsRtf(const FullPath: string);
     property  FormDestroying: Boolean read FFormDestroying write FFormDestroying;
     property  Grid: THackGrid read FGrid;   { The grid inside the panel: give it the focus with Form.ActiveControl }

     function  Count: Integer;
     procedure CopyAll;
     procedure CopyVisible;
     procedure CopyCurLine;
   published
     property ShowTime     : Boolean      read FShowTime    write setShowTime default FALSE;
     property ShowDate     : Boolean      read FShowDate    write setShowDate default FALSE;
     property AutoScroll   : Boolean      read FAutoScroll  write FAutoScroll default TRUE;

     property RamLog       : TRamLog      read FRamLog;

     property Verbosity    : TLogVerbLvl  read FVerbosity   write SetVerbFilter default lvVerbose;
     property OnVerbChanged: TNotifyEvent read FVerbChanged write FVerbChanged;   { Triggered when verbosity filter level changes }

     property TimeFormat     : string  read FTimeFormat      write setTimeFormat stored isTimeFormatStored;   { FormatDateTime format of the time column (ShowTime). Default 'hh:nn'. }
     property RefreshInterval: Integer read FRefreshInterval write setRefreshInterval default 0;              { Milliseconds. 0 = redraw at every new line. > 0 = redraw at most once per interval. }

     { Read-only, for GUI tests (Autopilot read_property). Never streamed: they have no setter. }
     property LineCount  : Integer read FFilteredRowCount;   { Lines that pass the verbosity filter }
     property TopRow     : Integer read FTopRow;             { Filtered line shown in the first data row }
     property Following  : Boolean read getFollowing;        { TRUE = the view follows the newest line }
     property VisibleText: string  read getVisibleText;      { The messages of the visible rows, one per line }
  end;

function Verbosity2Color (Verbosity: TLogVerbLvl; IsDark: Boolean = False): TColor;


{ Rich text helpers shared by TLogViewer.SaveAsRtf and TRichLog (LightVcl.Visual.RichLog) }
TYPE
  TLevelToColor = function(Level: TLogVerbLvl; IsDark: Boolean): TColor;   { The palette of a view: Verbosity2Color here, Level2Color in LightVcl.Visual.RichLogUtils }

function  RichEditTextLength(RichEdit: TCustomRichEdit): Integer;
function  AppendRichText    (RichEdit: TCustomRichEdit; CONST Text: string; Color: TColor; Bold: Boolean): Integer;
procedure SaveLogAsRtf      (Log: TRamLog; CONST FullPath: string; LevelToColor: TLevelToColor; IsDark: Boolean; Host: TWinControl);

procedure Register;


IMPLEMENTATION

USES
   Winapi.Messages, Winapi.RichEdit,
   LightCore, LightCore.Types,
   LightCore.Colors, LightVcl.Common.Clipboard, LightVcl.Common.LogFilter;

CONST
  DefaultTimeFormat = 'hh:nn';



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Create internal RamLog by default so "drop and use" works immediately.
    Use AssignExternalRamLog to connect to an external log (e.g., AppData.RamLog). }
  FOwnRamLog:= TRUE;
  if FOwnRamLog
  then FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver); 

  FShowTime   := FALSE;
  FShowDate   := FALSE;
  FVerbosity  := lvVerbose;
  FAutoScroll := TRUE;
  FFollowing  := TRUE;
  FDroppedSeen:= -1;
  FTimeFormat := DefaultTimeFormat;

  FScrollBar         := TScrollBar.Create(Self);
  FScrollBar.Parent  := Self; // Assumes a parent panel

  // Grid setup
  FGrid        := THackGrid.Create(Self);
  FGrid.Parent := Self;
end;


procedure TLogViewer.CreateWnd;
begin
  inherited CreateWnd;
  BevelOuter:= bvNone;
  ShowCaption:= FALSE;

  // ScrollBar
  FScrollBar.Visible    := FALSE;
  FScrollBar.Align      := alRight;
  FScrollBar.Kind       := sbVertical;
  FScrollBar.Width      := 12;
  FScrollBar.OnChange   := scrollBarChange;

  // Grid
  FGrid.Align           := alClient;
  FGrid.BevelOuter      := bvNone;
  FGrid.ScrollBars      := ssNone;         // Disable grid's built-in scrollbars
  FGrid.RowCount        := HeaderOverhead;
  FGrid.ColCount        := 1;
  FGrid.DefaultRowHeight:= 22;
  FGrid.Options         := FGrid.Options+ [goColSizing, goRowSelect] - [goRangeselect];
  FGrid.OnDrawCell      := GridDrawCell;

  // First setup
  setUpRows;
  resizeColumns;
end;


destructor TLogViewer.Destroy;
begin
  FreeAndNil(FRefreshTimer);   { No redraw may start while the viewer goes }

  if FOwnRamLog
  then FreeAndNil(FRamLog)
  else
    // External log: unregister observer so the log doesn't callback into freed memory. Only this viewer: other viewers may observe the same log.
    if Assigned(FRamLog)
    then FRamLog.UnregisterLogObserver(Self as ILogObserver);

  FreeAndNil(Fgrid);
  FreeAndNil(FScrollBar);
  inherited;
end;


{ Clears all log entries from both the display and the underlying RamLog. }
procedure TLogViewer.Clear;
begin
  Assert(FRamLog <> NIL, 'Clear: RamLog not assigned!');

  FRows:= NIL;
  FTopRow:= 0;
  FFollowing:= TRUE;
  FDroppedSeen:= -1;
  FRamLog.Clear;
  setUpRows;
  resizeColumns;
  FGrid.InvalidateGrid;
end;


{ Shows or hides the custom scrollbar and adjusts column widths accordingly. }
procedure TLogViewer.ChangeScrollBarVisibility(aVisible: Boolean);
VAR MustResize: Boolean;
begin
  MustResize:= aVisible <> FScrollBar.Visible;
  FScrollBar.Visible:= aVisible;
  if MustResize
  then resizeColumns;
end;


{--------------------------------------------------------------------------------------------------
   Configures the grid's row/column count and scrollbar based on current content and filter.
   Called when: data changes, filter changes, component resizes, or date/time display toggles.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.setUpRows;
VAR
  NewColCount, VisibleRows: Integer;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!');
  if NOT HandleAllocated then EXIT;  { Can only configure after handle is allocated }

  FGrid.BeginUpdate;
  TRY
    { Calculate visible rows based on grid height }
    VisibleRows:= Trunc(ClientHeight / FGrid.DefaultRowHeight) - HeaderOverhead;
    if VisibleRows < 1
    then VisibleRows:= 1;
    FGrid.RowCount:= VisibleRows + HeaderOverhead;
    FixFixedRow;

    { Set column count based on date/time display settings }
    if FShowDate or FShowTime
    then NewColCount:= 2
    else NewColCount:= 1;
    FGrid.ColCount:= NewColCount;
  FINALLY
    FGrid.EndUpdate;
  END;

  readPage;   { Now that RowCount is right: the rows, the line count and the scroll bar }
end;


{--------------------------------------------------------------------------------------------------
   Reads the visible page from the log (copies of the lines, see RLogPage), then sets the scroll bar to match.
   Following: the page is the newest lines.
   Not following: the page starts at FTopRow, moved back by the lines the log dropped since the last read, so the user keeps seeing the same lines.

   OnChange of the scroll bar is off while Max and Position change, so this routine does not call itself through scrollBarChange.
   Safe to clear OnChange without saving it: FScrollBar is private and its only handler is scrollBarChange.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.readPage;
VAR
  Page: RLogPage;
  VisibleRows: Integer;
begin
  if NOT HandleAllocated
  OR NOT Assigned(FRamLog) then EXIT;

  VisibleRows:= FGrid.RowCount - HeaderOverhead;
  if VisibleRows < 1
  then VisibleRows:= 1;

  Page:= Default(RLogPage);
  Page.Verbosity  := FVerbosity;
  Page.Follow     := FAutoScroll AND FFollowing;
  Page.TopRow     := FTopRow;
  Page.DroppedSeen:= FDroppedSeen;
  SetLength(Page.Rows, VisibleRows);
  FRamLog.Lines.ReadPage(Page);

  FRows            := Page.Rows;
  FTopRow          := Page.TopRow;
  FDroppedSeen     := Page.DroppedSeen;
  FFilteredRowCount:= Page.Total;

  FScrollBar.OnChange:= NIL;
  TRY
    if FFilteredRowCount > VisibleRows
    then
      begin
        FScrollBar.Max:= FFilteredRowCount - VisibleRows;
        FScrollBar.Position:= FTopRow;
        ChangeScrollBarVisibility(TRUE);
      end
    else
      begin
        FScrollBar.Max:= 0;
        FScrollBar.Position:= 0;
        FFollowing:= TRUE;          { Every line is in view, so the view shows the bottom }
        ChangeScrollBarVisibility(FALSE);
      end;
    FScrollBar.LargeChange:= VisibleRows;
  FINALLY
    FScrollBar.OnChange:= scrollBarChange;
  END;
end;


{ Sets the verbosity filter level and updates the display accordingly. }
procedure TLogViewer.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
    begin
      FVerbosity:= Value;
      FDroppedSeen:= -1;      { A drop count of another filter: read the page afresh }

      { Update associated UI element if it exists and needs syncing }
      if (FVerbTrackBar <> NIL)
      then
        if (FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity
        then (FVerbTrackBar as TLogVerbFilter).Verbosity:= Self.Verbosity;

      { Notify listeners that verbosity changed }
      if Assigned(FVerbChanged)
      then FVerbChanged(Self);

      { Refresh the grid content based on the new filter }
      setUpRows;
      FGrid.InvalidateGrid;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   EXTERNAL LOG ASSIGNMENT
   Allows the LogViewer to display messages from an externally managed TRamLog instance (e.g., AppData.RamLog) instead of its own internal log.
-------------------------------------------------------------------------------------------------------------}
procedure TLogViewer.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  if ExternalLog = NIL
  then raise Exception.Create('AssignExternalRamLog: ExternalLog parameter cannot be nil');

  { Detach from existing log before switching }
  if Assigned(FRamLog) then
    begin
      if FOwnRamLog
      then FreeAndNil(FRamLog)
      else FRamLog.UnregisterLogObserver(Self as ILogObserver);  // External log: just unregister (this viewer only), caller owns it
    end;

  FOwnRamLog:= FALSE;  { We received the log from an external source - don't auto-release it }
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);   { Added next to any viewer already observing this log (e.g. the hidden log window of TAppData) }

  { Populate the grid with the data from the newly assigned log }
  Populate;
end;


{ Refreshes the grid display from the RamLog data.
  Called by the ILogObserver interface when log content changes.
  RefreshInterval > 0: only marks the view as changed; refreshTimerTimer redraws. Safe because the grid holds copies, not pointers into the log.

  Guard against already-queued TThread.Queue closures firing after the host form's FormDestroy has set FormDestroying:=TRUE — without this guard the closure would touch a viewer that Application has just freed. }
procedure TLogViewer.Populate;
begin
  if FFormDestroying
  OR NOT Assigned(FRamLog) then EXIT;

  if FRefreshInterval > 0
  then FDirty:= TRUE
  else refreshNow;
end;


procedure TLogViewer.refreshNow;
begin
  FDirty:= FALSE;
  setUpRows;
  FGrid.InvalidateGrid;
end;


procedure TLogViewer.refreshTimerTimer(Sender: TObject);
begin
  if FDirty
  AND NOT FFormDestroying
  AND Assigned(FRamLog)
  then refreshNow;
end;


procedure TLogViewer.setRefreshInterval(Value: Integer);
begin
  Assert(Value >= 0, 'TLogViewer.RefreshInterval must be >= 0 (0 = off)');
  if Value = FRefreshInterval then EXIT;
  FRefreshInterval:= Value;

  if Value = 0 then
    begin
      FreeAndNil(FRefreshTimer);
      if FDirty AND NOT FFormDestroying AND Assigned(FRamLog)
      then refreshNow;
    end
  else
    begin
      if FRefreshTimer = NIL then
        begin
          FRefreshTimer:= TTimer.Create(NIL);   { Freed by Destroy, first }
          FRefreshTimer.OnTimer:= refreshTimerTimer;
        end;
      FRefreshTimer.Interval:= Value;
      FRefreshTimer.Enabled:= TRUE;
    end;
end;


procedure TLogViewer.setTimeFormat(const Value: string);
begin
  if FTimeFormat = Value then EXIT;
  FTimeFormat:= Value;
  resizeColumns;
  FGrid.InvalidateGrid;
end;


function TLogViewer.isTimeFormatStored: Boolean;
begin
  Result:= FTimeFormat <> DefaultTimeFormat;
end;


function TLogViewer.timeText(Time: TDateTime): string;
begin
  Result:= '';
  if ShowDate then Result:= DateToStr(Time);
  if ShowTime then Result:= Result + ' ' + FormatDateTime(FTimeFormat, Time);
end;


function TLogViewer.getFollowing: Boolean;
begin
  Result:= FAutoScroll AND FFollowing;
end;


function TLogViewer.getVisibleText: string;
VAR i: Integer;
begin
  Result:= '';
  for i:= 0 to High(FRows) do
    begin
      if i > 0
      then Result:= Result + sLineBreak;
      Result:= Result + FlattenLineBreaks(FRows[i].Msg);
    end;
end;


{ Returns the log line shown at the grid row, or NIL when the row shows no line.
  The pointer points into the viewer's own copy (FRows): use it at once, before the next refresh. }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
VAR Index: Integer;
begin
  Index:= Row - HeaderOverhead;
  if (Index < 0) OR (Index >= Length(FRows))
  then Result:= NIL
  else Result:= @FRows[Index];
end;


{--------------------------------------------------------------------------------------------------
   Custom cell drawing for the log grid.
   Handles header row, applies verbosity-based coloring, and displays date/time if enabled.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.GridDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
VAR
   s: string;
   CurLine: PLogLine;
begin
  { Skip drawing in design mode or during component creation }
  if (csDesigning in ComponentState)
  or (csCreating in ControlState)
  or (NOT FGrid.DefaultDrawing)
  then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  { Header row - let the grid's default drawing paint the captions from Cells[0,0]/Cells[1,0] (set by resizeColumns).
    Drawing them again here with Canvas.TextRect produces a visible duplicate/ghost overlapping the default-drawn text, especially with VCL skins that paint a themed header background. }
  if ARow = 0
  then EXIT;

  { Data rows - the viewer's copy of the line (FRows), NIL below the last line }
  CurLine:= getLineFiltered(ARow);
  if CurLine = NIL then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  { Build cell text based on column }
  if FGrid.ColCount = 2
  then
    case ACol of
      0: s:= timeText(CurLine.Time);
      1: s:= FlattenLineBreaks(CurLine.Msg);   { A multi-line entry is one grid row }
    else
      raise Exception.Create('GridDrawCell: Invalid column index');
    end
  else
    s:= FlattenLineBreaks(CurLine.Msg);

  { Draw with verbosity-based color }
  FGrid.Canvas.Font.Color:= Verbosity2Color(CurLine.Level, NOT IsLightStyleColor(clWindow));
  FGrid.Canvas.TextRect(ARect, ARect.Left + 5, ARect.Top + 2, s);
end;


{-------------------------------------------------------------------------------------------------------------
   GRID ACCESS & UI INTERACTION
-------------------------------------------------------------------------------------------------------------}

{ Returns the number of log entries visible with the current verbosity filter. }
function TLogViewer.Count: Integer;
begin
  if RamLog = NIL  // can it ever be nil?
  then Result:= 0
  else Result:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);
end;


{ Ensures the header row stays fixed when scrolling. }
procedure TLogViewer.FixFixedRow;
begin
  if (csCreating in ControlState) then Exit;

  if FGrid.RowCount > 1
  then FGrid.FixedRows := 1
  else FGrid.FixedRows := 0;
end;


procedure TLogViewer.setShowDate(const Value: Boolean);
begin
  if FShowDate <> Value then
    begin
      FShowDate:= Value;
      setUpRows;
      resizeColumns;
    end;
end;


procedure TLogViewer.setShowTime(const Value: Boolean);
begin
  if FShowTime <> Value then
    begin
      FShowTime := Value;
      setUpRows;
      resizeColumns;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   WINDOW / FORM INTERACTION
-------------------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------
   Shows the parent form containing this log viewer.
   Called via ILogObserver interface when errors occur and ShowOnError is enabled.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.PopUpWindow;
VAR
  ParentForm: TCustomForm;
begin
  if FFormDestroying then EXIT;

  ParentForm:= GetParentForm(Self);
  if Assigned(ParentForm) then
    begin
      { Show if hidden }
      if NOT ParentForm.Visible
      then ParentForm.Show;

      { Restore if minimized }
      if ParentForm.WindowState = TWindowState.wsMinimized
      then ParentForm.WindowState:= TWindowState.wsNormal;

      { Bring to front }
      ParentForm.BringToFront;
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   COLUMN MANAGEMENT
-------------------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------
   Adjusts column widths based on current display settings.
   - Single column mode: Message column takes full width.
   - Two column mode: Time column sized to fit date/time text, Message column takes remainder.
   Called when: component resizes, date/time display toggles, or scrollbar visibility changes.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.resizeColumns;
var
  sTime: string;
  TimeColWidth: Integer;
begin
  case FGrid.ColCount of  // Warning: This can be zero when the application starts because ColCount is set in CreateWnd which starts late.
    1: begin
         FGrid.Cells[0, 0] := 'Message';
         FGrid.ColWidths[0] := FGrid.ClientWidth;
       end;
    2: begin
         FGrid.Cells[0, 0] := 'Time';
         FGrid.Cells[1, 0] := 'Message';

         // Adjust as needed for date/time column
         sTime:= timeText(Now);

         TimeColWidth := FGrid.CalculateColWidth(Length(sTime), sTime);  // Make first col as large as the text
         FGrid.ColWidths[0] := TimeColWidth;
         FGrid.ColWidths[1] := FGrid.ClientWidth - TimeColWidth - FGrid.GridLineWidth;
       end;
  end;
end;


procedure TLogViewer.Resize;
begin
  inherited Resize;
  resizeColumns;          { Adjust column widths to the new ClientWidth. }

  { ClientHeight changed, so the number of visible rows changed too.
    setUpRows reads ClientHeight to recompute FGrid.RowCount and FScrollBar.Max, and ends by calling readPage.
    Without this call, growing the panel would leave empty rows at the bottom (RowCount stuck at the old value), and shrinking would orphan filled-but-invisible rows.
    Skip when not yet wired (Resize can fire before the constructor finishes assigning FRamLog). }
  if Assigned(FRamLog) 
  and HandleAllocated
  then setUpRows;
end;


{-------------------------------------------------------------------------------------------------------------
   CLIPBOARD OPERATIONS
-------------------------------------------------------------------------------------------------------------}

{ Copies all currently visible (filtered) log lines to the clipboard. }
procedure TLogViewer.CopyVisible;
VAR
  i: Integer;
  Lines: TStringList;
  CurLine: PLogLine;
begin
  Lines:= TStringList.Create;
  TRY
    Lines.BeginUpdate;
    TRY
      { Iterate through FILTERED rows currently visible in the grid }
      for i:= HeaderOverhead to FGrid.RowCount - 1 do
        begin
          CurLine:= GetLineFiltered(i);
          if CurLine <> NIL
          then Lines.Add(CurLine.Msg);
        end;
    FINALLY
      Lines.EndUpdate;
    END;

    LightVcl.Common.Clipboard.StringToClipboard(Lines.Text);
  FINALLY
    FreeAndNil(Lines);
  END;
end;


{ Copies ALL log lines to the clipboard, ignoring the current verbosity filter. }
procedure TLogViewer.CopyAll;
VAR sText: string;
begin
  if Assigned(FRamLog)
  then sText := RamLog.GetAsText
  else sText := 'No RAM log assigned!';
  LightVcl.Common.Clipboard.StringToClipboard(sText);
end;


{ Copies the currently selected log line to the clipboard. }
procedure TLogViewer.CopyCurLine;
VAR
  CurLine: PLogLine;
begin
  if FGrid.Row < HeaderOverhead then EXIT;  { No valid row selected }

  CurLine:= getLineFiltered(FGrid.Row);
  if CurLine <> NIL
  then LightVcl.Common.Clipboard.StringToClipboard(CurLine.Msg);
end;


{ Saves the whole log (no verbosity filter) to an RTF file, in the Verbosity2Color palette. The work is done by SaveLogAsRtf. }
procedure TLogViewer.SaveAsRtf(const FullPath: string);
begin
  SaveLogAsRtf(FRamLog, FullPath, Verbosity2Color, NOT IsLightStyleColor(clWindow), Self);
end;



{-------------------------------------------------------------------------------------------------------------
   RICH TEXT HELPERS
   Shared by TLogViewer.SaveAsRtf and TRichLog.
-------------------------------------------------------------------------------------------------------------}

{ The number of characters in the rich edit, counted as SelStart counts them: a paragraph break is one character.
  EM_GETTEXTLENGTHEX without GTL_USECRLF, as the print code of TCustomRichEdit does (Vcl.ComCtrls.pas). Copies no text, unlike Length(Text). }
function RichEditTextLength(RichEdit: TCustomRichEdit): Integer;
VAR TextLen: TGetTextLengthEx;
begin
  Assert(RichEdit <> NIL, 'RichEditTextLength: RichEdit is NIL');
  TextLen.flags   := GTL_NUMCHARS OR GTL_PRECISE;
  TextLen.codepage:= 1200;   { UTF-16 }
  Result:= SendMessage(RichEdit.Handle, EM_GETTEXTLENGTHEX, WPARAM(@TextLen), 0);
end;


{ Inserts Text at the end of the rich edit, in the given color and weight, and returns the number of characters the rich edit gained (measured, not assumed).
  Text should come from NormalizeLogBreaks.
  The insert is done with EM_REPLACESEL (wParam 0: not undoable), never with Lines.Add, which raises EOutOfResources on a lone #10 or #13 (TRichEditStrings.Insert counts only #13#10 pairs).
  A text that starts with an RTF signature is inserted in two parts, so the rich edit cannot read it as RTF. }
function AppendRichText(RichEdit: TCustomRichEdit; CONST Text: string; Color: TColor; Bold: Boolean): Integer;
VAR
  StartLen: Integer;
  Range: TCharRange;
  Head: string;
begin
  Assert(RichEdit <> NIL, 'AppendRichText: RichEdit is NIL');
  StartLen:= RichEditTextLength(RichEdit);
  if Text = '' then EXIT(0);

  Range.cpMin:= StartLen;
  Range.cpMax:= StartLen;
  SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, LPARAM(@Range));

  RichEdit.SelAttributes.Color:= Color;
  if Bold
  then RichEdit.SelAttributes.Style:= [fsBold]
  else RichEdit.SelAttributes.Style:= [];

  Head:= Copy(Text, 1, 5);
  if SameText(Head, '{\rtf') OR SameText(Head, '{urtf')
  then
    begin
      SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, LPARAM(PChar('{')));
      SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, LPARAM(PChar(Copy(Text, 2, MaxInt))));
    end
  else
    SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, LPARAM(PChar(Text)));

  Result:= RichEditTextLength(RichEdit) - StartLen;
end;


{--------------------------------------------------------------------------------------------------
   Saves every line of Log (no verbosity filter) to an RTF file.
   Each line gets the color LevelToColor gives for its level, and bold lines are bold.
   A multi-line message stays one entry with line breaks inside it.
   Host is the window that holds the temporary rich edit for the duration of the save; it must be able to get a handle.

   Thread Safety — two-phase pattern with VALUE copy (not pointer copy):
     Phase 1 copies the data we need (Msg, Level, Bold) out of each PLogLine into a local TArray<TRtfLine> under one read lock.
     The lock is held only for the copy (~microseconds for typical logs).
     Phase 2 walks the local array doing the slow TRichEdit work (Win32 SendMessage per line).
     No lock held; the local array owns its data.

   Why we copy values, not pointers:
     PLogLine pointers are owned by FRamLog.Lines.
     If a worker hits MaxEntries during Phase 2 and CheckAndSaveToDisk's overflow path runs SnapshotAndClear, it queues a "free the snapshot" closure on the main thread.
     TRichEdit operations (Lines.Add, SaveToFile) can pump messages — including queued closures — and the snapshot's PLogLine records would be Disposed mid-walk.
     Copying values up-front decouples Phase 2 from FRamLog's lifetime.

   Cost: ~16 bytes/line struct overhead (string ref + Level + Bold). String content is NOT duplicated.
     For a 1M-line log: ~16 MB array overhead + zero extra string content.
--------------------------------------------------------------------------------------------------}
procedure SaveLogAsRtf(Log: TRamLog; CONST FullPath: string; LevelToColor: TLevelToColor; IsDark: Boolean; Host: TWinControl);
type
  TRtfLine = record
    Msg  : string;
    Level: TLogVerbLvl;
    Bold : Boolean;
  end;
VAR
  RichEdit : TRichEdit;
  Snapshot : TArray<TRtfLine>;
  SnapCount: Integer;
  i        : Integer;
  Text     : string;
begin
  if FullPath = ''
  then raise Exception.Create('SaveAsRtf: FullPath parameter cannot be empty');

  if NOT Assigned(Log)
  then raise Exception.Create('SaveAsRtf: RamLog is not assigned');

  if NOT Assigned(Host)
  then raise Exception.Create('SaveAsRtf: Host is not assigned');

  Assert(Assigned(LevelToColor), 'SaveLogAsRtf: LevelToColor is NIL');

  { Phase 1 — copy values out of each PLogLine under a single read lock.
    Buffer starts at 256 and doubles on overflow — amortized O(N) growth. }
  SnapCount:= 0;
  SetLength(Snapshot, 256);
  Log.Lines.ForEachLocked(procedure (Line: PLogLine)
    begin
      if SnapCount >= Length(Snapshot)
      then SetLength(Snapshot, Length(Snapshot) * 2);
      Snapshot[SnapCount].Msg  := Line.Msg;     { Strings are COW — assignment is a refcount bump, not a copy. }
      Snapshot[SnapCount].Level:= Line.Level;
      Snapshot[SnapCount].Bold := Line.Bold;
      Inc(SnapCount);
    end);

  { Phase 2 — slow Win32 work, lock-free.
    The local Snapshot owns its data, so a concurrent overflow + snapshot-dispose during this loop is harmless. }
  RichEdit:= TRichEdit.Create(NIL);
  TRY
    RichEdit.Visible:= FALSE;
    RichEdit.MaxLength:= $7FFFFFF0;   { The text limit of TRichLog. Set before the handle exists: TCustomEdit.CreateWnd passes it to the window. }
    RichEdit.Parent:= Host;           { A rich edit needs a parent window to get a handle }
    RichEdit.PlainText:= FALSE;

    for i:= 0 to SnapCount - 1 do
      begin
        Text:= NormalizeLogBreaks(Snapshot[i].Msg);
        if i > 0
        then Text:= #13 + Text;       { The paragraph break that ends the entry before }
        AppendRichText(RichEdit, Text, LevelToColor(Snapshot[i].Level, IsDark), Snapshot[i].Bold);
      end;

    RichEdit.Lines.SaveToFile(FullPath);
  FINALLY
    FreeAndNil(RichEdit);
  END;
end;


{ Associates a verbosity filter control (TLogVerbFilter) with this log viewer.
  When the filter changes, the log viewer's verbosity is automatically updated. }
procedure TLogViewer.RegisterVerbFilter(TrackBar: TPanel);
begin
  FVerbTrackBar:= TrackBar;
end;


{ The user moved the scroll bar: at the bottom the view follows the newest line again, anywhere else it stays on the lines the user chose }
procedure TLogViewer.scrollBarChange(Sender: TObject);
begin
  FTopRow:= FScrollBar.Position;
  FFollowing:= FScrollBar.Position >= FScrollBar.Max;
  readPage;
  FGrid.InvalidateGrid;
end;


{ Maps a log verbosity level to its display color.
  Higher severity levels use warmer/more alarming colors. }
function Verbosity2Color(Verbosity: TLogVerbLvl; IsDark: Boolean): TColor;
begin
  if IsDark then
    case Verbosity of
      lvDebug    : Result:= TColor($909090);   { Medium gray }
      lvVerbose  : Result:= TColor($B0B0B0);   { Light gray }
      lvHints    : Result:= TColor($C0C0C0);   { Silver }
      lvInfos    : Result:= clWhite;
      lvImportant: Result:= clOrangeLt;
      lvWarnings : Result:= clOrange;
      lvErrors   : Result:= clRed;
    else
      raise Exception.Create('Verbosity2Color: Invalid log verbosity level');
    end
  else
    case Verbosity of
      lvDebug    : Result:= TColor($909090);   { Light gray }
      lvVerbose  : Result:= TColor($808080);   { Silver }
      lvHints    : Result:= TColor($707070);   { Gray }
      lvInfos    : Result:= clBlack;
      lvImportant: Result:= clOrangeDk;
      lvWarnings : Result:= clOrange;
      lvErrors   : Result:= clRed;
    else
      raise Exception.Create('Verbosity2Color: Invalid log verbosity level');
    end;
end;


function THackGrid.CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer;
begin
  Result:= CalcColWidth(ATextLength, ACaption, NIL);
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLogViewer]);
end; 



end.