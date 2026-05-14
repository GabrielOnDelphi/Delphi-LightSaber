UNIT LightVcl.Common.LogViewer;

{=============================================================================================================
   2026.05.13
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A log viewer based on TStringGrid.
   It can easily show up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   How to use it
      Drop a TLogViewer on your form and use it to log messages like this:
         LogViewer.RamLog.AddError('Something bad happened!');

   Application wide logging
      This component can also be used to see messages logged at the application level (see TAppData).
      For this, just assign
         LogViewer.RamLog:= AppData.RamLog;

      Now on you can send your logging messages directly to AppData, instead of sending them to the log window:
         LogViewer.RamLog.AddError('Something bad happened!');

      The log window will automatically pop-up when a error is received.

   Full demo in:
      c:\Projects\LightSaber\Demo\Demo LightLog\FMX\FMX_Demo_Log.dpr

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
     FFormDestroying: Boolean;         // Set TRUE in TfrmRamLog.FormDestroy so already-queued
                                       // TThread.Queue closures (posted by background-thread log appends) bail out instead of touching a freed viewer.
                                       // Distinct from TComponent.Destroying (which only flips once we are inside our own destructor — too late for a closure that has already entered Populate).
                                       // Mirrors the same pattern in LightFmx.Common.LogViewer.
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
     procedure FixFixedRow;
     procedure resizeColumns;
     procedure scrollToBottom;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  getLineFiltered(Row: Integer): PLogLine;
     procedure scrollBarChange(Sender: TObject);
     procedure refreshVisibleSlice;
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
  end;

function Verbosity2Color (Verbosity: TLogVerbLvl; IsDark: Boolean = False): TColor;

procedure Register;


IMPLEMENTATION

USES
   LightCore.Types,
   LightVcl.Common.Colors, LightVcl.Common.Clipboard, LightVcl.Common.LogFilter;



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

  FScrollBar         := TScrollBar.Create(Self);
  FScrollBar.Parent  := Self; // Assumes a parent panel

  // Grid setup
  FGrid        := THackGrid.Create(Self);
  FGrid.Parent := Self; // Set the panel as parent
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
  if FOwnRamLog
  then FreeAndNil(FRamLog)
  else
    // External log: unregister observer so the log doesn't callback into freed memory
    if Assigned(FRamLog)
    then FRamLog.UnregisterLogObserver;

  FreeAndNil(Fgrid);
  FreeAndNil(FScrollBar);
  inherited;
end;


{ Clears all log entries from both the display and the underlying RamLog. }
procedure TLogViewer.Clear;
begin
  Assert(FRamLog <> NIL, 'Clear: RamLog not assigned!');

  FGrid.RowCount:= HeaderOverhead;  { Reset to header only }
  FRamLog.Clear;
  resizeColumns;
end;


{ Shows or hides the custom scrollbar and adjusts column widths accordingly. }
procedure TLogViewer.ChangeScrollBarVisibility(aVisible: Boolean);
VAR MustResize: Boolean;
begin
  MustResize:= aVisible <> FScrollBar.Visible;
  FScrollBar.Visible:= aVisible;
  if MustResize
  then resizeColumns;  { Only resize if visibility actually changed }
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
    { Calculate filtered row count (cached for performance) }
    FFilteredRowCount:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);

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

    { Configure scrollbar visibility and range.
      Suppress FScrollBar.OnChange while we mutate Max/Position so we don't trigger a redundant refreshVisibleSlice — the explicit call after EndUpdate covers it.

      Safe to clobber OnChange unconditionally: FScrollBar is a private TScrollBar
      created in the constructor and never exposed. No external code can assign a
      different OnChange handler, so restoring scrollBarChange in FINALLY always
      reinstates the only legitimate value. If FScrollBar is ever made public or
      its OnChange is exposed, switch to save-and-restore via a local TNotifyEvent. }
    FScrollBar.OnChange:= NIL;
    TRY
      if FFilteredRowCount > VisibleRows
      then
        begin
          FScrollBar.Max:= FFilteredRowCount - VisibleRows;
          ChangeScrollBarVisibility(TRUE);
        end
      else
        begin
          FScrollBar.Max:= 0;
          FScrollBar.Position:= 0;
          ChangeScrollBarVisibility(FALSE);
        end;
      FScrollBar.LargeChange:= VisibleRows;
    FINALLY
      FScrollBar.OnChange:= scrollBarChange;
    END;
  FINALLY
    FGrid.EndUpdate;
  END;

  { Re-fill the visible PLogLine slice now that RowCount and ScrollBar.Max are correct. }
  refreshVisibleSlice;
end;


{ Sets the verbosity filter level and updates the display accordingly. }
procedure TLogViewer.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
    begin
      FVerbosity:= Value;

      { Update associated UI element if it exists and needs syncing }
      if (FVerbTrackBar <> NIL)
      then
        if (FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity
        then (FVerbTrackBar as TLogVerbFilter).Verbosity:= Self.Verbosity;

      { Notify listeners that verbosity changed }
      if Assigned(FVerbChanged)
      then FVerbChanged(Self);

      { Refresh the grid content based on the new filter
        (setUpRows internally calls refreshVisibleSlice). }
      setUpRows;
      FGrid.InvalidateGrid;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   EXTERNAL LOG ASSIGNMENT
   Allows the LogViewer to display messages from an externally managed TRamLog instance
   (e.g., AppData.RamLog) instead of its own internal log.
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
      else FRamLog.UnregisterLogObserver;  // External log: just unregister, caller owns it
    end;

  FOwnRamLog:= FALSE;  { We received the log from an external source - don't auto-release it }
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  { Populate the grid with the data from the newly assigned log }
  Populate;
end;


{ Refreshes the grid display from the RamLog data.
  Called by the ILogObserver interface when log content changes.
  setUpRows must run first: it updates FFilteredRowCount and FScrollBar.Max, then calls
  refreshVisibleSlice. scrollToBottom may then change FScrollBar.Position which fires
  scrollBarChange (refreshes the slice + repaints). If Position doesn't actually change,
  the slice from setUpRows is still correct. Either way, no extra work needed here.

  Guard against already-queued TThread.Queue closures firing after the host form's
  FormDestroy has set FormDestroying:=TRUE — without this guard the closure would
  touch a viewer that Application has just freed. }
procedure TLogViewer.Populate;
begin
  if FFormDestroying 
  OR NOT Assigned(FRamLog) then EXIT;

  setUpRows;

  if AutoScroll
  then scrollToBottom;

  FGrid.InvalidateGrid;
end;


{ Returns the log line at the specified grid row (after filtering).
  Reads the cached PLogLine pointer stored in the grid's Objects[0, Row] slot.
  Returns NIL if the row is invalid, out of bounds, or has no data. }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
begin
  Result:= NIL;
  if (Row < HeaderOverhead) or (Row >= FGrid.RowCount) then EXIT;
  Result:= PLogLine(FGrid.Objects[0, Row]);
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

  { Header row - let the grid's default drawing paint the captions from Cells[0,0]/Cells[1,0]
    (set by resizeColumns). Drawing them again here with Canvas.TextRect at (Left+5, Top+2)
    produced a visible duplicate/ghost overlapping the default-drawn text, especially with
    VCL skins that paint a themed header background. }
  if ARow = 0
  then EXIT;

  { Data rows - read the cached PLogLine pointer.
    Set by refreshVisibleSlice for every visible row, NIL otherwise. }
  CurLine:= PLogLine(FGrid.Objects[0, ARow]);
  if CurLine = NIL then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  { Build cell text based on column }
  if FGrid.ColCount = 2
  then
    case ACol of
      0: begin
           s:= '';
           if ShowDate then s:= DateToStr(CurLine.Time);
           if ShowTime then s:= s + ' ' + FormatDateTime('hh:nn', CurLine.Time);
         end;
      1: s:= CurLine.Msg;
    else
      raise Exception.Create('GridDrawCell: Invalid column index');
    end
  else
    s:= CurLine.Msg;

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


{ Scrolls the view to show the most recent log entries (bottom of the log). }
procedure TLogViewer.scrollToBottom;
VAR VisibleRows: Integer;
begin
  VisibleRows:= FGrid.RowCount - HeaderOverhead;
  if FFilteredRowCount > VisibleRows
  then FScrollBar.Position:= FFilteredRowCount - VisibleRows
  else FScrollBar.Position:= 0;
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
   Ensures the form is visible, restored (not minimized), and brought to front.
   Also bails out if the host form is in the process of being destroyed (see FFormDestroying).
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
         sTime:= '';
         if ShowDate then sTime:= DateToStr(Now);
         if ShowTime then sTime:= sTime+ ' '+  FormatDateTime('hh:nn', Now);

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
    setUpRows reads ClientHeight to recompute FGrid.RowCount and FScrollBar.Max, and ends
    by calling refreshVisibleSlice. Without this call, growing the panel would
    leave empty rows at the bottom (RowCount stuck at the old value), and
    shrinking would orphan filled-but-invisible rows. Skip when not yet wired
    (Resize can fire before the constructor finishes assigning FRamLog). }
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


{--------------------------------------------------------------------------------------------------
   Saves the log content to an RTF file with colors preserved.
   Each line is colored according to its verbosity level, and bold lines are formatted accordingly.

   Note: TRichEdit requires a parent window for certain operations.
         We temporarily parent it to Self (the LogViewer panel).

   Thread Safety — two-phase pattern with VALUE copy (not pointer copy):
     Phase 1 copies the data we need (Msg, Level, Bold) out of each PLogLine into
     a local TArray<TRtfLine> under one read lock. The lock is held only for the
     copy (~microseconds for typical logs).
     Phase 2 walks the local array doing the slow TRichEdit work (Win32 SendMessage
     per line). No lock held; the local array owns its data.

   Why we copy values, not pointers:
     PLogLine pointers are owned by FRamLog.Lines. If a worker hits MaxEntries
     during Phase 2 and CheckAndSaveToDisk's overflow path runs SnapshotAndClear,
     it queues a "free the snapshot" closure on the main thread. TRichEdit
     operations (Lines.Add, SaveToFile) can pump messages — including queued
     closures — and the snapshot's PLogLine records would be Disposed mid-walk.
     Copying values up-front decouples Phase 2 from FRamLog's lifetime.

   Cost: ~16 bytes/line struct overhead (string ref + Level + Bold). String content
     is NOT duplicated (Delphi strings are COW; assignment bumps a refcount).
     For a 1M-line log: ~16 MB array overhead + zero extra string content.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.SaveAsRtf(const FullPath: string);
type
  TRtfLine = record
    Msg  : string;
    Level: TLogVerbLvl;
    Bold : Boolean;
  end;
VAR
  RichEdit : TRichEdit;
  IsDark   : Boolean;
  Snapshot : TArray<TRtfLine>;
  SnapCount: Integer;
  i        : Integer;
begin
  if FullPath = ''
  then raise Exception.Create('SaveAsRtf: FullPath parameter cannot be empty');

  if NOT Assigned(FRamLog)
  then raise Exception.Create('SaveAsRtf: RamLog is not assigned');

  IsDark:= NOT IsLightStyleColor(clWindow);

  { Phase 1 — copy values out of each PLogLine under a single read lock.
    Buffer starts at 256 and doubles on overflow — amortized O(N) growth. }
  SnapCount:= 0;
  SetLength(Snapshot, 256);
  FRamLog.Lines.ForEachLocked(procedure (Line: PLogLine)
    begin
      if SnapCount >= Length(Snapshot)
      then SetLength(Snapshot, Length(Snapshot) * 2);
      Snapshot[SnapCount].Msg  := Line.Msg;     { Strings are COW — assignment is a refcount bump, not a copy. }
      Snapshot[SnapCount].Level:= Line.Level;
      Snapshot[SnapCount].Bold := Line.Bold;
      Inc(SnapCount);
    end);

  { Phase 2 — slow Win32 work, lock-free. The local Snapshot owns its data,
    so a concurrent overflow + snapshot-dispose during this loop is harmless. }
  RichEdit:= TRichEdit.Create(Self);
  TRY
    RichEdit.Parent:= Self;        { Required for TRichEdit to work properly }
    RichEdit.Visible:= FALSE;      { Keep it hidden }
    RichEdit.PlainText:= FALSE;

    for i:= 0 to SnapCount - 1 do
      begin
        RichEdit.SelAttributes.Color:= Verbosity2Color(Snapshot[i].Level, IsDark);
        if Snapshot[i].Bold
        then RichEdit.SelAttributes.Style:= [fsBold]
        else RichEdit.SelAttributes.Style:= [];
        RichEdit.Lines.Add(Snapshot[i].Msg);
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


{ Handles scrollbar position changes - re-fills the visible PLogLine slice and repaints. }
procedure TLogViewer.scrollBarChange(Sender: TObject);
begin
  refreshVisibleSlice;
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


{--------------------------------------------------------------------------------------------------
   Refills the grid's visible Object slots with PLogLine pointers from the underlying log.

   Delegates the walk to RamLog.Lines.GetFilteredSlice, which holds a single read lock for the
   entire scan in multithreaded mode -- no per-element lock-acquire tax. The visible slice lives
   directly in the grid's per-cell Objects array, so GridDrawCell reads it in O(1).

   Caller contract:
     - Must run AFTER setUpRows has set FGrid.RowCount and FScrollBar.Max for the current filter.
     - Must run AFTER any change to FScrollBar.Position.
     - Safe to call before HandleAllocated (early-exits).
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.refreshVisibleSlice;
VAR
  Buf: array of PLogLine;
  i, Filled, MaxRows, SliceLen: Integer;
begin
  if NOT HandleAllocated then EXIT;
  MaxRows:= FGrid.RowCount;

  { Clear all visible Object slots first so any failure (or empty log) leaves a clean state. }
  for i:= HeaderOverhead to MaxRows - 1 do
    FGrid.Objects[0, i]:= NIL;

  if NOT Assigned(RamLog) then EXIT;
  if NOT Assigned(RamLog.Lines) then EXIT;

  SliceLen:= MaxRows - HeaderOverhead;
  if SliceLen <= 0 then EXIT;

  SetLength(Buf, SliceLen);
  Filled:= RamLog.Lines.GetFilteredSlice(FVerbosity, FScrollBar.Position, Buf);
  for i:= 0 to Filled - 1 do
    FGrid.Objects[0, HeaderOverhead + i]:= TObject(Buf[i]);
end;


{ THackGrid - Wrapper to expose protected CalcColWidth method }
function THackGrid.CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer;
begin
  Result:= CalcColWidth(ATextLength, ACaption, NIL);
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLogViewer]);
end; 



end.