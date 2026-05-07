UNIT LightFmx.Common.LogViewer;

{=============================================================================================================
   2026.05.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A log viewer based on TStringGrid.
   It can easily show up to 1 million entries.
   Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   How to use it
      1. Standalone: Drop a TLogViewer on your form and use it to log messages:
                LogViewer.RamLog.AddError('Something bad happened!');

      2. Application-wide logging:
           Connect to AppData's log to show application-level messages:
                LogViewer.ObserveAppDataLog;
           Now all messages sent to AppData.RamLog will appear in this viewer:
                AppData.RamLog.AddError('Something bad happened!');
           The log window will automatically pop-up when an error is received.

   FMX Grid Notes:
      - In FMX TStringGrid, RowCount does NOT include the header row (unlike VCL)
      - OnDrawColumnCell Row parameter is 0-based for data rows
      - Column headers are managed separately via TColumn.Header property

   Full demo in:
      c:\Projects\LightSaber\Demo\Demo LightLog\FMX\FMX_Demo_Log.dpr

=============================================================================================================}

{TODO 5: Let user sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 5: Let user show/hide grid lines}

// IMPORTANT! In FMX, if we create subcomponents dynamically, they must be set like this:
// Sub.Locked:= TRUE;
// Sub.Stored:= FALSE:

INTERFACE

USES
   System.Classes, System.SysUtils, System.Types, System.Rtti, System.UITypes, System.Math,
   FMX.Types, FMX.Controls, FMX.Grid, FMX.Graphics, FMX.Platform, FMX.StdCtrls, FMX.Forms, FMX.Layouts, FMX.Presentation.Factory, FMX.Presentation.Style, FMX.Grid.Style,
   LightCore.LogRam, LightCore.LogTypes, LightCore.LogLinesAbstract;

TYPE
   TLogViewer = class(FMX.Grid.TStringGrid, ILogObserver)
   private
     FVerbChanged : TNotifyEvent;
     FVerbosity   : TLogVerbLvl;
     FAutoScroll  : Boolean;        // Autoscroll to bottom
     FShowTime    : Boolean;
     FShowDate    : Boolean;
     FRamLog      : TRamLog;
     FVerbTrackBar: TFmxObject;     // TLogVerbFilter
     FOwnRamLog   : Boolean;        // Frees RamLog if owned
     FFilteredRowCount: Integer;    // Cached count of filtered rows
     FIsDarkCache  : Boolean;       // Cached IsDarkStyle result, refreshed once per setUpRows. Avoids querying TStyleManager per cell during paint.
     FVisibleLines : TArray<PLogLine>;  // Snapshot of PLogLine pointers for the current filter, populated by setUpRows.
                                        // Why this exists:
                                        //   FMX OnDrawColumnCell fires per cell during paint, on the main thread.
                                        //   The previous implementation called Row2FilteredRow + Lines.Count + Lines[i]
                                        //   per draw — three separate read-lock acquisitions per cell, plus a TOCTOU
                                        //   window in MT mode where a worker Add could shift indices between the
                                        //   Row2FilteredRow lookup and the indexed read.
                                        //   Snapshotting under one lock in setUpRows lets MyDrawColumnCell run lock-free.
                                        // Lifetime: pointers are owned by RamLog.Lines. They become dangling if
                                        //   MaxEntries overflow runs SnapshotAndClear; CheckAndSaveToDisk hands the
                                        //   snapshot to the queued Populate closure so this array is refreshed
                                        //   before the snapshot's PLogLine records are disposed (see LightCore.LogRam.pas).
     FFormDestroying: Boolean;      // Set TRUE in TfrmRamLog.FormDestroy so already-queued
                                    // TThread.Queue closures (posted by background-thread
                                    // log appends) bail out instead of touching a freed viewer.
                                    // Distinct from TComponent.Destroying (which only flips
                                    // once we are inside our own destructor — too late for
                                    // a closure that has already entered Populate).
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
     procedure resizeColumns;
     procedure scrollToBottom;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  getLineFiltered(Row: Integer): PLogLine;
     procedure MyDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
     function  AddColumn(aHeader: string): TStringColumn;
    procedure RemoveAllColumns;
   protected
     procedure Resize; override;
     procedure setUpRows;
     function DefinePresentationName: string; override;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Clear;

     // Log
     procedure RegisterVerbFilter(TrackBar: TFmxObject); {TLogVerbFilter}
     procedure ConstructInternalRamLog;
     procedure AssignExternalRamLog(ExternalLog: TRamLog);
     procedure ObserveAppDataLog;
     property  RamLog: TRamLog read FRamLog;

     procedure Populate;
     procedure PopUpWindow;
     property  FormDestroying: Boolean read FFormDestroying write FFormDestroying;

     function  Count: Integer;
     procedure CopyAll;
     procedure CopyCurLine;
     procedure CopyVisible;
   published
     property ShowTime     : Boolean      read FShowTime    write setShowTime default FALSE;
     property ShowDate     : Boolean      read FShowDate    write setShowDate default FALSE;
     property AutoScroll   : Boolean      read FAutoScroll  write FAutoScroll default TRUE;

     property Verbosity    : TLogVerbLvl  read FVerbosity   write SetVerbFilter default lvVerbose;
     property OnVerbChanged: TNotifyEvent read FVerbChanged write FVerbChanged;   { Triggered before deleting the content of a cell }
  end;

function Verbosity2Color(Verbosity: TLogVerbLvl; IsDark: Boolean = False): TAlphaColor;

procedure Register;


IMPLEMENTATION

USES
   LightCore, LightCore.Time, LightCore.Types,
   LightFmx.Common.AppData,
   LightFmx.Common.Helpers, LightFmx.Common.Dialogs, LightFmx.Common.LogFilter, LightFmx.Common.Styles;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Create internal RamLog by default so "drop and use" works immediately }
  FOwnRamLog:= TRUE;
  FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver);

  FShowTime:= FALSE;
  FShowDate:= FALSE;
  FVerbosity:= lvVerbose;
  FAutoScroll:= TRUE;
  FFilteredRowCount:= 0;

  // Enable row selection by default
  Options:= Options + [TGridOption.RowSelect];

  RowHeight:= 22;
  OnDrawColumnCell:= MyDrawColumnCell;

  // setUpRows is called via Populate when RamLog notifies us
end;


destructor TLogViewer.Destroy;
begin
  if FOwnRamLog
  then FreeAndNil(FRamLog)
  else
    // External log: unregister observer so the log doesn't callback into freed memory
    if Assigned(FRamLog)
    then FRamLog.UnregisterLogObserver;

  inherited;
end;


procedure TLogViewer.Clear;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!');

  RowCount:= 0;
  FFilteredRowCount:= 0;
  SetLength(FVisibleLines, 0);   // Drop dangling pointers; will be repopulated by the Populate callback below.
  FRamLog.Clear;
  // No need to call Populate - RamLog.Clear triggers NotifyLogObserver which calls Populate
end;


{ Creates and adds a new string column to the grid.
  The grid takes ownership of the column and will free it on destruction. }
function TLogViewer.AddColumn(aHeader: string): TStringColumn;
begin
  Result:= TStringColumn.Create(Self);
  try
    Result.Header:= aHeader;
    Result.Width:= 100;
    Result.Parent:= Self;  // Setting Parent adds the column to the grid
  except
    FreeAndNil(Result);
    RAISE;
  end;
end;


{ Removes all columns from the grid.
  Columns are owned by Self (AddColumn passes Self as Owner), so FreeAndNil is
  the correct release — RemoveObject only detaches the parent link and leaves
  the column object alive on the owner list, leaking one column object per
  filter toggle for the viewer's lifetime. }
procedure TLogViewer.RemoveAllColumns;
VAR i: Integer;
begin
  for i:= ColumnCount - 1 downto 0 do
    begin
      var Col: TColumn;
      Col:= Columns[i];
      FreeAndNil(Col);  // FMX: free auto-detaches from parent list
    end;
end;



{-------------------------------------------------------------------------------------------------------------
   SETUP & DATA HANDLING
-------------------------------------------------------------------------------------------------------------}
{ Configures grid rows and columns based on current RamLog content and verbosity filter.
  In FMX TStringGrid, RowCount only includes data rows - the header row is managed separately.

  Thread-safety strategy:
    Snapshots the filtered PLogLine pointers into FVisibleLines under a single read lock
    (via GetFilteredSlice). MyDrawColumnCell then reads from the cache without locking.
    This eliminates the per-cell TOCTOU window between Row2FilteredRow and Lines[i] that
    the previous implementation had. The cache is refreshed every time Populate runs. }
procedure TLogViewer.setUpRows;
VAR
  RequiredColumnCount: Integer;
  Filled: Integer;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!');

  // Safety check - don't proceed if component isn't fully initialized
  if (csDestroying in ComponentState) OR (csLoading in ComponentState) then EXIT;

  // Lock updates for performance and visual stability
  BeginUpdate;
  try
    // Refresh the dark-mode flag once per Populate. Style changes between Populates
    // won't be picked up until the next setUpRows runs — acceptable; users rarely
    // toggle theme mid-session, and a manual Populate or filter change refreshes it.
    FIsDarkCache:= IsDarkStyle;

    // Snapshot the filtered slice under a single read lock. GetFilteredSlice fills as
    // many pointers as the buffer holds; we size the buffer to the current filtered count.
    // If concurrent Adds arrive between Count and GetFilteredSlice, the snapshot may
    // include slightly fewer entries than the live count — acceptable; the next Populate
    // will pick up the late entries.
    FFilteredRowCount:= FRamLog.Count(FVerbosity > lvDebug, FVerbosity);
    SetLength(FVisibleLines, FFilteredRowCount);
    if FFilteredRowCount > 0
    then begin
      Filled:= FRamLog.Lines.GetFilteredSlice(FVerbosity, 0, FVisibleLines);
      // Trim if the list shrank between the Count() above and GetFilteredSlice (e.g. a
      // concurrent Clear or MaxEntries-overflow SnapshotAndClear ran in between).
      // Filled cannot exceed the buffer length, so we never need to grow.
      if Filled < FFilteredRowCount
      then SetLength(FVisibleLines, Filled);
      FFilteredRowCount:= Filled;
    end;

    // Set RowCount - in FMX this is data rows only, header is separate
    RowCount:= FFilteredRowCount;

    // Determine required column count based on date/time display settings
    if FShowDate OR FShowTime
    then RequiredColumnCount:= 2
    else RequiredColumnCount:= 1;

    // Only recreate columns if the count has changed
    //ToDo: don't destroy existing columns. Reuse them!
    if ColumnCount <> RequiredColumnCount then
      begin
        RemoveAllColumns;

        if RequiredColumnCount = 2
        then
          begin
            AddColumn('Time');
            AddColumn('Message').Width:= 150;
          end
        else
          AddColumn('Message').Width:= 250;  //ToDo: Columns recreated too often. Performance penalty!
      end;

    // Column headers are set via TColumn.Header property in AddColumn
    ResizeColumns;
  finally
    EndUpdate;
  end;

  if FAutoScroll
  then ScrollToBottom;
end;


procedure TLogViewer.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
    begin
      FVerbosity:= Value;


      // Sync associated verbosity trackbar if it exists
      if (FVerbTrackBar <> NIL) then
        if (FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity
        then (FVerbTrackBar as TLogVerbFilter).Verbosity:= Self.Verbosity;

      // Notify listeners that verbosity changed
      if Assigned(FVerbChanged)
      then FVerbChanged(Self);

      // Refresh grid content with new filter (Populate calls setUpRows)
      Populate;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   RAM LOG
-------------------------------------------------------------------------------------------------------------}

{ Creates a new internal RamLog, replacing any existing log assignment.
  Use after AssignExternalRamLog to switch back to an internal log.
  Note: The constructor already creates an internal log, so this is only needed
  if you previously assigned an external log and want to switch back. }
procedure TLogViewer.ConstructInternalRamLog;
begin
  // Detach from prior log before allocating the new one. Mirror AssignExternalRamLog:
  // free if owned, unregister if external. Skipping the unregister branch (the prior
  // bug) leaves the external log holding a dangling observer pointer to this viewer.
  if Assigned(FRamLog) then
    begin
      if FOwnRamLog
      then FreeAndNil(FRamLog)
      else FRamLog.UnregisterLogObserver;
    end;

  FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver);
  FOwnRamLog:= TRUE;

  Populate;
end;


{ Assigns an external RamLog to this viewer.
  The viewer does NOT take ownership - caller is responsible for freeing the log. }
procedure TLogViewer.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'External TRamLog not assigned!');

  // Detach from existing log before switching
  if Assigned(FRamLog) then
    begin
      if FOwnRamLog
      then FreeAndNil(FRamLog)
      else FRamLog.UnregisterLogObserver;  // External log: just unregister, caller owns it
    end;

  FOwnRamLog:= FALSE;  // External log - we don't own it
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  Populate;
end;


{ Connects this viewer to AppData's global RamLog for application-wide logging }
procedure TLogViewer.ObserveAppDataLog;
begin
  Assert(AppData.RamLog <> NIL, 'AppData.RamLog not assigned!!');
  AssignExternalRamLog(AppData.RamLog);
end;





{-------------------------------------------------------------------------------------------------------------
   CONTENT & DRAWING
-------------------------------------------------------------------------------------------------------------}

{ ILogObserver implementation - called when RamLog content changes.
  Guard against already-queued TThread.Queue closures firing after the host form's
  FormDestroy has set FormDestroying:=TRUE — without this guard the closure would
  touch a viewer that Application has just freed. }
procedure TLogViewer.Populate;
begin
  if FFormDestroying OR NOT Assigned(FRamLog) then EXIT;

  setUpRows;  // Reconfigures rows/columns based on current filter

  if AutoScroll
  then scrollToBottom;
end;


{ Returns the log line at the specified filtered row index (0-based).
  Reads from FVisibleLines, the cached PLogLine snapshot populated by setUpRows under
  one read lock. No lock acquisition here — the cache is owned by the main thread.

  Returns NIL if the row is out of bounds, or if FVisibleLines is empty (e.g. before
  the first Populate). }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
begin
  if (Row < 0) OR (Row >= Length(FVisibleLines))
  then Result:= NIL
  else Result:= FVisibleLines[Row];
end;


{ Custom drawing handler for grid cells.
  In FMX TStringGrid, Row is 0-based and refers to data rows only (header is drawn separately).

  Note on Canvas.Fill.Color: in FMX, FillText uses the canvas Fill brush as the *text*
  color. Despite the name, this sets the foreground, not the background — same semantic
  as VCL's Verbosity2Color. }
procedure TLogViewer.MyDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
                                    const Bounds: TRectF; const Row: Integer; const Value: TValue;
                                    const State: TGridDrawStates);
VAR
   s: string;
   CurLine: PLogLine;
   DrawRect: TRectF;
begin
  // In FMX, Row is 0-based for data rows. Valid range is 0 to FFilteredRowCount-1.
  if (Row < 0)
  OR (Row >= FFilteredRowCount)
  OR NOT Assigned(RamLog)
  then EXIT;

  // Get the underlying log line for this visible row (Row is already 0-based)
  CurLine:= GetLineFiltered(Row);
  if CurLine = NIL
  then EXIT;

  // Determine text based on column configuration
  s:= '';
  case ColumnCount of
    1: if Column.Index = 0
       then s:= CurLine.Msg;

    2: case Column.Index of
         0: begin  // Time/Date column
              if FShowDate then s:= DateToStr(CurLine.Time);
              if FShowTime then s:= s + ' ' + FormatDateTime('hh:nn', CurLine.Time);
            end;
         1: s:= CurLine.Msg;
       end;
    else EXIT;
  end;

  // Pick the verbosity color appropriate for the active theme. FIsDarkCache is
  // populated by setUpRows so we don't probe TStyleManager per cell during paint.
  Canvas.Fill.Color:= Verbosity2Color(CurLine.Level, FIsDarkCache);

  // Set font style
  if CurLine.Bold
  then Canvas.Font.Style:= [TFontStyle.fsBold]
  else Canvas.Font.Style:= [];

  // Prepare drawing rectangle with padding
  DrawRect:= Bounds;
  DrawRect.Inflate(-2, -1);

  // Draw the text (left-aligned, vertically centered)
  Canvas.FillText(DrawRect, s, FALSE, 1.0, [], TTextAlign.Leading, TTextAlign.Center);
end;


{-------------------------------------------------------------------------------------------------------------
   GRID ACCESS & UI Interaction
-------------------------------------------------------------------------------------------------------------}
function TLogViewer.Count: Integer;
begin
  Result:= FFilteredRowCount; // Return the count of currently visible data rows
end;


procedure TLogViewer.scrollToBottom;
VAR
  TargetY: Single;
begin
  if (RowCount > 0) AND (RowHeight > 0) then
    begin
      TargetY:= (RowCount - 1) * RowHeight;
      ViewportPosition:= PointF(ViewportPosition.X, TargetY);
    end;
end;


procedure TLogViewer.setShowDate(const Value: Boolean);
begin
  if FShowDate <> Value then
    begin
      FShowDate:= Value;
      Populate;
    end;
end;


procedure TLogViewer.setShowTime(const Value: Boolean);
begin
  if FShowTime <> Value then
    begin
      FShowTime:= Value;
      Populate;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   WND / Form Interaction
-------------------------------------------------------------------------------------------------------------}

{ ILogObserver implementation - shows the form containing this control.
  Bails out if the host form is being destroyed (see FFormDestroying field comment). }
procedure TLogViewer.PopUpWindow;
VAR
  ParentForm: TCommonCustomForm;
begin
  if FFormDestroying then EXIT;

  ParentForm:= GetParentForm(Self);
  if Assigned(ParentForm) then
    begin
      if NOT ParentForm.Visible
      then ParentForm.Show;

      if ParentForm.WindowState = TWindowState.wsMinimized
      then ParentForm.WindowState:= TWindowState.wsNormal;

      ParentForm.BringToFront;
    end;
end;


{ Returns estimated scrollbar width (platform-dependent approximation) }
function GetScrollBarWidth: Single;
begin
  // This is an approximation. Real width depends on style and platform. A more robust way might involve checking style resources.
  // Consider platform specifics if necessary: TPlatformServices.Current.GetPlatformService(...)
  Result:= 18; // Common default width
end;


{-------------------------------------------------------------------------------------------------------------
   COLUMNS
-------------------------------------------------------------------------------------------------------------}

{ Adjusts column widths when the grid is resized }
procedure TLogViewer.resizeColumns;
VAR
  TimeColWidth: Single;
  MsgColWidth: Single;
  TotalWidth: Single;
begin
  TotalWidth:= Max(Width - GetScrollBarWidth, 1);

  if ColumnCount = 2
  then
    begin
       TimeColWidth:= 120;  // Fixed width for date/time
       MsgColWidth:= TotalWidth - TimeColWidth;
       if MsgColWidth < 100
       then MsgColWidth:= 100;  // Minimum message column width

       if Columns[0] <> NIL then Columns[0].Width:= TimeColWidth;
       if Columns[1] <> NIL then Columns[1].Width:= MsgColWidth;
    end
  else
    if ColumnCount = 1 then
      if Columns[0] <> NIL
      then Columns[0].Width:= TotalWidth;
end;



procedure TLogViewer.Resize;
begin
  inherited Resize;  // Call the inherited method first
  resizeColumns;     // Then adjust columns based on the new size
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT UTILITIES / MISC
-------------------------------------------------------------------------------------------------------------}

{ Copies only the visible (filtered) lines to clipboard }
procedure TLogViewer.CopyVisible;
VAR
  i: Integer;
  Lines: TStringList;
  CurLine: PLogLine;
  ClipboardService: IFMXClipboardService;
begin
  Lines:= TStringList.Create;
  try
    Lines.BeginUpdate;
    try
      // This loop iterates through FILTERED rows currently in the grid
      for i:= 0 to FFilteredRowCount - 1 do
        begin
          CurLine:= GetLineFiltered(i);
          if CurLine <> NIL
          then Lines.Add(CurLine.Msg);
        end;
    finally
      Lines.EndUpdate;
    end;

    if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService)
    then ClipboardService.SetClipboard(Lines.Text)
    else MessageError('Clipboard service not available.');
  finally
    FreeAndNil(Lines);
  end;
end;


{ Copies all lines to clipboard, even if a filter is applied }
procedure TLogViewer.CopyAll;
VAR
   ClipboardService: IFMXClipboardService;
   LogText: string;
begin
  // Get text
  if Assigned(FRamLog)
  then LogText:= RamLog.GetAsText
  else LogText:= 'No RAM log assigned!';

  // Copy to clipboard
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService)
  then ClipboardService.SetClipboard(LogText)
  else messageError('Clipboard service not available.');
end;


{ Copies the currently selected line to clipboard, optionally including timestamp }
procedure TLogViewer.CopyCurLine;
VAR
   ClipboardService: IFMXClipboardService;
   CurLine: PLogLine;
   SelectedRowIndex: Integer;
   LineText: string;
begin
  // FMX TStringGrid.Selected is 0-based index for data rows
  SelectedRowIndex:= Selected;

  // Check if a valid data row is selected (0-based, so >= 0)
  if (SelectedRowIndex >= 0)
  AND (SelectedRowIndex < FFilteredRowCount)
  then
    begin
      CurLine:= GetLineFiltered(SelectedRowIndex);
      if CurLine <> NIL then
        begin
          // Build text with optional timestamp
          LineText:= '';
          if FShowDate OR FShowTime then
            begin
              if FShowDate then LineText:= FormatDateTime('yyyy-mm-dd', CurLine.Time);
              if FShowTime then LineText:= LineText + ' ' + FormatDateTime('hh:nn:ss.zzz', CurLine.Time);
              LineText:= LineText + ': ';
            end;
          LineText:= LineText + CurLine.Msg;

          // Copy to clipboard
          if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService)
          then ClipboardService.SetClipboard(LineText);
        end;
    end;
end;


{ Registers a TLogVerbFilter control for bidirectional verbosity synchronization }
procedure TLogViewer.RegisterVerbFilter(TrackBar: TFmxObject);
begin
  FVerbTrackBar:= TrackBar as TLogVerbFilter;
end;


{ Maps a log verbosity level to its display *text* color (used by FillText via Canvas.Fill).
  Mirror of LightVcl.Common.LogViewer's Verbosity2Color — same scheme, FMX color types.

  IsDark: when TRUE, returns lighter shades suitable for dark-theme backgrounds.
  Defaults to FALSE so existing call sites without a theme query still compile and
  produce light-theme colors (the legacy behavior).

  Compiler-checked exhaustive mapping. If a future TLogVerbLvl enum value is added
  without extending these arrays, the compiler flags the array index at build time
  rather than at runtime. Mirrors the array-driven pattern used in
  LightCore.LogTypes.Verbosity2String.

  Raw hex values (not TAlphaColors.X record-constants) keep the typed-const arrays
  initializable at compile time on every supported Delphi version. The values match
  TAlphaColors.(White|Black|Orange|Darkorange|Red). }
const
  VerbColorsDark: array[TLogVerbLvl] of TAlphaColor =
    ($FF909090,                  { lvDebug     — Medium gray (same as light, both themes) }
     $FFB0B0B0,                  { lvVerbose   — Light gray }
     $FFC0C0C0,                  { lvHints     — Silver }
     $FFFFFFFF,                  { lvInfos     — White }
     $FFFFB060,                  { lvImportant — Light orange }
     $FFFFA500,                  { lvWarnings  — Orange }
     $FFFF0000);                 { lvErrors    — Red }

  VerbColorsLight: array[TLogVerbLvl] of TAlphaColor =
    ($FF909090,                  { lvDebug     — Light gray }
     $FF808080,                  { lvVerbose   — Silver }
     $FF707070,                  { lvHints     — Gray }
     $FF000000,                  { lvInfos     — Black }
     $FFFF8C00,                  { lvImportant — Darkorange }
     $FFFFA500,                  { lvWarnings  — Orange }
     $FFFF0000);                 { lvErrors    — Red }

function Verbosity2Color(Verbosity: TLogVerbLvl; IsDark: Boolean): TAlphaColor;
begin
  if IsDark
  then Result:= VerbColorsDark [Verbosity]
  else Result:= VerbColorsLight[Verbosity];
end;


{-------------------------------------------------------------------------------------------------------------
   REGISTRATION
-------------------------------------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TLogViewer]);

  // Register the presentation proxy for TLogViewer, telling FMX to style it using the same proxy as its base class, TStringGrid.
  // TPresentationProxyFactory.Current.Register(TLogViewer, TControlType.Styled, TStyledPresentationProxy<TStyledGrid>);
  // Registration procedure, @Lightfmx@Lblogviewer@Register$qqrv.Register in package LightFmxBase290.bpl raised exception class EPresentationProxy: Presentation Proxy class [TStyledPresentationProxy<FMX.Grid.Style.TStyledGrid>] for this presentation name [LogViewer-style] has already been registered..
end;


{
 GetDefaultStyleLookupName:
 Returns the full style lookup key (e.g., stringgridstyle) used to find style templates in the stylebook.
 Many FMX controls override this to provide a base name.

 By overriding GetDefaultStyleLookupName, the component insists on using "stringgridstyle" (or a variant) regardless of the proxy we registered via DefinePresentationName.
 This name did not match the actual style templates FMX had for TStyledGrid. This is not what we want!

 FMX checks GetDefaultStyleLookupName first. If it returns non-empty, FMX bypasses DefinePresentationName entirely,
 so, your Grid-Styled proxy registration never takes effect.


 The Inheritance Model (Using GetDefaultStyleLookupName)

    This is the most common and intuitive approach. It relies on piggybacking on the ancestor's style registration.
    The logic is as follows: "I am a TLogViewer, and for styling purposes, I want you to treat me exactly like a TStringGrid."

    You accomplish this by overriding the GetDefaultStyleLookupName function:

    This tells the framework that if no other StyleLookup is specified, it should search the style file for a definition named stringgridstyle. Since this is the same name used by TStringGrid, your component will inherit its appearance.

    The Prerequisite: This method has a critical dependency. It only works if the presentation for 'stringgridstyle' has already been registered with the factory. This registration happens in the initialization section of the FMX.Grid.Style unit. Therefore, for this method to succeed at runtime, the application must include FMX.Grid.Style in its uses clause. Without it, the factory has no entry for 'stringgridstyle', and the chain is broken.

function TLogViewer.GetDefaultStyleLookupName: string;
begin
  Result := 'stringgridstyle'; // This is the standard style name for TStringGrid
end; }


{ DefinePresentationName:
  Returns the base presentation name (e.g., Grid-) to which FMX appends a suffix (like Style) to form the actual lookup key (e.g., Grid-Style) for the stylebook. }
function TLogViewer.DefinePresentationName: string;
begin
  Result := 'grid-' + GetPresentationSuffix;  // "grid-style" suffix is exactly what FMX.Grid.Style registered for TStringGrid
end;


end.
