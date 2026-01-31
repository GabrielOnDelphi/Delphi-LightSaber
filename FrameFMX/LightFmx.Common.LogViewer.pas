UNIT LightFmx.Common.LogViewer;

{=============================================================================================================
   2026.01.31
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

function Verbosity2Color(Verbosity: TLogVerbLvl): TAlphaColor;

procedure Register;


IMPLEMENTATION

USES
   LightCore, LightCore.Time, LightCore.Types,
   LightFmx.Common.AppData,
   LightFmx.Common.Helpers, LightFmx.Common.Dialogs, LightFmx.Common.LogFilter;



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
  then FreeAndNil(FRamLog);

  inherited;
end;


procedure TLogViewer.Clear;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!');

  RowCount:= 0;
  FFilteredRowCount:= 0;
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
  Do not manually free columns before removing - the grid manages their lifecycle. }
procedure TLogViewer.RemoveAllColumns;
VAR
  i: Integer;
begin
  for i:= ColumnCount - 1 downto 0 do
    RemoveObject(Columns[i]);
end;



{-------------------------------------------------------------------------------------------------------------
   SETUP & DATA HANDLING
-------------------------------------------------------------------------------------------------------------}
{ Configures grid rows and columns based on current RamLog content and verbosity filter.
  In FMX TStringGrid, RowCount only includes data rows - the header row is managed separately. }
procedure TLogViewer.setUpRows;
VAR
  RequiredColumnCount: Integer;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!');

  // Safety check - don't proceed if component isn't fully initialized
  if (csDestroying in ComponentState) OR (csLoading in ComponentState) then EXIT;

  // Lock updates for performance and visual stability
  BeginUpdate;
  try
    // Recompute filtered row count based on current verbosity level
    FFilteredRowCount:= FRamLog.Count(FVerbosity > lvDebug, FVerbosity);

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
  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver);
  FOwnRamLog:= TRUE;

  Populate;
end;


{ Assigns an external RamLog to this viewer.
  The viewer does NOT take ownership - caller is responsible for freeing the log. }
procedure TLogViewer.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'External TRamLog not assigned!');

  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;  // External log - we don't own it
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  Populate;
end;


{ Connects this viewer to AppData's global RamLog for application-wide logging }
procedure TLogViewer.ObserveAppDataLog;
begin
  Assert(AppData.RamLog <> NIL, 'AppData.RamLog not assigned!');
  AssignExternalRamLog(AppData.RamLog);
end;





{-------------------------------------------------------------------------------------------------------------
   CONTENT & DRAWING
-------------------------------------------------------------------------------------------------------------}

{ ILogObserver implementation - called when RamLog content changes }
procedure TLogViewer.Populate;
begin
  if NOT Assigned(FRamLog) then EXIT;

  setUpRows;  // Reconfigures rows/columns based on current filter

  if AutoScroll
  then scrollToBottom;
end;


{ Returns the log line for the specified filtered row index (0-based).
  Maps the visible row index to the actual index in the unfiltered RamLog.Lines list. }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
VAR
   actualIndex: Integer;
begin
  Result:= NIL;
  if NOT Assigned(RamLog) OR (Row < 0)
  then EXIT;

  // Map filtered row index to actual index in the original list
  actualIndex:= RamLog.Lines.Row2FilteredRow(Row, FVerbosity);

  if (actualIndex >= 0) AND (actualIndex < RamLog.Lines.Count)
  then Result:= RamLog.Lines[actualIndex];
end;


{ Custom drawing handler for grid cells.
  In FMX TStringGrid, Row is 0-based and refers to data rows only (header is drawn separately). }
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

  // Set background color based on verbosity level
  Canvas.Fill.Color:= Verbosity2Color(CurLine.Level);

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

{ ILogObserver implementation - shows the form containing this control }
procedure TLogViewer.PopUpWindow;
VAR
  ParentForm: TCommonCustomForm;
begin
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
  Result:= 18;  // Common default width across platforms
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
    else messageError('Clipboard service not available.');
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
  if Assigned(FRamLog)
  then LogText:= RamLog.GetAsText
  else LogText:= 'No RAM log assigned!';

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
  if (SelectedRowIndex >= 0) AND (SelectedRowIndex < FFilteredRowCount)
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


function Verbosity2Color(Verbosity: TLogVerbLvl): TAlphaColor;
begin
  CASE Verbosity of
   lvDebug    : Result := TAlphaColors.Lightgray;
   lvVerbose  : Result := TAlphaColors.Gray;
   lvHints    : Result := TAlphaColors.Darkgray;
   lvInfos    : Result := TAlphaColors.Black; // Text color, background should be lighter? Check MyDrawCell
   lvImportant: Result := TAlphaColors.Orange;
   lvWarnings : Result := TAlphaColors.Darkorange;
   lvErrors   : Result := TAlphaColors.Red;
  else
    // Provide a default or raise a more specific error
    RAISE Exception.CreateFmt('Invalid log verbosity level: %d', [Ord(Verbosity)]);
  end;
  // Note: These colors are used for BACKGROUND in MyDrawColumnCell.
  // Ensure text drawn on top is readable. Consider returning lighter backgrounds.
  // Example Adjustments:
  // lvInfos: TAlphaColors.White or TAlphaColors.Aliceblue
  // lvDebug: TAlphaColors.Whitesmoke
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




