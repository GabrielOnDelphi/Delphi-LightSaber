UNIT LightFmx.Common.LogViewer;

{=============================================================================================================
   2025.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   A log viewer based on TStringGrid.
   It can easily show up to 1 million entries.
   Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   How to use it
      1. Alone: Drop a TLogViewer on your form and use it to log messages like this:
                LogViewer.ConstructInternalRamLog;
                LogViewer.RamLog.AddError('Something bad happent!');

      2. Application wide logging
           This component can also be used to see messages logged at the application level (see TAppData).
           For this, just call:
                LogViewer.ObserveAppDataLog;
           From now on, all messages sent to AppData, will also be shown in this log viewer:
                RamLog.AddError('Something bad happent!');
           The log window will automatically pop-up when a error is received.

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

  FOwnRamLog:= FALSE;

  FShowTime   := FALSE;
  FShowDate   := FALSE;
  FVerbosity  := lvVerbose;
  FAutoScroll := TRUE;

  // Apply FMX specific defaults if needed
  Options := Options + [TGridOption.RowSelect]; // Example: enable row selection

  //TStyledGrid.AlternatingBackgroundColor:= True; // Example: make rows easier to see

  // Header settings are often done via Styling in FMX, but basic properties exist
  //HeaderHeight := 25; // Example

  RowHeight   := 22;
  OnDrawColumnCell := MyDrawColumnCell;

  // Call setUpRows initially if you want columns defined at creation
  // setUpRows; // Or call this after RamLog is potentially assigned externally
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

  RowCount:= 0; // The value of RowCount includes the scrollable rows in the grid, but not the fixed row with the headers.
  FFilteredRowCount:= 0;
  //InvalidateContent; // Redraw content area
  FRamLog.Clear;
  // No need to call Populate here, RamLog.Clear triggers NotifyLogObserver which calls Populate.
end;


function TLogViewer.AddColumn(aHeader: string): TStringColumn;
begin
  // When we add a column using AddObject, the grid takes ownership of the column, meaning it will manage the column's lifecycle, including freeing it when the grid is destroyed.
  Result:= TStringColumn.Create(Self);
  try
    Result.Header := aHeader;
    Result.Width  := 100;       // Set the desired width
    Result.Parent := Self;     // Important: Set the parent to the grid
    AddObject(Result);         // Add the column to the grid
  except
    Result.Free; // Free the column if there's an error
    RAISE;
  end;
end;


procedure TLogViewer.RemoveAllColumns;
var i: Integer;
begin
  if ColumnCount > 0 then
    for i:= ColumnCount - 1 downto 0 do
      RemoveObject(Columns[i]);  // We should not manually call Free on the column before removing it from the grid, as this can lead to access violations or other unexpected behavior.
end;



{-------------------------------------------------------------------------------------------------------------
   SETUP & DATA HANDLING
-------------------------------------------------------------------------------------------------------------}
procedure TLogViewer.setUpRows;
VAR RequiredColumnCount: Integer;
begin
  Assert(FRamLog <> nil, 'RamLog not assigned!');     // 1) Ensure RamLog exists

  // Safety check - don't proceed if component isn't fully initialized
  if (csDestroying in ComponentState) or (csLoading in ComponentState) then Exit;

  // Lock updates for performance and visual stability
  BeginUpdate;
  try
    // Recompute filtered rows
    FFilteredRowCount:= FRamLog.Count(FVerbosity > lvDebug, FVerbosity); // Consider caching Filtered flag logic

    // Set RowCount (reserve row 0 for headers)
    RowCount:= FFilteredRowCount + 1; // +1 for the header row

    // Determine required column count
    if FShowDate or FShowTime
    then RequiredColumnCount := 2
    else RequiredColumnCount := 1;

    // Only recreate columns if the count has changed
    //ToDo: don't destroy existing columns. Reused them!
    if ColumnCount <> RequiredColumnCount then
      begin
        // Clear existing columns BEFORE adding new ones
        RemoveAllColumns;

        // Create new columns
        if RequiredColumnCount = 2
        then
          begin
            AddColumn('Time');
            AddColumn('Message').Width:= 150;
          end
        else
          AddColumn('Message').Width:= 250;  //ToDo: I call this way too often. This will result in serious performance penalty!!!
      end;

    // 5) Populate header row text (optional in FMX if using styled headers, but good practice)
    // The TColumn.Header property already sets the header text. No need for Cells[i, 0].

    ResizeColumns;

    // 7) Trigger redraw AFTER EndUpdate
    // Invalidate; // Invalidate is often called implicitly by EndUpdate or RowCount changes

  finally
    EndUpdate;
  end;

  if FAutoScroll
  then ScrollToBottom; //needed?
end;


procedure TLogViewer.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
   begin
     FVerbosity := Value;
     // Recalculate count based on NEW verbosity before potentially calling Populate
     // Note: Populate calls setUpRows which recalculates FFilteredRowCount anyway.
     // FFilteredRowCount := RamLog.Count(FVerbosity > lvDebug, FVerbosity);

     // Update associated UI element if it exists and needs syncing
     if (FVerbTrackBar <> NIL) then
       if (FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity
       then (FVerbTrackBar as TLogVerbFilter).Verbosity := Self.Verbosity;

     // Notify listeners that verbosity changed
     if Assigned(FVerbChanged)
     then FVerbChanged(Self);

     // Refresh the grid content based on the new filter
     Populate; // This will call setUpRows and redraw
   end;
end;


{-------------------------------------------------------------------------------------------------------------
   RAM LOG
-------------------------------------------------------------------------------------------------------------}
// This constructs an RamLog that we can use it internally with this LogViewer.
procedure TLogViewer.ConstructInternalRamLog;
begin
  Assert(FRamLog = NIL, 'TLogViewer.Name='+ Name+' already has a RamLog assigned! Free it before you assign a new one to it!');

  FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver);
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  // Populate the grid with the data from the newly assigned log
  Populate;

  FOwnRamLog:= TRUE;
end;


// This assigns a random RamLog to the LogViewer
procedure TLogViewer.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'External TRamLog not assigned!!');

  // Release owned log if it exists
  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;          // We received the log from an external source. We don't auto release it anymore
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  // Populate the grid with the data from the newly assigned log
  Populate;
end;


// This connects the LogViewer to the RamLog
procedure TLogViewer.ObserveAppDataLog;
begin
  Assert(AppData.RamLog <> NIL, 'AppData.RamLog not assigned!!');
  AssignExternalRamLog(AppData.RamLog);
end;





{-------------------------------------------------------------------------------------------------------------
   CONTENT & DRAWING
-------------------------------------------------------------------------------------------------------------}
procedure TLogViewer.Populate;
begin
  // Ensure RamLog is assigned before proceeding
  if not Assigned(FRamLog) then Exit;

  // Reconfigure rows/columns and update row count based on current filter
  setUpRows;

  // Optional: Force redraw if EndUpdate in setUpRows wasn't sufficient
  // InvalidateContent; // Usually not needed if RowCount changed

  // Scroll to bottom if auto-scroll is enabled
  if AutoScroll
  then scrollToBottom;
end;


{ Returns the content of the specified line, after the grid has been filtered }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
VAR
   actualIndex: Integer;
begin
  Result := NIL; // Default to nil
  if not Assigned(RamLog) or (Row < 0) // Check Row bounds against FILTERED count
  then Exit;

  // Find the index in the *original* list corresponding to the visible filtered row
  actualIndex := RamLog.Lines.Row2FilteredRow(Row, FVerbosity);

  if (actualIndex >= 0)
  and (actualIndex < RamLog.Lines.Count) // Check bounds of original list
  then Result := RamLog.Lines[actualIndex];
end;


procedure TLogViewer.MyDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
                                    const Bounds: TRectF; const Row: Integer; const Value: TValue;
                                    const State: TGridDrawStates);
VAR
   s: string;
   CurLine: PLogLine;
   DrawRect: TRectF; // Use a slightly padded rect for drawing text
begin
  // Row index is 0-based, Row 0 is the header. Data rows start at 1.
  if (Row <= 0)
  OR (Row > FFilteredRowCount)
  OR NOT Assigned(RamLog)
  then EXIT;

  // Get the underlying log line for this *visible* row (adjusting for header)
  CurLine := GetLineFiltered(Row - 1); // Subtract 1 because GetLineFiltered expects 0-based filtered index
  if CurLine = NIL
  then Exit;

  // Determine text based on column index
  case ColumnCount of
   1: if Column.Index = 0 then s:= CurLine.Msg;  // Message column only
   2: case Column.Index of
        0:
          begin                    // Time column
            s:= '';
            if FShowDate then s := DateToStr(CurLine.Time);
            if FShowTime then s := s + ' ' + FormatDateTime('hh:nn', CurLine.Time);
          end;
        1: s:= CurLine.Msg;        // Message column
      end;

    else Exit;   // No columns defined?
  end;

  // --- Custom Drawing ---
  // 1. Set Fill Color (Background) based on verbosity
  Canvas.Fill.Color := Verbosity2Color(CurLine.Level);
  // Optionally fill the background explicitly if default styling doesn't cover it
  // Canvas.FillRect(Bounds, 0, 0, [], 1); // If needed

  // 2. Set Font Style
  if CurLine.Bold
  then Canvas.Font.Style := [TFontStyle.fsBold]
  else Canvas.Font.Style := []; // Clear bold style if not bold

  // 3. Set Font Color (Foreground) - Consider contrast with background
  // Example: Use black/white text based on background brightness
  // if IsColorBright(Canvas.Fill.Color) then Canvas.Font.Color := TAlphaColors.Black else Canvas.Font.Color := TAlphaColors.White;
  //Canvas.Font.Color := TAlphaColors.Black; // Defaulting to black for now

  // 4. Prepare drawing rectangle (optional padding)
  DrawRect:= Bounds;
  DrawRect.Inflate(-2, -1); // Small horizontal/vertical padding

  // 5. Draw the Text
  // TTextAlign.taLeading for left-align, TTextAlign.taCenter for vertical center
  Canvas.FillText(DrawRect, s, FALSE, 1.0, [], TTextAlign.Leading, TTextAlign.Center); // Use DrawRect
end;


{-------------------------------------------------------------------------------------------------------------
   GRID ACCESS & UI Interaction
-------------------------------------------------------------------------------------------------------------}
function TLogViewer.Count: Integer;
begin
  Result:= FFilteredRowCount; // Return the count of currently visible data rows
end;


procedure TLogViewer.scrollToBottom;
var
  TargetY: Single;
begin
  // Ensure RowHeight is positive to avoid division by zero or unexpected behavior
  if (RowCount > 0) and (RowHeight > 0) then
  begin
    // Calculate the Y position of the top of the last row
    TargetY := (RowCount - 1) * RowHeight;

    // Ensure the calculated Y doesn't exceed maximum possible scroll position
    // MaxScrollY = ContentHeight - ViewportHeight
    // ContentHeight = RowCount * RowHeight
    // ViewportHeight = ClientHeight (approximately)
    // TargetY = Max(0, Min(TargetY, RowCount * RowHeight - ClientHeight)); // More precise scrolling

    // Set the vertical viewport position
    ViewportPosition := PointF(ViewportPosition.X, TargetY);
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
      FShowTime := Value;
      Populate;
    end;
end;


{-------------------------------------------------------------------------------------------------------------
   WND / Form Interaction
-------------------------------------------------------------------------------------------------------------}
{ Show the form that owns this control }
procedure TLogViewer.PopUpWindow;
var
  ParentForm: TCommonCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
    begin
      // Show if hidden
      if NOT ParentForm.Visible
      then ParentForm.Show;

      // Restore if minimized
      if ParentForm.WindowState = TWindowState.wsMinimized
      then ParentForm.WindowState := TWindowState.wsNormal;

      // Bring to front
      ParentForm.BringToFront;

      // Optionally focus the grid itself?
      // Self.SetFocus;
    end;
end;


// Helper to estimate scrollbar width (platform dependent)
function GetScrollBarWidth: Single;
begin
  // This is an approximation. Real width depends on style and platform. A more robust way might involve checking style resources.
  // Consider platform specifics if necessary: TPlatformServices.Current.GetPlatformService(...)
  Result:= 18; // Common default width
end;


{-------------------------------------------------------------------------------------------------------------
   COLUMNS
-------------------------------------------------------------------------------------------------------------}
{ Resize column width when the form is resized }
procedure TLogViewer.resizeColumns;
var
  TimeColWidth: Single;
  MsgColWidth: Single;
  TotalWidth: Single;
begin
  TotalWidth := Max(Width - GetScrollBarWidth, 1); // Usable width, account for potential scrollbar

  if ColumnCount = 2
  then
    begin
       // Assign fixed width for time, rest for message
       TimeColWidth := 120; // Adjust as needed for date/time format
       MsgColWidth  := TotalWidth - TimeColWidth;
       if MsgColWidth < 100 then MsgColWidth := 100;  // Ensure message column has a minimum width

       // Apply widths (check if columns exist before accessing)
       if Columns[0] <> nil then Columns[0].Width := TimeColWidth;
       if Columns[1] <> nil then Columns[1].Width := MsgColWidth;
    end
  else
    if ColumnCount = 1 then
      // Single column takes all available width
      if Columns[0] <> NIL
      then Columns[0].Width := TotalWidth;
end;



procedure TLogViewer.Resize;
begin
  inherited Resize;  // Call the inherited method first
  resizeColumns;     // Then adjust columns based on the new size
end;


{-------------------------------------------------------------------------------------------------------------
   TEXT UTILITIES / MISC
-------------------------------------------------------------------------------------------------------------}

procedure TLogViewer.CopyVisible;
VAR
  i: Integer;
  Lines: TStringList;
  CurLine: PLogLine;
begin
  Lines := TStringList.Create; // TStringList is a non-visual component
  try
    Lines.BeginUpdate;
    try
      // This loop iterates through FILTERED rows currently in the grid
      for i := 1 to Lines.Count - 1 do // Data rows are 1 to RowCount-1
        begin
          CurLine := GetLineFiltered(i - 1); // Get data for the visible row
          if CurLine <> NIL
          then Lines.Add(CurLine.Msg);
        end;
    finally
      Lines.EndUpdate;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;


{ Returns all lines, even if a filter is applied }
procedure TLogViewer.CopyAll;
VAR
   ClipboardService: IFMXClipboardService;
   LogText: string;
begin
  // Get text
  if Assigned(FRamLog)
  then LogText := RamLog.GetAsText
  else LogText := 'No RAM log assigned!';

  // Copy to clipboard
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService)
  then ClipboardService.SetClipboard(LogText)
  else messageError('Clipboard service not available.'); // Optional user feedback
end;


procedure TLogViewer.CopyCurLine;
VAR
   ClipboardService: IFMXClipboardService;
   CurLine: PLogLine;
   SelectedRowIndex: Integer;
   LineText: string;
begin
  // FMX TStringGrid uses Selected property for the selected cell/row index
  SelectedRowIndex := Selected; // This is the visual row index (including header)

  // Check if a valid data row is selected (Selected > 0)
  if  (SelectedRowIndex > 0)
  AND (SelectedRowIndex <= FFilteredRowCount) // Use FFilteredRowCount
  then
   begin
      // Get the corresponding log line (adjust for header row)
     CurLine := GetLineFiltered(SelectedRowIndex - 1);
     if CurLine <> NIL then
      begin
        // Construct the string to copy (maybe include time if shown?)
        LineText := '';
        if FShowDate or FShowTime then
          begin
            if FShowDate then LineText := FormatDateTime('yyyy-mm-dd', CurLine.Time);
            if FShowTime then LineText := LineText + ' ' + FormatDateTime('hh:nn:ss.zzz', CurLine.Time);
            LineText := LineText + ': '; // Separator
          end;
        LineText := LineText + CurLine.Msg;

        // Copy to clipboard
        if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService)
        then ClipboardService.SetClipboard(LineText);
      end;
   end;
end;


procedure TLogViewer.RegisterVerbFilter(TrackBar: TFmxObject);
begin
  FVerbTrackBar:= TrackBar as TLogVerbFilter;
  //Update gui?
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



