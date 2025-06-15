{
   I am an experienced Delphi VCL developer but my experience with FMX is low.
   I have this FMX component (see code below).

   Issue:
   I instantiate the component dynamically, at runtime.
   But the component is rendered only as a rectangle. Inside the rectangle I see instead this message:
      "A descendant of, TstyledPresentationProxy has not been registered for lass TLogViewer. Maybe it is necessary to add the FMX.Grid.Style module to the uses section."
   Don't concentrate on the DesignTime part (Register) since I get this message at runtime.
   Think like an experienced Delphi developer.
   Double check your facts.
   Do google searches if necessary.
   Do a good job or else...
}

UNIT LightFmx.lbLogViewer;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX version of TLogGrid, a log viewer based on TStringGrid.
   Drop a TLogGrid on your form and pass its RamLog property to objects needing to log.
   Converted from VCL to FMX for cross-platform support.
   Tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\FMX\FMX_Demo_Log.dpr

=============================================================================================================}

{TODO 5: Sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 5: Let user show/hide grid lines}

INTERFACE

USES
   System.Classes, System.SysUtils, System.Types, System.Rtti, System.UITypes, System.Math,
   FMX.Types, FMX.Controls, FMX.Grid, FMX.Graphics, FMX.Platform, FMX.StdCtrls, FMX.Forms, FMX.Layouts,
   FMX.Presentation.Factory, // Required for TPresentationProxyFactory
   FMX.Presentation.Style,   // Required for TStyledPresentationProxy<> (Likely)
   FMX.Grid.Style,           // Required for TStyledGrid AND already added previously
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
     procedure ResizeColumns;
     procedure scrollToBottom;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  GetLineFiltered(Row: Integer): PLogLine;
     procedure MyDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
   protected
     procedure Resize; override;
//     function GetDefaultStyleLookupName: string; override;
     function DefinePresentationName: string; override;
   public
     constructor Create(AOwner: TComponent); override;
     constructor AssignExternalRamLog(ExternalLog: TRamLog);
     destructor Destroy; override;
     procedure Clear;

     procedure RegisterVerbFilter(TrackBar: TFmxObject); {TLogVerbFilter}
     procedure Populate;
     procedure PopUpWindow;

     procedure setUpRows;
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
     property OnVerbChanged: TNotifyEvent read FVerbChanged write FVerbChanged;   { Triggered before deleting the content of a cell }
  end;

function Verbosity2Color(Verbosity: TLogVerbLvl): TAlphaColor;

procedure Register;



IMPLEMENTATION

USES
  LightCore.Core,
  LightFMX.lbHelpers, LightFMX.lbDialogs, LightFmx.lbLogFilter;

{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwnRamLog := TRUE;
  if FOwnRamLog
  then FRamLog := TRamLog.Create(TRUE, Self as ILogObserver);

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

  RowCount := 1; // Keep header row if applicable
  FFilteredRowCount := 0;
  //InvalidateContent; // Redraw content area
  FRamLog.Clear;
  // No need to call Populate here, RamLog.Clear triggers NotifyLogObserver which calls Populate.
end;


{-------------------------------------------------------------------------------------------------------------
   SETUP & DATA HANDLING
-------------------------------------------------------------------------------------------------------------}
procedure TLogViewer.setUpRows;
var
  col: TStringColumn;
begin
  Assert(FRamLog <> nil, 'RamLog not assigned!');     // 1) Ensure RamLog exists

  // Lock updates for performance and visual stability
  BeginUpdate;
  try

    // Recompute filtered rows
    FFilteredRowCount:= FRamLog.Count(FVerbosity > lvDebug, FVerbosity); // Consider caching Filtered flag logic

    // Set RowCount (reserve row 0 for headers)
    RowCount := FFilteredRowCount + 1; // +1 for the header row

    // Clear existing columns BEFORE adding new ones
    while ColumnCount > 0 do
      Columns[0].Free; // Freeing the first column shifts the next one to index 0

    // Create 1 or 2 columns based on ShowDate/ShowTime
    if FShowDate or FShowTime then
    begin
      // Time Column
      col := TStringColumn.Create(Self);
      col.Header := 'Time';

      // Set reasonable initial width, ResizeColumns will adjust later
      col.Width := 100;
      AddObject(col); // Add column to the grid

      // Message Column
      col := TStringColumn.Create(Self);
      col.Header := 'Message';

      // Let this column take up remaining space initially
      col.Width := 150; // Placeholder, ResizeColumns adjusts
      AddObject(col);
    end
    else
    begin
      // Message Column Only
      col := TStringColumn.Create(Self);
      col.Header := 'Message';
      col.Width := 250; // Placeholder, ResizeColumns adjusts
      AddObject(col);
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
   CONTENT & DRAWING
-------------------------------------------------------------------------------------------------------------}
constructor TLogViewer.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'External TRamLog not assigned!!');

  // Release owned log if it exists
  if FOwnRamLog
  and Assigned(FRamLog)
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;          // We received the log from an external source. We don't auto release it anymore
  FRamLog := ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  // Populate the grid with the data from the newly assigned log
  Populate;
end;


procedure TLogViewer.Populate;
begin
  // Ensure RamLog is assigned before proceeding
  if not Assigned(FRamLog) then Exit;

  // Reconfigure rows/columns and update row count based on current filter
  setUpRows;

  // Optional: Force redraw if EndUpdate in setUpRows wasn't sufficient
  // InvalidateContent; // Usually not needed if RowCount changed

  // Scroll to bottom if auto-scroll is enabled
  if FAutoScroll
  then ScrollToBottom;
end;


{ Returns the content of the specified line, after the grid has been filtered }
function TLogViewer.GetLineFiltered(Row: Integer): PLogLine;
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
  or (Row > FFilteredRowCount)
  or not Assigned(RamLog)
  then EXIT;

  // Get the underlying log line for this *visible* row (adjusting for header)
  CurLine := GetLineFiltered(Row - 1); // Subtract 1 because GetLineFiltered expects 0-based filtered index
  if CurLine = NIL
  then Exit;

  // Determine text based on column index
  if ColumnCount = 2
  then
    if Column.Index = 0 // Time column
    then
     begin
       s := '';
       if FShowDate then s := DateToStr(CurLine.Time);
       if FShowTime then s := s + ' ' + FormatDateTime('hh:nn', CurLine.Time);
      // Handle case where neither is shown (shouldn't happen with ColumnCount=2, but safe)
      if s = '' then s := '?time?';
     end
    else if Column.Index = 1 // Message column
    then s := CurLine.Msg
    else Exit              // Should not happen
  else
  if ColumnCount = 1 // Message column only
  then
     if Column.Index = 0
     then s := CurLine.Msg
     else Exit            // Should not happen
  else Exit;              // No columns defined?

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
  DrawRect := Bounds;
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
  Result := FFilteredRowCount; // Return the count of currently visible data rows
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
      if not ParentForm.Visible
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
  Result := 18; // Common default width
end;


{ Resize column width when the form is resized }
procedure TLogViewer.ResizeColumns;
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
       MsgColWidth := TotalWidth - TimeColWidth;
       // Ensure message column has a minimum width
       if MsgColWidth < 100 then MsgColWidth := 100;

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
  inherited Resize;  // Call inherited FIRST
  ResizeColumns;     // Then adjust columns based on the new size
end;


{-------------------------------------------------------------------------------------------------------------
   UTILITIES / MISC
-------------------------------------------------------------------------------------------------------------}

{ Note: The TrichEdit needs a parent window otherwise we get "EInvalidOperation - Control TRichEdit has no parent window." }
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
        if CurLine <> nil then
        begin
           // Construct line string similarly...
           Lines.Add(CurLine.Msg); // Add message part
        end;
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
  if (SelectedRowIndex > 0) and (SelectedRowIndex <= FFilteredRowCount) // Use FFilteredRowCount
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
  // Using standard TAlphaColors constants
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

  // Register the presentation proxy for TLogGrid, telling FMX to style it using the same proxy as its base class, TStringGrid.
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
  // "grid-style" suffix is exactly what FMX.Grid.Style registered for TStringGrid
  Result := 'grid-' + GetPresentationSuffix;
end;



end.