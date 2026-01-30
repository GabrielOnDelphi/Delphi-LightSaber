UNIT LightVcl.Common.LogViewer;

{=============================================================================================================
   2026.01.29
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
   System.Classes, System.SysUtils, Generics.Collections,
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
	 FFilteredIndices: TList<Integer>; // Caching 
     FFilteredRowCount: Integer;       // Cached count of filtered rows
     FScrollBar: TScrollBar;
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
     procedure FixFixedRow;
     procedure resizeColumns;
     procedure scrollToBottom;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  filteredRow(aRow: Integer): integer;
     function  getLineFiltered(Row: Integer): PLogLine;
     procedure scrollBarChange(Sender: TObject);
     procedure rebuildFilteredIndices;
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

function Verbosity2Color (Verbosity: TLogVerbLvl): TColor;

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
  FFilteredIndices := TList<Integer>.Create;  //Optimize with Caching

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
  then FreeAndNil(FRamLog);

  FreeAndNil(FFilteredIndices);
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

    { Configure scrollbar visibility and range }
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
    FGrid.EndUpdate;
  END;
end;


{ Sets the verbosity filter level and updates the display accordingly. }
procedure TLogViewer.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
    begin
      FVerbosity:= Value;
      rebuildFilteredIndices;

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

  { Release owned log if it exists }
  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;  { We received the log from an external source - don't auto-release it }
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);

  { Populate the grid with the data from the newly assigned log }
  Populate;
end;


{ Refreshes the grid display from the RamLog data.
  Called by the ILogObserver interface when log content changes.
  Rebuilds the filter cache and scrolls to bottom if AutoScroll is enabled. }
procedure TLogViewer.Populate;
begin
  if NOT Assigned(FRamLog) then EXIT;

  rebuildFilteredIndices;
  // Reconfigure rows/columns and update row count based on current filter  
  setUpRows;

  if AutoScroll
  then scrollToBottom;
end;


{ Converts a grid row index to the actual log line index after filtering.
  Accounts for the header row offset (HeaderOverhead). }
function TLogViewer.filteredRow(aRow: Integer): Integer;
begin
  Assert(RamLog <> NIL, 'filteredRow: RamLog not assigned');
  Assert(RamLog.Lines <> NIL, 'filteredRow: RamLog.Lines not assigned');
  Result:= RamLog.Lines.Row2FilteredRow(aRow - HeaderOverhead, FVerbosity);
end;


{ Returns the log line at the specified grid row (after filtering).
  Returns NIL if the row is invalid or out of bounds. }
function TLogViewer.getLineFiltered(Row: Integer): PLogLine;
VAR FilteredIdx: Integer;
begin
  Result:= NIL;
  if (RamLog = NIL) or (RamLog.Lines = NIL) then EXIT;

  FilteredIdx:= filteredRow(Row);
  if (FilteredIdx >= 0) and (FilteredIdx < RamLog.Lines.Count)
  then Result:= RamLog.Lines[FilteredIdx];
end;


{--------------------------------------------------------------------------------------------------
   Custom cell drawing for the log grid.
   Handles header row, applies verbosity-based coloring, and displays date/time if enabled.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.GridDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
VAR
   s: string;
   LogIndex, FilteredIndex: Integer;
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

  { Header row - draw column titles }
  if ARow = 0
  then
    begin
      if FGrid.ColCount = 2
      then
        case ACol of
          0: s:= 'Time';
          1: s:= 'Message';
        end
      else
        s:= 'Message';
      FGrid.Canvas.TextRect(ARect, ARect.Left + 5, ARect.Top + 2, s);
      EXIT;
    end;

  { Data rows - check if we have data to display }
  if (RamLog = NIL) or (FFilteredRowCount = 0) then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  { Calculate log index from scrollbar position }
  LogIndex:= FScrollBar.Position + ARow - HeaderOverhead;
  if (LogIndex < 0) or (LogIndex >= FFilteredIndices.Count) then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  { Map to filtered index }
  FilteredIndex:= FFilteredIndices[LogIndex];
  if FilteredIndex < 0 then
    begin
      FGrid.Canvas.FillRect(ARect);
      EXIT;
    end;

  CurLine:= RamLog.Lines[FilteredIndex];

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
  FGrid.Canvas.Font.Color:= Verbosity2Color(CurLine.Level);
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
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.PopUpWindow;
VAR
  ParentForm: TCustomForm;
begin
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
  inherited Resize;  // Call the inherited method first
  resizeColumns;     // Then adjust columns based on the new size
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
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.SaveAsRtf(const FullPath: string);
VAR
  i: Integer;
  RichEdit: TRichEdit;
  LogLine: PLogLine;
begin
  if FullPath = ''
  then raise Exception.Create('SaveAsRtf: FullPath parameter cannot be empty');

  if NOT Assigned(FRamLog)
  then raise Exception.Create('SaveAsRtf: RamLog is not assigned');

  RichEdit:= TRichEdit.Create(Self);
  TRY
    RichEdit.Parent:= Self;        { Required for TRichEdit to work properly }
    RichEdit.Visible:= FALSE;      { Keep it hidden }
    RichEdit.PlainText:= FALSE;

    for i:= 0 to FRamLog.Lines.Count - 1 do
      begin
        LogLine:= FRamLog.Lines[i];
        RichEdit.SelAttributes.Color:= Verbosity2Color(LogLine.Level);

        if LogLine.Bold
        then RichEdit.SelAttributes.Style:= [fsBold]
        else RichEdit.SelAttributes.Style:= [];

        RichEdit.Lines.Add(LogLine.Msg);
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


{ Handles scrollbar position changes - triggers grid repaint with new visible range. }
procedure TLogViewer.scrollBarChange(Sender: TObject);
begin
  FGrid.InvalidateGrid;
end;


{ Maps a log verbosity level to its display color.
  Higher severity levels use warmer/more alarming colors. }
function Verbosity2Color(Verbosity: TLogVerbLvl): TColor;
begin
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
   Rebuilds the cached list of filtered log line indices.
   Only lines with verbosity level >= FVerbosity are included.
   This cache improves performance by avoiding repeated filtering during grid drawing.
--------------------------------------------------------------------------------------------------}
procedure TLogViewer.rebuildFilteredIndices;
VAR
  i: Integer;
begin
  FFilteredIndices.Clear;
  FFilteredRowCount:= 0;

  if NOT Assigned(RamLog) then EXIT;
  if NOT Assigned(RamLog.Lines) then EXIT;

  for i:= 0 to RamLog.Lines.Count - 1 do
    if RamLog.Lines[i].Level >= FVerbosity
    then FFilteredIndices.Add(i);

  FFilteredRowCount:= FFilteredIndices.Count;
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