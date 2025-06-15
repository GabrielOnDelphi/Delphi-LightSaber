UNIT LightVcl.Common.LogViewer;

{=============================================================================================================
   2025.05.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   The logging system based on TStringGrid.
   Drop a TLogGrid on your form.
   It can easily show up to 1 million entries. Being a good citizen, when it reaches this number it saves existing data to disk and then clears it from RAM.

   See demo
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

{TODO 5: Sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 5: Let user show/hide grid lines}

INTERFACE

USES
   Winapi.Windows,
   System.Classes, System.SysUtils, Generics.Collections,
   Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Grids, Vcl.ExtCtrls, VCL.ComCtrls,
   LightCore.LogRam, LightCore.LogTypes, LightCore.LogLinesAbstract;

TYPE
  THackGrid = class(TStringGrid)
  public
    function CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer; // TCustomGrid.CalcColWidth is protected!
  end;

  TLogGrid = class(TPanel, ILogObserver)
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
     procedure SetUpRows;
     procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
   public
     constructor Create(AOwner: TComponent); override;
     constructor AssignExternalRamLog(ExternalLog: TRamLog);
     destructor Destroy; override;
     procedure Clear;

     procedure RegisterVerbFilter(TrackBar: TPanel{TLogVerbFilter});
     procedure Populate;
     procedure PopUpWindow;
     procedure ChangeScrollBarVisibility(aVisible: boolean);
     procedure SaveAsRtf(const FullPath: string);

     function  Count: Integer;
     procedure CopyAll;
     procedure CopyCurLine;
   published
     property ShowTime     : Boolean      read FShowTime    write setShowTime default FALSE;
     property ShowDate     : Boolean      read FShowDate    write setShowDate default FALSE;
     property AutoScroll   : Boolean      read FAutoScroll  write FAutoScroll default TRUE;

     property RamLog       : TRamLog      read FRamLog;

     property Verbosity    : TLogVerbLvl  read FVerbosity   write setVerbFilter;
     property OnVerbChanged: TNotifyEvent read FVerbChanged write FVerbChanged;   { Triggered before deleting the content of a cell }
  end;

function Verbosity2Color (Verbosity: TLogVerbLvl): TColor;

procedure Register;


IMPLEMENTATION

USES
   LightCore.Core, LightVcl.Common.Colors, LightVcl.Common.Clipboard, LightVcl.Common.LogFilter;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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


procedure TLogGrid.CreateWnd;
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


destructor TLogGrid.Destroy;
begin
  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FreeAndNil(FFilteredIndices);
  FreeAndNil(Fgrid);
  FreeAndNil(FScrollBar);
  inherited;
end;


procedure TLogGrid.Clear;
begin
  FGrid.RowCount := HeaderOverhead; // Reset to header only
  Assert(FRamLog <> NIL, 'RamLog not assigned!');

  FRamLog.Clear;
  resizeColumns;
end;


procedure TLogGrid.ChangeScrollBarVisibility(aVisible: boolean);
var MustResize: Boolean;
begin
  MustResize:= aVisible <> FScrollBar.Visible;
  FScrollBar.Visible:= aVisible;
  if MustResize
  then resizeColumns;        // Call this only if wthe scroll bar visibility changed (to prevent extra processing)
end;


procedure TLogGrid.setUpRows;
var
  NewColCount, VisibleRows: Integer;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!!');
  if NOT HandleAllocated then EXIT; { We can call setUpRows only after the component has a handle }

  FGrid.BeginUpdate;
  try
    // Row count
    FFilteredRowCount:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);   // cache this value to increase speed!

    VisibleRows:= Trunc((ClientHeight / FGrid.DefaultRowHeight)) - HeaderOverhead;
    if VisibleRows < 1
    then VisibleRows := 1;
    FGrid.RowCount:= VisibleRows + HeaderOverhead;
    FixFixedRow;

    // Column count
    if FShowDate or FShowTime
    then NewColCount := 2
    else NewColCount := 1;
    FGrid.ColCount:= NewColCount;

    // Configure scrollbar
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
    FScrollBar.LargeChange := VisibleRows;

  finally
    FGrid.EndUpdate;
  end;

  //del FGrid.Invalidate; // Trigger repaint
  //del FGrid.InvalidateGrid; // Mandatory because this will force the grid to paint itself and this is where we read the new information from RAMLog
end;


procedure TLogGrid.setVerbFilter(const Value: TLogVerbLvl);
begin
  if FVerbosity <> Value then
  begin
    FVerbosity:= Value;
    //del FFilteredRowCount:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);   //ToDo: cache this value to increase speed!
    rebuildFilteredIndices;

    if (FVerbTrackBar <> NIL)
    AND ((FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity)
    then (FVerbTrackBar as TLogVerbFilter).Verbosity:= Self.Verbosity;

    if Assigned(FVerbChanged)
    then FVerbChanged(Self);            { Let GUI know that the user changed the verbosity }

    setUpRows;
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   CONTENT
-------------------------------------------------------------------------------------------------------------}
constructor TLogGrid.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'RamLog not assigned!!');

  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;          // We received the log from an external source. We don't auto release it anymore
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);
end;


procedure TLogGrid.Populate;
begin
  rebuildFilteredIndices;
  setUpRows;
  if AutoScroll
  then scrollToBottom;
end;


{ Converts aRow to the real row number (visible on screen) after the filtering has been applied }
function TLogGrid.filteredRow(aRow: Integer): integer;
begin
  Result:= RamLog.Lines.Row2FilteredRow(aRow- HeaderOverhead, FVerbosity);
end;


{ Returns the content of the specified line, after the grid has been filtered }
function TLogGrid.getLineFiltered(Row: Integer): PLogLine;
begin
  Result:= RamLog.Lines[filteredRow(Row)];
end;


procedure TLogGrid.GridDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
VAR
   s: string;
   LogIndex, FilteredIndex: Integer;
   CurLine: PLogLine;
begin
  if (csDesigning in ComponentState)
  or (csCreating in ControlState) or
     (RamLog = nil) or (FFilteredRowCount = 0) or (ARow = 0) or not FGrid.DefaultDrawing then
  begin
    FGrid.Canvas.FillRect(ARect); // Clear the cell
    Exit;
  end;

  // Header row
  if ARow = 0 then
  begin
    if ACol = 0
	then S := 'Time'
    else
      if (ACol = 1) and (FGrid.ColCount = 2)
      then S := 'Message';
    FGrid.Canvas.TextRect(ARect, ARect.Left + 5, ARect.Top + 2, S);
    Exit;
  end;

  // Calculate log index from scrollbar position
  LogIndex := FScrollBar.Position + ARow - HeaderOverhead;
  if (LogIndex < 0)
  OR (LogIndex >= FFilteredIndices.Count) then  // use the cache
  begin
    FGrid.Canvas.FillRect(ARect); // Clear if out of bounds
    Exit;
  end;

  // Map to filtered index
  FilteredIndex := FFilteredIndices[LogIndex]; {del RamLog.Lines.Row2FilteredRow(LogIndex, FVerbosity);}
  if FilteredIndex < 0 then
  begin
    FGrid.Canvas.FillRect(ARect);
    EXIT;
  end;

  CurLine := RamLog.Lines[FilteredIndex];

  if FGrid.ColCount = 2
  then
    case ACol of
     0: begin
          s:= '';
          if ShowDate then s:= DateToStr(CurLine.Time);
          if ShowTime then s:= s+ ' '+  FormatDateTime('hh:nn', CurLine.Time);
        end;
     1: s:= CurLine.Msg;
    else
       raise Exception.Create('Invalid ColCount!');
    end
  else
    s:= CurLine.Msg;

  FGrid.Canvas.Font.Color := Verbosity2Color(CurLine.Level);
  FGrid.Canvas.TextRect(ARect, ARect.Left + 5, ARect.Top + 2, S);
end;


{-------------------------------------------------------------------------------------------------------------
   GRID ACCESS
-------------------------------------------------------------------------------------------------------------}
function TLogGrid.Count: Integer;
begin
  Result:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);
end;


procedure TLogGrid.scrollToBottom;
var
  VisibleRows: Integer;
begin
  VisibleRows := FGrid.RowCount - HeaderOverhead;
  if FFilteredRowCount > VisibleRows
  then FScrollBar.Position := FFilteredRowCount - VisibleRows
  else FScrollBar.Position := 0;
end;

procedure TLogGrid.FixFixedRow;
begin
  if (csCreating in ControlState) then Exit;

  if FGrid.RowCount > 1
  then FGrid.FixedRows := 1
  else FGrid.FixedRows := 0;
end;


procedure TLogGrid.setShowDate(const Value: Boolean);
begin
  if FShowDate <> Value then
  begin
    FShowDate := Value;
    setUpRows;
    resizeColumns;
  end;
end;


procedure TLogGrid.setShowTime(const Value: Boolean);
begin
  if FShowTime <> Value then
  begin
    FShowTime := Value;
    setUpRows;
    resizeColumns;
  end;
end;




{-------------------------------------------------------------------------------------------------------------
   WND
-------------------------------------------------------------------------------------------------------------}
{ Show the form that owns this control }
procedure TLogGrid.PopUpWindow;
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    if NOT ParentForm.Visible
    then ParentForm.Show
    else
      if ParentForm.WindowState = wsMinimized
      then ParentForm.WindowState := wsNormal;

    ParentForm.BringToFront;
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   COLUMNS
-------------------------------------------------------------------------------------------------------------}
{ Resize column width when the form is resized }
procedure TLogGrid.resizeColumns;
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


procedure TLogGrid.Resize;
begin
  inherited Resize;  // Call the inherited method first
  resizeColumns;
end;








{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}
{ Returns all lines, even if a filter is applied }
procedure TLogGrid.CopyAll;
begin
  LightVcl.Common.Clipboard.StringToClipboard(RamLog.GetAsText);
end;


procedure TLogGrid.CopyCurLine;
begin
  LightVcl.Common.Clipboard.StringToClipboard(getLineFiltered(FGrid.Row).Msg);
end;


{ Save as text with colors }
{ Note: The TrichEdit needs a parent window otherwise we get "EInvalidOperation - Control TRichEdit has no parent window." }
procedure TLogGrid.SaveAsRtf(const FullPath: string);
VAR
  i: Integer;
  RichEdit: TRichEdit;
begin
  RichEdit:= TRichEdit.Create(Self);
  try
    RichEdit.PlainText := False;

    for i := 0 to FRamLog.Lines.Count - 1 do
      begin
        with FRamLog.Lines[i]^ do
        begin
          RichEdit.SelAttributes.Color := Verbosity2Color(Level);
          RichEdit.SelAttributes.Style := [];
          if Bold
          then RichEdit.SelAttributes.Style := [fsBold];

          RichEdit.Lines.Add(Msg);
        end;
      end;

    RichEdit.Lines.SaveToFile(FullPath);
  finally
    FreeAndNil(RichEdit);
  end;
end;


procedure TLogGrid.RegisterVerbFilter(TrackBar: TPanel);
begin
 // mesaj('Trackbar registered for log');
  FVerbTrackBar:= TrackBar;  // Let the Log know that its verbosity is controlled by this TrackBar
end;


procedure TLogGrid.scrollBarChange(Sender: TObject);
begin
  FGrid.InvalidateGrid; // Redraw with new scrollbar position
  //InvalidateGrid; // Redraw with new data
end;


function Verbosity2Color(Verbosity: TLogVerbLvl): TColor;
begin
 CASE Verbosity of
   lvDebug    : Result:= TColor($909090);
   lvVerbose  : Result:= TColor($808080);    // Silver
   lvHints    : Result:= TColor($707070);    // Gray
   lvInfos    : Result:= clBlack;
   lvImportant: Result:= clOrangeDk;
   lvWarnings : Result:= clOrange;
   lvErrors   : Result:= clRed;
 else
   RAISE Exception.Create('Invalid log verbosity!');
 end;
end;


{ Rebuild the cache when needed }
procedure TLogGrid.rebuildFilteredIndices;
var
  i: Integer;
begin
  FFilteredIndices.Clear;

  for i := 0 to RamLog.Lines.Count - 1 do
    if RamLog.Lines[i].Level >= FVerbosity
    then FFilteredIndices.Add(i);

  FFilteredRowCount:= FFilteredIndices.Count;
end;


{ THackGrid }
function THackGrid.CalculateColWidth(const ATextLength: Integer; const ACaption: string): Integer;
begin
  Result:= CalcColWidth(ATextLength, ACaption, nil)
end;




procedure Register;
begin
  RegisterComponents('LightSaber', [TLogGrid]);
end;



end.