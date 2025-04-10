UNIT cvLog;

{=============================================================================================================
   Gabriel Moraru
   2024.09
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

   The new log (based on TStringGrid)
   Drop a TLogGrid on your form. Pass its RamLog property as reference to all TCube objects when I need to log stuff.
   This component is present in LightBase but it is installed by LightVisControls.

   Hint: http://stackoverflow.com/questions/11719454/why-dont-child-controls-of-a-tstringgrid-work-properly

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

{TODO 5: Sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 5: Let user show/hide grid lines}

INTERFACE

USES
   Winapi.Messages,
   Winapi.Windows,
   System.Classes,  System.SysUtils,
   Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Grids, Vcl.ExtCtrls, VCL.ComCtrls,
   ccLogRam, ccLogUtils, ccLogTypes, ccLogLinesAbstract,
   cbAppDataForm;

TYPE
  TLogGrid = class(TStringGrid, ILogObserver)
   private
     FVerbChanged : TNotifyEvent;
     FVerbosity   : TLogVerbLvl;
     FAutoScroll  : Boolean;        // Autoscroll to bottom
     FShowTime    : Boolean;
     FShowDate    : Boolean;
     FRamLog      : TRamLog;
     FOwnRamLog   : Boolean;
     FVerbTrackBar: TPanel; // TLogVerbFilter
     FFilteredRowCount: Integer;  // Cached value
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
     procedure FixFixedRow;
     procedure ResizeColumns;
     procedure ScrollToBottom;
     procedure setVerbFilter(const Value: TLogVerbLvl);
     function  FilteredRow(aRow: Integer): integer;
     function  GetLineFiltered(Row: Integer): PLogLine;
   protected
     procedure Resize; override;
     procedure WMCommand(var AMessage: TWMCommand); message WM_COMMAND;
     procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
   public
     constructor Create(AOwner: TComponent); override;
     constructor AssignExternalRamLog(ExternalLog: TRamLog);
     destructor Destroy; override;
     procedure Clear;

     procedure RegisterVerbFilter(TrackBar: TPanel{TLogVerbFilter});
     procedure Populate;
     procedure PopUpWindow;
     procedure SaveAsRtf(const FullPath: string);

     procedure setUpRows;
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
   ccCore, ccColors, csSystem, cvLogFilter;



{-------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogGrid.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FOwnRamLog:= TRUE;
 FRamLog:= TRamLog.Create(TRUE, Self as ILogObserver);

 ShowTime        := FALSE;
 ShowDate        := FALSE;
 FVerbosity      := lvVerbose;
 FAutoScroll     := TRUE;

 // TStringGrid initializations
 AutoScroll      := TRUE;
 BevelOuter      := bvNone;
 RowCount        := HeaderOverhead;
 FixedRows       := 0;
 ColCount        := 1;
 FixedCols       := 0;
 DefaultRowHeight:= 22;
 Options         := Options+ [goColSizing, goRowSelect] - [goRangeselect];

 setUpRows;
end;


destructor TLogGrid.Destroy;
begin
  if FOwnRamLog
  then FreeAndNil(FRamLog);

  inherited;
end;


procedure TLogGrid.Clear;
begin
 RowCount:= 1;
 Assert(FRamLog <> NIL, 'RamLog not assigned!');

 FRamLog.Clear;
end;


procedure TLogGrid.setUpRows;
CONST
  LessRam = FALSE;  { Without this we create a row for each line of the Log. This might be ok if the log has 10000 lines but not if it has 10000000 lines }
VAR
  NewRowCount: Integer;
  NewColCount: Integer;
begin
  Assert(FRamLog <> NIL, 'RamLog not assigned!!');

  FFilteredRowCount:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);   //ToDo: cache this value to increase speed!

  if LessRam then
    begin
      {This is if we need to show only what can fit into the screen
       We need to implement an external scrollbar for this }
      NewRowCount := Trunc((ClientHeight / DefaultRowHeight)) - HeaderOverhead;
      if NewRowCount > VisibleRowCount
      then NewRowCount := VisibleRowCount;
    end
  else
    NewRowCount:= Count;

  NewRowCount:= NewRowCount + HeaderOverhead;

  if RowCount <> NewRowCount
  then RowCount:= NewRowCount;

  // Determine the new column count
  if FShowDate or FShowTime
  then NewColCount := 2
  else NewColCount := 1;

  // Update only if changed
  if ColCount <> NewColCount
  then ColCount := NewColCount;

  ResizeColumns;
  FixFixedRow;
  InvalidateGrid; // Mandatory because this will force the grid to paint itself and this is where we read the new information from RAMLog
end;


procedure TLogGrid.setVerbFilter(const Value: TLogVerbLvl);
begin
  //todo: put this back: if FVerbFilter = Value then EXIT;

  FVerbosity:= Value;
  FFilteredRowCount:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);   //ToDo: cache this value to increase speed!

  if (FVerbTrackBar <> NIL)
  AND ((FVerbTrackBar as TLogVerbFilter).Verbosity <> Self.Verbosity)
  then (FVerbTrackBar as TLogVerbFilter).Verbosity:= Self.Verbosity;

  if Assigned(FVerbChanged)
  then FVerbChanged(Self);                                                                  { Let GUI know that the user changed the verbosity }
end;


{-------------------------------------------------------------------------------------------------------------
   CONTENT
-------------------------------------------------------------------------------------------------------------}
constructor TLogGrid.AssignExternalRamLog(ExternalLog: TRamLog);
begin
  Assert(ExternalLog <> NIL, 'RamLog not assigned!!');

  if FOwnRamLog
  then FreeAndNil(FRamLog);

  FOwnRamLog:= FALSE;          // We received the log from an external source. Don't release it anymore
  FRamLog:= ExternalLog;
  FRamLog.RegisterLogObserver(Self as ILogObserver);
end;


procedure TLogGrid.Populate;
begin
  setUpRows;
  if AutoScroll
  then ScrollToBottom;
end;


{ Converts aRow to the real row number (visible on screen) after the filtering has been applied }
function TLogGrid.FilteredRow(aRow: Integer): integer;
begin
  Result:= RamLog.Lines.Row2FilteredRow(aRow- HeaderOverhead, FVerbosity);
end;

{ Returns the content of the specified line, after the grid has been filtered }
function TLogGrid.GetLineFiltered(Row: Integer): PLogLine;
begin
  Result:= RamLog.Lines[FilteredRow(Row)];
end;


procedure TLogGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
VAR
   s: string;
   CurLine: PLogLine;
begin
  if (csDesigning in ComponentState)
  OR (csCreating in ControlState)
  OR (RamLog= NIL)                 // No log assigned
  OR (FFilteredRowCount= 0)        // Log is empty
  OR (ARow = 0)                    // Don't draw in the header
  OR NOT DefaultDrawing then
   begin
    inherited;
    EXIT;
   end;

  if ARow- HeaderOverhead > FFilteredRowCount then
   begin
    inherited;
    EXIT;
   end;

  if FilteredRow(aRow) < 0 then
   begin
    inherited;
    EXIT;
   end;

  CurLine:= GetLineFiltered(aRow);

  if ColCount = 2
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

  Canvas.Font.Color:= Verbosity2Color(CurLine.Level);
  Canvas.TextRect(ARect, ARect.Left+5, ARect.Top+2, s);
end;



{-------------------------------------------------------------------------------------------------------------
   GRID ACCESS
-------------------------------------------------------------------------------------------------------------}
function TLogGrid.Count: Integer;
begin
  Result:= RamLog.Count(FVerbosity > lvDebug, FVerbosity);
end;


procedure TLogGrid.ScrollToBottom;
begin
  if RowCount > VisibleRowCount
  then TopRow := RowCount - VisibleRowCount;
end;


procedure TLogGrid.FixFixedRow;
begin
  if (csCreating in ControlState) then Exit;

  if RowCount > 1
  then FixedRows := 1
  else FixedRows := 0;
end;


procedure TLogGrid.setShowDate(const Value: Boolean);
begin
  if FShowDate <> Value then
  begin
    FShowDate := Value;
    setUpRows;
  end;
end;


procedure TLogGrid.setShowTime(const Value: Boolean);
begin
  if FShowTime <> Value then
  begin
    FShowTime := Value;
    setUpRows;
  end;
end;


{ Returns all lines, even if a filter is applied }
procedure TLogGrid.CopyAll;
begin
  csSystem.StringToClipboard(RamLog.GetAsText);
end;


procedure TLogGrid.CopyCurLine;
begin
  csSystem.StringToClipboard(GetLineFiltered(Row).Msg);
end;


{-------------------------------------------------------------------------------------------------------------
   WND
-------------------------------------------------------------------------------------------------------------}
{ Allows the 'click' action to reach the Button }
procedure TLogGrid.WMCommand(var AMessage: TWMCommand);
begin
  if EditorMode AND (AMessage.Ctl = InplaceEditor.Handle)
  then inherited
  else
    if AMessage.Ctl <> 0
    then AMessage.Result := SendMessage(AMessage.Ctl, CN_COMMAND, TMessage(AMessage).WParam, TMessage(AMessage).LParam);
end;


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


{ Resize column width when the form is resized }
procedure TLogGrid.ResizeColumns;
var
  sTime: string;
  ScrollBarWidth: Integer;
begin
  if ScrollBars in [ssVertical, ssBoth]
  then ScrollBarWidth := GetSystemMetrics(SM_CXVSCROLL)
  else ScrollBarWidth := 0;

  // Adjust column widths here if necessary
  if ColCount = 2 then
   begin
     Cells[0, 0] := 'Time';
     Cells[1, 0] := 'Message';

     // Adjust as needed for date/time column
     sTime:= '';
     if ShowDate then sTime:= DateToStr(Now);
     if ShowTime then sTime:= sTime+ ' '+  FormatDateTime('hh:nn', Now);

     {del
     ColWdth:= 0;
     if ShowDate then ColWdth:= ColWdth+ 75;
     if ShowTime then ColWdth:= ColWdth+ 45; }
     ColWidths[0]:= CalcColWidth(Length(sTime), sTime, NIL);

     ColWidths[1]:= ClientWidth -ColWidths[0] -ScrollBarWidth;
   end
  else
   begin
     Cells[0, 0] := 'Message';
     ColWidths[0]:= ClientWidth -ScrollBarWidth; // Full width for single column
   end;
end;


procedure TLogGrid.Resize;
begin
  inherited Resize;  // Call the inherited method first
  ResizeColumns;
end;











{ Save as text with colors }
{ Note: The TrichEdit needs a parent window otherwise we get "EInvalidOperation - Control TRichEdit has no parent window." }
procedure TLogGrid.SaveAsRtf(const FullPath: string);
var
  i: Integer;
  RichEdit: TRichEdit;
begin
  RichEdit := TRichEdit.Create(Self);
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


function Verbosity2Color(Verbosity: TLogVerbLvl): TColor;
begin
 CASE Verbosity of
  lvDebug    : Result:= clSilverLight;
  lvVerbose  : Result:= clSilverDark;
  lvHints    : Result:= clGray;
  lvInfos    : Result:= clBlack;
  lvImportant: Result:= clOrangeDk;
  lvWarnings : Result:= clOrange;
  lvErrors   : Result:= clRed;
 else
   RAISE Exception.Create('Invalid log verbosity!');
 end;
end;



procedure Register;
begin
  RegisterComponents('LightSaber', [TLogGrid]);
end;

end.
