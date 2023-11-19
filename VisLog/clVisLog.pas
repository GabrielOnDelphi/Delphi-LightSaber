UNIT clVisLog;

{=============================================================================================================
  Visual Log
   CubicDesign
   2021.10.23
   See Copyright.txt

   The new log (based on TStringGrid)
   Drop a TVisLog on your form. Pass its RamLog property as reference to all TCube objects when I need to log stuff.

   Hint: http://stackoverflow.com/questions/11719454/why-dont-child-controls-of-a-tstringgrid-work-properly

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

{TODO 3: Sort lines by criticality (all errors together, all warnings together, etc) }
{TODO 3: Let user show/hide grid lines}

INTERFACE

USES
   Winapi.Messages, System.SysUtils, Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, vcl.Forms, Vcl.Grids, Vcl.ExtCtrls,
   clVisLogLines, clVisLogRam, clVisLogUtils;

//WORK IN PROGRESS
//BUG: ScrollBars not working!

TYPE
  // This is just an extention of TStringGrid
  TLogGrid = class(TStringGrid)
   private
     Initialized: Boolean;
     VerbFilter: TLogVerbLvl;
     AutoScroll: Boolean;
     ShowTime  : Boolean;
     ShowDate  : Boolean;
     RamLog    : TVisRamLog;
     procedure FixFixedRow;
   protected
     procedure WMCommand(var AMessage: TWMCommand); message WM_COMMAND;
     procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
   public
     constructor Create(AOwner: TComponent);   override;
     procedure CreateWindowHandle(const Params: TCreateParams); override;
  end;

  //This is the actual Log control
  TVisLog = class(TPanel)
   private
     Initialized: Boolean;
     FVerbFilter: TLogVerbLvl;
     FAutoScroll: Boolean;
     FShowTime  : Boolean;
     FShowDate  : Boolean;
     FLogError  : TNotifyEvent;
     FLogWarn   : TNotifyEvent;
     ScrollBar  : TScrollBar;
     FRamLog    : TVisRamLog;
     procedure BottomRow(CurAvaRow: Integer);
     procedure setShowDate(const Value: Boolean);
     procedure setShowTime(const Value: Boolean);
   protected
     Indent: Integer;                                                                           { Indent new added lines x spaces }
   public
     Grid       : TLogGrid;
     constructor Create(AOwner: TComponent);   override;
     destructor Destroy; override;
     procedure CreateWindowHandle(const Params: TCreateParams); override;
     procedure Clear;
     procedure ContentChanged;
     procedure SetUp;
   published
     property ShowTime   : Boolean      read FShowTime   write setShowTime default FALSE;
     property ShowDate   : Boolean      read FShowDate   write setShowDate default FALSE;
     property AutoScroll : Boolean      read FAutoScroll write FAutoScroll default TRUE;        { Automatically scroll to show the last line }
     property Verbosity  : TLogVerbLvl  read FVerbFilter write FVerbFilter;                     { Filter out all messages BELOW this verbosity level }

     property OnError    : TNotifyEvent read FLogError   write FLogError;                       { Can be used to inform the application to automatically switch to log when an error is listed }
     property OnWarn     : TNotifyEvent read FLogWarn    write FLogWarn;
     property RamLog     : TVisRamLog   read FRamLog;
  end;

procedure Register;

IMPLEMENTATION

USES ccCore;


constructor TVisLog.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);     // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 FRamLog:= TVisRamLog.Create;

 Grid:= TLogGrid.Create(Self);
 Grid.Parent:= Self;
 Grid.RamLog:= FRamLog;

 ShowCaption := FALSE;
 FShowTime   := FALSE;
 FShowDate   := FALSE;
 FVerbFilter := lvVerbose;
 FAutoScroll := TRUE;
 Indent      := 0;                                                                                  { Intend new added lines with x spaces }

 ScrollBar   := TScrollBar.Create(Self);    // Here I can set the parent. ANYWAY, it may not be a good idea to put controls into TStringGrid. It was not designed for that.
 ScrollBar.Parent:= Self;
end;


procedure TVisLog.CreateWindowHandle(const Params: TCreateParams);
begin
 inherited CreateWindowHandle(Params);

 //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
 if NOT Initialized then                   { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;

   ScrollBar.Kind  := sbVertical;
   ScrollBar.Align := alRight;
   ScrollBar.DoubleBuffered := FALSE;      { Fix delphi bug }

   BevelOuter:= bvNone;
   Grid.Align:= alClient;
  // Grid.ScrollBars:= ssNone;
   FRamLog.SetVisLog(Self);                { This must be the last one because it will upgrade the grid. }
  end;
end;


destructor TVisLog.Destroy;
begin
  FreeAndNil(FRamLog);
  inherited;
end;


procedure TVisLog.Clear;
begin
 Grid.RowCount:= 1;

 ScrollBar.Position:= 0;
 ScrollBar.Max:= 0;
 ScrollBar.Visible:= FALSE;

 if RamLog <> NIL
 then RamLog.Clear;
end;






{--------------------------------------------------------------------------------------------------
   CONTENT
--------------------------------------------------------------------------------------------------}
procedure TVisLog.SetUp;

  function computeRowCount: Integer;
  begin
   Result:= Trunc((ClientHeight / (Grid.DefaultRowHeight+1)));  // RoundDown
   if Result > FRamLog.Lines.Count+ HeaderOverhead
   then Result:= FRamLog.Lines.Count+ HeaderOverhead;
  end;

  procedure SetScrollBar;
  VAR Max: Integer;
  begin
   Max:= (FRamLog.Lines.Count+ HeaderOverhead)- Grid.RowCount;
   if Max < 0 then Max:= 0;
   ScrollBar.Max:= Max;
   ScrollBar.Visible:= Max > 0;
  end;

begin
 Assert(FRamLog <> NIL, 'RamLog not assigned!');

 Grid.RowCount:= computeRowCount;
 Grid.ColCount:= 3;
 SetScrollBar;

 Grid.FixFixedRow;
end;



procedure TVisLog.ContentChanged;
begin
 SetUp;
 Grid.InvalidateGrid;
 if AutoScroll
 then BottomRow(Grid.RowCount);
end;


procedure TVisLog.setShowDate(const Value: Boolean);
begin
  FShowDate := Value;
  //ToDo: auto resize first column to fit text inside
  //ToDo: don't show the first column if both ShowTime and ShowDate is false;
  Grid.InvalidateGrid;
end;


procedure TVisLog.setShowTime(const Value: Boolean);
begin
  FShowTime := Value;
  //ToDo: auto resize first column to fit text inside
  //ToDo: don't show the first column if both ShowTime and ShowDate is false;
  Grid.InvalidateGrid;
end;




{--------------------------------------------------------------------------------------------------
   STUFF
--------------------------------------------------------------------------------------------------} {
procedure TVisLog.setRamLog(const Value: TVisRamLog);
begin
 FRamLog := Value;

 if FRamLog <> NIL
 then FRamLog.Log:= Self;
end; }



procedure TVisLog.BottomRow(CurAvaRow: Integer);   { Similar to TopRow. Scroll to the last low }
VAR Rw: Integer;
begin
 Rw:= CurAvaRow- Grid.VisibleRowCount-1;
 if rw < 0
 then ScrollBar.Position:= Grid.FixedRows+1    {  Set TopRow to scroll the rows in the grid so that the row with index TopRow is the first row after the fixed rows  }
 else ScrollBar.Position:= Rw;

 ScrollBar.Position:= 99999;
end;




















{--------------------------------------------------------------------------------------------------
   TLogGrid
--------------------------------------------------------------------------------------------------}

constructor TLogGrid.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);     // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 ShowTime   := FALSE;
 ShowDate   := FALSE;
 VerbFilter := lvVerbose;
end;


//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TLogGrid.CreateWindowHandle(const Params: TCreateParams);
begin
 inherited CreateWindowHandle(Params);

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized     := TRUE;
   AutoScroll      := TRUE;
   BevelOuter      := bvNone;
   FixedCols       := 0;
   RowCount        := HeaderOverhead;
   ColCount        := 3;
   ColWidths[0]    := 110;
   ColWidths[1]    := Width- ColWidths[0]- {ScrollBar.Width}20;
   Cells[0, 0]     := 'Time';
   Cells[1, 0]     := 'Message';
   DefaultRowHeight:= 22; // I get "A call to an OS function failed" error here
   Options         := Options+ [goColSizing, goRowSelect] - [gorangeselect];
   //THIS GENERATE AN EXCEPTION:
   // ScrollBars      := ssNone;
   // Exception Notification - Project raised exception class EOSError with message 'A call to an OS function failed'.
  end;
end;




procedure TLogGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
VAR
   s: string;
   LogLine: PLogLine;
   FilteredRow: integer;
begin
 if (csDesigning in ComponentState)
 OR (csCreating in ControlState)
 OR (RamLog= NIL)                   // No log assigned
 OR (RamLog.Lines.Count= 0)         // Log is empty
 OR (ARow = 0)                       // Don't draw in the header
 OR NOT DefaultDrawing then
  begin
   inherited;
   EXIT;
  end;

 if ARow- HeaderOverhead > RamLog.Lines.Count then
  begin
   inherited;
   EXIT;
  end;

 {TODO: take into account the grid scrollbar }
 FilteredRow:= RamLog.Lines.GetFilteredRow(aRow- HeaderOverhead, VerbFilter);
 if FilteredRow < 0 then
  begin
   inherited;
   EXIT;
  end;
 LogLine:= RamLog.Lines[FilteredRow];

 case ACol of
  0: begin
       s:= '';
       if ShowDate then s:= DateToStr(LogLine.Time);
       if ShowTime then s:= s+ ' '+  FormatDateTime('hh:nn', LogLine.Time);
     end;
  1: s:= LogLine.Msg;
 end;

 Canvas.Font.Color:= Verbosity2Color(LogLine.Level);
 Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, s);
end;


procedure TLogGrid.FixFixedRow;
begin
 if (csCreating in ControlState) then EXIT;

 { Add fixed row }
 if (FixedRows< 1)
 AND (RowCount> 1)
 then FixedRows:= 1;

 { Remove fixed row }
 if (FixedRows> 0)
 AND (RowCount< 2)
 then FixedRows:= 0;
end;



{ Allows the 'click' action to reach the Button }
procedure TLogGrid.WMCommand(var AMessage: TWMCommand);
begin
  if EditorMode AND (AMessage.Ctl = InplaceEditor.Handle)
  then inherited
  else
    if AMessage.Ctl <> 0
    then AMessage.Result := SendMessage(AMessage.Ctl, CN_COMMAND, TMessage(AMessage).WParam, TMessage(AMessage).LParam);
end;



procedure Register;
begin
  RegisterComponents('LightSaber', [TVisLog]);
end;


end.
