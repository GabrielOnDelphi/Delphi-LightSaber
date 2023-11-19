UNIT cvStringGridBase;

{--------------------------------------------------------------------------------------------------
  CubicDesign
  2020-04-10

  Capabilities:

    MouseContentHints     Pop-up a hint showing cell's content.
    MouseDuplicateLine    Duplicate active line (create a new row) when the user Control+Click on grid
    Color
    Selection
    Find
    Move Row/Col
    Delete Row/Col
    Autoresize all columns to fill all available space

   ToDo: Autoresize row height when the font size is changed

--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows, System.SysUtils, Winapi.Messages, Vcl.Grids, Vcl.Forms, System.Classes, Vcl.Graphics, vcl.Controls;      

CONST
   MaxTextInCellBug= 2000;    { Work around this bug: http://stackoverflow.com/questions/30574585/tstringgrid-cannot-display-very-long-6k-strings }
   clHeaderColor   = clWhite;
   clDarkHeader    = TColor($777777);
   GridHeader      = 1;

TYPE
  TSortDirection = (sdZA, sdAZ);

  TBaseStrGrid= class(TStringGrid)
   private
    FHeaderCellSp      : Integer;
    FFileName          : string;
    FDelKeyDeleteRows  : Boolean;
    FDelKeyDelContent  : Boolean;
    FColorCursor       : TColor;
    FColorCursorText   : TColor;
    FColorTextDis      : TColor;
    FColorEnabled      : TColor;
    FOwnObjects        : Boolean;  
    FTextSpacing  : Integer;                                                                            { if true, the object will be freed when its row is deleted }    
    FMouseInsert       : Boolean;
    FContentHints      : Boolean;
    FAutoRowHeight     : Boolean;
    FAllowSort: Boolean;    
    {EVENTS}
    FBeforeDelete      : TNotifyEvent;
    FAfterDelete       : TNotifyEvent;
    FCursorChanged     : TNotifyEvent;

    procedure setAutoRowHgt       (const Value: Boolean);                                                                            { Update row's height based on the size of the current font used }
    procedure setSelColor         (CONST Value: TColor);
    procedure setClrTextDisabled  (CONST Value: TColor);
    procedure setClrTextEnabled   (CONST Value: TColor);
    procedure setCursorTextColor  (CONST Value: TColor);
    procedure setContentHints     (CONST Value: Boolean);
    function  getCount    : Integer;
    procedure doAutoRowHeight;

    procedure SizeLargestColumn;
    
    procedure setCursorPos(CONST Value: Integer);
    function  getCursorPos: Integer;
   protected
    SortDirection: TSortDirection;   
    Initialized: Boolean;
    FLargeColumn: Integer;    
    function  SelectCell(ACol, ARow: Integer): Boolean; override;
    function  FixCursor: Integer;
    procedure DrawCellText(aCol: Integer; aRect: TRect; s: string);
    procedure KeyDown  (var Key: Word; Shift: TShiftState); override;
    procedure DrawChevron(x, y: Integer);
   public
    SortedCol : Integer;
    constructor Create (AOwner: TComponent);       override;
    procedure   CreateWnd;                         override;
    procedure  Clear;    virtual;
    procedure  ClearAll;
    { SCROLL }
    procedure SetProportionalScroll;                                                                       { NETESTATA ! }  { Getting a TScrollbar control to Show a proportional thumb }
    function  IsVerticalScrollVisible: Boolean;
    function  IsHorizontalScrollVisible: Boolean;
    { MOUSE }
    procedure MouseMove         (Shift: TShiftState; X, Y: Integer); override;
    { FIND }
    function  FindNext          (CONST Text: string; CaseSens: boolean): Boolean;                          { Search all cells. Highlight the cell where text was found. If the text could not be found anymore, start from the beginning. }
    function  FindOnRow         (ARow: Integer; CONST Text: string; CaseSens: boolean): Integer;           { intoarce coloana in care a gasit string-ul }
    function  FindOnCol         (CONST AColumn: Integer; CONST Text: string; CaseSens: boolean): integer;  { intoarce linia in care a gasit string-ul }
    function  FindUniqueEntries (CONST ByColumn: integer): Integer;                                        { retruns the number of unique entries on the specified column. For example if that colum 1 contains name of persons, it will return how many uqinue name appears in the table }
    { SELECT }
    procedure SelectAll;
    function  SelCount: Integer;
    function  MoveSelectedUp  : Boolean;                                                                   { Move selected row  up one position. Returns TRUE if was possible to move the row up }
    function  MoveSelectedDown: Boolean;                                                                   { Move selected row dwn one position. Returns TRUE if was possible to move the row dwn }
    procedure MoveCursorDown;
    procedure MoveCursorUp;                                                                                { Move cursor to the previous row (if the up row is not header or -1 ) }
    { ITEMS}
    function  IsEmpty: Boolean;
    function  GetItem   (Index: Integer): TObject; virtual;
    procedure DeleteRow (ARow: Longint);           override;      //reintroduce;
    procedure DeleteCol (ACol: Longint);           virtual;
    procedure DeleteSelectedRows;
    procedure FreeObject(const Line: integer);
    procedure DeleteCurrentRow;                    dynamic;

    procedure InsertRow (BelowRow: Integer);
    procedure InsertColumn(AfterColumn: Integer);
    procedure AddRowAndFocus;
    procedure ClearRow(aRow: Integer);

    { STUFF }
    function  RowIsVisible (aRow: integer): Boolean;
    procedure ShowCellHint (X,Y:Integer);                                                                  { pop-up a hint with the cell's content }
    procedure CMFontChanged(VAR Message: TMessage); message CM_FONTCHANGED;
   // procedure BeginUpdate2;                                                                              { Stop the Grid from updateing itself to avoid flicker }
   // procedure EndUpdate;
    procedure ClearObjAssignment;

    { Resize }
    function  ResizeColText  (ACol: Integer): Integer;
    function  ResizeColHeader(ACol: Integer): Integer;

    procedure ResizeColHeaders;                                                                            { Resize all columns to match the length of the header text }
    procedure ResizeToFitAll;                                                                              { Resize column to match the longes string found in any of its cells }  { Hasn't been tested }  { A resize is also done in HeaderCell }
    {}
    procedure SetBottomRow(aRow: Integer);
    procedure FixFixedRow;
    procedure InvalidateCurRow;
    procedure InvalidateRow(ARow: Longint);
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateGrid;

    procedure FillCell(ACol, ARow: Integer; aColor: TColor);   overload;                                   { Fill color }
    procedure FillCell(Rect: TRect; aColor: TColor; State: TGridDrawState);           overload;
    procedure HeaderCell(ACol: Integer; const Value: string; AutoSize: Boolean= TRUE);                     { Write text to the header (Row 0) and autoresize the column to match the text inside the header }
   published
    { MOUSE }
    property MouseContentHints  : Boolean        read FContentHints   Write SetContentHints      default FALSE;   { pop-up a hint with the cell's content }
    property MouseDuplicateLine : Boolean        read FMouseInsert    Write FMouseInsert         default FALSE;   {TODO: implement this: duplicate active line (create a new row) when the user Control+Click a grid row }
    { COLORS }
    property CursorTextColor    : TColor         read FColorCursorText Write setCursorTextColor  default clHighlightText;
    property ColorTextDisabled  : TColor         read FColorTextDis   Write setClrTextDisabled   default clGrayText;
    property ColorTextEnabled   : TColor         read FColorEnabled   Write setClrTextEnabled    default clBlack;
    property ColorCursor        : TColor         read FColorCursor    Write setSelColor          default clHighlight;
    {}
    property Count              : Integer        read getCount;                                                    { Returns number of valid items (we don't consider the first item which is the header) }
    property DelKeyDeleteRows   : Boolean        read FDelKeyDeleteRows write FDelKeyDeleteRows  default FALSE;    { If true, Del key will delete selected rows. If false it will delete the content of the current cell (but not also if the cell is in header) }
    property DelDeletesContent  : Boolean        read FDelKeyDelContent write FDelKeyDelContent  default FALSE;

    property CursorPosition     : Integer        read getCursorPos    write setCursorPos;   { Indexed in 0 but it never returns 0 because row 0 is the header }
    property HeaderSpacing      : Integer        read FHeaderCellSp   write FHeaderCellSp        default 7;        { Amount of extra pixels to add to the left and to the right of the header text }

    property FileName           : string         read FFileName       write FFileName;
    property OwnObjects         : Boolean        read FOwnObjects     write FOwnObjects          default FALSE;    { if true, the object will be freed when its row is deleted }
    property AllowSort          : Boolean        read FAllowSort      write FAllowSort     default FALSE;
    property TextSpacing        : Integer        read FTextSpacing    write FTextSpacing   default 7;         { Amount of extra pixels to add to the left AND to the right of the header text }
    
    property AutoRowHeight      : Boolean        read FAutoRowHeight  write setAutoRowHgt        default TRUE;     { This will allow the grid to automatically update the row height in order to fit the new font size (when font is updated) }

    property LargeColumn        : Integer        read FLargeColumn    write FLargeColumn;                     { The largest column, from where we "steal" space for the other columns, on autoresize }
    
    { EVENT }
    property OnCurPosChanged    : TNotifyEvent   read FCursorChanged  write FCursorChanged;  { This is triggered only when the user manually positions the cursor (CursorPos:= x) to a new row or when it clicks a new row. It is not triggered when the cursor position is changed because rows were deleted from grid (for example, the cursor was on row 8 and after deletion only 5 rows were left. In this case it is recommended to use SelectCell }
    property OnBeforeDelete     : TNotifyEvent   read FBeforeDelete   write FBeforeDelete;   { Appears before deleting the content of a cell }
    property OnAfterDelete      : TNotifyEvent   read FAfterDelete    write FAfterDelete;
    property OnResize;
   end;


procedure Register;

IMPLEMENTATION

USES cmMath, csSystem, ccCore;

CONST
   MinColWidth= 20;
   MaxColWidth= 300;






constructor TBaseStrGrid.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                  // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 SortedCol        := -1;
 {VCL}
 FileName         := '';
 Width            := 380;
 Height           := 280;
 Anchors          := [akLeft, akTop, akRight, akBottom];
 DoubleBuffered   := TRUE;
 Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goDrawFocusSelected, goColSizing, goRowMoving, goThumbTracking];   { IMPORTANT. BUG: There is a bug in TStringGrid. The first line is fucked when goRowSelect and goEditing are both true. Details: http://stackoverflow.com/questions/24163773/very-odd-behavior-with-tstringgrid-how-to-fix-it }
 ParentShowHint   := FALSE;
 ShowHint         := TRUE;
 FContentHints    := FALSE;
 FMouseInsert     := FALSE;
 FDelKeyDeleteRows:= FALSE;
 FDelKeyDelContent:= FALSE;
 FOwnObjects      := FALSE;
 FHeaderCellSp    := 7;
 FAutoRowHeight   := TRUE;

 {COLORS}
 FColorCursor        := clNavy;
 FColorCursorText    := clYellow;
 FColorTextDis       := clGrayText;
 FColorEnabled       := clBlack;
 
 FTextSpacing     := 7;
 FAutoRowHeight   := FALSE; 
end;


{One of the statements in your constructor requires a valid window
handle to have been created. The window handle isn't created in the
object constructor, it is created "much" later, in Creatend. Move the
offending statement(s) into an overridden Creatend method, like this: }
procedure TBaseStrGrid.CreateWnd;
begin
 inherited CreateWnd;

 if NOT Initialized then  { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;
   DefaultColWidth  := 80;
   DefaultRowHeight := 15;
  end;
end;
//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html








procedure TBaseStrGrid.Clear;
begin
 FileName:= '';
 RowCount:= 1;                                                                                     { Keep the header. Keep the columns }
end;


procedure TBaseStrGrid.ClearAll;
begin
 ColCount := 1;      { ColCount cannot be zero }
 RowCount := 1;
 Rows[0].Clear;   { Clear header }
end;


function TBaseStrGrid.IsEmpty: Boolean;
begin
 Result:= (RowCount<= FixedRows)                                                                   { RowCountEx is indexed in 1 because I don't conside the first item }
       OR ((FixedRows= 0) AND (RowCount= 1));        { I NEED THIS to prevent an error at design time when the grid is set tohave one single row }
end;








{--------------------------------------------------------------------------------------------------
   INVALIDATE
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.InvalidateCell(ACol, ARow: Longint);                                         { Exposing this hidden procedure }
begin
 inherited InvalidateCell(ACol, ARow);
end;


procedure TBaseStrGrid.InvalidateGrid;                                                              { Exposing this hidden procedure }
begin
 inherited InvalidateGrid;
end;


procedure TBaseStrGrid.InvalidateCol(ACol: Longint);                                                { http://wiert.me/2010/01/12/delphi-tcustomgrid-invalidatecol-and-invalidaterow-bug-that-has-been-there-since-at-least-delphi-4h-1/#comment-15163 }
var
  ARect: TGridRect;
  ARow: Integer;
begin
 if NOT HandleAllocated then EXIT;

 ARect.Top := TopRow;                                                                              { bug in VCL: was 0 }
 ARect.Left:= ACol;
 ARect.Bottom := TopRow+VisibleRowCount+1;                                                         { bug in VCL: forgot to add TopRow }
 ARect.Right  := ACol;

 for ARow := ARect.Top to ARect.Bottom DO
   InvalidateCell(ACol, ARow);                                                                     { problem in VCL: TCustomGrid.InvalidateRect is private, so divert to TCustomGrid.InvalidateCell }

 { now take into account the fixed Rows, which the VCL by accident does }
 for ARow := 0 to FixedRows-1 do
   InvalidateCell(ACol, ARow);
end;


procedure TBaseStrGrid.InvalidateRow(ARow: Longint);
VAR
   ARect: TGridRect;
   ACol: Integer;
begin
 if NOT HandleAllocated then Exit;

 ARect.Top  := ARow;
 ARect.Left := LeftCol;                                                                            { bug in VCL: was 0 }
 ARect.Bottom:= ARow;
 ARect.Right := LeftCol+VisibleColCount+1;                                                         { bug in VCL: forgot to add LeftCol }

 for ACol := ARect.Left to ARect.Right DO
   InvalidateCell(ACol, ARow);                                                                     { problem in VCL: TCustomGrid.InvalidateRect is private, so divert to TCustomGrid.InvalidateCell }

 { now take into account the fixed Cols, which the VCL by accident does }
 for ACol := 0 to FixedCols-1 DO
   InvalidateCell(ACol, ARow);
 end;


procedure TBaseStrGrid.InvalidateCurRow;
begin
 InvalidateRow(Row);
end;



procedure TBaseStrGrid.FixFixedRow;
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



{ Write text to the header (Row 0) and autoresize the column to match the text inside the header }
procedure TBaseStrGrid.HeaderCell(ACol: Integer; CONST Value: string; AutoSize: Boolean= TRUE);                             { Set the text in the specified cell, then resize the column to fit the text }
begin
 Assert(acol < colcount, 'Invalid number of columns!');
 Cells[ACol, 0]:= Value;

 if AutoSize
 then ResizeColHeader(ACol);
end;





{--------------------------------------------------------------------------------------------------
   OBJECTS
--------------------------------------------------------------------------------------------------}
function TBaseStrGrid.getItem(Index: Integer): TObject;
begin
 Assert(Index> 0       , 'Index: '+ IntToStr(Index));
 Assert(Index< RowCount, 'Index: '+ IntToStr(Index)+ '. RowCount:'+ IntToStr(RowCount));

 Result:= Objects[0, Index];
end;


procedure TBaseStrGrid.ClearObjAssignment;
VAR ACol, ARow: Integer;
begin
  for ARow:= 0 to RowCount-1 DO
   for ACol:= 0 to ColCount-1 DO
    Objects[ACol, ARow]:= NIL;
end;










{--------------------------------------------------------------------------------------------------
   ITEMS DELETE
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.FreeObject(const Line: integer);
VAR Obj: TObject;
begin
 Obj:= Objects[0, Line];
 FreeAndNil(Obj);
end;


procedure TBaseStrGrid.DeleteSelectedRows;
VAR line: Integer;
begin
 if IsEmpty then EXIT;

 //if Assigned(FBeforeDeleteRow) then FBeforeDeleteRow(Self);

 for line:= Selection.Bottom downto Selection.Top
  DO DeleteRow(line);

 InvalidateGrid;

 //if Assigned(FBeforeDeleteRow) then FBeforeDeleteRow(Self);
end;


procedure TBaseStrGrid.DeleteRow(ARow: Longint);
var SelRow: Integer;
begin
 if OwnObjects then FreeObject(ARow);

 SelRow := Row;                                                                                    { ca sa pun selectia la loc }
 Rows[ARow].Clear;

 if RowCount > FixedRows                                                                           /// if (NOT ForceKeepHeaders)
 then inherited DeleteRow(ARow);

 if (SelRow < RowCount) AND (SelRow > 0)
 then Row:= SelRow;                                                                                { pun selectia la loc }

 FixFixedRow;
end;


procedure TBaseStrGrid.DeleteCurrentRow;
begin                                                                                              { nu sterge primul rand daca e cap de tabel }
 if  (Row>= FixedRows)
 AND (RowCount> 2)
 then DeleteRow(Row)                                                                               { ci doar goleste-i continutul }
 else Rows[Row].Clear;
end;


procedure TBaseStrGrid.DeleteCol(ACol: Longint);
var SelCol: Integer;
begin
 if Objects[ACol, Row]<> NIL then                                                                  { This column has an object associated. therefore, cannot be deleted. }
  begin
   MesajInfo('Cannot delete this column.');
   EXIT;
  end;

 SelCol:= Col;                                                                                     { store selection's position }
 Cols[ACol].Clear;                                                                                 { clear text in column before deleting the column }

 inherited DeleteColumn(ACol);

 if SelCol < ColCount
 then Col:= SelCol;                                                                                { pun selectia la loc }
end;




{--------------------------------------------------------------------------------------------------
   ITEMS ADD
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.InsertColumn(AfterColumn: Integer);
VAR NewColPos: Integer;
begin
 NewColPos:= AfterColumn+1;
 Assert(NewColPos > FixedCols-1);                                                                  {-1 pt ca FixedCols e indexat in 1 }
 ColCount:= ColCount+ 1;                                                                           { The new column will be created at the end of the grid }
 MoveColumn(ColCount- 1, NewColPos);                                                               { Move the new created column at the specified pos }
 Col:= NewColPos;                                                                                  { Select new column }
 Cols[NewColPos].Clear;                                                                            { golesc noua col }
end;


procedure TBaseStrGrid.InsertRow(BelowRow: Integer);
VAR NewRowPos: Integer;
begin
 NewRowPos:= BelowRow+1;
 Assert(NewRowPos > FixedRows-1);                                                                  {-1 pt ca FixedCols e indexat in 1 }

 RowCount:= RowCount+ 1;
 MoveRow(RowCount- 1, NewRowPos);                                                                  { mut ultima linie la cursor }
 Row:= NewRowPos;                                                                                  { mut cursorul pe noua linie }
 Rows[NewRowPos].Clear;                                                                            { golesc noua linie - doar de frumusete }
end;


procedure TBaseStrGrid.AddRowAndFocus;                                                             { add empty row at the end of the grid and focus on it }
begin
 RowCount:= RowCount+ 1;
 Row:= RowCount-1;                                                                                 { move focus to the latest row }
 ClearRow(Row);                                                                                    { This fixes: There is a problem with the grid: when I add a row in a grid and there previously there was another row (now deleted) it will show the data from that (deleted) row }
end;


procedure TBaseStrGrid.ClearRow(aRow: Integer);
VAR aCol: Integer;
begin                                                                                              { move focus to the latest row }
 for ACol:= 0 to ColCount-1 DO
  Cells[ACol, aRow]:= '';
end;


function TBaseStrGrid.getCount: Integer;                                                            { RowCountEx is indexed in 1 because I don't consider the first item }
begin
 Result:= RowCount- FixedRows;
end;


{  Scroll the rows in the grid so that the pecified row is at the top (the first row after the fixed rows) }
procedure TBaseStrGrid.SetBottomRow(aRow: Integer);    { Similar to TopRow }
VAR Rw: Integer;
begin
 Rw:= aRow- VisibleRowCount+1;
 if rw < 0
 then TopRow:= FixedRows+1
 else
   if Rw < rowCount
   then TopRow:= Rw;
end;











{--------------------------------------------------------------------------------------------------
                                KEYBOARD EVENTS
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.KeyDown(VAR Key: Word; Shift: TShiftState);
begin
 if Key = VK_DELETE
 then
   { Delete row }
   if DelKeyDeleteRows
   then DeleteSelectedRows
   else
     if DelDeletesContent AND (Col >= FixedCols) AND (Row >= FixedRows)
     then   { Delete cell content }
       begin
        if Assigned(FBeforeDelete) then FBeforeDelete(Self);
        Cells[Col, Row]:= '';
        if Assigned(FAfterDelete) then FAfterDelete(Self);
        MoveCursorDown;                                                                             { Move to the next row }
       end
     else
 else
   { Copy/Paste }
   if Shift = [ssCtrl]
   then
    case Key of
      86: Cells[Col, Row]:= StringFromClipboard;
      67: StringToClipboard(Cells[Col, Row]);
    end
   else
      inherited KeyDown(Key, Shift);
end;





{--------------------------------------------------------------------------------------------------
                                  MOVE ROWS
--------------------------------------------------------------------------------------------------}
function TBaseStrGrid.MoveSelectedUp: Boolean;                                                      { Move selected row up one position. Returns TRUE if was possible to move the row up }
Begin
 Result:= CursorPosition > Fixedrows;
 if Result
 then MoveRow(CursorPosition, CursorPosition- 1)
 else Bip50;
End;


function TBaseStrGrid.MoveSelectedDown: Boolean;                                                    { Move selected row dwn one position. Returns TRUE if was possible to move the row dwn }
Begin
 Result:= (CursorPosition>= Fixedrows) AND (CursorPosition< RowCount-1);
 if Result
 then MoveRow(CursorPosition, CursorPosition+ 1)
 else Bip50;
End;








{--------------------------------------------------------------------------------------------------
                                      CURSOR
--------------------------------------------------------------------------------------------------}
function TBaseStrGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result:= inherited SelectCell(ACol, ARow);
  if Assigned(FCursorChanged)            // not working. at this point Row still has the old value
  then FCursorChanged(Self);
end;


function TBaseStrGrid.getCursorPos: Integer;
begin
 if NOT (csDesigning in ComponentState) then
  begin
   Assert(NOT IsEmpty, 'TBaseStrGrid.getCursorPos: Grid is empty!');
   Assert(Row > 0, 'getCursorPos: The first row is the header!');
  end;
 Result:= Row;
end;

        
procedure TBaseStrGrid.setCursorPos(CONST Value: Integer);
VAR Sel: TGridRect;
begin
 if (csCreating in ControlState) OR (RowCount= 1) then EXIT;
 Assert(Value > -1, 'TBaseStrGrid.setCursorPos - Row < 0');

 if Value<> Row then
  begin
   { Set the focus rectangle }     { Without this I get a bug: when I add a new item at the bottom and I click the last row, the first column will print the string 'Name' instead of the actual name of the file. }
   Sel:= Selection;
   sel.Top:= Row;
   sel.Bottom:= Row;
   Selection:= Sel;

   {}
   Row:= Value;
   if Assigned(FCursorChanged)
   then FCursorChanged(Self);
  end;
end;


function TBaseStrGrid.FixCursor: Integer;                                                           { is working only if you don't use MultiSelect}
begin
 if (csCreating in ControlState) then EXIT(0);

 Result:= Row;
 if Result<= HeaderRow0
 then
    if NOT IsEmpty
    then
      begin
       Row   := HeaderRow0 +1;                                                                     { pun cursorul pe primul element din lista }
       Result:= HeaderRow0 +1;
      end
    else Result:= 0;                                                                              { Cannot fix cursor }
end;


procedure TBaseStrGrid.MoveCursorDown;                                                             { Move cursor to the next row (if there are any more rows there) }
begin
 if Row< RowCount-1
 then Row:= Row+1;
end;

procedure TBaseStrGrid.MoveCursorUp;                                                               { Move cursor to the previous row (if the up row is not header or -1 ) }
begin
 if (Row> 0) AND (Row >= fixedrows)
 then Row:= Row-1;
end;





{--------------------------------------------------------------------------------------------------
                                   HINT
--------------------------------------------------------------------------------------------------}
VAR
   LastRow, LastCol : Integer;


procedure TBaseStrGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if MouseContentHints then ShowCellHint(X,Y);
end;


procedure TBaseStrGrid.ShowCellHint(X,Y:Integer);
VAR
   CellText: string;
   ACol, ARow : Integer;
begin
  MouseToCell(X, Y, ACol, ARow);
  if   (ACol <> -1)     AND (ARow <> -1)
  AND ((ACol <>LastCol) OR  (ARow <> LastRow)) then
   begin
     CellText := Cells[ACol,ARow];
     if Length(CellText) > MaxTextInCellBug
     then CellText:= system.COPY(CellText, 1, MaxTextInCellBug)+ '...';     { Work around this bug: http://stackoverflow.com/questions/30574585/tstringgrid-cannot-display-very-long-6k-strings }

    Hint:= CellText;
    Application.CancelHint;   // cred ca imi afecteaza cumva DELPHI IDE

    LastCol:= ACol;
    LastRow:= ARow;
   end;
end;


procedure TBaseStrGrid.SetContentHints(CONST Value: Boolean);
begin
 FContentHints:= Value;
 if FContentHints
 then ShowHint:= TRUE
 else Hint:= '';
end;




{--------------------------------------------------------------------------------------------------
                                     FIND
--------------------------------------------------------------------------------------------------}
VAR LastFind: TPoint= (x: 0; y: 0);

function TBaseStrGrid.FindNext(CONST Text: string; CaseSens: boolean): Boolean;     { Search all cells. Highlight the cell where text was found. If the text could not be found anymore, start from the beginning. }
VAR LastCol, Cl, Rw: Integer;
//    FoundAt: TPoint;

   function SearchIt: Boolean;
   begin
    Result:= FALSE;
    if CaseSens
    then
      if Pos(Text, Cells[Cl, Rw]) > 0
      then EXIT(TRUE)
      else
    else
      if PosInsensitive(Text, Cells[Cl, Rw]) > 0
      then EXIT(TRUE);
   end;

begin
 Result:= FALSE;

 { Are we in the last cell? }
 if (Col= ColCount-1) AND (Row= RowCount-1) then
  begin
   Row:= FixedRows+1;
   Col:= FixedCols+1;
  end;

 { Are we in the last column? }
 if Col>= ColCount-1 then
  begin
   Row:= Row+1;
   Col:= 0;
  end;

 LastCol:= Col+1;

 for Rw:= Row to RowCount-1 DO
  for Cl:= LastCol to ColCount-1 DO  { Start the search from the next cell }
   begin
    if SearchIt then
      if SelectCell(Cl, Rw)
      then
       begin
        Row:= Rw;
        Col:= Cl;
        EXIT(TRUE);
       end
      else Continue;   { If cell cannot be selected, move on }
    LastCol:= 0;
   end;

(*
 { I reach the last column? }
 if LastFind.Y= ColCount-1 then
  begin
   inc(LastFind.X);
   LastFind.Y= 0;
  end;

 FoundAt:= FindText( LastFind, Text, CaseSens);

  begin
   Cl:= FindOnRow(Rw);
   if Cl > 0 then
    begin
     Result.X:= Rw;
     Result.Y:= Cl;

     EXIT(TRUE);
    end;
  end;  *)
end;

(*
function TBaseStrGrid.FindText(StartPoint: TPoint; CONST Text: string; CaseSens: Boolean): TPoint;     { Search all cells. Intoarce celula in care a gasit string-ul }
begin
 Result.X:= -1;
 Result.Y:= -1;
 (*
 for Rw:= StartPoint.X to RowCount-1 DO
  for cl:= StartPoint.Y to ColCount-1 DO
   if CaseSens
   then
     begin
      if (Cells[cl, ARow]= Text)
      then EXIT(cl)
     end
   else
     if SameText(Cells[cl, ARow], Text)
     then EXIT(x:= 1);

 for Rw:= StartRow to RowCount-1 DO
  begin
   Cl:= FindOnRow(Rw);
   if Cl > 0 then
    begin
     Result.X:= Rw;
     Result.Y:= Cl;
     EXIT;
    end;
  end;
end;   *)


function TBaseStrGrid.FindOnRow(ARow: Integer; CONST Text: string; CaseSens: boolean): integer;     { intoarce coloana in care a gasit string-ul }
VAR cl: Integer;
begin
 Result:= -1;
 for cl:= 0 TO ColCount-1 DO
  if CaseSens
  then
    begin
     if (Cells[cl, ARow]= Text)
     then EXIT(cl);
    end
  else
   if SameText(Cells[cl, ARow], Text)
   then EXIT(cl);
end;


function TBaseStrGrid.FindOnCol(CONST AColumn: Integer; CONST Text: string; CaseSens: boolean): integer;  { intoarce linia in care a gasit string-ul }
VAR ln: Integer;
begin
 Result:= -1;
 for ln:= 0 TO rowCount-1 DO
  if CaseSens
  then
    begin
     if (Cells[AColumn, ln]= Text)
     then EXIT(ln)
    end
  else
   if SameText(Cells[AColumn, ln], Text)
   then EXIT(ln);
end;


function TBaseStrGrid.FindUniqueEntries(CONST ByColumn: integer): Integer;                          { retruns the number of unique entries on the specified column. For example if that colum 1 contains name of persons, it will return how many uqinue name appears in the table }
VAR rw1, rw2: Integer;
    Unique: Boolean;
begin
 Result:= 0;
 for rw1:= 0 to RowCount-2 DO
  begin
   Unique:= TRUE;
   for rw2:= rw1+ 1 to RowCount-1 DO
    if SameText(Cells[ByColumn, rw1], Cells[ByColumn, rw2]) then
      begin
       Unique:= FALSE;
       Break;
      end;

   if Unique
   then Inc(Result);
  end;
end;






{--------------------------------------------------------------------------------------------------
   SELECTION
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.SelectAll;
VAR sel: TGridRect;
begin
 if goRangeSelect in Options then
  begin
   Sel:= Selection;
   sel.Top:= 1;
   sel.Bottom:= RowCount-1;
   Selection:= sel;
  end;
end;


function TBaseStrGrid.SelCount: Integer;
begin
 Result:= abs(Selection.Bottom- Selection.Top);
end;








{--------------------------------------------------------------------------------------------------
   FONT
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.CMFontChanged(var Message: TMessage);
begin
  inherited;                                                     { let TControl and TWinControl react first }
  if AutoRowHeight                                               { This will allow the grid to automatically update the row height in order to fit the new font size (when font is updated) }
  then doAutoRowHeight;
end;


procedure TBaseStrGrid.setAutoRowHgt(const Value: Boolean);      { Update row's height based on the size of the current font used }
begin
 FAutoRowHeight:= Value;

 if FAutoRowHeight
 then doAutoRowHeight;
end;


procedure TBaseStrGrid.doAutoRowHeight;      { Update row's height based on the size of the current font used }
begin
 if (Parent<> NIL) then                                            { This prevents the 'Control has no Parent window' message that happens during construction IF the control is created as RunTime (not at the design time) }
  begin
   Repaint;                                                      { I need this here in order to let the previous font to paint itself. Else I will not get the correct font height with TextHeight in the line below. For details see: http://stackoverflow.com/questions/7053419/how-to-automatically-resize-tstringgrid-row }
   DefaultRowHeight:= Canvas.TextHeight('X')+ 5;
  end;
end;



{--------------------------------------------------------------------------------------------------
   COLORS
--------------------------------------------------------------------------------------------------}
procedure TBaseStrGrid.setSelColor(const Value: TColor);
begin
 if FColorCursor <> Value then
 begin
   FColorCursor:= Value;
   InvalidateGrid;
 end;
end;


procedure TBaseStrGrid.setClrTextDisabled(const Value: TColor);
begin
 if FColorTextDis <> Value then
 begin
   FColorTextDis := Value;
   InvalidateGrid;
 end;
end;


procedure TBaseStrGrid.SetClrTextEnabled(const Value: TColor);
begin
 if FColorTextDis <> Value then
 begin
   FColorTextDis := Value;
   InvalidateGrid;
 end;
end;



procedure TBaseStrGrid.setCursorTextColor(const Value: TColor);
begin
 if FColorCursorText <> Value then
 begin
   FColorCursorText := Value;
   InvalidateGrid;
 end;
end;




(*
// https://stackoverflow.com/questions/2421646/how-to-change-controls-simultaneously-without-repainting-each-one
procedure TBaseStrGrid.BeginUpdate;  { Stop the Grid from updateing itself to avoid flicker }
begin
 Perform(WM_SETREDRAW, 0, 0);
end;

procedure TBaseStrGrid.EndUpdate;
begin
 Perform(WM_SETREDRAW, 1, 0);
 Invalidate;                         { Now force repaint }
end;
*)










{--------------------------------------------------------------------------------------------------
   SCROLL
--------------------------------------------------------------------------------------------------}

{ Not tested! }
{ Getting a TScrollbar control to Show a proportional thumb }
procedure TBaseStrGrid.SetProportionalScroll;
VAR info: TScrollInfo;
begin
  FillChar(info, SizeOf(info), 0);
  with info do
  begin
    cbsize := SizeOf(info);
    fmask  := SIF_ALL;
    GetScrollInfo(Handle, SB_VERT, info);
    fmask := fmask or SIF_PAGE;
    nPage := 5 * (nmax - nmin) div RowCount;                         // whatever number of cells you consider a "page"
  end;
  SetScrollInfo(Handle, SB_VERT, info, True);
end;


function TBaseStrGrid.IsVerticalScrollVisible: boolean;              { Can't I use ScrollBars= ssVertical or ssBoth ? }
begin
 Result:= (GetWindowlongPtr(Handle, GWL_STYLE) and WS_VSCROLL) <> 0; { getWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
end;


function TBaseStrGrid.IsHorizontalScrollVisible: boolean;            { getWindowLong was replaced with SetWindowLongPtr for 64 bit compatibility. Details: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows }
begin
 Result:= (GetWindowLongPtr(Handle, GWL_STYLE) and WS_HSCROLL) <> 0;
end;






{--------------------------------------------------------------------------------------------------
   RESIZE HEADER
--------------------------------------------------------------------------------------------------}

{ Resize largest column }
procedure TBaseStrGrid.sizeLargestColumn;
VAR NewSize: Integer;
begin
  NewSize:= 0;

  for VAR i:= 0 to ColCount-1 DO
    if i <> LargeColumn
    then NewSize:= NewSize+ ColWidths[i];
                                              //ToDo 1: add support for scrollbar here
  ColWidths[LargeColumn]:= ClientWidth - NewSize;  // expand this cell so that we fills the whole window
end;


{ Resize all columns to match the length of the header text }
procedure TBaseStrGrid.ResizeColHeaders;
VAR i: Integer;
begin
 for i:= 0 to ColCount-1 DO
   ResizeColHeader(i);
end;


{ Resize column to match the length of the header text }
function TBaseStrGrid.ResizeColHeader(aCol: Integer): Integer;
begin
 //if Parent= NIL then EXIT; { This prevents the 'Control has no Parent window' message that happens during construction IF the control is created as RunTime (not at the design time) }

 Result:= Canvas.TextWidth(Cells[ACol, 0]) + (TextSpacing* 2);

 NotHigherThan (Result, MaxColWidth);   { Ensure maxwidth }
 NotSmallerThan(Result, MinColWidth);   { Ensure minwidth }

 ColWidths[ACol]:= Result;
end;

(*
{ Resize column to match the length of the header text }
procedure TBaseStrGrid.ResizeColHeader(aCol: Integer);
VAR Width: Integer;
begin
 //if Parent= NIL then EXIT; { This prevents the 'Control has no Parent window' message that happens during construction IF the control is created as RunTime (not at the design time) }

 Width:= Canvas.TextWidth(Cells[ACol, 0]) + (HeaderSpacing* 2);

 if Width< MinColWidth
 then Width:= MinColWidth                                                                         { Ensure minimum width }
 else
   if Width > MaxColWidth
   then Width:= MaxColWidth;

 ColWidths[ACol]:= Width;
end;         *)



{--------------------------------------------------------------------------------------------------
   RESIZE
   Automatically resize a column to fit the longest string under that column
--------------------------------------------------------------------------------------------------}

{ Resize column to match the longes string found in any of its cells }  { Hasn't been tested }  { A resize is also done in HeaderCell }
procedure TBaseStrGrid.ResizeToFitAll;
begin
 for VAR cl:= 0 to ColCount-1 DO
   if Cells[cl, 0] <> '' then     // Don't resize columns that have an empty header. We need this for columns that show the icons in Daily Overview grid.
    begin
      VAR sHdr := ResizeColHeader(Cl);
      VAR sCell:= ResizeColText(Cl);
      if sHdr > sCell
      then ColWidths[Cl]:= ColWidths[Cl]+ 24;    // Make the column header some pixels larger, to fit the chevron
    end;

 sizeLargestColumn;
end;


{ Resize column to match the longes string found any of its cells. A resize is also done in HeaderCell }
function TBaseStrGrid.ResizeColText(aCol: Integer): Integer;
VAR
   iRow: Integer;
   CurWidth: Integer;
begin
 // if Parent= NIL then EXIT; { This prevents the 'Controlhas no Parent window' message that happens during construction IF the control is created as RunTime (not at the design time) }

 Result:= MinColWidth;
 Canvas.Font.Assign(Font);

  // Check text on each row
 for iRow:= GridHeader to RowCount - 1 do  { We don't include the header in the search }
  begin
   CurWidth:= Canvas.TextWidth(Cells[ACol, iRow]);
   if CurWidth > Result
   then Result:= CurWidth;
  end;

 Result:= Result+ (TextSpacing* 2);     { Spacing }
 NotHigherThan (Result, MaxColWidth);   { Ensure maxwidth }
 NotSmallerThan(Result, MinColWidth);   { Ensure minwidth }

 { Store it }
 if Result > ColWidths[aCol]
 then ColWidths[aCol]:= Result;         { Put the size in Grid also BUT only if the new calculated width is higher than the curent width (in other words: never shrink the col width, only enlarge }
end;

 ///CenterTextAuto(AvaCol);   from avaGrid                { The width changed so we also need to recenter the text }






{ Draw text taking into consideration the alignment }
procedure TBaseStrGrid.DrawCellText(aCol: Longint; aRect: TRect; s: string);
var
  x: Integer;
  sText: string;   // Local copy to improve speed
  TextRect: TRect; // Local copy to improve speed
begin
  TextRect := ARect;
  TextRect.Inflate(-1, -1);

  if NOT TextRect.IsEmpty then
  begin
    sText := s;
    case ColAlignments[ACol] of
       taLeftJustify : x := TextRect.Left + TextSpacing;
       taRightJustify:
              x := TextRect.Right- TextSpacing - Canvas.TextWidth(sText);
       taCenter      : x := TextRect.Left + (TextRect.Width - Canvas.TextWidth(sText)) div 2;
    else
       x := 0;
    end;
    Canvas.TextRect(TextRect, x, TextRect.Top + (TextRect.Height - Canvas.TextHeight(sText)) div 2, sText);
  end;
end;


procedure TBaseStrGrid.DrawChevron(x, y: integer);
begin
  if SortDirection = sdAZ
  then
   begin
    { Draw up arrow }
    Canvas.Pixels[x+2, y+7]:= clHeaderColor;
    Canvas.Pixels[x+3, y+6]:= clHeaderColor;
    Canvas.Pixels[x+4, y+5]:= clHeaderColor;
    Canvas.Pixels[x+5, y+6]:= clHeaderColor;
    Canvas.Pixels[x+6, y+7]:= clHeaderColor;
   end
  else
   begin
    { Draw down arrow }
    Canvas.Pixels[x+2, y+5]:= clHeaderColor;
    Canvas.Pixels[x+3, y+6]:= clHeaderColor;
    Canvas.Pixels[x+4, y+7]:= clHeaderColor;
    Canvas.Pixels[x+5, y+6]:= clHeaderColor;
    Canvas.Pixels[x+6, y+5]:= clHeaderColor;
   end;
end;



{--------------------------------------------------------------------------------------------------
   OTHERS
--------------------------------------------------------------------------------------------------}

procedure TBaseStrGrid.FillCell(ACol, ARow: Integer; aColor: TColor);
VAR Rect: TRect;
begin
 Rect:= CellRect(ACol, ARow);

 Canvas.Brush.Color:= aColor;
 Rect.Left:= Rect.Left-4;
 Canvas.FillRect(Rect);
end;


procedure TBaseStrGrid.FillCell(Rect: TRect; aColor: TColor; State: TGridDrawState);
begin
 if NOT (gdSelected in State) then
   begin
    Canvas.Brush.Color:= aColor;
    Rect.Left:= Rect.Left-4;
    Canvas.FillRect(Rect);
   end;
end;












{--------------------------------------------------------------------------------------------------
   OTHERS
--------------------------------------------------------------------------------------------------}
function TBaseStrGrid.RowIsVisible(aRow: integer): Boolean;
begin
 Result:= (aRow>= TopRow) AND (aRow<= TopRow+ VisibleRowCount);  { Check if correct }
end;





procedure Register;
begin
  RegisterComponents('LightSaber', [TBaseStrGrid]);
end;


end.
