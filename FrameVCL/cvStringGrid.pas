UNIT cvStringGrid;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  See this: http://www.delphiforfun.org/Programs/Delphi_Techniques/GridSort.htm

  Split from cmStringListOwn.pas as it was on 2012.08.15.
  Differences are in the way the Grid manages associated objects:
       cStrGridOwn has a list of objects (Fitems).
       cStrGrid    stores the objects on column zero using the Objects[] property.


  IMPORTANT. BUG:
    There is a bug in TStringGrid. The first line is fucked when goRowSelect and goEditing are both true. Details: http://stackoverflow.com/questions/24163773/very-odd-behavior-with-tstringgrid-how-to-fix-it


  Capabilities:

    OnEndEdit             Allows the user to easily implement a custom grid editor. NOTA: Nu merge daca AlwaysShowEditror= True
    Highlight string      Highlight in Bleo all cell that contains the specified string
    Sort
    Mouse sort             Sort by column when the user Shift+Click on grid's header
    MouseEditHeader       Toggle action. make to top row fixed or normal row every time the user Alt+Click on grid

    I/O:
      Save to disk
      Copy content as CSV
      Copy content to/from clipboard

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.Messages,
   System.SysUtils, System.Classes,
   Vcl.Grids, Vcl.Graphics, Vcl.Controls, Vcl.Clipbrd,
   cvStringGridBase, ccCore, cbDialogs;

{$WARN GARBAGE OFF}   {Silent the: 'W1011 Text after final END' warning }

TYPE
  //TSortDirection = (sdUp, sdDown);
  TMouseSortType= (msNoSort, msSortClick, msSortShiftClick);
  TCellEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;

TYPE
  TEnhStrGrid= class(TBaseStrGrid)
   private
    Ascending          : Boolean;                                                                       { Sorting order }
    FMouseSrtShift     : Boolean;
    FMouseSort         : TMouseSortType;
    FMouseSwitch       : Boolean;
    FForceKeepHdr      : Boolean;
    EditorPrevMode     : Boolean;
    EditorPrevRow      : LongInt;
    EditorPrevCol      : LongInt;
    FHighlight         : string;                                                                        { Highlight in Bleo all cell that contains the specified string }
    FCenterCols        : Boolean;
    {EVENTS}
    FOnSort            : TCellEvent;
    FEndEdit           : TCellEvent;
    FOnUserChangedCell : TSetEditEvent;
    FOnLinkClick       : TCellEvent;
    procedure setHighlight       (CONST Value: string);
   protected
    procedure WndProc(VAR Message: TMessage); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    { SORT }
    function  MouseOnSortPos     (MouseX, MouseColumn: Integer): Boolean;                               { This is a helper function for Sort. It checks if the mouse has the right coordinates. If the mouse is between two cells it means the user wants to resize the cell not to sort it }
    procedure NaturalSort        (SortCol: Integer);                                                    { In this procedure the algorithm expects numbers on that colum and not text }
    procedure FastSort           (SortCol: Integer; CaseSensitive: Boolean);
   public
    Delimiter: Char;                                                                                    { Delimiter between fields. Used when saving the file to disk }
    Tag1,Tag2,Tag3,Tag4,Tag5: string;                                                                   { User defined data to be stored when the grid is saved to disk }
    CenteredColumns: array of Boolean;                                                                  { There are 2 options to center the text: set CenterAllColumns to true to center ALL cells or use the CenteredColumns matrix to define specific cells that will be centered } { Indexed in 0. Set its length to 0 to disable this feature. If enabled, the length of this matrix MUST be identical with the number of columns. Set an element to true, to center the text in the coresponding column. Has no effect if CenterAllColumns is true. }

    constructor Create (AOwner: TComponent);     override;
    procedure   CreateWnd;                       override;

    { SORT }
    procedure SortColumn   (CONST ColumnToSort: Integer);
    procedure SwapRowText(i, j: Integer);
    procedure ReverseOrder;                                                                             { Swap ROWS (and data/objects associated with those rows) }

    { LOAD/SAVE }
    procedure Save;
    procedure SaveAsCsv          (CONST aFileName: string; CONST Delimiter: Char= ',');                 { Save the entire content to disk (including headers). The difference between this and SaveToFile is that SaveToFile also save the size of }
    procedure SaveToFile         (CONST aFileName: string);
    function  LoadFromFile       (CONST aFileName: string): Boolean;
    function  LoadHeaderWidths   (CONST aFileName: string): Boolean;
    procedure SaveHeaderWidths   (CONST aFileName: string);

    { CONTENT }
    function  GetContentAsHTML   (Truncate: Integer): string;                       overload;
    function  GetContentAsHTML   (Rectangle: TRect; SplitEvery: Integer) : string;  overload;
    function  GetContent         (Rectangle: TRect; Delimiter: Char= ','): string;
    function  GetAllContent      (Delimiter: Char= ','): string;
    function  GetSelectionContent(Delimiter: Char= ','): string;                                        { Returns the content of cell in the specified rectangle. The cells are separated by 'Delimiter' }
    procedure CopySel2Clipboard  (Delimiter: Char= Tab);
    procedure CopyColumn;
    procedure PasteFromClipboard;                                                                       { untested }
    procedure ImportColumnFromClpbrd;                                                                   { Put the clipboard content into the grid, at the current Column, starting at the current Row. }
    {$IFDEF AllowCSV}
    procedure ImportCSV(CsvBody: AnsiString; Separator: Char= ',');  {$ENDIF AllowCSV}

    { STUFF }
    function  WholeGridRect: TRect;
    procedure ToggleHeader;                                                                             { Make the top horizontal header visible/invisible }
    procedure Help;                                                                                     { how to use this grid }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ExpandCenteredColumns;
   published
    { EDITOR }
    procedure EndEdit (ACol, ARow: Longint);                                                                            { NOTA: Nu merge daca AlwaysShowEditro= True }
    property  ForceKeepHeaders  : Boolean        read FForceKeepHdr   Write FForceKeepHdr        default FALSE;         { If true, Delete key cannot delete grid's header (top line) }
    property  OnEndEdit         : TCellEvent     read FEndEdit        write FEndEdit;
    { MOUSE }
    property MouseEditHeader    : Boolean        read FMouseSwitch    Write FMouseSwitch         default FALSE;         { [toggle action] make to top row fixed or normal row every time the user Alt+Click on grid }
    { TEXT }
    property HighlightString    : string         read FHighlight      Write SetHighlight;                               { Highlight in Bleo all cell that contains the specified string }
    property CenterAllColumns   : Boolean        read FCenterCols     Write FCenterCols          default TRUE;          { There are 2 options to center the text: set CenterAllColumns to true to center ALL cells or use the CenteredColumns matrix to define specific cells that will be centered }
    { SORT }
    property MouseSort          : TMouseSortType read FMouseSort      Write FMouseSort           default msNoSort;      { msSortClick: Allows sort by column when the user clicks on grid's header.  msSortShiftClick: The user needs to press Shift in order to sort. MouseSort needs to be enabled also. }
    property OnSort             : TCellEvent     read FOnSort         write FOnSort;
    property OnLinkClick        : TCellEvent     read FOnLinkClick    write FOnLinkClick;                               { The user clicked a cell that has a link in it. Has meaning ONLY if an Avalanche Table is assigned to the grid }
   end;


procedure Register;

IMPLEMENTATION {$R *.res}

USES ccColors, ccTextFile, ccMath, csSystem, cbClipboard;



constructor TEnhStrGrid.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                                                         // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 Ascending        := TRUE;
 FMouseSort       := msNoSort;
 FMouseSrtShift   := TRUE;
 FMouseSwitch     := FALSE;
 FForceKeepHdr    := FALSE;
 SortedCol        := -1;                                                                           { Reset 'Sort' sign in header }
 FHighlight       := '';
 Delimiter        := chr(255);
 {COLORS}
 FCenterCols      := TRUE;
 DrawingStyle     := gdsGradient;                                                                  { gdsGradient looks better than gdThemed }
 {EDITOR}
 EditorPrevMode   := FALSE;                                                                        { are legatura cu OnEndEdit }
 EditorPrevRow    := Row;                                                                          { are legatura cu OnEndEdit }
 EditorPrevCol    := Col;
 SetLength(CenteredColumns, 0);
end;


//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TEnhStrGrid.CreateWnd;
begin
 inherited CreateWnd;
end;














{--------------------------------------------------------------------------------------------------
                                   DRAW CELL
--------------------------------------------------------------------------------------------------}
procedure TEnhStrGrid.DrawCell(aCol, aRow: Longint; aRect: TRect; aState: TGridDrawState);
VAR
    CellText: string;
    xStart: Integer;
begin
 if (csCreating in ControlState)
 OR NOT DefaultDrawing then
  begin
   inherited;
   EXIT;
  end;

 { SHOW 'SORTED' GLYPH }                                                                                                { Draw a small glyph if the grid is sorted by this column }
 if (aRow= 0) AND (aCol= SortedCol)
 then
   begin
    Canvas.Font.Color:= clBlack;     {TODO 4: use default color here }

    { Draw text }
    Canvas.TextRect(ARect, ARect.Left+10, ARect.Top+2, Cells[ACol, ARow]);
    { Draw box }
    Canvas.Brush.Color:= clOrangeGray;
    Canvas.Rectangle(ARect.Left+1, 2, ARect.Left+8, DefaultRowHeight-2);
    Canvas.Brush.Color:= Color;

    DrawChevron(ARect.Left, ARect.Top);
   end
 else
  begin
   {
   if Enabled then
     if  ARow = row // (gdFocused in AState)
     then Canvas.Font.Color:= CursorTextColor
     else Canvas.Font.Color:= ColorTextEnabled
   else Canvas.Font.Color:= ColorTextDisabled; }

   { HIGHLIGHT TEXT }                                                                                                   { Highlight the cell in BLEO if it contains this text }
   if  (FHighlight<> '')
   AND (Cells[ACol,ARow]= FHighlight)
   then Canvas.Brush.Color:= TColor($C0A0A0);

   CellText := Cells[ACol, ARow];
   Canvas.Font.Color:= clBlack;     {TODO 4: use default color here }

   if CellText = ''
   then inherited DrawCell(ACol, ARow, ARect, AState)
   else
     if CenterAllColumns                                                                                                 { There are 2 options to center the text: set CenterAllColumns to true to center ALL cells or use the CenteredColumns matrix to define specific cells that will be centered }
     OR
       (NOT CenterAllColumns                                                                                             { CenteText is not in use but... }
        AND (Length(CenteredColumns) > 0)                                                                                { ...but the CenteredColumns is }
        AND CenteredColumns[ACol] )                                                                                      { If this column was marked for centering }
     then
      { Center text }
      begin
       //DON'T CALL THIS because it will draw the text twice      inherited; x                                   { Call inherited in order to draw cell's background (important if the caller set previously the brush/font/pen to something like: Grid.Canvas.Brush.Color:= clGreen) }

       if Length(CellText) > MaxTextInCellBug
       then CellText:= system.COPY(CellText, 1, MaxTextInCellBug)+ '...';     { Work around this bug: http://stackoverflow.com/questions/30574585/tstringgrid-cannot-display-very-long-6k-strings }

       Assert((Length(CenteredColumns) <= 0)
          OR ((Length(CenteredColumns) > 0) AND (Length(CenteredColumns) = ColCount)), 'CenteredColumns array <> ColCount');      { Check the CenteredColumns matrix }

       xStart:= (ARect.Width - Canvas.TextWidth(CellText)) DIV 2;
       if xStart< 0 then xStart:= 0;

       {TODO: Here I use the a magic number 2 to center the text VERTICALLY. It won't work if the row's height will be something else than the default height! }
       Canvas.TextRect(ARect, ARect.Left+xStart, ARect.Top+2, CellText);                                                 // I tried to use Drawtext but it won't work because it will ignore all Canvas settings (color, font, etc) that I set for this cell: DrawText(Canvas.Handle, PChar(CellText), Length(CellText), TempRect, DT_CENTER);
      end
     else
       inherited DrawCell(ACol, ARow, ARect, AState);
  end;
end;















{--------------------------------------------------------------------------------------------------
                                MOUSE EVENTS
--------------------------------------------------------------------------------------------------}
procedure TEnhStrGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol, ARow: Integer;
begin
 inherited;
 MouseToCell(X, Y, ACol, ARow);

 { Check sort conditions }
 if  (ARow= 0)
 AND (FixedRows> 0)
 AND MouseOnSortPos(x, ACol)                                                                       { This is a helper function for Sort. It checks if the mouse has the right coordinates. If the mouse is between two cells it means the user wants to resize the cell not to sort it }
 then
  begin
   { Sort Event }
   if (MouseSort  = msSortClick)
   OR ((MouseSort = msSortShiftClick) AND (ssShift in Shift)) then
     if Assigned(FOnSort)
     then FOnSort(Self, aCol, aRow)  // In this event the user will call SortColumn(aCol), NaturalSort or his own sort routine
     else SortColumn(aCol);
  end;

 { Toggle header }
 if MouseEditHeader
 AND (ssAlt in Shift)                                                                              { ALT key is pressed? }
 then ToggleHeader;                                                                                { Make the top horizontal header visible/invisible }

 { The user performed a Control-Click? Then enter a new row }
 if MouseDuplicateLine
 AND (ssCtrl in Shift) then
  begin
   Assert(1=2, ' Not implemented yet. ');
   InsertRow(Row);
   Rows[Row]:= Rows[Row- 1];
  end;
end;














{--------------------------------------------------------------------------------------------------
                                    ON END EDIT
--------------------------------------------------------------------------------------------------}
procedure TEnhStrGrid.EndEdit(ACol, ARow: Integer);                                                { NOTA: Nu merge daca AlwaysShowEditror= True }
begin
 Cells[ACol, ARow]:= StringReplace(Cells[ACol, ARow], CRLFw, ' ', [rfReplaceAll]);                  { Replace ENTERs with space - This Grid cannot draw a text on multiple rows so enter character will he rndered as 2 squares. }
 if Assigned(FEndEdit)
 then FEndEdit(Self, EditorPrevCol, EditorPrevRow);
end;



procedure TEnhStrGrid.WndProc(var Message: TMessage);                                              { are legatura cu OnEndEdit  }
begin
 inherited;
 If EditorPrevMode Then
 Begin
   If NOT EditorMode then
   Begin
     EditorPrevMode:= EditorMode;
     EndEdit(EditorPrevCol, EditorPrevRow);
   End;
   EditorPrevRow := Row;
   EditorPrevCol := Col;
 End;
 EditorPrevMode := EditorMode;
end;












{--------------------------------------------------------------------------------------------------
                              LOAD / SAVE
--------------------------------------------------------------------------------------------------}

procedure TEnhStrGrid.Save;
begin
 SaveToFile(FileName);
end;


procedure TEnhStrGrid.SaveToFile(CONST aFileName: string);         { Save content of the entire grid (header & cells) as ANSI. It uses 255 as Delimiter instead of Enter }
VAR cl, CurRow: Integer;
    Body: string;
begin
 Body:= '';
 FileName:= aFileName;

 { Write magic no, version, comment }
 Body:= Body+ 'CBC' + Delimiter;                                                                   { Magic numner (Cubic) }
 Body:= Body+ '0003'+ Delimiter;                                                                   {  File version }
 Body:= Body+ Tag1  + Delimiter;                                                                   { store user defined data }
 Body:= Body+ Tag2  + Delimiter;                                                                   { store user defined data }
 Body:= Body+ Tag3  + Delimiter;                                                                   { store user defined data }
 Body:= Body+ Tag4  + Delimiter;                                                                   { store user defined data }
 Body:= Body+ Tag5  + Delimiter;                                                                   { store user defined data }

 { Write RowCount/ColCount }
 Body:= Body+ IntToStr(RowCount)+ Delimiter;                                                            { Row7 }
 Body:= Body+ IntToStr(ColCount)+ Delimiter;                                                            { Row8 }

 { Write column width }
 for cl:= 0 TO ColCount-1
  DO Body:= Body+ IntToStr(ColWidths[cl])+ Delimiter;

 { Write cell content }
 for CurRow:= 0 to RowCount-1 DO
  for Cl:= 0 TO ColCount-1 DO
    Body:= Body+ ReplaceCharF(Cells[Cl, CurRow], Delimiter, ' ')+ Delimiter;                       { Make sure the user didn't used the Delimiter in the text he entered into the grid }

 StringToFile(aFileName, Body, woOverwrite, wpAuto);
end;


function TEnhStrGrid.LoadFromFile(CONST aFileName: string): Boolean;  { It uses 255 as Delimiter instead of Enter }
VAR
   TslLine: Integer;
   TSL: TStringList;

  function LoadV2: Boolean;                                                                        { There are no programs that are using this version distributed on Internet. CAN BE DELETED }
  CONST MetaSize= 5;
  VAR Cl, CurRow: Integer;
  begin
   Result:= TRUE;

   { Restore number of cells }
   RowCount:= StrToInt(TSL[3]);
   ColCount:= StrToIntDef(TSL[4], 1);
   FixFixedRow;

   { Restore cells width }
   for Cl:= 0 TO ColCount-1
    DO ColWidths[Cl]:= StrToIntDef(TSL[Cl+MetaSize], 40);                                          { +MetaSize because I want to skip the first 3 lines (comments, rowcount, colcount) }

   { Restore cell content }
   for CurRow:= 0 to RowCount-1 DO
    for Cl:= 0 TO ColCount-1 DO
     begin
      TslLine:= Cl+ ((CurRow * ColCount)+ (MetaSize+ ColCount));                                   { +ColCount in order to skip over 'column width' data }
      if TslLine>= TSL.Count then EXIT(FALSE);
      Cells[Cl, CurRow]:= TSL[TslLine];
     end;
  end;

  function LoadV3: Boolean;
  CONST MetaSize= 9;
  VAR Cl, CurRow: Integer;
  begin
   Result:= TRUE;

   { Restore number of cells }
   Tag1:= TSL[2];
   Tag2:= TSL[3];
   Tag3:= TSL[4];
   Tag4:= TSL[5];
   Tag5:= TSL[6];

   RowCount:= StrToInt(TSL[7]);
   ColCount:= StrToIntDef(TSL[8], 1);
   FixFixedRow;

   { Restore cells width }
   for Cl:= 0 TO ColCount-1
    DO ColWidths[Cl]:= StrToIntDef(TSL[Cl+MetaSize], 40);                                          { +MetaSize because I want to skip the first 3 lines (comments, rowcount, colcount) }

   { Restore cell content }
   for CurRow:= 0 to RowCount-1 DO
    for Cl:= 0 TO ColCount-1 DO
     begin
      TslLine:= Cl+ ((CurRow * ColCount)+ (MetaSize+ ColCount));                                   { +ColCount in order to skip over 'column width' data }
      if TslLine>= TSL.Count then EXIT(FALSE);
      Cells[Cl, CurRow]:= TSL[TslLine];
     end;
  end;

begin
 Clear;
 FileName:= aFileName;

 Result:= FileExists(FileName, TRUE);
 if NOT Result then EXIT;

 TSL:= TStringList.Create;
 TRY
   TSL.LoadFromFile(FileName);

   { Convert delimiter to ENTER in order to have multiple lines in TSL }
   TSL.Delimiter      := Delimiter;
   TSL.StrictDelimiter:= TRUE;                                                                     { Fixes Delphi bug in which spaces were considered delimiters: http://jedqc.blogspot.com.es/2005/12/d2006-new-strictdelimiter-property.html }
   TSL.DelimitedText  := TSL.Text;

   { Check magic number }
   if TSL[0]<> 'CBC' then EXIT(FALSE);

   { Check version }
   if TSL[1]= '0002'
   then Result:= LoadV2
   else
     if TSL[1]= '0003'
     then Result:= LoadV3
     else EXIT(FALSE);

 FINALLY
   FreeAndNil(TSL);
 END;
end;




procedure TEnhStrGrid.SaveAsCsv (CONST aFileName: string; CONST Delimiter: Char= ',');      { Save the entire content to disk (including header cells) as CSV. The difference between this and SaveToFile is that SaveToFile also saves binary info (the size of grid) }
VAR
   CsvContent: string;
begin
 CsvContent:= GetAllContent(Delimiter);
 StringToFile(aFileName, CsvContent, woOverwrite, wpAuto);
end;
























procedure TEnhStrGrid.SaveHeaderWidths(CONST aFileName: string);         { Save content of the entire grid (header & cells) as ANSI. It uses 255 as Delimiter instead of Enter }
VAR cl: Integer;
    Body: string;
begin
 Body:= 'Column widths'+ CRLFw;
 Body:= Body+ IntToStr(ColCount)+ CRLFw;    { Write ColCount }

 { Write column width }
 for cl:= 0 TO ColCount-1
  DO Body:= Body+ IntToStr(ColWidths[cl])+ CRLFw;
 StringToFile(aFileName, Body, woOverwrite, wpOff);
end;



function TEnhStrGrid.LoadHeaderWidths(CONST aFileName: string): Boolean;  { It uses 255 as Delimiter instead of Enter }
VAR
   TSL: TStringList;
   Cl, ColCnt: Integer;
begin
 Result:= FileExists(aFileName, TRUE);
 if NOT Result then EXIT;

 TSL:= StringFromFileTSL(aFileName);
 TRY
   ColCnt:= StrToIntDef(TSL[1], -1);
   if ColCnt = -1        then EXIT(FALSE);   { 'ColumnCount' invalid }
   if ColCount <> ColCnt then EXIT(FALSE);   { 'ColumnCount' from file doesn't match the actual ColumnCount }

   { Restore cells width }
   for Cl:= 2 TO ColCount-1
    DO ColWidths[Cl-2]:= StrToIntDef(TSL[Cl], 40);
 FINALLY
   FreeAndNil(TSL);
 END;
end;































{--------------------------------------------------------------------------------------------------
   CONTENT
--------------------------------------------------------------------------------------------------}
function TEnhStrGrid.WholeGridRect: TRect;
begin
 Result.Left:= 0;
 Result.Top := 0;
 Result.Bottom:= RowCount-1;
 Result.Right := ColCount-1;
end;


function TEnhStrGrid.GetContent (Rectangle: TRect; Delimiter: Char= ','): string;                  { Returns the content of all cells. The cells are separated by 'Delimiter' }
VAR
   cl, rw: Integer;

  function GetCellSafe: string;
  begin
    Result:= Cells[cl, rw];
    { If the chousen Delimiter already exists in our text it will fuck up the CSV format so we need to replace it with semi column (;) }
    if Delimiter= ';'                                  { Make sure we won't replace semicol  }
    then ReplaceChar(Result, Delimiter, ',')
    else ReplaceChar(Result, Delimiter, ';');
  end;

begin
  Result := '';
  for rw := Rectangle.Top to Rectangle.Bottom DO
    for cl:= Rectangle.Left to Rectangle.Right DO
      if cl = Rectangle.Right
      then Result:= Result + GetCellSafe + CRLFw               { Don't put a comma after the last column. Put an ENTER instead }
      else Result:= Result + GetCellSafe + Delimiter;
end;



function TEnhStrGrid.GetSelectionContent(Delimiter: Char= ','): string;                            { Returns the content of cells in the specified rectangle. The cells are separated by 'Delimiter' }
begin
  Result:= GetContent(Rect(Selection.Left, Selection.Top, Selection.Right, Selection.Bottom), Delimiter);
end;



function TEnhStrGrid.GetAllContent(Delimiter: Char= ','): string;                                  { Returns the content of cells in the specified rectangle. The cells are separated by 'Delimiter' }
begin
 Result:= GetContent(WholeGridRect, Delimiter);
end;



procedure TEnhStrGrid.ImportColumnFromClpbrd;                                                      { Put the clipboard content into the grid, starting at the current cell. }
VAR CurRow: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
  TSL.Text:= StringFromClipboard;

  { Make sure I have enough rows in Grid }
  if RowCount- FixedRows- Row < TSL.Count
  then RowCount:= TSL.Count + FixedRows;

  { Store cell content }
  for CurRow:= 0 to TSL.Count-1
   DO Cells[Col, Row+CurRow]:= TSL[CurRow];
 FINALLY
   FreeAndNil(TSL);
 END;
end;

{$IFDEF AllowCSV}
procedure TEnhStrGrid.ImportCSV(CsvBody: AnsiString; Separator: Char= ',');
VAR
   CSV: TCSVFile;
   Rw: Integer;
   Cl: Integer;
begin
 Assert(CsvBody > '');

 CSV:= LoadCsvFromString(CsvBody, Separator);
 TRY
   //del CSV.Separator:= AnsiChar(Separator);

   RowCount:= CSV.RowCount;
   ColCount:= CSV.FieldCount;

   CSV.First;
   for Rw:= 0 to RowCount-1 DO
    begin
     for Cl:= 0 to ColCount-1
      DO Cells[Cl, Rw]:= CSV.Fields[Cl];

     if NOT CSV.EOF then CSV.Next;
    end;

 FINALLY
  FreeAndNil(CSV);
 END;
end;
{$ENDIF AllowCSV}


{EXAMPLE

function LoadCsvFromString(CONST S: string): TCSVFile;

begin
 Assert(s > '');
 Stream:= TStream.Create;
 TRY
  Stream.Write(@s, Length(s)* SizeOf(s[1]));
  Result:= TCSVFile.Create(Stream);
 FINALLY
  FreeAndNil(Stream);           <------ don't free it. still on use!
 END;
end;
}













procedure TEnhStrGrid.CopyColumn;  { Copy current column to clipboard }
VAR s: string;
    rw: Integer;
begin
 s:= '';
 for rw:= 0 to RowCount-1
  DO s:= s+ Cells[Col, rw]+ CRLFw;

 ClipBoard.AsText := s;
end;



procedure TEnhStrGrid.CopySel2Clipboard(Delimiter: Char= Tab);
begin
 ClipBoard.AsText := GetSelectionContent(delimiter);
end;


procedure TEnhStrGrid.PasteFromClipboard;                                                          { netestata }
VAR
  Grect: TGridRect;
  S, CS, F: string;
  L, R, C: Byte;
begin
 GRect:= Selection;
 L := GRect.Left;
 R := GRect.Top;
 S := ClipBoard.AsText;
 R := R - 1;
 while Pos(#13, S) > 0 DO
  begin
    R  := R + 1;
    C  := L - 1;
    CS := system.COPY(S, 1, Pos(#13, S));
    while Pos(Tab, CS) > 0 do
    begin
      C:= C + 1;
      if (C <= ColCount - 1) and (R <= RowCount - 1)
      then Cells[C, R] := system.COPY(CS, 1,Pos(Tab, CS) - 1);
      F := system.COPY(CS, 1,Pos(Tab, CS) - 1);
      System.Delete(CS, 1,Pos(Tab, CS));
    end;
    if (C <= ColCount - 1) and (R <= RowCount - 1)
    then Cells[C + 1,R] := system.COPY(CS, 1,Pos(#13, CS) - 1);
    System.Delete(S, 1,Pos(#13, S));
    if system.COPY(S, 1,1) = #10
    then System.Delete(S, 1,1);
  end;
end;



function TEnhStrGrid.GetContentAsHTML(Rectangle: TRect; SplitEvery: Integer): string;              { Truncate= truncate after x character is text too long }
VAR
   s: string;
   cl, lin: Integer;
begin
  Result := '<table width="'+IntToStr(Width)+'" border="1" align="center">' + CRLFw;

  for lin := Rectangle.Top to Rectangle.Bottom DO
   begin
    Result:= Result+ '<tr>'+CRLFw;
    for cl:= Rectangle.Left to Rectangle.Right DO
     begin
      s:= Cells[cl, lin];
      if Length(s) > SplitEvery
      then InsertCharEvery(' ', s, SplitEvery);                                                  { Truncate the string if it is too long }

      if lin = 0
      then Result:= Result + '  <th width="'+IntToStr(ColWidths[cl])+'" scope="col">'+ s + '</th>' + CRLFw
      else Result:= Result + '  <td>'                                                + s + '</td>' + CRLFw;
     end;
    Result:= Result+ '</tr>' + CRLFw;
   end;
  Result:= Result+ '</table>';
end;


function TEnhStrGrid.GetContentAsHTML(Truncate: Integer): string;
begin
 Result:= GetContentAsHTML(WholeGridRect, Truncate);
end;







{--------------------------------------------------------------------------------------------------
                                  SORT
--------------------------------------------------------------------------------------------------}

{ This is a helper function for Sort. It checks wether the mouse has the right coordinates.
  If the mouse is between two cells it means the user wants to resize the cell not to sort it. }
function TEnhStrGrid.MouseOnSortPos(MouseX, MouseColumn: Integer): Boolean;
CONST
   Trigger= 8;                                                                                                 { The user has toclick 8 pixels inside the header (left/right) in order to sort the grid. Otherwise the click will be ignored (no sorting will take place). This is needed in order to prevent conflict between the 'Resize column' function and 'Sort' function }
VAR
   cl, CellStart, CellEnd: Integer;
begin
 CellStart:= 0;
 for cl:= 0 to MouseColumn-1
  DO CellStart:= CellStart+ ColWidths[cl];                                                                   { Add the width of previous columns to the with of current column }

 CellEnd:= CellStart+ ColWidths[MouseColumn];

 Result:= (MouseX > CellStart + Trigger)            { Cell start }
      AND (MouseX < CellEnd   - Trigger);           { Cell end }
end;



Procedure TEnhStrGrid.SortColumn(CONST ColumnToSort: Integer);
Begin
 Cursor:= crHourglass;
 TRY
  { Close editor }
  if  Editormode
  AND (goAlwaysShowEditor in Options)
  AND (Assigned(FOnUserChangedCell))
  then FOnUserChangedCell(Self, Col, Row, Cells[col, row]);

  SortedCol:= ColumnToSort;
  BeginUpdate;

  { Sort }
  NaturalSort(SortedCol);

  { Reverse sort }
  Ascending:= NOT Ascending;
  if NOT Ascending
  then ReverseOrder;
 FINALLY
  Cursor:= crDefault;
  EndUpdate;
 END;
end;



{ This works only for text. It doesn't sort numbers corectly. For example it sorts like this: 15, 150, 16 }
procedure TEnhStrGrid.FastSort(SortCol: Integer; CaseSensitive: Boolean);                        //////// http://www.delphiforfun.org/Programs/Delphi_Techniques/GridSort.htm
CONST
   ctHeaderLine = 1;                                                                               { because we dont want to sort the header}
VAR
   I, J: Integer;
begin
  if CaseSensitive
  then
   begin
    for I := FixedRows to RowCount - 2 do                                                          { because we dont want to sort the header, -2 because last row has no next row}
      for J:= I + 1 to RowCount - 1 do                                                             {from next row to end}
       if CompareStr(Cells[SortCol, I], Cells[SortCol, J]) > 0
       then SwapRowText(j, i);                                                                         { swap raws }
   end
  else
    for I := FixedRows to RowCount - 2 do
     for J := I + 1 to RowCount- 1 do
       if CompareText(Cells[SortCol, I], Cells[SortCol, J]) > 0
       then SwapRowText(j, i);
end;


procedure TEnhStrGrid.NaturalSort(SortCol: Integer);
CONST
   ctHeaderLine = 1;                                                                               { because we dont want to sort the header}
VAR
   I, J: Integer;
begin
 for I := FixedRows to RowCount - 2 do                                                             { because we dont want to sort the header, -2 because last row has no next row}
  for J:= I + 1 to RowCount - 1 do                                                                 {from next row to end}
   if StrCmpLogicalW(PChar(Cells[SortCol, I]), PChar(Cells[SortCol, J])) > 0
   then SwapRowText(j, i);                                                                         { swap raws }
end;



{Slow because it converts text to int. To be used on columns that contain ONLY numbers
procedure TEnhStrGrid.NaturalSort(iFrom, iTo, SortCol: Integer);                                              { In this procedure the algorithm expects numbers on that colum and not text
VAR I, J: Integer;
    P: Integer;
begin
 REPEAT
  I := iFrom;
  J := iTo;
  P := StrToInt(Cells[SortCol, (iFrom + iTo) shr 1]);

  REPEAT
    while StrToInt(Cells[SortCol, I])< P DO Inc(I);
    while StrToInt(Cells[SortCol, J])> P DO Dec(J);

    if I <= J then
      begin
       if I <> J
       then SwapRowText( I, J );
       Inc(I);
       Dec(J);
      end;
  UNTIL I > J;

  if iFrom< J
  then NaturalSort(iFrom, J, SortCol);
  iFrom:= I;
 UNTIL I >= iTo;
end; }



procedure TEnhStrGrid.ReverseOrder;                                                                { Swap ROWS (and data/objects associated with those rows) }
VAR I, J: Integer;
begin
 if RowCount= 1 then EXIT; { There is nothing to sort }
 J:= RowCount;                                                                                     { RowCountEx is indexed in 1 because I don't conside the first item }
 for I:= Fixedrows to RoundDown((RowCount-Fixedrows) / 2)
  DO SwapRowText(I, J - I);
end;














{--------------------------------------------------------------------------------------------------
   OTHER
--------------------------------------------------------------------------------------------------}
procedure TEnhStrGrid.SwapRowText(i, j: Integer);                                                  { Swap the TEXT in row i with the text in row j }
VAR k: Integer;
Begin
 for k:= 0 to ColCount-1
  DO Cols[k].Exchange(i, j);
end;


{procedure TEnhStrGrid.SwapRows(R1, R2: Integer);
VAR Temp: TObject;
begin

 Assert(R1>= FixedRows);
 Assert(R2>= FixedRows);
 Assert(R2< rowCount);

 Temp:= Items[R1];
 FItems[R1]:= FItems[R2];
 FItems[R2]:= Temp;
end; }



procedure TEnhStrGrid.Help;                                                                        { how to use this grid }
begin
 if Assigned(FOnSort)
 AND (MouseSort= msSortShiftClick)
 then MessageInfo('Press SHIFT while clicking the top row to sort the grid.');
end;


procedure TEnhStrGrid.ToggleHeader;                                                                { Make the top horizontal header visible/invisible }
begin
 if FixedRows= 0
 then FixedRows:= 1
 else FixedRows:= 0;
end;


procedure TEnhStrGrid.SetHighlight(CONST Value: string);
begin
 if FHighlight<> Value then
  begin
   FHighlight:= Value;
   InvalidateGrid;
  end;
end;



Procedure TEnhStrGrid.ExpandCenteredColumns;
Begin
 SetLength(CenteredColumns, ColCount);
End;


{ del
Procedure TEnhStrGrid.ReverseOrder2;
VAR i, j: Integer;
Begin
 i:= Fixedrows;
 j:= RowCount-1;
 While i < j Do
  begin
   SwapRowText( I, J );
   Inc( i );
   Dec( j );
  end;
End; }









procedure Register;
begin
  RegisterComponents('LightSaber', [TEnhStrGrid]);
end;


end.(*============================================================================





{
 There are two routines to implement the OnColumnClick Methods for a TStringGrid.
 Set the first row as fixed and the Defaultdrawing to True.





procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Text: string;
begin
  with stringgrid1 do
  begin
    MouseRoCell(x, y, acol, arow);
    if (arow = 0) and (button = mbleft) then
      case acol of
        0..2:
          begin
            // Draws a 3D Effect (Push)
            // Zeichnet 3D-Effekt (Push)
            zelle := CellRect(acol, arow);
            Text := Cells[acol, arow];
            Canvas.Font := Font;
            Canvas.Brush.Color := clBtnFace;
            Canvas.FillRect(zelle);
            Canvas.TextRect(zelle, zelle.Left + 2, zelle.Top + 2, Text);
            DrawEdge(Canvas.Handle, zelle, 10, 2 or 4 or 8);
            DrawEdge(Canvas.Handle, zelle, 2 or 4, 1);
          end;
      end;
  end;
end;

procedure TForm1.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Text: string;
begin
  with StringGrid1 do
  begin
    // Draws a 3D-Effect (Up)
    // Zeichnet 3D-Effekt (Up)
    Text := Cells[acol, arow];
    if arow = 0 then
    begin
      Canvas.Font := Font;
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(zelle);
      Canvas.TextRect(zelle, zelle.Left + 2, zelle.Top + 2, Text);
      DrawEdge(Canvas.Handle, zelle, 4, 4 or 8);
      DrawEdge(Canvas.Handle, zelle, 4, 1 or 2);
      MouseToCell(zelle.Left, zelle.Top, acol, arow);
    end;
  end;
  if (arow = 0) and (Button = mbleft) then
    case acol of
      0..2:
        begin
          // Code to be executed...
          // Programmcode der ausgeführt werden soll
          ShowMessage('Column ' + IntToStr(acol));
          zelle := stringgrid1.CellRect(1, 1);
        end;
    end;
end;









 goFixedVertLine  Vertical lines are drawn to separate the fixed (nonscrolling) columns in the
 goFixedHorzLine  Horizontal lines are drawn to separate the fixed (nonscrolling) rows in the
 goVertLine          Vertical lines are drawn to separate the scrollable columns in the
 goHorzLine          Horizontal lines are drawn to separate the scrollable rows in the
 goRangeSelect          Users can select ranges of cells at one time. goRangeSelect is ignored if Options includes goEditing.

 goDrawFocusSelected  Cells with input focus are drawn with a special highlight color, just like selected cells without input focus. If goDrawFocusSelected is not included, the cell with input focus is distinguished by a focus rectangle, not by a special background color.
 goRowSizing          Scrollable rows can be individually resized.
 goColSizing          Scrollable columns can be individually resized.
 goRowMoving          Scrollable rows can be moved using the mouse.
 goColMoving          Scrollable columns can be moved using the mouse.

 goEditing          Users can edit the contents of cells. When goEditing is included in Options, goRangeSelect has no effect.
 goTabs                  Users can navigate through the cells in the grid using Tab and Shift+Tab.
 goRowSelect          Entire rows are selected rather than individual cells. If goRowSelect is included in Options, goAlwaysShowEditor has no effect.
 goAlwaysShowEditor  The grid is locked into edit mode. The user does not need to use Enter or F2 to turn on EditorMode. If Options does not include goEditing, goAlwaysShowEditor has no effect. If Options includes goRowSelect, goAlwaysShowEditor has no effect.

 goThumbTracking  The grid image updates while the user is dragging the thumb of the scroll bar. If goThumbTracking is not included, the image does not update until the user releases the thumb in a new Position.
}

