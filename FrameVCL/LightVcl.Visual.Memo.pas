UNIT LightVcl.Visual.Memo;

{=============================================================================================================
   2026.03.21
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  Tester:
     c:\MyProjects\Packages\CubicCommonControls-Testers\cubicMemo\

  Features:
     Allow user to press Ctrl+A to select all text
     Copy all to clipboard
     Insert/overwrite mode
     Sort lines
     Center line in view
     Remove empty lines
     Caret conversions
     Search
     Margins
     CountNonEmptyLines
     FilterLines             - Remove all lines that does not contain this text
     LoadFromFile            - Load text from specified file IF the file exists. Otherwise, don't show an error
     AutoScrollBars          - Auto show/hide vertical scrollbar based on content (requires ScrollBars = ssVertical or ssBoth)

   Note: The 'Text' property is MUCH faster than Assign. Details: https://stackoverflow.com/questions/46990961/tmemo-is-painfuly-slow-when-working-with-large-number-of-lines


See: What's the difference between CreateWnd and CreateWindowHandle? https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Math, System.Character, System.StrUtils,
  Vcl.Clipbrd, Vcl.StdCtrls, Vcl.Controls, VCL.graphics, Vcl.Themes;

TYPE
  TTypeMode= (tmInsert, tmOverwrite);

  TSearchOption = (soIgnoreCase, soFromStart, soWrap);   { soFromStart:  If this is active, the search will start from the top of the memo. But after this, I have to uncheck it so the search can continue, else it will not advance (it will always search from start) }
  TSearchOptions = set of TSearchOption;

  TLightMemo = class(TMemo)
   private
     FCursAfter: Boolean;                                                 { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection) }
     FTypeMode: TTypeMode;
     FAutoScrollBars: Boolean;                                            { Auto show/hide vertical scrollbar based on content }
     FUpdatingScrollBars: Boolean;                                        { Recursion guard for UpdateAutoScrollBars }
     procedure SetAutoScrollBars(Value: Boolean);
     procedure WMSize(var Message: TWMSize); message WM_SIZE;
   protected
     procedure Change; override;
     procedure KeyDown   (VAR Key: Word; Shift: TShiftState); override;   { "Select All" (CTRL+A) functionality }
     procedure KeyPress  (VAR Key: Char);                     override;
     procedure CreateWnd; override;
   public
     SearchOptions: TSearchOptions;
     constructor Create  (AOwner: TComponent); override;

     { Selection }
     procedure SelectLine(Line : Integer);   overload;
     procedure SelectLine(aText: string);    overload;                    { Select line containing specified text }
     procedure SelectCurrentLine;
     procedure RemoveSelection;
     procedure MoveCaretToChar(CharIndex: Integer);

     procedure Randomize;
     procedure SortLines;

     function  VisibleLines: Integer;
     function  CountNonEmptyLines: Integer;
     function  CountWords: Integer;

     { Util }
     procedure SwapLines(x, y: Integer);
     procedure CopyToClipboardAll;
     function  Search(SrcStr: string): Boolean;
     procedure AddInteger (Number: Integer);
     procedure AddString  (CONST aText: string);
     procedure AddEntry   (CONST aText: string; Number: Integer);
     procedure AddSeparator;

     function GetWordUnderCaret: string;
     function SelectWordUnderCaret: string;

     procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
     procedure WMPaste  (var Message: TWMPaste);   message WM_PASTE;
     //procedure FixEnters;                                               { Convert Unix enters to Windows enters because it doesn't handle well Unix enters. Details:  }

     { Edit }
     procedure RemoveLastEmptyLine;
     procedure RemoveEmptyLines;
     procedure RemoveEmptyLinesEx;                                        { Applies trim before removing empty lines. This way more lines will become empty }
     function  RemoveDuplicates: Integer;
     function  RemoveLines (const BadWord : string; PartialMatch: Boolean): Integer;   { Remove lines that contain the specified text }
     function  KeepLines   (const KeepText: string): Integer;             { Keep lines that contain the specified text }
     procedure KeepFirstLines(const HowManyLines: Integer);
     procedure Trim;                                                      { Trim empty spaces and control caracters at the begining/end of each line }
     function  FindLine(aText: string): Integer;                          { Find line containing the specified text } { Case insensitive }

     { Scroll }
     procedure CenterInView(LineNum: Integer);  overload;                 { Center the specified line in the middle of the view }
     procedure CenterInView;                    overload;
     procedure ScrollAtEnd;
     procedure ScrollAtTop;
     procedure MoveCursorAtEOL;                                           { Move cursor at the end of the current row }

     { Caret conversions }
     function  CharToLine(const CharNo: Integer): Integer;
     function  CursorToChar(CONST MousePosition: TPoint): Integer;
     function  ConvertCaretToChar: Integer;
     function  CurLineToCharNo: Integer;
     function  LineToCharNo(const Line: Integer): Integer;

     { Line }
     function  CurrentLine: Integer;                                      { Line containing the cursor }
     procedure LineUp;                                                    { Move current line up }
     procedure LineDown;
     function  LineLenght: Integer;                                       { Returns number of characters that can fit on a line }
     function  MaxVisLines: Integer;                                      { Returns the number of lines that fits in the TMemo }

     procedure SetMargins(CONST LeftMargin, RightMargin, TopBottomMargin: Integer);               { Set margins }
     procedure UpdateAutoScrollBars;                                                              { Show/hide vertical scrollbar based on content. Requires ScrollBars = ssVertical or ssBoth }
     function  LoadFromFile(FileName: string): Boolean;        { Loadtext from specified file IF the file exists. Otherwise, don't show an error }
   published
     property CursorAfterSearch: Boolean   read FCursAfter write  FCursAfter default FALSE;       { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection). Used in Matthias Kloss' program }
     property TypeMode         : TTypeMode read FTypeMode  write  FTypeMode;
     property AutoScrollBars   : Boolean   read FAutoScrollBars write SetAutoScrollBars default FALSE;  { Auto show/hide vertical scrollbar. Set ScrollBars to ssVertical or ssBoth for this to work }
  end;


procedure Register;

IMPLEMENTATION

USES
   LightVcl.Common.IO, LightCore;

{  How to know when the USER changed the text in a TMemo/TEdit?
   https://stackoverflow.com/questions/41719647/how-to-know-when-the-user-changed-the-text-in-a-tmemo-tedit/56832800#56832800


  How about using the Modified property?

  procedure TForm1.MyEditChange(Sender: TObject);
  begin
    if MyEdit.Modified then
    begin
        // The user changed the text since it was last reset (i.e. set programmatically)
    end;
  end;}



Constructor TLightMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);    // Note: Don't set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
  FTypeMode:= tmInsert;
  HideSelection:= FALSE;                                                                            { Show the selection even if the memo isn't focussed. }
  SearchOptions:= [soIgnoreCase, soWrap];
  FCursAfter:= FALSE;                                                                               { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection) }
  FAutoScrollBars:= FALSE;
  FUpdatingScrollBars:= FALSE;

  { Note: RegisterStyleHook is not needed — TMemoStyleHook is inherited from TMemo.
    Do NOT add UnRegisterStyleHook in Destroy — it would remove the VCL-provided hook,
    breaking styles for all TLightMemo instances.
    See: http://stackoverflow.com/questions/28463556/vcl-styles-breaks-randomly }
end;


procedure TLightMemo.CreateWnd;
begin
  inherited CreateWnd;
  //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
  UpdateAutoScrollBars;
end;


{ Load text from specified file IF the file exists. Otherwise, don't show an error }
function TLightMemo.LoadFromFile(FileName: string): Boolean;
begin
  Result:= FileExists(FileName) AND NOT FileIsLockedR(FileName);
  if Result
  then Lines.LoadFromFile(FileName);  // Text:= StringFromFile(FileName)
end;


procedure TLightMemo.Change;
begin
 inherited;
 UpdateAutoScrollBars;
end;


procedure TLightMemo.WMSize(var Message: TWMSize);
begin
 inherited;
 UpdateAutoScrollBars;
end;


procedure TLightMemo.SetAutoScrollBars(Value: Boolean);
begin
 if FAutoScrollBars <> Value then
   begin
    FAutoScrollBars:= Value;
    UpdateAutoScrollBars;
   end;
end;


{---------------------------------------------------------------------------------------------------------------
  Show/hide vertical scrollbar based on whether content exceeds the visible area.
  Requires ScrollBars to be ssVertical or ssBoth (the window needs WS_VSCROLL style).
  If ScrollBars = ssNone, this has no effect.
---------------------------------------------------------------------------------------------------------------}
procedure TLightMemo.UpdateAutoScrollBars;
VAR
  si: TScrollInfo;
begin
 if FUpdatingScrollBars then EXIT;          { Prevent recursion: ShowScrollBar can trigger WM_SIZE }
 if NOT HandleAllocated then EXIT;
 if NOT FAutoScrollBars then EXIT;

 FUpdatingScrollBars:= TRUE;
 TRY
   FillChar(si, SizeOf(si), 0);
   si.cbSize:= SizeOf(TScrollInfo);
   si.fMask:= SIF_RANGE or SIF_PAGE;
   GetScrollInfo(Handle, SB_VERT, si);

   ShowScrollBar(Handle, SB_VERT, si.nMax >= Integer(si.nPage));
 FINALLY
   FUpdatingScrollBars:= FALSE;
 END;
end;




function TLightMemo.LineToCharNo(CONST Line: Integer): Integer;                                    { Gets the character index of the first character of a specified line in a multiline edit control. INDEXED IN ZERO! }
begin
 Result:= Perform(EM_LINEINDEX, Line, 0);
end;

function TLightMemo.CurLineToCharNo: Integer;                                                      { As above but for current line (line that has the cursor) }
begin
 Result:= Perform(EM_LINEINDEX, 0, 0)
end;

function TLightMemo.CharToLine(CONST CharNo: Integer): Integer;                                    { Gets the index of the line that contains the specified character index in a multiline edit control.  }
begin
 Result:= Perform(EM_LINEFROMCHAR, CharNo, 0)                                                      { CharNo = The character index of the character contained in the line whose number is to be retrieved. If this parameter is -1, EM_LINEFROMCHAR retrieves either the line number of the current line (the line containing the caret) or, if there is a selection, the line number of the line containing the beginning of the selection.  }
end;

function TLightMemo.CursorToChar(CONST MousePosition: TPoint): Integer;                            { Returns the index of the character closest to the [mouse pointer] }
begin
 if (MousePosition.X < 0) OR (MousePosition.Y< 0) then EXIT(0);
 Result:= LoWord(Perform(EM_CHARFROMPOS, 0, MakeLong(MousePosition.x, MousePosition.Y)));
end;

function TLightMemo.ConvertCaretToChar: Integer;   { Returns the index of the character under caret }
VAR  Pt: TPoint;
begin
 WinApi.Windows.GetCaretPos(Pt);                                                                          { caret coordinates in pixels }

 { Negative coordinates mean the caret is scrolled out of view.
   EM_CHARFROMPOS raises RangeCheckError with negative coords, so we return 0 (first char). }
 if (Pt.X < 0) OR (Pt.Y < 0) then EXIT(0);

 Result:= LoWord(Perform(EM_CHARFROMPOS, 0, MakeLong(Pt.X, Pt.Y)));
end;


procedure TLightMemo.MoveCaretToChar(CharIndex: Integer);   { CharIndex is the index of the specified character in the Text }
begin
 SelStart:= CharIndex;
 SelLength:= 0;
end;











{ SEARCH }

{ Search for a string in the memo text.

  SearchOptions control the behavior:
    soIgnoreCase: Case-insensitive search
    soFromStart:  Always start searching from the beginning (use once, then disable to continue)
    soWrap:       Wrap to beginning if not found from current position

  CursorAfterSearch property:
    FALSE (default): Highlights/selects the found text
    TRUE: Places cursor after the found word without selection

  Returns TRUE if the string was found, FALSE otherwise.
  When found, centers the result in view. }
function TLightMemo.Search(SrcStr: string): Boolean;      //http://stackoverflow.com/questions/4232709/search-thru-a-memo-in-delphi
VAR
  sText: string;
  Index: Integer;
begin
  if soIgnoreCase in SearchOptions then
   begin
    SrcStr:= UpperCase(SrcStr);
    sText := UpperCase(Text);
   end
  else
   sText := Text;

  Index := 0;
  { First, try searching from current position (after current selection) }
  if NOT (soFromStart in SearchOptions)
  then Index := PosEx(SrcStr, sText, SelStart + SelLength + 1);

  { If not found, and wrap or from-start is enabled, search from beginning }
  if (Index = 0) AND ((soFromStart in SearchOptions) OR (soWrap in SearchOptions))
  then Index := PosEx(SrcStr, sText, 1);

  Result:= Index > 0;
  if Result then
   if CursorAfterSearch then                                                                       { Place cursor after the word (no selection) }
    begin
     SelStart := Index + Length(SrcStr) - 1;
     SelLength:= 0;
    end
   else
    begin                                                                                          { Highlight the found text }
     SelStart := Index - 1;
     SelLength:= Length(SrcStr);
    end;

 CenterInView;
end;


{ Returns the word under the caret position.
  Word boundaries are determined by spaces.
  Returns empty string if caret is not on a word. }
// https://stackoverflow.com/questions/6339446/delphi-get-the-whole-word-where-the-caret-is-in-a-memo
function TLightMemo.GetWordUnderCaret: string;
VAR
   Line    : Integer;
   Column  : Integer;
   LineText: string;
   InitPos : Integer;
   EndPos  : Integer;
begin
   Result:= '';

   //Get the caret position
   Line   := Perform(EM_LINEFROMCHAR, SelStart, 0);
   Column := SelStart - Perform(EM_LINEINDEX, Line, 0);
   //Validate the line number
   if (Line < 0) OR (Line >= Lines.Count) then EXIT;

   //Get the text of the line
   LineText := Lines[Line];
   if LineText = '' then EXIT;

   //Validate column is within line bounds
   Inc(Column);
   if (Column < 1) OR (Column > Length(LineText)) then EXIT;

   //search the initial position using the space symbol as separator
   InitPos := Column;
   WHILE (InitPos > 0) AND (LineText[InitPos] <> ' ')
     DO Dec(InitPos);

   //search the final position using the space symbol as separator
   EndPos := Column;
   while (EndPos <= Length(LineText)) and (LineText[EndPos] <> ' ') do Inc(EndPos);

   //Get the text
   Result := System.SysUtils.Trim(Copy(LineText, InitPos + 1, EndPos - InitPos - 1));
end;


{ Returns and selects the word under the caret position.
  Word boundaries are determined by spaces.
  Returns empty string if caret is not on a word. }
function TLightMemo.SelectWordUnderCaret: string;
VAR
   Line    : Integer;
   Column  : Integer;
   LineText: string;
   InitPos : Integer;
   EndPos  : Integer;
begin
   Result:= '';

   //Get the caret position
   Line   := Perform(EM_LINEFROMCHAR, SelStart, 0);
   Column := SelStart - Perform(EM_LINEINDEX, Line, 0);
   //Validate the line number
   if (Line < 0) OR (Line >= Lines.Count) then EXIT;

   //Get the text of the line
   LineText := Lines[Line];
   if LineText = '' then EXIT;

   //Validate column is within line bounds
   Inc(Column);
   if (Column < 1) OR (Column > Length(LineText)) then EXIT;

   //search the initial position using the space symbol as separator
   InitPos := Column;
   while (InitPos > 0) and (LineText[InitPos] <> ' ') do Dec(InitPos);

   //search the final position using the space symbol as separator
   EndPos := Column;
   while (EndPos <= Length(LineText)) and (LineText[EndPos] <> ' ') do Inc(EndPos);

   //Get the text
   Result := System.SysUtils.Trim(Copy(LineText, InitPos + 1, EndPos - InitPos - 1));

   //Finally select the text in the Memo
   if Result <> '' then
   begin
     SelStart  := Perform(EM_LINEINDEX, Line, 0) + InitPos;
     SelLength := Length(Result);
   end;
end;


function TLightMemo.CountWords: integer;
VAR
  i: Integer;
  txt: string;
  ThisWhite, PrevWhite: boolean;
begin
  txt:= Text;
  result:= 0;
  PrevWhite := true;
  for i := 1 to Length(txt) do
   begin
     ThisWhite := txt[i].IsWhiteSpace; // del Character.IsWhiteSpace(txt[i]);
     if PrevWhite AND NOT ThisWhite
     then inc(result);
     PrevWhite := ThisWhite;
   end;
end;


procedure TLightMemo.AddInteger(Number: Integer);
begin
 Lines.Add(IntToStr(Number));
end;

procedure TLightMemo.AddString(CONST aText: string);
begin
 Lines.Add(aText);
end;

procedure TLightMemo.AddEntry(CONST aText: string; Number: Integer);
begin
 Lines.Add(aText+ IntToStr(Number));
end;

procedure TLightMemo.AddSeparator;
begin
 Lines.Add(CRLFw+ '____________________'+ CRLFw);
end;










procedure TLightMemo.KeyDown(VAR Key: Word; Shift: TShiftState);
begin
 inherited;

 { "Select All" (CTRL+A) functionality }
 if (Key = Ord('A')) and (ssCtrl in Shift) then                                                    { http://delphi.about.com/od/adptips2004/a/bltip0804_4.htm }
  begin
    SelectAll;
    Key:= 0;
  end;

 { Overwrite capabilities }
 if (Key = VK_INSERT) AND (Shift = []) then
  if TypeMode= tmInsert    then TypeMode:= tmOverwrite else
  if TypeMode= tmOverwrite then TypeMode:= tmInsert;
end;


{ Overwrite capabilities }
procedure TLightMemo.KeyPress(VAR Key: Char);
begin
 inherited;
 if (SelLength = 0) AND (TypeMode= tmOverwrite)
 then SelLength := 1;
end;






{ Move current line up by swapping with the line above.
  Does nothing if already at the first line. Cursor stays on same line number. }
procedure TLightMemo.LineUp;
begin
 if CurrentLine > 0
 then SwapLines(CurrentLine, CurrentLine - 1);
end;


{ Move current line down by swapping with the line below, keeping cursor on the moved line }
procedure TLightMemo.LineDown;
VAR NewPos: TPoint;
begin
 if CurrentLine < Lines.Count-1 then
  begin
   SwapLines(CurrentLine, CurrentLine + 1);

   { Move cursor down to follow the line }
   NewPos.X:= CaretPos.X;
   NewPos.Y:= CaretPos.Y + 1;
   CaretPos:= NewPos;
  end;
end;



{ Returns the approximate number of characters that can fit on a single line.
  Uses average character width from text metrics, so actual fit varies with proportional fonts.
  Note: Function name has a typo (LineLenght) preserved for backward compatibility. }
function TLightMemo.LineLenght: integer;
Var
  Oldfont: HFont;
  DC: THandle;
  Tm: TTextMetric;
  TheRect: TRect;
begin
  DC:= GetDC(Handle);
  TRY
    OldFont:= SelectObject(DC, Font.Handle);
    TRY
      GetTextMetrics(DC, Tm);
      Perform(EM_GETRECT, 0, lparam(@TheRect));                        { Fixed for Win64 compatibility }
      Result:= (TheRect.Right - TheRect.Left) div (Tm.tmAveCharWidth);
    FINALLY
      SelectObject(DC, Oldfont);
    end;
  FINALLY
    ReleaseDC(Handle, DC);
  end;
end;


{ Returns the maximum number of text lines that can be displayed in the memo's visible area.
  Calculated by dividing the client rectangle height by the font's line height. }
function TLightMemo.MaxVisLines: integer;
Var
  Oldfont: HFont;
  DC: THandle;
  Tm: TTextMetric;
  TheRect: TRect;
begin
  DC:= GetDC(Handle);
  TRY
    OldFont:= SelectObject(DC, Font.Handle);
    try
      GetTextMetrics(DC, Tm);
      Perform(EM_GETRECT, 0, lparam(@TheRect));                        { Fixed for Win64 compatibility }
      Result:= (TheRect.Bottom - TheRect.Top) div (Tm.tmHeight);
    FINALLY
      SelectObject(DC, Oldfont);
    end;
  FINALLY
    ReleaseDC(Handle, DC);
  end;
end;




function TLightMemo.CurrentLine: Integer;      { Line containing the cursor. Indexed in zero }
begin
 Result:= CaretPos.Y;
end;


procedure TLightMemo.MoveCursorAtEOL;                                                              { Move cursor at the end of the current row }
VAR LineStart, LineLen: Integer;
begin
 LineStart:= Perform(EM_LINEINDEX, CaretPos.Y, 0);      { Get the index of the first character of the current line }
 LineLen  := Perform(EM_LINELENGTH, LineStart, 0);      { Get the length of the current line }
 SelStart := LineStart + LineLen;                       { Position cursor at the end of the line }
 SelLength:= 0;
end;


{ Select the specified line by index. Does nothing if line index is out of bounds. }
procedure TLightMemo.SelectLine(Line : Integer);                                                   { http://delphi.about.com/od/adptips2005/qt/memoselectline.htm }
begin
 if (Line < 0) OR (Line >= Lines.Count) then EXIT;

 SelStart  := Perform(EM_LINEINDEX, Line, 0);
 SelLength := Length(Lines[Line]);
end;


procedure TLightMemo.RemoveSelection;
begin
 SelStart := 0;
 SelLength:= 0;
end;


procedure TLightMemo.SelectLine(aText: string);       { Select line containing specified text }
VAR iPos: Integer;
begin
 iPos:= FindLine(aText);
 if iPos > -1
 then SelectLine(iPos);
end;


procedure TLightMemo.SelectCurrentLine;
begin
 SelectLine(Perform(EM_LINEFROMCHAR, SelStart, 0))
end;


function TLightMemo.FindLine(aText: string): Integer;   { Case insensitive }
VAR i: Integer;
begin
 Result:= -1;
 for i:= 0 to Lines.Count- 1 DO
  if SameText(Lines[i], aText) then EXIT(i);
end;












{ Set internal text margins (padding) within the memo control.
  This creates whitespace around the text without changing the control's outer bounds.
  Parameters are in pixels. TopBottomMargin is applied to both top and bottom. }
procedure TLightMemo.SetMargins(CONST LeftMargin, RightMargin, TopBottomMargin: Integer);
VAR R : TRect;
begin
 R       := ClientRect;
 R.Left  := R.Left + LeftMargin;
 R.Top   := R.Top  + TopBottomMargin;
 R.Bottom:= R.Bottom - TopBottomMargin;
 R.Right := R.Right - RightMargin;
 SendMessage(Handle, EM_SETRECT, 0, LPARAM(@R));
end;


{ Sort lines }
procedure TLightMemo.SortLines;
VAR SortList: TStringList;
begin
  SortList := TStringList.Create;
  try
    SortList.Assign(Lines);
    SortList.Sort;
    Lines.Text:= SortList.Text;
  finally
    FreeAndNil(SortList);
  end;
end;


procedure TLightMemo.Randomize;
VAR
   i, NewLine: Integer;
begin
 Lines.BeginUpdate;
 for i:= 0 to Lines.Count-1 DO
  begin
   NewLine:= Random(Lines.Count);
   SwapLines(i, NewLine);
  end;
 Lines.EndUpdate;
end;


{ Swap two lines in the memo. Does nothing if indices are equal or out of bounds. }
procedure TLightMemo.SwapLines(x, y: Integer);
VAR s: string;
begin
 if x = y then EXIT;
 if (x < 0) OR (x >= Lines.Count) then EXIT;
 if (y < 0) OR (y >= Lines.Count) then EXIT;

 s:= Lines[x];
 Lines[x]:= Lines[y];
 Lines[y]:= s;
end;


procedure TLightMemo.CopyToClipboardAll;                                                           { CopyToClipboard only copies the text if it is selected }
begin
 selectall;
 CopyToClipboard;
 SetSelLength(0);
end;























procedure TLightMemo.RemoveEmptyLines;
VAR i: Integer;
begin
 for i:= Lines.Count-1 downto 0 DO
  if Lines[i]= ''
  then Lines.Delete(i);
end;


{ Applies trim before removing empty lines. This way more lines will become empty }
procedure TLightMemo.RemoveEmptyLinesEx;
VAR i: Integer;
begin
 Trim;                                               { Trim empty spaces and control caracters at the begining/end of each line }
 for i:= Lines.Count-1 downto 0 DO
   if Lines[i]= ''
   then Lines.Delete(i);
end;


procedure TLightMemo.RemoveLastEmptyLine;
begin
 Text:= LightCore.RemoveLastEnter(Text);
end;


{ Remove duplicate lines from the memo (case-insensitive comparison).
  Returns the number of lines removed.
  Uses a TStringList copy for performance (avoids GUI refresh on each delete).
  Periodically calls Update to keep GUI responsive during large operations. }
function TLightMemo.RemoveDuplicates: Integer;
VAR i1, i2, IsBreakTime: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
  TSL.Assign(Lines);                                                                               { Work on copy - direct edits would trigger slow GUI refreshes }
  Result:= 0;
  IsBreakTime:= 0;

  { Compare each line with all previous lines, delete if duplicate found }
  for i1:= TSL.Count-1 downto 1 DO
   begin
    { Refresh GUI periodically during long operations }
    inc(IsBreakTime);
    if IsBreakTime > 8000 then
     begin
      Update;
      IsBreakTime:= 0;
     end;

    { Check if current line is duplicate of any earlier line }
    for i2:= i1-1 downto 0 DO
     if SameText(TSL.Strings[i1], TSL.Strings[i2]) then
      begin
       TSL.Delete(i1);
       inc(Result);
       Break;                                                                                      { Found duplicate, no need to check further }
      end;
   end;

  Lines.Text:= TSL.Text;
 FINALLY
   FreeAndNil(TSL);
 END;
end;


{ Remove all lines that contains the specified text.
  The function is case INSENSITIVE }
function TLightMemo.RemoveLines(const BadWord: string; PartialMatch: Boolean): Integer;
VAR Before, i: Integer;
begin
 Before:= Lines.Count;

 if PartialMatch then
   begin
     for i:= Lines.Count-1 downto 0 DO
       if PosInsensitive(BadWord, Lines[i]) > 0
       then Lines.Delete(i);
   end
 else
   for i:= Lines.Count-1 downto 0 DO
     if SameText(BadWord, Lines[i])
     then Lines.Delete(i);

 Result:= Before - Lines.Count;
end;


{ Remove all lines that does not contain this text }
function TLightMemo.KeepLines(CONST KeepText: string): Integer;
VAR i: Integer;
begin
 Result:= 0;
 for i:= Lines.Count-1 downto 0 DO
  if PosInsensitive(KeepText, Lines[i])= 0 then { if text not found, then delete line }
   begin
    Lines.Delete(i);
    Inc(Result);
   end;
end;



{ Delete all lines except the first 'HowManyLines' ones at the top.
  Does nothing if HowManyLines <= 0 or exceeds line count. }
procedure TLightMemo.KeepFirstLines(CONST HowManyLines: Integer);
VAR i: Integer;
begin
 if HowManyLines <= 0 then EXIT;
 for i:= Lines.Count-1 downto HowManyLines
   DO Lines.Delete(i);
end;


{ Trim empty spaces and control caracters at the begining/end of each line }
procedure TLightMemo.Trim;
VAR i: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;       { We do all the edits (trimming) in a non-visual control because it is WAY much faster then to update each line of a visual memo. Maybe I should use BeginUpdate }
 TRY
  TSL.Assign(Lines);

  for i:= TSL.Count-1 downto 0
   DO TSL[i]:= System.SysUtils.Trim(TSL[i]);

  Lines.Text:= TSL.Text;
 FINALLY
   FreeAndNil(TSL);
 END;
end;




{--------------------------------------------------------------------------------------------------
   SCROLL
--------------------------------------------------------------------------------------------------}
{ Center the specified line in the middle of the visible view.
  LineNum: 0-indexed line number to center.
  Does nothing if LineNum is out of bounds. }
procedure TLightMemo.CenterInView(LineNum: Integer);                                               { Source: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
var
  VisibLines: Integer;
  TopLine: Integer;
  FirstLine: Integer;
begin
  if (LineNum < 0) OR (LineNum >= Lines.Count) then EXIT;

  FirstLine := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  VisibLines := Round(ClientHeight / Abs(Font.Height));

  if VisibLines <= 1
  then TopLine := LineNum
  else TopLine := Max(LineNum - Round((VisibLines/2)) + 1, 0);

  if FirstLine <> TopLine
  then Perform(EM_LINESCROLL, 0, TopLine - FirstLine);
end;


procedure TLightMemo.CenterInView;
begin
 CenterInView(CaretPos.Y);
end;




procedure TLightMemo.ScrollAtEnd;
begin
 SelStart:= Length(Text);
 SelLength:= 0;
 Perform(EM_SCROLLCARET, 0, 0);
end;


procedure TLightMemo.ScrollAtTop;
begin
 SelStart:= 0;                                          { SelStart is 0-indexed }
 SelLength:= 0;
 Perform(EM_SCROLLCARET, 0, 0);
end;


function TLightMemo.VisibleLines: Integer;                                                         { From here: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
VAR C: Integer;
begin
  C := Font.Height;
  If C < 0 then C := C-2;             // If negative, add a bit for leading
  Result:= Abs(ClientHeight div C);
end;


function TLightMemo.CountNonEmptyLines: Integer;                                                         { From here: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
VAR s: string;
begin
 Result:= 0;
 for s in Lines DO
  if s<> '' then Inc(Result);
end;







{--------------------------------------------------------------------------------------------------
   FIX UNIX ENTERS
--------------------------------------------------------------------------------------------------}
{PUT IT BACK

THIS INTORDUCES A BUG: http://stackoverflow.com/questions/28463556/wmsettext-breaks-custom-styles }

procedure TLightMemo.WMSetText(var Message: TWMSetText);
var
  s: string;
begin
  s := Message.Text;
  s := AdjustLineBreaks(s);            { Convert text containing Mac and Unix-style line endings (LF) to Win }
  Message.Text := PChar(s);
  inherited;
end;

procedure TLightMemo.WMPaste(var Message: TWMPaste);
var
  s: string;
begin
  if Clipboard.HasFormat(cf_Text) then
  begin
    s := Clipboard.AsText;
    s := AdjustLineBreaks(s);    { Convert text containing Mac and Unix-style line endings (LF) to Win }
    SelText := s;
  end;
end;









procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightMemo]);
 // RegisterComponents('LightSaber VCL', [TLightMemo2]);
end;


end.