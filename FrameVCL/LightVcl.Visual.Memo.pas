UNIT LightVcl.Visual.Memo;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
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

   Note: The 'Text' property is MUCH faster than Assign. Details: https://stackoverflow.com/questions/46990961/tmemo-is-painfuly-slow-when-working-with-large-number-of-lines

   ToDo: 
     http://stackoverflow.com/questions/10212488/tmemo-with-auto-show-hide-scrollbars
     http://delphi.xcjc.net/viewthread.php?tid=44882

See: What's the difference between CreateWnd and CreateWindowHandle? https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle
=============================================================================================================}

INTERFACE
{$WARN GARBAGE OFF}   {Silence the: 'W1011 Text after final END' warning }

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.Math, System.Character, System.StrUtils,
  Vcl.Clipbrd, Vcl.StdCtrls, Vcl.Controls, VCL.graphics, Vcl.Themes;

TYPE
  TTypeMode= (tmInsert, tmOverwrite);

  TSearchOption = (soIgnoreCase, soFromStart, soWrap);   { soFromStart:  If this is active, the search will start from the top of the memo. But after this, I have to uncheck it so the search can continue, else it will not advance (it will always search from start) }
  TSearchOptions = set of TSearchOption;

  TCubicMemo = class(TMemo)
   strict private
   private
     FCursAfter: Boolean;                                                 { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection) }
     FTypeMode: TTypeMode;
   protected
     procedure KeyDown   (VAR Key: Word; Shift: TShiftState); override;   { "Select All" (CTRL+A) functionality }
     procedure KeyPress  (VAR Key: Char);                     override;
     procedure CreateWnd; override;
   public
     SearchOptions: TSearchOptions;
     constructor Create  (AOwner: TComponent); override;
     destructor Destroy; override;

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
     function  RemoveLines (const BadWord : string; PartialMatch: Boolean): Integer;             { Remove lines that contain the specified text }
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
     function  LineLenght: Integer;                                      { Returns number of characters that can fit on a line }
     function  MaxVisLines: Integer;                                     { Returns the number of lines that fits in the TMemo }

     procedure SetMargins(CONST LeftMargin, RightMargin, TopBottomMargin: Integer);               { Set margins }
     function  LoadFromFile(FileName: string): Boolean;        { Loadtext from specified file IF the file exists. Otherwise, don't show an error }
   published
     property CursorAfterSearch: Boolean   read FCursAfter write  FCursAfter default FALSE;       { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection). Used in Matthias Kloss' program }
     property TypeMode         : TTypeMode read FTypeMode  write  FTypeMode;
  end;




procedure Register;

IMPLEMENTATION

USES
   LightCore.TextFile, LightVcl.Common.IO, LightCore, LightCore.Time;

   {
How to know when the USER changed the text in a TMemo/TEdit?
   https://stackoverflow.com/questions/41719647/how-to-know-when-the-user-changed-the-text-in-a-tmemo-tedit/56832800#56832800


How about using the Modified property?

procedure TForm1.MyEditChange(Sender: TObject);
begin
    if MyEdit.Modified then
    begin
        // The user changed the text since it was last reset (i.e. set programmatically)
    end;
end;}



Constructor TCubicMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);    // Note: Don't set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
  FTypeMode:= tmInsert;
  HideSelection:= FALSE;                                                                            { Show the selection even if the memo isn't focussed. }
  SearchOptions:= [soIgnoreCase, soWrap];
  FCursAfter:= FALSE;                                                                               { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection) }

  {This won't work: it says: TMemoStyleHook is already registered for TMyMemo. }
  // TCustomStyleEngine.RegisterStyleHook(TCubicMemo, TMemoStyleHook);     { Fix: http://stackoverflow.com/questions/28463556/vcl-styles-breaks-randomly }
end;


procedure TCubicMemo.CreateWnd;
begin
  inherited CreateWnd;
  //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
end;


destructor TCubicMemo.Destroy;
begin
  {This seems to work: }
  TCustomStyleEngine.UnRegisterStyleHook(TCubicMemo, TMemoStyleHook);   { Fix: http://stackoverflow.com/questions/28463556/vcl-styles-breaks-randomly }
  inherited;
end;


{ Load text from specified file IF the file exists. Otherwise, don't show an error }
function TCubicMemo.LoadFromFile(FileName: string): Boolean;
begin
  Result:= FileExists(FileName) AND NOT FileIsLockedR(FileName);
  if Result
  then Lines.LoadFromFile(FileName);  // Text:= StringFromFile(FileName)
end;






function TCubicMemo.LineToCharNo(CONST Line: Integer): Integer;                                    { Gets the character index of the first character of a specified line in a multiline edit control. INDEXED IN ZERO! }
begin
 Result:= Perform(EM_LINEINDEX, Line, 0);
end;

function TCubicMemo.CurLineToCharNo: Integer;                                                      { As above but for current line (line that has the cursor) }
begin
 Result:= Perform(EM_LINEINDEX, 0, 0)
end;

function TCubicMemo.CharToLine(CONST CharNo: Integer): Integer;                                    { Gets the index of the line that contains the specified character index in a multiline edit control.  }
begin
 Result:= Perform(EM_LINEFROMCHAR, CharNo, 0)                                                      { CharNo = The character index of the character contained in the line whose number is to be retrieved. If this parameter is –1, EM_LINEFROMCHAR retrieves either the line number of the current line (the line containing the caret) or, if there is a selection, the line number of the line containing the beginning of the selection.  }
end;

function TCubicMemo.CursorToChar(CONST MousePosition: TPoint): Integer;                            { Returns the index of the character closest to the [mouse pointer] }
begin
 if (MousePosition.X < 0) OR (MousePosition.Y< 0) then EXIT(0);
 Result:= LoWord(Perform(EM_CHARFROMPOS, 0, MakeLong(MousePosition.x, MousePosition.Y)));
end;

function TCubicMemo.ConvertCaretToChar: Integer;   { Returns the index of the character under caret }
VAR  Pt: TPoint;
begin
 WinApi.Windows.GetCaretPos(Pt);                                                                          { caret coordinates in pixels }

 if Pt.x< 0 then EXIT(0); {fix this later}
 if Pt.y< 0 then EXIT(0); {fix this later}
 {imi da RangecheckError in linia de mai jos can cursorul e out of screen si apas o tasta }

 Result:= LoWord(Perform(EM_CHARFROMPOS, 0, MakeLong(Pt.x, Pt.Y)));
end;


procedure TCubicMemo.MoveCaretToChar(CharIndex: Integer);   { CharIndex is the index of the specified character in the Text }
begin
 SelStart:= CharIndex;
 SelLength:= 0;
end;











{ SEARCH }

function TCubicMemo.Search(SrcStr: string): Boolean;      //http://stackoverflow.com/questions/4232709/search-thru-a-memo-in-delphi
VAR
  sText: string;
  Index: Integer;
begin
  if soIgnoreCase in SearchOptions
  then
   begin
    SrcStr:= UpperCase(SrcStr);
    sText := UpperCase(Text);
   end
  else
   sText := Text;

  Index := 0;
  if NOT (soFromStart in SearchOptions)
  then Index := PosEx(SrcStr, sText, SelStart + SelLength + 1);

  if (Index = 0) AND ((soFromStart in SearchOptions) OR (soWrap in SearchOptions))
  then Index := PosEx(SrcStr, sText, 1);

  Result:= Index > 0;
  if Result then
   if CursorAfterSearch                                                                            { If TRUE, instead of highlighting the found word, place the cursor after the word (no selection) }
   then
    begin
     SelStart := Index + Length(SrcStr) - 1;
     SelLength:= 0;
    end
   else
    begin
     SelStart := Index - 1;
     SelLength:= Length(SrcStr);
    end;

 CenterInView;
end;


// https://stackoverflow.com/questions/6339446/delphi-get-the-whole-word-where-the-caret-is-in-a-memo
function TCubicMemo.GetWordUnderCaret: string;
VAR
   Line    : Integer;
   Column  : Integer;
   LineText: string;
   InitPos : Integer;
   EndPos  : Integer;
begin
   //Get the caret position
   Line   := Perform(EM_LINEFROMCHAR, SelStart, 0) ;
   Column := SelStart - Perform(EM_LINEINDEX, Line, 0) ;
   //Validate the line number
   if Lines.Count-1 < Line then Exit;

   //Get the text of the line
   LineText := Lines[Line];

   //search the initial position using the space symbol as separator
   Inc(Column);
   InitPos := Column;
   WHILE (InitPos > 0) AND (InitPos <= Length(LineText)) AND (LineText[InitPos] <> ' ')
     DO Dec(InitPos);
   Inc(Column);

   EndPos := Column;
   //search the final position using the space symbol as separator
   while (EndPos <= Length(LineText)) and (LineText[EndPos] <> ' ') do Inc(EndPos);

   //Get the text
   Result := System.SysUtils.Trim(Copy(LineText, InitPos, EndPos - InitPos));
end;


function TCubicMemo.SelectWordUnderCaret: string;
VAR
   Line    : Integer;
   Column  : Integer;
   LineText: string;
   InitPos : Integer;
   EndPos  : Integer;
begin
   //Get the caret position
   Line   := Perform(EM_LINEFROMCHAR, SelStart, 0) ;
   Column := SelStart - Perform(EM_LINEINDEX, Line, 0) ;
   //Validate the line number
   if Lines.Count-1 < Line then Exit;

   //Get the text of the line
   LineText := Lines[Line];

   Inc(Column);
   InitPos := Column;
   //search the initial position using the space symbol as separator
   while (InitPos > 0) and (LineText[InitPos] <> ' ') do Dec(InitPos);
   Inc(Column);

   EndPos := Column;
   //search the final position using the space symbol as separator
   while (EndPos <= Length(LineText)) and (LineText[EndPos] <> ' ') do Inc(EndPos);

   //Get the text
   Result := System.SysUtils.Trim(Copy(LineText, InitPos, EndPos - InitPos));

   //Finally select the text in the Memo
   SelStart  := Perform(EM_LINEINDEX, Line, 0)+InitPos;
   SelLength := Length(Result);
end;


function TCubicMemo.CountWords: integer;
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


procedure TCubicMemo.AddInteger(Number: Integer);
begin
 Lines.Add(IntToStr(Number));
end;

procedure TCubicMemo.AddString(CONST aText: string);
begin
 Lines.Add(aText);
end;

procedure TCubicMemo.AddEntry(CONST aText: string; Number: Integer);
begin
 Lines.Add(aText+ IntToStr(Number));
end;

procedure TCubicMemo.AddSeparator;
begin
 Lines.Add(CRLFw+ '____________________'+ CRLFw);
end;










procedure TCubicMemo.KeyDown(VAR Key: Word; Shift: TShiftState);
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
procedure TCubicMemo.KeyPress(VAR Key: Char);
begin
 inherited;
 if (SelLength = 0) AND (TypeMode= tmOverwrite)
 then SelLength := 1;
end;






procedure TCubicMemo.LineUp;     { Move current line up }
begin
 if CurrentLine > 0
 then SwapLines(CurrentLine, CurrentLine - 1);
end;


procedure TCubicMemo.LineDown;     { Move current line Down }
VAR a: TPoint;
begin
 if CurrentLine < Lines.Count-1 then
  begin
   SwapLines(CurrentLine, CurrentLine + 1);

   { move also the cursor down }
   a.x := CaretPos.X;
   a.y := CaretPos.Y;
   CaretPos := a;
  end;
end;



function TCubicMemo.LineLenght: integer;                               { Returns number of characters that can fit on a line }
Var
  Oldfont: HFont;                                                      {the old font}
  DC: THandle;                                                         {Device context handle}
  Tm: TTextMetric;                                                     {text metric structure}
  TheRect: TRect;
begin
  DC:= GetDC(Handle);                                                  {Get the memo's device context}
  TRY
    OldFont:= SelectObject(DC, Font.Handle);                           {Select the memo's font}
    TRY
      GetTextMetrics(DC, Tm);                                          {Get the text metric info}
      Perform(EM_GETRECT, 0, lparam(@TheRect));                        //done: fixed problems with win64 compatibility: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows                                        { function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint; }
      Result:= (TheRect.Right - TheRect.Left) div (Tm.tmAveCharWidth); // +Tm.tmExternalLeading
    FINALLY
      SelectObject(DC, Oldfont);                                       {Select the old font}
    end;
  FINALLY
    ReleaseDC(Handle, DC);                                             {Release the device context}
  end;
end;


function TCubicMemo.MaxVisLines: integer;                              { Returns the number of lines that fits in the TMemo }
Var
  Oldfont: HFont;                                                      {the old font}
  DC: THandle;                                                         {Device context handle}
  Tm: TTextMetric;                                                     {text metric structure}
  TheRect: TRect;
begin
  DC:= GetDC(Handle);                                                  {Get the memo's device context}
  TRY
    OldFont:= SelectObject(DC, Font.Handle);                           {Select the memo's font}
    try
      GetTextMetrics(DC, Tm);                                          {Get the text metric info}
      Perform(EM_GETRECT, 0, lparam(@TheRect));                        //done: fixed problems with win64 compatibility: http://docwiki.embarcadero.com/RADStudio/Seattle/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows
      Result:= (TheRect.Bottom - TheRect.Top) div (Tm.tmHeight);
    FINALLY
      SelectObject(DC, Oldfont);                                       {Select the old font}
    end;
  FINALLY
    ReleaseDC(Handle, DC);                                             {Release the device context}
  end;
end;




function TCubicMemo.CurrentLine: Integer;      { Line containing the cursor. Indexed in zero }
begin
 Result:= CaretPos.Y;
end;


procedure TCubicMemo.MoveCursorAtEOL;                                                              { Move cursor at the end of the current row }
begin
 SelStart := Perform(EM_LINEINDEX, CaretPos.Y, 0) ;
 SelLength:= 0;
end;


procedure TCubicMemo.SelectLine(Line : Integer);                                                   { http://delphi.about.com/od/adptips2005/qt/memoselectline.htm }
begin
 if Line >= Lines.Count then EXIT;

 SelStart  := Perform(EM_LINEINDEX, Line, 0);
 SelLength := Length(Lines[Line]);
end;


procedure TCubicMemo.RemoveSelection;
begin
 SelStart := 0;
 SelLength:= 0;
end;


procedure TCubicMemo.SelectLine(aText: string);       { Select line containing specified text }
VAR iPos: Integer;
begin
 iPos:= FindLine(aText);
 if iPos > -1
 then SelectLine(iPos);
end;


procedure TCubicMemo.SelectCurrentLine;
begin
 SelectLine(Perform(EM_LINEFROMCHAR, SelStart, 0))
end;


function TCubicMemo.FindLine(aText: string): Integer;   { Case insensitive }
VAR i: Integer;
begin
 Result:= -1;
 for i:= 0 to Lines.Count- 1 DO
  if SameText(Lines[i], aText) then EXIT(i);
end;












{ Set margins }
procedure TCubicMemo.SetMargins(CONST LeftMargin, RightMargin, TopBottomMargin: Integer);
VAR R : TRect;
begin
 R       := ClientRect;
 R.Left  := R.Left + LeftMargin;
 R.Top   := R.Top  + TopBottomMargin;
 R.Bottom:= R.Bottom- TopBottomMargin;
 R.Right := R.Right - RightMargin;
 SendMessage(Handle, EM_SETRECT, 0, LPARAM(@R));
end;


{ Sort lines }
procedure TCubicMemo.SortLines;
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


procedure TCubicMemo.Randomize;
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


procedure TCubicMemo.SwapLines(x, y: Integer);
VAR s: string;
begin
 if x = y then EXIT;

 s:= Lines[x];
 Lines[x]:= Lines[y];
 Lines[y]:= s;
end;


procedure TCubicMemo.CopyToClipboardAll;                                                           { CopyToClipboard only copies the text if it is selected }
begin
 selectall;
 CopyToClipboard;
 SetSelLength(0);
end;























procedure TCubicMemo.RemoveEmptyLines;
VAR i: Integer;
begin
 for i:= Lines.Count-1 downto 0 DO
  if Lines[i]= ''
  then Lines.Delete(i);
end;


{ Applies trim before removing empty lines. This way more lines will become empty }
procedure TCubicMemo.RemoveEmptyLinesEx;
VAR i: Integer;
begin
 Trim;                                               { Trim empty spaces and control caracters at the begining/end of each line }
 for i:= Lines.Count-1 downto 0 DO
  begin
   if (Lines[i]= '')
   OR (System.SysUtils.Trim(Lines[i])= '')
   then Lines.Delete(i);
  end;
end;


procedure TCubicMemo.RemoveLastEmptyLine;
begin
 Text:= LightCore.RemoveLastEnter(Text);
end;


function TCubicMemo.RemoveDuplicates: Integer;
VAR i1, i2, IsBreakTime: Integer;
    TSL: TStringList;
begin
 TSL:= TStringList.Create;
 TRY
  TSL.Assign(Lines);                                                                               { nu sterg direct din SELF pt ca o sa faca refresh si o sa fie prea lent }
  Result:= 0;
  IsBreakTime:= 0;

  for i1:= TSL.Count-1 downto 1 DO
   begin
    { Refresh GUI after a long operation }
    inc(IsBreakTime);
    if IsBreakTime> 8000 then
     begin
      Update;
      IsBreakTime:= 0;
     end;

    { Real processing takes place here }
    for i2:= i1-1 downto 0 DO
     if SameText(TSL.Strings[i1], TSL.Strings[i2]) then
      begin
       TSL.Delete(i1);
       inc(Result);
       Break;
      end;
   end;

  Lines.Text:= TSL.Text;
 FINALLY
   FreeAndNil(TSL);
 END;
end;


{ Remove all lines that contains the specified text.
  The function is case INSENSITIVE }
function TCubicMemo.RemoveLines(const BadWord: string; PartialMatch: Boolean): Integer;
VAR Before, i: Integer;
begin
 Before:= Lines.Count;

 if PartialMatch
 then
   for i:= Lines.Count-1 downto 0 DO
     if PosInsensitive(BadWord, Lines[i]) > 0
     then Lines.Delete(i)
     else
 else
   for i:= Lines.Count-1 downto 0 DO
     if SameText(BadWord, Lines[i])
     then Lines.Delete(i);

 Result:= Before - Lines.Count;
end;


{ Remove all lines that does not contain this text }
function TCubicMemo.KeepLines(CONST KeepText: string): Integer;
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



procedure TCubicMemo.KeepFirstLines(CONST HowManyLines: Integer);                                  { Delete all lines except_ the first 'HowManyLines' ones at the top }
VAR i: Integer;
begin
 for i:= Lines.Count-1 downto HowManyLines
   DO Lines.Delete(i);
end;


{ Trim empty spaces and control caracters at the begining/end of each line }
procedure TCubicMemo.Trim;
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
procedure TCubicMemo.CenterInView(LineNum: Integer);                                               { Center the specified line in the middle of the view } { Source: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
var
  VisibLines: Integer;
  TopLine: Integer;
  FirstLine: Integer;
begin
  FirstLine := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  VisibLines := Round(ClientHeight / Abs(Font.Height));

  if VisibLines <= 1
  then TopLine := LineNum
  else TopLine := Max(LineNum - Round((VisibLines/2)) + 1, 0);

  if FirstLine <> TopLine
  then Perform(EM_LINESCROLL, 0, TopLine - FirstLine);
end;


procedure TCubicMemo.CenterInView;
begin
 CenterInView(CaretPos.Y);
end;




procedure TCubicMemo.ScrollAtEnd;
begin
 SelStart:= Length(Text);
 SelLength:= 0;
 Perform(EM_SCROLLCARET, 0, 0);
end;


procedure TCubicMemo.ScrollAtTop;
begin
 SelStart:= 1;
 SelLength:= 0;
 Perform(EM_SCROLLCARET, 0, 0);
end;


function TCubicMemo.VisibleLines: Integer;                                                         { From here: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
VAR C:Integer;
begin
  C := Font.Height;
  If C < 0 then C := C-2;             // If - we add a bit for leading
  Result:= Abs(Height div C);
end;


function TCubicMemo.CountNonEmptyLines: Integer;                                                         { From here: http://stackoverflow.com/questions/2822471/delphi-center-specific-line-in-trichedit-by-scrolling }
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

procedure TCubicMemo.WMSetText(var Message: TWMSetText);
var
  s: string;
begin
  s := Message.Text;
  s := AdjustLineBreaks(s);            { Convert text containing Mac and Unix-style line endings (LF) to Win }
  Message.Text := PChar(s);
  inherited;
end;

procedure TCubicMemo.WMPaste(var Message: TWMPaste);
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
  RegisterComponents('LightSaber VCL', [TCubicMemo]);
 // RegisterComponents('LightSaber VCL', [TCubicMemo2]);
end;


end.{==============================================================================================





procedure TCubicMemo.ScrollAtEnd;    Del
VAR ScrollMessage:TWMVScroll;
    i:integer;
begin
 ScrollMessage.Msg:= WM_VScroll;
 for i := 0 to Lines.Count-1 do
  begin
   ScrollMessage.ScrollCode:= sb_LineDown;
   ScrollMessage.Pos:=0;
   Dispatch(ScrollMessage) ;
  end;
end; }

