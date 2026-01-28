UNIT LightCore.WrapString;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
==============================================================================================================

   Wraps and truncates strings

   !!!!!!!!!!!!!!
   see also: https://stackoverflow.com/questions/28603463/how-can-i-do-autosize-item-height-in-listview-for-variable-multirow-long-text-wi



=============================================================================================================}

INTERFACE

USES System.SysUtils, System.Classes;

 function  WrapStringForced (CONST s: string; MaxRowLength: integer): string; {old name WrapString }           { Makes sure that none of the lines of 's' is longer than RowLenght chars }     { Also exists: System.SysUtils.WrapText }
 function  WrapStringForcedA(CONST s: AnsiString; MaxRowLength: integer): AnsiString;

 function  UnWrapString     (CONST s: string): string;
 function  UnwrapText       (      s: string; Separators: string; RemoveExtraEnters, AddExtraSpace: Boolean): string;

 function TruncateToWord    (CONST s: string; MaxChars: Integer): string;


IMPLEMENTATION

USES LightCore, LightCore.StrBuilder;

{-----------------------------------------------------------------------------------------------------------------------
   ForcedWrapString
   This is used to break DNA (Fasta files) string in pieces
------------------------------------------------------------------------------------------------------------------------

  Also exists:
    System.SysUtils.WrapText (282ms vs 31ms my ForcedWrapString function).
    It will nicely (like in a Word doc) wrap the text.
    This means that some lines will be shorter than MaxRowLenght intentionally in order to keep the words unbreaked.
    It wraps the text also when it encounters a separator char (like space, tab).
    If the separator is not encountered, the text is never wrapped !!!
    So, it is great for splitting actual text (like in books) but not for DNA strings.

  ----------

  Speed test: 500x loop; test file: TesterForm.pas 2.7K ; wrap after 20 chars

    484ms    SysUtils.WrapText - unbuffered
    5788ms   WrapString        - unbuffered (Result:= Result+ s[i])
    31ms     WrapString        - buffered (cmStrBuilder)
-----------------------------------------------------------------------------------------------------------------------}

{ Wraps a string by inserting line breaks at fixed intervals.
  Unlike System.SysUtils.WrapText, this forces breaks regardless of word boundaries.
  Useful for DNA sequences, fixed-width data, etc. }
function WrapStringForced(const s: string; MaxRowLength: Integer): string;
var
   BufferSize, i, Row: Integer;
   StrBuilder: LightCore.StrBuilder.TCStringBuilder;
begin
  Row:= 0;
  Result:= '';
  BufferSize:= Length(s);
  BufferSize:= BufferSize + Round(BufferSize * 0.02);  { Add 2% buffer for line breaks }
  StrBuilder:= TCStringBuilder.Create(BufferSize);
  try
    for i:= 1 to Length(s) do
    begin
      Inc(Row);
      StrBuilder.AddChar(s[i]);
      if Row >= MaxRowLength then
      begin
        StrBuilder.AddEnter;
        Row:= 0;
      end;
    end;
    Result:= StrBuilder.AsText;
  finally
    FreeAndNil(StrBuilder);
  end;
end;



function WrapStringForcedA(CONST s: AnsiString; MaxRowLength: integer): AnsiString;    { Also exists: System.SysUtils.WrapText }
begin
 Result:= ansistring(WrapStringForced(string(s), MaxRowLength));
end;


{ Truncates string to full words, ensuring result never exceeds MaxChars.
  If no word boundary found within MaxChars, performs hard truncation. }
function TruncateToWord(const s: string; MaxChars: Integer): string;
var
  CurSize: Integer;
  TSL: TStringList;
  Ln: string;
begin
  Result:= '';
  CurSize:= 0;

  TSL:= TStringList.Create;
  try
    { WrapText breaks at word boundaries; if no separator found, text isn't wrapped }
    TSL.Text:= System.SysUtils.WrapText(s, MaxChars);

    for Ln in TSL do
    begin
      CurSize:= CurSize + Ln.Length;
      if CurSize > MaxChars
      then Break
      else Result:= Result + Ln;
    end;

    { Fallback: hard truncate if no words fit }
    if Result.Length < 1
    then Result:= Copy(s, 1, MaxChars);

    { Safety check - should never fail given the logic above }
    if Result.Length > MaxChars
    then Result:= Copy(Result, 1, MaxChars);
  finally
    FreeAndNil(TSL);
  end;
end;





















{ Merges ALL lines into a single block of text by removing line breaks.
  Replaces CRLF with nothing and TAB with space.
  Note: Performance warning - slow for large files (75s for 1.87MB). }
function UnWrapString(const s: string): string;
begin
  Result:= StringReplace(s,      CRLF, '',  [rfReplaceAll]);
  Result:= StringReplace(Result, Tab,  ' ', [rfReplaceAll]);
end;


{ Restores text that has been formatted with broken lines (e.g., different page layouts).

  Parameters:
    Separators - Characters that indicate intentional line breaks. Use #tab for tab, #enter for enter.
    RemoveExtraEnters - Collapses multiple consecutive empty lines into one.
    AddExtraSpace - Adds a space where lines are joined. }
function UnwrapText(s: string; Separators: string; RemoveExtraEnters, AddExtraSpace: Boolean): string;
var
  x: Integer;
  IsSepa: Boolean;
  SeparatorSet: TSysCharSet;

  function PreviousCharIsSeparator: Boolean;
  begin
    Result:= CharInSet(s[x-1], SeparatorSet);
  end;

begin
  { Handle short strings }
  if Length(s) < 3 then
  begin
    Result:= s;
    EXIT;
  end;

  { Remove extra enters }
  if RemoveExtraEnters then
  begin
    s:= StringReplace(s, CRLF+CRLF+CRLF+CRLF, CRLF+CRLF, [rfReplaceAll]);
    s:= StringReplace(s, CRLF+CRLF+CRLF,      CRLF+CRLF, [rfReplaceAll]);
  end;

  { Remove multiple consecutive spaces }
  s:= StringReplace(s, '    ', ' ', [rfReplaceAll]);

  { Build separator set from string }
  Separators:= StringReplace(Separators, '#tab',   Tab,  [rfReplaceAll, rfIgnoreCase]);
  Separators:= StringReplace(Separators, '#enter', CRLF, [rfReplaceAll, rfIgnoreCase]);
  SeparatorSet:= [];
  for x:= 1 to Length(Separators) do
    SeparatorSet:= SeparatorSet + [Separators[x]];

  { Process the string }
  x:= 3;
  Result:= s[1] + s[2];
  repeat
    if (s[x] = #13) AND (s[x+1] = #10) then
    begin
      IsSepa:= PreviousCharIsSeparator;
      if IsSepa then
      begin
        { Keep the line break (it follows a separator like period) }
        Result:= Result + CRLF;
        Inc(x, 2);
      end
      else
      begin
        { Remove the line break (unwrap the text) }
        Inc(x, 2);
        if AddExtraSpace
        then Result:= Result + ' ';
      end
    end
    else
    begin
      Result:= Result + s[x];
      Inc(x);
    end;
  until x >= Length(s);
end;


end.
