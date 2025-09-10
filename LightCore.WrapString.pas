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

 function  WrapStringEdi    (InStrg: String; WrapWidth: Integer): string;  // not tested
 function  WrapStringForced (CONST s: string; MaxRowLength: integer): string; {old name WrapString }           { Makes sure that none of the lines of 's' is longer than RowLenght chars }     { Also exists: System.SysUtils.WrapText }
 function  WrapStringForcedA(CONST s: AnsiString; MaxRowLength: integer): AnsiString;

 function  UnWrapString     (CONST s: string): string;
 function  UnwrapText       (      s: string; Separators: string; RemoveExtraEnters, AddExtraSpace: Boolean): string;

 function TruncateToWord    (CONST s: string; MaxChars: Integer): string;


IMPLEMENTATION

USES LightCore, LightCore.Types, LightCore.StrBuilder;

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

function WrapStringForced(CONST s: string; MaxRowLength: integer): string;
VAR BufferSize, i, Row: Integer;
    StrBuilder: LightCore.StrBuilder.TCStringBuilder;
Begin
 Row:= 0;
 Result:= '';
 BufferSize:= Length(s);
 BufferSize:= BufferSize+ Round(BufferSize * 0.02);  { Add 2 percent more }
 StrBuilder:= TCStringBuilder.Create(BufferSize);
 TRY
  for i:= 1 TO Length(s) DO
   begin
    inc(Row);
    StrBuilder.AddChar(s[i]);   { trunchez liniile la 'RowLength' caractere no matter what }
    if Row >= MaxRowLength then                                                                        { trunchez liniile la 'RowLength' caractere }
     begin
      StrBuilder.AddEnter;
      Row:= 0;
     end;
   end;
  Result:= StrBuilder.AsText;
 FINALLY
  FreeAndNil(StrBuilder);
 END;
End;



function WrapStringForcedA(CONST s: AnsiString; MaxRowLength: integer): AnsiString;    { Also exists: System.SysUtils.WrapText }
begin
 Result:= ansistring(WrapStringForced(string(s), MaxRowLength));
end;


// Truncates s to full words. But the result will never pass over the MaxChars limit!
function TruncateToWord(CONST s: string; MaxChars: Integer): string;
VAR CurSize: Integer;
begin
  Result:= '';
  CurSize:= 0;

  VAR TSL:= TStringList.Create;
  TRY
    TSL.Text:= System.SysUtils.WrapText(s, MaxChars);  // WrapText has a problem: if the separator is not encountered, the text is never wrapped !!!

    for VAR Ln in TSL DO
      begin
       CurSize:= CurSize + Ln.Length;
       if CurSize > MaxChars
       then Break
       else Result:= Result+ Ln;
      end;

    if Result.Length < 1
    then Result:= Copy(s, 1, MaxChars);

    Assert(Result.Length <= MaxChars)
  FINALLY
    FreeAndNil(TSL);
  END;
end;





















{ It marges ALL lines in a file into a single block of text.
  Painfully slow! Takes 75 seconds to process a 1.87 MB text file }
function UnWrapString(CONST s: string): string;
Begin
 Result:= StringReplace(s     , CRLF, '' , [rfReplaceAll, rfIgnoreCase]);
 Result:= StringReplace(Result, Tab  , ' ', [rfReplaceAll, rfIgnoreCase]);
End;


{
 Description:
   Text Unwrap can restore the text in a document that has been formatted with a broken line / row, for example, those files that have short rows because they were formatted for a page layout different than yours (like A4/L8).</p>

 Paramaters:
   Separators - If one of these characters is found before an empty line then that line will not be unwrapped. Use #tab for tab and #enter for enter.
   RemoveExtraEnters - If the program encounters a block of multiple empty lines it eliminates some of them.
   AddExtraSpace - Put a space at the place where the text is cancatenated.
}
function UnwrapText(s: string; Separators: string; RemoveExtraEnters, AddExtraSpace: Boolean): string;
VAR
   x: integer;
   IsSepa: boolean;
   enum: TSysCharSet;

  function PreviousCharIsSeparator: boolean;
  VAR c: char;
  begin
   c:= s[x-1];
   Result:= CharInSet(c, enum);
  end;

begin
 { Remove extra enters }
 if RemoveExtraEnters then
  begin
   s:= StringReplace(s, CRLF+CRLF+CRLF+CRLF, CRLF+CRLF, [rfReplaceAll]);
   s:= StringReplace(s, CRLF+CRLF+CRLF     , CRLF+CRLF, [rfReplaceAll]);
  end;

 { Remove multiple consecutive spaces }
 s:= StringReplace(s, '    ', ' ', [rfReplaceAll]);

 { Extract separators }
 Separators:= StringReplace(Separators, '#tab'  , Tab , [rfReplaceAll, rfIgnoreCase]);
 Separators:= StringReplace(Separators, '#enter', CRLF, [rfReplaceAll, rfIgnoreCase]);
 enum:= [];
 for x:= 1 to Length(Separators)
   DO enum:= enum+ [Separators[x]];

 { Start }
 x:= 3;
 Result:= s[1] + s[2];
 REPEAT                                                                       //pentru poate randurile
  if (s[x]= #13) AND (s[x+1]= #10)
  then
    Begin
      IsSepa:= PreviousCharIsSeparator;

      if IsSepa
      then                                      { daca s-a gasit ENTER nejustificat (nu s-a gasit PUNCT), atunci ignore this current fake enter }
       begin
        Result:= Result + CRLF;
        inc(x, 2);
       end
      else
       begin
        inc(x, 2);
        if AddExtraSpace
        then Result:= Result + ' ';
       end
    End
  else
   begin
    Result:= Result + s[x];
    inc(x);
   end;
 UNTIL x >= length(s);
end;



{ Not tested }
function WrapStringEdi(InStrg: String; WrapWidth: Integer): String;
var
   I,u : integer;
   BStrg: String;

  function findenter: Integer;
  var j,imax:integer;
  begin
   result:= 0;
   if WrapWidth <= Length(InStrg)
   then imax:= WrapWidth
   else imax:= Length(InStrg);

   for j := iMax downto 1 do
    begin
     if (InStrg[j]=#13) or (InStrg[j]=#10) then
       result:= j+1;
     Break            //?????????????????
    end;
  end;

begin
  Result:='';
  BStrg:='';
  u:= findenter;
  I := WrapWidth+U;

  while true do
   begin
     if i> length(InStrg) then
      begin
        result:= result+ InStrg;
        exit;
      end;

     if InStrg[i]= ' '
     then
       begin
        BStrg:= Copy(InStrg, 1, WrapWidth);
        InStrg:= Copy (InStrg, WrapWidth+1, High(integer));
        u:= findenter;
        i:= WrapWidth+u;
        Result:= Result+ BStrg+ CRLF;
       end
     else
       begin
         REPEAT
           dec(I);
         UNTIL InStrg[i]= ' ';

         if I >= WrapWidth  then
           begin
            BStrg := Copy(InStrg, 1, i);
            InStrg:= Copy(InStrg, WrapWidth+1, High(integer));
            u:= findenter;
            i:= WrapWidth+u;
           end
         else
           begin
            BStrg := Copy(InStrg, 1, i);
            InStrg:= Copy(InStrg, i+1, High(integer));
            u:= findenter;
            i:= WrapWidth+u;
           end;

         Result:= Result+ BStrg + CRLF;
       end;
   end;
end;


end.
