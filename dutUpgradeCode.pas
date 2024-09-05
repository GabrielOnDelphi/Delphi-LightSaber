UNIT dutUpgradeCode;

{ Upgrade code to higher standards:
    * Find Try/Except abusive usage
    * Fix the Embarcadero SetFocus problem.  }

INTERFACE

USES
  System.SysUtils, System.Classes, cmSearchResult, dutWin64;

type
  TDutUpgrade= class(TDutWin64)
   private
    Counter: Integer;          { Incremented each time a Try/Except is found. }
    Const LogUnit= 'uMyLog';
   public
    procedure FindSetFocus(Replace: Boolean);
    procedure FindTryExcept(Replace: Boolean);
  end;


IMPLEMENTATION

USES
   cmPascal, ccCore, ccIO;


{ Returns the line(s) where the text was found }
procedure TDutUpgrade.FindTryExcept(Replace: Boolean);
var
   TextBody: TStringList;
   Front: string;
   sPrev, sCurrLine, NextLine, sWarnings: string;
   Found: Boolean;
   iLine: Integer;

  function ResultAfterExcept: Boolean;
  begin
    Result:= Pos('Result', Trim(NextLine))= 1;
  end;

  function AddLog: string;
  begin
    Result:= CRLF + Front+ 'DebugLog(''Exception'', ''#LogCntr '+ IntToStr(Counter) +''');'
  end;

begin
 Front := '';
 Found:= FALSE;
 sWarnings:= SearchResults.Last.FileName+ CRLF+ 'Warnings' + CRLF+ CRLF;

 TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
 try
   for iLine:= 1 to TextBody.Count-2 do
     begin
       sPrev    := TextBody[iLine-1];
       sCurrLine:= TextBody[iLine];
       NextLine := TextBody[iLine+1];

       if LineIsAComment(sCurrLine) then Continue;     // Ignore comments

       Found:= IsKeyword(sCurrLine, 'except');
       if Found then
        begin
          SearchResults.Last.AddNewPos(iLine, 1, sCurrLine);       // Returns the line(s) where the text was found

          if Replace then
           begin
             sCurrLine:= ReplaceString(sCurrLine, '//ToDo: Don''t swallow exceptions! At least log them!', '{Logged exception}');

             Inc(Counter);

             { Add identation }
             VAR First:= FirstNonSpace(sCurrLine);
             if First > -1
             then Front:= System.StringOfChar(' ', First-1+2); // We put a nice number of spaces (same as Except)

             // Case 4. The code uses try/except to calculate a result of an opperation but it uses a boolean variable (B) instead of "Result"
             if RelaxedSearch(sPrev, 'b:=true;')
             then sWarnings:= sWarnings+ CRLF+ 'Use of b:=true @line'+ IntToStr(iLine);

             //Case 2. the tool always inserts the logging code if there is nothing between Except/end
             if PosInsensitive('end;', trim(NextLine)) = 1
             then sCurrLine:= sCurrLine+ AddLog
             else
               //Case 1: the tool will not insert the logging code if there is a "result:= something" line after Except
               if ResultAfterExcept
               then
                begin
                  EmptyDummy;
                  Continue;
                end
               else
                 // case 3. if there is any other code between except/and the tool does insert the logging code BUT it will write to a file so we can look manually over those cases...
                 begin
                   sCurrLine:= sCurrLine+ AddLog;
                   sWarnings:= sWarnings+ CRLF+ 'Code after except @line: '+ IntToStr(iLine);
                 end;

             { We store the information back }
             TextBody[iLine]:= sCurrLine;
           end;
        end;
     end;

   if Found and Replace then
     begin
       AddUnitToUses(TextBody, LogUnit);
       if BackupFile
       then BackupFileBak(SearchResults.Last.FileName);
       StringToFile(SearchResults.Last.FileName, TextBody.Text, woOverwrite, TRUE);
       StringToFile(ForceExtension(SearchResults.Last.FileName, '.txt'), sWarnings, woOverwrite);
     end;

 finally
   FreeAndNil(TextBody);
 end;
end;



{ Fix problems with Embarcadero's SetFocus.
  See also: uUtilsFocus.pas }
{ Returns the line(s) where the text was found
  If Replace is True, the bad code will be automatically fixed. }
procedure TDutUpgrade.FindSetFocus;
var
   TextBody: TStringList;
   Front: string;
   sLine: string;
   Found: Boolean;
   iPos, i: Integer;
begin
 Front := '';
 Found:= FALSE;

 TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
 try
   for i:= 0 to TextBody.Count-1 do
     begin
       sLine:= TextBody[i];
       iPos:= PosInsensitive('.SetFocus;', sLine);   // We search for something like: Edit2.SetFocus;
       if iPos > 0 then
        begin
          // Ignore lines that start with a comment symbol:   // { (*
          if LineIsAComment(sLine) then Continue;

          SearchResults.Last.AddNewPos(i, iPos, sLine);                       // Returns the line(s) where the text was found

          if Replace then
           begin
             { Restore spaces }
             VAR First:= FirstNonSpace(sLine);
             if First > -1
             then Front:= System.StringOfChar(' ', First-1);

             // SetFocus() can be found in cmVclUtils.pas
             TextBody[i]:= Front+ 'uUtilsFocus.SetFocus('+ ExtractObjectName(sLine)+ ');';  // We write something like SetFocus(Edit2);
             Found:= TRUE;
           end;
        end;
     end;

   if Found and Replace then
     begin
       AddUnitToUses(TextBody, 'uUtilsFocus, ');
       if BackupFile
       then BackupFileBak(SearchResults.Last.FileName);
       StringToFile(SearchResults.Last.FileName, TextBody.Text, woOverwrite, TRUE);
     end;

 finally
   FreeAndNil(TextBody);
 end;
end;


end.
