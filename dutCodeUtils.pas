unit dutCodeUtils;

{=============================================================================================================
   Gabriel Moraru
   2022
=============================================================================================================}

interface

uses
  System.SysUtils, System.Classes, System.Math, cmSearchResult, dutUpgradeCode;

type
  TDutUtils= class(TDutUpgrade)
  private
   public
     // UTF8
     procedure HasBOM;
     procedure ConvertToUtf(AddBOM: Boolean);
     procedure ConvertToAnsi;
     // Utils
     procedure FormatCodeTight;
     procedure FindImplementation(const MethodToFind, IntfName: String);
  end;



IMPLEMENTATION
USES
   ccTextFile, ccIO, cmPascal, ccCore;



{=============================================================================================================
   UTF8
   Convert between ANSI and UTF8

   Wikipedia:
   The UTF-8 representation of the BOM is the (hexadecimal) byte sequence EF BB BF .
   The Unicode Standard permits the BOM in UTF-8, but does not require or recommend its use.
=============================================================================================================}

procedure TDutUtils.HasBOM;
begin
  if NOT ccTextFile.FileHasBOM(SearchResults.Last.FileName)
  then SearchResults.Last.AddNewPos('File without BOM.');         // We indicate that this file was found without BOM
end;


{ CONVERT }
procedure TDutUtils.ConvertToUtf(AddBOM: Boolean);
begin
 if NOT ccTextFile.FileHasBOM(SearchResults.Last.FileName) then
   begin
     ccTextFile.ConvertToUTF(SearchResults.Last.FileName);
     SearchResults.Last.AddNewPos('Converted to UTF8');
   end;
end;


procedure TDutUtils.ConvertToAnsi;
begin
  if ccTextFile.ConvertToAnsi(SearchResults.Last.FileName)
  then SearchResults.Last.AddNewPos('Converted to ANSI');
end;




{=============================================================================================================
   INTERFACE IMPLEMENTATION
=============================================================================================================}

{ Finds the class(es) that implement the specified method }
procedure TDutUtils.FindImplementation(CONST MethodToFind, IntfName: String);
var
   TextBody: TStringList;
   sLine: string;
   iColumn, iLine: Integer;

       function FindMethod(const RoutineType: string): Boolean;
       var iPos2, iPos3: Integer;
       begin
        Result:= False;
        iColumn:= PosInsensitive(RoutineType, sLine);
        if iColumn > 0 then
         begin
            iPos2:= PosInsensitive('.'+ MethodToFind+ '(', sLine);  // Search only full words
            iPos3:= PosInsensitive('.'+ MethodToFind+ ';', sLine);  // Search only full words

            const x= iColumn+ Length(RoutineType)+ 3;

            if (iPos2 > x) or (iPos3 > x)
            then Result:= True;   // Returns the line(s) where the text was found
         end;
       end;

       function FindInterface: Boolean;
       begin
          if IntfName = ''
          then Exit(True)
          else Result:= False;

          var UpPoint:= Min(0, iLine-100);
          for var j:= iLine downto UpPoint do
            if (PosInsensitive(',' + IntfName, TextBody[j]) > 7)
            or (PosInsensitive(', '+ IntfName, TextBody[j]) > 7)
            then Exit(True);
       end;
begin
 TextBody:= StringFromFileTSL(SearchResults.Last.FileName);
 try
   for iLine:= 0 to TextBody.Count-1 do
     begin
       sLine:= TextBody[iLine];
       if not LineIsAComment(sLine) then    // Ignore lines that start with a comment symbol:   // { (*
        begin
          // ToDo: let user choose if he wants to search for a function or a procedure
          if FindMethod('procedure T')     // c
          and FindInterface
          then SearchResults.Last.AddNewPos(iLine, iColumn, sLine);

          if FindMethod('function T')      // c
          and FindInterface
          then SearchResults.Last.AddNewPos(iLine, iColumn, sLine);
        end;
     end;
 finally
   FreeAndNil(TextBody);
 end;
end;




{=============================================================================================================
   CODE FORMAT TIGHT
=============================================================================================================}

{ Returns the line(s) where the text was found }
procedure TDutUtils.FormatCodeTight; //(Replace: Boolean);
var
   TextBody: TStringList;
   Front: string;
   Comments, sLine: string;
   iPos, i: Integer;
begin
 Front := '';

 TextBody:= StringFromFileTSL(SearchResults.Last.FileName, TEncoding.UTF8);
 try
   for i:= 0 to TextBody.Count-1 do
     begin
       sLine:= TextBody[i];
       if LineIsAComment(sLine) then Continue;    // Ignore lines that start with a comment symbol:   // { (*
       if IsMethod(sLine) then Continue;

       (*
       // FIRST, check lines that are full comments
       // PROBLEM. Difficult to distinguish between human comments and actual code. We could say that if the line has at least one one Delphi specific character, it is a line of code. Sepcifics:   ; ( ) = if then else begin end
       if LineIsAComment(sLine) {(Trim(sLine) = '') and (Comments > '')} then
         begin
           TextBody[i]:= 'COMMENTS OUT!    '+ sLine+ Comments;
           Continue;
         end; *)

       // Then extract comments (//) for lines that are mixed, and process only the actual code. At the end put the comment back
       SepparateComments(sLine, Comments);

       //Fix:
       //ToDo: problem on lines like this: property ImageList: IList<TGUID> read FImageList;   <-------- the space after > will be removed! ToDo: add a check for "read"
       //ToDo: problem on lines like this: Something = (enOn,enOff);                           <-------- the spaces will be removed!
       //ToDo: problem on lines like this: Type CRLF TProcess = TProcess;                      <-------- the spaces will be removed!
       //ToDo: do not apply if the punctuation is inside a string. Example: if Pos('Test punctuation wrong. Wrong');  <--- the space after the dot will be removed!

       //Add new features:
       //ToDo: check for enters or other text after end.
       //ToDo: check if there is an empty line above and below {------------------------------------------}
       //ToDo: check parentheses: Assert( FParent<>nil);
       //ToDo: add emtyp line after the first "uses". Also after the last one? Yes

       sLine:= ReplaceString(sLine, ' <', '<');
       sLine:= ReplaceString(sLine, '< ', '<');

       sLine:= ReplaceString(sLine, ' >', '>');
       sLine:= ReplaceString(sLine, '> ', '>');

       sLine:= ReplaceString(sLine, ' =', '=');
       sLine:= ReplaceString(sLine, '= ', '=');

       sLine:= ReplaceString(sLine, ' ,', ',');
       sLine:= ReplaceString(sLine, ', ', ',');

       if Pos('  :=', sLine) < 1 then      // ignore code where the assignment symbol on multiple lines where arranged to the same column (I do that a lot)
         begin
           sLine:= ReplaceString(sLine, ' :=', ':=');
           sLine:= ReplaceString(sLine, ':= ', ':=');
         end;

       sLine:= ReplaceString(sLine, ' ;', ';');

       // Lower case code
       sLine:= ReplaceWholeWords(sLine, 'Raise', 'raise');
       sLine:= ReplaceWholeWords(sLine, 'True' , 'true');
       sLine:= ReplaceWholeWords(sLine, 'False', 'false');

       iPos:= PosInsensitive(' ;', sLine);
       if (iPos > 0)
       AND not IsMethod(sLine)
       then sLine:= ReplaceString(sLine, ' ;', ';');

       iPos:= PosInsensitive(' ;', sLine);
       if (iPos > 0)
       AND not IsMethod(sLine)
       then sLine:= ReplaceString(sLine, '; ', ';');

       TextBody[i]:= sLine+ Comments;
     end;

   var NewName:= IncrementFileName(SearchResults.Last.FileName, True);
   StringToFile(NewName, TextBody.Text, woOverwrite, wpAuto);
 finally
   FreeAndNil(TextBody);
 end;
end;


end.
