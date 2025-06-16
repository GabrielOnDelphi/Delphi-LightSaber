UNIT LightCore.Pascal;

{=============================================================================================================
   Gabriel Moraru
   2021
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Very crude routines for processing PAS files.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.RegularExpressions, System.StrUtils;


// CLASSES & OBJECTS
function ExtractObjectName(Line: string): String;
function IsMethod         (CONST CodeLine: string): Boolean;

// SECTIONS
function AddUnitToUses    (PasBody: TStringList; CONST UnitToAdd: string): Boolean;
function FindSection      (PasBody: TStringList; bInterface: Boolean): Integer;    { Find the INTERFACE/IMPLEMENTATION section }

// COMMENTS
function SeparateComments (var CodeLine: string; out Comment: string): Boolean;
function LineIsAComment   (Line: string): Boolean;
function CountComments    (CONST FileName: string): Integer;

// Search
function RelaxedSearch    (      CodeLine, Instruction: string): Boolean;
function RelaxedSearchI   (CONST CodeLine, Instruction: string): Integer;
function IsKeyword        (      CodeLine, Instruction: string): Boolean;   // Find delphi keywords (that don't require a semicolon) like: begin, try, except.
function IsReservedKeyword(CONST CodeLine: string): Boolean;
function FindLine         (CONST Needle: string; Haystack: TStringList; StartAt: integer): Integer;   // Returns the first line where the 'Needle' was found
function WordPos          (CONST Needle, HayStack: string): Integer;        // Returns the position where Needle was found in the haystack. BUT does so only if the character in front of the Needle is a non-alphabetic character (a-z, A-Z)

// New functions
function RelaxedSearchEx  (Query: string; Haystack: TStringList; StartAt: Integer = 0): Integer;

IMPLEMENTATION

USES
  LightCore;

{-------------------------------------------------------------------------------------------------------------
    COMMENTS
-------------------------------------------------------------------------------------------------------------}
{ Returns true if the lines that start with a comment symbol:   // { (*
  Line is a line of Pascal code. }
function LineIsAComment(Line: string): Boolean;
begin
  Result:= FALSE;
  Line:= Trim(Line);
  if Line = '' then EXIT(FALSE);

  if (Pos('{', Line) = 1)
  then EXIT(TRUE);

  if Length(Line) > 1
  then Result:= ((Line[1] = '/') AND (Line[2] = '/'))
             OR ((Line[1] = '(') AND (Line[2] = '*'));
end;


{ Extract comments (//) and process only the actual code. At the end put the comment back }
function SeparateComments(var CodeLine: string; out Comment: string): Boolean;
begin
  var iPos:= Pos('//', CodeLine);
  Result:= iPos > 0;
  if Result then
   begin
    Comment := Copy(CodeLine, iPos, MaxInt);  // Get the comment BEFORE I modify the sLine
    CodeLine:= Copy(CodeLine, 1, iPos-1);
   end;
end;


function StripComments(const Line: string): string;
var s: string;
begin
  s := TRegEx.Replace(Line, '//.*$', '');                     // Remove single line comments
  s := TRegEx.Replace(s, '\{.*?\}', '', [roMultiLine]);       // Remove curly braces comments
  s := TRegEx.Replace(s, '\(\*.*?\*\)', '', [roMultiLine]);   // Remove parentheses comments
  Result := s;
end;


{ Returns the approximate number of comment lines }
function CountComments(const FileName: string): Integer;
begin
  Result:= 0;
  var TSL:= TStringList.Create;
  Try
    TSL.LoadFromFile(FileName);
    for var Line in TSL do
      if LineIsAComment(Line)
      then Inc(Result);
  Finally
    FreeAndNil(TSL);
  end;
end;


{-------------------------------------------------------------------------------------------------------------
    CLASSES & OBJECTS
-------------------------------------------------------------------------------------------------------------}

{ Separate an object from its properties.
  Line is a line of Pascal code
  Example: Form1.Button1.SetFocus returns Form1.Button1 }
function ExtractObjectName(Line: string): string;
begin
  Line:= Trim(Line);
  VAR LastDot:= LightCore.LastPos('.', Line)-1;
  Result:= CopyTo(Line, 1, LastDot);
end;


{ Returns true if this line of code starts with "procedure" or "function" }
function IsMethod(const CodeLine: string): Boolean;
begin
  Result:= (PosInsensitive('function' , CodeLine) > 0)
        OR (PosInsensitive('procedure', CodeLine) > 0)
        OR (PosInsensitive('constructor', CodeLine) > 0)    // constructor TMyClass.Create(x, y: integer);
        OR (PosInsensitive('class', CodeLine) > 0)          // Examples: TMyAliasis = class;    TMyAliasis = class(Tobject)
        OR (PosInsensitive('record', CodeLine) > 0)         // TMyRec = packed record
        OR (PosInsensitive('^', CodeLine) > 10);            // PMyRec = ^TMyRec;
end;



{-------------------------------------------------------------------------------------------------------------
    SECTIONS
-------------------------------------------------------------------------------------------------------------}

{ Find the INTERFACE/IMPLEMENTATION section }
function FindSection(PasBody: TStringList; bInterface: Boolean): Integer;
VAR
   i: Integer;
   Section, sLine: string;
begin
 if bInterface
 then Section:= 'INTERFACE'
 else Section:= 'IMPLEMENTATION';

 for i:= 0 to PasBody.Count-1 do
   begin
    sLine:= PasBody[i];
    sLine:= StringReplace(sLine, ' ', '', [rfReplaceAll]);
    if PosInsensitive(Section, sLine) = 1
    then EXIT(i);
   end;
 Result:= -1;
end;


{ Returns True if this unit is already included in the USES }
function UnitIncluded(PasBody: TStringList; UnitToFind: string; StartAt: integer): Boolean;
VAR
   iPos, i: Integer;
   sLine: string;
begin
 iPos:= FindLine('uses', PasBody, StartAt);
 if iPos < 1
 then EXIT(FALSE);

 for i:= iPos to PasBody.Count-1 do
   begin
     sLine:= PasBody[i];

     if PosInsensitive(UnitToFind, sLine) > 0
     then EXIT(TRUE);

     if Pos(';', sLine) > 0
     then EXIT(FALSE); // Stop when we encounter the ;
   end;

 Result:= FALSE;
end;


{ Add the specified unit to the "uses" clause.
  Returns True if the unit was added or if it already existed in the Uses.
  Warning! This will add the unit only to IMPLEMENTATION }
function AddUnitToUses(PasBody: TStringList; CONST UnitToAdd: string): Boolean;
VAR
   sLine: string;
   iPos, i: Integer;
   MultiLine: Boolean;
begin
 Result:= FALSE;

 { Find the INTERFACE section }
 iPos:= FindSection(PasBody, TRUE);
 if iPos < 1 then EXIT(FALSE);

 { Check and don't add the unit if it already exists }
 if UnitIncluded(PasBody, UnitToAdd, iPos)
 then EXIT(TRUE);

 { Find the IMPLEMENTATION section }
 iPos:= FindSection(PasBody, FALSE);
 if iPos < 1 then EXIT(FALSE);

 { Check and don't add the unit if it already exists }
 if UnitIncluded(PasBody, UnitToAdd, iPos)
 then EXIT(TRUE);

 for i:= iPos to PasBody.Count-1 do
   begin
     sLine:= PasBody[i];

     iPos:= PosInsensitive('USES', sLine);
     if (iPos > 0) then
      begin
        { Is the "uses" written as a single line or as multiple lines? }
        MultiLine:= UpperCase(Trim(sLine)) = 'USES';
        if MultiLine
        then PasBody[i+1]:= '  '+ UnitToAdd+',' + Trim(PasBody[i+1])  // Warning: this will crash and burn if the Pas file is invalid (if there is no next line after "uses" )
        else
         begin
           VAR Units:= CopyfromTo(Trim(PasBody[i]), ' ', ';', TRUE); //the rest of the line;
           PasBody[i]:= 'USES ' + UnitToAdd+ ', '+ Trim(Units);
         end;

        EXIT(TRUE);
      end;
   end;
end;



{-------------------------------------------------------------------------------------------------------------
    PARSE LINES OF CODE
-------------------------------------------------------------------------------------------------------------}

{ Returns the first line where the 'Needle' was found }
function FindLine(CONST Needle: string; Haystack: TStringList; StartAt: integer): Integer;
begin
 for var i:= StartAt to Haystack.Count-1 do
   if PosInsensitive(Needle, Haystack[i]) > 0
   then EXIT(i);
 Result:= -1;
end;


{ Checks a line of code if it is equal with instruction.
  For this we ignore all spaces and we ignore cases.
  Example:
    For these parameters the function will return true:
      CodeLine:    B:= True;
      Instruction: b:=true;
    We need to make sure that the string in the second parameter (Instruction) is correctly formated. }


//out
function RelaxedSearch(CodeLine, Instruction: string): Boolean;
begin
  // Strip comments from the line
  CodeLine:= Trim(CodeLine);
  CodeLine := StripComments(CodeLine);

  // Remove all spaces and make strings lowercase for case-insensitive comparison
  CodeLine    := StringReplace(CodeLine, ' ', '', [rfReplaceAll]);
  Instruction := StringReplace(Instruction, ' ', '', [rfReplaceAll]);

  Result := AnsiStartsText(Instruction, CodeLine);  // Case-insensitive comparison
end;


{ Find delphi keywords (that don't require a semicolon) like: begin, try, except. }
function IsKeyword(CodeLine, Instruction: string): Boolean;
var
  Regex: string;
begin
  // Strip comments from the line
  CodeLine := Trim(StripComments(CodeLine));

  // Convert to lowercase for case-insensitive comparison
  CodeLine := LowerCase(CodeLine);
  Instruction := LowerCase(Instruction);

  // Create a regular expression to match the instruction as a word
  // \b indicates a word boundary, so it will match "except" but not "ExceptObject"
  Regex := '\b' + Instruction + '\b';

  // Use regular expression to check if the instruction appears in the code line
  Result := TRegEx.IsMatch(CodeLine, Regex);
end;


function IsReservedKeyword(const CodeLine: string): Boolean;
const
  Keywords: array[1..12] of string = ('begin', 'end', 'try', 'except', 'finally', 'repeat', 'uses','interface','asm','initialization','finalization','implementation');
begin
  Result := False;
  for var i := Low(Keywords) to High(Keywords) do
    if IsKeyword(CodeLine, Keywords[i])
    then Exit(True);
end;


{
  As above. It finds a method even if it has some spaces in it.
  The advantage of this is that it returns the exact position where the text was found.
  Example. Both work: SetFocus( and SetFocus (
}
//ToDo: The function assumes LineLower and SubstringLower are always lowercase. Instead of LowerCase calls inside the loop, preprocess once.
function RelaxedSearchI(const CodeLine, Instruction: string): Integer;
var
  SubstringLower,
  LineLower: string;
  i, j: Integer;
begin
  Result := -1;
  SubstringLower := LowerCase(Instruction);
  LineLower := LowerCase(CodeLine);
  i := 1;
  while i <= Length(LineLower) do
   begin
     // Skip non-alphanumeric characters
     while (i <= Length(LineLower)) AND NOT CharInSet(LineLower[i], ['a'..'z', '0'..'9'])
       do Inc(i);

     // Check if Substring matches
     j := 1;
     while (j <= Length(SubstringLower))
     AND (i <= Length(LineLower))
     AND (LineLower[i] = SubstringLower[j]) do
      begin
        Inc(i);
        Inc(j);
      end;

     // If we have matched the entire Substring, then return the position
     if j > Length(SubstringLower)
     then Exit(i - Length(SubstringLower));

     // Move to the next character
     Inc(i);
   end;
end;


{ Returns the position where Needle was found in the haystack.
  BUT does so only if the character in front of the Needle is a non-alphabetic character (a-z, A-Z) }
//ToDo: Pos = 1 is valid only if it's a standalone word. The function should check if HayStack[Pos + Length(Needle)] is also a separator.
function WordPos(const Needle, HayStack: string): Integer;
begin
  var Pos:= PosInsensitive(Needle, HayStack);
  if (Pos < 1) then Exit(Pos);     // Not found
  if (Pos = 1) then Exit(Pos);     // The Needle is at the beginning og the haystack. There is nothing more to check.

  // Check if the previous character is a letter or a space (or other signs like .,_+-!; etc)
  if CharInSet(HayStack[Pos-1], Alphabet)
  then Result:= 0                // Not a whole word!
  else Result:= Pos;             // Still valid
end;






{ Search multiple strings sepparated by special instructions like [OR] and [AND] and [NOT].
  For example if I look for a line of code that must contain both the keywords 'if' and 'then' than I will write my query as:
  usage:
     FindLine(InputText, 'if[AND]then'): Integer;  }

// !!!!!!!!!!!! NOT TESTED YET !!!!!!!!!!!!
// ChatGPT    (better)
function RelaxedSearchEx(Query: string; Haystack: TStringList; StartAt: Integer = 0): Integer;
var
  Terms: TArray<string>;
  IncludeList, ExcludeList: TArray<string>;
  Line, Term: string;
  Found, MatchAND, MatchOR: Boolean;
  i: Integer;
begin
  Result := -1;
  Terms := Query.Split(['[AND]', '[OR]', '[NOT]'], TStringSplitOptions.ExcludeEmpty);
  // Process Terms into Include and Exclude lists
  for Term in Terms do
  begin
    if Pos('[NOT]', Query) > 0
    then ExcludeList := ExcludeList + [Trim(Term)]
    else
      if Pos('[AND]', Query) > 0
      then IncludeList := IncludeList + [Trim(Term)]
      else IncludeList := IncludeList + [Trim(Term)];
  end;
  // Search
  for i := StartAt to Haystack.Count - 1 do
  begin
    Line := LowerCase(Trim(Haystack[i]));
    MatchAND := True;
    MatchOR := False;
    // Check Include terms (AND condition)
    for Term in IncludeList do
      if Pos(LowerCase(Term), Line) = 0 then
      begin
        MatchAND := False;
        Break;
      end;
    // Check Exclude terms (NOT condition)
    for Term in ExcludeList do
      if Pos(LowerCase(Term), Line) > 0 then
      begin
        MatchAND := False;
        Break;
      end;
    // If we have `[OR]` conditions, allow at least one match
    for Term in IncludeList do
      if Pos(LowerCase(Term), Line) > 0 then
      begin
        MatchOR := True;
        Break;
      end;
    // Result
    Found := (MatchAND and (Pos('[AND]', Query) > 0)) or
             (MatchOR  and (Pos('[OR]', Query) > 0)) or
             (MatchAND and NOT MatchOR);
    if Found then Exit(i);
  end;
end;


END.
