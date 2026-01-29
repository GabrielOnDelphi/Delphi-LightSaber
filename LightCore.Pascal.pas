UNIT LightCore.Pascal;

{=============================================================================================================
   Gabriel Moraru
   2021
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Simple routines for processing Pascal source files.

   Features:
     - Extract object/method names from code lines
     - Find and manipulate USES clauses
     - Detect and strip comments
     - Search code with relaxed matching (ignores spacing/case)
     - Search with [AND], [OR], [NOT] operators

   Limitations:
     - Not a full parser - uses heuristics and may have edge cases
     - Comment detection doesn't handle // inside string literals
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
  LightCore, LightCore.Types;

{-------------------------------------------------------------------------------------------------------------
    COMMENTS
-------------------------------------------------------------------------------------------------------------}

{ Returns True if the line starts with a comment symbol: //, {, or (*
  Note: Does not detect lines that are continuations of multi-line comments. }
function LineIsAComment(Line: string): Boolean;
begin
  Result:= FALSE;
  Line:= Trim(Line);
  if Line = '' then EXIT(FALSE);

  { Check for curly brace comment }
  if Line[1] = '{'
  then EXIT(TRUE);

  { Check for // and (* comments }
  if Length(Line) > 1
  then Result:= ((Line[1] = '/') AND (Line[2] = '/'))
             OR ((Line[1] = '(') AND (Line[2] = '*'));
end;


{ Extracts // comments from a line of code.
  Returns True if a comment was found and separated.
  CodeLine is modified to contain only the code (without comment).
  Comment receives the comment portion (including //).
  Note: Does not handle // inside string literals - a known limitation. }
function SeparateComments(var CodeLine: string; out Comment: string): Boolean;
var iPos: Integer;
begin
  Comment:= '';
  iPos:= Pos('//', CodeLine);
  Result:= iPos > 0;
  if Result then
  begin
    Comment := Copy(CodeLine, iPos, MaxInt);
    CodeLine:= Copy(CodeLine, 1, iPos - 1);
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


{ Returns the approximate number of comment lines.
  Returns -1 if file does not exist. }
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

{ Separates an object from its method/property.
  Line is a line of Pascal code.
  Example: Form1.Button1.SetFocus returns Form1.Button1
  Returns empty string if no dot is found. }
function ExtractObjectName(Line: string): string;
var LastDot: Integer;
begin
  Line:= Trim(Line);
  LastDot:= LightCore.LastPos('.', Line);
  if LastDot < 1
  then Result:= ''
  else Result:= CopyTo(Line, 1, LastDot - 1);
end;


{ Returns true if this line declares a method, type, or type alias.
  Checks for: function, procedure, constructor, destructor, class, record, and pointer types.
  Note: This is a heuristic check, not a full parser. May have false positives. }
function IsMethod(const CodeLine: string): Boolean;
begin
  Result:= (PosInsensitive('function', CodeLine) > 0)
        OR (PosInsensitive('procedure', CodeLine) > 0)
        OR (PosInsensitive('constructor', CodeLine) > 0)
        OR (PosInsensitive('destructor', CodeLine) > 0)
        OR (PosInsensitive('class', CodeLine) > 0)
        OR (PosInsensitive('record', CodeLine) > 0)
        OR (Pos('^', CodeLine) > 0);  { Pointer type declaration: PMyRec = ^TMyRec }
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
  Note: This will add the unit to the IMPLEMENTATION section's uses clause. }
function AddUnitToUses(PasBody: TStringList; CONST UnitToAdd: string): Boolean;
var
  sLine: string;
  iPos, i: Integer;
  MultiLine: Boolean;
  Units: string;
begin
  Result:= FALSE;
  if (PasBody.Count = 0) then RAISE Exception.Create('AddUnitToUses!');

  { Find the INTERFACE section }
  iPos:= FindSection(PasBody, TRUE);
  if iPos < 1 then EXIT(FALSE);

  { Check and don't add the unit if it already exists in interface }
  if UnitIncluded(PasBody, UnitToAdd, iPos)
  then EXIT(TRUE);

  { Find the IMPLEMENTATION section }
  iPos:= FindSection(PasBody, FALSE);
  if iPos < 1 then EXIT(FALSE);

  { Check and don't add the unit if it already exists in implementation }
  if UnitIncluded(PasBody, UnitToAdd, iPos)
  then EXIT(TRUE);

  for i:= iPos to PasBody.Count - 1 do
  begin
    sLine:= PasBody[i];

    iPos:= PosInsensitive('USES', sLine);
    if iPos > 0 then
    begin
      { Is the "uses" written as a single line or as multiple lines? }
      MultiLine:= UpperCase(Trim(sLine)) = 'USES';
      if MultiLine then
      begin
        { Multi-line uses - insert on next line }
        if i + 1 < PasBody.Count
        then PasBody[i + 1]:= '  ' + UnitToAdd + ', ' + Trim(PasBody[i + 1])
        else PasBody.Add('  ' + UnitToAdd + ';');  { Handle malformed file }
      end
      else
      begin
        { Single-line uses - insert unit at beginning }
        Units:= CopyfromTo(Trim(PasBody[i]), ' ', ';', TRUE);
        PasBody[i]:= 'USES ' + UnitToAdd + ', ' + Trim(Units);
      end;

      EXIT(TRUE);
    end;
  end;
end;



{-------------------------------------------------------------------------------------------------------------
    PARSE LINES OF CODE
-------------------------------------------------------------------------------------------------------------}

{ Returns the first line index where the 'Needle' was found, or -1 if not found.
  Search starts at StartAt (0-based index). }
function FindLine(CONST Needle: string; Haystack: TStringList; StartAt: Integer): Integer;
begin
  if (Haystack = NIL) 
  OR (StartAt < 0) 
  OR (StartAt >= Haystack.Count)
  then EXIT(-1);

  for var i:= StartAt to Haystack.Count - 1 do
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


{ Finds a substring even if it has extra spaces around it.
  Returns the position where the text was found, or -1 if not found.
  Example: Both "SetFocus(" and "SetFocus (" are matched. }
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


{ Returns the position where Needle was found in the haystack as a whole word.
  Returns 0 if not found or if found as part of another word.
  A "whole word" means the characters before and after are non-alphabetic. }
function WordPos(const Needle, HayStack: string): Integer;
var
  FoundPos, EndPos: Integer;
  CharBefore, CharAfter: Boolean;
begin
  FoundPos:= PosInsensitive(Needle, HayStack);
  if FoundPos < 1 then EXIT(0);  { Not found }

  { Check character before the needle }
  if FoundPos = 1
  then CharBefore:= True  { Beginning of string - valid }
  else CharBefore:= NOT CharInSet(HayStack[FoundPos - 1], Alphabet);

  { Check character after the needle }
  EndPos:= FoundPos + Length(Needle);
  if EndPos > Length(HayStack)
  then CharAfter:= True  { End of string - valid }
  else CharAfter:= NOT CharInSet(HayStack[EndPos], Alphabet);

  if CharBefore AND CharAfter
  then Result:= FoundPos
  else Result:= 0;
end;






{ Search for lines matching a query with [AND], [OR], and [NOT] operators.
  Returns the line index where a match was found, or -1 if not found.

  Syntax:
    - [AND] between terms: ALL terms must be present
    - [OR] between terms: ANY term must be present
    - [NOT] before a term: Term must NOT be present

  Examples:
    'if[AND]then'         - Line must contain both 'if' AND 'then'
    'begin[OR]end'        - Line must contain 'begin' OR 'end'
    'procedure[NOT]virtual' - Line must contain 'procedure' but NOT 'virtual'

  Note: Only supports one operator type per query. Mixed operators not supported. }
function RelaxedSearchEx(Query: string; Haystack: TStringList; StartAt: Integer = 0): Integer;
var
  Line, Term: string;
  i: Integer;
  HasAND, HasOR, HasNOT: Boolean;
  Parts: TArray<string>;
  MainTerm, ExcludeTerm: string;
  AllMatch, AnyMatch: Boolean;
begin
  Result:= -1;
  if (Haystack = NIL) OR (Haystack.Count = 0)
  then EXIT;
  if (StartAt < 0) OR (StartAt >= Haystack.Count)
  then EXIT;

  Query:= Trim(Query);
  if Query = ''
  then EXIT;

  HasAND:= Pos('[AND]', Query) > 0;
  HasOR:= Pos('[OR]', Query) > 0;
  HasNOT:= Pos('[NOT]', Query) > 0;

  { Handle [NOT] operator: term[NOT]excluded }
  if HasNOT then
  begin
    Parts:= Query.Split(['[NOT]'], TStringSplitOptions.ExcludeEmpty);
    if Length(Parts) < 2
    then EXIT;
    MainTerm:= LowerCase(Trim(Parts[0]));
    ExcludeTerm:= LowerCase(Trim(Parts[1]));

    for i:= StartAt to Haystack.Count - 1 do
    begin
      Line:= LowerCase(Haystack[i]);
      if (Pos(MainTerm, Line) > 0) AND (Pos(ExcludeTerm, Line) = 0)
      then EXIT(i);
    end;
    EXIT;
  end;

  { Handle [AND] operator: all terms must match }
  if HasAND then
  begin
    Parts:= Query.Split(['[AND]'], TStringSplitOptions.ExcludeEmpty);
    for i:= StartAt to Haystack.Count - 1 do
    begin
      Line:= LowerCase(Haystack[i]);
      AllMatch:= True;
      for Term in Parts do
        if Pos(LowerCase(Trim(Term)), Line) = 0 then
        begin
          AllMatch:= False;
          Break;
        end;
      if AllMatch
      then EXIT(i);
    end;
    EXIT;
  end;

  { Handle [OR] operator: any term must match }
  if HasOR then
  begin
    Parts:= Query.Split(['[OR]'], TStringSplitOptions.ExcludeEmpty);
    for i:= StartAt to Haystack.Count - 1 do
    begin
      Line:= LowerCase(Haystack[i]);
      AnyMatch:= False;
      for Term in Parts do
        if Pos(LowerCase(Trim(Term)), Line) > 0 then
        begin
          AnyMatch:= True;
          Break;
        end;
      if AnyMatch
      then EXIT(i);
    end;
    EXIT;
  end;

  { No operators - simple search }
  for i:= StartAt to Haystack.Count - 1 do
    if Pos(LowerCase(Query), LowerCase(Haystack[i])) > 0
    then EXIT(i);
end;


END.
