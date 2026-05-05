UNIT LightCore.Pascal;

{=============================================================================================================
   Gabriel Moraru
   2026.05.05
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
  LightCore;

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


{ Strips comments from a single line of code.
  Removes //... to end-of-line, and curly-brace or paren-star comments when both
  delimiters are on the SAME line. Multi-line braced comments that span across lines
  are NOT detected (caller would need to feed the whole source as one string and
  use roSingleLine). }
function StripComments(const Line: string): string;
var s: string;
begin
  s := TRegEx.Replace(Line, '//.*$', '');           // Remove single line comments
  s := TRegEx.Replace(s, '\{.*?\}', '');            // Remove curly braces comments
  s := TRegEx.Replace(s, '\(\*.*?\*\)', '');        // Remove parentheses comments
  Result := s;
end;


{ Returns the approximate number of comment lines.
  Returns -1 if the file does not exist.
  Raises (propagates from TStringList.LoadFromFile) if the file exists but cannot be read. }
function CountComments(const FileName: string): Integer;
var TSL: TStringList;
begin
  if NOT FileExists(FileName)
  then EXIT(-1);

  Result:= 0;
  TSL:= TStringList.Create;
  try
    TSL.LoadFromFile(FileName);
    for var Line in TSL do
      if LineIsAComment(Line)
      then Inc(Result);
  finally
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
    { Require the keyword at column 1 (after stripping spaces) so we don't match it
      inside comments like `// implementation note` or string literals. }
    if PosInsensitive(Section, sLine) = 1
    then EXIT(i);
   end;
 Result:= -1;
end;


{ Returns True if this unit is already included in the USES clause located at/after StartAt.
  Searches up to (but not including) StopAt - pass the IMPLEMENTATION line index to avoid
  leaking from the INTERFACE search into the IMPLEMENTATION uses. Pass MaxInt if no upper bound. }
function UnitIncluded(PasBody: TStringList; CONST UnitToFind: string; StartAt, StopAt: integer): Boolean;
VAR
   iPos, i, LastLine: Integer;
   sLine: string;
begin
 iPos:= FindLine('uses', PasBody, StartAt);
 if (iPos < 0) OR (iPos >= StopAt)
 then EXIT(FALSE);

 { Clamp the loop's upper bound: StopAt is exclusive, the loop is inclusive,
   so the last index we may visit is min(Count-1, StopAt-1). }
 LastLine:= PasBody.Count - 1;
 if StopAt - 1 < LastLine
 then LastLine:= StopAt - 1;

 for i:= iPos to LastLine do
   begin
     sLine:= PasBody[i];

     if WordPos(UnitToFind, sLine) > 0
     then EXIT(TRUE);

     { A semicolon terminates the uses clause - everything after it is no longer in scope. }
     if Pos(';', sLine) > 0
     then EXIT(FALSE);
   end;

 Result:= FALSE;
end;


{ True if the line begins a Pascal construct that proves there is no IMPLEMENTATION
  uses clause (const, var, type, function body, begin, etc.). Used by the scanner in
  AddUnitToUses to know when to stop looking and fall through to "create uses clause". }
function StartsNonUsesSection(CONST Line: string): Boolean;
var
  TrimmedLow: string;
begin
  TrimmedLow:= LowerCase(Trim(Line));
  Result:= StartsStr('const', TrimmedLow)
        OR StartsStr('var', TrimmedLow)
        OR StartsStr('type', TrimmedLow)
        OR StartsStr('resourcestring', TrimmedLow)
        OR StartsStr('threadvar', TrimmedLow)
        OR StartsStr('function', TrimmedLow)
        OR StartsStr('procedure', TrimmedLow)
        OR StartsStr('constructor', TrimmedLow)
        OR StartsStr('destructor', TrimmedLow)
        OR StartsStr('begin', TrimmedLow)
        OR StartsStr('initialization', TrimmedLow)
        OR StartsStr('finalization', TrimmedLow)
        OR StartsStr('end', TrimmedLow);
end;


{ Add the specified unit to the IMPLEMENTATION section's "uses" clause.
  Returns True if the unit was added (or already existed in the INTERFACE/IMPLEMENTATION uses).
  Returns False if no INTERFACE or IMPLEMENTATION section is found.
  If IMPLEMENTATION exists but has no uses clause, one is created.
  Raises exception if PasBody is nil or empty. }
function AddUnitToUses(PasBody: TStringList; CONST UnitToAdd: string): Boolean;
var
  sLine, Units: string;
  IFacePos, ImplPos, UsesLine, InsertAt, i: Integer;
  MultiLine: Boolean;
begin
  if PasBody = NIL
  then RAISE EArgumentNilException.Create('AddUnitToUses: PasBody is nil.');
  if PasBody.Count = 0
  then RAISE EArgumentException.Create('AddUnitToUses: PasBody is empty.');

  { Find the INTERFACE section (0-based; -1 if missing) }
  IFacePos:= FindSection(PasBody, TRUE);
  if IFacePos < 0 then EXIT(FALSE);

  { Find the IMPLEMENTATION section }
  ImplPos:= FindSection(PasBody, FALSE);
  if ImplPos < 0 then EXIT(FALSE);

  { Check INTERFACE uses - don't leak past INTERFACE }
  if UnitIncluded(PasBody, UnitToAdd, IFacePos, ImplPos)
  then EXIT(TRUE);

  { Check IMPLEMENTATION uses }
  if UnitIncluded(PasBody, UnitToAdd, ImplPos, MaxInt)
  then EXIT(TRUE);

  { Locate the IMPLEMENTATION uses clause: a real Pascal `uses` keyword,
    not a substring inside a comment, string literal, or identifier. }
  UsesLine:= -1;
  for i:= ImplPos + 1 to PasBody.Count - 1 do
  begin
    sLine:= PasBody[i];
    if LineIsAComment(sLine) then CONTINUE;
    if Trim(sLine) = '' then CONTINUE;
    if WordPos('uses', sLine) > 0 then
    begin
      UsesLine:= i;
      BREAK;
    end;
    { Stop scanning if we hit non-uses code. The IMPLEMENTATION's uses clause (when present)
      is always the very first non-blank/non-comment construct after IMPLEMENTATION. So if the
      first such construct is anything else (const, var, function body, etc.), there is no
      uses clause at all - fall through to the "create uses clause" branch below. }
    if StartsNonUsesSection(sLine)
    then BREAK;
  end;

  if UsesLine < 0 then
  begin
    { IMPLEMENTATION exists but has no uses clause - create one right after IMPLEMENTATION }
    PasBody.Insert(ImplPos + 1, '');
    PasBody.Insert(ImplPos + 2, 'USES');
    PasBody.Insert(ImplPos + 3, '  ' + UnitToAdd + ';');
    PasBody.Insert(ImplPos + 4, '');
    EXIT(TRUE);
  end;

  sLine:= PasBody[UsesLine];

  { Multi-line if the line contains ONLY the word `uses` (optionally with trailing comment).
    Strip comments before testing so `uses // foo` still counts as multi-line. }
  MultiLine:= UpperCase(Trim(StripComments(sLine))) = 'USES';

  if MultiLine then
  begin
    { Multi-line uses - insert before the first real (non-blank, non-comment) unit line }
    InsertAt:= UsesLine + 1;
    while (InsertAt < PasBody.Count)
       AND (LineIsAComment(PasBody[InsertAt]) OR (Trim(PasBody[InsertAt]) = ''))
       do Inc(InsertAt);

    if InsertAt < PasBody.Count
    then PasBody[InsertAt]:= '  ' + UnitToAdd + ', ' + Trim(PasBody[InsertAt])
    else PasBody.Add('  ' + UnitToAdd + ';');  { Malformed file - no unit list found }
  end
  else
  begin
    { Single-line uses - insert unit at beginning }
    Units:= CopyFromTo(Trim(sLine), ' ', ';', TRUE);
    PasBody[UsesLine]:= 'USES ' + UnitToAdd + ', ' + Trim(Units);
  end;

  Result:= TRUE;
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
    We need to make sure that the string in the second parameter (Instruction) is correctly formatted. }
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
  if Instruction = '' then EXIT(FALSE);

  // Strip comments from the line
  CodeLine := Trim(StripComments(CodeLine));

  // Convert to lowercase for case-insensitive comparison
  CodeLine := LowerCase(CodeLine);
  Instruction := LowerCase(Instruction);

  // Create a regular expression to match the instruction as a word.
  // \b indicates a word boundary, so it will match "except" but not "ExceptObject".
  // TRegEx.Escape protects callers that might pass strings containing regex metachars.
  Regex := '\b' + TRegEx.Escape(Instruction) + '\b';

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
  if Instruction = '' then EXIT;
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
  Returns 0 if not found or if found as part of another identifier.
  A "whole word" means the characters before and after are not part of a Pascal identifier
  (i.e. not in [A-Z, a-z, 0-9, _]). This matches Delphi's identifier rules. }
function WordPos(const Needle, HayStack: string): Integer;
const
  IdentChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
var
  FoundPos, EndPos: Integer;
  CharBefore, CharAfter: Boolean;
begin
  if Needle = '' then EXIT(0);

  FoundPos:= PosInsensitive(Needle, HayStack);
  if FoundPos < 1 then EXIT(0);  { Not found }

  { Check character before the needle }
  if FoundPos = 1
  then CharBefore:= True  { Beginning of string - valid }
  else CharBefore:= NOT CharInSet(HayStack[FoundPos - 1], IdentChars);

  { Check character after the needle }
  EndPos:= FoundPos + Length(Needle);
  if EndPos > Length(HayStack)
  then CharAfter:= True  { End of string - valid }
  else CharAfter:= NOT CharInSet(HayStack[EndPos], IdentChars);

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
