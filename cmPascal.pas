UNIT cmPascal;

{=============================================================================================================
   2023.03
   See Copyright.txt
==============================================================================================================

   Very crude routines for processing PAS files.

=============================================================================================================}


INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, System.Character, Vcl.Forms;


function ExtractObjectName(Line: string): string;
function LineIsAComment   (Line: string): Boolean;
function AddUnitToUses    (PasBody: TStringList; CONST UnitToAdd: string): Boolean;
function RelaxedSearch    (CodeLine, Instruction: string): Boolean;
function FindSection      (PasBody: TStringList; bInterface: Boolean): Integer;    { Find the INTERFACE/IMPLEMENTATION section }
function IsMethod         (const CodeLine: string): Boolean;
function SepparateComments(var CodeLine: string; out Comment: string): Boolean;

IMPLEMENTATION

USES ccCore, ccINIFile, ccIO;




{ Separate an object from its properties.
  Line is a line of Pascal code
  Example: Form1.Button1.SetFocus returns Form1.Button1 }
function ExtractObjectName(Line: string): string;
begin
  Line:= Trim(Line);
  VAR LastDot:= ccCore.LastPos('.', Line)-1;
  Result:= CopyTo(Line, 1, LastDot);
end;


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
 Result:= -1
end;



{ Returns the first line where the 'Instruction' was found }
function FindLine(PasBody: TStringList; Instruction: string; StartAt: integer): integer;
VAR
   i: Integer;
   sLine: string;
begin
 for i:= StartAt to PasBody.Count-1 do
   begin
     sLine:= PasBody[i];
     if PosInsensitive(Instruction, sLine) > 0 then EXIT(i);
   end;

 Result:= -1;
end;


{ Returns True if this unit is already included in the USES }
function UnitIncluded(PasBody: TStringList; UnitToFind: string; StartAt: integer): Boolean;
VAR
   iPos, i: Integer;
   sLine: string;
begin
 iPos:= FindLine(PasBody, 'USES', StartAt);
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



{ Checks a line of code if it is equal with instruction.
  For this we ignore all spaces and we ignore cases.
  Example:
    For these parameters the function will return true:
      CodeLine:    B:= True;
      Instruction: b:=true;
    We need to make sure that the string in the second parameter (Instruction) is correctly formated. }
function RelaxedSearch(CodeLine, Instruction: string): Boolean;     //old name:  CodeLineIs
begin
  CodeLine:= StringReplace(CodeLine, ' ', '', [rfReplaceAll]);
  Result:= PosInsensitive(Instruction, CodeLine) = 1;
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


{ extract comments (//) and process only the actual code. At the end put the comment back }
function SepparateComments(var CodeLine: string; out Comment: string): Boolean;
begin
  var iPos:= Pos('//', CodeLine);
  Result:= iPos > 0;
  if Result then
   begin
    Comment := Copy(CodeLine, iPos, MaxInt);  // Get the comment BEFORE I modify the sLine
    CodeLine:= Copy(CodeLine, 1, iPos-1);
   end;
end;


end.
