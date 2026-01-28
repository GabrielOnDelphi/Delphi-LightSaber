UNIT LightCore.StringList;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   TStringList class helper
     Remove duplicate items
     Remove lines containing certain text
     Trim
     Shuffle
     Reverse sort
     HighestString

   Yes, I know, we all hate class helpers because the last used class helper will overrite the other ones.

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes;

TYPE
  TTSL= class helper for TStringList
  private
    public
     procedure RemoveDuplicateString(CONST s: string);            { Removes the first occurrence of the specified string (case-insensitive) }
     procedure RemoveDuplicateFile(CONST FileName: string);       { Removes the first occurrence of the specified filename (case-insensitive) }
     procedure RemoveDuplicates;                                  { THIS WILL SORT THE LIST !!! }
     procedure RemoveEmptyLines;
     function  RemoveLines   (const BadWord: string): Integer;
     function  KeepLines     (const KeepText: string): Integer;
     procedure Trim;                                              { Trim empty spaces, tab, enters, etc on each line }
     procedure Shuffle;
     function  FindLine(const Needle: string): Integer;           { Find line that contains the specified text }
     procedure SortReverse;
     function  HighestString: string;
     function  Concatenate(const Separator: string): String;

     // Top
     function  GetTopLines(Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;
     procedure RemoveTopLines(const aCount: Integer);
   end;


 // Work on a multi-line text
 function  String2TSL     (CONST s: string): TStringList;                                                     { Converts a string to a TStringList. In other words it breaks the text to multiple lines. I need to call Free after this! }
 function  ExtractTopLines(CONST Text: string; Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;      { Returns the top x lines from a text (multiple lines) }
 function  FindLine       (CONST Needle, Haystack: string): string;


IMPLEMENTATION
USES LightCore;



{ Returns all lines concatenated in one single line.
  Good to convert a list of items into a single string, where items are separated by the specified separator.
  Note: Returns trailing separator after last item. }
function TTSL.Concatenate(const Separator: string): string;
begin
  Result:= '';
  for var s in Self do
    Result:= Result+ s+ Separator;
end;


{ Finds the first line that contains the specified text (case-sensitive).
  Returns the line index, or -1 if not found. }
function TTSL.FindLine(const Needle: string): Integer;
VAR i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if Pos(Needle, Self[i]) > 0
    then EXIT(i);
end;


{ Remove all lines that contain the specified text (case-insensitive).
  Returns the number of lines removed. }
function TTSL.RemoveLines(const BadWord: string): Integer;
VAR i: Integer;
begin
  Result:= 0;
  for i:= Count-1 downto 0 do
    if PosInsensitive(BadWord, Self[i]) > 0 then
    begin
      Delete(i);
      Inc(Result);
    end;
end;


{ Remove all lines that do NOT contain the specified text (case-insensitive).
  Returns the number of lines removed. }
function TTSL.KeepLines(const KeepText: string): Integer;
VAR i: Integer;
begin
  Result:= 0;
  for i:= Count-1 downto 0 do
    if PosInsensitive(KeepText, Self[i]) = 0 then
    begin
      Delete(i);
      Inc(Result);
    end;
end;


{ Removes all empty lines from the list. }
procedure TTSL.RemoveEmptyLines;
VAR i: Integer;
begin
  for i:= Count-1 downto 0 do
    if Self[i] = ''
    then Self.Delete(i);
end;


{ Trim whitespace (spaces, tabs, line breaks) from each line. }
procedure TTSL.Trim;
VAR i: Integer;
begin
  for i:= Count-1 downto 0 do
    Self[i]:= LightCore.RemoveFormatings(Self[i]);

  for i:= Count-1 downto 0 do
    Self[i]:= System.SysUtils.Trim(Self[i]);
end;


{ Returns the lexicographically highest string in the list.
  For example: [ABC, ABCD] returns ABCD; [ABCD, B] returns B. }
function TTSL.HighestString: string;
VAR i: Integer;
begin
  Result:= '';
  for i:= 0 to Self.Count-1 do
    if Self[i] > Result
    then Result:= Self[i];
end;


{ Compare function for reverse sorting. }
function StringListSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result:= AnsiCompareStr(List[Index2], List[Index1]);
end;


{ Sorts the list in reverse (descending) order. }
procedure TTSL.SortReverse;
begin
  CustomSort(StringListSortCompare);
end;


{ Randomly shuffles the items in the list (Fisher-Yates algorithm). }
procedure TTSL.Shuffle;
VAR i: Integer;
begin
  for i:= Count-1 downto 1 do
    Exchange(i, Random(i + 1));
end;


{ Removes the first occurrence of the specified string (case-insensitive). }
procedure TTSL.RemoveDuplicateString(const s: string);
VAR i: Integer;
begin
  for i:= Count-1 downto 0 do
    if SameText(s, Self[i]) then
    begin
      Delete(i);
      Break;
    end;
end;


{ Removes the first occurrence of the specified filename (case-insensitive). }
procedure TTSL.RemoveDuplicateFile(const FileName: string);
var
   i: Integer;
begin
  for i:= Count-1 downto 0 do
    if SameText(FileName, Self[i]) then
    begin
      Delete(i);
      Break;
    end;
end;


{ Removes duplicate entries from the list.
  WARNING: This will SORT the list! }
procedure TTSL.RemoveDuplicates;
var
  Buffer: TStringList;
  i: Integer;
begin
  Buffer:= TStringList.Create;
  try
    Buffer.Sorted:= True;
    Buffer.Duplicates:= dupIgnore;
    Buffer.BeginUpdate;
    for i:= 0 to Count - 1 do
      Buffer.Add(Self[i]);
    Buffer.EndUpdate;
    Assign(Buffer);
  finally
    FreeAndNil(Buffer);
  end;

  { Cancel Sorted flag to allow editing, otherwise "Operation not allowed on sorted list" }
  Sorted:= FALSE;
end;






{ TOP }

{ Remove the first x lines from the list. }
procedure TTSL.RemoveTopLines(CONST aCount: Integer);
var
   i: Integer;
begin
  if aCount > Count
  then raise EArgumentOutOfRangeException.CreateFmt('RemoveTopLines: aCount (%d) exceeds list Count (%d)', [aCount, Count]);

  for i:= aCount-1 downto 0 do
    Self.Delete(i);
end;


{ Returns the first Count lines as a single string.
  If IgnoreEmptyLines is True, empty lines are skipped (not counted). }
function TTSL.GetTopLines(Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;
var
   s: string;
   Total: Integer;
begin
  Total:= 0;
  Result:= '';

  for s in Self do
  begin
    { Skip empty lines if requested }
    if IgnoreEmptyLines AND (s = '')
    then Continue;

    Inc(Total);
    Result:= Result + s + CRLF;
    if Total = Count
    then Break;
  end;

  Result:= RemoveLastEnter(Result);
end;












{ Converts a multi-line string to a TStringList.
  IMPORTANT: Caller must free the returned object! }
function String2TSL(const s: string): TStringList;
begin
  Result:= TStringList.Create;
  Result.Text:= s;
end;



{-----------------------------------------------------------------------------
   EXTRACT
-----------------------------------------------------------------------------}

{ Returns the first Count lines from a multi-line text. }
function ExtractTopLines(const Text: string; Count: Integer; IgnoreEmptyLines: Boolean= TRUE): string;
var TSL: TStringList;
begin
  TSL:= TStringList.Create;
  try
    TSL.Text:= Text;
    Result:= TSL.GetTopLines(Count, IgnoreEmptyLines);
  finally
    FreeAndNil(TSL);
  end;
end;





{ Searches for Needle (partial match) in a multi-line Haystack.
  Returns the entire line containing the Needle, or empty string if not found. }
function FindLine(const Needle, Haystack: string): string;
var
   TSL: TStringList;
   s: string;
begin
  Result:= '';
  TSL:= String2TSL(Haystack);
  try
    for s in TSL do
      if Pos(Needle, s) > 0
      then EXIT(s);
  finally
    FreeAndNil(TSL);
  end;
end;


end.

