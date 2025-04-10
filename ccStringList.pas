UNIT ccStringList;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
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
   System.SysUtils, System.Classes, System.Types, System.AnsiStrings, Generics.Collections;

TYPE
  TTSL= class helper for TStringList
    public
     procedure RemoveDuplicateString(CONST s: string);            { Removes the specified item from the list if it exists there }
     procedure RemoveDuplicateFile(CONST FileName: string);       { Similar as above but it uses a faster algorithm specialized in comparing file names }
     procedure RemoveDuplicates;                                  { THIS WILL SORT THE LIST !!! }
     procedure RemoveEmptyLines;
     procedure RemoveTopLines(const aCount: Integer);
     function  RemoveLines   (const BadWord: string): Integer;
     function  KeepLines     (const KeepText: string): Integer;
     procedure Trim;                                              { Trim empty spaces, tab, enters, etc on each line }
     procedure Shuffle;
     function  FindLine(const Needle: string): Integer;           { Find line that contains the specified text }
     procedure SortReverse;
     function  HighestString: string;
     function  Concatenate(const Separator: string): String;
   end;



IMPLEMENTATION

USES ccCore;



{ Returns all lines concatenated in one single line.
Good to convert a list of items into a single string, where items are separated by semicolumn }
function TTSL.Concatenate(const Separator: string): String;
begin
  Result:= '';
  for var s in Self do
    Result:= Result+ s+ Separator;
end;


function TTSL.FindLine(CONST Needle: string): Integer;    { Find line that contains the specified text }
VAR
   i: Integer;
begin
 Result:= -1;
 for i:= 0 to Count-1 DO
  if Pos(Needle, Self[i]) > 0
  then EXIT(i);
end;


function TTSL.RemoveLines(const BadWord: string): Integer;  { Remove all lines that contains the specified text }
VAR i: Integer;
begin
 Result:= 0;
 for i:= Count-1 downto 0 DO
  if PosInsensitive(BadWord, self[i]) > 0 then   { if text found, then delete line }
   begin
    Delete(i);
    Inc(Result);
   end;
end;


function TTSL.KeepLines(CONST KeepText: string): Integer;    { Remove all lines that does not contain this text }
VAR i: Integer;
begin
 Result:= 0;
 for i:= Count-1 downto 0 DO
  if PosInsensitive(KeepText, self[i])= 0 then { if text not found, then delete line }
   begin
    Delete(i);
    Inc(Result);
   end;
end;


procedure TTSL.RemoveTopLines(CONST aCount: Integer);    { Remove the firs x lines }
VAR
   i: Integer;
begin
 Assert(acount <= Count);
 for i:= aCount-1 downto 0
  DO Self.Delete(i);
end;


procedure TTSL.RemoveEmptyLines;     
VAR
   i: Integer;
begin
 for i:= Count-1 downto 0 DO
   if Self[i]= ''
   then Self.Delete(i);
end;


procedure TTSL.Trim;    { Trim empty spaces, tab, enters, etc on each line }
VAR
   i: Integer;
begin
 for i:= Count-1 downto 0 DO
    Self[i]:= ccCore.RemoveFormatings(Self[i]);

 for i:= Count-1 downto 0 DO
    Self[i]:= System.SysUtils.Trim(Self[i]);
end;


{ Returns the 'highest' string. For example, if the list contains ABC and ABCD it will return the ABCD string. If the list contains ABCD and B it will return B }
function TTSL.HighestString: string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 0 to Self.Count-1 DO
   if Self[i] > Result
   then Result:= Self[i];
end;


function StringListSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareStr(List[Index2], List[Index1]); // Will sort in reverse order
end;


procedure TTSL.SortReverse;
begin
 CustomSort(StringListSortCompare);
end;


procedure TTSL.Shuffle;
VAR i: Integer;
begin
  for i:= Count-1 downto 1 DO
    Exchange(i, Random(i+1));
end;


procedure TTSL.RemoveDuplicateString(CONST s: string);
VAR
   i: Integer;
begin
 for i:= Count-1 downto 0 DO
  if SameText(s, Self[i]) then
   begin
    Delete(i);
    Break;
   end;
end;


procedure TTSL.RemoveDuplicateFile(CONST FileName: string);
VAR
   i: Integer;
begin
 for i:= Count-1 downto 0 DO
  if SameText(FileName, Self[i]) then
   begin
    Delete(i);
    Break;
   end;
end;


procedure TTSL.RemoveDuplicates;             { THIS WILL SORT THE LIST !!! }
VAR
  buffer: TStringList;
  cnt: Integer;
begin
 Sort;
 buffer := TStringList.Create;
 TRY
   buffer.Sorted:= True;
   buffer.Duplicates:= System.Types.dupIgnore;
   buffer.BeginUpdate;
   for cnt := 0 to Count - 1
     DO buffer.Add(Self[cnt]) ;
   buffer.EndUpdate;
   Assign(buffer);
 FINALLY
   FreeandNil(buffer);
 END;

 Sorted:= FALSE;    { We need to cancel Sorted now, otherwise I get "EStringListError: Operation not allowed on sorted list" when I try to edit the lines }
end;


end.

