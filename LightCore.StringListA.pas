UNIT LightCore.StringListA;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Ansi StringList class

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, Generics.Collections;

TYPE
  { ANSI TStringList }
  AnsiTSL = TList<AnsiString>;
  TAnsiTSL= class(AnsiTSL)
  private
    function  GetTextStr: AnsiString;
    procedure SetTextStr(const Value: AnsiString);
   public
    property Text: AnsiString read GetTextStr write SetTextStr;
  end;



IMPLEMENTATION



const
  AnsiCRLF: AnsiString = #13#10;

{ Parses a multi-line AnsiString and adds each line to the list.
  Handles CR, LF, and CRLF line endings.
  The scan stops at the length of Value, not at the first #0: a #0 stays inside its line, as in the RTL TStrings.SetTextStr (System.Classes.pas). }
procedure TAnsiTSL.SetTextStr(const Value: AnsiString);
var
  P, PEnd, Start: PAnsiChar;
  S: AnsiString;
begin
  Clear;
  P:= Pointer(Value);
  if P = nil then EXIT;
  PEnd:= P + Length(Value);

  { Fast path: scan for CR/LF characters directly }
  while P < PEnd do
  begin
    Start:= P;
    while (P < PEnd) AND NOT (P^ in [#10, #13]) do
      Inc(P);
    SetString(S, Start, P - Start);
    Add(S);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
  end;
end;


{ Concatenates all lines into a single AnsiString with CRLF line endings.
  Note: Adds CRLF after the last line as well. }
function TAnsiTSL.GetTextStr: AnsiString;
var
  i, Len, TotalSize: Integer;
  P: PAnsiChar;
  Line: AnsiString;
const
  LineBreakLen = 2;  { Length of #13#10 }
begin
  { Calculate total size needed }
  TotalSize:= 0;
  for i:= 0 to Count - 1 do
    Inc(TotalSize, Length(Self[i]) + LineBreakLen);

  SetString(Result, nil, TotalSize);
  P:= Pointer(Result);

  for i:= 0 to Count - 1 do
  begin
    Line:= Self[i];
    Len:= Length(Line);
    if Len > 0 then
    begin
      System.Move(Pointer(Line)^, P^, Len);
      Inc(P, Len);
    end;
    { Add CRLF }
    P^:= #13;
    Inc(P);
    P^:= #10;
    Inc(P);
  end;
end;


end.

