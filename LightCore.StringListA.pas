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
   System.SysUtils, System.AnsiStrings, Generics.Collections;

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
  Handles CR, LF, and CRLF line endings. }
procedure TAnsiTSL.SetTextStr(const Value: AnsiString);
var
  P, Start: PAnsiChar;
  S: AnsiString;
begin
  Clear;
  P:= Pointer(Value);
  if P = nil then EXIT;

  { Fast path: scan for CR/LF characters directly }
  while P^ <> #0 do
  begin
    Start:= P;
    while NOT (P^ in [#0, #10, #13]) do
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

