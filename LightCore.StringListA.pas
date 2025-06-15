UNIT LightCore.StringListA;

{=============================================================================================================
   2023.01
   www.GabrielMoraru.com
   See Copyright file
==============================================================================================================

   Ansi StringList class

=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Types, System.AnsiStrings, Generics.Collections;

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



procedure TAnsiTSL.SetTextStr(const Value: AnsiString);
var
  P, Start, LB: PAnsiChar;
  S: AnsiString;
  LineBreakLen: Integer;
begin
 Clear;
 P := Pointer(Value);
 if P <> nil then
   if CompareStr(#13#10, sLineBreak) = 0 then
   begin
     // This is a lot faster than using StrPos/AnsiStrPos when LineBreak is the default (#13#10)
     while P^ <> #0 do
     begin
       Start := P;
       while not (P^ in [#0, #10, #13]) do Inc(P);
       SetString(S, Start, P - Start);
       Add(S);
       if P^ = #13 then Inc(P);
       if P^ = #10 then Inc(P);
     end;
   end
   else
   begin
     LineBreakLen := Length(#13#10);
     while P^ <> #0 DO
     begin
       Start := P;
       LB := System.AnsiStrings.AnsiStrPos(P, PAnsiChar(#13#10));
       WHILE (P^ <> #0) AND (P <> LB)
        DO Inc(P);
       SetString(S, Start, P - Start);
       Add(S);
       if P = LB
       then Inc(P, LineBreakLen);
     end;
   end;
end;


function TAnsiTSL.GetTextStr: AnsiString;
var
  I, L, Size: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
begin
  //Count := GetCount;
  Size := 0;
  LB := #13#10;
  for I := 0 to Count - 1 do Inc(Size, Length(Self[I]) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Self[I];
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;


end.

