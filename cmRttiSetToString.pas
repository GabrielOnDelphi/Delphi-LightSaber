UNIT cmRttiSetToString;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

   Converts a set to its string representation (and vice-versa)
   Adapted from ansi-version listed at: stackoverflow.com/questions/6351355


   Procedure Test;
   var
     A: TAlignSet;
     S: AnsiString;
   begin
     A := [alLeft, alRight];
     S := SetToString(TypeInfo(TAlignSet), A, True);
     ShowMessage(Format('%s ($%x)', [S, Byte(A)]));

     S := '[alNone, alRight, alCustom]';
     StringToSet(TypeInfo(TAlignSet), A, S);
     ShowMessage(Format('%s ($%x)', [SetToString(TypeInfo(TAlignSet), A, True), Byte(A)]));
   end;

=============================================================================================================}

INTERFACE

USES
  SysUtils, TypInfo;


function  SetToString(Info: PTypeInfo; const SetParam): String;
procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: String);


IMPLEMENTATION


function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte: Result := Byte(SetParam);
    otSWord, otUWord: Result := Word(SetParam);
    otSLong, otULong: Result := Integer(SetParam);
  end;
end;


procedure SetOrdValue(Info: PTypeInfo; var SetParam; Value: Integer);
begin
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte: Byte(SetParam) := Value;
    otSWord, otUWord: Word(SetParam) := Value;
    otSLong, otULong: Integer(SetParam) := Value;
  end;
end;


function SetToString(Info: PTypeInfo; const SetParam): String;
var
  I: Integer;
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
begin
  Result := '';

  Integer(S) := GetOrdValue(Info, SetParam);
  TypeInfo   := GetTypeData(Info)^.CompType^;

  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> ''
      then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
end;


procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: String);
var
  P: PChar;
  EnumInfo: PTypeInfo;
  EnumName: String;
  EnumValue, SetValue: Longint;

  function NextWord(var P: PChar): String;
  var
    I: Integer;
  begin
    I := 0;

    // Find first white space
    while NOT CharInSet(P[I], [',', ' ', #0,']']) do
      Inc(I);

    SetString(Result, P, I);

    // Skip whitespace
    while CharInSet(P[I], [',', ' ',']']) do
      Inc(I);
    Inc(P, I);
  end;

begin
  SetOrdValue(Info, SetParam, 0);
  if Value = '' then EXIT;
  SetValue := 0;
  P := PChar(Value);

  // skip leading bracket and whitespace
  while CharInSet(P^, ['[',' ']) do
    Inc(P);

  EnumInfo := GetTypeData(Info)^.CompType^;
  EnumName := NextWord(P);

  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
    begin
      SetOrdValue(Info, SetParam, 0);
      Exit;
    end;
    Include(TIntegerSet(SetValue), EnumValue);
    EnumName := NextWord(P);
  end;

  SetOrdValue(Info, SetParam, SetValue);
end;



end.
