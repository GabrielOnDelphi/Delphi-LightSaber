UNIT LightCore.RttiSetToString;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

   Converts a set to its string representation (and vice-versa) using RTTI.
   Adapted from ansi-version listed at: stackoverflow.com/questions/6351355

   Limitations:
     - Sets are limited to 32 elements (Integer size)
     - Element names must match exactly (case-sensitive)

   Example:
     procedure Test;
     var
       A: TAlignSet;
       S: string;
     begin
       A := [alLeft, alRight];
       S := SetToString(TypeInfo(TAlignSet), A);
       ShowMessage(Format('%s ($%x)', [S, Byte(A)]));

       S := '[alNone, alRight, alCustom]';
       StringToSet(TypeInfo(TAlignSet), A, S);
       ShowMessage(Format('%s ($%x)', [SetToString(TypeInfo(TAlignSet), A), Byte(A)]));
     end;

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.TypInfo;


function  SetToString(Info: PTypeInfo; const SetParam): string;
procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: string);


IMPLEMENTATION


{ Extracts the ordinal value from a set based on its storage size. }
function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Assert(Info <> NIL, 'GetOrdValue: Info parameter is nil');
  Result:= 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte: Result:= Byte(SetParam);
    otSWord, otUWord: Result:= Word(SetParam);
    otSLong, otULong: Result:= Integer(SetParam);
  end;
end;


{ Sets the ordinal value of a set based on its storage size. }
procedure SetOrdValue(Info: PTypeInfo; var SetParam; Value: Integer);
begin
  Assert(Info <> NIL, 'SetOrdValue: Info parameter is nil');

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte: Byte(SetParam):= Value;
    otSWord, otUWord: Word(SetParam):= Value;
    otSLong, otULong: Integer(SetParam):= Value;
  end;
end;


{ Converts a set to a comma-separated string of element names.
  Example: [alLeft, alRight] -> 'alLeft,alRight' }
function SetToString(Info: PTypeInfo; const SetParam): string;
var
  I: Integer;
  S: TIntegerSet;
  EnumInfo: PTypeInfo;
begin
  Assert(Info <> NIL, 'SetToString: Info parameter is nil');
  Result:= '';

  Integer(S):= GetOrdValue(Info, SetParam);
  EnumInfo:= GetTypeData(Info)^.CompType^;

  for I:= 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> ''
      then Result:= Result + ',';
      Result:= Result + GetEnumName(EnumInfo, I);
    end;
end;


{ Parses a comma-separated string of element names into a set.
  Accepts formats: 'alLeft,alRight' or '[alLeft, alRight]'
  Returns empty set if any element name is invalid. }
procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: string);
var
  P: PChar;
  EnumInfo: PTypeInfo;
  EnumName: string;
  EnumValue, SetValue: Integer;

  { Extracts the next word (enum name) from the string. }
  function NextWord(var P: PChar): string;
  var
    I: Integer;
  begin
    I:= 0;

    { Find delimiter or end of string }
    while NOT CharInSet(P[I], [',', ' ', #0, ']']) do
      Inc(I);

    SetString(Result, P, I);

    { Skip delimiters }
    while CharInSet(P[I], [',', ' ', ']']) do
      Inc(I);
    Inc(P, I);
  end;

begin
  Assert(Info <> NIL, 'StringToSet: Info parameter is nil');

  SetOrdValue(Info, SetParam, 0);
  if Value = '' then EXIT;

  SetValue:= 0;
  P:= PChar(Value);

  { Skip leading bracket and whitespace }
  while CharInSet(P^, ['[', ' ']) do
    Inc(P);

  EnumInfo:= GetTypeData(Info)^.CompType^;
  EnumName:= NextWord(P);

  while EnumName <> '' do
  begin
    EnumValue:= GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
    begin
      { Invalid enum name - return empty set }
      SetOrdValue(Info, SetParam, 0);
      EXIT;
    end;
    Include(TIntegerSet(SetValue), EnumValue);
    EnumName:= NextWord(P);
  end;

  SetOrdValue(Info, SetParam, SetValue);
end;



end.
