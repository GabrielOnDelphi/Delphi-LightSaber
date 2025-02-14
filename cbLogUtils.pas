UNIT cbLogUtils;

{=============================================================================================================
   Gabriel Moraru
   2025.01
   www.GabrielMoraru.com
   See Copyright file
=============================================================================================================}

INTERFACE

{$I Frameworks.inc}

USES
   System.SysUtils,
   system.UITypes,
   {$IFDEF MsWindows}
   Vcl.Graphics,
   //{$ELSE FRAMEWORK_FMX}
   //FMX.Graphics,
   {$Endif}
   cbLogTypes,
   ccColors;

//{$IFDEF FRAMEWORK_FMX}
//TYPE TColor= TAlphaColor;
//{$ENDIF}


function Verbosity2String(Verbosity: TLogVerbLvl): string;
function Verbosity2Color (Verbosity: TLogVerbLvl): TColor;


IMPLEMENTATION


function Verbosity2String(Verbosity: TLogVerbLvl): string;
begin
 case Verbosity of
   lvDebug     : Result := 'Debug';
   lvVerbose   : Result := 'Verbose';
   lvHints     : Result := 'Hints';
   lvInfos     : Result := 'Info';      { This is the default level of verbosity }
   lvImportant : Result := 'Important';
   lvWarnings  : Result := 'Warnings';
   lvErrors    : Result := 'Errors';
 else
   RAISE Exception.Create('Invalid verbosity');
 end;
end;


function Verbosity2Color(Verbosity: TLogVerbLvl): TColor;
begin
 CASE Verbosity of
  lvDebug    : Result:= clSilverLight;
  lvVerbose  : Result:= clSilverDark;
  lvHints    : Result:= clGray;
  lvInfos    : Result:= clBlack;
  lvImportant: Result:= clOrangeDk;
  lvWarnings : Result:= clOrange;
  lvErrors   : Result:= clRed;
 else
   RAISE exception.Create('Invalid log verbosity!');
 end;
end;



end.
