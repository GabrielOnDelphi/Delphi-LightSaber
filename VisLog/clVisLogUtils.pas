UNIT clVisLogUtils;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics, ccCore, csSystem, cbDialogs;

TYPE
  TLogVerbLvl= (lvDebug, lvVerbose, lvHints {Default}, lvInfos, lvImportant, lvWarnings, lvErrors);  { Exist also 7 which is of type 'Msg' and it is always shown in log }

CONST
   DefaultVerbosity= lvInfos;

function Verbosity2String(Verbosity: TLogVerbLvl): string;
function Verbosity2Color (Verbosity: TLogVerbLvl): TColor;


IMPLEMENTATION



function Verbosity2String(Verbosity: TLogVerbLvl): string;
begin
 CASE Verbosity of
  lvDebug    : Result:= 'Debug';
  lvVerbose  : Result:= 'Verbose';
  lvHints    : Result:= 'Hints';
  lvInfos    : Result:= 'Info';           { This is the default level of verbosity }
  lvImportant: Result:= 'Important';
  lvWarnings : Result:= 'Warnings';
  lvErrors   : Result:= 'Errors';
 else
   Raise Exception.Create('Invalid verbosity');
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
