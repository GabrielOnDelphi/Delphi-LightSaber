UNIT LightVcl.Common.SystemConsole;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.Classes, System.SysUtils, Vcl.Graphics;


procedure SetConsoleColor(AColor: TColor);


IMPLEMENTATION


{ Change the color of console output.
  Source: https://stackoverflow.com/questions/57980596/change-text-color-in-delphi-console-application }
procedure SetConsoleColor(AColor: TColor);
VAR
  hConsole: THandle;
  Attr: Word;
begin
  hConsole:= GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE
  then EXIT;

  case AColor of
    clRed:    Attr:= FOREGROUND_RED or FOREGROUND_INTENSITY;
    clGreen:  Attr:= FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    clBlue:   Attr:= FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    clMaroon: Attr:= FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    clPurple: Attr:= FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    clAqua:   Attr:= FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  else
    Attr:= FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;  { White/default for unsupported colors }
  end;

  SetConsoleTextAttribute(hConsole, Attr);
end;

end.
