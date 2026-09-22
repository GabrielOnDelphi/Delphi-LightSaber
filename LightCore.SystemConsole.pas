UNIT LightCore.SystemConsole;

{=============================================================================================================
   2026.09.12
   www.GabrielMoraru.com

   Sets the foreground colour of the text that a console program writes, so that an error can be printed in
   red and a success message in green.
   SetConsoleColor is a procedure, so it is declared on every platform. Off Windows its body is empty and the
   console colour simply stays unchanged, which is cosmetic only and nothing depends on it.
=============================================================================================================}

INTERFACE
USES
   {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
   System.Classes, System.SysUtils, System.UITypes;


procedure SetConsoleColor(AColor: TColor);


IMPLEMENTATION


{ Change the color of console output.
  Source: https://stackoverflow.com/questions/57980596/change-text-color-in-delphi-console-application }
procedure SetConsoleColor(AColor: TColor);
{$IFDEF MSWINDOWS}
VAR
  hConsole: THandle;
  Attr: Word;
begin
  hConsole:= GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE
  then EXIT;

  case AColor of
    TColors.Red:    Attr:= FOREGROUND_RED or FOREGROUND_INTENSITY;
    TColors.Green:  Attr:= FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    TColors.Blue:   Attr:= FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    TColors.Maroon: Attr:= FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    TColors.Purple: Attr:= FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    TColors.Aqua:   Attr:= FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  else
    Attr:= FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;  { White/default for unsupported colors }
  end;

  SetConsoleTextAttribute(hConsole, Attr);
end;
{$ELSE}
begin
end;
{$ENDIF}

end.
