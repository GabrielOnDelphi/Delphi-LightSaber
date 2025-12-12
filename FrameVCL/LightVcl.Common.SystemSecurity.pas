UNIT LightVcl.Common.SystemSecurity;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows,
   System.SysUtils;

procedure Hack_DisableSystemKeys(Disable: Boolean);
function  CalculatePasswordStrength(const Password: string): Integer;


IMPLEMENTATION




{ Disable task switching
  Using the SystemParametersInfo API function you can trick Windows into thinking that the screen saver is running. Doing so disables the Ctrl-Alt-Delete key sequence from displaying the "Close Program" dialog and rebooting the computer and Alt-Tab from switching to another application. It also stops Ctrl-Esc from opening the Start Menu.
  If you wish to disable those keys while your application is running call the following SystemKeys function (place it in the Implementation section of your unit's code - and call from any procedure in your application - where needed). When you call SystemKeys, if Disable is True, the keys will be disabled, False otherwise.
  After a call to SystemKeys(True) the program runs, but you are unable to Alt-Tab to it nor switch to it in the task list. You can't invoke the Start button, either. }
procedure Hack_DisableSystemKeys(Disable: Boolean);
VAR OldVal : LongInt;
begin
 SystemParametersInfo (SPI_SCREENSAVERRUNNING, Word(Disable), @OldVal, 0);
end;


function CalculatePasswordStrength(const Password: string): Integer;
begin
  Result := 0;

  // Length criteria
  if Length(Password) >= 8 then Inc(Result);

  // Uppercase letter criteria
  if Password.ToUpper <> Password then Inc(Result);

  // Lowercase letter criteria
  if Password.ToLower <> Password then Inc(Result);

  // Digit criteria
  if Password.IndexOfAny(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) >= 0 then Inc(Result);

  // Special character criteria
  if Password.IndexOfAny(['!', '@', '#', '$', '%', '^', '&', '*', '(', ')']) >= 0 then Inc(Result);
end;


end.
