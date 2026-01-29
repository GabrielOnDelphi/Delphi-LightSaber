UNIT LightVcl.Common.Keyboard;

{=============================================================================================================
   SYSTEM - Keyboard
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  System function to access the keyboard
  Also see cmKbShortcuts

   In this group:
     * LightVcl.Common.Shell.pas
     * csSystem.pas
     * csWindow.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms;


 procedure SimulateKeyDown  (Key : byte);
 procedure SimulateKeyUp    (Key : byte);
 procedure SimulateKeystroke(Key: byte; extra: Cardinal);
 procedure SendKeys(s: string);
 procedure SendText(text: string);                          { Set the focus to a control and send it a string}
 procedure CapsLock;                                        { Toggle the 'CAP Lock'}

 { Is key down? }
 function GetModifierKeyState: TShiftState;
 function IsCtrlDown : Boolean;
 function IsShiftDown: Boolean;
 function IsAltDown  : Boolean;


IMPLEMENTATION


{ Sending keystrokes to any window control capable of receiving keyboard input. You may use this technique to toggle the num lock, caps lock, and scroll lock keys under Windows NT. This same technique works for toggling caps lock and scroll lock keys under Windows 95. but it will not work for num lock. Note that there are four procedures provided: SimulateKeyDown(), SimulateKeyUp(), SimulateKeystroke(), and SendKeys(), to allow greater control in your ability to send keystrokes. The SimulateKeyDown(), SimulateKeyUp(), and SimulateKeystroke() procedures expect a virtural key code (like VK_F1). The SimulateKeystroke() procedure accepts an extra parameter that is useful when simulating the PrintScreen key. When extra is set to zero, the entire screen will be captured to the windows clipboard. When extra is set to one, only the active window will be captured.}
procedure SimulateKeyDown(Key : byte);
begin
  keybd_event(Key, 0, 0, 0);
end;


procedure SimulateKeyUp(Key : byte);
begin
  keybd_event(Key, 0, KEYEVENTF_KEYUP, 0);
end;


procedure SimulateKeystroke(Key: byte; extra: DWORD);
begin
  keybd_event(Key, extra, 0, 0);
  keybd_event(Key, extra, KEYEVENTF_KEYUP, 0);
end;


{ Send a string as if the keys were pressed }
procedure SendKeys(s : string);
var
  i: Integer;
  CapsLockWasOn: Boolean;
  w: Word;
begin
  { Check if Caps Lock is currently ON (low bit of GetKeyState indicates toggle state) }
  CapsLockWasOn:= (GetKeyState(VK_CAPITAL) and 1) <> 0;
  if CapsLockWasOn
  then SimulateKeystroke(VK_CAPITAL, 0);                                                           { Turn off Caps Lock to prevent case interference }
  for i:= 1 to Length(s) do
    begin
      w:= VkKeyScan(s[i]);

      { Check for valid key translation (both bytes must not be $FF) }
      if (HiByte(w) <> $FF) AND (LoByte(w) <> $FF) then
        begin
          { Hold Shift if required (bit 0 of high byte indicates Shift needed) }
          if (HiByte(w) and 1) = 1
          then SimulateKeyDown(VK_SHIFT);

          SimulateKeystroke(LoByte(w), 0);

          { Release Shift if it was held }
          if (HiByte(w) and 1) = 1
          then SimulateKeyUp(VK_SHIFT);
        end;
    end;

  { Restore Caps Lock state if it was originally on }
  if CapsLockWasOn
  then SimulateKeystroke(VK_CAPITAL, 0);
end;


{ Send a string to the currently focused control.
  Note: Ensure the target control has focus before calling this function.
  The caller is responsible for proper focus management. }
procedure SendText(text: string);
begin
  SendKeys(text);
end;



procedure CapsLock;   {Toggle the cap lock}
begin
  SimulateKeystroke(VK_CAPITAL, 0);
end;


function IsCtrlDown: Boolean;
VAR State: TKeyboardState;
begin
  Result:= FALSE;
  if GetKeyboardState(State)
  then Result:= (State[VK_CONTROL] and 128) <> 0;
end;


function IsShiftDown: Boolean;
VAR State: TKeyboardState;
begin
  Result:= FALSE;
  if GetKeyboardState(State)
  then Result:= (State[VK_SHIFT] and 128) <> 0;
end;


function IsAltDown: Boolean;
VAR State: TKeyboardState;
begin
  Result:= FALSE;
  if GetKeyboardState(State)
  then Result:= (State[VK_MENU] and 128) <> 0;
end;


function GetModifierKeyState: TShiftState;
var
  State: TKeyboardState;
begin
  Result := [];
  if GetKeyboardState(State) then 
  begin
    if ((State[vk_Shift] and 128) <> 0) 
	then Include(Result, ssShift);
    if ((State[VK_CONTROL] and 128) <> 0) 
	then Include(Result, ssCtrl);
    if ((State[VK_MENU] and 128) <> 0) 
	then Include(Result, ssAlt);
  end;
end;


end.
