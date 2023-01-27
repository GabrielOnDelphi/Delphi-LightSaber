UNIT cmKeyboard;

{=============================================================================================================
   2023.01
   See Copyright.txt
==============================================================================================================

  System function to access the keyboard
  Also see cmKbShortcuts

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
  i : integer;
  flag : bool;
  w : word;
begin
  flag := not GetKeyState(VK_CAPITAL) and 1 = 0;                                                   {Get the state of the caps lock key}
  if flag then SimulateKeystroke(VK_CAPITAL, 0);                                                   {If the caps lock key is on then turn it off}
  for i := 1 to Length(s) do
  begin
    w := VkKeyScan(s[i]);                                                                          {If there is not an error in the key translation}
    if ((HiByte(w) <> $FF) and (LoByte(w) <> $FF)) then
    begin                                                                                          {If the key requires the shift key down - hold it down}
      if HiByte(w) and 1 = 1 then SimulateKeyDown(VK_SHIFT);
      SimulateKeystroke(LoByte(w), 0);                                                             {Send the VK_KEY}
      if HiByte(w) and 1 = 1 then SimulateKeyUp(VK_SHIFT);                                         {If the key required the shift key down - release it}
    end;
  end;
  if flag then SimulateKeystroke(VK_CAPITAL, 0);                                                   {if the caps lock key was on at start, turn it back on}
end;


procedure SendText(text: string);                                                                  {Set the focus to a window (edit control) and send it a string}
begin
  Application.ProcessMessages;   // https://blog.dummzeuch.de/2018/09/29/calling-application-processmessages-in-a-delphi-program/
  SendKeys(text);
end;



procedure CapsLock;   {Toggle the cap lock}
begin
  SimulateKeystroke(VK_CAPITAL, 0);
end;


function IsCtrlDown: Boolean;
VAR State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Control] And 128) <> 0) ;
end;


function IsShiftDown: Boolean;
VAR State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Shift] and 128) <> 0) ;
end;


function IsAltDown: Boolean;
VAR State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Menu] and 128) <> 0) ;
end;


function GetModifierKeyState: TShiftState;
var
  State: TKeyboardState;
begin
  Result := [];
  if GetKeyboardState(State) then begin
    if ((State[vk_Shift] and 128) <> 0) then
      Include(Result, ssShift);
    if ((State[VK_CONTROL] and 128) <> 0) then
      Include(Result, ssCtrl);
    if ((State[VK_MENU] and 128) <> 0) then
      Include(Result, ssAlt);
  end;
end;


end.
