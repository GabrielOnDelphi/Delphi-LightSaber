unit Test.LightCore.Keyboard;

{=============================================================================================================
   2026.10.07
   Unit tests for LightCore.Keyboard.pas
   Tests keyboard simulation and key state detection functions.

   Key state: the tests write the keyboard state table of the test thread with SetKeyboardState, then read it back through the routine under test. That table belongs to the calling thread only, so nothing outside the test EXE sees the change (https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setkeyboardstate). The original table is restored after each test.

   Keystroke simulation: TKeyCatcher installs a low-level keyboard hook that counts the injected events of one key (or of every key) and swallows them, so the key never reaches the system or the window that has the focus (https://learn.microsoft.com/en-us/windows/win32/winmsg/lowlevelkeyboardproc).

   The whole fixture is compiled only when MSWINDOWS is defined. LightCore.Keyboard declares its 4 key-state functions only on Windows, and every test here names an identifier from Winapi.Windows.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  LightCore.Keyboard;

type
  [TestFixture]
  TTestKeyboard = class
  private
    FSavedState: TKeyboardState;
    procedure SetModifiers(Shift, Ctrl, Alt: Boolean);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Key State Tests }
    [Test]
    procedure Test_IsCtrlDown_FollowsKeyboardState;

    [Test]
    procedure Test_IsShiftDown_FollowsKeyboardState;

    [Test]
    procedure Test_IsAltDown_FollowsKeyboardState;

    [Test]
    procedure Test_GetModifierKeyState_ReturnsShiftState;

    [Test]
    procedure Test_GetModifierKeyState_NoKeysPressed;

    { Keystroke Simulation Tests }
    [Test]
    procedure Test_SimulateKeystroke_InjectsDownAndUp;

    [Test]
    procedure Test_CapsLock_InjectsCapitalKey;

    [Test]
    procedure Test_SendKeys_EmptyString;

    [Test]
    procedure Test_SendText_EmptyString;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

TYPE
  { KBDLLHOOKSTRUCT - not declared in Winapi.Windows.
    https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-kbdllhookstruct }
  TKbdLLHookStruct = record
    vkCode     : DWORD;
    scanCode   : DWORD;
    flags      : DWORD;
    time       : DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  PKbdLLHookStruct = ^TKbdLLHookStruct;

CONST
  LLKHF_INJECTED = $00000010;
  LLKHF_UP       = KF_UP shr 8;
  ANY_KEY        = 0;     { No key has the virtual-key code 0, so TKeyCatcher.Start(ANY_KEY) watches every key }

TYPE
  { Counts the injected key-down and key-up events of one virtual key (or of every key, for ANY_KEY), and swallows them.
    A hook procedure receives no user data, so the counters are class vars. }
  TKeyCatcher = class
  strict private
    class var Hook: HHOOK;
    class var WatchedKey: DWORD;
    class function HookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; static;
  public
    class var Downs: Integer;
    class var Ups: Integer;
    class procedure Start(VirtualKey: Byte);
    class procedure WaitForEvents(Count: Integer);
    class procedure Stop;
  end;


class function TKeyCatcher.HookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT;
VAR
  Info: PKbdLLHookStruct;
begin
  if nCode = HC_ACTION then
    begin
      Info:= PKbdLLHookStruct(lParam);
      if ((WatchedKey = ANY_KEY) OR (Info.vkCode = WatchedKey)) AND ((Info.flags AND LLKHF_INJECTED) <> 0) then
        begin
          if (Info.flags AND LLKHF_UP) <> 0
          then Inc(Ups)
          else Inc(Downs);
          EXIT(1);  { A nonzero result stops the event: the key never reaches the system }
        end;
    end;
  Result:= CallNextHookEx(Hook, nCode, wParam, lParam);
end;


class procedure TKeyCatcher.Start(VirtualKey: Byte);
begin
  Downs:= 0;
  Ups:= 0;
  WatchedKey:= VirtualKey;
  Hook:= SetWindowsHookEx(WH_KEYBOARD_LL, @TKeyCatcher.HookProc, HInstance, 0);
  if Hook = 0
  then RaiseLastOSError;
end;


{ The system calls a low-level hook by sending a message to the thread that installed it, so this thread must pump messages until the events arrive }
class procedure TKeyCatcher.WaitForEvents(Count: Integer);
VAR
  Msg: TMsg;
  StartTime: UInt64;
begin
  StartTime:= GetTickCount64;
  while (Downs + Ups < Count) AND (GetTickCount64 - StartTime < 3000) do
    begin
      while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        DispatchMessage(Msg);
      Sleep(5);
    end;

  { A short extra pump, so an event too many is counted too }
  StartTime:= GetTickCount64;
  while GetTickCount64 - StartTime < 100 do
    begin
      while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        DispatchMessage(Msg);
      Sleep(5);
    end;
end;


class procedure TKeyCatcher.Stop;
begin
  if Hook <> 0 then
    begin
      UnhookWindowsHookEx(Hook);
      Hook:= 0;
    end;
end;




procedure TTestKeyboard.Setup;
begin
  Assert.IsTrue(GetKeyboardState(FSavedState), 'GetKeyboardState failed');
end;


procedure TTestKeyboard.TearDown;
begin
  SetKeyboardState(FSavedState);
end;


{ Writes the three modifier keys into the keyboard state table of this thread. Bit 7 set = key down. }
procedure TTestKeyboard.SetModifiers(Shift, Ctrl, Alt: Boolean);
VAR
  State: TKeyboardState;
begin
  State:= FSavedState;
  if Shift then State[VK_SHIFT]  := $80 else State[VK_SHIFT]  := 0;
  if Ctrl  then State[VK_CONTROL]:= $80 else State[VK_CONTROL]:= 0;
  if Alt   then State[VK_MENU]   := $80 else State[VK_MENU]   := 0;
  Assert.IsTrue(SetKeyboardState(State), 'SetKeyboardState failed');
end;


{ Key State Tests }

procedure TTestKeyboard.Test_IsCtrlDown_FollowsKeyboardState;
begin
  SetModifiers(FALSE, TRUE, FALSE);
  Assert.IsTrue(IsCtrlDown, 'Ctrl is down in the keyboard state');

  SetModifiers(TRUE, FALSE, TRUE);
  Assert.IsFalse(IsCtrlDown, 'Ctrl is up in the keyboard state (Shift and Alt are down)');
end;


procedure TTestKeyboard.Test_IsShiftDown_FollowsKeyboardState;
begin
  SetModifiers(TRUE, FALSE, FALSE);
  Assert.IsTrue(IsShiftDown, 'Shift is down in the keyboard state');

  SetModifiers(FALSE, TRUE, TRUE);
  Assert.IsFalse(IsShiftDown, 'Shift is up in the keyboard state (Ctrl and Alt are down)');
end;


procedure TTestKeyboard.Test_IsAltDown_FollowsKeyboardState;
begin
  SetModifiers(FALSE, FALSE, TRUE);
  Assert.IsTrue(IsAltDown, 'Alt is down in the keyboard state');

  SetModifiers(TRUE, TRUE, FALSE);
  Assert.IsFalse(IsAltDown, 'Alt is up in the keyboard state (Shift and Ctrl are down)');
end;


procedure TTestKeyboard.Test_GetModifierKeyState_ReturnsShiftState;
begin
  SetModifiers(TRUE, FALSE, TRUE);
  Assert.IsTrue(GetModifierKeyState = [ssShift, ssAlt], 'Shift + Alt down must give [ssShift, ssAlt]');

  SetModifiers(FALSE, TRUE, FALSE);
  Assert.IsTrue(GetModifierKeyState = [ssCtrl], 'Ctrl down must give [ssCtrl]');
end;


procedure TTestKeyboard.Test_GetModifierKeyState_NoKeysPressed;
begin
  SetModifiers(FALSE, FALSE, FALSE);
  Assert.IsTrue(GetModifierKeyState = [], 'No modifier down must give an empty set');
end;


{ Keystroke Simulation Tests }

procedure TTestKeyboard.Test_SimulateKeystroke_InjectsDownAndUp;
begin
  TKeyCatcher.Start(VK_SCROLL);
  try
    SimulateKeystroke(VK_SCROLL, 0);
    TKeyCatcher.WaitForEvents(2);
  finally
    TKeyCatcher.Stop;
  end;

  Assert.AreEqual(1, TKeyCatcher.Downs, 'SimulateKeystroke must inject one key-down');
  Assert.AreEqual(1, TKeyCatcher.Ups,   'SimulateKeystroke must inject one key-up');
end;


procedure TTestKeyboard.Test_CapsLock_InjectsCapitalKey;
begin
  { The hook swallows the events, so the Caps Lock state of the machine does not change }
  TKeyCatcher.Start(VK_CAPITAL);
  try
    CapsLock;
    TKeyCatcher.WaitForEvents(2);
  finally
    TKeyCatcher.Stop;
  end;

  Assert.AreEqual(1, TKeyCatcher.Downs, 'CapsLock must inject one Caps Lock key-down');
  Assert.AreEqual(1, TKeyCatcher.Ups,   'CapsLock must inject one Caps Lock key-up');
end;


{ An empty string presses no key. The only keys SendKeys may press for it are the two Caps Lock toggles around the text,
  and only when Caps Lock is on. The hook watches EVERY injected key and swallows it, so a stray key never reaches the window that has the focus. }
procedure TTestKeyboard.Test_SendKeys_EmptyString;
VAR
  CapsToggles: Integer;
begin
  if (GetKeyState(VK_CAPITAL) AND 1) <> 0
  then CapsToggles:= 2
  else CapsToggles:= 0;

  TKeyCatcher.Start(ANY_KEY);
  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        SendKeys('');
      end, 'SendKeys('''') must not raise');
    TKeyCatcher.WaitForEvents(2 * CapsToggles);
  finally
    TKeyCatcher.Stop;
  end;

  Assert.AreEqual(CapsToggles, TKeyCatcher.Downs, 'SendKeys('''') must inject no key-down except the Caps Lock toggles');
  Assert.AreEqual(CapsToggles, TKeyCatcher.Ups,   'SendKeys('''') must inject no key-up except the Caps Lock toggles');
end;


procedure TTestKeyboard.Test_SendText_EmptyString;
VAR
  CapsToggles: Integer;
begin
  if (GetKeyState(VK_CAPITAL) AND 1) <> 0
  then CapsToggles:= 2
  else CapsToggles:= 0;

  TKeyCatcher.Start(ANY_KEY);
  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        SendText('');
      end, 'SendText('''') must not raise');
    TKeyCatcher.WaitForEvents(2 * CapsToggles);
  finally
    TKeyCatcher.Stop;
  end;

  Assert.AreEqual(CapsToggles, TKeyCatcher.Downs, 'SendText('''') must inject no key-down except the Caps Lock toggles');
  Assert.AreEqual(CapsToggles, TKeyCatcher.Ups,   'SendText('''') must inject no key-up except the Caps Lock toggles');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestKeyboard);

{$ENDIF}

end.
