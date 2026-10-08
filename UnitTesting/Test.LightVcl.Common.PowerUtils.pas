unit Test.LightVcl.Common.PowerUtils;

{=============================================================================================================
   Unit tests for LightVcl.Common.PowerUtils.pas
   Tests power management utility functions.

   IMPORTANT: Only read-only/query functions are tested.
   Functions that would actually sleep/shutdown the system are NOT tested for safety.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestPowerUtils = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { PowerStatus Tests - Read-only, safe to call }
    [Test]
    procedure TestPowerStatus_ReturnsValidValue;

    [Test]
    procedure TestPowerStatusString_ReturnsNonEmpty;

    [Test]
    procedure TestPowerStatusString_ContainsExpectedText;

    { BatteryLeft Tests }
    [Test]
    procedure TestBatteryLeft_ReturnsValidRange;

    [Test]
    procedure TestBatteryLeft_NotAbove100;

    { BatteryAsText Tests }
    [Test]
    procedure TestBatteryAsText_ExactText;

    [Test]
    procedure TestBatteryAsText_NoErrorMessage;

    { Power Capability Tests - External DLL functions, safe to query }
    [Test]
    procedure TestIsHibernateAllowed_ReturnsBool;

    [Test]
    procedure TestIsPwrSuspendAllowed_ReturnsBool;

    [Test]
    procedure TestIsPwrShutdownAllowed_ReturnsBool;

    { IsScreenSaverOn Tests }
    [Test]
    procedure TestIsScreenSaverOn_ReturnsBool;
  end;

implementation

uses
  Winapi.Windows,
  LightVcl.Common.PowerUtils;


procedure TTestPowerUtils.Setup;
begin
  // No setup needed for read-only tests
end;


procedure TTestPowerUtils.TearDown;
begin
  // No cleanup needed
end;


{ PowerStatus Tests }

{ Expected TPowerType from the documented ACLineStatus codes: 0 = offline (battery), 1 = online (AC), 255 = unknown.
  https://learn.microsoft.com/en-us/windows/win32/api/winbase/ns-winbase-system_power_status }
procedure TTestPowerUtils.TestPowerStatus_ReturnsValidValue;
var
  SysPowerStatus: TSystemPowerStatus;
  Expected: TPowerType;
begin
  Assert.IsTrue(GetSystemPowerStatus(SysPowerStatus), 'GetSystemPowerStatus failed');
  case SysPowerStatus.ACLineStatus of
    0: Expected:= pwTypeBat;
    1: Expected:= pwTypeAC;
  else Expected:= pwUnknown;
  end;

  Assert.AreEqual(Ord(Expected), Ord(PowerStatus), 'PowerStatus for ACLineStatus = ' + IntToStr(SysPowerStatus.ACLineStatus));
end;


procedure TTestPowerUtils.TestPowerStatusString_ReturnsNonEmpty;
var
  StatusStr: string;
begin
  StatusStr:= PowerStatusString;
  Assert.IsNotEmpty(StatusStr, 'PowerStatusString should return non-empty string');
end;


{ The expected text follows from the documented ACLineStatus code (same page as above) }
procedure TTestPowerUtils.TestPowerStatusString_ContainsExpectedText;
var
  SysPowerStatus: TSystemPowerStatus;
  Expected: string;
begin
  Assert.IsTrue(GetSystemPowerStatus(SysPowerStatus), 'GetSystemPowerStatus failed');
  case SysPowerStatus.ACLineStatus of
    0: Expected:= 'Running on batteries!';
    1: Expected:= 'Running on AC.';
  else Expected:= 'Power supply status unavailable.';
  end;

  Assert.AreEqual(Expected, PowerStatusString, 'PowerStatusString for ACLineStatus = ' + IntToStr(SysPowerStatus.ACLineStatus));
end;


{ BatteryLeft Tests }

procedure TTestPowerUtils.TestBatteryLeft_ReturnsValidRange;
var
  Battery: Integer;
begin
  Battery:= BatteryLeft;
  // Returns -1 if unknown, or 0-100 for percentage
  Assert.IsTrue((Battery >= -1) AND (Battery <= 100),
    'BatteryLeft should return -1 (unknown) or 0-100 (percentage)');
end;


procedure TTestPowerUtils.TestBatteryLeft_NotAbove100;
var
  Battery: Integer;
begin
  Battery:= BatteryLeft;
  Assert.IsTrue(Battery <= 100, 'BatteryLeft should never exceed 100%');
end;


{ BatteryAsText Tests }

{ The exact text, built from the documented BatteryFlag bits: 1 = High, 2 = Low, 4 = Critical, 8 = Charging,
  128 = no system battery, 255 = unknown; no bit set = 'Normal' (same page as above) }
procedure TTestPowerUtils.TestBatteryAsText_ExactText;
CONST
  BitNames: array[0..3] of string = ('High', 'Low', 'Critical', 'Charging');
var
  SysPowerStatus: TSystemPowerStatus;
  Expected: string;
  Bit: Integer;
begin
  Assert.IsTrue(GetSystemPowerStatus(SysPowerStatus), 'GetSystemPowerStatus failed');

  if SysPowerStatus.BatteryFlag = 255
  then Expected:= 'Unknown status'
  else
    if (SysPowerStatus.BatteryFlag AND 128) = 128
    then Expected:= 'No system battery'
    else
      begin
        Expected:= '';
        for Bit:= 0 to 3 DO
          if (SysPowerStatus.BatteryFlag AND (1 SHL Bit)) <> 0 then
            begin
              if Expected <> '' then Expected:= Expected + ' ';
              Expected:= Expected + BitNames[Bit];
            end;
        if Expected = '' then Expected:= 'Normal';
      end;

  Assert.AreEqual(Expected, BatteryAsText, 'BatteryAsText for BatteryFlag = ' + IntToStr(SysPowerStatus.BatteryFlag));
end;


{ BatteryFlag codes: 128 = no system battery, 255 = unknown (same page as above) }
procedure TTestPowerUtils.TestBatteryAsText_NoErrorMessage;
var
  SysPowerStatus: TSystemPowerStatus;
  Text: string;
begin
  Assert.IsTrue(GetSystemPowerStatus(SysPowerStatus), 'GetSystemPowerStatus failed');
  Text:= BatteryAsText;

  Assert.AreNotEqual('Could not get the SYSTEM POWER STATUS', Text, 'GetSystemPowerStatus works here, so no error text');
  if SysPowerStatus.BatteryFlag = 255
  then Assert.AreEqual('Unknown status', Text)
  else
    if (SysPowerStatus.BatteryFlag AND 128) = 128
    then Assert.AreEqual('No system battery', Text)
    else
      if (SysPowerStatus.BatteryFlag AND 1) = 1
      then Assert.IsTrue(Pos('High', Text) = 1, 'BatteryFlag has bit 1 (High): ' + Text)
      else Assert.IsTrue(Pos('High', Text) = 0, 'BatteryFlag lacks bit 1 (High): ' + Text);
end;


{ Power Capability Tests
  These routines are bare 'external' declarations, so the only thing that can be wrong is the export they bind to.
  The reference calls the documented export of powrprof.dll by name. }

type
  TPowrProfQuery = function: Boolean; stdcall;

function CallPowrProf(CONST ExportName: string): Boolean;
VAR
  Lib: HMODULE;
  Query: TPowrProfQuery;
begin
  Lib:= LoadLibrary('powrprof.dll');
  Assert.IsTrue(Lib <> 0, 'powrprof.dll not loaded');
  TRY
    Query:= TPowrProfQuery(GetProcAddress(Lib, PChar(ExportName)));
    Assert.IsTrue(Assigned(Query), 'powrprof.dll has no export ' + ExportName);
    Result:= Query();
  FINALLY
    FreeLibrary(Lib);
  END;
end;


procedure TTestPowerUtils.TestIsHibernateAllowed_ReturnsBool;
begin
  Assert.IsTrue(CallPowrProf('IsPwrHibernateAllowed') = IsHibernateAllowed, 'IsHibernateAllowed must bind IsPwrHibernateAllowed');
end;


procedure TTestPowerUtils.TestIsPwrSuspendAllowed_ReturnsBool;
begin
  Assert.IsTrue(CallPowrProf('IsPwrSuspendAllowed') = IsPwrSuspendAllowed, 'IsPwrSuspendAllowed must bind IsPwrSuspendAllowed');
end;


procedure TTestPowerUtils.TestIsPwrShutdownAllowed_ReturnsBool;
begin
  Assert.IsTrue(CallPowrProf('IsPwrShutdownAllowed') = IsPwrShutdownAllowed, 'IsPwrShutdownAllowed must bind IsPwrShutdownAllowed');
end;


{ IsScreenSaverOn Tests }

function FakeSaverWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result:= DefWindowProc(Wnd, Msg, wParam, lParam);
end;


{ Registers a hidden top-level window of the class a running screen saver uses, so IsScreenSaverOn must see it }
procedure TTestPowerUtils.TestIsScreenSaverOn_ReturnsBool;
CONST
  SaverClass = 'WindowsScreenSaverClass';
VAR
  WndClass: TWndClass;
  Wnd: HWND;
begin
  if FindWindow(SaverClass, NIL) <> 0 then
    begin
      Assert.Pass('A real screen saver is running - the fake one cannot be told apart');
      EXIT;
    end;

  Assert.IsFalse(IsScreenSaverOn, 'No screen saver window exists yet');

  FillChar(WndClass, SizeOf(WndClass), 0);
  WndClass.lpfnWndProc  := @FakeSaverWndProc;
  WndClass.hInstance    := HInstance;
  WndClass.lpszClassName:= SaverClass;
  Assert.IsTrue(Winapi.Windows.RegisterClass(WndClass) <> 0, 'RegisterClass failed');
  TRY
    Wnd:= CreateWindowEx(0, SaverClass, 'Fake screen saver', WS_POPUP, 0, 0, 1, 1, 0, 0, HInstance, NIL);
    Assert.IsTrue(Wnd <> 0, 'CreateWindowEx failed');
    TRY
      Assert.IsTrue(IsScreenSaverOn, 'A window of class ' + SaverClass + ' exists, so the screen saver is on');
    FINALLY
      DestroyWindow(Wnd);
    END;
  FINALLY
    Winapi.Windows.UnregisterClass(SaverClass, HInstance);
  END;

  Assert.IsFalse(IsScreenSaverOn, 'The fake screen saver window is gone');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestPowerUtils);

end.
