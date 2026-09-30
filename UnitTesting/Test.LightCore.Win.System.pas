unit Test.LightCore.Win.System;

{=============================================================================================================
   Unit tests for LightCore.Win.System.pas
   Tests system-level Windows API utilities

   Note: Some functions (ServiceStart/Stop) require elevated privileges
   and are not tested here to avoid side effects. Only read-only safe functions are tested.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightCore.Win.System;

type
  [TestFixture]
  TTestSystem = class
  public
    { Computer info tests }
    [Test]
    procedure TestGetUserNameEx_SamCompatible;

    { Service status tests - read-only, safe to run }
    [Test]
    procedure TestServiceGetStatus_NonExistent;

    [Test]
    procedure TestServiceGetStatusName_NonExistent;

    { Error string tests }
    [Test]
    procedure TestGetWin32ErrorString_Success;

    [Test]
    procedure TestGetWin32ErrorString_FileNotFound;

    [Test]
    procedure TestGetWin32ErrorString_AccessDenied;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


{ TTestSystem }

procedure TTestSystem.TestGetUserNameEx_SamCompatible;
var
  Name: string;
begin
  { NameSamCompatible = 2, returns DOMAIN\Username format }
  Name:= GetUserNameEx(2);
  Assert.IsNotEmpty(Name, 'UserNameEx should not be empty');
  Assert.IsTrue(Pos('\', Name) > 0, 'SAM compatible name should contain backslash');
end;


procedure TTestSystem.TestServiceGetStatus_NonExistent;
var
  Status: DWord;
begin
  { Non-existent service should return 0 }
  Status:= ServiceGetStatus('', 'NonExistentService12345');
  Assert.AreEqual(DWord(0), Status);
end;


procedure TTestSystem.TestServiceGetStatusName_NonExistent;
var
  StatusName: string;
begin
  { Non-existent service should return 'UNKNOWN STATE' }
  StatusName:= ServiceGetStatusName('', 'NonExistentService12345');
  Assert.AreEqual('UNKNOWN STATE', StatusName);
end;


procedure TTestSystem.TestGetWin32ErrorString_Success;
var
  Msg: string;
begin
  Msg:= GetWin32ErrorString(ERROR_SUCCESS);
  Assert.AreEqual('Operation completed successfully.', Msg);
end;


procedure TTestSystem.TestGetWin32ErrorString_FileNotFound;
var
  Msg: string;
begin
  Msg:= GetWin32ErrorString(ERROR_FILE_NOT_FOUND);
  Assert.IsNotEmpty(Msg, 'Error message should not be empty');
  { The exact message depends on Windows localization }
end;


procedure TTestSystem.TestGetWin32ErrorString_AccessDenied;
var
  Msg: string;
begin
  Msg:= GetWin32ErrorString(ERROR_ACCESS_DENIED);
  Assert.IsNotEmpty(Msg, 'Error message should not be empty');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystem);
{$ENDIF}

end.
