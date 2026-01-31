unit Test.LightVcl.Common.System;

{=============================================================================================================
   Unit tests for LightVcl.Common.System.pas
   Tests system-level Windows API utilities

   Note: Some functions (ServiceStart/Stop, InstallFont) require elevated privileges
   and are not tested here to avoid side effects. Only read-only safe functions are tested.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Winapi.Windows,
  LightVcl.Common.System;

type
  [TestFixture]
  TTestSystem = class
  public
    { Computer info tests }
    [Test]
    procedure TestGetComputerName;

    [Test]
    procedure TestGetHostName;

    [Test]
    procedure TestGetLogonName;

    [Test]
    procedure TestGetDomainName;

    [Test]
    procedure TestGetUserName;

    [Test]
    procedure TestGetUserNameEx_SamCompatible;

    { Display tests }
    [Test]
    procedure TestGetDisplayModes;

    { BIOS tests }
    [Test]
    procedure TestBiosDate;

    [Test]
    procedure TestBiosID;

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

  [TestFixture]
  TTestFontFunctions = class
  public
    { UseUninstalledFont parameter validation }
    [Test]
    procedure TestUseUninstalledFont_EmptyParam;

    [Test]
    procedure TestUseUninstalledFont_FileNotFound;

    [Test]
    procedure TestFreeUninstalledFont_EmptyParam;

    [Test]
    procedure TestFreeUninstalledFont_FileNotFound;

    { InstallFont parameter validation }
    [Test]
    procedure TestInstallFont_EmptyParam;

    [Test]
    procedure TestInstallFont_FileNotFound;
  end;

implementation


{ TTestSystem }

procedure TTestSystem.TestGetComputerName;
var
  Name: string;
begin
  Name:= GetComputerName;
  Assert.IsNotEmpty(Name, 'Computer name should not be empty');
  Assert.IsTrue(Length(Name) <= MAX_COMPUTERNAME_LENGTH, 'Computer name too long');
end;


procedure TTestSystem.TestGetHostName;
var
  Name: string;
begin
  Name:= GetHostName;
  { Host name may be empty if WinSock initialization fails, but shouldn't raise }
  Assert.IsTrue(Length(Name) <= 100, 'Host name too long');
end;


procedure TTestSystem.TestGetLogonName;
var
  Name: string;
begin
  Name:= GetLogonName;
  Assert.IsNotEmpty(Name, 'Logon name should not be empty');
  Assert.AreEqual(UpperCase(Name), Name, 'Logon name should be uppercase');
end;


procedure TTestSystem.TestGetDomainName;
var
  Name: string;
begin
  Name:= GetDomainName;
  Assert.IsNotEmpty(Name, 'Domain name should not be empty');
end;


procedure TTestSystem.TestGetUserName;
var
  Name: string;
begin
  Name:= LightVcl.Common.System.GetUserName(False);
  Assert.IsNotEmpty(Name, 'User name should not be empty');
end;


procedure TTestSystem.TestGetUserNameEx_SamCompatible;
var
  Name: string;
begin
  { NameSamCompatible = 2, returns DOMAIN\Username format }
  Name:= GetUserNameEx(2);
  Assert.IsNotEmpty(Name, 'UserNameEx should not be empty');
  Assert.IsTrue(Pos('\', Name) > 0, 'SAM compatible name should contain backslash');
end;


procedure TTestSystem.TestGetDisplayModes;
var
  Modes: string;
begin
  Modes:= GetDisplayModes;
  Assert.IsNotEmpty(Modes, 'Display modes should not be empty');
  Assert.IsTrue(Pos('x', Modes) > 0, 'Display modes should contain resolution format');
end;


procedure TTestSystem.TestBiosDate;
var
  DateStr: string;
begin
  DateStr:= BiosDate;
  Assert.IsNotEmpty(DateStr, 'BIOS date should not be empty');
  { BIOS date is typically in format MM/DD/YY or similar }
end;


procedure TTestSystem.TestBiosID;
var
  ID: string;
begin
  ID:= BiosID;
  Assert.IsNotEmpty(ID, 'BIOS ID should not be empty');
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


{ TTestFontFunctions }

procedure TTestFontFunctions.TestUseUninstalledFont_EmptyParam;
begin
  Assert.WillRaise(
    procedure
    begin
      UseUninstalledFont('');
    end,
    Exception);
end;


procedure TTestFontFunctions.TestUseUninstalledFont_FileNotFound;
begin
  Assert.WillRaise(
    procedure
    begin
      UseUninstalledFont('C:\NonExistent\Font.ttf');
    end,
    Exception);
end;


procedure TTestFontFunctions.TestFreeUninstalledFont_EmptyParam;
begin
  Assert.WillRaise(
    procedure
    begin
      FreeUninstalledFont('');
    end,
    Exception);
end;


procedure TTestFontFunctions.TestFreeUninstalledFont_FileNotFound;
begin
  Assert.WillRaise(
    procedure
    begin
      FreeUninstalledFont('C:\NonExistent\Font.ttf');
    end,
    Exception);
end;


procedure TTestFontFunctions.TestInstallFont_EmptyParam;
begin
  Assert.WillRaise(
    procedure
    begin
      InstallFont('');
    end,
    Exception);
end;


procedure TTestFontFunctions.TestInstallFont_FileNotFound;
begin
  Assert.WillRaise(
    procedure
    begin
      InstallFont('C:\NonExistent\Font.ttf');
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSystem);
  TDUnitX.RegisterTestFixture(TTestFontFunctions);

end.
