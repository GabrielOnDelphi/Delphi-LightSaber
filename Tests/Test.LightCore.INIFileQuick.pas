unit Test.LightCore.INIFileQuick;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.INIFileQuick
   Tests quick INI file read/write functions

   IMPORTANT: These tests require AppDataCore to be initialized before running!
   The functions in LightCore.INIFileQuick depend on AppDataCore.AppName and AppDataCore.IniFile.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils;

type
  [TestFixture]
  TTestINIFileQuick = class
  private
    FTestIniPath: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Integer Tests }
    [Test]
    procedure TestWriteReadInteger;

    [Test]
    procedure TestReadInteger_Default;

    { String Tests }
    [Test]
    procedure TestWriteReadString;

    [Test]
    procedure TestReadString_Default;

    [Test]
    procedure TestWriteReadString_Empty;

    { Boolean Tests }
    [Test]
    procedure TestWriteReadBool_True;

    [Test]
    procedure TestWriteReadBool_False;

    [Test]
    procedure TestReadBoolean_Default;

    { Double Tests }
    [Test]
    procedure TestWriteReadDbl;

    [Test]
    procedure TestReadDbl_Default;

    [Test]
    procedure TestWriteReadDbl_Negative;

    { Date Tests }
    [Test]
    procedure TestWriteReadDate;

    [Test]
    procedure TestReadDate_Default;
  end;

implementation

uses
  LightCore.AppData,
  LightCore.INIFileQuick;


procedure TTestINIFileQuick.Setup;
begin
  { These tests require AppDataCore to be initialized.
    If AppDataCore is not initialized, the tests will fail. }
  if AppDataCore = NIL
  then raise Exception.Create('AppDataCore must be initialized before running these tests');
end;


procedure TTestINIFileQuick.TearDown;
begin
  { No teardown needed - INI file will be cleaned up by AppData }
end;


{ Integer Tests }

procedure TTestINIFileQuick.TestWriteReadInteger;
var
  Value: Integer;
begin
  WriteInteger('TestInt', 42);
  Value:= ReadInteger('TestInt');
  Assert.AreEqual(42, Value);
end;


procedure TTestINIFileQuick.TestReadInteger_Default;
var
  Value: Integer;
begin
  Value:= ReadInteger('NonExistentInt', 999);
  Assert.AreEqual(999, Value);
end;


{ String Tests }

procedure TTestINIFileQuick.TestWriteReadString;
var
  Value: string;
begin
  WriteString('TestStr', 'Hello World');
  Value:= ReadString('TestStr');
  Assert.AreEqual('Hello World', Value);
end;


procedure TTestINIFileQuick.TestReadString_Default;
var
  Value: string;
begin
  Value:= ReadString('NonExistentStr', 'DefaultValue');
  Assert.AreEqual('DefaultValue', Value);
end;


procedure TTestINIFileQuick.TestWriteReadString_Empty;
var
  Value: string;
begin
  WriteString('TestEmptyStr', '');
  Value:= ReadString('TestEmptyStr', 'not empty');
  Assert.AreEqual('', Value);
end;


{ Boolean Tests }

procedure TTestINIFileQuick.TestWriteReadBool_True;
var
  Value: Boolean;
begin
  WriteBool('TestBoolTrue', True);
  Value:= ReadBoolean('TestBoolTrue');
  Assert.IsTrue(Value);
end;


procedure TTestINIFileQuick.TestWriteReadBool_False;
var
  Value: Boolean;
begin
  WriteBool('TestBoolFalse', False);
  Value:= ReadBoolean('TestBoolFalse');
  Assert.IsFalse(Value);
end;


procedure TTestINIFileQuick.TestReadBoolean_Default;
var
  Value: Boolean;
begin
  Value:= ReadBoolean('NonExistentBool', True);
  Assert.IsTrue(Value);
end;


{ Double Tests }

procedure TTestINIFileQuick.TestWriteReadDbl;
var
  Value: Double;
begin
  WriteDbl('TestDbl', 3.14159);
  Value:= ReadDbl('TestDbl');
  Assert.AreEqual(Double(3.14159), Value, 0.00001);
end;


procedure TTestINIFileQuick.TestReadDbl_Default;
var
  Value: Double;
begin
  Value:= ReadDbl('NonExistentDbl', 1.5);
  Assert.AreEqual(Double(1.5), Value, 0.00001);
end;


procedure TTestINIFileQuick.TestWriteReadDbl_Negative;
var
  Value: Double;
begin
  WriteDbl('TestDblNeg', -273.15);
  Value:= ReadDbl('TestDblNeg');
  Assert.AreEqual(Double(-273.15), Value, 0.00001);
end;


{ Date Tests }

procedure TTestINIFileQuick.TestWriteReadDate;
var
  TestDate: TDateTime;
  Value: TDateTime;
begin
  TestDate:= EncodeDate(2025, 6, 15);
  WriteDate('TestDate', TestDate);
  Value:= ReadDate('TestDate', 0);
  Assert.AreEqual(TestDate, Value, 0.00001);
end;


procedure TTestINIFileQuick.TestReadDate_Default;
var
  DefaultDate: TDateTime;
  Value: TDateTime;
begin
  DefaultDate:= EncodeDate(2000, 1, 1);
  Value:= ReadDate('NonExistentDate', DefaultDate);
  Assert.AreEqual(DefaultDate, Value, 0.00001);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestINIFileQuick);

end.
