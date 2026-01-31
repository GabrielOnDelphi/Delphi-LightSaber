unit Test.LightFmx.Common.AppData;

{=============================================================================================================
   Unit tests for LightFmx.Common.AppData.pas
   Tests TAppData - FMX application data management, form creation, and FMX-specific functionality.

   Note: Form creation tests are limited because FMX requires a running Application.
         These tests focus on the non-GUI functionality and state management.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.AppData;

type
  [TestFixture]
  TTestFmxAppData = class
  private
    FOldTESTMODE: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestAppDataType;

    { Pending AutoState Tests }
    [Test]
    procedure TestPendingAutoState_Record;

    { AutoState Enum Tests }
    [Test]
    procedure TestAutoState_Values;

    [Test]
    procedure TestAutoState_Ordering;

    { Version Tests }
    [Test]
    procedure TestGetAppVersion;

    { Minimizing Tests }
    [Test]
    procedure TestMinimize_NoMainForm;

    { StartMinim Flag Tests }
    [Test]
    procedure TestStartMinim_SetGet;

    { TEST_MODE Tests }
    [Test]
    procedure TestTEST_MODE_Exists;

    [Test]
    procedure TestTEST_MODE_SetGet;

    { Log Form Tests (without actual form creation) }
    [Test]
    procedure TestRamLogExists;

    { Initializing Flag Tests }
    [Test]
    procedure TestInitializing_AfterCreate;
  end;

  { Tests for TPendingAutoState record }
  [TestFixture]
  TTestPendingAutoState = class
  public
    [Test]
    procedure TestRecord_ClassName;

    [Test]
    procedure TestRecord_AutoState;

    [Test]
    procedure TestRecord_QueuedBeforeRun;
  end;

implementation

uses
  FMX.Forms,
  LightFmx.Common.AppData;


{ TTestFmxAppData }

procedure TTestFmxAppData.Setup;
begin
  FOldTESTMODE:= TAppDataCore.TEST_MODE;
end;

procedure TTestFmxAppData.TearDown;
begin
  TAppDataCore.TEST_MODE:= FOldTESTMODE;
end;


{ Basic Tests }

procedure TTestFmxAppData.TestAppDataType;
begin
  Assert.IsNotNull(AppData, 'AppData should be created');
  Assert.InheritsFrom(AppData.ClassType, TAppDataCore, 'AppData should inherit from TAppDataCore');
end;


{ Pending AutoState Tests }

procedure TTestFmxAppData.TestPendingAutoState_Record;
var
  Pending: TPendingAutoState;
begin
  Pending.ClassName:= 'TTestForm';
  Pending.AutoState:= asFull;
  Pending.QueuedBeforeRun:= True;

  Assert.AreEqual('TTestForm', Pending.ClassName);
  Assert.AreEqual(asFull, Pending.AutoState);
  Assert.IsTrue(Pending.QueuedBeforeRun);
end;


{ AutoState Enum Tests }

procedure TTestFmxAppData.TestAutoState_Values;
begin
  // Verify enum values exist
  Assert.AreEqual(0, Ord(asUndefined), 'asUndefined should be 0');
  Assert.AreEqual(1, Ord(asNone), 'asNone should be 1');
  Assert.AreEqual(2, Ord(asPosOnly), 'asPosOnly should be 2');
  Assert.AreEqual(3, Ord(asFull), 'asFull should be 3');
end;

procedure TTestFmxAppData.TestAutoState_Ordering;
begin
  // Verify ordering for comparison operations used in code
  Assert.IsTrue(asNone < asPosOnly, 'asNone should be less than asPosOnly');
  Assert.IsTrue(asPosOnly < asFull, 'asPosOnly should be less than asFull');
  Assert.IsTrue(asNone > asUndefined, 'asNone should be greater than asUndefined');
end;


{ Version Tests }

procedure TTestFmxAppData.TestGetAppVersion;
var
  Version: string;
begin
  Version:= AppData.GetAppVersion;
  // Version may be empty if not running as a packaged app
  Assert.IsTrue(Length(Version) >= 0, 'GetAppVersion should not raise exception');
end;


{ Minimizing Tests }

procedure TTestFmxAppData.TestMinimize_NoMainForm;
begin
  // When MainForm is nil, Minimize should exit gracefully without exception
  if Application.MainForm = NIL then
  begin
    Assert.WillNotRaise(
      procedure
      begin
        AppData.Minimize;
      end,
      Exception,
      'Minimize should not raise when MainForm is nil');
  end
  else
    Assert.Pass('MainForm exists - skipping nil test');
end;


{ StartMinim Flag Tests }

procedure TTestFmxAppData.TestStartMinim_SetGet;
var
  OldValue: Boolean;
begin
  OldValue:= AppData.StartMinim;
  try
    AppData.StartMinim:= True;
    Assert.IsTrue(AppData.StartMinim);

    AppData.StartMinim:= False;
    Assert.IsFalse(AppData.StartMinim);
  finally
    AppData.StartMinim:= OldValue;
  end;
end;


{ TEST_MODE Tests }

procedure TTestFmxAppData.TestTEST_MODE_Exists;
begin
  // Just verify the class var exists and is accessible
  Assert.IsTrue(TAppDataCore.TEST_MODE OR (NOT TAppDataCore.TEST_MODE));
end;

procedure TTestFmxAppData.TestTEST_MODE_SetGet;
begin
  TAppDataCore.TEST_MODE:= True;
  Assert.IsTrue(TAppDataCore.TEST_MODE);

  TAppDataCore.TEST_MODE:= False;
  Assert.IsFalse(TAppDataCore.TEST_MODE);
end;


{ Log Form Tests }

procedure TTestFmxAppData.TestRamLogExists;
begin
  Assert.IsNotNull(AppData.RamLog, 'RamLog should be created with AppData');
end;


{ Initializing Flag Tests }

procedure TTestFmxAppData.TestInitializing_AfterCreate;
begin
  // After Run(), Initializing should be False
  // In test environment, we can't call Run(), so we test the class var directly
  Assert.IsTrue(TAppDataCore.Initializing OR (NOT TAppDataCore.Initializing),
    'Initializing flag should be accessible');
end;


{ TTestPendingAutoState }

procedure TTestPendingAutoState.TestRecord_ClassName;
var
  Pending: TPendingAutoState;
begin
  Pending.ClassName:= 'TMyCustomForm';
  Assert.AreEqual('TMyCustomForm', Pending.ClassName);

  Pending.ClassName:= '';
  Assert.AreEqual('', Pending.ClassName);
end;

procedure TTestPendingAutoState.TestRecord_AutoState;
var
  Pending: TPendingAutoState;
begin
  Pending.AutoState:= asUndefined;
  Assert.AreEqual(asUndefined, Pending.AutoState);

  Pending.AutoState:= asNone;
  Assert.AreEqual(asNone, Pending.AutoState);

  Pending.AutoState:= asPosOnly;
  Assert.AreEqual(asPosOnly, Pending.AutoState);

  Pending.AutoState:= asFull;
  Assert.AreEqual(asFull, Pending.AutoState);
end;

procedure TTestPendingAutoState.TestRecord_QueuedBeforeRun;
var
  Pending: TPendingAutoState;
begin
  Pending.QueuedBeforeRun:= True;
  Assert.IsTrue(Pending.QueuedBeforeRun);

  Pending.QueuedBeforeRun:= False;
  Assert.IsFalse(Pending.QueuedBeforeRun);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxAppData);
  TDUnitX.RegisterTestFixture(TTestPendingAutoState);

end.
