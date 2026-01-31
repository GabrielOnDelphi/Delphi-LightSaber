unit Test.LightFmx.Common.AppData.Form;

{=============================================================================================================
   Unit tests for LightFmx.Common.AppData.Form.pas
   Tests TLightForm - FMX form with auto-save/load capabilities.

   Note: Form creation and GUI tests are limited because FMX requires a running Application.
         These tests focus on the testable non-GUI functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightCore.AppData;

type
  [TestFixture]
  TTestLightForm = class
  private
    FOldTESTMODE: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { AutoState Tests }
    [Test]
    procedure TestAutoState_DefaultIsUndefined;

    [Test]
    procedure TestAutoState_Comparisons;

    { CloseOnEscape Property Tests }
    [Test]
    procedure TestCloseOnEscape_DefaultValue;

    { Saved Flag Tests }
    [Test]
    procedure TestSaved_InitialValue;

    { MainFormCaption Tests }
    [Test]
    procedure TestMainFormCaption_Format;

    [Test]
    procedure TestMainFormCaption_EmptyCaption;

    [Test]
    procedure TestMainFormCaption_WithCaption;
  end;

  { Tests for AutoState enum behavior }
  [TestFixture]
  TTestAutoStateBehavior = class
  public
    [Test]
    procedure TestAutoState_NoneIsLessThanPosOnly;

    [Test]
    procedure TestAutoState_PosOnlyIsLessThanFull;

    [Test]
    procedure TestAutoState_CompareWithNone;
  end;

implementation

uses
  FMX.Forms,
  LightFmx.Common.AppData,
  LightFmx.Common.AppData.Form;


{ TTestLightForm }

procedure TTestLightForm.Setup;
begin
  FOldTESTMODE:= TAppDataCore.TEST_MODE;
  TAppDataCore.TEST_MODE:= True;  // Prevent modal dialogs during tests
end;

procedure TTestLightForm.TearDown;
begin
  TAppDataCore.TEST_MODE:= FOldTESTMODE;
end;


{ AutoState Tests }

procedure TTestLightForm.TestAutoState_DefaultIsUndefined;
var
  State: TAutoState;
begin
  // Default enum value should be asUndefined (ordinal 0)
  State:= Default(TAutoState);
  Assert.AreEqual(asUndefined, State, 'Default AutoState should be asUndefined');
end;

procedure TTestLightForm.TestAutoState_Comparisons;
begin
  // Test that AutoState values can be compared correctly
  // This is important because saveBeforeExit uses "AutoState > asNone"
  Assert.IsTrue(asPosOnly > asNone, 'asPosOnly should be greater than asNone');
  Assert.IsTrue(asFull > asNone, 'asFull should be greater than asNone');
  Assert.IsTrue(asFull > asPosOnly, 'asFull should be greater than asPosOnly');
  Assert.IsFalse(asNone > asNone, 'asNone should not be greater than asNone');
end;


{ CloseOnEscape Property Tests }

procedure TTestLightForm.TestCloseOnEscape_DefaultValue;
var
  Form: TLightForm;
begin
  // Note: This test requires AppData to be initialized
  if AppData = NIL then
  begin
    Assert.Pass('AppData not available - skipping form creation test');
    EXIT;
  end;

  Form:= TLightForm.Create(NIL, asNone);
  try
    // Default value of CloseOnEscape should be False (not initialized)
    Assert.IsFalse(Form.CloseOnEscape, 'CloseOnEscape default should be False');
  finally
    Form.Free;
  end;
end;


{ Saved Flag Tests }

procedure TTestLightForm.TestSaved_InitialValue;
var
  Form: TLightForm;
begin
  if AppData = NIL then
  begin
    Assert.Pass('AppData not available - skipping form creation test');
    EXIT;
  end;

  Form:= TLightForm.Create(NIL, asNone);
  try
    Assert.IsFalse(Form.Saved, 'Saved should be False after creation');
  finally
    Form.Free;
  end;
end;


{ MainFormCaption Tests }

procedure TTestLightForm.TestMainFormCaption_Format;
begin
  // Test that AppData.AppName is available for caption formatting
  Assert.IsNotEmpty(AppData.AppName, 'AppData.AppName should not be empty');
end;

procedure TTestLightForm.TestMainFormCaption_EmptyCaption;
var
  Form: TLightForm;
  ExpectedCaption: string;
begin
  if AppData = NIL then
  begin
    Assert.Pass('AppData not available - skipping form test');
    EXIT;
  end;

  Form:= TLightForm.Create(NIL, asNone);
  try
    Form.MainFormCaption('');

    // When caption is empty, just AppName should be shown (plus debug/running home suffixes)
    Assert.IsTrue(Pos(AppData.AppName, Form.Caption) > 0,
      'Caption should contain AppName when empty caption passed');
  finally
    Form.Free;
  end;
end;

procedure TTestLightForm.TestMainFormCaption_WithCaption;
var
  Form: TLightForm;
begin
  if AppData = NIL then
  begin
    Assert.Pass('AppData not available - skipping form test');
    EXIT;
  end;

  Form:= TLightForm.Create(NIL, asNone);
  try
    Form.MainFormCaption('Test Caption');

    // Should contain both AppName and the custom caption
    Assert.IsTrue(Pos(AppData.AppName, Form.Caption) > 0,
      'Caption should contain AppName');
    Assert.IsTrue(Pos('Test Caption', Form.Caption) > 0,
      'Caption should contain custom text');
    Assert.IsTrue(Pos(' - ', Form.Caption) > 0,
      'Caption should contain separator');
  finally
    Form.Free;
  end;
end;


{ TTestAutoStateBehavior }

procedure TTestAutoStateBehavior.TestAutoState_NoneIsLessThanPosOnly;
begin
  Assert.IsTrue(asNone < asPosOnly);
end;

procedure TTestAutoStateBehavior.TestAutoState_PosOnlyIsLessThanFull;
begin
  Assert.IsTrue(asPosOnly < asFull);
end;

procedure TTestAutoStateBehavior.TestAutoState_CompareWithNone;
begin
  // This tests the logic used in saveBeforeExit: "if AutoState > asNone"
  Assert.IsFalse(asNone > asNone, 'asNone should not trigger save');
  Assert.IsTrue(asPosOnly > asNone, 'asPosOnly should trigger save');
  Assert.IsTrue(asFull > asNone, 'asFull should trigger save');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightForm);
  TDUnitX.RegisterTestFixture(TTestAutoStateBehavior);

end.
