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
    FOldUnattended: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { CloseOnEscape Property Tests }
    [Test]
    procedure TestCloseOnEscape_DefaultValue;

    { FFormSaved Flag Tests }
    [Test]
    procedure TestSaved_InitialValue;

    { MainFormCaption Tests }
    [Test]
    procedure TestMainFormCaption_ExactText;

    [Test]
    procedure TestMainFormCaption_EmptyCaption;

    [Test]
    procedure TestMainFormCaption_WithCaption;
  end;

implementation

uses
  FMX.Forms,
  LightFmx.Common.AppData,
  LightFmx.Common.AppData.Form;

type
  TLightFormCracker = class(TLightForm);   { Protected-member access for the tests (FFormSaved) }


{ TTestLightForm }

procedure TTestLightForm.Setup;
begin
  FOldUnattended:= TAppDataCore.Unattended;
  TAppDataCore.Unattended:= True;  // Prevent modal dialogs during tests
end;

procedure TTestLightForm.TearDown;
begin
  TAppDataCore.Unattended:= FOldUnattended;
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


{ FFormSaved Flag Tests }

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
    Assert.IsFalse(TLightFormCracker(Form).FFormSaved, 'FFormSaved should be False after creation');
  finally
    Form.Free;
  end;
end;


{ MainFormCaption Tests }

procedure TTestLightForm.TestMainFormCaption_ExactText;
var
  Form: TLightForm;
  Expected: string;
begin
  if AppData = NIL then
  begin
    Assert.Pass('AppData not available - skipping form test');
    EXIT;
  end;

  { "AppName - caption", then one tag per active mode }
  Expected:= AppData.AppName + ' - Exact';
  if AppData.RunningHome    then Expected:= Expected + ' [Running home]';
  if AppData.BetaTesterMode then Expected:= Expected + ' [BetaTesterMode]';
  {$IFDEF DEBUG}
  Expected:= Expected + ' [Debug]';
  {$ENDIF}

  Form:= TLightForm.Create(NIL, asNone);
  try
    Form.MainFormCaption('Exact');
    Assert.AreEqual(Expected, Form.Caption, 'MainFormCaption must build the exact caption');
  finally
    FreeAndNil(Form);
  end;
end;

procedure TTestLightForm.TestMainFormCaption_EmptyCaption;
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


initialization
  TDUnitX.RegisterTestFixture(TTestLightForm);

end.
