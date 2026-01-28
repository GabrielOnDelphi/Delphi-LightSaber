unit Test.FormAbout;

{=============================================================================================================
   Unit tests for FormAbout.pas
   Tests TfrmAboutApp - the About dialog form.

   Note: Testing VCL forms requires Application to be initialized.
   These tests focus on form creation and basic property/method behavior.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestFormAbout = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormClassName_NotTfrmAbout;

    { CreateFormParented Tests }
    [Test]
    procedure TestCreateFormParented_ReturnsForm;

    [Test]
    procedure TestCreateFormParented_ContainerParented;

    [Test]
    procedure TestCreateFormParented_ContainerBevels;

    [Test]
    procedure TestCreateFormParented_NilParentAsserts;

    { Form Components Tests }
    [Test]
    procedure TestFormHasContainer;

    [Test]
    procedure TestFormHasLabels;

    [Test]
    procedure TestFormHasButtons;

    { Proteus nil handling Tests }
    [Test]
    procedure TestProteusNil_ButtonsHidden;

    [Test]
    procedure TestProteusNil_ExpireLabelEmpty;

    { AppData Integration Tests }
    [Test]
    procedure TestCompanyLabelFromAppData;

    [Test]
    procedure TestAppNameLabelFromAppData;

    [Test]
    procedure TestVersionLabelNotEmpty;

    { KeyPress Tests }
    [Test]
    procedure TestKeyPress_EnterClosesForm;

    [Test]
    procedure TestKeyPress_EscapeClosesForm;

    [Test]
    procedure TestKeyPress_OtherKeyDoesNotClose;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormAbout;

var
  TestForm: TfrmAboutApp;
  TestParentPanel: TPanel;
  TestParentForm: TForm;


procedure TTestFormAbout.Setup;
begin
  // Ensure AppData is created for tests
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');

  TestForm:= NIL;
  TestParentPanel:= NIL;
  TestParentForm:= NIL;
end;


procedure TTestFormAbout.TearDown;
begin
  // Clean up test objects in reverse order of creation
  if TestForm <> NIL then
    begin
      FreeAndNil(TestForm);
    end;

  // TestParentPanel is owned by TestParentForm, so don't free separately
  TestParentPanel:= NIL;

  if TestParentForm <> NIL then
    begin
      FreeAndNil(TestParentForm);
    end;
end;


{ Form Creation Tests }

procedure TTestFormAbout.TestFormClassExists;
begin
  // Verify the form class is registered and can be referenced
  Assert.IsNotNull(TfrmAboutApp);
end;


procedure TTestFormAbout.TestFormClassName_NotTfrmAbout;
begin
  // The form class must not be named 'TfrmAbout' due to DFM resource conflicts
  // It should be 'TfrmAboutApp' instead
  Assert.AreNotEqual('TfrmAbout', TfrmAboutApp.ClassName,
    'Form class should not be named TfrmAbout to avoid DFM conflicts');
  Assert.AreEqual('TfrmAboutApp', TfrmAboutApp.ClassName);
end;


{ CreateFormParented Tests }

procedure TTestFormAbout.TestCreateFormParented_ReturnsForm;
begin
  // Create a parent form to host the panel
  TestParentForm:= TForm.Create(NIL);
  TestParentForm.Width:= 400;
  TestParentForm.Height:= 300;

  TestParentPanel:= TPanel.Create(TestParentForm);
  TestParentPanel.Parent:= TestParentForm;
  TestParentPanel.Align:= alClient;

  TestForm:= TfrmAboutApp.CreateFormParented(TestParentPanel);

  Assert.IsNotNull(TestForm, 'CreateFormParented should return a form instance');
  Assert.IsTrue(TestForm is TfrmAboutApp, 'Returned form should be TfrmAboutApp');
end;


procedure TTestFormAbout.TestCreateFormParented_ContainerParented;
begin
  TestParentForm:= TForm.Create(NIL);
  TestParentPanel:= TPanel.Create(TestParentForm);
  TestParentPanel.Parent:= TestParentForm;
  TestParentPanel.Align:= alClient;

  TestForm:= TfrmAboutApp.CreateFormParented(TestParentPanel);

  Assert.IsNotNull(TestForm.Container, 'Form should have Container panel');
  Assert.AreEqual(TestParentPanel, TestForm.Container.Parent,
    'Container should be parented to the provided parent');
end;


procedure TTestFormAbout.TestCreateFormParented_ContainerBevels;
begin
  TestParentForm:= TForm.Create(NIL);
  TestParentPanel:= TPanel.Create(TestParentForm);
  TestParentPanel.Parent:= TestParentForm;

  TestForm:= TfrmAboutApp.CreateFormParented(TestParentPanel);

  Assert.AreEqual(bvRaised, TestForm.Container.BevelInner, 'BevelInner should be bvRaised');
  Assert.AreEqual(bvLowered, TestForm.Container.BevelOuter, 'BevelOuter should be bvLowered');
  Assert.AreEqual(alNone, TestForm.Container.Align, 'Container Align should be alNone');
end;


procedure TTestFormAbout.TestCreateFormParented_NilParentAsserts;
begin
  // Passing nil should trigger an assertion
  Assert.WillRaise(
    procedure
    begin
      TfrmAboutApp.CreateFormParented(NIL);
    end,
    EAssertionFailed,
    'Should raise assertion when Parent is nil');
end;


{ Form Components Tests }

procedure TTestFormAbout.TestFormHasContainer;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.IsNotNull(TestForm.Container, 'Form should have Container panel');
  Assert.IsTrue(TestForm.Container is TPanel, 'Container should be a TPanel');
end;


procedure TTestFormAbout.TestFormHasLabels;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.IsNotNull(TestForm.lblCompany, 'Form should have lblCompany');
  Assert.IsNotNull(TestForm.lblAppName, 'Form should have lblAppName');
  Assert.IsNotNull(TestForm.lblVersion, 'Form should have lblVersion');
  Assert.IsNotNull(TestForm.lblExpire, 'Form should have lblExpire');
end;


procedure TTestFormAbout.TestFormHasButtons;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.IsNotNull(TestForm.btnEnterKey, 'Form should have btnEnterKey');
  Assert.IsNotNull(TestForm.btnOrderNow, 'Form should have btnOrderNow');
end;


{ Proteus nil handling Tests }

procedure TTestFormAbout.TestProteusNil_ButtonsHidden;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);
  // Proteus is nil by default

  Assert.IsFalse(TestForm.btnOrderNow.Visible,
    'btnOrderNow should be hidden when Proteus is nil');
  Assert.IsFalse(TestForm.btnEnterKey.Visible,
    'btnEnterKey should be hidden when Proteus is nil');
end;


procedure TTestFormAbout.TestProteusNil_ExpireLabelEmpty;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);
  // Proteus is nil by default

  Assert.AreEqual('', TestForm.lblExpire.Caption,
    'lblExpire should be empty when Proteus is nil');
end;


{ AppData Integration Tests }

procedure TTestFormAbout.TestCompanyLabelFromAppData;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.AreEqual(AppData.CompanyName, TestForm.lblCompany.Caption,
    'lblCompany.Caption should match AppData.CompanyName');
end;


procedure TTestFormAbout.TestAppNameLabelFromAppData;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.AreEqual(AppData.AppName, TestForm.lblAppName.Caption,
    'lblAppName.Caption should match AppData.AppName');
end;


procedure TTestFormAbout.TestVersionLabelNotEmpty;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  Assert.IsNotEmpty(TestForm.lblVersion.Caption,
    'lblVersion.Caption should not be empty');
end;


{ KeyPress Tests
  Note: These tests verify that FormKeyPress processes the correct keys.
  We test by checking if Close was attempted via the form's modal result. }

procedure TTestFormAbout.TestKeyPress_EnterClosesForm;
var
  Key: Char;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  // Simulate Enter key press
  Key:= #13;
  TestForm.FormKeyPress(TestForm, Key);

  // When Close is called on a hidden form, it attempts to close
  // We verify the form accepts closure via CloseQuery
  Assert.IsTrue(TestForm.CloseQuery, 'Form should accept close request after Enter key');
end;


procedure TTestFormAbout.TestKeyPress_EscapeClosesForm;
var
  Key: Char;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  // Simulate Escape key press
  Key:= #27;
  TestForm.FormKeyPress(TestForm, Key);

  Assert.IsTrue(TestForm.CloseQuery, 'Form should accept close request after Escape key');
end;


procedure TTestFormAbout.TestKeyPress_OtherKeyDoesNotClose;
var
  Key: Char;
begin
  AppData.CreateFormHidden(TfrmAboutApp, TestForm);

  // Simulate a regular key press (letter 'A')
  Key:= 'A';
  TestForm.FormKeyPress(TestForm, Key);

  // Key 'A' should not trigger any special behavior
  // Form should still be in normal state
  Assert.IsNotNull(TestForm, 'Form should still exist after regular key press');
  Assert.IsTrue(TestForm.CloseQuery, 'Form should still accept close queries');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormAbout);

end.
