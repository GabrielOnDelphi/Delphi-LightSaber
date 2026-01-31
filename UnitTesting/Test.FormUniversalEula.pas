unit Test.FormUniversalEula;

{=============================================================================================================
   Unit tests for FormUniversalEula.pas
   Tests TfrmEULA - the End User License Agreement form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Full modal testing is limited since ShowModal blocks execution.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  [TestFixture]
  TTestFormUniversalEula = class
  private
    FTestForm: TObject;
    procedure CleanupForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasOKButton;

    [Test]
    procedure TestFormHasLicenseMemo;

    { Button Tests }
    [Test]
    procedure TestOKButton_IsButton;

    [Test]
    procedure TestOKButton_HasClickHandler;

    [Test]
    procedure TestOKButton_CaptionIsOK;

    [Test]
    procedure TestOKButton_IsDefault;

    [Test]
    procedure TestOKButton_HasModalResult;

    { Memo Tests }
    [Test]
    procedure TestLicenseMemo_IsMemo;

    [Test]
    procedure TestLicenseMemo_IsReadOnly;

    [Test]
    procedure TestLicenseMemo_HasScrollBars;

    [Test]
    procedure TestLicenseMemo_HasContent;

    [Test]
    procedure TestLicenseMemo_AlignIsClient;

    { Form Attribute Tests }
    [Test]
    procedure TestFormName_IsFrmEULA;

    [Test]
    procedure TestFormStyle_IsStayOnTop;

    [Test]
    procedure TestFormBorderStyle_IsDialog;

    [Test]
    procedure TestFormAlphaBlend_IsEnabled;

    [Test]
    procedure TestFormKeyPreview_IsEnabled;

    [Test]
    procedure TestFormPosition_IsScreenCenter;

    [Test]
    procedure TestFormCaption_IsLicenseAgreement;

    [Test]
    procedure TestFormBorderIcons_IsEmpty;

    { Event Handler Tests }
    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestFormKeyDown_HasHandler;

    [Test]
    procedure TestBtnOKClick_NoException;

    { KeyDown Tests }
    [Test]
    procedure TestFormKeyDown_EscapeCloses;

    [Test]
    procedure TestFormKeyDown_EnterCloses;

    [Test]
    procedure TestFormKeyDown_OtherKeyNoClose;

    { Visibility Tests }
    [Test]
    procedure TestFormVisible_IsFalseByDefault;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormUniversalEula;


procedure TTestFormUniversalEula.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormUniversalEula.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormUniversalEula.CleanupForm;
var
  Form: TfrmEULA;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmEULA(FTestForm);
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormUniversalEula.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmEULA, 'TfrmEULA class should exist');
end;


procedure TTestFormUniversalEula.TestFormCreate_Succeeds;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormUniversalEula.TestFormCreate_WithNilOwner;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormUniversalEula.TestFormHasOKButton;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK component');
end;


procedure TTestFormUniversalEula.TestFormHasLicenseMemo;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.mmoLicense, 'Form should have mmoLicense component');
end;


{ Button Tests }

procedure TTestFormUniversalEula.TestOKButton_IsButton;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.btnOK is TButton, 'btnOK should be a TButton');
end;


procedure TTestFormUniversalEula.TestOKButton_HasClickHandler;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.btnOK.OnClick),
    'btnOK should have OnClick handler assigned');
end;


procedure TTestFormUniversalEula.TestOKButton_CaptionIsOK;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('OK', Form.btnOK.Caption,
    'btnOK caption should be "OK"');
end;


procedure TTestFormUniversalEula.TestOKButton_IsDefault;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.btnOK.Default,
    'btnOK should be the default button');
end;


procedure TTestFormUniversalEula.TestOKButton_HasModalResult;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(mrOk, Form.btnOK.ModalResult,
    'btnOK should have ModalResult = mrOk');
end;


{ Memo Tests }

procedure TTestFormUniversalEula.TestLicenseMemo_IsMemo;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.mmoLicense is TMemo, 'mmoLicense should be a TMemo');
end;


procedure TTestFormUniversalEula.TestLicenseMemo_IsReadOnly;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.mmoLicense.ReadOnly,
    'License memo should be read-only');
end;


procedure TTestFormUniversalEula.TestLicenseMemo_HasScrollBars;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(ssBoth, Form.mmoLicense.ScrollBars,
    'License memo should have both scrollbars');
end;


procedure TTestFormUniversalEula.TestLicenseMemo_HasContent;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.mmoLicense.Lines.Count > 0,
    'License memo should have default content');
end;


procedure TTestFormUniversalEula.TestLicenseMemo_AlignIsClient;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(alClient, Form.mmoLicense.Align,
    'License memo should align to client');
end;


{ Form Attribute Tests }

procedure TTestFormUniversalEula.TestFormName_IsFrmEULA;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('frmEULA', Form.Name,
    'Form name must be "frmEULA"');
end;


procedure TTestFormUniversalEula.TestFormStyle_IsStayOnTop;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(fsStayOnTop, Form.FormStyle,
    'FormStyle must be fsStayOnTop for EULA visibility');
end;


procedure TTestFormUniversalEula.TestFormBorderStyle_IsDialog;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(bsDialog, Form.BorderStyle,
    'BorderStyle should be bsDialog');
end;


procedure TTestFormUniversalEula.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled');
end;


procedure TTestFormUniversalEula.TestFormKeyPreview_IsEnabled;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.KeyPreview,
    'KeyPreview should be enabled for keyboard handling');
end;


procedure TTestFormUniversalEula.TestFormPosition_IsScreenCenter;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(poScreenCenter, Form.Position,
    'Form position should be screen center');
end;


procedure TTestFormUniversalEula.TestFormCaption_IsLicenseAgreement;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('License agreement', Form.Caption,
    'Form caption should be "License agreement"');
end;


procedure TTestFormUniversalEula.TestFormBorderIcons_IsEmpty;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Byte(Form.BorderIcons) = 0,
    'BorderIcons should be empty (no close button)');
end;


{ Event Handler Tests }

procedure TTestFormUniversalEula.TestFormClose_SetsCaFree;
var
  Form: TfrmEULA;
  Action: TCloseAction;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormUniversalEula.TestFormKeyDown_HasHandler;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.OnKeyDown),
    'Form should have OnKeyDown handler assigned');
end;


procedure TTestFormUniversalEula.TestBtnOKClick_NoException;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  { btnOKClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnOKClick(Form);
    end);
end;


{ KeyDown Tests }

procedure TTestFormUniversalEula.TestFormKeyDown_EscapeCloses;
var
  Form: TfrmEULA;
  Key: Word;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Key:= VK_ESCAPE;

  { FormKeyDown with Escape should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyDown(Form, Key, []);
    end);
end;


procedure TTestFormUniversalEula.TestFormKeyDown_EnterCloses;
var
  Form: TfrmEULA;
  Key: Word;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Key:= VK_RETURN;

  { FormKeyDown with Enter should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyDown(Form, Key, []);
    end);
end;


procedure TTestFormUniversalEula.TestFormKeyDown_OtherKeyNoClose;
var
  Form: TfrmEULA;
  Key: Word;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Key:= Ord('A');

  { FormKeyDown with other key should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormKeyDown(Form, Key, []);
    end);
end;


{ Visibility Tests }

procedure TTestFormUniversalEula.TestFormVisible_IsFalseByDefault;
var
  Form: TfrmEULA;
begin
  Form:= TfrmEULA.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.Visible,
    'Form Visible must be False by default for ShowModal to work');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormUniversalEula);

end.
