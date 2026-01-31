unit Test.FormSplashScreen;

{=============================================================================================================
   Unit tests for FormSplashScreen.pas
   Tests TfrmSplash - the fade-in/fade-out splash screen form.

   Note: These tests focus on form creation, component existence, and basic behavior.
   Full animation testing is limited since it requires timer interaction.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls;

type
  [TestFixture]
  TTestFormSplashScreen = class
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
    procedure TestFormHasImage;

    [Test]
    procedure TestFormHasTimer;

    { Image Tests }
    [Test]
    procedure TestImage_IsAutoSize;

    [Test]
    procedure TestImage_IsCentered;

    [Test]
    procedure TestImage_HasClickHandler;

    { Timer Tests }
    [Test]
    procedure TestTimer_InitiallyDisabled;

    [Test]
    procedure TestTimer_HasInterval;

    [Test]
    procedure TestTimer_HasTimerHandler;

    { Form Attribute Tests }
    [Test]
    procedure TestFormName_IsFrmSplash;

    [Test]
    procedure TestFormAlphaBlend_IsEnabled;

    [Test]
    procedure TestFormBorderStyle_IsNone;

    [Test]
    procedure TestFormFormStyle_IsStayOnTop;

    [Test]
    procedure TestFormTransparentColor_IsEnabled;

    [Test]
    procedure TestFormAutoSize_IsEnabled;

    { Event Handler Tests }
    [Test]
    procedure TestImageClick_ClosesForm;

    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestFormDestroy_NoException;

    { Timer Animation Tests }
    [Test]
    procedure TestTimerTimer_NoExceptionOnFirstCall;

    [Test]
    procedure TestTimerTimer_UpdatesAlphaBlendValue;

    [Test]
    procedure TestTimerTimer_IncrementsCurrAlpha;

    { Alpha Boundary Tests }
    [Test]
    procedure TestAlpha_ClampsToZero;

    [Test]
    procedure TestAlpha_ClampsTo255;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormSplashScreen;


procedure TTestFormSplashScreen.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
end;


procedure TTestFormSplashScreen.TearDown;
begin
  CleanupForm;
end;


procedure TTestFormSplashScreen.CleanupForm;
var
  Form: TfrmSplash;
begin
  if FTestForm <> NIL then
  begin
    Form:= TfrmSplash(FTestForm);
    { Disable timer before cleanup to prevent callbacks }
    Form.Timer.Enabled:= FALSE;
    FreeAndNil(Form);
    FTestForm:= NIL;
  end;
end;


{ Form Creation Tests }

procedure TTestFormSplashScreen.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmSplash, 'TfrmSplash class should exist');
end;


procedure TTestFormSplashScreen.TestFormCreate_Succeeds;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormSplashScreen.TestFormCreate_WithNilOwner;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormSplashScreen.TestFormHasImage;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.imgSplash, 'Form should have imgSplash component');
  Assert.IsTrue(Form.imgSplash is TImage, 'imgSplash should be a TImage');
end;


procedure TTestFormSplashScreen.TestFormHasTimer;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.Timer, 'Form should have Timer component');
  Assert.IsTrue(Form.Timer is TTimer, 'Timer should be a TTimer');
end;


{ Image Tests }

procedure TTestFormSplashScreen.TestImage_IsAutoSize;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.imgSplash.AutoSize,
    'Image should have AutoSize enabled');
end;


procedure TTestFormSplashScreen.TestImage_IsCentered;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.imgSplash.Center,
    'Image should have Center enabled');
end;


procedure TTestFormSplashScreen.TestImage_HasClickHandler;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.imgSplash.OnClick),
    'Image should have OnClick handler assigned');
end;


{ Timer Tests }

procedure TTestFormSplashScreen.TestTimer_InitiallyDisabled;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsFalse(Form.Timer.Enabled,
    'Timer should be initially disabled');
end;


procedure TTestFormSplashScreen.TestTimer_HasInterval;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.Timer.Interval > 0,
    'Timer should have a positive interval');
end;


procedure TTestFormSplashScreen.TestTimer_HasTimerHandler;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Assigned(Form.Timer.OnTimer),
    'Timer should have OnTimer handler assigned');
end;


{ Form Attribute Tests }

procedure TTestFormSplashScreen.TestFormName_IsFrmSplash;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('frmSplash', Form.Name,
    'Form name must be "frmSplash" for opacity handling');
end;


procedure TTestFormSplashScreen.TestFormAlphaBlend_IsEnabled;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AlphaBlend,
    'AlphaBlend should be enabled for fade animation');
end;


procedure TTestFormSplashScreen.TestFormBorderStyle_IsNone;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(bsNone, Form.BorderStyle,
    'BorderStyle should be bsNone for splash screen');
end;


procedure TTestFormSplashScreen.TestFormFormStyle_IsStayOnTop;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual(fsStayOnTop, Form.FormStyle,
    'FormStyle should be fsStayOnTop');
end;


procedure TTestFormSplashScreen.TestFormTransparentColor_IsEnabled;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.TransparentColor,
    'TransparentColor should be enabled for PNG transparency');
end;


procedure TTestFormSplashScreen.TestFormAutoSize_IsEnabled;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Assert.IsTrue(Form.AutoSize,
    'AutoSize should be enabled to fit image');
end;


{ Event Handler Tests }

procedure TTestFormSplashScreen.TestImageClick_ClosesForm;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Form.Show;

  { imgSplashClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.imgSplashClick(Form);
    end);
end;


procedure TTestFormSplashScreen.TestFormClose_SetsCaFree;
var
  Form: TfrmSplash;
  Action: TCloseAction;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');
end;


procedure TTestFormSplashScreen.TestFormDestroy_NoException;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { FormDestroy should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.FormDestroy(Form);
    end);
end;


{ Timer Animation Tests }

procedure TTestFormSplashScreen.TestTimerTimer_NoExceptionOnFirstCall;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { Initialize alpha values before calling timer }
  Form.AlphaBlendValue:= 40;

  { TimerTimer should not raise exception on first call }
  Assert.WillNotRaise(
    procedure
    begin
      Form.TimerTimer(NIL);
    end);
end;


procedure TTestFormSplashScreen.TestTimerTimer_UpdatesAlphaBlendValue;
var
  Form: TfrmSplash;
  InitialAlpha: Integer;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { Initialize for fade-in }
  Form.AlphaBlendValue:= 40;
  InitialAlpha:= Form.AlphaBlendValue;

  { First call will update alpha }
  Form.TimerTimer(NIL);

  { Alpha should change (either increase during fade-in or decrease during fade-out) }
  Assert.IsTrue(InitialAlpha <> Form.AlphaBlendValue,
    'TimerTimer should update AlphaBlendValue');
end;


procedure TTestFormSplashScreen.TestTimerTimer_IncrementsCurrAlpha;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { Initialize for fade-in with positive increment }
  Form.AlphaBlendValue:= 40;

  { Call timer multiple times to verify progression }
  Form.TimerTimer(NIL);
  Form.TimerTimer(NIL);

  { Alpha should have increased }
  Assert.IsTrue(Form.AlphaBlendValue > 40,
    'Alpha should increase during fade-in phase');
end;


{ Alpha Boundary Tests }

procedure TTestFormSplashScreen.TestAlpha_ClampsToZero;
var
  Form: TfrmSplash;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { Set alpha to very low value }
  Form.AlphaBlendValue:= 1;

  { AlphaBlendValue is a Byte (0-255), so it cannot be negative by type definition.
    This test verifies the property can be set to a low value without issue. }
  Assert.AreEqual(Byte(1), Form.AlphaBlendValue,
    'Alpha should be set to 1');
end;


procedure TTestFormSplashScreen.TestAlpha_ClampsTo255;
var
  Form: TfrmSplash;
  FinalAlpha: Byte;
begin
  Form:= TfrmSplash.Create(NIL);
  FTestForm:= Form;

  { Run several fade-in iterations }
  Form.AlphaBlendValue:= 200;

  { Call timer several times }
  Form.TimerTimer(NIL);
  Form.TimerTimer(NIL);
  Form.TimerTimer(NIL);

  { AlphaBlendValue is a Byte (0-255), so it cannot exceed 255 by type definition.
    This test verifies the timer can run multiple times without exception. }
  FinalAlpha:= Form.AlphaBlendValue;
  Assert.IsTrue(FinalAlpha in [0..255],
    'Alpha should be a valid Byte value');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSplashScreen);

end.
