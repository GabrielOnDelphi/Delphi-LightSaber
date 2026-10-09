unit Test.LightFmx.Common.Helpers;

{=============================================================================================================
   Unit tests for LightFmx.Common.Helpers.pas
   Tests form traversal and clipboard helper functions.

   Note: FMX tests require platform initialization. Some tests may be skipped on non-GUI environments.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.Platform;

type
  [TestFixture]
  TTestLightFmxCommonHelpers = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { FindImmediateParentForm Tests }
    [Test]
    procedure TestFindImmediateParentForm_NilInput;

    [Test]
    procedure TestFindImmediateParentForm_FormReturnsItself;

    { GetParentForm Tests }
    [Test]
    procedure TestGetParentForm_NilControlAssertion;

    [Test]
    procedure TestGetParentForm_ControlWithNoParent;

    [Test]
    procedure TestGetParentForm_ControlOnForm;

    [Test]
    procedure TestGetParentForm_TopFormTrue;

    [Test]
    procedure TestGetParentForm_TopFormFalse;

    { CopyToClipboard Tests }
    [Test]
    procedure TestCopyToClipboard_BasicText;

    [Test]
    procedure TestCopyToClipboard_EmptyString;
  private
    FForm: TForm;
    FChildForm: TForm;
    FButton: TButton;
  end;

implementation

uses
  System.Rtti,
  LightFmx.Common.Helpers;


procedure TTestLightFmxCommonHelpers.Setup;
begin
  FForm:= nil;
  FChildForm:= nil;
  FButton:= nil;
end;


procedure TTestLightFmxCommonHelpers.TearDown;
begin
  // Clean up in reverse order of creation
  if Assigned(FButton) then
  begin
    FButton.Parent:= nil;
    FreeAndNil(FButton);
  end;

  if Assigned(FChildForm) then
  begin
    FChildForm.Parent:= nil;
    FreeAndNil(FChildForm);
  end;

  FreeAndNil(FForm);
end;


{ FindImmediateParentForm Tests }

procedure TTestLightFmxCommonHelpers.TestFindImmediateParentForm_NilInput;
begin
  // Passing nil should return nil without crashing
  Assert.IsNull(FindImmediateParentForm(nil));
end;


procedure TTestLightFmxCommonHelpers.TestFindImmediateParentForm_FormReturnsItself;
begin
  FForm:= TForm.CreateNew(nil);
  // A form passed to FindImmediateParentForm should return itself
  Assert.AreSame(FForm, FindImmediateParentForm(FForm));
end;


{ GetParentForm Tests }

procedure TTestLightFmxCommonHelpers.TestGetParentForm_NilControlAssertion;
begin
  // GetParentForm should raise assertion when Control is nil
  Assert.WillRaise(
    procedure
    begin
      GetParentForm(nil);
    end,
    EAssertionFailed);
end;


procedure TTestLightFmxCommonHelpers.TestGetParentForm_ControlWithNoParent;
begin
  FButton:= TButton.Create(nil);
  // A control with no parent should return nil
  Assert.IsNull(GetParentForm(FButton));
end;


procedure TTestLightFmxCommonHelpers.TestGetParentForm_ControlOnForm;
begin
  FForm:= TForm.CreateNew(nil);
  FButton:= TButton.Create(FForm);
  FButton.Parent:= FForm;

  // Control on a form should return that form
  Assert.AreSame(FForm, GetParentForm(FButton));
end;


procedure TTestLightFmxCommonHelpers.TestGetParentForm_TopFormTrue;
begin
  // Create parent form
  FForm:= TForm.CreateNew(nil);

  // Create child form with parent form as parent
  FChildForm:= TForm.CreateNew(FForm);
  FChildForm.Parent:= FForm;

  // Create button on child form
  FButton:= TButton.Create(FChildForm);
  FButton.Parent:= FChildForm;

  // With TopForm=True, should return the topmost form (FForm)
  Assert.AreSame(FForm, GetParentForm(FButton, True));
end;


procedure TTestLightFmxCommonHelpers.TestGetParentForm_TopFormFalse;
begin
  // Create parent form
  FForm:= TForm.CreateNew(nil);

  // Create child form with parent form as parent
  FChildForm:= TForm.CreateNew(FForm);
  FChildForm.Parent:= FForm;

  // Create button on child form
  FButton:= TButton.Create(FChildForm);
  FButton.Parent:= FChildForm;

  // With TopForm=False, should return the immediate parent form (FChildForm)
  Assert.AreSame(FChildForm, GetParentForm(FButton, False));
end;


{ CopyToClipboard Tests }

procedure TTestLightFmxCommonHelpers.TestCopyToClipboard_BasicText;
var
  ClipboardService: IFMXClipboardService;
  ClipboardAvailable: Boolean;
  CopyResult: Boolean;
begin
  // Check if clipboard service is available on this platform
  ClipboardAvailable:= TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService);

  if NOT ClipboardAvailable then
  begin
    Assert.Pass('Clipboard service not available on this platform - test skipped');
    EXIT;
  end;

  { Another text first, so the read-back below can only match after a real write }
  ClipboardService.SetClipboard('Text before the test');

  // Test copying text
  CopyResult:= CopyToClipboard('Test clipboard text');
  Assert.IsTrue(CopyResult, 'CopyToClipboard should return True when clipboard is available');
  Assert.AreEqual('Test clipboard text', ClipboardService.GetClipboard.ToString, 'The text must be on the clipboard');
end;


procedure TTestLightFmxCommonHelpers.TestCopyToClipboard_EmptyString;
var
  ClipboardService: IFMXClipboardService;
  ClipboardAvailable: Boolean;
  CopyResult: Boolean;
  ClipValue: TValue;
begin
  ClipboardAvailable:= TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService);

  if NOT ClipboardAvailable then
  begin
    Assert.Pass('Clipboard service not available on this platform - test skipped');
    EXIT;
  end;

  { Another text first, so the empty read-back below can only come from the write }
  ClipboardService.SetClipboard('Text before the test');

  // Empty string should still succeed (copies empty text to clipboard)
  CopyResult:= CopyToClipboard('');
  Assert.IsTrue(CopyResult, 'CopyToClipboard should succeed with empty string');

  { An empty TEXT is on the clipboard, not "nothing": GetClipboard returns TValue.Empty only when no text and no image is there
    (c:\Delphi\Delphi 13\source\fmx\FMX.Clipboard.Win.pas:114-121) }
  ClipValue:= ClipboardService.GetClipboard;
  Assert.IsFalse(ClipValue.IsEmpty, 'An empty text must still be on the clipboard');
  Assert.AreEqual('', ClipValue.AsString, 'The old text must be replaced by the empty one');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightFmxCommonHelpers);

end.
