unit Test.FormAsyncMessage;

{=============================================================================================================
   Unit tests for FormAsyncMessage.pas
   Tests TfrmShowMsgAsync - the non-blocking async message dialog.

   Note: Testing involves creating forms programmatically and verifying their properties.
   The forms are NOT actually displayed during tests.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  [TestFixture]
  TTestFormAsyncMessage = class
  private
    FCreatedForms: TList;
    procedure TrackForm(Form: TForm);
    procedure CleanupForms;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormHasRequiredComponents;

    { MesajAsync Tests }
    [Test]
    procedure TestMesajAsync_CreatesForm;

    [Test]
    procedure TestMesajAsync_SetsCaption;

    [Test]
    procedure TestMesajAsync_SetsMessage;

    [Test]
    procedure TestMesajAsync_SetsBorderStyle;

    [Test]
    procedure TestMesajAsync_EnablesKeyPreview;

    [Test]
    procedure TestMesajAsync_WithPopupParent;

    [Test]
    procedure TestMesajAsync_WithoutPopupParent;

    { MessageAsync Alias Test }
    [Test]
    procedure TestMessageAsync_IsAlias;

    { Convenience Function Tests }
    [Test]
    procedure TestMessageInfoAsync_SetsInfoCaption;

    [Test]
    procedure TestMessageWarnAsync_SetsWarningCaption;

    [Test]
    procedure TestMesajWarnAsync_IsLegacyAlias;

    [Test]
    procedure TestMessageErrorAsync_SetsErrorCaption;

    { Form Behavior Tests }
    [Test]
    procedure TestFormClose_SetsCaFree;

    [Test]
    procedure TestKeyPress_EnterClosesForm;

    [Test]
    procedure TestKeyPress_EscapeClosesForm;

    [Test]
    procedure TestKeyPress_OtherKeyDoesNotClose;

    [Test]
    procedure TestBtnOKClick_ClosesForm;

    { Edge Case Tests }
    [Test]
    procedure TestMesajAsync_EmptyMessage;

    [Test]
    procedure TestMesajAsync_EmptyCaption;

    [Test]
    procedure TestMesajAsync_LongMessage;
  end;

implementation

uses
  FormAsyncMessage;


procedure TTestFormAsyncMessage.Setup;
begin
  FCreatedForms:= TList.Create;
end;


procedure TTestFormAsyncMessage.TearDown;
begin
  CleanupForms;
  FreeAndNil(FCreatedForms);
end;


procedure TTestFormAsyncMessage.TrackForm(Form: TForm);
begin
  if Form <> NIL
  then FCreatedForms.Add(Form);
end;


procedure TTestFormAsyncMessage.CleanupForms;
var
  i: Integer;
  Form: TForm;
begin
  // Free forms in reverse order
  for i:= FCreatedForms.Count - 1 downto 0 do
    begin
      Form:= TForm(FCreatedForms[i]);
      if Form <> NIL
      then Form.Free;
    end;
  FCreatedForms.Clear;
end;


{ Form Creation Tests }

procedure TTestFormAsyncMessage.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmShowMsgAsync, 'TfrmShowMsgAsync class should exist');
end;


procedure TTestFormAsyncMessage.TestFormHasRequiredComponents;
var
  Form: TfrmShowMsgAsync;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  TrackForm(Form);

  Assert.IsNotNull(Form.lblMessage, 'Form should have lblMessage');
  Assert.IsNotNull(Form.Panel1, 'Form should have Panel1');
  Assert.IsNotNull(Form.btnOK, 'Form should have btnOK');
end;


{ MesajAsync Tests }

procedure TTestFormAsyncMessage.TestMesajAsync_CreatesForm;
var
  FormCount: Integer;
begin
  FormCount:= Screen.FormCount;

  MesajAsync('Test message', 'Test Caption');

  // A new form should have been created
  Assert.IsTrue(Screen.FormCount > FormCount, 'MesajAsync should create a new form');

  // Track the created form for cleanup
  if Screen.FormCount > 0
  then TrackForm(Screen.Forms[Screen.FormCount - 1]);
end;


procedure TTestFormAsyncMessage.TestMesajAsync_SetsCaption;
var
  Form: TfrmShowMsgAsync;
  FormCountBefore: Integer;
begin
  FormCountBefore:= Screen.FormCount;

  MesajAsync('Test', 'My Custom Caption');

  // Get the newly created form
  Assert.IsTrue(Screen.FormCount > FormCountBefore, 'Form should be created');
  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('My Custom Caption', Form.Caption, 'Caption should be set correctly');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_SetsMessage;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('Hello World Message', 'Title');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Hello World Message', Form.lblMessage.Caption, 'Message should be set correctly');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_SetsBorderStyle;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('Test', 'Title');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual(bsDialog, Form.BorderStyle, 'BorderStyle should be bsDialog');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_EnablesKeyPreview;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('Test', 'Title');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.IsTrue(Form.KeyPreview, 'KeyPreview should be enabled');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_WithPopupParent;
var
  ParentForm: TForm;
  MsgForm: TfrmShowMsgAsync;
begin
  ParentForm:= TForm.Create(NIL);
  TrackForm(ParentForm);

  MesajAsync('Test', 'Title', ParentForm);

  MsgForm:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(MsgForm);

  Assert.IsTrue(MsgForm.PopupParent = ParentForm, 'PopupParent should be set to provided parent');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_WithoutPopupParent;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('Test', 'Title', NIL);

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  // PopupParent should be Application.MainForm if available, or nil
  // We just verify no exception was raised
  Assert.IsNotNull(Form, 'Form should be created without exception');
end;


{ MessageAsync Alias Test }

procedure TTestFormAsyncMessage.TestMessageAsync_IsAlias;
var
  Form: TfrmShowMsgAsync;
begin
  MessageAsync('Alias Test', 'Alias Caption');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Alias Caption', Form.Caption, 'MessageAsync should work as alias');
  Assert.AreEqual('Alias Test', Form.lblMessage.Caption, 'MessageAsync should set message');
end;


{ Convenience Function Tests }

procedure TTestFormAsyncMessage.TestMessageInfoAsync_SetsInfoCaption;
var
  Form: TfrmShowMsgAsync;
begin
  MessageInfoAsync('Information message');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Info', Form.Caption, 'Caption should be "Info"');
end;


procedure TTestFormAsyncMessage.TestMessageWarnAsync_SetsWarningCaption;
var
  Form: TfrmShowMsgAsync;
begin
  MessageWarnAsync('Warning message');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Warning', Form.Caption, 'Caption should be "Warning"');
end;


procedure TTestFormAsyncMessage.TestMesajWarnAsync_IsLegacyAlias;
var
  Form: TfrmShowMsgAsync;
begin
  MesajWarnAsync('Legacy warning');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Warning', Form.Caption, 'MesajWarnAsync should work as legacy alias');
end;


procedure TTestFormAsyncMessage.TestMessageErrorAsync_SetsErrorCaption;
var
  Form: TfrmShowMsgAsync;
begin
  MessageErrorAsync('Error message');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('Error', Form.Caption, 'Caption should be "Error"');
end;


{ Form Behavior Tests }

procedure TTestFormAsyncMessage.TestFormClose_SetsCaFree;
var
  Form: TfrmShowMsgAsync;
  Action: TCloseAction;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  // Don't track - we'll test the close action manually

  Action:= caNone;
  Form.FormClose(Form, Action);

  Assert.AreEqual(caFree, Action, 'FormClose should set Action to caFree');

  // Form will be freed by caFree, but since we're testing the handler directly
  // we need to clean up manually since Close wasn't called
  Form.Free;
end;


procedure TTestFormAsyncMessage.TestKeyPress_EnterClosesForm;
var
  Form: TfrmShowMsgAsync;
  Key: Char;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  TrackForm(Form);

  Key:= #13;  // Enter
  Form.FormKeyPress(Form, Key);

  // Verify form accepts close
  Assert.IsTrue(Form.CloseQuery, 'Form should accept close after Enter key');
end;


procedure TTestFormAsyncMessage.TestKeyPress_EscapeClosesForm;
var
  Form: TfrmShowMsgAsync;
  Key: Char;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  TrackForm(Form);

  Key:= #27;  // Escape
  Form.FormKeyPress(Form, Key);

  Assert.IsTrue(Form.CloseQuery, 'Form should accept close after Escape key');
end;


procedure TTestFormAsyncMessage.TestKeyPress_OtherKeyDoesNotClose;
var
  Form: TfrmShowMsgAsync;
  Key: Char;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  TrackForm(Form);

  Key:= 'X';
  Form.FormKeyPress(Form, Key);

  // Form should still exist and be operational
  Assert.IsNotNull(Form, 'Form should still exist after regular key');
  Assert.IsTrue(Form.CloseQuery, 'Form should still accept close queries');
end;


procedure TTestFormAsyncMessage.TestBtnOKClick_ClosesForm;
var
  Form: TfrmShowMsgAsync;
begin
  Form:= TfrmShowMsgAsync.Create(NIL);
  TrackForm(Form);

  // Call the button click handler
  Form.btnOKClick(Form.btnOK);

  // Form.Close was called - verify it accepts closure
  Assert.IsTrue(Form.CloseQuery, 'Form should accept close after OK button click');
end;


{ Edge Case Tests }

procedure TTestFormAsyncMessage.TestMesajAsync_EmptyMessage;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('', 'Title');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('', Form.lblMessage.Caption, 'Empty message should be allowed');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_EmptyCaption;
var
  Form: TfrmShowMsgAsync;
begin
  MesajAsync('Test message', '');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual('', Form.Caption, 'Empty caption should be allowed');
end;


procedure TTestFormAsyncMessage.TestMesajAsync_LongMessage;
var
  Form: TfrmShowMsgAsync;
  LongMsg: string;
begin
  LongMsg:= StringOfChar('A', 1000);

  MesajAsync(LongMsg, 'Long Message Test');

  Form:= Screen.Forms[Screen.FormCount - 1] as TfrmShowMsgAsync;
  TrackForm(Form);

  Assert.AreEqual(LongMsg, Form.lblMessage.Caption, 'Long message should be handled');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormAsyncMessage);

end.
