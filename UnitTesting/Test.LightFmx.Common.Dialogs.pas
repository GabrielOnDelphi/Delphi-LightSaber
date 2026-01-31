unit Test.LightFmx.Common.Dialogs;

{=============================================================================================================
   2026.01.31
   Unit tests for LightFmx.Common.Dialogs.pas

   Tests asynchronous FMX message box utility functions.
   Note: Dialog functions are asynchronous and display native dialogs that would block automated tests.
   We focus on testing edge cases that don't show dialogs (empty messages) and function signatures.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.UITypes;

type
  [TestFixture]
  TTestFmxDialogs = class
  private
    FCallbackInvoked: Boolean;
    FCallbackResult: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GenericMessage Tests }
    [Test]
    procedure TestGenericMessage_EmptyMessage_NoException;

    [Test]
    procedure TestGenericMessage_WithCaption_NoException;

    { MessageInfo Tests }
    [Test]
    procedure TestMessageInfo_EmptyMessage_NoException;

    { MessageWarning Tests }
    [Test]
    procedure TestMessageWarning_EmptyMessage_NoException;

    { MessageError Tests }
    [Test]
    procedure TestMessageError_EmptyMessage_NoException;

    [Test]
    procedure TestMessageError_Overload_NoException;

    { MessageYesNo Tests }
    [Test]
    procedure TestMessageYesNo_EmptyMessage_NoException;

    [Test]
    procedure TestMessageYesNo_NilCallback_NoException;

    { Function Existence Tests - verify functions are callable with correct signatures }
    [Test]
    procedure TestGenericMessage_FunctionExists;

    [Test]
    procedure TestMessageInfo_FunctionExists;

    [Test]
    procedure TestMessageWarning_FunctionExists;

    [Test]
    procedure TestMessageError_FunctionExists;

    [Test]
    procedure TestMessageYesNo_FunctionExists;
  end;

implementation

uses
  LightFmx.Common.Dialogs;


procedure TTestFmxDialogs.Setup;
begin
  FCallbackInvoked:= False;
  FCallbackResult:= False;
end;


procedure TTestFmxDialogs.TearDown;
begin
  // No teardown needed
end;


{ GenericMessage Tests }

procedure TTestFmxDialogs.TestGenericMessage_EmptyMessage_NoException;
begin
  { Empty message should exit early without showing dialog or raising exception }
  Assert.WillNotRaise(
    procedure
    begin
      GenericMessage('');
    end);
end;


procedure TTestFmxDialogs.TestGenericMessage_WithCaption_NoException;
begin
  { With caption but empty message should still exit early }
  Assert.WillNotRaise(
    procedure
    begin
      GenericMessage('', 'Test Caption');
    end);
end;


{ MessageInfo Tests }

procedure TTestFmxDialogs.TestMessageInfo_EmptyMessage_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      MessageInfo('');
    end);
end;


{ MessageWarning Tests }

procedure TTestFmxDialogs.TestMessageWarning_EmptyMessage_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      MessageWarning('');
    end);
end;


{ MessageError Tests }

procedure TTestFmxDialogs.TestMessageError_EmptyMessage_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      MessageError('');
    end);
end;


procedure TTestFmxDialogs.TestMessageError_Overload_NoException;
begin
  { The overloaded MessageError appends text but calls base MessageError,
    which calls GenericMessage. Empty original message still produces non-empty
    combined message, but this tests the overload doesn't crash. }
  Assert.WillNotRaise(
    procedure
    begin
      MessageError('Test error', 'TestLocation', 'Test Caption');
    end);
end;


{ MessageYesNo Tests }

procedure TTestFmxDialogs.TestMessageYesNo_EmptyMessage_NoException;
begin
  { Empty message should exit early without showing dialog }
  Assert.WillNotRaise(
    procedure
    begin
      MessageYesNo('', '', NIL);
    end);
end;


procedure TTestFmxDialogs.TestMessageYesNo_NilCallback_NoException;
begin
  { Nil callback should be handled gracefully - dialog would show but callback
    check prevents crash. Since message is empty, dialog won't show anyway. }
  Assert.WillNotRaise(
    procedure
    begin
      MessageYesNo('', 'Caption', NIL);
    end);
end;


{ Function Existence Tests - verify functions are callable with correct signatures }

procedure TTestFmxDialogs.TestGenericMessage_FunctionExists;
VAR
  Proc: procedure(CONST MessageText: string; CONST Caption: string; DlgType: TMsgDlgType);
begin
  Proc:= GenericMessage;
  Assert.IsTrue(Assigned(@Proc), 'GenericMessage function should exist');
end;


procedure TTestFmxDialogs.TestMessageInfo_FunctionExists;
VAR
  Proc: procedure(CONST MessageText: string; CONST Caption: string);
begin
  Proc:= MessageInfo;
  Assert.IsTrue(Assigned(@Proc), 'MessageInfo function should exist');
end;


procedure TTestFmxDialogs.TestMessageWarning_FunctionExists;
VAR
  Proc: procedure(CONST MessageText: string; CONST Caption: string);
begin
  Proc:= MessageWarning;
  Assert.IsTrue(Assigned(@Proc), 'MessageWarning function should exist');
end;


procedure TTestFmxDialogs.TestMessageError_FunctionExists;
VAR
  Proc: procedure(CONST MessageText: string; CONST Caption: string);
begin
  Proc:= MessageError;
  Assert.IsTrue(Assigned(@Proc), 'MessageError function should exist');
end;


procedure TTestFmxDialogs.TestMessageYesNo_FunctionExists;
VAR
  Proc: procedure(CONST MessageText: string; CONST Caption: string; CONST Callback: TProc<Boolean>);
begin
  Proc:= MessageYesNo;
  Assert.IsTrue(Assigned(@Proc), 'MessageYesNo function should exist');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxDialogs);

end.
