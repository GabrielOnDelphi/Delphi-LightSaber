unit Test.LightVcl.Common.Dialogs;

{=============================================================================================================
   2026.01
   Unit tests for LightVcl.Common.Dialogs.pas

   Tests message box utility functions.
   Note: Most dialog functions display modal dialogs that would block automated tests.
   We focus on testing edge cases that don't show dialogs (empty messages, exceptions).
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Forms;

type
  [TestFixture]
  TTestDialogs = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { MesajGeneric Tests }
    [Test]
    procedure TestMesajGeneric_EmptyMessage_ReturnsZero;

    [Test]
    procedure TestMesajGeneric_EmptyMessage_NoException;

    { MesajYesNo Tests }
    [Test]
    procedure TestMesajYesNo_EmptyMessage_RaisesException;

    [Test]
    procedure TestMesajYesNo_EmptyMessage_ExceptionMessage;

    { MesajTaskDlg Tests }
    [Test]
    procedure TestMesajTaskDlg_EmptyMessage_NoException;

    { Function Existence Tests - verify functions are callable with correct signatures }
    [Test]
    procedure TestMessageInfo_FunctionExists;

    [Test]
    procedure TestMessageWarning_FunctionExists;

    [Test]
    procedure TestMessageError_FunctionExists;

    [Test]
    procedure TestMesajErrDetail_FunctionExists;

    [Test]
    procedure TestMesajYesNo_FunctionExists;
  end;

implementation

uses
  LightVcl.Common.Dialogs;


procedure TTestDialogs.Setup;
begin
  // No setup needed
end;


procedure TTestDialogs.TearDown;
begin
  // No teardown needed
end;


{ MesajGeneric Tests }

procedure TTestDialogs.TestMesajGeneric_EmptyMessage_ReturnsZero;
var
  Result: Integer;
begin
  { Empty message should return 0 without showing dialog }
  Result:= MesajGeneric('');

  Assert.AreEqual(0, Result, 'Empty message should return 0');
end;


procedure TTestDialogs.TestMesajGeneric_EmptyMessage_NoException;
begin
  { Empty message should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      MesajGeneric('');
    end);
end;


{ MesajYesNo Tests }

procedure TTestDialogs.TestMesajYesNo_EmptyMessage_RaisesException;
begin
  { Empty message should raise exception }
  Assert.WillRaise(
    procedure
    begin
      MesajYesNo('');
    end,
    Exception);
end;


procedure TTestDialogs.TestMesajYesNo_EmptyMessage_ExceptionMessage;
begin
  { Verify the exception message is correct }
  try
    MesajYesNo('');
    Assert.Fail('Should have raised exception');
  except
    on E: Exception do
      Assert.AreEqual('No message provided for MesajYesNo() !', E.Message,
        'Exception message should match');
  end;
end;


{ MesajTaskDlg Tests }

procedure TTestDialogs.TestMesajTaskDlg_EmptyMessage_NoException;
begin
  { Empty message should exit early without showing dialog }
  Assert.WillNotRaise(
    procedure
    begin
      MesajTaskDlg('', '');
    end);
end;


{ Function Existence Tests - verify functions are callable with correct signatures }

procedure TTestDialogs.TestMessageInfo_FunctionExists;
var
  Proc: procedure(CONST MessageText: string; CONST Title: string);
begin
  Proc:= MessageInfo;
  Assert.IsTrue(Assigned(@Proc), 'MessageInfo function should exist');
end;


procedure TTestDialogs.TestMessageWarning_FunctionExists;
var
  Proc: procedure(CONST MessageText: string; CONST Title: string);
begin
  Proc:= MessageWarning;
  Assert.IsTrue(Assigned(@Proc), 'MessageWarning function should exist');
end;


procedure TTestDialogs.TestMessageError_FunctionExists;
var
  Proc: procedure(CONST MessageText: string; CONST Title: string);
begin
  Proc:= MessageError;
  Assert.IsTrue(Assigned(@Proc), 'MessageError function should exist');
end;


procedure TTestDialogs.TestMesajErrDetail_FunctionExists;
var
  Proc: procedure(CONST MessageText, Where: string);
begin
  Proc:= MesajErrDetail;
  Assert.IsTrue(Assigned(@Proc), 'MesajErrDetail function should exist');
end;


procedure TTestDialogs.TestMesajYesNo_FunctionExists;
var
  Func: function(CONST MessageText: string; CONST Title: string): Boolean;
begin
  Func:= MesajYesNo;
  Assert.IsTrue(Assigned(@Func), 'MesajYesNo function should exist');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDialogs);

end.
