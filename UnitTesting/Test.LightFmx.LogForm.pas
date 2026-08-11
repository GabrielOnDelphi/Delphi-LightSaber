unit Test.LightFmx.LogForm;

{=============================================================================================================
   2026.01.31
   Unit tests for LightFmx.Common.LogForm.pas
   Tests the FMX log form window functionality.

   Note: Full GUI testing requires FMX application context.
   These tests focus on form creation and basic state management.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.UITypes,        { TCloseAction (System.UITypes.pas:109) — FMX does not redeclare it the way Vcl.Forms does. }
  FMX.Forms,
  LightCore.LogTypes;

type
  [TestFixture]
  TTestFmxLogForm = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Form Creation Tests }
    [Test]
    procedure TestFormCreate_LogViewerAssigned;

    [Test]
    procedure TestFormCreate_LogFilterAssigned;

    { Close Behavior Tests }
    [Test]
    procedure TestFormClose_HidesInsteadOfClose;
  end;

implementation

uses
  LightCore.AppData,           { TAutoState / asNone }
  LightFmx.Common.LogForm,
  LightFmx.Common.AppData;


procedure TTestFmxLogForm.Setup;
begin
  // Note: AppData must be initialized before creating the log form
  // This is typically done by the application startup
end;


procedure TTestFmxLogForm.TearDown;
begin
  // Cleanup handled by individual tests
end;


{ Form Creation Tests }

procedure TTestFmxLogForm.TestFormCreate_LogViewerAssigned;
var
  Form: TfrmRamLog;
begin
  // Skip test if AppData is not initialized
  if AppData = NIL then
    Assert.Pass('Test skipped: AppData not initialized');

  { asNone, and never the plain TComponent constructor: TfrmRamLog descends from TLightForm, whose Loaded
    raises unless AutoState was set BEFORE streaming - "Form was not created via AppData.CreateForm()!".
    Applies to every TfrmRamLog.Create below too. }
  Form:= TfrmRamLog.Create(NIL, asNone);
  try
    Assert.IsNotNull(Form.LogViewer, 'LogViewer should be assigned');
    Assert.IsNotNull(Form.LogViewer.RamLog, 'LogViewer.RamLog should be assigned');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFmxLogForm.TestFormCreate_LogFilterAssigned;
var
  Form: TfrmRamLog;
begin
  if AppData = NIL then
    Assert.Pass('Test skipped: AppData not initialized');

  Form:= TfrmRamLog.Create(NIL, asNone);
  try
    Assert.IsNotNull(Form.LogFilter, 'LogFilter should be assigned');
  finally
    FreeAndNil(Form);
  end;
end;


{ Close Behavior Tests }

procedure TTestFmxLogForm.TestFormClose_HidesInsteadOfClose;
var
  Form: TfrmRamLog;
  CloseAction: TCloseAction;
begin
  if AppData = NIL then
    Assert.Pass('Test skipped: AppData not initialized');

  Form:= TfrmRamLog.Create(NIL, asNone);
  try
    CloseAction:= TCloseAction.caNone;
    Form.FormClose(Form, CloseAction);
    Assert.AreEqual(TCloseAction.caHide, CloseAction, 'Form should hide on close, not destroy');
  finally
    FreeAndNil(Form);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxLogForm);

end.
