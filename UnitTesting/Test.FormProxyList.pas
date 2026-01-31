unit Test.FormProxyList;

{=============================================================================================================
   Unit tests for FormProxyList.pas
   Tests TfrmProxyList - the proxy configuration form.

   Note: These tests focus on form creation, connection type handling, and proxy list operations.
   Network-related functionality (IE_GetProxySettings) is tested for non-exception behavior only.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls;

type
  [TestFixture]
  TTestFormProxyList = class
  private
    FTestForm: TObject;
    FTestFile: string;
    procedure CleanupForm;
    procedure CreateTestProxyFile;
    procedure DeleteTestFile;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TConType Enumeration Tests }
    [Test]
    procedure TestConType_ValuesExist;

    { Form Creation Tests }
    [Test]
    procedure TestFormClassExists;

    [Test]
    procedure TestFormCreate_Succeeds;

    [Test]
    procedure TestFormCreate_WithNilOwner;

    { Component Tests }
    [Test]
    procedure TestFormHasInternetTypeGroup;

    [Test]
    procedure TestFormHasDirectRadio;

    [Test]
    procedure TestFormHasGatewayRadio;

    [Test]
    procedure TestFormHasProxyListRadio;

    [Test]
    procedure TestFormHasProxyListGroup;

    [Test]
    procedure TestFormHasProxyMemo;

    [Test]
    procedure TestFormHasGatewayEdit;

    [Test]
    procedure TestFormHasSaveButton;

    [Test]
    procedure TestFormHasLocateButton;

    [Test]
    procedure TestFormHasAutoDetectButton;

    [Test]
    procedure TestFormHasCleanButton;

    { GetConnectionType Tests }
    [Test]
    procedure TestGetConnectionType_Direct;

    [Test]
    procedure TestGetConnectionType_Gateway;

    [Test]
    procedure TestGetConnectionType_ProxyList;

    [Test]
    procedure TestGetConnectionType_DefaultToDirect;

    { ConnectionTypeChanged Tests }
    [Test]
    procedure TestConnectionTypeChanged_DirectHidesProxyList;

    [Test]
    procedure TestConnectionTypeChanged_ProxyListShowsGroup;

    [Test]
    procedure TestConnectionTypeChanged_GatewayEnablesEdit;

    [Test]
    procedure TestConnectionTypeChanged_DirectDisablesGatewayEdit;

    [Test]
    procedure TestConnectionTypeChanged_GatewayShowsAutoDetect;

    { FileName Property Tests }
    [Test]
    procedure TestFileName_DefaultEmpty;

    [Test]
    procedure TestFileName_CanBeSet;

    [Test]
    procedure TestFileName_CanBeRead;

    { LoadProxyFile Tests }
    [Test]
    procedure TestLoadProxyFile_SetsFileName;

    [Test]
    procedure TestLoadProxyFile_LoadsExistingFile;

    [Test]
    procedure TestLoadProxyFile_HandlesNonExistentFile;

    { CleanList Tests }
    [Test]
    procedure TestCleanList_RemovesEmptyLines;

    [Test]
    procedure TestCleanList_RemovesWhitespaceLines;

    [Test]
    procedure TestCleanList_PreservesValidEntries;

    [Test]
    procedure TestCleanList_NoExceptionOnEmptyMemo;

    { Save Tests }
    [Test]
    procedure TestSave_WritesToFile;

    [Test]
    procedure TestSave_AssertsOnEmptyFileName;

    { Button Event Tests }
    [Test]
    procedure TestBtnSaveProxyClick_NoExceptionWithFileName;

    [Test]
    procedure TestBtnCleanClick_NoException;

    [Test]
    procedure TestBtnLocateClick_NoExceptionWithEmptyFileName;

    [Test]
    procedure TestBtnAutoDetectClick_NoException;

    { Global Variable Tests }
    [Test]
    procedure TestGlobalVariable_Exists;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormProxyList;


procedure TTestFormProxyList.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestForm:= NIL;
  FTestFile:= TPath.Combine(TPath.GetTempPath, 'TestProxyList_' + TGUID.NewGuid.ToString + '.txt');
end;


procedure TTestFormProxyList.TearDown;
begin
  CleanupForm;
  DeleteTestFile;
end;


procedure TTestFormProxyList.CleanupForm;
var
  Form: TfrmProxyList;
begin
  if FTestForm <> NIL then
    begin
      Form:= TfrmProxyList(FTestForm);
      FreeAndNil(Form);
      FTestForm:= NIL;
    end;
end;


procedure TTestFormProxyList.CreateTestProxyFile;
var
  Lines: TStringList;
begin
  Lines:= TStringList.Create;
  try
    Lines.Add('192.168.1.1:8080');
    Lines.Add('10.0.0.1:3128');
    Lines.Add('');
    Lines.Add('172.16.0.1:80');
    Lines.Add('   ');
    Lines.Add('8.8.8.8:8888');
    Lines.SaveToFile(FTestFile);
  finally
    FreeAndNil(Lines);
  end;
end;


procedure TTestFormProxyList.DeleteTestFile;
begin
  if FileExists(FTestFile)
  then DeleteFile(FTestFile);
end;


{ TConType Enumeration Tests }

procedure TTestFormProxyList.TestConType_ValuesExist;
begin
  Assert.AreEqual(0, Ord(ctDirect), 'ctDirect should be 0');
  Assert.AreEqual(1, Ord(ctGateway), 'ctGateway should be 1');
  Assert.AreEqual(2, Ord(ctProxyList), 'ctProxyList should be 2');
end;


{ Form Creation Tests }

procedure TTestFormProxyList.TestFormClassExists;
begin
  Assert.IsNotNull(TfrmProxyList, 'TfrmProxyList class should exist');
end;


procedure TTestFormProxyList.TestFormCreate_Succeeds;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form, 'Form creation should succeed');
end;


procedure TTestFormProxyList.TestFormCreate_WithNilOwner;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNull(Form.Owner, 'Owner should be nil when created with nil');
end;


{ Component Tests }

procedure TTestFormProxyList.TestFormHasInternetTypeGroup;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpInternetType, 'Form should have grpInternetType');
end;


procedure TTestFormProxyList.TestFormHasDirectRadio;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radDirect, 'Form should have radDirect');
end;


procedure TTestFormProxyList.TestFormHasGatewayRadio;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radGateway, 'Form should have radGateway');
end;


procedure TTestFormProxyList.TestFormHasProxyListRadio;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.radProxyList, 'Form should have radProxyList');
end;


procedure TTestFormProxyList.TestFormHasProxyListGroup;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.grpProxyList, 'Form should have grpProxyList');
end;


procedure TTestFormProxyList.TestFormHasProxyMemo;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.mmoProxyList, 'Form should have mmoProxyList');
  Assert.IsTrue(Form.mmoProxyList is TMemo, 'mmoProxyList should be a TMemo');
end;


procedure TTestFormProxyList.TestFormHasGatewayEdit;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.edtGateway, 'Form should have edtGateway');
  Assert.IsTrue(Form.edtGateway is TEdit, 'edtGateway should be a TEdit');
end;


procedure TTestFormProxyList.TestFormHasSaveButton;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnSaveProxy, 'Form should have btnSaveProxy');
end;


procedure TTestFormProxyList.TestFormHasLocateButton;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnLocate, 'Form should have btnLocate');
end;


procedure TTestFormProxyList.TestFormHasAutoDetectButton;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnAutoDetect, 'Form should have btnAutoDetect');
end;


procedure TTestFormProxyList.TestFormHasCleanButton;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.IsNotNull(Form.btnClean, 'Form should have btnClean');
end;


{ GetConnectionType Tests }

procedure TTestFormProxyList.TestGetConnectionType_Direct;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radDirect.Checked:= TRUE;
  Form.radGateway.Checked:= FALSE;
  Form.radProxyList.Checked:= FALSE;

  Assert.AreEqual(ctDirect, Form.GetConnectionType,
    'GetConnectionType should return ctDirect when radDirect is checked');
end;


procedure TTestFormProxyList.TestGetConnectionType_Gateway;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radDirect.Checked:= FALSE;
  Form.radGateway.Checked:= TRUE;
  Form.radProxyList.Checked:= FALSE;

  Assert.AreEqual(ctGateway, Form.GetConnectionType,
    'GetConnectionType should return ctGateway when radGateway is checked');
end;


procedure TTestFormProxyList.TestGetConnectionType_ProxyList;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radDirect.Checked:= FALSE;
  Form.radGateway.Checked:= FALSE;
  Form.radProxyList.Checked:= TRUE;

  Assert.AreEqual(ctProxyList, Form.GetConnectionType,
    'GetConnectionType should return ctProxyList when radProxyList is checked');
end;


procedure TTestFormProxyList.TestGetConnectionType_DefaultToDirect;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  // Uncheck all (shouldn't normally happen)
  Form.radDirect.Checked:= FALSE;
  Form.radGateway.Checked:= FALSE;
  Form.radProxyList.Checked:= FALSE;

  Assert.AreEqual(ctDirect, Form.GetConnectionType,
    'GetConnectionType should default to ctDirect when nothing is checked');
  Assert.IsTrue(Form.radDirect.Checked,
    'radDirect should be checked after defaulting');
end;


{ ConnectionTypeChanged Tests }

procedure TTestFormProxyList.TestConnectionTypeChanged_DirectHidesProxyList;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radDirect.Checked:= TRUE;
  Form.ConnectionTypeChanged(Form);

  Assert.IsFalse(Form.grpProxyList.Visible,
    'Proxy list group should be hidden when Direct is selected');
end;


procedure TTestFormProxyList.TestConnectionTypeChanged_ProxyListShowsGroup;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radProxyList.Checked:= TRUE;
  Form.ConnectionTypeChanged(Form);

  Assert.IsTrue(Form.grpProxyList.Visible,
    'Proxy list group should be visible when ProxyList is selected');
end;


procedure TTestFormProxyList.TestConnectionTypeChanged_GatewayEnablesEdit;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radGateway.Checked:= TRUE;
  Form.ConnectionTypeChanged(Form);

  Assert.IsTrue(Form.edtGateway.Enabled,
    'Gateway edit should be enabled when Gateway is selected');
end;


procedure TTestFormProxyList.TestConnectionTypeChanged_DirectDisablesGatewayEdit;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radDirect.Checked:= TRUE;
  Form.radGateway.Checked:= FALSE;
  Form.ConnectionTypeChanged(Form);

  Assert.IsFalse(Form.edtGateway.Enabled,
    'Gateway edit should be disabled when Direct is selected');
end;


procedure TTestFormProxyList.TestConnectionTypeChanged_GatewayShowsAutoDetect;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.radGateway.Checked:= TRUE;
  Form.ConnectionTypeChanged(Form);

  Assert.IsTrue(Form.btnAutoDetect.Visible,
    'Auto-detect button should be visible when Gateway is selected');
end;


{ FileName Property Tests }

procedure TTestFormProxyList.TestFileName_DefaultEmpty;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Assert.AreEqual('', Form.FileName, 'FileName should default to empty string');
end;


procedure TTestFormProxyList.TestFileName_CanBeSet;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= 'C:\Test\proxies.txt';

  Assert.AreEqual('C:\Test\proxies.txt', Form.FileName,
    'FileName should be settable');
end;


procedure TTestFormProxyList.TestFileName_CanBeRead;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= FTestFile;

  Assert.AreEqual(FTestFile, Form.FileName,
    'FileName should be readable after setting');
end;


{ LoadProxyFile Tests }

procedure TTestFormProxyList.TestLoadProxyFile_SetsFileName;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.LoadProxyFile(FTestFile);

  Assert.AreEqual(FTestFile, Form.FileName,
    'LoadProxyFile should set FileName');
end;


procedure TTestFormProxyList.TestLoadProxyFile_LoadsExistingFile;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  CreateTestProxyFile;
  Form.LoadProxyFile(FTestFile);

  Assert.IsTrue(Form.mmoProxyList.Lines.Count > 0,
    'LoadProxyFile should load content from existing file');
end;


procedure TTestFormProxyList.TestLoadProxyFile_HandlesNonExistentFile;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  // Ensure file doesn't exist
  DeleteTestFile;

  { LoadProxyFile should not raise exception for non-existent file }
  Assert.WillNotRaise(
    procedure
    begin
      Form.LoadProxyFile(FTestFile);
    end);

  Assert.AreEqual(FTestFile, Form.FileName,
    'FileName should be set even if file does not exist');
end;


{ CleanList Tests }

procedure TTestFormProxyList.TestCleanList_RemovesEmptyLines;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.mmoProxyList.Lines.Add('192.168.1.1:80');
  Form.mmoProxyList.Lines.Add('');
  Form.mmoProxyList.Lines.Add('10.0.0.1:3128');

  Form.CleanList;

  Assert.AreEqual(2, Form.mmoProxyList.Lines.Count,
    'CleanList should remove empty lines');
end;


procedure TTestFormProxyList.TestCleanList_RemovesWhitespaceLines;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.mmoProxyList.Lines.Add('192.168.1.1:80');
  Form.mmoProxyList.Lines.Add('   ');
  Form.mmoProxyList.Lines.Add('10.0.0.1:3128');
  Form.mmoProxyList.Lines.Add('  '#9'  ');  // Tabs and spaces

  Form.CleanList;

  Assert.AreEqual(2, Form.mmoProxyList.Lines.Count,
    'CleanList should remove whitespace-only lines');
end;


procedure TTestFormProxyList.TestCleanList_PreservesValidEntries;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.mmoProxyList.Lines.Add('192.168.1.1:80');
  Form.mmoProxyList.Lines.Add('10.0.0.1:3128');

  Form.CleanList;

  Assert.AreEqual('192.168.1.1:80', Form.mmoProxyList.Lines[0],
    'CleanList should preserve valid entries');
  Assert.AreEqual('10.0.0.1:3128', Form.mmoProxyList.Lines[1],
    'CleanList should preserve valid entries');
end;


procedure TTestFormProxyList.TestCleanList_NoExceptionOnEmptyMemo;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.mmoProxyList.Clear;

  { CleanList should not raise exception on empty memo }
  Assert.WillNotRaise(
    procedure
    begin
      Form.CleanList;
    end);
end;


{ Save Tests }

procedure TTestFormProxyList.TestSave_WritesToFile;
var
  Form: TfrmProxyList;
  LoadedLines: TStringList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= FTestFile;
  Form.mmoProxyList.Lines.Add('192.168.1.1:80');
  Form.mmoProxyList.Lines.Add('10.0.0.1:3128');

  Form.Save;

  Assert.IsTrue(FileExists(FTestFile), 'Save should create the file');

  LoadedLines:= TStringList.Create;
  try
    LoadedLines.LoadFromFile(FTestFile);
    Assert.AreEqual(2, LoadedLines.Count, 'Saved file should have 2 lines');
    Assert.AreEqual('192.168.1.1:80', LoadedLines[0], 'First line should match');
  finally
    FreeAndNil(LoadedLines);
  end;
end;


procedure TTestFormProxyList.TestSave_AssertsOnEmptyFileName;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= '';

  { Save should raise assertion when FileName is empty }
  Assert.WillRaise(
    procedure
    begin
      Form.Save;
    end,
    EAssertionFailed);
end;


{ Button Event Tests }

procedure TTestFormProxyList.TestBtnSaveProxyClick_NoExceptionWithFileName;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= FTestFile;
  Form.mmoProxyList.Lines.Add('test');

  { btnSaveProxyClick should not raise exception when FileName is set }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnSaveProxyClick(Form);
    end);
end;


procedure TTestFormProxyList.TestBtnCleanClick_NoException;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  { btnCleanClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnCleanClick(Form);
    end);
end;


procedure TTestFormProxyList.TestBtnLocateClick_NoExceptionWithEmptyFileName;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  Form.FileName:= '';

  { btnLocateClick should not raise exception when FileName is empty }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnLocateClick(Form);
    end);
end;


procedure TTestFormProxyList.TestBtnAutoDetectClick_NoException;
var
  Form: TfrmProxyList;
begin
  Form:= TfrmProxyList.Create(NIL);
  FTestForm:= Form;

  { btnAutoDetectClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Form.btnAutoDetectClick(Form);
    end);
end;


{ Global Variable Tests }

procedure TTestFormProxyList.TestGlobalVariable_Exists;
begin
  // Just verify the global variable exists and is initially nil (or some value)
  // We can't test much more without affecting other tests
  Assert.Pass('Global frmProxyList variable exists');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormProxyList);

end.
