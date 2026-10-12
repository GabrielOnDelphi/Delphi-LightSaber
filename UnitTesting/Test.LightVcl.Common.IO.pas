unit Test.LightVcl.Common.IO;

{=============================================================================================================
   Unit tests for LightVcl.Common.IO
   Tests file/folder operations, and file dialogs
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,
  Winapi.ShellAPI;

type
  [TestFixture]
  TTestVclCommonIO = class
  private
    FTestFolder: string;
    FTestFile: string;
    procedure CleanupTestFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { File Age Tests }
    [Test]
    procedure TestFileAge_ExistingFile;

    [Test]
    procedure TestFileAge_NonExistentFile;

    { Validation Helper Tests }
    [Test]
    procedure TestValidateForFileOperation_ControlPanel;

    [Test]
    procedure TestValidateForFileOperation_RecycleBin;

    { Parameter Validation Tests }
    [Test]
    procedure TestMoveFolderMsg_EmptyFrom_ShouldRaise;

    [Test]
    procedure TestMoveFolderMsg_EmptyTo_ShouldRaise;

    [Test]
    procedure TestRecycleItem_EmptyItemName_ShouldRaise;
  end;

implementation

uses
  LightVcl.Common.IO,
  LightCore.IO;

procedure TTestVclCommonIO.Setup;
begin
  FTestFolder := TPath.Combine(TPath.GetTempPath, 'LightVclIOTest_' + IntToStr(Random(100000)));
  ForceDirectoriesE(FTestFolder);
  FTestFile := TPath.Combine(FTestFolder, 'TestFile.txt');
  TFile.WriteAllText(FTestFile, 'Test content');
end;

procedure TTestVclCommonIO.TearDown;
begin
  CleanupTestFiles;
end;

procedure TTestVclCommonIO.CleanupTestFiles;
begin
  LightCore.IO.TryDeleteFile(FTestFile);
  if DirectoryExists(FTestFolder) then
    TDirectory.Delete(FTestFolder, True);
end;

{ File Age Tests }

procedure TTestVclCommonIO.TestFileAge_ExistingFile;
var
  Age: TDateTime;
begin
  Age := FileAge(FTestFile);
  Assert.IsTrue(Age > 0, 'File age should be positive for existing file');
  Assert.IsTrue(Age <= Now, 'File age should not be in the future');
end;

procedure TTestVclCommonIO.TestFileAge_NonExistentFile;
var
  Age: TDateTime;
begin
  Age := FileAge(FTestFolder + '\NonExistent.txt');
  Assert.AreEqual(Double(-1), Double(Age), 0.0001, 'Non-existent file should return -1');
end;

{ Validation Helper Tests }

procedure TTestVclCommonIO.TestValidateForFileOperation_ControlPanel;
begin
  { FileOperation should fail for 'Control Panel' path }
  Assert.IsFalse(FileOperation('Control Panel', '', FO_DELETE, 0),
    'FileOperation should fail for Control Panel');
end;

procedure TTestVclCommonIO.TestValidateForFileOperation_RecycleBin;
begin
  { FileOperation should fail for 'Recycle Bin' path }
  Assert.IsFalse(FileOperation('Recycle Bin', '', FO_DELETE, 0),
    'FileOperation should fail for Recycle Bin');
end;


{ Parameter Validation Tests }

procedure TTestVclCommonIO.TestMoveFolderMsg_EmptyFrom_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      MoveFolderMsg('', 'C:\Temp', True);
    end,
    Exception);
end;


procedure TTestVclCommonIO.TestMoveFolderMsg_EmptyTo_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      MoveFolderMsg('C:\Temp', '', True);
    end,
    Exception);
end;


procedure TTestVclCommonIO.TestRecycleItem_EmptyItemName_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      RecycleItem('');
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestVclCommonIO);

end.
