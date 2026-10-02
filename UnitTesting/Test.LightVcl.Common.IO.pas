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

    { File Operations Tests }
    [Test]
    procedure TestFileMoveTo;

    [Test]
    procedure TestFileMoveToDir;

    { Validation Helper Tests }
    [Test]
    procedure TestValidateForFileOperation_ControlPanel;

    [Test]
    procedure TestValidateForFileOperation_RecycleBin;

    { Parameter Validation Tests }
    [Test]
    procedure TestFileMoveTo_EmptyFrom_ShouldRaise;

    [Test]
    procedure TestFileMoveTo_EmptyTo_ShouldRaise;

    [Test]
    procedure TestFileMoveToDir_EmptyFrom_ShouldRaise;

    [Test]
    procedure TestFileMoveToDir_EmptyTo_ShouldRaise;

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

{ File Operations Tests }

procedure TTestVclCommonIO.TestFileMoveTo;
var
  SourceFile, DestFile: string;
  Result: Boolean;
begin
  SourceFile := TPath.Combine(FTestFolder, 'MoveSource.txt');
  DestFile := TPath.Combine(FTestFolder, 'MoveDest.txt');

  TFile.WriteAllText(SourceFile, 'Move test');

  { Explicitly qualified: LightCore.IO also declares a FileMoveTo with the identical
    signature: unqualified, Delphi's "last unit in uses wins" rule would silently resolve
    to LightCore.IO.FileMoveTo instead of the unit under test. }
  Result := LightVcl.Common.IO.FileMoveTo(SourceFile, DestFile);

  Assert.IsTrue(Result, 'FileMoveTo should succeed');
  Assert.IsFalse(FileExists(SourceFile), 'Source should not exist after move');
  Assert.IsTrue(FileExists(DestFile), 'Destination should exist after move');

  { Cleanup }
  LightCore.IO.TryDeleteFile(DestFile);
end;

procedure TTestVclCommonIO.TestFileMoveToDir;
var
  SourceFile, DestFolder, DestFile: string;
  Result: Boolean;
begin
  SourceFile := TPath.Combine(FTestFolder, 'MoveToDirSource.txt');
  DestFolder := TPath.Combine(FTestFolder, 'DestSubFolder');
  DestFile := TPath.Combine(DestFolder, 'MoveToDirSource.txt');

  TFile.WriteAllText(SourceFile, 'Move to dir test');
  ForceDirectoriesE(DestFolder);

  Result := LightVcl.Common.IO.FileMoveToDir(SourceFile, DestFolder, True);

  Assert.IsTrue(Result, 'FileMoveToDir should succeed');
  Assert.IsFalse(FileExists(SourceFile), 'Source should not exist after move');
  Assert.IsTrue(FileExists(DestFile), 'Destination should exist after move');

  { Cleanup }
  LightCore.IO.TryDeleteFile(DestFile);
  if DirectoryExists(DestFolder) then RemoveDir(DestFolder);
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

procedure TTestVclCommonIO.TestFileMoveTo_EmptyFrom_ShouldRaise;
begin
  { Explicitly qualified: see comment in TestFileMoveTo. LightCore.IO.FileMoveTo swallows
    exceptions and returns False instead of raising, so an unqualified call here would
    silently test the wrong unit and never raise. }
  Assert.WillRaise(
    procedure
    begin
      LightVcl.Common.IO.FileMoveTo('', 'C:\Temp\Test.txt');
    end,
    Exception);
end;


procedure TTestVclCommonIO.TestFileMoveTo_EmptyTo_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      LightVcl.Common.IO.FileMoveTo('C:\Temp\Test.txt', '');
    end,
    Exception);
end;


procedure TTestVclCommonIO.TestFileMoveToDir_EmptyFrom_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      LightVcl.Common.IO.FileMoveToDir('', 'C:\Temp', True);
    end,
    Exception);
end;


procedure TTestVclCommonIO.TestFileMoveToDir_EmptyTo_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    begin
      LightVcl.Common.IO.FileMoveToDir('C:\Temp\Test.txt', '', True);
    end,
    Exception);
end;


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
