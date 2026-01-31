unit Test.LightVcl.Visual.PathEdit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.PathEdit.pas
   Tests the TCubicPathEdit component - a file/folder path editor with browse functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms;

type
  [TestFixture]
  TTestCubicPathEdit = class
  private
    FForm: TForm;
    FTestFolder: string;
    FTestFile: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_NotNil;

    [Test]
    procedure TestCreate_DefaultInputType;

    [Test]
    procedure TestCreate_DefaultShowCreateBtn;

    [Test]
    procedure TestCreate_DefaultShowOpenSrc;

    [Test]
    procedure TestCreate_DefaultShowApplyBtn;

    [Test]
    procedure TestCreate_DefaultDimensions;

    { Path Property Tests - Folder Mode }
    [Test]
    procedure TestPath_SetValidFolder;

    [Test]
    procedure TestPath_SetInvalidFolder;

    [Test]
    procedure TestPath_EmptyString;

    [Test]
    procedure TestPath_TrailsFolder;

    { Path Property Tests - File Mode }
    [Test]
    procedure TestPath_FileMode_SetValidFile;

    [Test]
    procedure TestPath_FileMode_SetInvalidFile;

    { PathHasValidChars Tests }
    [Test]
    procedure TestPathHasValidChars_EmptyPath;

    [Test]
    procedure TestPathHasValidChars_ValidPath;

    [Test]
    procedure TestPathHasValidChars_TooLongPath;

    { PathIsValid Tests }
    [Test]
    procedure TestPathIsValid_ExistingFolder;

    [Test]
    procedure TestPathIsValid_NonExistingFolder;

    [Test]
    procedure TestPathIsValid_EmptyPath;

    { InputType Tests }
    [Test]
    procedure TestInputType_SwitchToFile;

    [Test]
    procedure TestInputType_SwitchToFolder;

    { Button Visibility Tests }
    [Test]
    procedure TestShowCreateBtn_True;

    [Test]
    procedure TestShowCreateBtn_False;

    [Test]
    procedure TestShowOpenSrc_True;

    [Test]
    procedure TestShowOpenSrc_False;

    [Test]
    procedure TestShowApplyBtn_True;

    [Test]
    procedure TestShowApplyBtn_False;

    { IsReadOnly Tests }
    [Test]
    procedure TestIsReadOnly_True;

    [Test]
    procedure TestIsReadOnly_False;

    { Enabled Tests }
    [Test]
    procedure TestEnabled_False;

    [Test]
    procedure TestEnabled_True;

    { GetFiles Tests }
    [Test]
    procedure TestGetFiles_NoException;
  end;

implementation

uses
  LightVcl.Visual.PathEdit,
  LightCore.IO;


procedure TTestCubicPathEdit.Setup;
begin
  FForm:= TForm.CreateNew(nil);
  FForm.Width:= 800;
  FForm.Height:= 600;

  { Create a temp folder for testing }
  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'TestPathEdit_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTestFolder);

  { Create a temp file for testing }
  FTestFile:= TPath.Combine(FTestFolder, 'TestFile.txt');
  TFile.WriteAllText(FTestFile, 'Test content');
end;


procedure TTestCubicPathEdit.TearDown;
begin
  { Clean up test files/folders }
  if TFile.Exists(FTestFile)
  then TFile.Delete(FTestFile);

  if TDirectory.Exists(FTestFolder)
  then TDirectory.Delete(FTestFolder, True);

  FreeAndNil(FForm);
end;


{ Constructor Tests }

procedure TTestCubicPathEdit.TestCreate_NotNil;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.IsNotNull(PathEdit, 'Component should be created');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestCreate_DefaultInputType;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.AreEqual(Ord(itFolder), Ord(PathEdit.InputType), 'Default InputType should be itFolder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestCreate_DefaultShowCreateBtn;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.IsTrue(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be TRUE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestCreate_DefaultShowOpenSrc;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.IsTrue(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be TRUE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestCreate_DefaultShowApplyBtn;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.IsFalse(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be FALSE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestCreate_DefaultDimensions;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  try
    Assert.AreEqual(41, PathEdit.Height, 'Default Height should be 41');
    Assert.AreEqual(350, PathEdit.Width, 'Default Width should be 350');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Path Property Tests - Folder Mode }

procedure TTestCubicPathEdit.TestPath_SetValidFolder;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    Assert.IsTrue(PathEdit.Path.StartsWith(FTestFolder), 'Path should contain the set folder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPath_SetInvalidFolder;
var
  PathEdit: TCubicPathEdit;
  InvalidPath: string;
begin
  InvalidPath:= 'Z:\NonExistent\Path\That\Does\Not\Exist\';
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= InvalidPath;
    { Path should still be set even if folder doesn't exist }
    Assert.IsTrue(PathEdit.Path.Contains('NonExistent'), 'Path should be set even for invalid folder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPath_EmptyString;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= '';
    Assert.AreEqual('', PathEdit.Path, 'Empty path should remain empty');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPath_TrailsFolder;
var
  PathEdit: TCubicPathEdit;
  PathWithoutTrail: string;
begin
  PathWithoutTrail:= 'C:\TestFolder';
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFolder;
    PathEdit.Path:= PathWithoutTrail;
    Assert.IsTrue(PathEdit.Path.EndsWith('\'), 'Folder path should be trailed');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Path Property Tests - File Mode }

procedure TTestCubicPathEdit.TestPath_FileMode_SetValidFile;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    PathEdit.Path:= FTestFile;
    Assert.AreEqual(FTestFile, PathEdit.Path, 'File path should be set exactly');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPath_FileMode_SetInvalidFile;
var
  PathEdit: TCubicPathEdit;
  InvalidFile: string;
begin
  InvalidFile:= 'Z:\NonExistent\File.txt';
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    PathEdit.Path:= InvalidFile;
    Assert.AreEqual(InvalidFile, PathEdit.Path, 'File path should be set even for non-existent file');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ PathHasValidChars Tests }

procedure TTestCubicPathEdit.TestPathHasValidChars_EmptyPath;
var
  PathEdit: TCubicPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= '';
    ErrMsg:= PathEdit.PathHasValidChars;
    Assert.IsTrue(ErrMsg <> '', 'Empty path should return error message');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPathHasValidChars_ValidPath;
var
  PathEdit: TCubicPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    ErrMsg:= PathEdit.PathHasValidChars;
    Assert.AreEqual('', ErrMsg, 'Valid path should return empty string (no error)');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPathHasValidChars_TooLongPath;
var
  PathEdit: TCubicPathEdit;
  LongPath: string;
  ErrMsg: string;
begin
  LongPath:= 'C:\' + StringOfChar('A', 300) + '\';
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= LongPath;
    ErrMsg:= PathEdit.PathHasValidChars;
    Assert.IsTrue(ErrMsg.Contains('too long'), 'Too long path should return error about length');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ PathIsValid Tests }

procedure TTestCubicPathEdit.TestPathIsValid_ExistingFolder;
var
  PathEdit: TCubicPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    ErrMsg:= PathEdit.PathIsValid;
    Assert.AreEqual('', ErrMsg, 'Existing folder should be valid');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPathIsValid_NonExistingFolder;
var
  PathEdit: TCubicPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= 'C:\NonExistent_Folder_12345\';
    ErrMsg:= PathEdit.PathIsValid;
    Assert.IsTrue(ErrMsg.Contains('does not exist'), 'Non-existing folder should return error');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestPathIsValid_EmptyPath;
var
  PathEdit: TCubicPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= '';
    ErrMsg:= PathEdit.PathIsValid;
    Assert.IsTrue(ErrMsg <> '', 'Empty path should return error');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ InputType Tests }

procedure TTestCubicPathEdit.TestInputType_SwitchToFile;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    Assert.AreEqual(Ord(itFile), Ord(PathEdit.InputType), 'InputType should be itFile');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestInputType_SwitchToFolder;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    PathEdit.InputType:= itFolder;
    Assert.AreEqual(Ord(itFolder), Ord(PathEdit.InputType), 'InputType should be itFolder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Button Visibility Tests }

procedure TTestCubicPathEdit.TestShowCreateBtn_True;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowCreateBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestShowCreateBtn_False;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowCreateBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestShowOpenSrc_True;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowOpenSrc:= TRUE;
    Assert.IsTrue(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestShowOpenSrc_False;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowOpenSrc:= FALSE;
    Assert.IsFalse(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestShowApplyBtn_True;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowApplyBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestShowApplyBtn_False;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowApplyBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ IsReadOnly Tests }

procedure TTestCubicPathEdit.TestIsReadOnly_True;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.IsReadOnly:= TRUE;
    Assert.IsTrue(PathEdit.IsReadOnly, 'IsReadOnly should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestIsReadOnly_False;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.IsReadOnly:= FALSE;
    Assert.IsFalse(PathEdit.IsReadOnly, 'IsReadOnly should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Enabled Tests }

procedure TTestCubicPathEdit.TestEnabled_False;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Enabled:= FALSE;
    Assert.IsFalse(PathEdit.Enabled, 'Enabled should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTestCubicPathEdit.TestEnabled_True;
var
  PathEdit: TCubicPathEdit;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Enabled:= FALSE;
    PathEdit.Enabled:= TRUE;
    Assert.IsTrue(PathEdit.Enabled, 'Enabled should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ GetFiles Tests }

procedure TTestCubicPathEdit.TestGetFiles_NoException;
var
  PathEdit: TCubicPathEdit;
  Files: TStringList;
begin
  PathEdit:= TCubicPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    Assert.WillNotRaise(
      procedure
      begin
        Files:= PathEdit.GetFiles('*.txt', True, False, nil);
        try
          { Just check it doesn't raise }
        finally
          FreeAndNil(Files);
        end;
      end);
  finally
    FreeAndNil(PathEdit);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicPathEdit);

end.
