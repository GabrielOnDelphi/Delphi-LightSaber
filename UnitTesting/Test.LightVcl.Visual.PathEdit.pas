unit Test.LightVcl.Visual.PathEdit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.PathEdit.pas
   Tests the TlightPathEdit component - a file/folder path editor with browse functionality.

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
  TTesTlightPathEdit = class
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


procedure TTesTlightPathEdit.Setup;
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


procedure TTesTlightPathEdit.TearDown;
begin
  { Clean up test files/folders }
  if TFile.Exists(FTestFile)
  then TFile.Delete(FTestFile);

  if TDirectory.Exists(FTestFolder)
  then TDirectory.Delete(FTestFolder, True);

  FreeAndNil(FForm);
end;


{ Constructor Tests }

procedure TTesTlightPathEdit.TestCreate_NotNil;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.IsNotNull(PathEdit, 'Component should be created');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestCreate_DefaultInputType;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.AreEqual(Ord(itFolder), Ord(PathEdit.InputType), 'Default InputType should be itFolder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestCreate_DefaultShowCreateBtn;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.IsTrue(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be TRUE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestCreate_DefaultShowOpenSrc;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.IsTrue(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be TRUE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestCreate_DefaultShowApplyBtn;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.IsFalse(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be FALSE by default');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestCreate_DefaultDimensions;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  try
    Assert.AreEqual(41, PathEdit.Height, 'Default Height should be 41');
    Assert.AreEqual(350, PathEdit.Width, 'Default Width should be 350');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Path Property Tests - Folder Mode }

procedure TTesTlightPathEdit.TestPath_SetValidFolder;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    Assert.IsTrue(PathEdit.Path.StartsWith(FTestFolder), 'Path should contain the set folder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPath_SetInvalidFolder;
var
  PathEdit: TlightPathEdit;
  InvalidPath: string;
begin
  InvalidPath:= 'Z:\NonExistent\Path\That\Does\Not\Exist\';
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= InvalidPath;
    { Path should still be set even if folder doesn't exist }
    Assert.IsTrue(PathEdit.Path.Contains('NonExistent'), 'Path should be set even for invalid folder');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPath_EmptyString;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= '';
    Assert.AreEqual('', PathEdit.Path, 'Empty path should remain empty');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPath_TrailsFolder;
var
  PathEdit: TlightPathEdit;
  PathWithoutTrail: string;
begin
  PathWithoutTrail:= 'C:\TestFolder';
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestPath_FileMode_SetValidFile;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    PathEdit.Path:= FTestFile;
    Assert.AreEqual(FTestFile, PathEdit.Path, 'File path should be set exactly');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPath_FileMode_SetInvalidFile;
var
  PathEdit: TlightPathEdit;
  InvalidFile: string;
begin
  InvalidFile:= 'Z:\NonExistent\File.txt';
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestPathHasValidChars_EmptyPath;
var
  PathEdit: TlightPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= '';
    ErrMsg:= PathEdit.PathHasValidChars;
    Assert.IsTrue(ErrMsg <> '', 'Empty path should return error message');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPathHasValidChars_ValidPath;
var
  PathEdit: TlightPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    ErrMsg:= PathEdit.PathHasValidChars;
    Assert.AreEqual('', ErrMsg, 'Valid path should return empty string (no error)');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPathHasValidChars_TooLongPath;
var
  PathEdit: TlightPathEdit;
  LongPath: string;
  ErrMsg: string;
begin
  LongPath:= 'C:\' + StringOfChar('A', 300) + '\';
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestPathIsValid_ExistingFolder;
var
  PathEdit: TlightPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    ErrMsg:= PathEdit.PathIsValid;
    Assert.AreEqual('', ErrMsg, 'Existing folder should be valid');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPathIsValid_NonExistingFolder;
var
  PathEdit: TlightPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= 'C:\NonExistent_Folder_12345\';
    ErrMsg:= PathEdit.PathIsValid;
    Assert.IsTrue(ErrMsg.Contains('does not exist'), 'Non-existing folder should return error');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestPathIsValid_EmptyPath;
var
  PathEdit: TlightPathEdit;
  ErrMsg: string;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestInputType_SwitchToFile;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.InputType:= itFile;
    Assert.AreEqual(Ord(itFile), Ord(PathEdit.InputType), 'InputType should be itFile');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestInputType_SwitchToFolder;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestShowCreateBtn_True;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowCreateBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestShowCreateBtn_False;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowCreateBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestShowOpenSrc_True;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowOpenSrc:= TRUE;
    Assert.IsTrue(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestShowOpenSrc_False;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowOpenSrc:= FALSE;
    Assert.IsFalse(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestShowApplyBtn_True;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowApplyBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestShowApplyBtn_False;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.ShowApplyBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ IsReadOnly Tests }

procedure TTesTlightPathEdit.TestIsReadOnly_True;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.IsReadOnly:= TRUE;
    Assert.IsTrue(PathEdit.IsReadOnly, 'IsReadOnly should be TRUE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestIsReadOnly_False;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.IsReadOnly:= FALSE;
    Assert.IsFalse(PathEdit.IsReadOnly, 'IsReadOnly should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ Enabled Tests }

procedure TTesTlightPathEdit.TestEnabled_False;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Enabled:= FALSE;
    Assert.IsFalse(PathEdit.Enabled, 'Enabled should be FALSE');
  finally
    FreeAndNil(PathEdit);
  end;
end;


procedure TTesTlightPathEdit.TestEnabled_True;
var
  PathEdit: TlightPathEdit;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
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

procedure TTesTlightPathEdit.TestGetFiles_NoException;
var
  PathEdit: TlightPathEdit;
  Files: TStringList;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
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
  TDUnitX.RegisterTestFixture(TTesTlightPathEdit);

end.
