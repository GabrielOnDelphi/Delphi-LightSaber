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
  Vcl.ExtCtrls,
  Vcl.Forms,
  LightVcl.Visual.PathEdit;

type
  [TestFixture]
  TTesTlightPathEdit = class
  private
    FForm: TForm;
    FTestFolder: string;
    FTestFile: string;
    function ChildControl(Owner: TComponent; CONST aName: string): TControl;
    function EditBox(PathEdit: TlightPathEdit): TButtonedEdit;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
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
    procedure TestGetFiles_ListsMatchingFiles;
  end;

implementation

uses
  LightCore.Colors,
  LightCore.IO;


procedure TTesTlightPathEdit.Setup;
begin
  FForm:= TForm.CreateNew(nil);
  FForm.Width:= 800;
  FForm.Height:= 600;

  { Create a temp folder for testing }
  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'TestPathEdit_' + TGUID.NewGuid.ToString);
  ForceDirectoriesE(FTestFolder);

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


{ The buttons and the edit box are private fields; TlightPathEdit owns them and gives each a Name }
function TTesTlightPathEdit.ChildControl(Owner: TComponent; CONST aName: string): TControl;
begin
  Result:= Owner.FindComponent(aName) as TControl;
  Assert.IsNotNull(Result, 'Child control not found: ' + aName);
end;


function TTesTlightPathEdit.EditBox(PathEdit: TlightPathEdit): TButtonedEdit;
begin
  Result:= ChildControl(PathEdit, 'PathEdit') as TButtonedEdit;
end;


{ Constructor Tests }


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
    PathEdit.Path:= FTestFolder;
    Assert.AreEqual(FTestFolder + '\', PathEdit.Path, 'Precondition: the folder path must be set first');

    PathEdit.Path:= '';
    Assert.AreEqual('', PathEdit.Path, 'Setting an empty path must clear the path');
    Assert.AreEqual('', EditBox(PathEdit).Text, 'Setting an empty path must clear the edit box');
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
    Assert.AreEqual('The path is empty!', ErrMsg, 'Empty path must return the empty-path message');
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
    Assert.AreEqual('The path is empty!', ErrMsg, 'Empty path must return the empty-path message, not a "does not exist" one');
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
    PathEdit.Caption:= 'Folder';
    PathEdit.Path:= FTestFolder;
    Assert.AreEqual(clGreenWashed, EditBox(PathEdit).Color, 'Precondition: an existing folder must be green in folder mode');

    PathEdit.InputType:= itFile;
    Assert.AreEqual(Ord(itFile), Ord(PathEdit.InputType), 'InputType should be itFile');

    { The switch must re-check the path: a folder path is not an existing file }
    Assert.AreEqual(clRedFade, EditBox(PathEdit).Color, 'A folder path must turn red in file mode');
    Assert.AreEqual('Browse for a file', EditBox(PathEdit).RightButton.Hint, 'Browse button hint');
    Assert.AreEqual('Locate this file in Windows Explorer', ChildControl(PathEdit, 'ButtonExplore').Hint, 'Explore button hint');
    Assert.AreEqual('File', PathEdit.Caption, 'The default caption must follow the input type');
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
    PathEdit.Caption:= 'File';
    PathEdit.InputType:= itFile;
    PathEdit.Path:= FTestFile;
    Assert.AreEqual(clGreenWashed, EditBox(PathEdit).Color, 'Precondition: an existing file must be green in file mode');
    Assert.AreEqual('Browse for a file', EditBox(PathEdit).RightButton.Hint, 'Precondition: file-mode hint');

    PathEdit.InputType:= itFolder;
    Assert.AreEqual(Ord(itFolder), Ord(PathEdit.InputType), 'InputType should be itFolder');

    { The switch must re-check the path: a file path is not an existing folder }
    Assert.AreEqual(clRedFade, EditBox(PathEdit).Color, 'A file path must turn red in folder mode');
    Assert.AreEqual('Browse for a folder', EditBox(PathEdit).RightButton.Hint, 'Browse button hint');
    Assert.AreEqual('Locate this folder in Windows Explorer', ChildControl(PathEdit, 'ButtonExplore').Hint, 'Explore button hint');
    Assert.AreEqual('Folder', PathEdit.Caption, 'The default caption must follow the input type');
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
    PathEdit.ShowCreateBtn:= FALSE;   { Default is TRUE, so go through FALSE first }
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonCreate').Visible, 'Precondition: the Create button must be hidden');
    PathEdit.ShowCreateBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be TRUE');
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonCreate').Visible, 'The Create button must be visible');
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
    PathEdit.ShowCreateBtn:= TRUE;
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonCreate').Visible, 'Precondition: the Create button must be visible');
    PathEdit.ShowCreateBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowCreateBtn, 'ShowCreateBtn should be FALSE');
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonCreate').Visible, 'The Create button must be hidden');
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
    PathEdit.ShowOpenSrc:= FALSE;     { Default is TRUE, so go through FALSE first }
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonExplore').Visible, 'Precondition: the Explore button must be hidden');
    PathEdit.ShowOpenSrc:= TRUE;
    Assert.IsTrue(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be TRUE');
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonExplore').Visible, 'The Explore button must be visible');
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
    PathEdit.ShowOpenSrc:= TRUE;
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonExplore').Visible, 'Precondition: the Explore button must be visible');
    PathEdit.ShowOpenSrc:= FALSE;
    Assert.IsFalse(PathEdit.ShowOpenSrc, 'ShowOpenSrc should be FALSE');
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonExplore').Visible, 'The Explore button must be hidden');
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
    PathEdit.ShowApplyBtn:= FALSE;
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonApply').Visible, 'Precondition: the Apply button must be hidden');
    PathEdit.ShowApplyBtn:= TRUE;
    Assert.IsTrue(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be TRUE');
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonApply').Visible, 'The Apply button must be visible');
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
    PathEdit.ShowApplyBtn:= TRUE;     { Default is FALSE, so go through TRUE first }
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonApply').Visible, 'Precondition: the Apply button must be visible');
    PathEdit.ShowApplyBtn:= FALSE;
    Assert.IsFalse(PathEdit.ShowApplyBtn, 'ShowApplyBtn should be FALSE');
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonApply').Visible, 'The Apply button must be hidden');
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
    PathEdit.IsReadOnly:= TRUE;
    Assert.IsTrue(EditBox(PathEdit).ReadOnly, 'Precondition: the edit box must be read-only');
    PathEdit.IsReadOnly:= FALSE;
    Assert.IsFalse(PathEdit.IsReadOnly, 'IsReadOnly should be FALSE');
    Assert.IsFalse(EditBox(PathEdit).ReadOnly, 'The edit box must no longer be read-only');
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
    { SetEnabled must pass the state to the children; the VCL does not do that by itself }
    Assert.IsFalse(ChildControl(PathEdit, 'PathEdit').Enabled,       'The edit box must be disabled');
    Assert.IsFalse(ChildControl(PathEdit, 'ButtonExplore').Enabled,  'The Explore button must be disabled');
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
    Assert.IsTrue(ChildControl(PathEdit, 'PathEdit').Enabled,       'The edit box must be enabled again');
    Assert.IsTrue(ChildControl(PathEdit, 'ButtonExplore').Enabled,  'The Explore button must be enabled again');
  finally
    FreeAndNil(PathEdit);
  end;
end;


{ GetFiles Tests }

procedure TTesTlightPathEdit.TestGetFiles_ListsMatchingFiles;
var
  PathEdit: TlightPathEdit;
  Files: TStringList;
begin
  PathEdit:= TlightPathEdit.Create(FForm);
  PathEdit.Parent:= FForm;
  try
    PathEdit.Path:= FTestFolder;
    TFile.WriteAllText(TPath.Combine(FTestFolder, 'Other.dat'), 'x');   { Must be filtered out by the mask }
    Files:= PathEdit.GetFiles('*.txt', True, False, nil);
    try
      Assert.AreEqual(1, Files.Count, 'Only the one .txt file must be listed');
      Assert.AreEqual(FTestFile, Files[0], 'GetFiles must return the full path of the file in Path');
    finally
      FreeAndNil(Files);
    end;
  finally
    FreeAndNil(PathEdit);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTesTlightPathEdit);

end.
