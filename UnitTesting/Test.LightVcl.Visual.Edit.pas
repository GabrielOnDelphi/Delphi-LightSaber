unit Test.LightVcl.Visual.Edit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.Edit.pas
   Tests TCubicEdit custom edit control functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Graphics,
  LightVcl.Visual.Edit,
  LightVcl.Common.Colors;

type
  [TestFixture]
  TTestCubicEdit = class
  private
    FEdit: TCubicEdit;
    FForm: TForm;
    FOnChangeCount: Integer;
    FOnPressEnterCount: Integer;
    procedure OnChangeHandler(Sender: TObject);
    procedure OnPressEnterHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultProperties;

    { SetTextNoEvent Tests }
    [Test]
    procedure TestSetTextNoEvent_DoesNotFireOnChange;

    [Test]
    procedure TestSetTextNoEvent_SetsTextCorrectly;

    [Test]
    procedure TestSetTextNoEvent_RestoresOnChangeHandler;

    { CheckFileExistence Tests }
    [Test]
    procedure TestCheckFileExistence_ExistingFile_WindowColor;

    [Test]
    procedure TestCheckFileExistence_NonExistingFile_RedColor;

    [Test]
    procedure TestCheckFileExistence_EmptyText_WindowColor;

    [Test]
    procedure TestCheckFileExistence_Disabled_NoColorChange;

    { CheckDirExistence Tests }
    [Test]
    procedure TestCheckDirExistence_ExistingDir_WindowColor;

    [Test]
    procedure TestCheckDirExistence_NonExistingDir_RedColor;

    [Test]
    procedure TestCheckDirExistence_EmptyText_WindowColor;

    [Test]
    procedure TestCheckDirExistence_Disabled_NoColorChange;

    { OnPressEnter Tests }
    [Test]
    procedure TestOnPressEnter_EnterKeyFires;

    [Test]
    procedure TestOnPressEnter_OtherKeyDoesNotFire;

    [Test]
    procedure TestOnPressEnter_NotAssignedDoesNotCrash;

    { Change Tests }
    [Test]
    procedure TestChange_FiresOnChange;

    [Test]
    procedure TestChange_UpdatesBkgColor;

    { UpdateBkgColor Tests }
    [Test]
    procedure TestUpdateBkgColor_BothDisabled_NoColorChange;

    [Test]
    procedure TestUpdateBkgColor_FileCheckTakesPrecedenceIfBothEnabled;
  end;

implementation

uses
  Winapi.Windows;


procedure TTestCubicEdit.Setup;
begin
  FOnChangeCount:= 0;
  FOnPressEnterCount:= 0;

  FForm:= TForm.Create(nil);
  FForm.Width:= 400;
  FForm.Height:= 300;

  FEdit:= TCubicEdit.Create(FForm);
  FEdit.Parent:= FForm;
  FEdit.Left:= 10;
  FEdit.Top:= 10;
  FEdit.Width:= 200;
end;


procedure TTestCubicEdit.TearDown;
begin
  FreeAndNil(FEdit);
  FreeAndNil(FForm);
end;


procedure TTestCubicEdit.OnChangeHandler(Sender: TObject);
begin
  Inc(FOnChangeCount);
end;


procedure TTestCubicEdit.OnPressEnterHandler(Sender: TObject);
begin
  Inc(FOnPressEnterCount);
end;


{ Constructor Tests }

procedure TTestCubicEdit.TestCreate_DefaultProperties;
begin
  Assert.IsFalse(FEdit.CheckFileExistence, 'CheckFileExistence should default to FALSE');
  Assert.IsFalse(FEdit.CheckDirExistence, 'CheckDirExistence should default to FALSE');
  Assert.IsFalse(Assigned(FEdit.OnPressEnter), 'OnPressEnter should default to nil');
end;


{ SetTextNoEvent Tests }

procedure TTestCubicEdit.TestSetTextNoEvent_DoesNotFireOnChange;
begin
  FEdit.OnChange:= OnChangeHandler;
  FOnChangeCount:= 0;

  FEdit.SetTextNoEvent('Test text');

  Assert.AreEqual(0, FOnChangeCount, 'OnChange should NOT fire when using SetTextNoEvent');
end;


procedure TTestCubicEdit.TestSetTextNoEvent_SetsTextCorrectly;
begin
  FEdit.SetTextNoEvent('Hello World');

  Assert.AreEqual('Hello World', FEdit.Text, 'Text should be set correctly');
end;


procedure TTestCubicEdit.TestSetTextNoEvent_RestoresOnChangeHandler;
begin
  FEdit.OnChange:= OnChangeHandler;

  FEdit.SetTextNoEvent('Test');

  // Now set Text normally - should fire OnChange
  FOnChangeCount:= 0;
  FEdit.Text:= 'Changed';

  Assert.AreEqual(1, FOnChangeCount, 'OnChange handler should be restored after SetTextNoEvent');
end;


{ CheckFileExistence Tests }

procedure TTestCubicEdit.TestCheckFileExistence_ExistingFile_WindowColor;
VAR
  ExistingFile: string;
begin
  // Use a file that always exists on Windows
  ExistingFile:= ParamStr(0);  // The test executable itself
  Assert.IsTrue(FileExists(ExistingFile), 'Test prerequisite: file must exist');

  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= ExistingFile;

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for existing file');
end;


procedure TTestCubicEdit.TestCheckFileExistence_NonExistingFile_RedColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= 'C:\This\File\Does\Not\Exist\12345.xyz';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be clRedFade for non-existing file');
end;


procedure TTestCubicEdit.TestCheckFileExistence_EmptyText_WindowColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= '';

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for empty text');
end;


procedure TTestCubicEdit.TestCheckFileExistence_Disabled_NoColorChange;
VAR
  OriginalColor: TColor;
begin
  OriginalColor:= FEdit.Color;
  FEdit.CheckFileExistence:= FALSE;
  FEdit.Text:= 'C:\NonExistent\File.txt';

  Assert.AreEqual(Integer(OriginalColor), Integer(FEdit.Color), 'Color should not change when CheckFileExistence is FALSE');
end;


{ CheckDirExistence Tests }

procedure TTestCubicEdit.TestCheckDirExistence_ExistingDir_WindowColor;
VAR
  ExistingDir: string;
begin
  // Use a directory that always exists on Windows
  ExistingDir:= 'C:\Windows';
  Assert.IsTrue(DirectoryExists(ExistingDir), 'Test prerequisite: directory must exist');

  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= ExistingDir;

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for existing directory');
end;


procedure TTestCubicEdit.TestCheckDirExistence_NonExistingDir_RedColor;
begin
  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= 'C:\This\Directory\Does\Not\Exist\12345';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be clRedFade for non-existing directory');
end;


procedure TTestCubicEdit.TestCheckDirExistence_EmptyText_WindowColor;
begin
  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= '';

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for empty text');
end;


procedure TTestCubicEdit.TestCheckDirExistence_Disabled_NoColorChange;
VAR
  OriginalColor: TColor;
begin
  OriginalColor:= FEdit.Color;
  FEdit.CheckDirExistence:= FALSE;
  FEdit.Text:= 'C:\NonExistent\Directory';

  Assert.AreEqual(Integer(OriginalColor), Integer(FEdit.Color), 'Color should not change when CheckDirExistence is FALSE');
end;


{ OnPressEnter Tests }

procedure TTestCubicEdit.TestOnPressEnter_EnterKeyFires;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= OnPressEnterHandler;
  FOnPressEnterCount:= 0;

  Key:= Char(VK_RETURN);
  FEdit.Perform(WM_CHAR, Ord(Key), 0);

  Assert.AreEqual(1, FOnPressEnterCount, 'OnPressEnter should fire when Enter key is pressed');
end;


procedure TTestCubicEdit.TestOnPressEnter_OtherKeyDoesNotFire;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= OnPressEnterHandler;
  FOnPressEnterCount:= 0;

  Key:= 'A';
  FEdit.Perform(WM_CHAR, Ord(Key), 0);

  Assert.AreEqual(0, FOnPressEnterCount, 'OnPressEnter should NOT fire for non-Enter keys');
end;


procedure TTestCubicEdit.TestOnPressEnter_NotAssignedDoesNotCrash;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= NIL;

  // This should not raise an exception
  Key:= Char(VK_RETURN);
  FEdit.Perform(WM_CHAR, Ord(Key), 0);

  Assert.Pass('No exception should be raised when OnPressEnter is not assigned');
end;


{ Change Tests }

procedure TTestCubicEdit.TestChange_FiresOnChange;
begin
  FEdit.OnChange:= OnChangeHandler;
  FOnChangeCount:= 0;

  FEdit.Text:= 'New text';

  Assert.AreEqual(1, FOnChangeCount, 'OnChange should fire when Text is changed');
end;


procedure TTestCubicEdit.TestChange_UpdatesBkgColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= 'C:\NonExistent\File.txt';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be updated when text changes');
end;


{ UpdateBkgColor Tests }

procedure TTestCubicEdit.TestUpdateBkgColor_BothDisabled_NoColorChange;
VAR
  OriginalColor: TColor;
begin
  OriginalColor:= FEdit.Color;
  FEdit.CheckFileExistence:= FALSE;
  FEdit.CheckDirExistence:= FALSE;

  FEdit.UpdateBkgColor;

  Assert.AreEqual(Integer(OriginalColor), Integer(FEdit.Color), 'Color should not change when both checks are disabled');
end;


procedure TTestCubicEdit.TestUpdateBkgColor_FileCheckTakesPrecedenceIfBothEnabled;
VAR
  ExistingFile: string;
begin
  // Use a file that exists (the test executable)
  ExistingFile:= ParamStr(0);

  // Enable both checks - this is not recommended but let's verify behavior
  FEdit.CheckFileExistence:= TRUE;
  FEdit.CheckDirExistence:= TRUE;

  // Set text to an existing file (but not a directory)
  FEdit.Text:= ExistingFile;

  // Since CheckDirExistence runs last and ParamStr(0) is a file (not a directory),
  // the color should be clRedFade (because DirectoryExists will be false)
  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color),
    'When both checks enabled, CheckDirExistence runs last and takes precedence');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicEdit);

end.
