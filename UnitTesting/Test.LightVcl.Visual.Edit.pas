unit Test.LightVcl.Visual.Edit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.Edit.pas
   Tests TLightEdit custom edit control functionality.

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
  LightCore.Colors;

type
  [TestFixture]
  TTesTLightEdit = class
  private
    FEdit: TLightEdit;
    FForm: TForm;
    FOnChangeCount: Integer;
    FOnPressEnterCount: Integer;
    FKeyPressed: Char;
    procedure OnChangeHandler(Sender: TObject);
    procedure OnPressEnterHandler(Sender: TObject);
    procedure OnKeyPressHandler(Sender: TObject; var Key: Char);
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
    procedure TestUpdateBkgColor_DirCheckTakesPrecedenceIfBothEnabled;
  end;

implementation

uses
  Winapi.Windows,      { VK_RETURN }
  Winapi.Messages;     { WM_CHAR }

type
  TLightEditAccess = class(TLightEdit);   { Opens the protected KeyPress }


procedure TTesTLightEdit.Setup;
begin
  FOnChangeCount:= 0;
  FOnPressEnterCount:= 0;

  FForm:= TForm.Create(nil);
  FForm.Width:= 400;
  FForm.Height:= 300;

  FEdit:= TLightEdit.Create(FForm);
  FEdit.Parent:= FForm;
  FEdit.Left:= 10;
  FEdit.Top:= 10;
  FEdit.Width:= 200;
end;


procedure TTesTLightEdit.TearDown;
begin
  FreeAndNil(FEdit);
  FreeAndNil(FForm);
end;


procedure TTesTLightEdit.OnChangeHandler(Sender: TObject);
begin
  Inc(FOnChangeCount);
end;


procedure TTesTLightEdit.OnPressEnterHandler(Sender: TObject);
begin
  Inc(FOnPressEnterCount);
end;


procedure TTesTLightEdit.OnKeyPressHandler(Sender: TObject; var Key: Char);
begin
  FKeyPressed:= Key;
end;


{ Constructor Tests }

procedure TTesTLightEdit.TestCreate_DefaultProperties;
begin
  Assert.IsFalse(FEdit.CheckFileExistence, 'CheckFileExistence should default to FALSE');
  Assert.IsFalse(FEdit.CheckDirExistence, 'CheckDirExistence should default to FALSE');
  Assert.IsFalse(Assigned(FEdit.OnPressEnter), 'OnPressEnter should default to nil');
end;


{ SetTextNoEvent Tests }

procedure TTesTLightEdit.TestSetTextNoEvent_DoesNotFireOnChange;
begin
  FEdit.OnChange:= OnChangeHandler;
  FOnChangeCount:= 0;

  FEdit.SetTextNoEvent('Test text');

  Assert.AreEqual(0, FOnChangeCount, 'OnChange should NOT fire when using SetTextNoEvent');
end;


procedure TTesTLightEdit.TestSetTextNoEvent_SetsTextCorrectly;
begin
  FEdit.SetTextNoEvent('Hello World');

  Assert.AreEqual('Hello World', FEdit.Text, 'Text should be set correctly');
end;


procedure TTesTLightEdit.TestSetTextNoEvent_RestoresOnChangeHandler;
begin
  FEdit.OnChange:= OnChangeHandler;

  FEdit.SetTextNoEvent('Test');

  // Now set Text normally - should fire OnChange
  FOnChangeCount:= 0;
  FEdit.Text:= 'Changed';

  Assert.AreEqual(1, FOnChangeCount, 'OnChange handler should be restored after SetTextNoEvent');
end;


{ CheckFileExistence Tests }

procedure TTesTLightEdit.TestCheckFileExistence_ExistingFile_WindowColor;
VAR
  NewFile: string;
  Stream: TFileStream;
begin
  { A file that does not exist yet: the control must turn red first, so a do-nothing UpdateBkgColor cannot pass }
  NewFile:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'LightEdit_' + IntToStr(GetCurrentProcessId) + '_' + IntToStr(GetTickCount) + '.tmp';
  Assert.IsFalse(FileExists(NewFile), 'Test prerequisite: the file must not exist yet: ' + NewFile);

  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= NewFile;
  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Precondition: a missing file paints the control red');

  { Now the same text names an existing file }
  Stream:= TFileStream.Create(NewFile, fmCreate);
  FreeAndNil(Stream);
  try
    Assert.IsTrue(FileExists(NewFile), 'Test prerequisite: the file must exist now');
    FEdit.UpdateBkgColor;
    Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color must go back to clWindow once the file exists');
  finally
    System.SysUtils.DeleteFile(NewFile);
  end;
end;


procedure TTesTLightEdit.TestCheckFileExistence_NonExistingFile_RedColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= 'C:\This\File\Does\Not\Exist\12345.xyz';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be clRedFade for non-existing file');
end;


procedure TTesTLightEdit.TestCheckFileExistence_EmptyText_WindowColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= '';

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for empty text');
end;


procedure TTesTLightEdit.TestCheckFileExistence_Disabled_NoColorChange;
VAR
  OriginalColor: TColor;
begin
  OriginalColor:= FEdit.Color;
  FEdit.CheckFileExistence:= FALSE;
  FEdit.Text:= 'C:\NonExistent\File.txt';

  Assert.AreEqual(Integer(OriginalColor), Integer(FEdit.Color), 'Color should not change when CheckFileExistence is FALSE');
end;


{ CheckDirExistence Tests }

procedure TTesTLightEdit.TestCheckDirExistence_ExistingDir_WindowColor;
VAR
  ExistingDir: string;
begin
  { The Windows folder, wherever this PC has it }
  ExistingDir:= GetEnvironmentVariable('WINDIR');
  Assert.IsTrue((ExistingDir <> '') AND DirectoryExists(ExistingDir), 'Test prerequisite: the WINDIR folder must exist: ' + ExistingDir);

  { A missing folder first: the control must turn red, so a do-nothing UpdateBkgColor cannot pass }
  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= 'C:\This\Directory\Does\Not\Exist\12345';
  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Precondition: a missing folder paints the control red');

  FEdit.Text:= ExistingDir;

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color must go back to clWindow for an existing directory');
end;


procedure TTesTLightEdit.TestCheckDirExistence_NonExistingDir_RedColor;
begin
  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= 'C:\This\Directory\Does\Not\Exist\12345';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be clRedFade for non-existing directory');
end;


procedure TTesTLightEdit.TestCheckDirExistence_EmptyText_WindowColor;
begin
  FEdit.CheckDirExistence:= TRUE;
  FEdit.Text:= '';

  Assert.AreEqual(Integer(clWindow), Integer(FEdit.Color), 'Color should be clWindow for empty text');
end;


procedure TTesTLightEdit.TestCheckDirExistence_Disabled_NoColorChange;
VAR
  OriginalColor: TColor;
begin
  OriginalColor:= FEdit.Color;
  FEdit.CheckDirExistence:= FALSE;
  FEdit.Text:= 'C:\NonExistent\Directory';

  Assert.AreEqual(Integer(OriginalColor), Integer(FEdit.Color), 'Color should not change when CheckDirExistence is FALSE');
end;


{ OnPressEnter Tests }

procedure TTesTLightEdit.TestOnPressEnter_EnterKeyFires;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= OnPressEnterHandler;
  FOnPressEnterCount:= 0;

  Key:= Char(VK_RETURN);
  FEdit.Perform(WM_CHAR, Ord(Key), 0);

  Assert.AreEqual(1, FOnPressEnterCount, 'OnPressEnter should fire when Enter key is pressed');
end;


procedure TTesTLightEdit.TestOnPressEnter_OtherKeyDoesNotFire;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= OnPressEnterHandler;
  FOnPressEnterCount:= 0;

  Key:= 'A';
  FEdit.Perform(WM_CHAR, Ord(Key), 0);

  Assert.AreEqual(0, FOnPressEnterCount, 'OnPressEnter should NOT fire for non-Enter keys');
end;


procedure TTesTLightEdit.TestOnPressEnter_NotAssignedDoesNotCrash;
VAR
  Key: Char;
begin
  FEdit.OnPressEnter:= NIL;
  FEdit.OnKeyPress:= OnKeyPressHandler;   { Records the key; it must not clear it, or the NIL test in KeyPress would never be reached }
  FKeyPressed:= #0;

  { KeyPress is called directly, so the key never reaches the Windows edit control }
  Key:= Char(VK_RETURN);
  Assert.WillNotRaiseAny(
    procedure
    begin
      TLightEditAccess(FEdit).KeyPress(Key);
    end,
    'Enter must not raise when OnPressEnter is not assigned');

  Assert.AreEqual(Ord(Char(VK_RETURN)), Ord(FKeyPressed), 'KeyPress must still pass Enter to the inherited OnKeyPress');
  Assert.AreEqual(Ord(Char(VK_RETURN)), Ord(Key), 'KeyPress must leave the key unchanged');
  Assert.AreEqual(0, FOnPressEnterCount, 'No OnPressEnter handler may run');

  { The same control still fires a handler assigned afterwards }
  FEdit.OnPressEnter:= OnPressEnterHandler;
  Key:= Char(VK_RETURN);
  TLightEditAccess(FEdit).KeyPress(Key);
  Assert.AreEqual(1, FOnPressEnterCount, 'OnPressEnter must fire once a handler is assigned');
end;


{ Change Tests }

procedure TTesTLightEdit.TestChange_FiresOnChange;
begin
  FEdit.OnChange:= OnChangeHandler;
  FOnChangeCount:= 0;

  FEdit.Text:= 'New text';

  Assert.AreEqual(1, FOnChangeCount, 'OnChange should fire when Text is changed');
end;


procedure TTesTLightEdit.TestChange_UpdatesBkgColor;
begin
  FEdit.CheckFileExistence:= TRUE;
  FEdit.Text:= 'C:\NonExistent\File.txt';

  Assert.AreEqual(Integer(clRedFade), Integer(FEdit.Color), 'Color should be updated when text changes');
end;


{ UpdateBkgColor Tests }

procedure TTesTLightEdit.TestUpdateBkgColor_BothDisabled_NoColorChange;
begin
  { A missing path and a color that neither branch sets: any check that runs would paint clRedFade }
  FEdit.CheckFileExistence:= FALSE;
  FEdit.CheckDirExistence:= FALSE;
  FEdit.Text:= 'C:\NonExistent\Folder\File.txt';
  FEdit.Color:= clYellow;

  FEdit.UpdateBkgColor;

  Assert.AreEqual(Integer(clYellow), Integer(FEdit.Color), 'Color should not change when both checks are disabled');
end;


procedure TTesTLightEdit.TestUpdateBkgColor_DirCheckTakesPrecedenceIfBothEnabled;
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
  TDUnitX.RegisterTestFixture(TTesTLightEdit);

end.
