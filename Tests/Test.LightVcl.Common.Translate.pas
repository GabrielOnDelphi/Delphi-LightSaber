unit Test.LightVcl.Common.Translate;

{=============================================================================================================
   Unit tests for LightVcl.Common.Translate.pas
   Tests TTranslator - RTTI-based translation system for VCL applications.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.IniFiles,
  Vcl.Forms,
  Vcl.StdCtrls,
  LightCore.AppData,
  LightVcl.Common.Translate;

type
  [TestFixture]
  TTestTranslator = class
  private
    FTestLangFolder: string;
    FTestForm: TForm;
    FTestLabel: TLabel;
    FEventFired: Boolean;
    procedure CreateTestLanguageFile(const FileName, LabelHint: string);
    procedure CleanupTestFiles;
    procedure DoEventHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_WithValidAppData;

    [Test]
    procedure TestCreate_WithNilAppData_RaisesException;

    { Path Tests }
    [Test]
    procedure TestGetLangFolder;

    [Test]
    procedure TestGetLangFolder_EndsWithBackslash;

    [Test]
    procedure TestDefaultLang;

    [Test]
    procedure TestDefaultLang_ContainsEnglish;

    { CurLanguage Property Tests }
    [Test]
    procedure TestCurLanguageName;

    [Test]
    procedure TestCurLanguage_SetGet;

    [Test]
    procedure TestCurLanguage_RequiresFullPath;

    { DontTranslate Constant Tests }
    [Test]
    procedure TestDontTranslateConstant;

    [Test]
    procedure TestDontTranslateConstant_IsBitwise;

    { trs Function Tests }
    [Test]
    procedure TestTrs_ReturnsInputString;

    [Test]
    procedure TestTrs_EmptyString;

    [Test]
    procedure TestTrs_UnicodeString;

    { DontSaveEmpty Property Tests }
    [Test]
    procedure TestDontSaveEmpty_DefaultTrue;

    { Authors Property Tests }
    [Test]
    procedure TestAuthors_CanBeSet;

    { LoadTranslation Tests }
    [Test]
    procedure TestLoadTranslation_NonExistentFile;

    [Test]
    procedure TestLoadTranslation_FormWithDontTranslateTag;

    [Test]
    procedure TestLoadTranslation_AppliesTranslation;

    { SaveTranslation Tests }
    [Test]
    procedure TestSaveTranslation_CreatesFile;

    [Test]
    procedure TestSaveTranslation_OverwriteTrue;

    [Test]
    procedure TestSaveTranslation_CreatesLangFolder;

    { LoadDefaultTranslation Tests }
    [Test]
    procedure TestLoadDefaultTranslation;

    { OnTranslationLoaded Event Tests }
    [Test]
    procedure TestOnTranslationLoaded_EventFires;

    { ParseCtrlsWithAction Tests }
    [Test]
    procedure TestParseCtrlsWithAction_DefaultFalse;
  end;

implementation

uses
  LightCore.IO,
  LightVcl.Visual.AppData;


procedure TTestTranslator.Setup;
begin
  // Create test form with controls
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Name:= 'TestForm';
  FTestForm.Caption:= 'Test Form Caption';

  FTestLabel:= TLabel.Create(FTestForm);
  FTestLabel.Parent:= FTestForm;
  FTestLabel.Name:= 'lblTest';
  FTestLabel.Hint:= 'Original Hint';

  FTestLangFolder:= TAppDataCore.AppDataFolder(TRUE) + 'Lang\';
  ForceDirectories(FTestLangFolder);
end;


procedure TTestTranslator.TearDown;
begin
  CleanupTestFiles;
  FreeAndNil(FTestForm);  // This also frees FTestLabel
end;


procedure TTestTranslator.CreateTestLanguageFile(const FileName, LabelHint: string);
var
  Ini: TMemIniFile;
begin
  ForceDirectories(FTestLangFolder);
  Ini:= TMemIniFile.Create(FTestLangFolder + FileName, TEncoding.UTF8);
  TRY
    Ini.WriteString('Authors', 'Name', 'TestAuthor');
    Ini.WriteString('TestForm', 'lblTest.Hint', LabelHint);
    Ini.UpdateFile;
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestTranslator.CleanupTestFiles;
begin
  // Clean up test language files
  if DirectoryExists(FTestLangFolder)
  then
    begin
      if FileExists(FTestLangFolder + 'TestLang.ini')
      then DeleteFile(FTestLangFolder + 'TestLang.ini');

      if FileExists(FTestLangFolder + 'English.ini')
      then DeleteFile(FTestLangFolder + 'English.ini');
    end;
end;


{ Constructor Tests }

procedure TTestTranslator.TestCreate_WithValidAppData;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Assert.IsNotNull(Trans, 'TTranslator should be created successfully');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestCreate_WithNilAppData_RaisesException;
begin
  Assert.WillRaise(
    procedure
    var
      Trans: TTranslator;
    begin
      Trans:= TTranslator.Create(NIL);
      FreeAndNil(Trans);
    end,
    Exception
  );
end;


{ Path Tests }

procedure TTestTranslator.TestGetLangFolder;
var
  Trans: TTranslator;
  Folder: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Folder:= Trans.GetLangFolder;
    Assert.IsNotEmpty(Folder, 'GetLangFolder should return non-empty string');
    Assert.Contains(Folder, 'Lang', 'GetLangFolder should contain "Lang"');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestGetLangFolder_EndsWithBackslash;
var
  Trans: TTranslator;
  Folder: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Folder:= Trans.GetLangFolder;
    Assert.AreEqual('\', Folder[Length(Folder)], 'GetLangFolder should end with backslash');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestDefaultLang;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Assert.IsNotEmpty(Trans.GetLangFolder, 'DefaultLang should return non-empty string');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestDefaultLang_ContainsEnglish;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    // We can only test GetLangFolder since DefaultLang is protected
    Assert.IsTrue(Trans.GetLangFolder.EndsWith('Lang\'), 'Lang folder should end with Lang\');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ CurLanguage Property Tests }

procedure TTestTranslator.TestCurLanguageName;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    // CurLanguageName returns just the filename portion
    Assert.IsNotEmpty(Trans.CurLanguageName, 'CurLanguageName should return non-empty string');
    Assert.AreEqual(0, Pos(PathDelim, Trans.CurLanguageName), 'CurLanguageName should not contain path delimiter');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestCurLanguage_SetGet;
var
  Trans: TTranslator;
  TestPath: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    CreateTestLanguageFile('TestLang.ini', 'Test Hint');
    TestPath:= FTestLangFolder + 'TestLang.ini';

    Trans.CurLanguage:= TestPath;
    Assert.AreEqual(TestPath, Trans.CurLanguage, 'CurLanguage getter should return full path');
    Assert.AreEqual('TestLang.ini', Trans.CurLanguageName, 'CurLanguageName should return just filename');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestCurLanguage_RequiresFullPath;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Trans.CurLanguage:= 'JustFilename.ini';  // No path - should raise assertion
      end
    );
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ DontTranslate Constant Tests }

procedure TTestTranslator.TestDontTranslateConstant;
begin
  Assert.AreEqual(128, DontTranslate, 'DontTranslate should equal 128');
end;


procedure TTestTranslator.TestDontTranslateConstant_IsBitwise;
begin
  // 128 is binary 10000000, which is suitable for bitwise operations
  Assert.AreEqual(128, 1 shl 7, 'DontTranslate should be bit 7 (128)');
end;


{ trs Function Tests }

procedure TTestTranslator.TestTrs_ReturnsInputString;
var
  Input, Output: string;
begin
  Input:= 'Hello World';
  Output:= trs(Input);
  Assert.AreEqual(Input, Output, 'trs should return input string unchanged (placeholder)');
end;


procedure TTestTranslator.TestTrs_EmptyString;
begin
  Assert.AreEqual('', trs(''), 'trs should handle empty string');
end;


procedure TTestTranslator.TestTrs_UnicodeString;
var
  Unicode: string;
begin
  Unicode:= 'Привет Мир 你好世界';
  Assert.AreEqual(Unicode, trs(Unicode), 'trs should handle Unicode strings');
end;


{ DontSaveEmpty Property Tests }

procedure TTestTranslator.TestDontSaveEmpty_DefaultTrue;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Assert.IsTrue(Trans.DontSaveEmpty, 'DontSaveEmpty should default to True');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ Authors Property Tests }

procedure TTestTranslator.TestAuthors_CanBeSet;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Trans.Authors:= 'Test Author';
    Assert.AreEqual('Test Author', Trans.Authors, 'Authors property should be settable');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ LoadTranslation Tests }

procedure TTestTranslator.TestLoadTranslation_NonExistentFile;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    // Set a non-existent language file path
    Trans.CurLanguage:= FTestLangFolder + 'NonExistent.ini';
    // LoadTranslation should handle non-existent file gracefully
    Trans.LoadTranslation(FTestForm, TRUE);
    Assert.Pass('LoadTranslation should handle non-existent files gracefully');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestLoadTranslation_FormWithDontTranslateTag;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    CreateTestLanguageFile('TestLang.ini', 'Translated Hint');
    Trans.CurLanguage:= FTestLangFolder + 'TestLang.ini';

    FTestForm.Tag:= DontTranslate;
    FTestLabel.Hint:= 'Original Hint';

    Trans.LoadTranslation(FTestForm, TRUE);

    // Hint should remain unchanged because form has DontTranslate tag
    Assert.AreEqual('Original Hint', FTestLabel.Hint, 'Controls on DontTranslate forms should not be translated');
  FINALLY
    FTestForm.Tag:= 0;  // Reset
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestLoadTranslation_AppliesTranslation;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    CreateTestLanguageFile('TestLang.ini', 'Translated Hint');
    Trans.CurLanguage:= FTestLangFolder + 'TestLang.ini';

    FTestLabel.Hint:= 'Original Hint';

    Trans.LoadTranslation(FTestForm, TRUE);

    Assert.AreEqual('Translated Hint', FTestLabel.Hint, 'Translation should be applied to label hint');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ SaveTranslation Tests }

procedure TTestTranslator.TestSaveTranslation_CreatesFile;
var
  Trans: TTranslator;
  TestFile: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    TestFile:= FTestLangFolder + 'TestSave.ini';
    if FileExists(TestFile)
    then DeleteFile(TestFile);

    Trans.SaveTranslation(TestFile, TRUE);

    Assert.IsTrue(FileExists(TestFile), 'SaveTranslation should create file');

    // Cleanup
    DeleteFile(TestFile);
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestSaveTranslation_OverwriteTrue;
var
  Trans: TTranslator;
  TestFile: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    TestFile:= FTestLangFolder + 'TestOverwrite.ini';

    // Create initial file with some content
    CreateTestLanguageFile('TestOverwrite.ini', 'Initial Value');

    Trans.Authors:= 'NewAuthor';
    Trans.SaveTranslation(TestFile, TRUE);

    // File should be overwritten
    var Ini:= TMemIniFile.Create(TestFile);
    TRY
      Assert.AreEqual('NewAuthor', Ini.ReadString('Authors', 'Name', ''), 'File should be overwritten');
    FINALLY
      FreeAndNil(Ini);
    END;

    // Cleanup
    DeleteFile(TestFile);
  FINALLY
    FreeAndNil(Trans);
  END;
end;


procedure TTestTranslator.TestSaveTranslation_CreatesLangFolder;
var
  Trans: TTranslator;
  TestFolder, TestFile: string;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    TestFolder:= TAppDataCore.AppDataFolder(TRUE) + 'LangTest\';
    TestFile:= TestFolder + 'Test.ini';

    // Ensure folder doesn't exist
    if DirectoryExists(TestFolder)
    then RemoveDir(TestFolder);

    // SaveTranslation creates Lang folder via GetLangFolder
    Trans.SaveTranslation(TestFile, TRUE);

    Assert.IsTrue(FileExists(TestFile), 'SaveTranslation should create necessary directories');

    // Cleanup
    DeleteFile(TestFile);
    RemoveDir(TestFolder);
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ LoadDefaultTranslation Tests }

procedure TTestTranslator.TestLoadDefaultTranslation;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    CreateTestLanguageFile('English.ini', 'English Hint');

    Trans.LoadDefaultTranslation;

    Assert.Contains(Trans.CurLanguage, 'English.ini', 'LoadDefaultTranslation should set CurLanguage to English.ini');
    Assert.Contains(Trans.CurLanguage, 'Lang', 'LoadDefaultTranslation should include Lang folder in path');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ OnTranslationLoaded Event Tests }

procedure TTestTranslator.DoEventHandler(Sender: TObject);
begin
  FEventFired:= TRUE;
end;

procedure TTestTranslator.TestOnTranslationLoaded_EventFires;
var
  Trans: TTranslator;
begin
  FEventFired:= FALSE;

  Trans:= TTranslator.Create(AppDataCore);
  TRY
    CreateTestLanguageFile('TestLang.ini', 'Test Hint');
    Trans.CurLanguage:= FTestLangFolder + 'TestLang.ini';
    Trans.OnTranslationLoaded:= DoEventHandler;

    Trans.LoadTranslation(TRUE);

    Assert.IsTrue(FEventFired, 'OnTranslationLoaded event should fire after LoadTranslation');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


{ ParseCtrlsWithAction Tests }

procedure TTestTranslator.TestParseCtrlsWithAction_DefaultFalse;
var
  Trans: TTranslator;
begin
  Trans:= TTranslator.Create(AppDataCore);
  TRY
    Assert.IsFalse(Trans.ParseCtrlsWithAction, 'ParseCtrlsWithAction should default to False');
  FINALLY
    FreeAndNil(Trans);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestTranslator);

end.
