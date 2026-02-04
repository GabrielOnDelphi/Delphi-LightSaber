unit Test.LightVcl.TranslatorAPI;

{=============================================================================================================
   Unit tests for LightVcl.Common.TranslatorAPI.pas
   Tests TTranslatorAPI base class and TDeepLTranslator implementation.

   Note: These tests focus on unit testing without making actual API calls.
   Integration tests with real API would require a valid API key.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils;

type
  [TestFixture]
  TTestTranslatorAPI = class
  private
    FTestFolder: string;
    FSourceINI: string;
    FTargetINI: string;
    procedure CreateTestINI;
    procedure CleanupTestFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Class Existence Tests }
    [Test]
    procedure TestTDeepLTranslatorClassExists;

    [Test]
    procedure TestTDeepLTranslatorCreate_Succeeds;

    { Property Tests }
    [Test]
    procedure TestApiKey_DefaultEmpty;

    [Test]
    procedure TestApiKey_CanBeSet;

    [Test]
    procedure TestUseFreeAPI_DefaultTrue;

    [Test]
    procedure TestUseFreeAPI_CanBeSet;

    [Test]
    procedure TestTimeout_HasDefaultValue;

    [Test]
    procedure TestTimeout_CanBeSet;

    [Test]
    procedure TestLastError_InitiallyEmpty;

    [Test]
    procedure TestProviderName_IsDeepL;

    { Language Mapping Tests }
    [Test]
    procedure TestLanguageNameToDeepL_English;

    [Test]
    procedure TestLanguageNameToDeepL_German;

    [Test]
    procedure TestLanguageNameToDeepL_French;

    [Test]
    procedure TestLanguageNameToDeepL_Spanish;

    [Test]
    procedure TestLanguageNameToDeepL_Chinese;

    [Test]
    procedure TestLanguageNameToDeepL_Romanian;

    [Test]
    procedure TestLanguageNameToDeepL_Unknown_ReturnsEmpty;

    [Test]
    procedure TestLanguageNameToDeepL_CaseInsensitive;

    [Test]
    procedure TestDeepLToLanguageName_EN;

    [Test]
    procedure TestDeepLToLanguageName_DE;

    [Test]
    procedure TestDeepLToLanguageName_Unknown_ReturnsEmpty;

    { GetSupportedLanguages Tests }
    [Test]
    procedure TestGetSupportedLanguages_NotEmpty;

    [Test]
    procedure TestGetSupportedLanguages_ContainsEnglish;

    [Test]
    procedure TestGetSupportedLanguages_ContainsGerman;

    [Test]
    procedure TestGetSupportedLanguages_AtLeast10Languages;

    { EstimateCharacters Tests }
    [Test]
    procedure TestEstimateCharacters_EmptyArray;

    [Test]
    procedure TestEstimateCharacters_SingleText;

    [Test]
    procedure TestEstimateCharacters_MultipleTexts;

    { GetLanguageCode Tests }
    [Test]
    procedure TestGetLanguageCode_ReturnsCorrectCode;

    [Test]
    procedure TestGetLanguageName_ReturnsCorrectName;

    { API Key Validation Tests }
    [Test]
    procedure TestTranslateText_NoApiKey_SetsError;

    [Test]
    procedure TestTranslateBatch_NoApiKey_SetsError;

    [Test]
    procedure TestTestConnection_NoApiKey_ReturnsFalse;

    { TranslateBatch Empty Input Tests }
    [Test]
    procedure TestTranslateBatch_EmptyArray_ReturnsEmpty;

    { INI Translation Tests (without actual API call) }
    [Test]
    procedure TestTranslateINIFile_SourceNotFound_SetsError;
  end;

implementation

uses
  LightVcl.TranslatorAPI,
  LightCore.TextFile;


procedure TTestTranslatorAPI.Setup;
begin
  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'TranslatorAPITest_' + IntToStr(Random(100000)));
  ForceDirectories(FTestFolder);
  FSourceINI:= TPath.Combine(FTestFolder, 'English.ini');
  FTargetINI:= TPath.Combine(FTestFolder, 'German.ini');
end;


procedure TTestTranslatorAPI.TearDown;
begin
  CleanupTestFiles;
end;


procedure TTestTranslatorAPI.CleanupTestFiles;
begin
  if DirectoryExists(FTestFolder)
  then TDirectory.Delete(FTestFolder, True);
end;


procedure TTestTranslatorAPI.CreateTestINI;
var
  Content: string;
begin
  Content:=
    '[Authors]' + sLineBreak +
    'Name=Test' + sLineBreak +
    sLineBreak +
    '[MainForm]' + sLineBreak +
    'btnOK.Caption=OK' + sLineBreak +
    'btnCancel.Caption=Cancel' + sLineBreak +
    'lblTitle.Caption=Hello World' + sLineBreak;

  StringToFile(FSourceINI, Content, woOverwrite);
end;


{ Class Existence Tests }

procedure TTestTranslatorAPI.TestTDeepLTranslatorClassExists;
begin
  Assert.IsNotNull(TDeepLTranslator, 'TDeepLTranslator class should exist');
end;


procedure TTestTranslatorAPI.TestTDeepLTranslatorCreate_Succeeds;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.IsNotNull(Translator, 'Create should succeed');
  finally
    FreeAndNil(Translator);
  end;
end;


{ Property Tests }

procedure TTestTranslatorAPI.TestApiKey_DefaultEmpty;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.AreEqual('', Translator.ApiKey, 'ApiKey should be empty by default');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestApiKey_CanBeSet;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= 'test-key-123';
    Assert.AreEqual('test-key-123', Translator.ApiKey, 'ApiKey should be settable');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestUseFreeAPI_DefaultTrue;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.IsTrue(Translator.UseFreeAPI, 'UseFreeAPI should default to True');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestUseFreeAPI_CanBeSet;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.UseFreeAPI:= FALSE;
    Assert.IsFalse(Translator.UseFreeAPI, 'UseFreeAPI should be settable to False');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestTimeout_HasDefaultValue;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.IsTrue(Translator.Timeout > 0, 'Timeout should have a positive default value');
    Assert.AreEqual(30000, Translator.Timeout, 'Default timeout should be 30000ms');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestTimeout_CanBeSet;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.Timeout:= 60000;
    Assert.AreEqual(60000, Translator.Timeout, 'Timeout should be settable');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestLastError_InitiallyEmpty;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.AreEqual('', Translator.LastError, 'LastError should be empty initially');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestProviderName_IsDeepL;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.AreEqual('DeepL', Translator.ProviderName, 'ProviderName should be DeepL');
  finally
    FreeAndNil(Translator);
  end;
end;


{ Language Mapping Tests }

procedure TTestTranslatorAPI.TestLanguageNameToDeepL_English;
begin
  Assert.AreEqual('EN', LanguageNameToDeepL('English'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_German;
begin
  Assert.AreEqual('DE', LanguageNameToDeepL('German'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_French;
begin
  Assert.AreEqual('FR', LanguageNameToDeepL('French'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_Spanish;
begin
  Assert.AreEqual('ES', LanguageNameToDeepL('Spanish'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_Chinese;
begin
  Assert.AreEqual('ZH', LanguageNameToDeepL('Chinese'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_Romanian;
begin
  Assert.AreEqual('RO', LanguageNameToDeepL('Romanian'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_Unknown_ReturnsEmpty;
begin
  Assert.AreEqual('', LanguageNameToDeepL('Klingon'));
end;


procedure TTestTranslatorAPI.TestLanguageNameToDeepL_CaseInsensitive;
begin
  Assert.AreEqual('EN', LanguageNameToDeepL('english'));
  Assert.AreEqual('DE', LanguageNameToDeepL('GERMAN'));
  Assert.AreEqual('FR', LanguageNameToDeepL('FrEnCh'));
end;


procedure TTestTranslatorAPI.TestDeepLToLanguageName_EN;
begin
  Assert.AreEqual('English', DeepLToLanguageName('EN'));
end;


procedure TTestTranslatorAPI.TestDeepLToLanguageName_DE;
begin
  Assert.AreEqual('German', DeepLToLanguageName('DE'));
end;


procedure TTestTranslatorAPI.TestDeepLToLanguageName_Unknown_ReturnsEmpty;
begin
  Assert.AreEqual('', DeepLToLanguageName('XX'));
end;


{ GetSupportedLanguages Tests }

procedure TTestTranslatorAPI.TestGetSupportedLanguages_NotEmpty;
var
  Languages: TArray<string>;
begin
  Languages:= GetSupportedLanguages;
  Assert.IsTrue(Length(Languages) > 0, 'Should return at least one language');
end;


procedure TTestTranslatorAPI.TestGetSupportedLanguages_ContainsEnglish;
var
  Languages: TArray<string>;
  Found: Boolean;
begin
  Languages:= GetSupportedLanguages;
  Found:= FALSE;
  for var Lang in Languages do
    if SameText(Lang, 'English')
    then Found:= TRUE;

  Assert.IsTrue(Found, 'Supported languages should contain English');
end;


procedure TTestTranslatorAPI.TestGetSupportedLanguages_ContainsGerman;
var
  Languages: TArray<string>;
  Found: Boolean;
begin
  Languages:= GetSupportedLanguages;
  Found:= FALSE;
  for var Lang in Languages do
    if SameText(Lang, 'German')
    then Found:= TRUE;

  Assert.IsTrue(Found, 'Supported languages should contain German');
end;


procedure TTestTranslatorAPI.TestGetSupportedLanguages_AtLeast10Languages;
var
  Languages: TArray<string>;
begin
  Languages:= GetSupportedLanguages;
  Assert.IsTrue(Length(Languages) >= 10, 'Should support at least 10 languages');
end;


{ EstimateCharacters Tests }

procedure TTestTranslatorAPI.TestEstimateCharacters_EmptyArray;
var
  Translator: TDeepLTranslator;
  Texts: TArray<string>;
begin
  Translator:= TDeepLTranslator.Create;
  try
    SetLength(Texts, 0);
    Assert.AreEqual(0, Translator.EstimateCharacters(Texts));
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestEstimateCharacters_SingleText;
var
  Translator: TDeepLTranslator;
  Texts: TArray<string>;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Texts:= ['Hello'];
    Assert.AreEqual(5, Translator.EstimateCharacters(Texts));
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestEstimateCharacters_MultipleTexts;
var
  Translator: TDeepLTranslator;
  Texts: TArray<string>;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Texts:= ['Hello', 'World', '!'];
    Assert.AreEqual(11, Translator.EstimateCharacters(Texts));
  finally
    FreeAndNil(Translator);
  end;
end;


{ GetLanguageCode Tests }

procedure TTestTranslatorAPI.TestGetLanguageCode_ReturnsCorrectCode;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.AreEqual('DE', Translator.GetLanguageCode('German'));
    Assert.AreEqual('FR', Translator.GetLanguageCode('French'));
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestGetLanguageName_ReturnsCorrectName;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Assert.AreEqual('German', Translator.GetLanguageName('DE'));
    Assert.AreEqual('French', Translator.GetLanguageName('FR'));
  finally
    FreeAndNil(Translator);
  end;
end;


{ API Key Validation Tests }

procedure TTestTranslatorAPI.TestTranslateText_NoApiKey_SetsError;
var
  Translator: TDeepLTranslator;
  Result: string;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= '';
    Result:= Translator.TranslateText('Hello', 'DE');

    Assert.AreEqual('', Result, 'Result should be empty without API key');
    Assert.AreNotEqual('', Translator.LastError, 'LastError should be set');
    Assert.IsTrue(Pos('API key', Translator.LastError) > 0,
      'Error should mention API key');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestTranslateBatch_NoApiKey_SetsError;
var
  Translator: TDeepLTranslator;
  Texts: TArray<string>;
  Results: TArray<string>;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= '';
    Texts:= ['Hello', 'World'];
    Results:= Translator.TranslateBatch(Texts, 'DE');

    Assert.AreEqual(0, Length(Results), 'Results should be empty without API key');
    Assert.AreNotEqual('', Translator.LastError, 'LastError should be set');
  finally
    FreeAndNil(Translator);
  end;
end;


procedure TTestTranslatorAPI.TestTestConnection_NoApiKey_ReturnsFalse;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= '';
    Assert.IsFalse(Translator.TestConnection, 'TestConnection should fail without API key');
    Assert.AreNotEqual('', Translator.LastError, 'LastError should be set');
  finally
    FreeAndNil(Translator);
  end;
end;


{ TranslateBatch Empty Input Tests }

procedure TTestTranslatorAPI.TestTranslateBatch_EmptyArray_ReturnsEmpty;
var
  Translator: TDeepLTranslator;
  Texts: TArray<string>;
  Results: TArray<string>;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= 'test-key';
    SetLength(Texts, 0);
    Results:= Translator.TranslateBatch(Texts, 'DE');

    Assert.AreEqual(0, Length(Results), 'Empty input should return empty output');
    Assert.AreEqual('', Translator.LastError, 'No error for empty input');
  finally
    FreeAndNil(Translator);
  end;
end;


{ INI Translation Tests }

procedure TTestTranslatorAPI.TestTranslateINIFile_SourceNotFound_SetsError;
var
  Translator: TDeepLTranslator;
begin
  Translator:= TDeepLTranslator.Create;
  try
    Translator.ApiKey:= 'test-key';
    Translator.TranslateINIFile(
      TPath.Combine(FTestFolder, 'NonExistent.ini'),
      FTargetINI,
      'DE');

    Assert.AreNotEqual('', Translator.LastError, 'LastError should be set');
    Assert.IsTrue(Pos('not found', Translator.LastError) > 0,
      'Error should mention file not found');
  finally
    FreeAndNil(Translator);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestTranslatorAPI);

end.
