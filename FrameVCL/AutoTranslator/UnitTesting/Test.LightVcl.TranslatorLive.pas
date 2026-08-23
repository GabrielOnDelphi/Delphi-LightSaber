UNIT Test.LightVcl.TranslatorLive;

{=============================================================================================================
   2026.08.22
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   LIVE tests for LightVcl.TranslatorAPI.pas

   These call the REAL DeepL server over the network and spend characters from the account quota. That is why
   they sit in their own project (Tests_LightVcl.TranslatorLive) instead of in Tests_LightVcl.Translator:
   the normal suite has to stay offline, fast and repeatable, and it must never depend on a key.

   NOTHING here runs until a key is supplied. Put it in either place:
     - environment variable   DEEPL_API_KEY
     - text file              %APPDATA%\LightSaber\DeepL.key      (the key on the first line, nothing else)

   With no key, every test SKIPS with a message saying where to put one. It never fails for a missing key.

   The key is read only from outside the repository, so it cannot be committed by accident.

   Free and Pro keys use different servers, and a key for one is rejected by the other. A DeepL API Free key
   carries the suffix ':fx' (https://developers.deepl.com/docs/getting-started/auth), so the tier is detected
   from the key itself and no second setting has to be kept in sync.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  LightVcl.TranslatorAPI;

TYPE
  [TestFixture]
  TTestTranslatorLive = class
  private
    FTestFolder: string;
    FSourceINI : string;
    FTargetINI : string;
    FApiKey    : string;
    procedure CreateSourceINI;
    function  NewTranslator: TDeepLTranslator;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestConnection_RealServer;

    [Test]
    procedure TestTranslateText_EnglishToGerman;

    [Test]
    procedure TestTranslateINIFile_TranslatesValuesKeepsKeys;
  end;


IMPLEMENTATION

USES
  System.SysUtils, System.IOUtils, System.IniFiles,
  LightCore.TextFile,
  LightCore.IO;

CONST
   SkipMsg = 'SKIPPED - no DeepL key. Set the DEEPL_API_KEY environment variable, or put the key in %APPDATA%\LightSaber\DeepL.key';


{ Reads the key from outside the repository only. Returns '' when there is none. }
function ReadApiKey: string;
VAR
   KeyFile: string;
begin
  Result:= Trim(GetEnvironmentVariable('DEEPL_API_KEY'));
  if Result <> '' then EXIT;

  KeyFile:= GetEnvironmentVariable('APPDATA')+ '\LightSaber\DeepL.key';
  if FileExists(KeyFile)
  then Result:= Trim(StringFromFile(KeyFile));
end;


{ Builds a translator already pointed at the right server for this key. }
function TTestTranslatorLive.NewTranslator: TDeepLTranslator;
begin
  Result:= TDeepLTranslator.Create;
  Result.ApiKey:= FApiKey;
  Result.UseFreeAPI:= Copy(FApiKey, Length(FApiKey)-2, 3) = ':fx';
end;


procedure TTestTranslatorLive.Setup;
begin
  FApiKey:= ReadApiKey;

  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'LightSaber_TranslatorLive');
  ForceDirectories(FTestFolder);
  FSourceINI:= TPath.Combine(FTestFolder, 'English.ini');
  FTargetINI:= TPath.Combine(FTestFolder, 'German.ini');
end;


procedure TTestTranslatorLive.TearDown;
begin
  if DirectoryExists(FTestFolder)
  then DeleteFolder(FTestFolder);
end;


procedure TTestTranslatorLive.CreateSourceINI;
VAR
   Content: string;
begin
  Content:=
    '[Authors]'                        + sLineBreak +
    'Name=Test'                        + sLineBreak +
                                         sLineBreak +
    '[MainForm]'                       + sLineBreak +
    'btnOK.Caption=OK'                 + sLineBreak +
    'btnCancel.Caption=Cancel'         + sLineBreak +
    'lblTitle.Caption=Hello World'     + sLineBreak;

  StringToFile(FSourceINI, Content);
end;


procedure TTestTranslatorLive.TestConnection_RealServer;
VAR
   Translator: TDeepLTranslator;
begin
  if FApiKey = '' then
    begin
      Assert.Pass(SkipMsg);
      EXIT;
    end;

  Translator:= NewTranslator;
  TRY
    Assert.IsTrue(Translator.TestConnection, 'DeepL refused the connection. LastError: '+ Translator.LastError);
    Assert.AreEqual('', Translator.LastError, 'A successful connection must leave LastError empty');
  FINALLY
    FreeAndNil(Translator);
  END;
end;


procedure TTestTranslatorLive.TestTranslateText_EnglishToGerman;
VAR
   Translator: TDeepLTranslator;
   Translated: string;
begin
  if FApiKey = '' then
    begin
      Assert.Pass(SkipMsg);
      EXIT;
    end;

  Translator:= NewTranslator;
  TRY
    Translated:= Translator.TranslateText('Hello', 'DE');

    Assert.IsNotEmpty(Translated, 'Nothing came back. LastError: '+ Translator.LastError);
    Assert.AreNotEqual('Hello', Translated, 'DeepL handed back the English word unchanged - nothing was translated');
  FINALLY
    FreeAndNil(Translator);
  END;
end;


{ The one the offline suite could never cover: a real INI in, a translated INI out. }
procedure TTestTranslatorLive.TestTranslateINIFile_TranslatesValuesKeepsKeys;
VAR
   Translator: TDeepLTranslator;
   Ini: TIniFile;
   Title: string;
begin
  if FApiKey = '' then
    begin
      Assert.Pass(SkipMsg);
      EXIT;
    end;

  CreateSourceINI;
  Assert.IsTrue(FileExists(FSourceINI), 'Setup failed: the source INI was not written');

  Translator:= NewTranslator;
  TRY
    Translator.TranslateINIFile(FSourceINI, FTargetINI, 'DE');
    Assert.AreEqual('', Translator.LastError, 'TranslateINIFile reported an error');
    Assert.IsTrue(FileExists(FTargetINI), 'No target INI was produced');
  FINALLY
    FreeAndNil(Translator);
  END;

  Ini:= TIniFile.Create(FTargetINI);
  TRY
    { The keys must survive - only the values get translated }
    Assert.IsTrue(Ini.ValueExists('MainForm', 'btnOK.Caption'),     'Key btnOK.Caption was lost');
    Assert.IsTrue(Ini.ValueExists('MainForm', 'btnCancel.Caption'), 'Key btnCancel.Caption was lost');
    Assert.IsTrue(Ini.ValueExists('MainForm', 'lblTitle.Caption'),  'Key lblTitle.Caption was lost');

    { 'OK' is the same word in German, so only the sentence is worth checking for change }
    Title:= Ini.ReadString('MainForm', 'lblTitle.Caption', '');
    Assert.IsNotEmpty(Title, 'lblTitle.Caption came back empty');
    Assert.AreNotEqual('Hello World', Title, 'lblTitle.Caption was copied over untranslated');
  FINALLY
    FreeAndNil(Ini);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestTranslatorLive);

end.
