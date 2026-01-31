UNIT LightVcl.Common.TranslatorAPI;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   AUTOMATIC TRANSLATION API CLIENT

   Provides automatic translation of text using external APIs (DeepL, Google, etc).
   Designed for integration with LightVcl.Common.Translate.pas translation system.

--------------------------------------------------------------------------------------------------------------
   SUPPORTED PROVIDERS

     * DeepL (implemented)
     * Google Translate (future)
     * OpenAI/GPT (future)
     * Other LLMs (future)

--------------------------------------------------------------------------------------------------------------
   ARCHITECTURE

     TTranslatorAPI (abstract base class)
         |
         +-- TDeepLTranslator (DeepL implementation)
         +-- TGoogleTranslator (future)
         +-- TOpenAITranslator (future)

--------------------------------------------------------------------------------------------------------------
   COST INFORMATION (as of January 2026)

     DeepL API Free:
       - Cost: Free
       - Limit: 500,000 characters/month
       - No next-gen models, lower priority
       - Endpoint: https://api-free.deepl.com

     DeepL API Pro:
       - Base fee: $5.49/month + $25 per 1,000,000 characters
       - Unlimited characters, next-gen models, priority processing
       - Endpoint: https://api.deepl.com

     Typical usage:
       - A translation INI file is ~500-2000 characters
       - Free tier allows ~250-1000 file translations per month

--------------------------------------------------------------------------------------------------------------
   USAGE

     var
       Translator: TDeepLTranslator;
     begin
       Translator:= TDeepLTranslator.Create;
       try
         Translator.ApiKey:= 'your-api-key';
         Translator.UseFreeAPI:= True;

         // Translate single text
         Result:= Translator.TranslateText('Hello', 'DE');

         // Translate batch
         Results:= Translator.TranslateBatch(['Hello', 'World'], 'DE');

         // Translate INI file values
         Translator.TranslateINIFile('English.ini', 'German.ini', 'DE');
       finally
         FreeAndNil(Translator);
       end;
     end;

--------------------------------------------------------------------------------------------------------------
   NOTES

     - API keys should be stored securely (not in source code)
     - DeepL language codes: EN, DE, FR, ES, IT, PT, NL, PL, RU, JA, ZH, etc.
     - Source language is auto-detected if not specified
     - Max 50 texts per batch request (DeepL limit)
     - Max 128 KiB request size (DeepL limit)

   See also:
     - https://developers.deepl.com/docs
     - https://developers.deepl.com/api-reference/translate
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.IniFiles, System.JSON, System.Math,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Generics.Collections;

TYPE
  { Progress event for batch translations }
  TTranslationProgressEvent = procedure(Sender: TObject; Current, Total: Integer; const Text: string) of object;

  { Base class for all translation API providers }
  TTranslatorAPI = class abstract
  protected
    FApiKey: string;
    FLastError: string;
    FTimeout: Integer;
    FOnProgress: TTranslationProgressEvent;
    function GetProviderName: string; virtual; abstract;
  public
    constructor Create; virtual;

    { Core translation methods - must be implemented by descendants }
    function TranslateText(const Text, TargetLang: string; const SourceLang: string = ''): string; virtual; abstract;
    function TranslateBatch(const Texts: TArray<string>; const TargetLang: string; const SourceLang: string = ''): TArray<string>; virtual; abstract;

    { Test connection to the API }
    function TestConnection: Boolean; virtual; abstract;

    { Utility methods }
    function GetLanguageCode(const LanguageName: string): string; virtual;
    function GetLanguageName(const LanguageCode: string): string; virtual;
    function EstimateCharacters(const Texts: TArray<string>): Integer;

    { INI file translation }
    procedure TranslateINIFile(const SourceFile, TargetFile, TargetLang: string; TranslateEmptyOnly: Boolean = FALSE);

    property ApiKey: string read FApiKey write FApiKey;
    property LastError: string read FLastError;
    property Timeout: Integer read FTimeout write FTimeout;  { Milliseconds }
    property ProviderName: string read GetProviderName;
    property OnProgress: TTranslationProgressEvent read FOnProgress write FOnProgress;
  end;


  { DeepL API implementation }
  TDeepLTranslator = class(TTranslatorAPI)
  private
    CONST
      API_URL_FREE = 'https://api-free.deepl.com/v2/translate';
      API_URL_PRO  = 'https://api.deepl.com/v2/translate';
      MAX_TEXTS_PER_REQUEST = 50;  { DeepL limit }
    var
      FUseFreeAPI: Boolean;
    function GetAPIURL: string;
    function DoHttpPost(const URL: string; const RequestBody: TStringStream): string;
    function ParseTranslationResponse(const JSONResponse: string): TArray<string>;
  protected
    function GetProviderName: string; override;
  public
    constructor Create; override;

    function TranslateText(const Text, TargetLang: string; const SourceLang: string = ''): string; override;
    function TranslateBatch(const Texts: TArray<string>; const TargetLang: string; const SourceLang: string = ''): TArray<string>; override;
    function TestConnection: Boolean; override;

    property UseFreeAPI: Boolean read FUseFreeAPI write FUseFreeAPI;
  end;


{ Language code mapping }
function LanguageNameToDeepL(const LanguageName: string): string;
function DeepLToLanguageName(const DeepLCode: string): string;

{ Supported languages list }
function GetSupportedLanguages: TArray<string>;


IMPLEMENTATION

USES
  LightCore, LightCore.IO, LightCore.TextFile;

TYPE
  { Language mapping record }
  TLanguageMap = record
    Name: string;      { Human-readable name (used in INI file names) }
    DeepLCode: string; { DeepL API language code }
  end;

CONST
  { Language code mappings - add more as needed }
  LanguageMappings: array[0..19] of TLanguageMap = (
    (Name: 'English';    DeepLCode: 'EN'),
    (Name: 'German';     DeepLCode: 'DE'),
    (Name: 'French';     DeepLCode: 'FR'),
    (Name: 'Spanish';    DeepLCode: 'ES'),
    (Name: 'Italian';    DeepLCode: 'IT'),
    (Name: 'Portuguese'; DeepLCode: 'PT'),
    (Name: 'Dutch';      DeepLCode: 'NL'),
    (Name: 'Polish';     DeepLCode: 'PL'),
    (Name: 'Russian';    DeepLCode: 'RU'),
    (Name: 'Japanese';   DeepLCode: 'JA'),
    (Name: 'Chinese';    DeepLCode: 'ZH'),
    (Name: 'Korean';     DeepLCode: 'KO'),
    (Name: 'Romanian';   DeepLCode: 'RO'),
    (Name: 'Bulgarian';  DeepLCode: 'BG'),
    (Name: 'Czech';      DeepLCode: 'CS'),
    (Name: 'Danish';     DeepLCode: 'DA'),
    (Name: 'Greek';      DeepLCode: 'EL'),
    (Name: 'Finnish';    DeepLCode: 'FI'),
    (Name: 'Hungarian';  DeepLCode: 'HU'),
    (Name: 'Swedish';    DeepLCode: 'SV')
  );


{-------------------------------------------------------------------------------------------------------------
   LANGUAGE MAPPING FUNCTIONS
-------------------------------------------------------------------------------------------------------------}

function LanguageNameToDeepL(const LanguageName: string): string;
begin
  Result:= '';
  for var Map in LanguageMappings do
    if SameText(Map.Name, LanguageName)
    then EXIT(Map.DeepLCode);
end;


function DeepLToLanguageName(const DeepLCode: string): string;
begin
  Result:= '';
  for var Map in LanguageMappings do
    if SameText(Map.DeepLCode, DeepLCode)
    then EXIT(Map.Name);
end;


function GetSupportedLanguages: TArray<string>;
begin
  SetLength(Result, Length(LanguageMappings));
  for var i:= 0 to High(LanguageMappings) do
    Result[i]:= LanguageMappings[i].Name;
end;



{-------------------------------------------------------------------------------------------------------------
   TTranslatorAPI - Base class
-------------------------------------------------------------------------------------------------------------}

constructor TTranslatorAPI.Create;
begin
  inherited Create;
  FTimeout:= 30000; { 30 seconds default }
  FLastError:= '';
end;


function TTranslatorAPI.GetLanguageCode(const LanguageName: string): string;
begin
  Result:= LanguageNameToDeepL(LanguageName);
end;


function TTranslatorAPI.GetLanguageName(const LanguageCode: string): string;
begin
  Result:= DeepLToLanguageName(LanguageCode);
end;


function TTranslatorAPI.EstimateCharacters(const Texts: TArray<string>): Integer;
begin
  Result:= 0;
  for var Text in Texts do
    Result:= Result + Length(Text);
end;


{ Translates all values in an INI file to a target language.
  SourceFile: Path to the source INI file (e.g., English.ini)
  TargetFile: Path to the target INI file (e.g., German.ini)
  TargetLang: DeepL language code (e.g., 'DE')
  TranslateEmptyOnly: If True, only translates keys that are empty or missing in target }
procedure TTranslatorAPI.TranslateINIFile(const SourceFile, TargetFile, TargetLang: string; TranslateEmptyOnly: Boolean);
var
  SourceIni, TargetIni: TMemIniFile;
  Sections: TStringList;
  Keys: TStringList;
  TextsToTranslate: TList<string>;
  KeysToTranslate: TList<TPair<string, string>>; { Section, Key pairs }
  Translations: TArray<string>;
  SourceValue, TargetValue: string;
  i: Integer;
begin
  FLastError:= '';
  Sections:= nil;
  Keys:= nil;
  TextsToTranslate:= nil;
  KeysToTranslate:= nil;
  SourceIni:= nil;
  TargetIni:= nil;

  if NOT FileExists(SourceFile) then
    begin
      FLastError:= 'Source file not found: ' + SourceFile;
      EXIT;
    end;

  try
    Sections:= TStringList.Create;
    Keys:= TStringList.Create;
    TextsToTranslate:= TList<string>.Create;
    KeysToTranslate:= TList<TPair<string, string>>.Create;

    SourceIni:= TMemIniFile.Create(SourceFile, TEncoding.UTF8);
    TargetIni:= TMemIniFile.Create(TargetFile, TEncoding.UTF8);

    { Collect all texts that need translation }
    SourceIni.ReadSections(Sections);
    for var Section in Sections do
      begin
        Keys.Clear;
        SourceIni.ReadSection(Section, Keys);
        for var Key in Keys do
          begin
            SourceValue:= SourceIni.ReadString(Section, Key, '');

            { Skip empty source values }
            if SourceValue.Trim.IsEmpty
            then CONTINUE;

            { If TranslateEmptyOnly, check if target already has a value }
            if TranslateEmptyOnly then
              begin
                TargetValue:= TargetIni.ReadString(Section, Key, '');
                if NOT TargetValue.Trim.IsEmpty
                then CONTINUE;
              end;

            { Add to translation queue }
            TextsToTranslate.Add(SourceValue);
            KeysToTranslate.Add(TPair<string, string>.Create(Section, Key));
          end;
      end;

    { Nothing to translate? }
    if TextsToTranslate.Count = 0 then
      begin
        TargetIni.UpdateFile;
        EXIT;
      end;

    { Translate in batches }
    Translations:= TranslateBatch(TextsToTranslate.ToArray, TargetLang);

    if Length(Translations) <> TextsToTranslate.Count then
      begin
        FLastError:= 'Translation count mismatch';
        EXIT;
      end;

    { Write translations to target INI }
    for i:= 0 to High(Translations) do
      TargetIni.WriteString(
        KeysToTranslate[i].Key,   { Section }
        KeysToTranslate[i].Value, { Key }
        Translations[i]);         { Translated value }

    TargetIni.UpdateFile;

  finally
    FreeAndNil(KeysToTranslate);
    FreeAndNil(TextsToTranslate);
    FreeAndNil(Keys);
    FreeAndNil(Sections);
    FreeAndNil(TargetIni);
    FreeAndNil(SourceIni);
  end;
end;



{-------------------------------------------------------------------------------------------------------------
   TDeepLTranslator
-------------------------------------------------------------------------------------------------------------}

constructor TDeepLTranslator.Create;
begin
  inherited Create;
  FUseFreeAPI:= TRUE; { Default to free API }
end;


function TDeepLTranslator.GetProviderName: string;
begin
  Result:= 'DeepL';
end;


function TDeepLTranslator.GetAPIURL: string;
begin
  if FUseFreeAPI
  then Result:= API_URL_FREE
  else Result:= API_URL_PRO;
end;


{ Performs HTTP POST request to DeepL API }
function TDeepLTranslator.DoHttpPost(const URL: string; const RequestBody: TStringStream): string;
var
  HttpClient: TNetHTTPClient;
  Request: TNetHTTPRequest;
  Response: IHTTPResponse;
begin
  Result:= '';
  FLastError:= '';
  HttpClient:= nil;
  Request:= nil;

  try
    HttpClient:= TNetHTTPClient.Create(nil);
    HttpClient.ConnectionTimeout:= FTimeout;
    HttpClient.ResponseTimeout:= FTimeout;

    Request:= TNetHTTPRequest.Create(nil);
    Request.Client:= HttpClient;
    Request.CustomHeaders['Authorization']:= 'DeepL-Auth-Key ' + FApiKey;
    Request.CustomHeaders['Content-Type']:= 'application/json';

    try
      Response:= Request.Post(URL, RequestBody);
    except
      on E: Exception do
        begin
          FLastError:= 'HTTP request failed: ' + E.Message;
          EXIT;
        end;
    end;

    if NOT Assigned(Response) then
      begin
        FLastError:= 'No response received from server';
        EXIT;
      end;

    case Response.StatusCode of
      200: Result:= Response.ContentAsString;
      401: FLastError:= 'Authentication failed. Check your API key.';
      403: FLastError:= 'Access forbidden. Your API key may not have sufficient permissions.';
      429: FLastError:= 'Too many requests. Rate limit exceeded. Please try again later.';
      456: FLastError:= 'Quota exceeded. You have reached your character limit.';
      else FLastError:= Format('HTTP error %d: %s', [Response.StatusCode, Response.ContentAsString]);
    end;

  finally
    Response:= nil; { Release interface before destroying client }
    FreeAndNil(Request);
    FreeAndNil(HttpClient);
  end;
end;


{ Parses DeepL JSON response and extracts translated texts }
function TDeepLTranslator.ParseTranslationResponse(const JSONResponse: string): TArray<string>;
var
  RootObj: TJSONObject;
  TransArr: TJSONArray;
  TransObj: TJSONObject;
  i: Integer;
begin
  SetLength(Result, 0);
  RootObj:= nil;

  if JSONResponse.IsEmpty then
    begin
      FLastError:= 'Empty response from server';
      EXIT;
    end;

  try
    RootObj:= TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;

    if NOT Assigned(RootObj) then
      begin
        FLastError:= 'Invalid JSON response';
        EXIT;
      end;

    if NOT RootObj.TryGetValue<TJSONArray>('translations', TransArr) then
      begin
        FLastError:= 'No translations found in response';
        EXIT;
      end;

    SetLength(Result, TransArr.Count);
    for i:= 0 to TransArr.Count - 1 do
      begin
        TransObj:= TransArr.Items[i] as TJSONObject;
        Result[i]:= TransObj.GetValue<string>('text', '');
      end;

  finally
    FreeAndNil(RootObj);
  end;
end;


{ Translates a single text string }
function TDeepLTranslator.TranslateText(const Text, TargetLang: string; const SourceLang: string): string;
var
  Results: TArray<string>;
begin
  Result:= '';
  Results:= TranslateBatch([Text], TargetLang, SourceLang);
  if Length(Results) > 0
  then Result:= Results[0];
end;


{ Translates multiple texts in batch.
  Automatically splits into multiple requests if exceeding DeepL's 50 text limit. }
function TDeepLTranslator.TranslateBatch(const Texts: TArray<string>; const TargetLang: string; const SourceLang: string): TArray<string>;
var
  RequestJSON: TJSONObject;
  TextArray: TJSONArray;
  RequestBody: TStringStream;
  Response: string;
  BatchResults: TArray<string>;
  BatchStart, BatchEnd, BatchSize: Integer;
  TotalProcessed: Integer;
begin
  SetLength(Result, 0);
  FLastError:= '';

  if Length(Texts) = 0
  then EXIT;

  if FApiKey.Trim.IsEmpty then
    begin
      FLastError:= 'API key is not set';
      EXIT;
    end;

  SetLength(Result, Length(Texts));
  TotalProcessed:= 0;
  BatchStart:= 0;

  { Process in batches of MAX_TEXTS_PER_REQUEST }
  while BatchStart < Length(Texts) do
    begin
      BatchEnd:= Min(BatchStart + MAX_TEXTS_PER_REQUEST - 1, High(Texts));
      BatchSize:= BatchEnd - BatchStart + 1;

      RequestJSON:= nil;
      RequestBody:= nil;
      try
        { Build JSON request }
        RequestJSON:= TJSONObject.Create;

        TextArray:= TJSONArray.Create;
        for var i:= BatchStart to BatchEnd do
          TextArray.Add(Texts[i]);

        RequestJSON.AddPair('text', TextArray);
        RequestJSON.AddPair('target_lang', TargetLang.ToUpper);

        if NOT SourceLang.IsEmpty
        then RequestJSON.AddPair('source_lang', SourceLang.ToUpper);

        RequestBody:= TStringStream.Create(RequestJSON.ToString, TEncoding.UTF8);

        { Send request }
        Response:= DoHttpPost(GetAPIURL, RequestBody);

        if FLastError <> ''
        then EXIT;

        { Parse response }
        BatchResults:= ParseTranslationResponse(Response);

        if FLastError <> ''
        then EXIT;

        if Length(BatchResults) <> BatchSize then
          begin
            FLastError:= Format('Expected %d translations, got %d', [BatchSize, Length(BatchResults)]);
            EXIT;
          end;

        { Copy batch results to main result array }
        for var i:= 0 to High(BatchResults) do
          Result[BatchStart + i]:= BatchResults[i];

        { Progress callback }
        TotalProcessed:= TotalProcessed + BatchSize;
        if Assigned(FOnProgress)
        then FOnProgress(Self, TotalProcessed, Length(Texts), Format('Translated %d of %d texts', [TotalProcessed, Length(Texts)]));

      finally
        FreeAndNil(RequestBody);
        FreeAndNil(RequestJSON);
      end;

      BatchStart:= BatchEnd + 1;
    end;
end;


{ Tests API connection by translating a simple phrase }
function TDeepLTranslator.TestConnection: Boolean;
var
  TestResult: string;
begin
  Result:= FALSE;
  FLastError:= '';

  if FApiKey.Trim.IsEmpty then
    begin
      FLastError:= 'API key is not set';
      EXIT;
    end;

  TestResult:= TranslateText('Hello', 'DE');
  Result:= (FLastError = '') AND (TestResult <> '');

  if Result AND (NOT SameText(TestResult, 'Hallo'))
  then Result:= TRUE; { Translation worked, even if result differs slightly }
end;



end.
