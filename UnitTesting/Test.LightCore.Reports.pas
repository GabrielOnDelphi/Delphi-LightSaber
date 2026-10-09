unit Test.LightCore.Reports;

{=============================================================================================================
   Unit tests for LightCore.Reports
   Tests diagnostic report generation

   Note: These tests require TAppDataCore to be initialized for full functionality.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.Reports,
  LightCore.AppData;

type
  [TestFixture]
  TTestLightCoreReports = class
  private
    FAppDataCreated: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GenerateAppRep Tests }
    [Test]
    procedure TestGenerateAppRep_ContainsHeader;

    [Test]
    procedure TestGenerateAppRep_ContainsAppName;

    [Test]
    procedure TestGenerateAppRep_ContainsAppFolder;

    [Test]
    procedure TestGenerateAppRep_ContainsAppDataFolder;

    [Test]
    procedure TestGenerateAppRep_ContainsIniFile;

    [Test]
    procedure TestGenerateAppRep_ContainsLastUsedFolder;

    [Test]
    procedure TestGenerateAppRep_HoldsAppNameValue;

    { GenerateCoreReport Tests }
    [Test]
    procedure TestGenerateCoreReport_ContainsAppDataHeader;

    [Test]
    procedure TestGenerateCoreReport_ContainsPlatformHeader;

    [Test]
    procedure TestGenerateCoreReport_ContainsCompilerHeader;

    [Test]
    procedure TestGenerateCoreReport_HoldsAppAndPlatformReports;

    [Test]
    procedure TestGenerateCoreReport_MultipleLines;

    { Report Format Tests }
    [Test]
    procedure TestReportFormat_UsesTabsForAlignment;

    [Test]
    procedure TestReportFormat_UsesCRLFForNewLines;
  end;

implementation

uses
  System.IOUtils, LightCore, LightCore.Platform;


{ The text after LabelText on the report line that starts with LabelText (leading spaces and the tabs around the value removed).
  Returns '<label not found>' when no line starts with LabelText. }
function ReportValue(CONST Report, LabelText: string): string;
VAR
  Line: string;
begin
  for Line in Report.Split([#13#10]) do
    if TrimLeft(Line).StartsWith(LabelText)
    then EXIT(Trim(Copy(TrimLeft(Line), Length(LabelText) + 1, MaxInt)));
  Result:= '<label not found>';
end;


{ The folder of the test EXE, from the RTL. TAppDataCore.AppFolder must give the same on a desktop OS. }
function ExpectedAppFolder: string;
begin
  Result:= ExtractFilePath(ParamStr(0));
end;


{ The per-user data folder: %AppData%\<AppName>\ from the RTL (TPath.GetHomePath, c:\Delphi\Delphi 13\source\rtl\common\System.IOUtils.pas:5087),
  or the EXE folder when a 'portable.marker' file sits next to the EXE (portable mode). }
function ExpectedAppDataFolder: string;
begin
  if FileExists(ExpectedAppFolder + 'portable.marker')
  then Result:= ExpectedAppFolder
  else Result:= IncludeTrailingPathDelimiter(TPath.Combine(TPath.GetHomePath, TAppDataCore.AppName));
end;


procedure TTestLightCoreReports.Setup;
begin
  { Create AppDataCore if not already created }
  if AppDataCore = NIL then
  begin
    AppDataCore:= TAppDataCore.Create('TestApp_LightCoreReports');
    FAppDataCreated:= True;
  end
  else
    FAppDataCreated:= False;
end;


procedure TTestLightCoreReports.TearDown;
begin
  { Only free if we created it }
  if FAppDataCreated AND (AppDataCore <> NIL) then
  begin
    FreeAndNil(AppDataCore);
  end;
end;


{ GenerateAppRep Tests }

procedure TTestLightCoreReports.TestGenerateAppRep_ContainsHeader;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('[APPDATA PATHS]', Report) > 0, 'Should contain header');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsAppName;
var
  Report, Value: string;
begin
  Report:= GenerateAppRep;
  Value:= ReportValue(Report, 'AppName:');

  { No source outside TAppDataCore knows the name the runner gave it, so the value is compared with the class property }
  Assert.IsNotEmpty(Value, 'The AppName value must not be empty. Report: ' + Report);
  Assert.AreEqual(TAppDataCore.AppName, Value, 'The AppName line must hold the application name. Report: ' + Report);
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsAppFolder;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.AreEqual(ExpectedAppFolder, ReportValue(Report, 'AppFolder:'), 'The AppFolder line must hold the folder of the EXE. Report: ' + Report);
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsAppDataFolder;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.AreEqual(ExpectedAppDataFolder, ReportValue(Report, 'AppDataFolder:'), 'The AppDataFolder line must hold %AppData%\<AppName>\. Report: ' + Report);
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsIniFile;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.AreEqual(ExpectedAppDataFolder + TAppDataCore.AppName + '.ini', ReportValue(Report, 'IniFile:'), 'The IniFile line must hold <AppDataFolder>\<AppName>.ini. Report: ' + Report);
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsLastUsedFolder;
var
  Report, Value: string;
begin
  Assert.IsNotNull(AppDataCore, 'Setup must have an AppDataCore');
  Report:= GenerateAppRep;
  Value:= ReportValue(Report, 'LastUsedFolder:');

  { AppDataCore exists, so the report must take the branch that prints its LastUsedFolder, an existing folder }
  Assert.AreNotEqual('(AppDataCore not initialized)', Value, 'The report took the branch for a NIL AppDataCore');
  Assert.AreEqual(AppDataCore.LastUsedFolder, Value, 'The LastUsedFolder line must hold AppDataCore.LastUsedFolder. Report: ' + Report);
  Assert.IsTrue(DirectoryExists(Value), 'LastUsedFolder must name an existing folder. Got: ' + Value);
end;


procedure TTestLightCoreReports.TestGenerateAppRep_HoldsAppNameValue;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('  AppName: ' + Tab + Tab + TAppDataCore.AppName + CRLF, Report) > 0, 'The AppName line must carry TAppDataCore.AppName. Report: ' + Report);
end;


{ GenerateCoreReport Tests }

procedure TTestLightCoreReports.TestGenerateCoreReport_ContainsAppDataHeader;
var
  Report: string;
begin
  Report:= GenerateCoreReport;
  Assert.IsTrue(Pos('[APPDATA PATHS]', Report) > 0, 'Should contain app data header');
end;


procedure TTestLightCoreReports.TestGenerateCoreReport_ContainsPlatformHeader;
var
  Report: string;
begin
  Report:= GenerateCoreReport;
  Assert.IsTrue(Pos('[PLATFORM OS]', Report) > 0, 'Should contain platform header');
end;


procedure TTestLightCoreReports.TestGenerateCoreReport_ContainsCompilerHeader;
var
  Report: string;
begin
  Report:= GenerateCoreReport;
  Assert.IsTrue(Pos('[COMPILER]', Report) > 0, 'Should contain compiler header');
end;


procedure TTestLightCoreReports.TestGenerateCoreReport_HoldsAppAndPlatformReports;
var
  Report: string;
begin
  Report:= GenerateCoreReport;
  Assert.AreEqual(1, Pos(GenerateAppRep + CRLF + CRLF, Report), 'The core report must open with the whole app report');
  Assert.IsTrue(Pos(GeneratePlatformRep, Report) > 0, 'The core report must hold the whole platform report');
end;


procedure TTestLightCoreReports.TestGenerateCoreReport_MultipleLines;
var
  Report: string;
  LineCount: Integer;
begin
  Report:= GenerateCoreReport;
  LineCount:= Report.CountChar(#10);

  { 20 line breaks, counted in the source: 8 in GenerateAppRep (LightCore.Reports.pas), 2 after it, 3 in GeneratePlatformRep (LightCore.Platform.pas),
    2 after it, 5 in GenerateCompilerReport (LightCore.Debugger.pas). No value in these reports holds a line break. }
  Assert.AreEqual(20, LineCount, 'Line breaks in the core report');
  Assert.AreEqual(20, Report.CountChar(#13), 'Every line break of the core report must be CRLF');
end;


{ Report Format Tests }

procedure TTestLightCoreReports.TestReportFormat_UsesTabsForAlignment;
var
  Report: string;
  Lines: TArray<string>;
begin
  Report:= GenerateAppRep;
  Lines:= Report.Split([#13#10]);

  { A header line and 8 value lines; every value line separates its label from the value with ': ' and a tab }
  Assert.AreEqual(9, Length(Lines), 'Lines of the app report');
  for var i:= 1 to High(Lines) do
    Assert.IsTrue(Pos(': ' + #9, Lines[i]) > 0, 'No tab after the label in line: ' + Lines[i]);

  { 12 tabs, counted in GenerateAppRep (LightCore.Reports.pas): 2 each for AppName, AppFolder, AppSysDir, IniFile; 1 each for the other 4 lines }
  Assert.AreEqual(12, Report.CountChar(#9), 'Tabs in the app report');
end;


procedure TTestLightCoreReports.TestReportFormat_UsesCRLFForNewLines;
var
  Report: string;
begin
  Report:= GenerateAppRep;

  { 8 line breaks, counted in GenerateAppRep (LightCore.Reports.pas); no value in the report holds a line break }
  Assert.AreEqual(8, Report.CountChar(#10), 'Line breaks in the app report');
  Assert.AreEqual(0, Pos(#10, StringReplace(Report, CRLF, '', [rfReplaceAll])), 'A line break of the app report is a bare LF, not CRLF');
  Assert.AreEqual(0, Pos(#13, StringReplace(Report, CRLF, '', [rfReplaceAll])), 'A line break of the app report is a bare CR, not CRLF');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreReports);

end.
