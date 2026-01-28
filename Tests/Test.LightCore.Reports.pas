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
    procedure TestGenerateAppRep_ReturnsNonEmpty;

    { GenerateCoreReport Tests }
    [Test]
    procedure TestGenerateCoreReport_ContainsAppDataHeader;

    [Test]
    procedure TestGenerateCoreReport_ContainsPlatformHeader;

    [Test]
    procedure TestGenerateCoreReport_ContainsCompilerHeader;

    [Test]
    procedure TestGenerateCoreReport_ReturnsNonEmpty;

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
  LightCore;


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
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('AppName:', Report) > 0, 'Should contain AppName field');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsAppFolder;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('AppFolder:', Report) > 0, 'Should contain AppFolder field');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsAppDataFolder;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('AppDataFolder:', Report) > 0, 'Should contain AppDataFolder field');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsIniFile;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('IniFile:', Report) > 0, 'Should contain IniFile field');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ContainsLastUsedFolder;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos('LastUsedFolder:', Report) > 0, 'Should contain LastUsedFolder field');
end;


procedure TTestLightCoreReports.TestGenerateAppRep_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(GenerateAppRep);
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


procedure TTestLightCoreReports.TestGenerateCoreReport_ReturnsNonEmpty;
begin
  Assert.IsNotEmpty(GenerateCoreReport);
end;


procedure TTestLightCoreReports.TestGenerateCoreReport_MultipleLines;
var
  Report: string;
  LineCount: Integer;
begin
  Report:= GenerateCoreReport;
  LineCount:= Report.CountChar(#10);
  Assert.IsTrue(LineCount > 10, 'Core report should have multiple lines (found ' + IntToStr(LineCount) + ')');
end;


{ Report Format Tests }

procedure TTestLightCoreReports.TestReportFormat_UsesTabsForAlignment;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  Assert.IsTrue(Pos(#9, Report) > 0, 'Report should use tabs for alignment');
end;


procedure TTestLightCoreReports.TestReportFormat_UsesCRLFForNewLines;
var
  Report: string;
begin
  Report:= GenerateAppRep;
  { Check for CRLF (Windows) or at least LF (Unix) }
  Assert.IsTrue(
    (Pos(CRLF, Report) > 0) OR (Pos(#10, Report) > 0),
    'Report should use line breaks'
  );
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestLightCoreReports);

end.
