unit Test.LightFmx.Common.AppData;

{=============================================================================================================
   Unit tests for LightFmx.Common.AppData.pas
   Tests TAppData - FMX application data management, form creation, and FMX-specific functionality.

   Note: Form creation tests are limited because FMX requires a running Application.
         These tests focus on the non-GUI functionality and state management.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.AppData;

type
  [TestFixture]
  TTestFmxAppData = class
  private
    FOldUnattended: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Version Tests }
    [Test]
    procedure TestGetAppVersion;

    { Minimizing Tests }
    [Test]
    procedure TestMinimize_NoMainForm;

    { Log Form Tests (without actual form creation) }
    [Test]
    procedure TestRamLogExists;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  FMX.Forms,
  LightFmx.Common.AppData;


{ TTestFmxAppData }

procedure TTestFmxAppData.Setup;
begin
  FOldUnattended:= TAppDataCore.Unattended;
end;

procedure TTestFmxAppData.TearDown;
begin
  TAppDataCore.Unattended:= FOldUnattended;
end;


{ Version Tests }

{ This unit may be linked by more than one test EXE, so on Windows the expected version is read from the running EXE
  itself, straight through GetFileVersionInfo/VerQueryValue. FMX's IFMXApplicationService.AppVersion returns only
  Major.Minor on Windows (TPlatformWin.GetVersionString in FMX.Platform.Win.pas).
  Tests_LightFmx.dproj gives its EXE the version 1.2.3.4, linked by the $R *.res line in Tests_LightFmx.dpr; that EXE must always take the real leg. }
procedure TTestFmxAppData.TestGetAppVersion;
{$IFDEF MSWINDOWS}
CONST
  VersionedTestExe = 'Tests_LightFmx.exe';
VAR
  Size, Dummy, InfoLen: DWORD;
  Buffer: TBytes;
  Info: PVSFixedFileInfo;
  Expected: string;
begin
  Expected:= '';
  Size:= GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if Size > 0 then
    begin
      SetLength(Buffer, Size);
      if GetFileVersionInfo(PChar(ParamStr(0)), 0, Size, Buffer)
      AND VerQueryValue(Buffer, '\', Pointer(Info), InfoLen)
      AND (InfoLen >= SizeOf(TVSFixedFileInfo))
      then Expected:= IntToStr(HiWord(Info.dwFileVersionMS))+ '.'+ IntToStr(LoWord(Info.dwFileVersionMS));
    end;

  if Expected = '' then
    if SameText(ExtractFileName(ParamStr(0)), VersionedTestExe)
    then Assert.Fail(VersionedTestExe + ' must carry a version resource ($R *.res in its .dpr, VerInfo_* in its .dproj)')
    else Assert.Pass('No version resource in ' + ExtractFileName(ParamStr(0)));

  Assert.AreEqual(Expected, AppData.GetAppVersion, 'GetAppVersion must return the Major.Minor of the running EXE');
end;
{$ELSE}
begin
  Assert.IsNotEmpty(AppData.GetAppVersion, 'GetAppVersion must return the version of the app package');
end;
{$ENDIF}


{ Minimizing Tests }

procedure TTestFmxAppData.TestMinimize_NoMainForm;
begin
  // When MainForm is nil, Minimize should exit gracefully without exception
  if Application.MainForm = NIL then
  begin
    Assert.WillNotRaiseAny(
      procedure
      begin
        AppData.Minimize;
      end,
      'Minimize should not raise when MainForm is nil');
  end
  else
    Assert.Pass('MainForm exists - skipping nil test');
end;


{ Log Form Tests }

procedure TTestFmxAppData.TestRamLogExists;
begin
  Assert.IsNotNull(AppData.RamLog, 'RamLog should be created with AppData');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxAppData);

end.
