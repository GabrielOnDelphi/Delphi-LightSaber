unit Test.LightVcl.Common.CpuMonitor;

{=============================================================================================================
   Unit tests for LightVcl.Common.CpuMonitor.pas
   Tests the INI persistence of TPowerSettings (LoadFromIni / SaveToIni).
   Each test uses its own INI file in a run folder of its own (Test.RunFolder), deleted in TearDown.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.IniFiles,
  LightCore.INIFile, LightVcl.Common.CpuMonitor;

type
  [TestFixture]
  TTestPowerSettingsIni = class
  private
    FRunFolder: string;
    FIniPath: string;
    procedure setNonDefaults(Settings: TPowerSettings);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure SaveLoad_RoundTrip_KeepsUserValues;
    [Test]
    procedure Load_MissingKeys_GivesDefaults;
    [Test]
    procedure Save_WritesDocumentedKeyNames;
    [Test]
    procedure Load_ThresholdOutOfRange_IsClamped;
  end;


implementation

USES
  Test.RunFolder;

CONST
  TestSection = 'TestSection';
  RunPrefix   = 'TestPowerSettings';


procedure TTestPowerSettingsIni.Setup;
begin
  FRunFolder:= NewRunFolder(RunPrefix);
  FIniPath:= FRunFolder + 'PowerSettings.ini';
end;


procedure TTestPowerSettingsIni.TearDown;
begin
  DeleteRunFolder(FRunFolder, RunPrefix);
  FRunFolder:= '';
end;


{ Values that differ from the defaults (TRUE, TRUE, 80), so a load that does nothing is caught. }
procedure TTestPowerSettingsIni.setNonDefaults(Settings: TPowerSettings);
begin
  Settings.CheckBatteries   := FALSE;
  Settings.NotifyPowerChange:= FALSE;
  Settings.HighCpuThreshold := 50;
end;


procedure TTestPowerSettingsIni.SaveLoad_RoundTrip_KeepsUserValues;
begin
  VAR Saved:= TPowerSettings.Create;
  TRY
    setNonDefaults(Saved);
    VAR Ini:= TIniFileEx.Create(TestSection, FIniPath);
    TRY
      Saved.SaveToIni(Ini);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Saved);
  END;

  VAR Loaded:= TPowerSettings.Create;   { Starts with the defaults TRUE, TRUE, 80 }
  TRY
    VAR Ini:= TIniFileEx.Create(TestSection, FIniPath);
    TRY
      Loaded.LoadFromIni(Ini);
    FINALLY
      FreeAndNil(Ini);
    END;
    Assert.IsFalse(Loaded.CheckBatteries,    'CheckBatteries not read back');
    Assert.IsFalse(Loaded.NotifyPowerChange, 'NotifyPowerChange not read back');
    Assert.AreEqual(50, Loaded.HighCpuThreshold, 'HighCpuThreshold not read back');
  FINALLY
    FreeAndNil(Loaded);
  END;
end;


procedure TTestPowerSettingsIni.Load_MissingKeys_GivesDefaults;
begin
  Assert.IsFalse(FileExists(FIniPath), 'The test INI must not exist yet');

  VAR Settings:= TPowerSettings.Create;
  TRY
    setNonDefaults(Settings);
    VAR Ini:= TIniFileEx.Create(TestSection, FIniPath);
    TRY
      Settings.LoadFromIni(Ini);
    FINALLY
      FreeAndNil(Ini);
    END;
    Assert.IsTrue(Settings.CheckBatteries,    'Missing key must give the default TRUE');
    Assert.IsTrue(Settings.NotifyPowerChange, 'Missing key must give the default TRUE');
    Assert.AreEqual(80, Settings.HighCpuThreshold, 'Missing key must give the default 80');
  FINALLY
    FreeAndNil(Settings);
  END;
end;


{ BioniX and other programs depend on these key names: a rename would lose every user's settings. }
procedure TTestPowerSettingsIni.Save_WritesDocumentedKeyNames;
begin
  VAR Settings:= TPowerSettings.Create;
  TRY
    setNonDefaults(Settings);
    VAR Ini:= TIniFileEx.Create(TestSection, FIniPath);
    TRY
      Settings.SaveToIni(Ini);
    FINALLY
      FreeAndNil(Ini);
    END;
  FINALLY
    FreeAndNil(Settings);
  END;

  VAR Raw:= TMemIniFile.Create(FIniPath);
  TRY
    Assert.AreEqual(50, Raw.ReadInteger(TestSection, 'PowerSettings.HighCpuThreshold', -1));
    Assert.IsFalse(Raw.ReadBool(TestSection, 'PowerSettings.CheckBatteries', TRUE));
    Assert.IsFalse(Raw.ReadBool(TestSection, 'PowerSettings.NotifyPowerChange', TRUE));
  FINALLY
    FreeAndNil(Raw);
  END;
end;


{ The INI is user-editable. The GUI spin box allows 2..101 (FormPower.dfm); 101 disables the CPU check.
  A threshold of 0 would make CpuIsBusy TRUE for ever, so the wallpaper would never change. }
procedure TTestPowerSettingsIni.Load_ThresholdOutOfRange_IsClamped;

  function loadThreshold(Value: Integer): Integer;
  begin
    VAR Raw:= TMemIniFile.Create(FIniPath);
    TRY
      Raw.WriteInteger(TestSection, 'PowerSettings.HighCpuThreshold', Value);
      Raw.UpdateFile;
    FINALLY
      FreeAndNil(Raw);
    END;

    VAR Settings:= TPowerSettings.Create;
    TRY
      VAR Ini:= TIniFileEx.Create(TestSection, FIniPath);
      TRY
        Settings.LoadFromIni(Ini);
      FINALLY
        FreeAndNil(Ini);
      END;
      Result:= Settings.HighCpuThreshold;
    FINALLY
      FreeAndNil(Settings);
    END;
  end;

begin
  Assert.AreEqual(2,   loadThreshold(0),    'Below the GUI minimum must clamp to 2');
  Assert.AreEqual(101, loadThreshold(500),  'Above the GUI maximum must clamp to 101');
  Assert.AreEqual(101, loadThreshold(101),  '101 (check disabled) must stay 101');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestPowerSettingsIni);

end.
