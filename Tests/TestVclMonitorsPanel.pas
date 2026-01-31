UNIT TestVclMonitorsPanel;
{-------------------------------------------------------------------------------------------------------------
   CubicDesign
   2026.01.31

   Unit tests for vclMonitorsPanel.pas
   Tests the TMonitorsPnl component properties and behaviors.

   Note: TMonitorsPnl depends on TMonitors/TMonitorsEx and visual monitor configuration,
   which makes full integration testing challenging. These tests focus on:
   - Property accessors and setters
   - ShowHints state management
   - INI read/write operations

   Usage:
     Run via TestInsight in Delphi IDE or via DUnitX console runner.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,
  LightCore.INIFile;

TYPE
  [TestFixture]
  TTestMonitorsPnl = class
  private
    FTestForm: TForm;
    FTempIniPath: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Property tests }
    [Test]
    procedure Test_Create_DefaultBkgColor;
    [Test]
    procedure Test_Create_DefaultShowHints;
    [Test]
    procedure Test_BkgColor_SetGet;

    { ShowHints tests }
    [Test]
    procedure Test_ShowHints_SetTrue_CreatesControls;
    [Test]
    procedure Test_ShowHints_SetFalse_DestroysControls;
    [Test]
    procedure Test_ShowHints_SetTrueTwice_NoDuplicateControls;
    [Test]
    procedure Test_ShowHints_SetFalseTwice_NoError;

    { INI persistence tests }
    [Test]
    procedure Test_ReadHint_True_ShowsHint;
    [Test]
    procedure Test_ReadHint_False_HidesHint;
    [Test]
    procedure Test_WriteHint_SavesVisibleState;
    [Test]
    procedure Test_WriteHint_SavesHiddenState;
  end;


IMPLEMENTATION

USES
  Vcl.Graphics, LightVcl.Common.Colors, vclMonitorsPanel;


{-------------------------------------------------------------------------------------------------------------
   SETUP / TEARDOWN
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorsPnl.Setup;
begin
  { Create a test form to host the panel }
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Width:= 800;
  FTestForm.Height:= 600;

  { Create temp INI file path }
  FTempIniPath:= System.SysUtils.GetEnvironmentVariable('TEMP') + '\TestMonitorsPnl.ini';

  { Delete any existing temp file }
  if FileExists(FTempIniPath) then
    DeleteFile(FTempIniPath);
end;


procedure TTestMonitorsPnl.TearDown;
begin
  FreeAndNil(FTestForm);

  { Clean up temp INI file }
  if FileExists(FTempIniPath) then
    DeleteFile(FTempIniPath);
end;


{-------------------------------------------------------------------------------------------------------------
   PROPERTY TESTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorsPnl.Test_Create_DefaultBkgColor;
var
  Panel: TMonitorsPnl;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Assert.AreEqual(Integer(clBlueBkg), Integer(Panel.BkgColor), 'Default BkgColor should be clBlueBkg');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_Create_DefaultShowHints;
var
  Panel: TMonitorsPnl;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Assert.IsFalse(Panel.ShowHints, 'Default ShowHints should be FALSE');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_BkgColor_SetGet;
var
  Panel: TMonitorsPnl;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Panel.BkgColor:= clRed;
    Assert.AreEqual(Integer(clRed), Integer(Panel.BkgColor), 'BkgColor should persist after setting');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   SHOWHINTS TESTS

   These tests verify the ShowHints property correctly manages the hint controls
   and prevents memory leaks from duplicate control creation.
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorsPnl.Test_ShowHints_SetTrue_CreatesControls;
var
  Panel: TMonitorsPnl;
  HintPanelFound: Boolean;
  i: Integer;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Panel.ShowHints:= TRUE;

    { Verify hint panel was created by checking child controls }
    HintPanelFound:= FALSE;
    for i:= 0 to Panel.ControlCount - 1 do
      if Panel.Controls[i].Name = 'pnlHintsMini' then
        begin
          HintPanelFound:= TRUE;
          Break;
        end;

    Assert.IsTrue(HintPanelFound, 'ShowHints=TRUE should create pnlHintsMini panel');
    Assert.IsTrue(Panel.ShowHints, 'ShowHints property should return TRUE after setting');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_ShowHints_SetFalse_DestroysControls;
var
  Panel: TMonitorsPnl;
  HintPanelFound: Boolean;
  i: Integer;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;

    { First create the hint controls }
    Panel.ShowHints:= TRUE;

    { Then destroy them }
    Panel.ShowHints:= FALSE;

    { Verify hint panel was destroyed }
    HintPanelFound:= FALSE;
    for i:= 0 to Panel.ControlCount - 1 do
      if Panel.Controls[i].Name = 'pnlHintsMini' then
        begin
          HintPanelFound:= TRUE;
          Break;
        end;

    Assert.IsFalse(HintPanelFound, 'ShowHints=FALSE should destroy pnlHintsMini panel');
    Assert.IsFalse(Panel.ShowHints, 'ShowHints property should return FALSE after setting');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_ShowHints_SetTrueTwice_NoDuplicateControls;
var
  Panel: TMonitorsPnl;
  HintPanelCount: Integer;
  i: Integer;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;

    { Set TRUE twice - should not create duplicate controls }
    Panel.ShowHints:= TRUE;
    Panel.ShowHints:= TRUE;

    { Count hint panels - should be exactly 1 }
    HintPanelCount:= 0;
    for i:= 0 to Panel.ControlCount - 1 do
      if Panel.Controls[i].Name = 'pnlHintsMini' then
        Inc(HintPanelCount);

    Assert.AreEqual(1, HintPanelCount, 'Setting ShowHints=TRUE twice should not create duplicate panels');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_ShowHints_SetFalseTwice_NoError;
var
  Panel: TMonitorsPnl;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;

    { Set FALSE twice - should not raise exception }
    Panel.ShowHints:= FALSE;
    Panel.ShowHints:= FALSE;

    Assert.IsFalse(Panel.ShowHints, 'Setting ShowHints=FALSE twice should not raise exception');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   INI PERSISTENCE TESTS

   Tests the ReadHint and WriteHint methods that persist hint visibility state.
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorsPnl.Test_ReadHint_True_ShowsHint;
var
  Panel: TMonitorsPnl;
  IniFile: TIniFileEx;
begin
  { Write TRUE to INI file }
  IniFile:= TIniFileEx.Create(FTempIniPath);
  TRY
    IniFile.Write('HintsDragDrop', TRUE);
  FINALLY
    FreeAndNil(IniFile);
  END;

  { Create panel and read hint state }
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;

    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      Panel.ReadHint(IniFile);
    FINALLY
      FreeAndNil(IniFile);
    END;

    Assert.IsTrue(Panel.ShowHints, 'ReadHint with TRUE should set ShowHints=TRUE');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_ReadHint_False_HidesHint;
var
  Panel: TMonitorsPnl;
  IniFile: TIniFileEx;
begin
  { Write FALSE to INI file }
  IniFile:= TIniFileEx.Create(FTempIniPath);
  TRY
    IniFile.Write('HintsDragDrop', FALSE);
  FINALLY
    FreeAndNil(IniFile);
  END;

  { Create panel and read hint state }
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;

    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      Panel.ReadHint(IniFile);
    FINALLY
      FreeAndNil(IniFile);
    END;

    Assert.IsFalse(Panel.ShowHints, 'ReadHint with FALSE should set ShowHints=FALSE');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_WriteHint_SavesVisibleState;
var
  Panel: TMonitorsPnl;
  IniFile: TIniFileEx;
  SavedValue: Boolean;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Panel.ShowHints:= TRUE;

    { Write hint state to INI }
    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      Panel.WriteHint(IniFile);
    FINALLY
      FreeAndNil(IniFile);
    END;

    { Read back and verify }
    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      SavedValue:= IniFile.Read('HintsDragDrop', FALSE);
    FINALLY
      FreeAndNil(IniFile);
    END;

    Assert.IsTrue(SavedValue, 'WriteHint should save TRUE when hints are visible');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


procedure TTestMonitorsPnl.Test_WriteHint_SavesHiddenState;
var
  Panel: TMonitorsPnl;
  IniFile: TIniFileEx;
  SavedValue: Boolean;
begin
  Panel:= TMonitorsPnl.Create(FTestForm);
  TRY
    Panel.Parent:= FTestForm;
    Panel.ShowHints:= FALSE;

    { Write hint state to INI }
    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      Panel.WriteHint(IniFile);
    FINALLY
      FreeAndNil(IniFile);
    END;

    { Read back and verify }
    IniFile:= TIniFileEx.Create(FTempIniPath);
    TRY
      SavedValue:= IniFile.Read('HintsDragDrop', TRUE);  { Default TRUE so we can verify FALSE was written }
    FINALLY
      FreeAndNil(IniFile);
    END;

    Assert.IsFalse(SavedValue, 'WriteHint should save FALSE when hints are hidden');
  FINALLY
    FreeAndNil(Panel);
  END;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestMonitorsPnl);

end.
