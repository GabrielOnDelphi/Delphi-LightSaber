unit Test.FormUpdaterSettings;

{=============================================================================================================
   2026.08.11
   Unit tests for FrameVCL\FormUpdaterSettings.pas

   Covers the ordering defect found on the FMX side and ported here: chkForceNewsFound is a DEBUG flag,
   shown only to beta testers. It must be cleared for everybody else - and it must survive for a beta tester.
   The old FormCreate cleared it BEFORE GuiFromObject, which then immediately loaded the persisted value
   back over the top.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework;

TYPE
  [TestFixture]
  TTestFormUpdaterSettings = class
  private
    FBetaFile: string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_FormCreate_NotBetaTester_ClearsForceNewsFound;

    [Test]
    procedure Test_Apply_NotBetaTester_WritesClearedFlagBack;

    [Test]
    procedure Test_FormCreate_BetaTester_KeepsForceNewsFound;
  end;


IMPLEMENTATION

USES
  System.SysUtils,
  LightCore.AppData, LightCore.IO, LightCore.TextFile,
  LightVcl.Visual.AppData,
  ciUpdater, FormUpdaterSettings;


{ BetaTesterMode is decided by a file on disk (TAppDataCore.BetaTesterMode), so the tests switch modes
  by creating/deleting it. AppFolder is where the test EXE lives. }
procedure TTestFormUpdaterSettings.Setup;
begin
  FBetaFile:= TAppDataCore.AppFolder + 'betatester';

  { Defensive: a test that died mid-run could have left the global behind, and TUpdater.Create asserts
    it is NIL. Note that inside a DUnitX test unit the intrinsic Assert() is shadowed by DUnitX's
    Assert CLASS, so this cannot be written as a plain Assert(). }
  FreeAndNil(Updater);
  Updater:= TUpdater.Create('');      { Empty URL: the timer is created disabled, so nothing goes to the network }
end;


procedure TTestFormUpdaterSettings.TearDown;
begin
  FreeAndNil(Updater);
  TryDeleteFile(FBetaFile);
end;


procedure TTestFormUpdaterSettings.Test_FormCreate_NotBetaTester_ClearsForceNewsFound;
VAR
  Form: TfrmUpdaterSettings;
begin
  Assert.IsFalse(AppData.BetaTesterMode, 'Precondition: a leftover "betatester" file next to the test EXE would invalidate this test');

  Updater.ForceNewsFound:= TRUE;      { As if left over from an earlier beta session }

  AppData.CreateFormHidden(TfrmUpdaterSettings, Form);
  TRY
    Assert.IsFalse(Form.chkForceNewsFound.Visible, 'The debug checkbox belongs to beta testers only');
    Assert.IsFalse(Form.chkForceNewsFound.Checked, 'A hidden debug flag must be cleared, not carried over invisibly from the INI');
  FINALLY
    FreeAndNil(Form);
  END;
end;


{ The consequence the user actually feels: the flag is written back cleared, so the updater stops
  claiming it found news. }
procedure TTestFormUpdaterSettings.Test_Apply_NotBetaTester_WritesClearedFlagBack;
VAR
  Form: TfrmUpdaterSettings;
begin
  Assert.IsFalse(AppData.BetaTesterMode, 'Precondition: a leftover "betatester" file next to the test EXE would invalidate this test');

  Updater.ForceNewsFound:= TRUE;

  AppData.CreateFormHidden(TfrmUpdaterSettings, Form);
  TRY
    Form.Apply;
    Assert.IsFalse(Updater.ForceNewsFound, 'Apply must persist the cleared flag, otherwise the updater reports "news found" forever');
  FINALLY
    FreeAndNil(Form);
  END;
end;


{ The other direction: clearing the flag must NOT reach a real beta tester. }
procedure TTestFormUpdaterSettings.Test_FormCreate_BetaTester_KeepsForceNewsFound;
VAR
  Form: TfrmUpdaterSettings;
begin
  StringToFile(FBetaFile, 'Created by Test.FormUpdaterSettings. Safe to delete.');
  Assert.IsTrue(AppData.BetaTesterMode, 'Setup failed: could not switch the app into BetaTester mode');

  Updater.ForceNewsFound:= TRUE;

  AppData.CreateFormHidden(TfrmUpdaterSettings, Form);
  TRY
    Assert.IsTrue(Form.chkForceNewsFound.Visible, 'A beta tester must see the debug checkbox');
    Assert.IsTrue(Form.chkForceNewsFound.Checked, 'A beta tester keeps the persisted flag');
  FINALLY
    FreeAndNil(Form);
  END;
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestFormUpdaterSettings);

end.
