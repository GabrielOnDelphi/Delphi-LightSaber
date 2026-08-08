unit Test.FormSkinsDisk;

{=============================================================================================================
   Regression tests for FrameVCL\FormSkinsDisk.pas - the disk-based VCL style loader.

   Contract under test (full reasoning in Docs\Skins-VCL.md):
     * LoadLastStyle RAISES if Application.MainForm is already assigned. This is the regression that
       kept coming back and that leaks the main form's TMainMenuBarStyleHook (AV on the next menu click).
     * INI: reads 'LastStyle', falls back to the pre-2026-02-23 'LastSkin', writes ONLY 'LastStyle'.
     * Degrades gracefully: missing file, invalid file, DefWinTheme, empty INI + empty default.
     * TStyleManager.ActiveStyle.Name really changes after a successful load.
     * The selector NEVER applies a style live - it only saves the choice.

   The fixture frees the process' main form to reach the "MainForm = NIL" state and recreates it
   afterwards; TApplication.ControlDestroyed clears FMainForm on destruction (Vcl.Forms.pas:12477-12479).
=============================================================================================================}

interface

uses
  Winapi.Windows,
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Themes,
  Vcl.Styles;

type
  [TestFixture]
  TTestFormSkinsDisk = class
  private
    FStyleFile: string;    { Short name of a real .vsf copied into the app's Skins folder by Setup }
    FStyleName: string;    { The style NAME declared inside FStyleFile (what ActiveStyle.Name becomes) }
    FBrokenFile: string;   { Short name of the deliberately-corrupt .vsf }
    procedure WithoutMainForm(Proc: TProc);
    procedure RestoreWindowsStyle;
    procedure ClearIniKeys;
    function  StylesFolder: string;
    function  FindShippedStyle: string;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    { 1. The ordering contract }
    [Test] procedure LoadLastStyle_RaisesWhenMainFormExists;
    [Test] procedure LoadLastStyle_ErrorMessageNamesCreateMainForm;
    [Test] procedure LoadLastStyle_DoesNotRaiseWhenMainFormIsNil;

    { 2. INI round-trip and backward compatibility }
    [Test] procedure Ini_RoundTripsLastStyle;
    [Test] procedure Ini_FallsBackToLastSkinWhenLastStyleEmpty;
    [Test] procedure Ini_LastStyleWinsOverLastSkin;
    [Test] procedure Ini_OnlyTheNewKeyIsEverWritten;

    { 3. Graceful degradation }
    [Test] procedure Degrade_MissingFileDoesNotChangeStyle;
    [Test] procedure Degrade_InvalidFileLogsErrorAndDoesNotRaise;
    [Test] procedure Degrade_DefWinThemeAppliesNoStyle;
    [Test] procedure Degrade_EmptyIniAndEmptyDefaultDoNothing;

    { 4. The style is really applied }
    [Test] procedure ActiveStyleChangesAfterSuccessfulLoad;

    { 5. The selector }
    [Test] procedure Selector_ListsDefaultThemeFirstAndTheVsfFiles;
    [Test] procedure Selector_PreselectsTheConfiguredStyle;
    [Test] procedure Selector_PreselectsDefaultThemeWhenNothingConfigured;
    [Test] procedure Selector_ClickSavesToIniButDoesNotApplyStyleLive;
    [Test] procedure Selector_ClosingSetsCaFree;
  end;

implementation

uses
  LightCore.IO,
  LightCore.TextFile,
  LightCore.LogTypes,
  LightCore.INIFileQuick,
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormSkinsDisk;

CONST
  BrokenStyleName = 'ZZ Broken (test).vsf';


{-----------------------------------------------------------------------------------------------------------------------
   FIXTURE
-----------------------------------------------------------------------------------------------------------------------}

function TTestFormSkinsDisk.StylesFolder: string;
begin
  Result:= AppData.AppSysDir + 'Skins\';
end;


{ The repo ships real .vsf files with the Full template. Walk up from the EXE to find one, so the
  test works whatever output folder the compiler used. }
function TTestFormSkinsDisk.FindShippedStyle: string;
CONST
  RelPath = 'Demo\VCL\Template App Full\System\Skins\CyanDusk.vsf';
var
  Folder: string;
  i: Integer;
begin
  Folder:= ExtractFilePath(ParamStr(0));
  for i:= 0 to 5 do
  begin
    if FileExists(Folder + RelPath)
    then EXIT(Folder + RelPath);
    Folder:= ExtractFilePath(ExcludeTrailingPathDelimiter(Folder));
    if Folder = '' then BREAK;
  end;
  Result:= '';
end;


procedure TTestFormSkinsDisk.Setup;
var
  Source: string;
  Info: TStyleInfo;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  AppData.RamLog.ShowOnError:= FALSE;   { A popped-up log window would block a headless run }

  LightCore.IO.ForceDirectoriesB(StylesFolder);

  Source:= FindShippedStyle;
  Assert.IsTrue(Source <> '', 'Test asset not found: Demo\VCL\Template App Full\System\Skins\CyanDusk.vsf. The style tests cannot verify anything without a real .vsf.');

  FStyleFile:= ExtractFileName(Source);
  Assert.IsTrue(LightCore.IO.CopyFile(Source, StylesFolder + FStyleFile), 'Could not copy the test style into ' + StylesFolder);

  Assert.IsTrue(TStyleManager.IsValidStyle(StylesFolder + FStyleFile, Info), 'The shipped test style is not a valid .vsf: ' + Source);
  FStyleName:= Info.Name;

  { A file that has the right extension but is not a style at all }
  FBrokenFile:= BrokenStyleName;
  LightCore.TextFile.StringToFile(StylesFolder + FBrokenFile, 'This is not a VCL style file.');
end;


procedure TTestFormSkinsDisk.TearDown;
begin
  RestoreWindowsStyle;
  ClearIniKeys;
  if FBrokenFile <> ''
  then System.SysUtils.DeleteFile(StylesFolder + FBrokenFile);
end;


procedure TTestFormSkinsDisk.ClearIniKeys;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle,    '');
  LightCore.INIFileQuick.WriteString(IniKeyStyleOld, '');
end;


{ Only ever called while no form exists (see WithoutMainForm): a live SetStyle is exactly what BUG 5 forbids. }
procedure TTestFormSkinsDisk.RestoreWindowsStyle;
begin
  if TStyleManager.ActiveStyle.Name <> TStyleManager.SystemStyleName
  then TStyleManager.SetStyle(TStyleManager.SystemStyleName);
end;


{ Runs Proc with Application.MainForm = NIL, then puts a fresh hidden main form back.
  The test project deliberately does NOT free its main form itself, so nothing dangles here. }
procedure TTestFormSkinsDisk.WithoutMainForm(Proc: TProc);
var
  Form: TForm;
begin
  Form:= Application.MainForm;
  Assert.IsNotNull(Form, 'Precondition: the test app must have a main form');
  FreeAndNil(Form);
  try
    Assert.IsNull(Application.MainForm, 'Freeing the main form must clear Application.MainForm');
    Proc();
    RestoreWindowsStyle;    { revert while no form is alive to receive CM_CUSTOMSTYLECHANGED }
  finally
    Application.CreateForm(TForm, Form);
    Form.Visible:= FALSE;
  end;
end;



{-----------------------------------------------------------------------------------------------------------------------
   1. THE ORDERING CONTRACT
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsDisk.LoadLastStyle_RaisesWhenMainFormExists;
begin
  Assert.IsNotNull(Application.MainForm, 'Precondition: a main form must exist for this test to mean anything');

  Assert.WillRaise(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
    end,
    Exception,
    'LoadLastStyle MUST raise when Application.MainForm is already assigned. Without this guard a late style change leaks TMainMenuBarStyleHook and AVs on the next menu click.');
end;


procedure TTestFormSkinsDisk.LoadLastStyle_ErrorMessageNamesCreateMainForm;
var
  Msg: string;
begin
  Msg:= '';
  try
    FormSkinsDisk.LoadLastStyle('');
  except
    on E: Exception do Msg:= E.Message;
  end;

  Assert.IsTrue(Pos('CreateMainForm', Msg) > 0, 'The guard must tell the developer what to do. Got: "' + Msg + '"');
end;


procedure TTestFormSkinsDisk.LoadLastStyle_DoesNotRaiseWhenMainFormIsNil;
begin
  ClearIniKeys;
  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');       { must not raise }
      Assert.AreEqual('', FormSkinsDisk.CurrentStyle, 'Empty INI + empty default must leave the style unset');
    end);
end;



{-----------------------------------------------------------------------------------------------------------------------
   2. INI ROUND-TRIP AND BACKWARD COMPATIBILITY
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsDisk.Ini_RoundTripsLastStyle;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle, FStyleFile);
  Assert.AreEqual(FStyleFile, LightCore.INIFileQuick.ReadString(IniKeyStyle, ''), 'LastStyle must survive a write/read round-trip');
end;


procedure TTestFormSkinsDisk.Ini_FallsBackToLastSkinWhenLastStyleEmpty;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle,    '');
  LightCore.INIFileQuick.WriteString(IniKeyStyleOld, FStyleFile);

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
      Assert.AreEqual(FStyleFile, FormSkinsDisk.CurrentStyle, 'An INI written before the 2026-02-23 rename must still be honoured');
      Assert.AreEqual(FStyleName, TStyleManager.ActiveStyle.Name, 'The style read from the legacy key must be really applied, not just remembered');
    end);
end;


procedure TTestFormSkinsDisk.Ini_LastStyleWinsOverLastSkin;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle,    'Current choice.vsf');
  LightCore.INIFileQuick.WriteString(IniKeyStyleOld, 'Legacy choice.vsf');

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('Default.vsf');
      Assert.AreEqual('Current choice.vsf', FormSkinsDisk.CurrentStyle, 'The current key must win over the legacy one and over the default');
    end);
end;


procedure TTestFormSkinsDisk.Ini_OnlyTheNewKeyIsEverWritten;
CONST
  Sentinel = 'Legacy value.vsf';
var
  Form: TfrmStyleDisk;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle,    '');
  LightCore.INIFileQuick.WriteString(IniKeyStyleOld, Sentinel);

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
      Assert.AreEqual(Sentinel, FormSkinsDisk.CurrentStyle, 'Precondition: the legacy key must have been read');
    end);

  { Save path. FormPreRelease is gated on AppData.Initializing (still TRUE in a test run), so drive
    the other writer - the same one the user triggers by picking a skin. }
  Form:= TfrmStyleDisk.Create(NIL);
  try
    Form.lBox.ItemIndex:= Form.lBox.Items.IndexOf(FStyleFile);
    Form.lBoxClick(Form);
  finally
    FreeAndNil(Form);
  end;

  Assert.AreEqual(FStyleFile, LightCore.INIFileQuick.ReadString(IniKeyStyle,    ''), 'The new choice must be written to LastStyle');
  Assert.AreEqual(Sentinel,   LightCore.INIFileQuick.ReadString(IniKeyStyleOld, ''), 'The legacy key must never be written - it stays exactly as the old version left it');
end;



{-----------------------------------------------------------------------------------------------------------------------
   3. GRACEFUL DEGRADATION
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsDisk.Degrade_MissingFileDoesNotChangeStyle;
var
  Before: string;
begin
  ClearIniKeys;
  Before:= TStyleManager.ActiveStyle.Name;

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('No such style on disk.vsf');
      Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'A missing .vsf must leave the active style untouched');
      Assert.AreEqual('No such style on disk.vsf', FormSkinsDisk.CurrentStyle, 'The requested name is still remembered so the user sees his own choice in the selector');
    end);
end;


procedure TTestFormSkinsDisk.Degrade_InvalidFileLogsErrorAndDoesNotRaise;
var
  ErrorsBefore: Integer;
  Before: string;
begin
  ClearIniKeys;
  ErrorsBefore:= AppData.RamLog.Count(TRUE, lvErrors);
  Before:= TStyleManager.ActiveStyle.Name;

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle(BrokenStyleName);      { must not raise: it runs before Application.Run, where an exception kills startup }
      Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'A corrupt .vsf must leave the active style untouched');
    end);

  Assert.IsTrue(AppData.RamLog.Count(TRUE, lvErrors) > ErrorsBefore, 'A corrupt .vsf must surface an error in the log. Silence here is how a broken skin ships unnoticed.');
end;


procedure TTestFormSkinsDisk.Degrade_DefWinThemeAppliesNoStyle;
var
  Before: string;
begin
  ClearIniKeys;
  Before:= TStyleManager.ActiveStyle.Name;

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle(DefWinTheme);
      Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'DefWinTheme means "load nothing" - it must not touch TStyleManager');
      Assert.AreEqual(DefWinTheme, FormSkinsDisk.CurrentStyle, 'DefWinTheme must be remembered as the current choice');
    end);
end;


procedure TTestFormSkinsDisk.Degrade_EmptyIniAndEmptyDefaultDoNothing;
var
  Before: string;
begin
  ClearIniKeys;
  Before:= TStyleManager.ActiveStyle.Name;

  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
      Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'Nothing configured => nothing applied');
      Assert.AreEqual('', FormSkinsDisk.CurrentStyle, 'Nothing configured => nothing remembered');
    end);
end;



{-----------------------------------------------------------------------------------------------------------------------
   4. THE STYLE IS REALLY APPLIED
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsDisk.ActiveStyleChangesAfterSuccessfulLoad;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle, FStyleFile);

  WithoutMainForm(
    procedure
    begin
      Assert.AreEqual(TStyleManager.SystemStyleName, TStyleManager.ActiveStyle.Name, 'Precondition: the run must start on the system style');

      FormSkinsDisk.LoadLastStyle('');

      Assert.AreEqual(FStyleName, TStyleManager.ActiveStyle.Name, 'After a successful load TStyleManager must really be on the new style - not merely free of exceptions');
      Assert.AreNotEqual(TStyleManager.SystemStyleName, TStyleManager.ActiveStyle.Name, 'The style must no longer be the Windows default');
    end);
end;



{-----------------------------------------------------------------------------------------------------------------------
   5. THE SELECTOR
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsDisk.Selector_ListsDefaultThemeFirstAndTheVsfFiles;
var
  Form: TfrmStyleDisk;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  try
    Assert.AreEqual(DefWinTheme, Form.lBox.Items[0], 'The first entry must be the default Windows theme');
    Assert.IsTrue(Form.lBox.Items.IndexOf(FStyleFile) > 0, 'The .vsf files found in ' + StylesFolder + ' must be listed. Missing: ' + FStyleFile);
    Assert.IsTrue(Form.lBox.Items.IndexOf(FBrokenFile) > 0, 'PopulateStyles lists by extension, so even an unreadable .vsf shows up');
  finally
    FreeAndNil(Form);
  end;
end;


{ The list holds .vsf FILENAMES while TStyleManager.ActiveStyle.Name is the style's INTERNAL name,
  so preselecting by ActiveStyle.Name silently matched nothing. Found by driving the real app. }
procedure TTestFormSkinsDisk.Selector_PreselectsTheConfiguredStyle;
var
  Form: TfrmStyleDisk;
begin
  LightCore.INIFileQuick.WriteString(IniKeyStyle, FStyleFile);
  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
    end);

  Form:= TfrmStyleDisk.Create(NIL);
  try
    Assert.IsTrue(Form.lBox.ItemIndex >= 0, 'The configured skin must be preselected, otherwise the user cannot see which one he is running');
    Assert.AreEqual(FStyleFile, Form.lBox.Items[Form.lBox.ItemIndex], 'The preselected entry must be the configured skin, matched by FILENAME (not by TStyleManager.ActiveStyle.Name, which is the internal style name)');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormSkinsDisk.Selector_PreselectsDefaultThemeWhenNothingConfigured;
var
  Form: TfrmStyleDisk;
begin
  ClearIniKeys;
  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');
    end);

  Form:= TfrmStyleDisk.Create(NIL);
  try
    Assert.AreEqual(0, Form.lBox.ItemIndex, 'With no skin configured the list must fall back to the first entry (the Windows default theme), never to "nothing selected"');
  finally
    FreeAndNil(Form);
  end;
end;


{ The BUG 5 regression net: picking a skin must ONLY write the INI. The moment this dialog calls
  SetStyle again, the main form's TMainMenuBarStyleHook leaks and the app AVs on the next menu click. }
procedure TTestFormSkinsDisk.Selector_ClickSavesToIniButDoesNotApplyStyleLive;
var
  Form: TfrmStyleDisk;
  StyleBefore: string;
begin
  ClearIniKeys;
  WithoutMainForm(
    procedure
    begin
      FormSkinsDisk.LoadLastStyle('');    { CurrentStyle := '' so the click below is a real change }
    end);

  StyleBefore:= TStyleManager.ActiveStyle.Name;

  Form:= TfrmStyleDisk.Create(NIL);
  try
    Form.lBox.ItemIndex:= Form.lBox.Items.IndexOf(FStyleFile);
    Assert.IsTrue(Form.lBox.ItemIndex >= 0, 'Precondition: the test style must be in the list');

    Form.lBoxClick(Form);

    Assert.AreEqual(FStyleFile, LightCore.INIFileQuick.ReadString(IniKeyStyle, ''), 'Picking a skin must persist it to the INI');
    Assert.AreEqual(FStyleFile, FormSkinsDisk.CurrentStyle, 'Picking a skin must update the remembered choice');
    Assert.AreEqual(StyleBefore, TStyleManager.ActiveStyle.Name, 'The selector must NOT apply the style live - that is what leaks TMainMenuBarStyleHook and crashes on the next menu click');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormSkinsDisk.Selector_ClosingSetsCaFree;
var
  Form: TfrmStyleDisk;
  Action: TCloseAction;
begin
  Form:= TfrmStyleDisk.Create(NIL);
  try
    Action:= caNone;
    Form.FormClose(Form, Action);
    Assert.AreEqual(caFree, Action, 'The dialog must free itself on close, otherwise every open leaks a form');
  finally
    FreeAndNil(Form);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSkinsDisk);

end.
