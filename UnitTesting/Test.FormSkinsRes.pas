unit Test.FormSkinsRes;

{=============================================================================================================
   Regression tests for FrameVCL\FormSkinsRes.pas - the resource-based VCL skin loader.

   This unit's contract is DELIBERATELY different from FormSkinsDisk's (full reasoning in Docs\Skins-VCL.md):
     * NO "before CreateMainForm" guard. Its consumers call LoadLastStyle from inside the main form,
       one of them from FormPostInitialize. A guard here would raise at startup - invisibly, because
       those apps set Application.ShowMainForm := FALSE.
     * INI key is 'LastSkin' and it stores a style NAME, not a .vsf filename.
     * TrySetStyle, not SetStyle: a style that is no longer linked into the EXE must not kill startup.
     * It DOES apply styles live (that is what FormSkinsDisk was moved away from).
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
  TTestFormSkinsRes = class
  private
    procedure RestoreWindowsStyle;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    { The contract difference from FormSkinsDisk }
    [Test] procedure LoadLastStyle_DoesNotRaiseWithMainFormAlive;
    [Test] procedure LoadLastStyle_UnknownStyleNameDoesNotRaiseAndKeepsCurrentStyle;

    { INI }
    [Test] procedure Ini_ReadsTheLastSkinKey;
    [Test] procedure Ini_DefaultUsedWhenKeyIsEmpty;
    [Test] procedure Ini_DefWinThemeAppliesNoStyle;

    { The style is really applied }
    [Test] procedure ActiveStyleMatchesTheLoadedName;

    { The selector }
    [Test] procedure Selector_ListsExactlyTheStylesLinkedInTheExe;
    [Test] procedure Selector_PreselectsTheActiveStyle;
    [Test] procedure Selector_ClickRecordsTheChoiceAndReEnablesTheList;
    [Test] procedure Selector_ClosingSetsCaFree;
  end;

implementation

uses
  LightCore.INIFileQuick,
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FormSkinsRes;


procedure TTestFormSkinsRes.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  AppData.RamLog.ShowOnError:= FALSE;
end;


procedure TTestFormSkinsRes.TearDown;
begin
  RestoreWindowsStyle;
  LightCore.INIFileQuick.WriteString(IniKeySkin, '');
end;


procedure TTestFormSkinsRes.RestoreWindowsStyle;
begin
  if TStyleManager.ActiveStyle.Name <> TStyleManager.SystemStyleName
  then TStyleManager.SetStyle(TStyleManager.SystemStyleName);
end;



{-----------------------------------------------------------------------------------------------------------------------
   THE CONTRACT DIFFERENCE
-----------------------------------------------------------------------------------------------------------------------}

{ FormSkinsDisk.LoadLastStyle raises here; this one must not. Do not "unify" the two units. }
procedure TTestFormSkinsRes.LoadLastStyle_DoesNotRaiseWithMainFormAlive;
begin
  Assert.IsNotNull(Application.MainForm, 'Precondition: a main form must exist for this test to mean anything');
  LightCore.INIFileQuick.WriteString(IniKeySkin, '');

  Assert.WillNotRaise(
    procedure
    begin
      FormSkinsRes.LoadLastStyle('');
    end,
    Exception,
    'FormSkinsRes must have NO before-CreateMainForm guard: QuickSilver JpgCompressor calls it from FormPostInitialize, where the main form already exists');
end;


{ Proves the TrySetStyle choice: SetStyle would raise ECustomStyleException and kill startup as soon as
  a build stops linking a .vsf that a user had already selected. }
procedure TTestFormSkinsRes.LoadLastStyle_UnknownStyleNameDoesNotRaiseAndKeepsCurrentStyle;
var
  Before: string;
begin
  Before:= TStyleManager.ActiveStyle.Name;
  LightCore.INIFileQuick.WriteString(IniKeySkin, 'Style that is not linked into this exe');

  FormSkinsRes.LoadLastStyle('');

  Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'An unknown style name must leave the active style untouched');
  Assert.AreEqual('Style that is not linked into this exe', FormSkinsRes.CurrentStyle, 'The name is still remembered, so the user sees his own choice in the selector');
end;



{-----------------------------------------------------------------------------------------------------------------------
   INI
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsRes.Ini_ReadsTheLastSkinKey;
begin
  LightCore.INIFileQuick.WriteString(IniKeySkin, TStyleManager.SystemStyleName);

  FormSkinsRes.LoadLastStyle('Some other default');

  Assert.AreEqual(TStyleManager.SystemStyleName, FormSkinsRes.CurrentStyle, 'The INI value must win over the caller''s default');
end;


procedure TTestFormSkinsRes.Ini_DefaultUsedWhenKeyIsEmpty;
begin
  LightCore.INIFileQuick.WriteString(IniKeySkin, '');

  FormSkinsRes.LoadLastStyle(TStyleManager.SystemStyleName);

  Assert.AreEqual(TStyleManager.SystemStyleName, FormSkinsRes.CurrentStyle, 'With no INI value the caller''s default must be used (first run)');
end;


procedure TTestFormSkinsRes.Ini_DefWinThemeAppliesNoStyle;
var
  Before: string;
begin
  Before:= TStyleManager.ActiveStyle.Name;
  LightCore.INIFileQuick.WriteString(IniKeySkin, DefWinTheme);

  FormSkinsRes.LoadLastStyle('');

  Assert.AreEqual(Before, TStyleManager.ActiveStyle.Name, 'DefWinTheme means "apply nothing" - it must not reach TStyleManager');
  Assert.AreEqual(DefWinTheme, FormSkinsRes.CurrentStyle, 'DefWinTheme must be remembered as the current choice');
end;



{-----------------------------------------------------------------------------------------------------------------------
   THE STYLE IS REALLY APPLIED
-----------------------------------------------------------------------------------------------------------------------}

{ The test EXE links no .vsf resources, so the only guaranteed style is the system one. Assert on the
  real observable anyway: after LoadLastStyle the active style IS the one that was asked for. }
procedure TTestFormSkinsRes.ActiveStyleMatchesTheLoadedName;
begin
  LightCore.INIFileQuick.WriteString(IniKeySkin, TStyleManager.SystemStyleName);

  FormSkinsRes.LoadLastStyle('');

  Assert.AreEqual(FormSkinsRes.CurrentStyle, TStyleManager.ActiveStyle.Name, 'The remembered style and the style TStyleManager actually applied must be the same');
end;



{-----------------------------------------------------------------------------------------------------------------------
   THE SELECTOR
-----------------------------------------------------------------------------------------------------------------------}

procedure TTestFormSkinsRes.Selector_ListsExactlyTheStylesLinkedInTheExe;
var
  Form: TfrmSkinRes;
  StyleName: string;
  Expected: Integer;
begin
  Expected:= 0;
  for StyleName in TStyleManager.StyleNames do
    Inc(Expected);

  Form:= TfrmSkinRes.Create(NIL);
  try
    Assert.AreEqual(Expected, Form.lBox.Items.Count, 'The list must show every style linked into the EXE and nothing else');
    Assert.IsTrue(Form.lBox.Items.IndexOf(TStyleManager.SystemStyleName) >= 0, 'The system style is always linked, so it must always be listed');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormSkinsRes.Selector_PreselectsTheActiveStyle;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  try
    Assert.IsTrue(Form.lBox.ItemIndex >= 0, 'The active style must be preselected, otherwise the user cannot see what he is running');
    Assert.AreEqual(TStyleManager.ActiveStyle.Name, Form.lBox.Items[Form.lBox.ItemIndex], 'The preselected entry must be the active style');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormSkinsRes.Selector_ClickRecordsTheChoiceAndReEnablesTheList;
var
  Form: TfrmSkinRes;
begin
  Form:= TfrmSkinRes.Create(NIL);
  try
    Form.lBox.ItemIndex:= Form.lBox.Items.IndexOf(TStyleManager.SystemStyleName);
    Assert.IsTrue(Form.lBox.ItemIndex >= 0, 'Precondition: the system style must be in the list');

    Form.lBoxClick(Form);

    Assert.AreEqual(TStyleManager.SystemStyleName, FormSkinsRes.CurrentStyle, 'Clicking an entry must record it as the current choice');
    Assert.AreEqual(TStyleManager.SystemStyleName, TStyleManager.ActiveStyle.Name, 'This unit applies the style LIVE - the active style must follow the click');
    Assert.IsTrue(Form.lBox.Enabled, 'The list is disabled during the switch and MUST be re-enabled by the finally block');
  finally
    FreeAndNil(Form);
  end;
end;


procedure TTestFormSkinsRes.Selector_ClosingSetsCaFree;
var
  Form: TfrmSkinRes;
  Action: TCloseAction;
begin
  Form:= TfrmSkinRes.Create(NIL);
  try
    Action:= caNone;
    Form.FormClose(Form, Action);
    Assert.AreEqual(caFree, Action, 'The dialog must free itself on close, otherwise every open leaks a form');
  finally
    FreeAndNil(Form);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFormSkinsRes);

end.
