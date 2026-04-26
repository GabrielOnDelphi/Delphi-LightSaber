unit Test.LightFmx.Visual.SvgFlatButton;

{=============================================================================================================
   Unit tests for LightFmx.Visual.SvgFlatButton.pas
   Tests the TSvgButton FMX component — public surface, state transitions, compact logic.

   Notes:
     - Mouse-driven hover tests (DoMouseEnter/Leave) require a real FMX message loop; we test the
       deterministic paths only (Toggle, Compact, IconPosition, SvgData, Text).
     - Compact tests force-bypass AutoCompact so we can drive the state machine directly.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Layouts,
  LightFmx.Visual.SvgFlatButton;

TYPE
  [TestFixture]
  TTestSvgButton = class
  strict private
    FButton: TSvgButton;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Construction defaults }
    [Test] procedure TestCreate_Defaults;
    [Test] procedure TestCreate_IconStartsHidden;
    [Test] procedure TestCreate_LabelExists;

    { Text }
    [Test] procedure TestSetText_Roundtrip;
    [Test] procedure TestSetText_Empty;

    { SvgData }
    [Test] procedure TestSetSvgData_ShowsIcon;
    [Test] procedure TestSetSvgData_EmptyHidesIcon;
    [Test] procedure TestLoadSvgPath_SameAsSvgData;
    [Test] procedure TestSvgData_IpCenter_LabelHiddenWhenIconShown;
    [Test] procedure TestSvgData_IpCenter_LabelShownWhenNoIcon;

    { IsToggled }
    [Test] procedure TestIsToggled_DefaultsFalse;
    [Test] procedure TestIsToggled_RoundTrip;
    [Test] procedure TestIsToggled_SameValue_NoChange;

    { IconPosition }
    [Test] procedure TestIconPosition_DefaultIpLeft;
    [Test] procedure TestIconPosition_SetIpTop_LabelAlignBottom;
    [Test] procedure TestIconPosition_SetIpCenter_IconAlignClient;
    [Test] procedure TestIconPosition_SetIpLeft_IconAlignLeft;

    { HoverBackground }
    [Test] procedure TestHoverBackground_DefaultsTrue;
    [Test] procedure TestHoverBackground_RoundTrip;

    { Compact / AutoCompact }
    [Test] procedure TestCompact_DefaultsFalse;
    [Test] procedure TestAutoCompact_DefaultsFalse;
    [Test] procedure TestCompactThreshold_DefaultsTablet;

    [Test] procedure TestSetCompact_True_SwitchesToIpCenter;
    [Test] procedure TestSetCompact_False_RestoresPosition;
    [Test] procedure TestSetCompact_PreservesIconPosition;
    [Test] procedure TestSetCompact_PreservesWidth;
    [Test] procedure TestSetCompact_FiresEvent;
    [Test] procedure TestSetCompact_SameValue_DoesNotFireEvent;
    [Test] procedure TestSetCompact_IgnoredWhenAutoCompact;
    [Test] procedure TestSetIconPosition_WhileCompacted_SurvivesExpand;
    [Test] procedure TestSetAutoCompact_TurningOffWhileCompacted_RestoresExpanded;

    [Test] procedure TestCompactSvgButtons_RecursesIntoChildren;
    [Test] procedure TestCompactSvgButtons_SkipsAutoCompactButtons;

    { Misc }
    [Test] procedure TestApplyThemeColors_DoesNotRaise;
    [Test] procedure TestRegister_NoException;
  end;


  { Helper to spy on the OnCompactChanged event }
  TCompactSpy = class
  public
    Fired: Boolean;
    LastIsCompact: Boolean;
    Count: Integer;
    procedure Handle(Sender: TObject; IsCompact: Boolean);
  end;


IMPLEMENTATION


{ TCompactSpy }
procedure TCompactSpy.Handle(Sender: TObject; IsCompact: Boolean);
begin
  Fired:= True;
  LastIsCompact:= IsCompact;
  Inc(Count);
end;


{ TTestSvgButton }
procedure TTestSvgButton.Setup;
begin
  FButton:= TSvgButton.Create(NIL);
  FButton.Width:= 120;
  FButton.Height:= 40;
end;


procedure TTestSvgButton.TearDown;
begin
  FreeAndNil(FButton);
end;


{ Construction }
procedure TTestSvgButton.TestCreate_Defaults;
begin
  Assert.IsNotNull(FButton);
  Assert.IsFalse(FButton.IsToggled,        'IsToggled should default to FALSE');
  Assert.IsTrue (FButton.HoverBackground,  'HoverBackground should default to TRUE');
  Assert.IsFalse(FButton.Compact,          'Compact should default to FALSE');
  Assert.IsFalse(FButton.AutoCompact,      'AutoCompact should default to FALSE');
  Assert.AreEqual(Ord(ipLeft),  Ord(FButton.IconPosition),     'IconPosition default ipLeft');
  Assert.AreEqual(Ord(ctTablet),Ord(FButton.CompactThreshold), 'CompactThreshold default ctTablet');
end;


procedure TTestSvgButton.TestCreate_IconStartsHidden;
begin
  Assert.IsFalse(FButton.Icon.Visible, 'Icon should be hidden until SvgData is set');
end;


procedure TTestSvgButton.TestCreate_LabelExists;
begin
  Assert.IsNotNull(FButton.TextLabel);
  Assert.AreEqual('', FButton.TextLabel.Text, 'Label text should start empty (Name=''Caption'' was reset)');
end;


{ Text }
procedure TTestSvgButton.TestSetText_Roundtrip;
begin
  FButton.Text:= 'Settings';
  Assert.AreEqual('Settings', FButton.Text);
  Assert.AreEqual('Settings', FButton.TextLabel.Text, 'Label text should mirror Text property');
end;


procedure TTestSvgButton.TestSetText_Empty;
begin
  FButton.Text:= 'Hello';
  FButton.Text:= '';
  Assert.AreEqual('', FButton.Text);
end;


{ SvgData }
procedure TTestSvgButton.TestSetSvgData_ShowsIcon;
const SAMPLE_PATH = 'M0,0 L10,10';
begin
  FButton.SvgData:= SAMPLE_PATH;
  { TPath.Data.Data round-trips with a trailing whitespace token, so trim before comparing. }
  Assert.AreEqual(SAMPLE_PATH, Trim(FButton.SvgData), 'SvgData getter should round-trip');
  Assert.IsTrue(FButton.Icon.Visible, 'Icon should become visible when SvgData set');
end;


procedure TTestSvgButton.TestSetSvgData_EmptyHidesIcon;
begin
  FButton.SvgData:= 'M0,0 L1,1';
  Assert.IsTrue(FButton.Icon.Visible, 'precondition: icon visible');
  FButton.SvgData:= '';
  Assert.IsFalse(FButton.Icon.Visible, 'Icon should hide when SvgData cleared');
end;


procedure TTestSvgButton.TestLoadSvgPath_SameAsSvgData;
const PATH_A = 'M0,0 L20,20';
begin
  FButton.LoadSvgPath(PATH_A);
  Assert.AreEqual(PATH_A, Trim(FButton.SvgData), 'LoadSvgPath should be equivalent to setting SvgData');
end;


procedure TTestSvgButton.TestSvgData_IpCenter_LabelHiddenWhenIconShown;
begin
  FButton.IconPosition:= ipCenter;
  FButton.SvgData:= 'M0,0 L1,1';
  Assert.IsTrue (FButton.Icon.Visible,      'Icon should be visible');
  Assert.IsFalse(FButton.TextLabel.Visible, 'Label should be hidden in ipCenter when icon is visible');
end;


procedure TTestSvgButton.TestSvgData_IpCenter_LabelShownWhenNoIcon;
begin
  FButton.IconPosition:= ipCenter;
  FButton.SvgData:= '';
  Assert.IsTrue(FButton.TextLabel.Visible, 'Label should be visible in ipCenter when there is no icon');
end;


{ IsToggled }
procedure TTestSvgButton.TestIsToggled_DefaultsFalse;
begin
  Assert.IsFalse(FButton.IsToggled);
end;


procedure TTestSvgButton.TestIsToggled_RoundTrip;
begin
  FButton.IsToggled:= True;
  Assert.IsTrue(FButton.IsToggled);
  FButton.IsToggled:= False;
  Assert.IsFalse(FButton.IsToggled);
end;


procedure TTestSvgButton.TestIsToggled_SameValue_NoChange;
begin
  FButton.IsToggled:= False;
  FButton.IsToggled:= False;
  Assert.IsFalse(FButton.IsToggled);
end;


{ IconPosition }
procedure TTestSvgButton.TestIconPosition_DefaultIpLeft;
begin
  Assert.AreEqual(Ord(ipLeft), Ord(FButton.IconPosition));
  Assert.AreEqual(Ord(TAlignLayout.Left), Ord(FButton.Icon.Align), 'Icon Align should be Left for ipLeft');
end;


procedure TTestSvgButton.TestIconPosition_SetIpTop_LabelAlignBottom;
begin
  FButton.IconPosition:= ipTop;
  Assert.AreEqual(Ord(ipTop), Ord(FButton.IconPosition));
  Assert.AreEqual(Ord(TAlignLayout.Top),    Ord(FButton.Icon.Align),       'Icon should align Top');
  Assert.AreEqual(Ord(TAlignLayout.Bottom), Ord(FButton.TextLabel.Align),  'Label should align Bottom');
end;


procedure TTestSvgButton.TestIconPosition_SetIpCenter_IconAlignClient;
begin
  FButton.IconPosition:= ipCenter;
  Assert.AreEqual(Ord(ipCenter), Ord(FButton.IconPosition));
  Assert.AreEqual(Ord(TAlignLayout.Client), Ord(FButton.Icon.Align), 'Icon should align Client in ipCenter');
end;


procedure TTestSvgButton.TestIconPosition_SetIpLeft_IconAlignLeft;
begin
  FButton.IconPosition:= ipTop;     { switch away first to make sure setter does work }
  FButton.IconPosition:= ipLeft;
  Assert.AreEqual(Ord(TAlignLayout.Left),   Ord(FButton.Icon.Align),      'Icon should align Left');
  Assert.AreEqual(Ord(TAlignLayout.Client), Ord(FButton.TextLabel.Align), 'Label should align Client');
  Assert.IsTrue(FButton.TextLabel.Visible, 'Label visible in ipLeft');
end;


{ HoverBackground }
procedure TTestSvgButton.TestHoverBackground_DefaultsTrue;
begin
  Assert.IsTrue(FButton.HoverBackground);
end;


procedure TTestSvgButton.TestHoverBackground_RoundTrip;
begin
  FButton.HoverBackground:= False;
  Assert.IsFalse(FButton.HoverBackground);
  FButton.HoverBackground:= True;
  Assert.IsTrue(FButton.HoverBackground);
end;


{ Compact / AutoCompact }
procedure TTestSvgButton.TestCompact_DefaultsFalse;
begin
  Assert.IsFalse(FButton.Compact);
end;


procedure TTestSvgButton.TestAutoCompact_DefaultsFalse;
begin
  Assert.IsFalse(FButton.AutoCompact);
end;


procedure TTestSvgButton.TestCompactThreshold_DefaultsTablet;
begin
  Assert.AreEqual(Ord(ctTablet), Ord(FButton.CompactThreshold));
end;


procedure TTestSvgButton.TestSetCompact_True_SwitchesToIpCenter;
begin
  FButton.IconPosition:= ipLeft;
  FButton.Compact:= True;
  Assert.IsTrue(FButton.Compact, 'Compact flag should be set');
  Assert.AreEqual(Ord(ipCenter), Ord(FButton.IconPosition), 'IconPosition should switch to ipCenter');
end;


procedure TTestSvgButton.TestSetCompact_False_RestoresPosition;
begin
  FButton.IconPosition:= ipLeft;
  FButton.Compact:= True;
  FButton.Compact:= False;
  Assert.IsFalse(FButton.Compact);
  Assert.AreEqual(Ord(ipLeft), Ord(FButton.IconPosition), 'IconPosition should be restored on expand');
end;


procedure TTestSvgButton.TestSetCompact_PreservesIconPosition;
begin
  FButton.IconPosition:= ipTop;
  FButton.Compact:= True;
  FButton.Compact:= False;
  Assert.AreEqual(Ord(ipTop), Ord(FButton.IconPosition), 'Original IconPosition (ipTop) should round-trip');
end;


procedure TTestSvgButton.TestSetCompact_PreservesWidth;
begin
  FButton.Align:= TAlignLayout.None;   { Width is mutable when Align is None }
  FButton.Width:= 200;
  FButton.Compact:= True;
  Assert.AreEqual(Single(FButton.Height), Single(FButton.Width), 'Compact should make button square (Width=Height)');
  FButton.Compact:= False;
  Assert.AreEqual(Single(200), Single(FButton.Width), 'Original Width should round-trip');
end;


procedure TTestSvgButton.TestSetCompact_FiresEvent;
var Spy: TCompactSpy;
begin
  Spy:= TCompactSpy.Create;
  try
    FButton.OnCompactChanged:= Spy.Handle;
    FButton.Compact:= True;
    Assert.IsTrue (Spy.Fired,         'Event should fire on compact change');
    Assert.IsTrue (Spy.LastIsCompact, 'Event should report IsCompact=True');
    Assert.AreEqual(1, Spy.Count);
  finally
    FreeAndNil(Spy);
  end;
end;


procedure TTestSvgButton.TestSetCompact_SameValue_DoesNotFireEvent;
var Spy: TCompactSpy;
begin
  Spy:= TCompactSpy.Create;
  try
    FButton.OnCompactChanged:= Spy.Handle;
    FButton.Compact:= False;     { already False }
    Assert.IsFalse(Spy.Fired, 'Event should not fire when value unchanged');
    Assert.AreEqual(0, Spy.Count);
  finally
    FreeAndNil(Spy);
  end;
end;


procedure TTestSvgButton.TestSetCompact_IgnoredWhenAutoCompact;
begin
  FButton.AutoCompact:= True;
  FButton.Compact:= True;     { should be silently ignored }
  Assert.IsFalse(FButton.Compact, 'Manual Compact should be ignored when AutoCompact=True');
end;


procedure TTestSvgButton.TestSetIconPosition_WhileCompacted_SurvivesExpand;
begin
  FButton.IconPosition:= ipLeft;
  FButton.Compact:= True;
  Assert.AreEqual(Ord(ipCenter), Ord(FButton.IconPosition), 'precondition: visual is ipCenter while compacted');

  { User changes IconPosition while compacted — visual should stay compact, value queued for expand }
  FButton.IconPosition:= ipTop;
  Assert.AreEqual(Ord(ipCenter), Ord(FButton.IconPosition), 'Visual should remain ipCenter while compacted');

  FButton.Compact:= False;
  Assert.AreEqual(Ord(ipTop), Ord(FButton.IconPosition), 'On expand, the value the user set while compacted should win');
end;


procedure TTestSvgButton.TestSetAutoCompact_TurningOffWhileCompacted_RestoresExpanded;
begin
  FButton.IconPosition:= ipLeft;
  FButton.Compact:= True;
  Assert.IsTrue(FButton.Compact, 'precondition: button compacted manually');

  FButton.AutoCompact:= True;       { takes ownership; no scene attached so it does not flip state }
  FButton.AutoCompact:= False;      { turning off should auto-expand if currently compacted }

  Assert.IsFalse(FButton.Compact, 'Turning AutoCompact off should restore expanded state');
  Assert.AreEqual(Ord(ipLeft), Ord(FButton.IconPosition), 'Original IconPosition should be restored');
end;


procedure TTestSvgButton.TestCompactSvgButtons_RecursesIntoChildren;
var
  Container: TLayout;
  ChildBtn:  TSvgButton;
begin
  Container:= TLayout.Create(NIL);
  try
    ChildBtn:= TSvgButton.Create(Container);
    ChildBtn.Parent:= Container;

    Assert.IsFalse(ChildBtn.Compact, 'Precondition: child not compact');
    CompactSvgButtons(Container, True);
    Assert.IsTrue(ChildBtn.Compact, 'Recursive helper should compact descendant TSvgButton');

    CompactSvgButtons(Container, False);
    Assert.IsFalse(ChildBtn.Compact, 'Recursive helper should expand again');
  finally
    FreeAndNil(Container);   { Container owns ChildBtn }
  end;
end;


procedure TTestSvgButton.TestCompactSvgButtons_SkipsAutoCompactButtons;
var
  Container: TLayout;
  ChildBtn:  TSvgButton;
begin
  Container:= TLayout.Create(NIL);
  try
    ChildBtn:= TSvgButton.Create(Container);
    ChildBtn.Parent:= Container;
    ChildBtn.AutoCompact:= True;     { takes ownership of state }

    CompactSvgButtons(Container, True);
    Assert.IsFalse(ChildBtn.Compact, 'AutoCompact button must be skipped by helper');
  finally
    FreeAndNil(Container);
  end;
end;


{ Misc }
procedure TTestSvgButton.TestApplyThemeColors_DoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FButton.ApplyThemeColors;
    end,
    Exception,
    'ApplyThemeColors should be safe to call without an active style');
end;


procedure TTestSvgButton.TestRegister_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Register;
    end,
    Exception,
    'Register should not raise');
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestSvgButton);

end.
