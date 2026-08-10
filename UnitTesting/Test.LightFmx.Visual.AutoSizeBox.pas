unit Test.LightFmx.Visual.AutoSizeBox;

{=============================================================================================================
   Unit tests for LightFmx.Visual.AutoSizeBox.pas and LightFmx.Visual.AutosizeBoxText.pas
   Tests component creation, property setters, and basic behavior.

   Note: Full visual testing requires a running FMX application.
   These tests focus on non-visual logic and property behavior.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,        { TAlphaColor, TAlphaColorRec }
  FMX.Types,
  FMX.Controls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Forms,
  LightFmx.Visual.AutoSizeBox,
  LightFmx.Visual.AutosizeBoxText;

type
  [TestFixture]
  TTestAutoSizeBox = class
  private
    FParent: TLayout;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TAutosizeBoxText Creation Tests }
    [Test]
    procedure TestCreate_WithOwner;

    [Test]
    procedure TestCreate_DefaultBoxType;

    [Test]
    procedure TestCreate_DefaultAlignment;

    [Test]
    procedure TestCreate_HasShadowEffect;

    [Test]
    procedure TestCreate_DefaultColors;

    { BoxType Property Tests }
    [Test]
    procedure TestSetBoxType_User;

    [Test]
    procedure TestSetBoxType_Model;

    [Test]
    procedure TestSetBoxType_UserMargins;

    [Test]
    procedure TestSetBoxType_ModelMargins;

    [Test]
    procedure TestSetBoxType_UserColor;

    [Test]
    procedure TestSetBoxType_ModelColor;

    { TAutosizeBoxText Specific Tests }
    [Test]
    procedure TestText_SetAndGet;

    [Test]
    procedure TestText_EmptyString;

    [Test]
    procedure TestText_NoChangeIfSame;

    [Test]
    procedure TestText_DefaultNotEmpty;

    { MakeTextBubble Factory Tests }
    [Test]
    procedure TestMakeTextBubble_CreatesInstance;

    [Test]
    procedure TestMakeTextBubble_SetsParent;

    [Test]
    procedure TestMakeTextBubble_SetsBoxType;

    [Test]
    procedure TestMakeTextBubble_SetsText;

    [Test]
    procedure TestMakeTextBubble_StoredIsFalse;

    [Test]
    procedure TestMakeTextBubble_HighPositionY;

    { Constants Tests }
    [Test]
    procedure TestConstants_WhatsAppGreen;

    [Test]
    procedure TestConstants_WhatsAppGrey;

    [Test]
    procedure TestConstants_ResizeTolerance;

    [Test]
    procedure TestConstants_TextHeightBuffer;

    { UpdateSize Tests }
    [Test]
    procedure TestUpdateSize_ZeroWidthDoesNotCrash;

    [Test]
    procedure TestUpdateSize_NegativeWidthDoesNotCrash;

    { Parent Width Calculation Tests }
    [Test]
    procedure TestGetParentContentWidth_WithParent;

    [Test]
    procedure TestGetParentContentWidth_NoParent;
  end;

implementation


procedure TTestAutoSizeBox.Setup;
begin
  FParent:= TLayout.Create(nil);
  FParent.Width:= 400;
  FParent.Height:= 600;
end;


procedure TTestAutoSizeBox.TearDown;
begin
  FreeAndNil(FParent);
end;


{ TAutosizeBoxText Creation Tests }

procedure TTestAutoSizeBox.TestCreate_WithOwner;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    Assert.IsNotNull(Box, 'Box should be created');
    Assert.AreSame(FParent, Box.Owner, 'Owner should be set correctly');   { AreSame, not AreEqual: TLayout vs TComponent are different types, so AreEqual<T> cannot infer T }
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestCreate_DefaultBoxType;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    Assert.AreEqual(bxModel, Box.BoxType, 'Default BoxType should be bxModel');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestCreate_DefaultAlignment;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    Assert.AreEqual(TAlignLayout.Top, Box.Align, 'Default alignment should be Top');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestCreate_HasShadowEffect;
var
  Box: TAutosizeBoxText;
  i: Integer;
  HasShadow: Boolean;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    HasShadow:= FALSE;
    for i:= 0 to Box.ChildrenCount - 1 do
      if Box.Children[i].ClassName = 'TShadowEffect' then
        HasShadow:= TRUE;
    Assert.IsTrue(HasShadow, 'Box should have a shadow effect');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestCreate_DefaultColors;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    // Default is bxModel, which uses grey after setBoxType is called
    // But constructor sets green initially, then setBoxType changes it
    Assert.AreEqual(WhatsAppGreen, Box.Fill.Color, 'Initial color should be WhatsAppGreen');
  finally
    FreeAndNil(Box);
  end;
end;


{ BoxType Property Tests }

procedure TTestAutoSizeBox.TestSetBoxType_User;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxUser;
    Assert.AreEqual(bxUser, Box.BoxType, 'BoxType should be bxUser');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestSetBoxType_Model;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxModel;
    Assert.AreEqual(bxModel, Box.BoxType, 'BoxType should be bxModel');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestSetBoxType_UserMargins;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxUser;
    // User: wide LEFT margin (40) pushes bubble to right
    Assert.AreEqual(Single(40), Box.Margins.Left, 'User left margin should be 40');
    Assert.AreEqual(Single(5), Box.Margins.Right, 'User right margin should be 5');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestSetBoxType_ModelMargins;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxModel;
    // Model: wide RIGHT margin (40) pushes bubble to left
    Assert.AreEqual(Single(5), Box.Margins.Left, 'Model left margin should be 5');
    Assert.AreEqual(Single(40), Box.Margins.Right, 'Model right margin should be 40');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestSetBoxType_UserColor;
var
  Box: TAutosizeBoxText;
  UserColor, ModelColor: TAlphaColor;
begin
  { setBoxType DERIVES the fill from the active style's background (GetThemeBackgroundColor ->
    DeriveBubbleColor) and falls back to WhatsAppGreen/WhatsAppGrey only when no style background is
    available. A test process always has a style loaded, so asserting the fallback constant here would
    check the branch that does NOT run. What the component promises in both branches is that the user
    and model bubbles stay visually distinct and fully opaque. }
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxUser;
    UserColor:= Box.Fill.Color;

    Box.BoxType:= bxModel;
    ModelColor:= Box.Fill.Color;

    Assert.AreNotEqual(Integer(UserColor), Integer(ModelColor), 'User and model bubbles must not share a colour');
    Assert.AreEqual(Integer($FF), Integer(TAlphaColorRec(UserColor).A), 'User bubble must be fully opaque');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestSetBoxType_ModelColor;
var
  Box: TAutosizeBoxText;
  DefaultColor, ModelColor, ContentColor: TAlphaColor;
begin
  { Same reason as TestSetBoxType_UserColor: the fill is style-derived, so only the relationships between
    the three box types are stable enough to assert. }
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    DefaultColor:= Box.Fill.Color;          { the constructor sets WhatsAppGreen before any BoxType is applied }

    Box.BoxType:= bxModel;
    ModelColor:= Box.Fill.Color;

    Box.BoxType:= bxContent;
    ContentColor:= Box.Fill.Color;

    Assert.AreNotEqual(Integer(DefaultColor), Integer(ModelColor), 'Setting bxModel must repaint the bubble');
    Assert.AreNotEqual(Integer(ModelColor), Integer(ContentColor), 'Model and content bubbles must not share a colour');
    Assert.AreEqual(Integer($FF), Integer(TAlphaColorRec(ModelColor).A), 'Model bubble must be fully opaque');
  finally
    FreeAndNil(Box);
  end;
end;


{ TAutosizeBoxText Specific Tests }

procedure TTestAutoSizeBox.TestText_SetAndGet;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Text:= 'Hello World';
    Assert.AreEqual('Hello World', Box.Text, 'Text should be set correctly');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestText_EmptyString;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Text:= '';
    Assert.AreEqual('', Box.Text, 'Empty text should be allowed');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestText_NoChangeIfSame;
var
  Box: TAutosizeBoxText;
  OriginalText: string;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Text:= 'Test';
    OriginalText:= Box.Text;
    Box.Text:= 'Test';  // Same value
    Assert.AreEqual(OriginalText, Box.Text, 'Text should remain unchanged');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestText_DefaultNotEmpty;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  try
    Assert.IsTrue(Length(Box.Text) > 0, 'Default text should not be empty');
    Assert.IsTrue(Pos('WhatsApp', Box.Text) > 0, 'Default text should mention WhatsApp');
  finally
    FreeAndNil(Box);
  end;
end;


{ MakeTextBubble Factory Tests }

procedure TTestAutoSizeBox.TestMakeTextBubble_CreatesInstance;
var
  Bubble: TAutosizeBoxText;
begin
  Bubble:= MakeTextBubble(FParent, 'Test', bxUser);
  try
    Assert.IsNotNull(Bubble, 'MakeTextBubble should create an instance');
  finally
    FreeAndNil(Bubble);
  end;
end;


procedure TTestAutoSizeBox.TestMakeTextBubble_SetsParent;
var
  Bubble: TAutosizeBoxText;
begin
  Bubble:= MakeTextBubble(FParent, 'Test', bxUser);
  try
    Assert.AreSame(FParent, Bubble.Parent, 'Parent should be set');        { AreSame, not AreEqual: TLayout vs TFmxObject are different types }
  finally
    FreeAndNil(Bubble);
  end;
end;


procedure TTestAutoSizeBox.TestMakeTextBubble_SetsBoxType;
var
  Bubble: TAutosizeBoxText;
begin
  Bubble:= MakeTextBubble(FParent, 'Test', bxUser);
  try
    Assert.AreEqual(bxUser, Bubble.BoxType, 'BoxType should be bxUser');
  finally
    FreeAndNil(Bubble);
  end;

  Bubble:= MakeTextBubble(FParent, 'Test', bxModel);
  try
    Assert.AreEqual(bxModel, Bubble.BoxType, 'BoxType should be bxModel');
  finally
    FreeAndNil(Bubble);
  end;
end;


procedure TTestAutoSizeBox.TestMakeTextBubble_SetsText;
var
  Bubble: TAutosizeBoxText;
begin
  Bubble:= MakeTextBubble(FParent, 'Hello World', bxUser);
  try
    Assert.AreEqual('Hello World', Bubble.Text, 'Text should be set');
  finally
    FreeAndNil(Bubble);
  end;
end;


procedure TTestAutoSizeBox.TestMakeTextBubble_StoredIsFalse;
var
  Bubble: TAutosizeBoxText;
begin
  Bubble:= MakeTextBubble(FParent, 'Test', bxUser);
  try
    Assert.IsFalse(Bubble.Stored, 'Stored should be FALSE');
  finally
    FreeAndNil(Bubble);
  end;
end;


procedure TTestAutoSizeBox.TestMakeTextBubble_HighPositionY;
var
  First, Second: TAutosizeBoxText;
begin
  { MakeTextBubble sets Position.Y := 99999999 so the new bubble sorts LAST among the parent's children.
    That number is a sort key, not a final coordinate: TAutoSizeBox is created with Align = TAlignLayout.Top
    (LightFmx.Visual.AutoSizeBox.pas:73), so the parent's Realign immediately replaces Y with the real
    stacked position. Asserting Y > 1000000 therefore tested a value that never survives. The observable
    contract is the ORDER — a bubble made later sits below one made earlier. }
  First := MakeTextBubble(FParent, 'First bubble', bxUser);
  Second:= MakeTextBubble(FParent, 'Second bubble', bxUser);
  try
    Assert.IsTrue(First.Height > 0, 'Bubble must have a real height, otherwise the order check below is vacuous');
    Assert.IsTrue(Second.Position.Y > First.Position.Y, 'A newly made bubble must stack below the previous one');
  finally
    FreeAndNil(Second);
    FreeAndNil(First);
  end;
end;


{ Constants Tests }

procedure TTestAutoSizeBox.TestConstants_WhatsAppGreen;
begin
  Assert.AreEqual(Cardinal($FFE4F3E2), WhatsAppGreen, 'WhatsAppGreen constant should be correct');
end;


procedure TTestAutoSizeBox.TestConstants_WhatsAppGrey;
begin
  Assert.AreEqual(Cardinal($FFEFEFEF), WhatsAppGrey, 'WhatsAppGrey constant should be correct');
end;


procedure TTestAutoSizeBox.TestConstants_ResizeTolerance;
begin
  Assert.AreEqual(1, CResizeTolerance, 'CResizeTolerance should be 1');
end;


procedure TTestAutoSizeBox.TestConstants_TextHeightBuffer;
begin
  Assert.AreEqual(Single(1.0), CTextHeightBuffer, 0.001, 'CTextHeightBuffer should be 1.0');
end;


{ UpdateSize Tests }

procedure TTestAutoSizeBox.TestUpdateSize_ZeroWidthDoesNotCrash;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Width:= 0;
    Box.UpdateSize;
    Assert.IsTrue(Box.Height >= 0, 'Height should be non-negative after UpdateSize with zero width');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestUpdateSize_NegativeWidthDoesNotCrash;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.Width:= -10;
    Box.UpdateSize;
    Assert.IsTrue(Box.Height >= 0, 'Height should be non-negative after UpdateSize with negative width');
  finally
    FreeAndNil(Box);
  end;
end;


{ Parent Width Calculation Tests }

procedure TTestAutoSizeBox.TestGetParentContentWidth_WithParent;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(FParent);
  Box.Parent:= FParent;
  try
    Box.BoxType:= bxUser;
    Assert.IsTrue(Box.Width > 0, 'Box width should be positive when parented');
    Assert.IsTrue(Box.Width <= FParent.Width, 'Box width should not exceed parent width');
  finally
    FreeAndNil(Box);
  end;
end;


procedure TTestAutoSizeBox.TestGetParentContentWidth_NoParent;
var
  Box: TAutosizeBoxText;
begin
  Box:= TAutosizeBoxText.Create(nil);
  try
    Box.UpdateSize;
    Assert.IsTrue(Box.Height >= 0, 'Height should be non-negative after UpdateSize without parent');
  finally
    FreeAndNil(Box);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAutoSizeBox);

end.
