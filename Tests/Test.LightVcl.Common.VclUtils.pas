unit Test.LightVcl.Common.VclUtils;

{=============================================================================================================
   Unit tests for LightVcl.Common.VclUtils.pas
   Tests VCL utility functions including control positioning, menu handling, and debugging helpers.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.Graphics,
  Vcl.Imaging.PngImage;

type
  [TestFixture]
  TTestVclUtils = class
  private
    FTestForm: TForm;
    FPanel: TPanel;
    FButton: TButton;
    FCheckBox1: TCheckBox;
    FCheckBox2: TCheckBox;
    FRadioButton: TRadioButton;
    FPageControl: TPageControl;
    FTabSheet1: TTabSheet;
    FTabSheet2: TTabSheet;
    FMainMenu: TMainMenu;
    FMenuItem: TMenuItem;
    FSubMenuItem: TMenuItem;
    FActionList: TActionList;
    FAction: TAction;
    procedure CleanupControls;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { ShowInheritanceTree Tests }
    [Test]
    procedure TestShowInheritanceTree_ReturnsHierarchy;

    [Test]
    procedure TestShowInheritanceTree_NilRaisesException;

    { ShowControlState Tests }
    [Test]
    procedure TestShowControlState_ReturnsStateString;

    [Test]
    procedure TestShowControlState_NilRaisesException;

    { ShowComponentState Tests }
    [Test]
    procedure TestShowComponentState_ReturnsStateString;

    [Test]
    procedure TestShowComponentState_NilRaisesException;

    { CanFocus Tests }
    [Test]
    procedure TestCanFocus_NilReturnsFalse;

    [Test]
    procedure TestCanFocus_VisibleEnabledReturnsTrue;

    [Test]
    procedure TestCanFocus_DisabledReturnsFalse;

    [Test]
    procedure TestCanFocus_InvisibleReturnsFalse;

    { SetFocus Tests }
    [Test]
    procedure TestSetFocus_NilNoException;

    { EnableDisable Tests }
    [Test]
    procedure TestEnableDisable_DisablesAllChildren;

    [Test]
    procedure TestEnableDisable_EnablesAllChildren;

    [Test]
    procedure TestEnableDisable_NilRaisesException;

    { ToggleCheckbox Tests }
    [Test]
    procedure TestToggleCheckbox_DisablesWhenBasedOnChecked;

    [Test]
    procedure TestToggleCheckbox_EnablesWhenBasedOnUnchecked;

    [Test]
    procedure TestToggleCheckbox_NilCheckBoxRaisesException;

    [Test]
    procedure TestToggleCheckbox_NilBasedOnRaisesException;

    [Test]
    procedure TestToggleCheckbox_WorksWithRadioButton;

    { PushControlDown Tests }
    [Test]
    procedure TestPushControlDown_PositionsCorrectly;

    [Test]
    procedure TestPushControlDown_NilBottomRaisesException;

    [Test]
    procedure TestPushControlDown_NilTopRaisesException;

    { SetActivePage Tests }
    [Test]
    procedure TestSetActivePage_FindsPageByName;

    [Test]
    procedure TestSetActivePage_ReturnsNilForNonexistent;

    [Test]
    procedure TestSetActivePage_NilPageControlRaisesException;

    [Test]
    procedure TestSetActivePage_EmptyNameRaisesException;

    { RefreshNow Tests }
    [Test]
    procedure TestRefreshNow_NilRaisesException;

    { DoubleBuffer Tests }
    [Test]
    procedure TestDoubleBuffer_EnablesDoubleBuffering;

    [Test]
    procedure TestDoubleBuffer_DisablesDoubleBuffering;

    [Test]
    procedure TestDoubleBuffer_NilRaisesException;

    { CreateControl Tests }
    [Test]
    procedure TestCreateControl_CreatesControl;

    [Test]
    procedure TestCreateControl_NilParentRaisesException;

    { HasAction Tests }
    [Test]
    procedure TestHasAction_ReturnsFalseForNil;

    [Test]
    procedure TestHasAction_ReturnsFalseWhenNoAction;

    [Test]
    procedure TestHasAction_ReturnsTrueWhenActionAssigned;

    { ActionVisibility Tests }
    [Test]
    procedure TestActionVisibility_ShowsAction;

    [Test]
    procedure TestActionVisibility_HidesAction;

    [Test]
    procedure TestActionVisibility_NilRaisesException;

    { MenuVisibility Tests }
    [Test]
    procedure TestMenuVisibility_ShowsMenuItem;

    [Test]
    procedure TestMenuVisibility_HidesMenuItem;

    [Test]
    procedure TestMenuVisibility_NilRaisesException;

    { AddSubMenu Tests }
    [Test]
    procedure TestAddSubMenu_AddsMenuItem;

    [Test]
    procedure TestAddSubMenu_NilParentRaisesException;

    { RemoveSubmenus Tests }
    [Test]
    procedure TestRemoveSubmenus_RemovesAllSubmenus;

    [Test]
    procedure TestRemoveSubmenus_NilRaisesException;

    { CopyControl2Bitmap Tests }
    [Test]
    procedure TestCopyControl2Bitmap_CreatesBitmap;

    [Test]
    procedure TestCopyControl2Bitmap_NilRaisesException;

    { CopyControl2Png Tests }
    [Test]
    procedure TestCopyControl2Png_CreatesPng;

    { ScrollAppTitle Tests }
    [Test]
    procedure TestScrollAppTitle_ScrollsLeft;

    [Test]
    procedure TestScrollAppTitle_ScrollsRight;

    [Test]
    procedure TestScrollAppTitle_ShortTitleNoChange;

    { ScrollFormCaption Tests }
    [Test]
    procedure TestScrollFormCaption_ScrollsCaption;

    [Test]
    procedure TestScrollFormCaption_NilRaisesException;

    [Test]
    procedure TestScrollFormCaption_ShortCaptionNoChange;

    { BlinkControl Tests }
    [Test]
    procedure TestBlinkControl_NilRaisesException;

    { SetChildVisibility Tests }
    [Test]
    procedure TestSetChildVisibility_SetsVisibility;

    [Test]
    procedure TestSetChildVisibility_NilRaisesException;
  end;

implementation

uses
  LightVcl.Common.VclUtils;


procedure TTestVclUtils.Setup;
begin
  FTestForm:= NIL;
  FPanel:= NIL;
  FButton:= NIL;
  FCheckBox1:= NIL;
  FCheckBox2:= NIL;
  FRadioButton:= NIL;
  FPageControl:= NIL;
  FTabSheet1:= NIL;
  FTabSheet2:= NIL;
  FMainMenu:= NIL;
  FMenuItem:= NIL;
  FSubMenuItem:= NIL;
  FActionList:= NIL;
  FAction:= NIL;
end;


procedure TTestVclUtils.TearDown;
begin
  CleanupControls;
end;


procedure TTestVclUtils.CleanupControls;
begin
  FreeAndNil(FAction);
  FreeAndNil(FActionList);
  FreeAndNil(FSubMenuItem);
  FreeAndNil(FMenuItem);
  FreeAndNil(FMainMenu);
  FreeAndNil(FTabSheet2);
  FreeAndNil(FTabSheet1);
  FreeAndNil(FPageControl);
  FreeAndNil(FRadioButton);
  FreeAndNil(FCheckBox2);
  FreeAndNil(FCheckBox1);
  FreeAndNil(FButton);
  FreeAndNil(FPanel);
  FreeAndNil(FTestForm);
end;


{ ShowInheritanceTree Tests }

procedure TTestVclUtils.TestShowInheritanceTree_ReturnsHierarchy;
var
  Result: string;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;

  Result:= ShowInheritanceTree(FButton);

  Assert.IsTrue(Pos('TButton', Result) > 0, 'Should contain TButton');
  Assert.IsTrue(Pos('TControl', Result) > 0, 'Should contain TControl');
  Assert.IsTrue(Pos('TObject', Result) > 0, 'Should contain TObject');
end;


procedure TTestVclUtils.TestShowInheritanceTree_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ShowInheritanceTree(NIL);
    end,
    Exception);
end;


{ ShowControlState Tests }

procedure TTestVclUtils.TestShowControlState_ReturnsStateString;
var
  Result: string;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;

  Result:= ShowControlState(FButton);

  Assert.IsTrue(Pos('ControlState:', Result) > 0, 'Should contain ControlState prefix');
end;


procedure TTestVclUtils.TestShowControlState_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ShowControlState(NIL);
    end,
    Exception);
end;


{ ShowComponentState Tests }

procedure TTestVclUtils.TestShowComponentState_ReturnsStateString;
var
  Result: string;
begin
  FTestForm:= TForm.CreateNew(NIL);

  Result:= ShowComponentState(FTestForm);

  Assert.IsTrue(Pos('ComponentState:', Result) > 0, 'Should contain ComponentState prefix');
end;


procedure TTestVclUtils.TestShowComponentState_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ShowComponentState(NIL);
    end,
    Exception);
end;


{ CanFocus Tests }

procedure TTestVclUtils.TestCanFocus_NilReturnsFalse;
begin
  Assert.IsFalse(LightVcl.Common.VclUtils.CanFocus(NIL), 'CanFocus(nil) should return False');
end;


procedure TTestVclUtils.TestCanFocus_VisibleEnabledReturnsTrue;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Visible:= TRUE;
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Visible:= TRUE;
  FButton.Enabled:= TRUE;

  Assert.IsTrue(LightVcl.Common.VclUtils.CanFocus(FButton), 'Visible and enabled button should be focusable');
end;


procedure TTestVclUtils.TestCanFocus_DisabledReturnsFalse;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Visible:= TRUE;
  FButton.Enabled:= FALSE;

  Assert.IsFalse(LightVcl.Common.VclUtils.CanFocus(FButton), 'Disabled button should not be focusable');
end;


procedure TTestVclUtils.TestCanFocus_InvisibleReturnsFalse;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Visible:= FALSE;
  FButton.Enabled:= TRUE;

  Assert.IsFalse(LightVcl.Common.VclUtils.CanFocus(FButton), 'Invisible button should not be focusable');
end;


{ SetFocus Tests }

procedure TTestVclUtils.TestSetFocus_NilNoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      LightVcl.Common.VclUtils.SetFocus(NIL);
    end,
    'SetFocus(nil) should not raise exception');
end;


{ EnableDisable Tests }

procedure TTestVclUtils.TestEnableDisable_DisablesAllChildren;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;

  FButton:= TButton.Create(FPanel);
  FButton.Parent:= FPanel;
  FButton.Enabled:= TRUE;

  FCheckBox1:= TCheckBox.Create(FPanel);
  FCheckBox1.Parent:= FPanel;
  FCheckBox1.Enabled:= TRUE;

  EnableDisable(FPanel, FALSE);

  Assert.IsFalse(FPanel.Enabled, 'Panel should be disabled');
  Assert.IsFalse(FButton.Enabled, 'Button should be disabled');
  Assert.IsFalse(FCheckBox1.Enabled, 'CheckBox should be disabled');
end;


procedure TTestVclUtils.TestEnableDisable_EnablesAllChildren;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Enabled:= FALSE;

  FButton:= TButton.Create(FPanel);
  FButton.Parent:= FPanel;
  FButton.Enabled:= FALSE;

  EnableDisable(FPanel, TRUE);

  Assert.IsTrue(FPanel.Enabled, 'Panel should be enabled');
  Assert.IsTrue(FButton.Enabled, 'Button should be enabled');
end;


procedure TTestVclUtils.TestEnableDisable_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      EnableDisable(NIL, TRUE);
    end,
    Exception);
end;


{ ToggleCheckbox Tests }

procedure TTestVclUtils.TestToggleCheckbox_DisablesWhenBasedOnChecked;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FCheckBox1:= TCheckBox.Create(FTestForm);
  FCheckBox1.Parent:= FTestForm;
  FCheckBox1.Checked:= TRUE;
  FCheckBox1.Enabled:= TRUE;

  FCheckBox2:= TCheckBox.Create(FTestForm);
  FCheckBox2.Parent:= FTestForm;
  FCheckBox2.Checked:= TRUE;

  ToggleCheckbox(FCheckBox1, FCheckBox2);

  Assert.IsFalse(FCheckBox1.Enabled, 'CheckBox should be disabled when BasedOn is checked');
  Assert.IsFalse(FCheckBox1.Checked, 'CheckBox should be unchecked when disabled');
end;


procedure TTestVclUtils.TestToggleCheckbox_EnablesWhenBasedOnUnchecked;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FCheckBox1:= TCheckBox.Create(FTestForm);
  FCheckBox1.Parent:= FTestForm;
  FCheckBox1.Enabled:= FALSE;

  FCheckBox2:= TCheckBox.Create(FTestForm);
  FCheckBox2.Parent:= FTestForm;
  FCheckBox2.Checked:= FALSE;

  ToggleCheckbox(FCheckBox1, FCheckBox2);

  Assert.IsTrue(FCheckBox1.Enabled, 'CheckBox should be enabled when BasedOn is unchecked');
end;


procedure TTestVclUtils.TestToggleCheckbox_NilCheckBoxRaisesException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FCheckBox2:= TCheckBox.Create(FTestForm);
  FCheckBox2.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      ToggleCheckbox(NIL, FCheckBox2);
    end,
    Exception);
end;


procedure TTestVclUtils.TestToggleCheckbox_NilBasedOnRaisesException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FCheckBox1:= TCheckBox.Create(FTestForm);
  FCheckBox1.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      ToggleCheckbox(FCheckBox1, NIL);
    end,
    Exception);
end;


procedure TTestVclUtils.TestToggleCheckbox_WorksWithRadioButton;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FCheckBox1:= TCheckBox.Create(FTestForm);
  FCheckBox1.Parent:= FTestForm;
  FCheckBox1.Enabled:= TRUE;

  FRadioButton:= TRadioButton.Create(FTestForm);
  FRadioButton.Parent:= FTestForm;
  FRadioButton.Checked:= TRUE;

  ToggleCheckbox(FCheckBox1, FRadioButton);

  Assert.IsFalse(FCheckBox1.Enabled, 'CheckBox should be disabled when RadioButton is checked');
end;


{ PushControlDown Tests }

procedure TTestVclUtils.TestPushControlDown_PositionsCorrectly;
var
  ExpectedTop: Integer;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Top:= 100;
  FPanel.Height:= 50;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Top:= 0;

  ExpectedTop:= FPanel.Top + FPanel.Height - 1;
  PushControlDown(FButton, FPanel);

  Assert.AreEqual(ExpectedTop, FButton.Top, 'Button should be positioned below panel');
end;


procedure TTestVclUtils.TestPushControlDown_NilBottomRaisesException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      PushControlDown(NIL, FPanel);
    end,
    Exception);
end;


procedure TTestVclUtils.TestPushControlDown_NilTopRaisesException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      PushControlDown(FButton, NIL);
    end,
    Exception);
end;


{ SetActivePage Tests }

procedure TTestVclUtils.TestSetActivePage_FindsPageByName;
var
  ResultTab: TTabSheet;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPageControl:= TPageControl.Create(FTestForm);
  FPageControl.Parent:= FTestForm;

  FTabSheet1:= TTabSheet.Create(FPageControl);
  FTabSheet1.PageControl:= FPageControl;
  FTabSheet1.Name:= 'TabSheet1';

  FTabSheet2:= TTabSheet.Create(FPageControl);
  FTabSheet2.PageControl:= FPageControl;
  FTabSheet2.Name:= 'TabSheet2';

  ResultTab:= SetActivePage(FPageControl, 'TabSheet2');

  Assert.IsNotNull(ResultTab, 'Should find TabSheet2');
  Assert.AreEqual('TabSheet2', ResultTab.Name, 'Should return correct tab');
  Assert.AreEqual(1, FPageControl.ActivePageIndex, 'Should set active page index');
end;


procedure TTestVclUtils.TestSetActivePage_ReturnsNilForNonexistent;
var
  ResultTab: TTabSheet;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPageControl:= TPageControl.Create(FTestForm);
  FPageControl.Parent:= FTestForm;

  FTabSheet1:= TTabSheet.Create(FPageControl);
  FTabSheet1.PageControl:= FPageControl;
  FTabSheet1.Name:= 'TabSheet1';

  ResultTab:= SetActivePage(FPageControl, 'NonExistent');

  Assert.IsNull(ResultTab, 'Should return nil for non-existent page');
end;


procedure TTestVclUtils.TestSetActivePage_NilPageControlRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetActivePage(NIL, 'SomePage');
    end,
    Exception);
end;


procedure TTestVclUtils.TestSetActivePage_EmptyNameRaisesException;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPageControl:= TPageControl.Create(FTestForm);
  FPageControl.Parent:= FTestForm;

  Assert.WillRaise(
    procedure
    begin
      SetActivePage(FPageControl, '');
    end,
    Exception);
end;


{ RefreshNow Tests }

procedure TTestVclUtils.TestRefreshNow_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      RefreshNow(NIL);
    end,
    Exception);
end;


{ DoubleBuffer Tests }

procedure TTestVclUtils.TestDoubleBuffer_EnablesDoubleBuffering;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;

  DoubleBuffer(FTestForm, TRUE);

  Assert.IsTrue(TWinControl(FPanel).DoubleBuffered, 'Panel should be double buffered');
  Assert.IsTrue(TWinControl(FButton).DoubleBuffered, 'Button should be double buffered');
end;


procedure TTestVclUtils.TestDoubleBuffer_DisablesDoubleBuffering;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  TWinControl(FPanel).DoubleBuffered:= TRUE;

  DoubleBuffer(FTestForm, FALSE);

  Assert.IsFalse(TWinControl(FPanel).DoubleBuffered, 'Panel should not be double buffered');
end;


procedure TTestVclUtils.TestDoubleBuffer_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      DoubleBuffer(NIL, TRUE);
    end,
    Exception);
end;


{ CreateControl Tests }

procedure TTestVclUtils.TestCreateControl_CreatesControl;
var
  NewControl: TControl;
begin
  FTestForm:= TForm.CreateNew(NIL);

  NewControl:= CreateControl(TButton, 'TestButton', FTestForm, 10, 20, 100, 30);
  TRY
    Assert.IsNotNull(NewControl, 'Control should be created');
    Assert.AreEqual('TestButton', NewControl.Name, 'Name should match');
    Assert.AreEqual(10, NewControl.Left, 'Left should be 10');
    Assert.AreEqual(20, NewControl.Top, 'Top should be 20');
    Assert.AreEqual(100, NewControl.Width, 'Width should be 100');
    Assert.AreEqual(30, NewControl.Height, 'Height should be 30');
    Assert.IsTrue(NewControl.Visible, 'Control should be visible');
  FINALLY
    { Control is owned by FTestForm, no need to free separately }
  END;
end;


procedure TTestVclUtils.TestCreateControl_NilParentRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      CreateControl(TButton, 'TestButton', NIL, 0, 0, 100, 30);
    end,
    Exception);
end;


{ HasAction Tests }

procedure TTestVclUtils.TestHasAction_ReturnsFalseForNil;
begin
  Assert.IsFalse(HasAction(NIL), 'HasAction(nil) should return False');
end;


procedure TTestVclUtils.TestHasAction_ReturnsFalseWhenNoAction;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Action:= NIL;

  Assert.IsFalse(HasAction(FButton), 'Button without action should return False');
end;


procedure TTestVclUtils.TestHasAction_ReturnsTrueWhenActionAssigned;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FActionList:= TActionList.Create(FTestForm);
  FAction:= TAction.Create(FActionList);

  FButton:= TButton.Create(FTestForm);
  FButton.Parent:= FTestForm;
  FButton.Action:= FAction;

  Assert.IsTrue(HasAction(FButton), 'Button with action should return True');
end;


{ ActionVisibility Tests }

procedure TTestVclUtils.TestActionVisibility_ShowsAction;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FActionList:= TActionList.Create(FTestForm);
  FAction:= TAction.Create(FActionList);
  FAction.Enabled:= FALSE;
  FAction.Visible:= FALSE;

  ActionVisibility(FAction, TRUE);

  Assert.IsTrue(FAction.Enabled, 'Action should be enabled');
  Assert.IsTrue(FAction.Visible, 'Action should be visible');
end;


procedure TTestVclUtils.TestActionVisibility_HidesAction;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FActionList:= TActionList.Create(FTestForm);
  FAction:= TAction.Create(FActionList);
  FAction.Enabled:= TRUE;
  FAction.Visible:= TRUE;

  ActionVisibility(FAction, FALSE);

  Assert.IsFalse(FAction.Enabled, 'Action should be disabled');
  Assert.IsFalse(FAction.Visible, 'Action should be invisible');
end;


procedure TTestVclUtils.TestActionVisibility_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ActionVisibility(NIL, TRUE);
    end,
    Exception);
end;


{ MenuVisibility Tests }

procedure TTestVclUtils.TestMenuVisibility_ShowsMenuItem;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FMainMenu:= TMainMenu.Create(FTestForm);
  FMenuItem:= TMenuItem.Create(FMainMenu);
  FMainMenu.Items.Add(FMenuItem);
  FMenuItem.Enabled:= FALSE;
  FMenuItem.Visible:= FALSE;

  MenuVisibility(FMenuItem, TRUE, TRUE);

  Assert.IsTrue(FMenuItem.Enabled, 'MenuItem should be enabled');
  Assert.IsTrue(FMenuItem.Visible, 'MenuItem should be visible');
end;


procedure TTestVclUtils.TestMenuVisibility_HidesMenuItem;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FMainMenu:= TMainMenu.Create(FTestForm);
  FMenuItem:= TMenuItem.Create(FMainMenu);
  FMainMenu.Items.Add(FMenuItem);
  FMenuItem.Enabled:= TRUE;
  FMenuItem.Visible:= TRUE;

  MenuVisibility(FMenuItem, FALSE, FALSE);

  Assert.IsFalse(FMenuItem.Enabled, 'MenuItem should be disabled');
  Assert.IsFalse(FMenuItem.Visible, 'MenuItem should be invisible');
end;


procedure TTestVclUtils.TestMenuVisibility_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      MenuVisibility(NIL, TRUE, TRUE);
    end,
    Exception);
end;


{ AddSubMenu Tests }

procedure TTestVclUtils.TestAddSubMenu_AddsMenuItem;
var
  NewItem: TMenuItem;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FMainMenu:= TMainMenu.Create(FTestForm);
  FMenuItem:= TMenuItem.Create(FMainMenu);
  FMainMenu.Items.Add(FMenuItem);

  NewItem:= AddSubMenu(FMenuItem, 'SubItem', NIL);
  TRY
    Assert.IsNotNull(NewItem, 'SubMenu should be created');
    Assert.AreEqual('SubItem', NewItem.Caption, 'Caption should match');
    Assert.AreEqual(1, FMenuItem.Count, 'Parent should have 1 child');
  FINALLY
    { Item is owned by FMenuItem }
  END;
end;


procedure TTestVclUtils.TestAddSubMenu_NilParentRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      AddSubMenu(NIL, 'Test', NIL);
    end,
    Exception);
end;


{ RemoveSubmenus Tests }

procedure TTestVclUtils.TestRemoveSubmenus_RemovesAllSubmenus;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FMainMenu:= TMainMenu.Create(FTestForm);
  FMenuItem:= TMenuItem.Create(FMainMenu);
  FMainMenu.Items.Add(FMenuItem);

  FSubMenuItem:= TMenuItem.Create(FMenuItem);
  FSubMenuItem.Caption:= 'Sub1';
  FMenuItem.Add(FSubMenuItem);
  FSubMenuItem:= NIL; { Will be freed by RemoveSubmenus }

  var Sub2:= TMenuItem.Create(FMenuItem);
  Sub2.Caption:= 'Sub2';
  FMenuItem.Add(Sub2);

  RemoveSubmenus(FMenuItem);

  Assert.AreEqual(0, FMenuItem.Count, 'Parent should have no children after RemoveSubmenus');
end;


procedure TTestVclUtils.TestRemoveSubmenus_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      RemoveSubmenus(NIL);
    end,
    Exception);
end;


{ CopyControl2Bitmap Tests }

procedure TTestVclUtils.TestCopyControl2Bitmap_CreatesBitmap;
var
  Bmp: TBitmap;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Width:= 100;
  FPanel.Height:= 50;

  Bmp:= CopyControl2Bitmap(FPanel);
  TRY
    Assert.IsNotNull(Bmp, 'Bitmap should be created');
    Assert.AreEqual(100, Bmp.Width, 'Bitmap width should match control');
    Assert.AreEqual(50, Bmp.Height, 'Bitmap height should match control');
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


procedure TTestVclUtils.TestCopyControl2Bitmap_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      CopyControl2Bitmap(NIL);
    end,
    Exception);
end;


{ CopyControl2Png Tests }

procedure TTestVclUtils.TestCopyControl2Png_CreatesPng;
var
  Png: TPngImage;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FPanel:= TPanel.Create(FTestForm);
  FPanel.Parent:= FTestForm;
  FPanel.Width:= 100;
  FPanel.Height:= 50;

  Png:= CopyControl2Png(FPanel);
  TRY
    Assert.IsNotNull(Png, 'PNG should be created');
    Assert.AreEqual(100, Integer(Png.Width), 'PNG width should match control');
    Assert.AreEqual(50, Integer(Png.Height), 'PNG height should match control');
  FINALLY
    FreeAndNil(Png);
  END;
end;


{ ScrollAppTitle Tests }

procedure TTestVclUtils.TestScrollAppTitle_ScrollsLeft;
var
  OriginalTitle: string;
begin
  OriginalTitle:= Application.Title;
  TRY
    Application.Title:= 'ABCD';
    ScrollAppTitle(TRUE);
    Assert.AreEqual('BCDA', Application.Title, 'Title should scroll left');
  FINALLY
    Application.Title:= OriginalTitle;
  END;
end;


procedure TTestVclUtils.TestScrollAppTitle_ScrollsRight;
var
  OriginalTitle: string;
begin
  OriginalTitle:= Application.Title;
  TRY
    Application.Title:= 'ABCD';
    ScrollAppTitle(FALSE);
    Assert.AreEqual('DABC', Application.Title, 'Title should scroll right');
  FINALLY
    Application.Title:= OriginalTitle;
  END;
end;


procedure TTestVclUtils.TestScrollAppTitle_ShortTitleNoChange;
var
  OriginalTitle: string;
begin
  OriginalTitle:= Application.Title;
  TRY
    Application.Title:= 'A';
    ScrollAppTitle(TRUE);
    Assert.AreEqual('A', Application.Title, 'Single char title should not change');
  FINALLY
    Application.Title:= OriginalTitle;
  END;
end;


{ ScrollFormCaption Tests }

procedure TTestVclUtils.TestScrollFormCaption_ScrollsCaption;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Caption:= 'ABCD';

  ScrollFormCaption(FTestForm);

  Assert.AreEqual('BCDA', FTestForm.Caption, 'Caption should scroll left');
end;


procedure TTestVclUtils.TestScrollFormCaption_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      ScrollFormCaption(NIL);
    end,
    Exception);
end;


procedure TTestVclUtils.TestScrollFormCaption_ShortCaptionNoChange;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FTestForm.Caption:= 'A';

  ScrollFormCaption(FTestForm);

  Assert.AreEqual('A', FTestForm.Caption, 'Single char caption should not change');
end;


{ BlinkControl Tests }

procedure TTestVclUtils.TestBlinkControl_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      BlinkControl(NIL);
    end,
    Exception);
end;


{ SetChildVisibility Tests }

procedure TTestVclUtils.TestSetChildVisibility_SetsVisibility;
begin
  FTestForm:= TForm.CreateNew(NIL);
  FMainMenu:= TMainMenu.Create(FTestForm);
  FMenuItem:= TMenuItem.Create(FMainMenu);
  FMainMenu.Items.Add(FMenuItem);
  FMenuItem.Visible:= TRUE;

  FSubMenuItem:= TMenuItem.Create(FMenuItem);
  FSubMenuItem.Caption:= 'Sub';
  FMenuItem.Add(FSubMenuItem);
  FSubMenuItem.Visible:= TRUE;

  SetChildVisibility(FMenuItem, FALSE);

  Assert.IsFalse(FMenuItem.Visible, 'Parent should be invisible');
  Assert.IsFalse(FSubMenuItem.Visible, 'SubMenuItem should be invisible');
end;


procedure TTestVclUtils.TestSetChildVisibility_NilRaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetChildVisibility(NIL, TRUE, TRUE);
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestVclUtils);

end.
