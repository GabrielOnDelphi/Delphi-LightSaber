UNIT Test.LightFmx.Visual.ComboBox;

{=============================================================================================================
   Unit tests for LightFmx.Visual.ComboBox.pas
   Tests TLightComboBox: selection, dual items, and object association.

   Note: FMX components require an FMX application context.
   These tests should be run in an FMX test application.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.ListBox,
  LightFmx.Visual.ComboBox;

TYPE
  [TestFixture]
  TTestLightComboBox = class
  private
    FComboBox: TLightComboBox;
    FDummyObj1: TObject;
    FDummyObj2: TObject;
    FDummyObj3: TObject;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor tests }
    [Test]
    procedure TestCreate_DefaultValues;

    [Test]
    procedure TestCreate_NameValueSeparator;

    { SelectedItem tests - Single mode }
    [Test]
    procedure TestSelectedItem_SingleMode;

    [Test]
    procedure TestSelectedItem_NoSelection_Assertion;

    { SelectedItemSafe tests }
    [Test]
    procedure TestSelectedItemSafe_NoSelection;

    [Test]
    procedure TestSelectedItemSafe_EmptyList;

    [Test]
    procedure TestSelectedItemSafe_SingleMode;

    [Test]
    procedure TestSelectedItemSafe_DualMode;

    { SelectedItemForce tests }
    [Test]
    procedure TestSelectedItemForce_EmptyList;

    [Test]
    procedure TestSelectedItemForce_NoSelection;

    [Test]
    procedure TestSelectedItemForce_ExistingSelection;

    { SelectItem tests - Single mode }
    [Test]
    procedure TestSelectItem_SingleMode_Found;

    [Test]
    procedure TestSelectItem_SingleMode_NotFound;

    [Test]
    procedure TestSelectItem_SingleMode_CaseInsensitive;

    [Test]
    procedure TestSelectItem_EmptyList;

    { SelectItem tests - Dual mode }
    [Test]
    procedure TestSelectItem_DualMode_ByName;

    [Test]
    procedure TestSelectItem_DualMode_ByName_CaseInsensitive;

    [Test]
    procedure TestSelectItem_DualMode_NotFound;

    { SelectFirstItem tests }
    [Test]
    procedure TestSelectFirstItem_WithItems;

    [Test]
    procedure TestSelectFirstItem_EmptyList;

    { SelectObject tests }
    [Test]
    procedure TestSelectObject_Found;

    [Test]
    procedure TestSelectObject_NotFound;

    [Test]
    procedure TestSelectObject_EmptyList;

    { SelectedObject tests }
    [Test]
    procedure TestSelectedObject_WithSelection;

    [Test]
    procedure TestSelectedObject_NoSelection;

    { Dual item tests }
    [Test]
    procedure TestDualItem_SelectDualItem_Found;

    [Test]
    procedure TestDualItem_SelectDualItem_NotFound;

    [Test]
    procedure TestDualItem_SelectDualItem_CaseInsensitive;

    [Test]
    procedure TestDualItem_SelectedDualItem_WithSelection;

    [Test]
    procedure TestDualItem_SelectedDualItem_NoSelection;

    [Test]
    procedure TestDualItem_SelectedItem_ReturnsName;

    [Test]
    procedure TestDualItem_Assertion_WhenNotDualMode;

    { Edge cases }
    [Test]
    procedure TestSelectItem_DeselectsOnNotFound;

    [Test]
    procedure TestMultipleSelections;
  end;


IMPLEMENTATION


procedure TTestLightComboBox.Setup;
begin
  // FMX requires an application context
  if Application = nil then
    Application := TApplication.Create(nil);

  FComboBox:= TLightComboBox.Create(nil);

  // Create dummy objects for object association tests
  FDummyObj1:= TObject.Create;
  FDummyObj2:= TObject.Create;
  FDummyObj3:= TObject.Create;
end;


procedure TTestLightComboBox.TearDown;
begin
  FreeAndNil(FComboBox);
  FreeAndNil(FDummyObj1);
  FreeAndNil(FDummyObj2);
  FreeAndNil(FDummyObj3);
end;


{ Constructor tests }

procedure TTestLightComboBox.TestCreate_DefaultValues;
begin
  Assert.IsFalse(FComboBox.IsDualItem, 'Default IsDualItem should be False');
  Assert.AreEqual(-1, FComboBox.ItemIndex, 'Default ItemIndex should be -1');
end;


procedure TTestLightComboBox.TestCreate_NameValueSeparator;
begin
  Assert.AreEqual('|', FComboBox.Items.NameValueSeparator, 'NameValueSeparator should be |');
end;


{ SelectedItem tests - Single mode }

procedure TTestLightComboBox.TestSelectedItem_SingleMode;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.Items.Add('Cherry');
  FComboBox.ItemIndex:= 1;

  Assert.AreEqual('Banana', FComboBox.SelectedItem);
end;


procedure TTestLightComboBox.TestSelectedItem_NoSelection_Assertion;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.ItemIndex:= -1;

  Assert.WillRaise(
    procedure
    begin
      FComboBox.SelectedItem;
    end,
    EAssertionFailed);
end;


{ SelectedItemSafe tests }

procedure TTestLightComboBox.TestSelectedItemSafe_NoSelection;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.ItemIndex:= -1;

  Assert.AreEqual('', FComboBox.SelectedItemSafe);
end;


procedure TTestLightComboBox.TestSelectedItemSafe_EmptyList;
begin
  Assert.AreEqual('', FComboBox.SelectedItemSafe);
end;


procedure TTestLightComboBox.TestSelectedItemSafe_SingleMode;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.ItemIndex:= 0;

  Assert.AreEqual('Apple', FComboBox.SelectedItemSafe);
end;


procedure TTestLightComboBox.TestSelectedItemSafe_DualMode;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');
  FComboBox.ItemIndex:= 0;

  // In dual mode, SelectedItemSafe returns the Name (internal command)
  Assert.AreEqual('cmd_save', FComboBox.SelectedItemSafe);
end;


{ SelectedItemForce tests }

procedure TTestLightComboBox.TestSelectedItemForce_EmptyList;
begin
  Assert.AreEqual('', FComboBox.SelectedItemForce, 'Empty list should return empty string');
end;


procedure TTestLightComboBox.TestSelectedItemForce_NoSelection;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.ItemIndex:= -1;

  Assert.AreEqual('Apple', FComboBox.SelectedItemForce, 'Should select and return first item');
  Assert.AreEqual(0, FComboBox.ItemIndex, 'ItemIndex should be 0 after force select');
end;


procedure TTestLightComboBox.TestSelectedItemForce_ExistingSelection;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.ItemIndex:= 1;

  Assert.AreEqual('Banana', FComboBox.SelectedItemForce, 'Should return existing selection');
  Assert.AreEqual(1, FComboBox.ItemIndex, 'ItemIndex should remain unchanged');
end;


{ SelectItem tests - Single mode }

procedure TTestLightComboBox.TestSelectItem_SingleMode_Found;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.Items.Add('Cherry');

  Assert.AreEqual(1, FComboBox.SelectItem('Banana'));
  Assert.AreEqual(1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestSelectItem_SingleMode_NotFound;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');

  Assert.AreEqual(-1, FComboBox.SelectItem('Orange'));
  Assert.AreEqual(-1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestSelectItem_SingleMode_CaseInsensitive;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');

  Assert.AreEqual(1, FComboBox.SelectItem('BANANA'), 'Should find case-insensitively');
  Assert.AreEqual(1, FComboBox.SelectItem('banana'), 'Should find case-insensitively');
end;


procedure TTestLightComboBox.TestSelectItem_EmptyList;
begin
  Assert.AreEqual(-1, FComboBox.SelectItem('Apple'));
end;


{ SelectItem tests - Dual mode }

procedure TTestLightComboBox.TestSelectItem_DualMode_ByName;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');
  FComboBox.Items.Add('cmd_close|Close File');

  // SelectItem in dual mode should search by Name (internal command)
  Assert.AreEqual(1, FComboBox.SelectItem('cmd_open'));
  Assert.AreEqual(1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestSelectItem_DualMode_ByName_CaseInsensitive;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');

  Assert.AreEqual(0, FComboBox.SelectItem('CMD_SAVE'), 'Should find case-insensitively');
end;


procedure TTestLightComboBox.TestSelectItem_DualMode_NotFound;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');

  // Searching by screen name should not find anything
  Assert.AreEqual(-1, FComboBox.SelectItem('Save File'));
end;


{ SelectFirstItem tests }

procedure TTestLightComboBox.TestSelectFirstItem_WithItems;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.ItemIndex:= -1;

  Assert.IsTrue(FComboBox.SelectFirstItem, 'Should return True when list has items');
  Assert.AreEqual(0, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestSelectFirstItem_EmptyList;
begin
  Assert.IsFalse(FComboBox.SelectFirstItem, 'Should return False when list is empty');
end;


{ SelectObject tests }

procedure TTestLightComboBox.TestSelectObject_Found;
begin
  FComboBox.Items.AddObject('Item1', FDummyObj1);
  FComboBox.Items.AddObject('Item2', FDummyObj2);
  FComboBox.Items.AddObject('Item3', FDummyObj3);

  Assert.IsTrue(FComboBox.SelectObject(FDummyObj2), 'Should return True when found');
  Assert.AreEqual(1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestSelectObject_NotFound;
var
  UnknownObj: TObject;
begin
  UnknownObj:= TObject.Create;
  try
    FComboBox.Items.AddObject('Item1', FDummyObj1);
    FComboBox.Items.AddObject('Item2', FDummyObj2);

    Assert.IsFalse(FComboBox.SelectObject(UnknownObj), 'Should return False when not found');
  finally
    FreeAndNil(UnknownObj);
  end;
end;


procedure TTestLightComboBox.TestSelectObject_EmptyList;
begin
  Assert.IsFalse(FComboBox.SelectObject(FDummyObj1));
end;


{ SelectedObject tests }

procedure TTestLightComboBox.TestSelectedObject_WithSelection;
begin
  FComboBox.Items.AddObject('Item1', FDummyObj1);
  FComboBox.Items.AddObject('Item2', FDummyObj2);
  FComboBox.ItemIndex:= 1;

  Assert.AreSame(FDummyObj2, FComboBox.SelectedObject);
end;


procedure TTestLightComboBox.TestSelectedObject_NoSelection;
begin
  FComboBox.Items.AddObject('Item1', FDummyObj1);
  FComboBox.ItemIndex:= -1;

  Assert.IsNull(FComboBox.SelectedObject);
end;


{ Dual item tests }

procedure TTestLightComboBox.TestDualItem_SelectDualItem_Found;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');
  FComboBox.Items.Add('cmd_close|Close File');

  // SelectDualItem searches by screen name (Value)
  Assert.AreEqual(1, FComboBox.SelectDualItem('Open File'));
  Assert.AreEqual(1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestDualItem_SelectDualItem_NotFound;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');

  Assert.AreEqual(-1, FComboBox.SelectDualItem('Delete File'));
  Assert.AreEqual(-1, FComboBox.ItemIndex);
end;


procedure TTestLightComboBox.TestDualItem_SelectDualItem_CaseInsensitive;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');

  Assert.AreEqual(0, FComboBox.SelectDualItem('SAVE FILE'), 'Should find case-insensitively');
  Assert.AreEqual(0, FComboBox.SelectDualItem('save file'), 'Should find case-insensitively');
end;


procedure TTestLightComboBox.TestDualItem_SelectedDualItem_WithSelection;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');
  FComboBox.ItemIndex:= 1;

  // SelectedDualItem returns screen name (Value)
  Assert.AreEqual('Open File', FComboBox.SelectedDualItem);
end;


procedure TTestLightComboBox.TestDualItem_SelectedDualItem_NoSelection;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.ItemIndex:= -1;

  Assert.AreEqual('', FComboBox.SelectedDualItem);
end;


procedure TTestLightComboBox.TestDualItem_SelectedItem_ReturnsName;
begin
  FComboBox.IsDualItem:= True;
  FComboBox.Items.Add('cmd_save|Save File');
  FComboBox.Items.Add('cmd_open|Open File');
  FComboBox.ItemIndex:= 0;

  // SelectedItem returns Name (internal command) in dual mode
  Assert.AreEqual('cmd_save', FComboBox.SelectedItem);
end;


procedure TTestLightComboBox.TestDualItem_Assertion_WhenNotDualMode;
begin
  FComboBox.IsDualItem:= False;
  FComboBox.Items.Add('Item1');

  Assert.WillRaise(
    procedure
    begin
      FComboBox.SelectDualItem('Item1');
    end,
    EAssertionFailed,
    'SelectDualItem should assert when IsDualItem is False');

  Assert.WillRaise(
    procedure
    begin
      FComboBox.SelectedDualItem;
    end,
    EAssertionFailed,
    'SelectedDualItem should assert when IsDualItem is False');
end;


{ Edge cases }

procedure TTestLightComboBox.TestSelectItem_DeselectsOnNotFound;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.ItemIndex:= 1;

  // When item not found, should deselect (set ItemIndex to -1)
  FComboBox.SelectItem('Orange');

  Assert.AreEqual(-1, FComboBox.ItemIndex, 'Should deselect when item not found');
end;


procedure TTestLightComboBox.TestMultipleSelections;
begin
  FComboBox.Items.Add('Apple');
  FComboBox.Items.Add('Banana');
  FComboBox.Items.Add('Cherry');

  FComboBox.SelectItem('Apple');
  Assert.AreEqual(0, FComboBox.ItemIndex);
  Assert.AreEqual('Apple', FComboBox.SelectedItem);

  FComboBox.SelectItem('Cherry');
  Assert.AreEqual(2, FComboBox.ItemIndex);
  Assert.AreEqual('Cherry', FComboBox.SelectedItem);

  FComboBox.SelectItem('Banana');
  Assert.AreEqual(1, FComboBox.ItemIndex);
  Assert.AreEqual('Banana', FComboBox.SelectedItem);
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestLightComboBox);

end.
