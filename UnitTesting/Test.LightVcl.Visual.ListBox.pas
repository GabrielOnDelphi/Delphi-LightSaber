unit Test.LightVcl.Visual.ListBox;

{=============================================================================================================
   Unit tests for LightVcl.Visual.ListBox.pas
   Tests TCubicListBox enhanced listbox functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  LightVcl.Visual.ListBox;

type
  { Simple test object for testing object associations }
  TTestData = class
  public
    Value: Integer;
    constructor Create(AValue: Integer);
  end;

  [TestFixture]
  TTestCubicListBox = class
  private
    FListBox: TCubicListBox;
    FForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { DeleteFirst Tests }
    [Test]
    procedure TestDeleteFirst_DeletesCorrectItems;

    [Test]
    procedure TestDeleteFirst_WithZeroCount;

    [Test]
    procedure TestDeleteFirst_MoreThanAvailable;

    [Test]
    procedure TestDeleteFirst_EmptyList;

    { SwapItems Tests }
    [Test]
    procedure TestSwapItems_SwapsStrings;

    [Test]
    procedure TestSwapItems_SwapsObjects;

    { Selection Tests }
    [Test]
    procedure TestSelectedItem_ReturnsCorrectItem;

    [Test]
    procedure TestSelectedItem_EmptyList;

    [Test]
    procedure TestSelectedItemI_ReturnsIndex;

    [Test]
    procedure TestSelectedItemI_NoSelection;

    [Test]
    procedure TestSelectedItemForce_ForcesSelection;

    [Test]
    procedure TestSelectedItems_MultipleSelection;

    [Test]
    procedure TestSelCountEx_CountsSelected;

    [Test]
    procedure TestSelCountEx_NoSelection;

    { Select methods }
    [Test]
    procedure TestSelectItemSafe_ValidIndex;

    [Test]
    procedure TestSelectItemSafe_NegativeIndex;

    [Test]
    procedure TestSelectItemSafe_OutOfBoundsIndex;

    [Test]
    procedure TestSelectItemSafe_EmptyList;

    [Test]
    procedure TestSelectItem_ByObject;

    [Test]
    procedure TestSelectFirstItem;

    [Test]
    procedure TestSelectLastItem;

    [Test]
    procedure TestSelectNext;

    [Test]
    procedure TestDeselectAll;

    { MoveUp/MoveDown Tests }
    [Test]
    procedure TestMoveUp_MovesItem;

    [Test]
    procedure TestMoveUp_AtTop;

    [Test]
    procedure TestMoveDown_MovesItem;

    [Test]
    procedure TestMoveDown_AtBottom;

    [Test]
    procedure TestMoveUp_PreservesObjects;

    { Remove Tests }
    [Test]
    procedure TestRemoveDuplicates_RemovesDupes;

    [Test]
    procedure TestRemoveDuplicates_CaseInsensitive;

    [Test]
    procedure TestRemoveDuplicates_NoDupes;

    [Test]
    procedure TestRemoveEmptyLines_RemovesEmpty;

    [Test]
    procedure TestRemoveEmptyLines_NoEmpty;

    [Test]
    procedure TestRemoveLongLines_RemovesLong;

    [Test]
    procedure TestTrim_TrimsWhitespace;

    { FindItem Tests }
    [Test]
    procedure TestFindItem_Found;

    [Test]
    procedure TestFindItem_NotFound;

    [Test]
    procedure TestFindItem_CaseInsensitive;

    { MoveItemsTo Tests }
    [Test]
    procedure TestMoveItemsTo_MovesItems;

    [Test]
    procedure TestMoveItemsTo_PreservesObjects;

    [Test]
    procedure TestMoveItemsTo_ZeroCount;

    [Test]
    procedure TestMoveItemsTo_MoreThanAvailable;

    { VisibleItems Tests }
    [Test]
    procedure TestVisibleItems_ZeroItemHeight;

    { DeleteSelected Tests }
    [Test]
    procedure TestDeleteSelected_DeletesSelected;

    [Test]
    procedure TestDeleteSelected_FreesObjects;

    [Test]
    procedure TestDeleteSelected_MultipleSelected;

    { I/O Tests - these require file system }
    [Test]
    procedure TestLoadFromFile_NonExistent;

    { AddLimit Tests }
    [Test]
    procedure TestAddLimit_LimitsItems;
  end;

implementation

uses
  System.IOUtils;


{ TTestData }

constructor TTestData.Create(AValue: Integer);
begin
  inherited Create;
  Value:= AValue;
end;


{ TTestCubicListBox }

procedure TTestCubicListBox.Setup;
begin
  FForm:= TForm.Create(nil);
  FForm.Width:= 400;
  FForm.Height:= 400;

  FListBox:= TCubicListBox.Create(FForm);
  FListBox.Parent:= FForm;
  FListBox.Left:= 10;
  FListBox.Top:= 10;
  FListBox.Width:= 200;
  FListBox.Height:= 200;
  FListBox.ItemHeight:= 20;
end;


procedure TTestCubicListBox.TearDown;
begin
  FreeAndNil(FListBox);
  FreeAndNil(FForm);
end;


{ DeleteFirst Tests }

procedure TTestCubicListBox.TestDeleteFirst_DeletesCorrectItems;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');
  FListBox.Items.Add('Fourth');

  FListBox.DeleteFirst(2);

  Assert.AreEqual(2, FListBox.Items.Count);
  Assert.AreEqual('Third', FListBox.Items[0]);
  Assert.AreEqual('Fourth', FListBox.Items[1]);
end;


procedure TTestCubicListBox.TestDeleteFirst_WithZeroCount;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.DeleteFirst(0);

  Assert.AreEqual(2, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestDeleteFirst_MoreThanAvailable;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.DeleteFirst(10);

  Assert.AreEqual(0, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestDeleteFirst_EmptyList;
begin
  FListBox.DeleteFirst(5);
  Assert.AreEqual(0, FListBox.Items.Count);
end;


{ SwapItems Tests }

procedure TTestCubicListBox.TestSwapItems_SwapsStrings;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.SwapItems(0, 2);

  Assert.AreEqual('Third', FListBox.Items[0]);
  Assert.AreEqual('Second', FListBox.Items[1]);
  Assert.AreEqual('First', FListBox.Items[2]);
end;


procedure TTestCubicListBox.TestSwapItems_SwapsObjects;
var
  Obj1, Obj2: TTestData;
begin
  Obj1:= TTestData.Create(100);
  Obj2:= TTestData.Create(200);

  FListBox.Items.AddObject('First', Obj1);
  FListBox.Items.AddObject('Second', Obj2);

  FListBox.SwapItems(0, 1);

  Assert.AreEqual('Second', FListBox.Items[0]);
  Assert.AreEqual('First', FListBox.Items[1]);
  Assert.AreEqual(200, TTestData(FListBox.Items.Objects[0]).Value);
  Assert.AreEqual(100, TTestData(FListBox.Items.Objects[1]).Value);

  Obj1.Free;
  Obj2.Free;
end;


{ Selection Tests }

procedure TTestCubicListBox.TestSelectedItem_ReturnsCorrectItem;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.ItemIndex:= 1;
  FListBox.Selected[1]:= TRUE;

  Assert.AreEqual('Second', FListBox.SelectedItem);
end;


procedure TTestCubicListBox.TestSelectedItem_EmptyList;
begin
  Assert.AreEqual('', FListBox.SelectedItem);
end;


procedure TTestCubicListBox.TestSelectedItemI_ReturnsIndex;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.Selected[2]:= TRUE;

  Assert.AreEqual(2, FListBox.SelectedItemI);
end;


procedure TTestCubicListBox.TestSelectedItemI_NoSelection;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  Assert.AreEqual(-1, FListBox.SelectedItemI);
end;


procedure TTestCubicListBox.TestSelectedItemForce_ForcesSelection;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  // No selection initially
  Assert.AreEqual('First', FListBox.SelectedItemForce);
  Assert.IsTrue(FListBox.Selected[0]);
end;


procedure TTestCubicListBox.TestSelectedItems_MultipleSelection;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.Selected[0]:= TRUE;
  FListBox.Selected[2]:= TRUE;

  var Result:= FListBox.SelectedItems;
  Assert.IsTrue(Pos('First', Result) > 0);
  Assert.IsTrue(Pos('Third', Result) > 0);
  Assert.IsFalse(Pos('Second', Result) > 0);
end;


procedure TTestCubicListBox.TestSelCountEx_CountsSelected;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.Selected[0]:= TRUE;
  FListBox.Selected[2]:= TRUE;

  Assert.AreEqual(2, FListBox.SelCountEx);
end;


procedure TTestCubicListBox.TestSelCountEx_NoSelection;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  Assert.AreEqual(0, FListBox.SelCountEx);
end;


{ Select methods }

procedure TTestCubicListBox.TestSelectItemSafe_ValidIndex;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.SelectItemSafe(1);

  Assert.AreEqual(1, FListBox.ItemIndex);
  Assert.IsTrue(FListBox.Selected[1]);
end;


procedure TTestCubicListBox.TestSelectItemSafe_NegativeIndex;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.SelectItemSafe(-5);

  Assert.AreEqual(0, FListBox.ItemIndex);
end;


procedure TTestCubicListBox.TestSelectItemSafe_OutOfBoundsIndex;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.SelectItemSafe(100);

  Assert.AreEqual(1, FListBox.ItemIndex);  // Should select last item
end;


procedure TTestCubicListBox.TestSelectItemSafe_EmptyList;
begin
  // Should not crash
  FListBox.SelectItemSafe(5);
  Assert.AreEqual(-1, FListBox.ItemIndex);
end;


procedure TTestCubicListBox.TestSelectItem_ByObject;
var
  Obj: TTestData;
begin
  Obj:= TTestData.Create(42);

  FListBox.Items.Add('First');
  FListBox.Items.AddObject('Second', Obj);
  FListBox.Items.Add('Third');

  FListBox.SelectItem(Obj);

  Assert.AreEqual(1, FListBox.ItemIndex);
  Assert.IsTrue(FListBox.Selected[1]);

  Obj.Free;
end;


procedure TTestCubicListBox.TestSelectFirstItem;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.SelectFirstItem;

  Assert.AreEqual(0, FListBox.ItemIndex);
  Assert.IsTrue(FListBox.Selected[0]);
end;


procedure TTestCubicListBox.TestSelectLastItem;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.SelectLastItem;

  Assert.AreEqual(2, FListBox.ItemIndex);
  Assert.IsTrue(FListBox.Selected[2]);
end;


procedure TTestCubicListBox.TestSelectNext;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.ItemIndex:= 0;
  FListBox.SelectNext;

  Assert.AreEqual(1, FListBox.ItemIndex);
  Assert.IsTrue(FListBox.Selected[1]);
end;


procedure TTestCubicListBox.TestDeselectAll;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.Selected[0]:= TRUE;
  FListBox.Selected[1]:= TRUE;
  FListBox.Selected[2]:= TRUE;

  FListBox.DeselectAll;

  Assert.IsFalse(FListBox.Selected[0]);
  Assert.IsFalse(FListBox.Selected[1]);
  Assert.IsFalse(FListBox.Selected[2]);
end;


{ MoveUp/MoveDown Tests }

procedure TTestCubicListBox.TestMoveUp_MovesItem;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.ItemIndex:= 2;
  FListBox.MoveUp;

  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Third', FListBox.Items[1]);
  Assert.AreEqual('Second', FListBox.Items[2]);
  Assert.AreEqual(1, FListBox.ItemIndex);
end;


procedure TTestCubicListBox.TestMoveUp_AtTop;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.ItemIndex:= 0;
  FListBox.MoveUp;  // Should do nothing

  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Second', FListBox.Items[1]);
end;


procedure TTestCubicListBox.TestMoveDown_MovesItem;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.ItemIndex:= 0;
  FListBox.MoveDown;

  Assert.AreEqual('Second', FListBox.Items[0]);
  Assert.AreEqual('First', FListBox.Items[1]);
  Assert.AreEqual('Third', FListBox.Items[2]);
  Assert.AreEqual(1, FListBox.ItemIndex);
end;


procedure TTestCubicListBox.TestMoveDown_AtBottom;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  FListBox.ItemIndex:= 1;
  FListBox.MoveDown;  // Should do nothing

  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Second', FListBox.Items[1]);
end;


procedure TTestCubicListBox.TestMoveUp_PreservesObjects;
var
  Obj1, Obj2: TTestData;
begin
  Obj1:= TTestData.Create(100);
  Obj2:= TTestData.Create(200);

  FListBox.Items.AddObject('First', Obj1);
  FListBox.Items.AddObject('Second', Obj2);

  FListBox.ItemIndex:= 1;
  FListBox.MoveUp;

  Assert.AreEqual('Second', FListBox.Items[0]);
  Assert.AreEqual(200, TTestData(FListBox.Items.Objects[0]).Value);
  Assert.AreEqual('First', FListBox.Items[1]);
  Assert.AreEqual(100, TTestData(FListBox.Items.Objects[1]).Value);

  Obj1.Free;
  Obj2.Free;
end;


{ Remove Tests }

procedure TTestCubicListBox.TestRemoveDuplicates_RemovesDupes;
begin
  FListBox.Items.Add('Apple');
  FListBox.Items.Add('Banana');
  FListBox.Items.Add('Apple');
  FListBox.Items.Add('Cherry');
  FListBox.Items.Add('banana');

  var Removed:= FListBox.RemoveDuplicates;

  Assert.AreEqual(2, Removed);
  Assert.AreEqual(3, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestRemoveDuplicates_CaseInsensitive;
begin
  FListBox.Items.Add('Test');
  FListBox.Items.Add('TEST');
  FListBox.Items.Add('test');

  var Removed:= FListBox.RemoveDuplicates;

  Assert.AreEqual(2, Removed);
  Assert.AreEqual(1, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestRemoveDuplicates_NoDupes;
begin
  FListBox.Items.Add('Apple');
  FListBox.Items.Add('Banana');
  FListBox.Items.Add('Cherry');

  var Removed:= FListBox.RemoveDuplicates;

  Assert.AreEqual(0, Removed);
  Assert.AreEqual(3, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestRemoveEmptyLines_RemovesEmpty;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('');
  FListBox.Items.Add('Second');
  FListBox.Items.Add(' ');
  FListBox.Items.Add('Third');

  var Removed:= FListBox.RemoveEmptyLines;

  Assert.AreEqual(2, Removed);
  Assert.AreEqual(3, FListBox.Items.Count);
end;


procedure TTestCubicListBox.TestRemoveEmptyLines_NoEmpty;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  var Removed:= FListBox.RemoveEmptyLines;

  Assert.AreEqual(0, Removed);
end;


procedure TTestCubicListBox.TestRemoveLongLines_RemovesLong;
begin
  FListBox.Items.Add('Short');
  FListBox.Items.Add('This is a very long line that exceeds the limit');
  FListBox.Items.Add('OK');

  var Removed:= FListBox.RemoveLongLines(10);

  Assert.AreEqual(1, Removed);
  Assert.AreEqual(2, FListBox.Items.Count);
  Assert.AreEqual('Short', FListBox.Items[0]);
  Assert.AreEqual('OK', FListBox.Items[1]);
end;


procedure TTestCubicListBox.TestTrim_TrimsWhitespace;
begin
  FListBox.Items.Add('  First  ');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('  Third');

  FListBox.Trim;

  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Second', FListBox.Items[1]);
  Assert.AreEqual('Third', FListBox.Items[2]);
end;


{ FindItem Tests }

procedure TTestCubicListBox.TestFindItem_Found;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  Assert.AreEqual(1, FListBox.FindItem('Second'));
end;


procedure TTestCubicListBox.TestFindItem_NotFound;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  Assert.AreEqual(-1, FListBox.FindItem('NotThere'));
end;


procedure TTestCubicListBox.TestFindItem_CaseInsensitive;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');

  Assert.AreEqual(1, FListBox.FindItem('SECOND'));
  Assert.AreEqual(1, FListBox.FindItem('second'));
end;


{ MoveItemsTo Tests }

procedure TTestCubicListBox.TestMoveItemsTo_MovesItems;
var
  TargetListBox: TCubicListBox;
begin
  TargetListBox:= TCubicListBox.Create(FForm);
  TargetListBox.Parent:= FForm;
  try
    FListBox.Items.Add('First');
    FListBox.Items.Add('Second');
    FListBox.Items.Add('Third');

    FListBox.MoveItemsTo(TargetListBox, 2);

    Assert.AreEqual(1, FListBox.Items.Count);
    Assert.AreEqual('Third', FListBox.Items[0]);

    Assert.AreEqual(2, TargetListBox.Items.Count);
    Assert.AreEqual('First', TargetListBox.Items[0]);
    Assert.AreEqual('Second', TargetListBox.Items[1]);
  finally
    FreeAndNil(TargetListBox);
  end;
end;


procedure TTestCubicListBox.TestMoveItemsTo_PreservesObjects;
var
  TargetListBox: TCubicListBox;
  Obj1, Obj2: TTestData;
begin
  TargetListBox:= TCubicListBox.Create(FForm);
  TargetListBox.Parent:= FForm;

  Obj1:= TTestData.Create(100);
  Obj2:= TTestData.Create(200);

  try
    FListBox.Items.AddObject('First', Obj1);
    FListBox.Items.AddObject('Second', Obj2);

    FListBox.MoveItemsTo(TargetListBox, 1);

    Assert.AreEqual(1, TargetListBox.Items.Count);
    Assert.AreEqual('First', TargetListBox.Items[0]);
    Assert.AreEqual(100, TTestData(TargetListBox.Items.Objects[0]).Value);
  finally
    FreeAndNil(TargetListBox);
    Obj1.Free;
    Obj2.Free;
  end;
end;


procedure TTestCubicListBox.TestMoveItemsTo_ZeroCount;
var
  TargetListBox: TCubicListBox;
begin
  TargetListBox:= TCubicListBox.Create(FForm);
  TargetListBox.Parent:= FForm;
  try
    FListBox.Items.Add('First');
    FListBox.Items.Add('Second');

    FListBox.MoveItemsTo(TargetListBox, 0);

    Assert.AreEqual(2, FListBox.Items.Count);
    Assert.AreEqual(0, TargetListBox.Items.Count);
  finally
    FreeAndNil(TargetListBox);
  end;
end;


procedure TTestCubicListBox.TestMoveItemsTo_MoreThanAvailable;
var
  TargetListBox: TCubicListBox;
begin
  TargetListBox:= TCubicListBox.Create(FForm);
  TargetListBox.Parent:= FForm;
  try
    FListBox.Items.Add('First');
    FListBox.Items.Add('Second');

    FListBox.MoveItemsTo(TargetListBox, 10);

    Assert.AreEqual(0, FListBox.Items.Count);
    Assert.AreEqual(2, TargetListBox.Items.Count);
  finally
    FreeAndNil(TargetListBox);
  end;
end;


{ VisibleItems Tests }

procedure TTestCubicListBox.TestVisibleItems_ZeroItemHeight;
begin
  FListBox.ItemHeight:= 0;

  Assert.AreEqual(0, FListBox.VisibleItems);
end;


{ DeleteSelected Tests }

procedure TTestCubicListBox.TestDeleteSelected_DeletesSelected;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');

  FListBox.Selected[1]:= TRUE;
  FListBox.DeleteSelected;

  Assert.AreEqual(2, FListBox.Items.Count);
  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Third', FListBox.Items[1]);
end;


procedure TTestCubicListBox.TestDeleteSelected_FreesObjects;
var
  Obj: TTestData;
begin
  Obj:= TTestData.Create(42);

  FListBox.Items.Add('First');
  FListBox.Items.AddObject('Second', Obj);
  FListBox.Items.Add('Third');

  FListBox.Selected[1]:= TRUE;
  FListBox.DeleteSelected(TRUE);  // FreeObject = TRUE

  Assert.AreEqual(2, FListBox.Items.Count);
  // Object should be freed, we can't check its Value anymore
end;


procedure TTestCubicListBox.TestDeleteSelected_MultipleSelected;
begin
  FListBox.Items.Add('First');
  FListBox.Items.Add('Second');
  FListBox.Items.Add('Third');
  FListBox.Items.Add('Fourth');

  FListBox.Selected[1]:= TRUE;
  FListBox.Selected[3]:= TRUE;
  FListBox.DeleteSelected;

  Assert.AreEqual(2, FListBox.Items.Count);
  Assert.AreEqual('First', FListBox.Items[0]);
  Assert.AreEqual('Third', FListBox.Items[1]);
end;


{ I/O Tests }

procedure TTestCubicListBox.TestLoadFromFile_NonExistent;
begin
  // Should not crash when file doesn't exist
  FListBox.LoadFromFile('C:\NonExistentFile_12345.txt');
  Assert.AreEqual(0, FListBox.Items.Count);
end;


{ AddLimit Tests }

procedure TTestCubicListBox.TestAddLimit_LimitsItems;
begin
  // With ItemHeight=20 and Height=200, VisibleItems should be around 9
  // AddLimit should keep items within visible range

  for var i:= 1 to 20 do
    FListBox.AddLimit('Item ' + IntToStr(i));

  // Should have limited items based on visible count
  Assert.IsTrue(FListBox.Items.Count <= FListBox.VisibleItems + 1);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicListBox);

end.
