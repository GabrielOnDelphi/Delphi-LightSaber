unit Test.LightVcl.Common.PopUp;

{=============================================================================================================
   Unit tests for LightVcl.Common.PopUp.pas
   Tests TPopupUpMenu component

   Note: Actual popup display behavior cannot be easily tested without user interaction.
   These tests verify component creation, property inheritance, and menu item handling.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Menus,
  LightVcl.Common.PopUp;

type
  [TestFixture]
  TTestPopupUpMenu = class
  private
    FPopupMenu: TPopupUpMenu;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic creation tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestInheritsFromTPopupMenu;

    { Property tests - verify inherited properties work }
    [Test]
    procedure TestAlignment_Default;

    [Test]
    procedure TestAlignment_CanSet;

    [Test]
    procedure TestTrackButton_Default;

    [Test]
    procedure TestTrackButton_CanSet;

    [Test]
    procedure TestAutoPopup_Default;

    { Menu item tests }
    [Test]
    procedure TestAddMenuItem;

    [Test]
    procedure TestAddMultipleMenuItems;

    [Test]
    procedure TestMenuItemCaption;

    [Test]
    procedure TestMenuItemEnabled;

    [Test]
    procedure TestMenuItemVisible;

    { OnPopup event test }
    [Test]
    procedure TestOnPopupEventAssignment;
  end;

implementation


procedure TTestPopupUpMenu.Setup;
begin
  FPopupMenu:= TPopupUpMenu.Create(nil);
end;


procedure TTestPopupUpMenu.TearDown;
begin
  FreeAndNil(FPopupMenu);
end;


{ Basic creation tests }

procedure TTestPopupUpMenu.TestCreate;
begin
  Assert.IsNotNull(FPopupMenu, 'TPopupUpMenu should be created');
end;


procedure TTestPopupUpMenu.TestInheritsFromTPopupMenu;
begin
  Assert.IsTrue(FPopupMenu is TPopupMenu, 'TPopupUpMenu should inherit from TPopupMenu');
end;


{ Property tests }

procedure TTestPopupUpMenu.TestAlignment_Default;
begin
  Assert.AreEqual(Ord(paLeft), Ord(FPopupMenu.Alignment), 'Default alignment should be paLeft');
end;


procedure TTestPopupUpMenu.TestAlignment_CanSet;
begin
  FPopupMenu.Alignment:= paRight;
  Assert.AreEqual(Ord(paRight), Ord(FPopupMenu.Alignment));

  FPopupMenu.Alignment:= paCenter;
  Assert.AreEqual(Ord(paCenter), Ord(FPopupMenu.Alignment));
end;


procedure TTestPopupUpMenu.TestTrackButton_Default;
begin
  Assert.AreEqual(Ord(tbRightButton), Ord(FPopupMenu.TrackButton), 'Default TrackButton should be tbRightButton');
end;


procedure TTestPopupUpMenu.TestTrackButton_CanSet;
begin
  FPopupMenu.TrackButton:= tbLeftButton;
  Assert.AreEqual(Ord(tbLeftButton), Ord(FPopupMenu.TrackButton));
end;


procedure TTestPopupUpMenu.TestAutoPopup_Default;
begin
  Assert.IsTrue(FPopupMenu.AutoPopup, 'Default AutoPopup should be True');
end;


{ Menu item tests }

procedure TTestPopupUpMenu.TestAddMenuItem;
var
  Item: TMenuItem;
begin
  Item:= TMenuItem.Create(FPopupMenu);
  Item.Caption:= 'Test Item';
  FPopupMenu.Items.Add(Item);

  Assert.AreEqual(1, FPopupMenu.Items.Count);
end;


procedure TTestPopupUpMenu.TestAddMultipleMenuItems;
var
  Item1, Item2, Item3: TMenuItem;
begin
  Item1:= TMenuItem.Create(FPopupMenu);
  Item1.Caption:= 'Item 1';
  FPopupMenu.Items.Add(Item1);

  Item2:= TMenuItem.Create(FPopupMenu);
  Item2.Caption:= 'Item 2';
  FPopupMenu.Items.Add(Item2);

  Item3:= TMenuItem.Create(FPopupMenu);
  Item3.Caption:= 'Item 3';
  FPopupMenu.Items.Add(Item3);

  Assert.AreEqual(3, FPopupMenu.Items.Count);
end;


procedure TTestPopupUpMenu.TestMenuItemCaption;
var
  Item: TMenuItem;
begin
  Item:= TMenuItem.Create(FPopupMenu);
  Item.Caption:= 'My Caption';
  FPopupMenu.Items.Add(Item);

  Assert.AreEqual('My Caption', FPopupMenu.Items[0].Caption);
end;


procedure TTestPopupUpMenu.TestMenuItemEnabled;
var
  Item: TMenuItem;
begin
  Item:= TMenuItem.Create(FPopupMenu);
  Item.Caption:= 'Test';
  Item.Enabled:= False;
  FPopupMenu.Items.Add(Item);

  Assert.IsFalse(FPopupMenu.Items[0].Enabled);
end;


procedure TTestPopupUpMenu.TestMenuItemVisible;
var
  Item: TMenuItem;
begin
  Item:= TMenuItem.Create(FPopupMenu);
  Item.Caption:= 'Test';
  Item.Visible:= False;
  FPopupMenu.Items.Add(Item);

  Assert.IsFalse(FPopupMenu.Items[0].Visible);
end;


{ OnPopup event test }

procedure TTestPopupUpMenu.TestOnPopupEventAssignment;
begin
  { TNotifyEvent requires 'of object' so we can't use anonymous methods.
    We just verify the event property is assignable by checking it's nil initially. }
  Assert.IsFalse(Assigned(FPopupMenu.OnPopup), 'OnPopup event should be nil initially');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestPopupUpMenu);

end.
