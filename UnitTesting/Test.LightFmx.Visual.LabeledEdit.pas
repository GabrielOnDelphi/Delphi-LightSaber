unit Test.LightFmx.Visual.LabeledEdit;

{=============================================================================================================
   Unit tests for LightFmx.Visual.LabeledEdit.pas
   Tests the TLabeledEdit FMX component.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.Controls,
  FMX.Edit,
  FMX.StdCtrls,
  LightFmx.Visual.LabeledEdit;

type
  [TestFixture]
  TTestLabeledEdit = class
  private
    FLabeledEdit: TLabeledEdit;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_ChildrenNotNil;

    [Test]
    procedure TestCreate_DefaultLabelText;

    [Test]
    procedure TestCreate_DefaultEditText;

    [Test]
    procedure TestCreate_LabelPositionedAboveEdit;

    { Property Tests }
    [Test]
    procedure TestSetLabelText;

    [Test]
    procedure TestSetText;

    [Test]
    procedure TestEditPropertyReturnsInternalEdit;

    [Test]
    procedure TestLabelControlPropertyReturnsInternalLabel;

    { Layout Tests }
    [Test]
    procedure TestResize_EditWidthMatchesParent;

    [Test]
    procedure TestResize_LabelWidthMatchesParent;

    { Component Registration }
    [Test]
    procedure TestRegister_NoException;
  end;

implementation


procedure TTestLabeledEdit.Setup;
begin
  FLabeledEdit:= TLabeledEdit.Create(NIL);
end;


procedure TTestLabeledEdit.TearDown;
begin
  FreeAndNil(FLabeledEdit);
end;


{ Constructor Tests }

procedure TTestLabeledEdit.TestCreate_ChildrenNotNil;
begin
  Assert.IsNotNull(FLabeledEdit.Edit, 'Edit child should be created');
  Assert.IsNotNull(FLabeledEdit.LabelControl, 'Label child should be created');
end;


procedure TTestLabeledEdit.TestCreate_DefaultLabelText;
begin
  Assert.AreEqual('Label:', FLabeledEdit.LabelText, 'Default label text should be "Label:"');
end;


procedure TTestLabeledEdit.TestCreate_DefaultEditText;
begin
  Assert.AreEqual('', FLabeledEdit.Text, 'Default edit text should be empty');
end;


procedure TTestLabeledEdit.TestCreate_LabelPositionedAboveEdit;
begin
  Assert.IsTrue(FLabeledEdit.LabelControl.Position.Y < FLabeledEdit.Edit.Position.Y,
    'Label should be positioned above Edit');
end;


{ Property Tests }

procedure TTestLabeledEdit.TestSetLabelText;
const
  TEST_TEXT = 'Test Label:';
begin
  FLabeledEdit.LabelText:= TEST_TEXT;
  Assert.AreEqual(TEST_TEXT, FLabeledEdit.LabelText);
  Assert.AreEqual(TEST_TEXT, FLabeledEdit.LabelControl.Text, 'Internal label should have same text');
end;


procedure TTestLabeledEdit.TestSetText;
const
  TEST_TEXT = 'Test Value';
begin
  FLabeledEdit.Text:= TEST_TEXT;
  Assert.AreEqual(TEST_TEXT, FLabeledEdit.Text);
  Assert.AreEqual(TEST_TEXT, FLabeledEdit.Edit.Text, 'Internal edit should have same text');
end;


procedure TTestLabeledEdit.TestEditPropertyReturnsInternalEdit;
begin
  FLabeledEdit.Text:= 'Direct Test';
  Assert.AreEqual('Direct Test', FLabeledEdit.Edit.Text,
    'Edit property should provide access to internal TEdit');
end;


procedure TTestLabeledEdit.TestLabelControlPropertyReturnsInternalLabel;
begin
  FLabeledEdit.LabelText:= 'Direct Label Test';
  Assert.AreEqual('Direct Label Test', FLabeledEdit.LabelControl.Text,
    'LabelControl property should provide access to internal TLabel');
end;


{ Layout Tests }

procedure TTestLabeledEdit.TestResize_EditWidthMatchesParent;
const
  NEW_WIDTH = 250;
begin
  FLabeledEdit.Width:= NEW_WIDTH;
  // Trigger resize manually since we have no parent
  FLabeledEdit.SetBounds(0, 0, NEW_WIDTH, FLabeledEdit.Height);

  Assert.AreEqual(NEW_WIDTH, FLabeledEdit.Edit.Width, 0.1,
    'Edit width should match parent width after resize');
end;


procedure TTestLabeledEdit.TestResize_LabelWidthMatchesParent;
const
  NEW_WIDTH = 250;
begin
  FLabeledEdit.Width:= NEW_WIDTH;
  // Trigger resize manually since we have no parent
  FLabeledEdit.SetBounds(0, 0, NEW_WIDTH, FLabeledEdit.Height);

  Assert.AreEqual(NEW_WIDTH, FLabeledEdit.LabelControl.Width, 0.1,
    'Label width should match parent width after resize');
end;


{ Component Registration }

procedure TTestLabeledEdit.TestRegister_NoException;
begin
  // Simply verify Register doesn't raise an exception
  Assert.WillNotRaise(
    procedure
    begin
      Register;
    end,
    Exception,
    'Register should not raise an exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLabeledEdit);

end.
