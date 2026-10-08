unit Test.LightVcl.Visual.RichEditResize;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichEditResize.pas
   Tests the TRichEditResize component - auto-resizing RichEdit functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: These tests require a VCL application context with a valid window handle.
   Some tests may not run in headless/console mode.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.UITypes,      { TScrollStyle }
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls,
  LightVcl.Visual.RichEditResize;

type
  [TestFixture]
  TTestRichEditResize = class
  private
    FForm: TForm;
    FPanel: TPanel;
    FRichEdit: TRichEditResize;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestDefaultMinHeight;

    [Test]
    procedure TestScrollBarsDisabled;

    { MinHeight Property Tests }
    [Test]
    procedure TestMinHeight_SetGet;

    [Test]
    procedure TestMinHeight_RespectsMinimum;

    { Resize Behavior Tests }
    [Test]
    procedure TestResize_SingleLine;

    [Test]
    procedure TestResize_MultipleLines;

    [Test]
    procedure TestResize_ParentHeightAdjusted;

    [Test]
    procedure TestResize_HeightIncreasesWithContent;

    { Edge Cases }
    [Test]
    procedure TestResize_EmptyText;

    [Test]
    procedure TestResize_LongText;
  end;

implementation


procedure TTestRichEditResize.Setup;
begin
  FForm:= TForm.CreateNew(nil);
  FForm.Width:= 400;
  FForm.Height:= 400;
  FForm.Position:= poScreenCenter;

  { Create a panel as parent - TRichEditResize resizes its parent }
  FPanel:= TPanel.Create(FForm);
  FPanel.Parent:= FForm;
  FPanel.Align:= alTop;
  FPanel.Height:= 100;
  FPanel.AutoSize:= False; { AutoSize must be false for TRichEditResize }

  FRichEdit:= TRichEditResize.Create(FForm);
  FRichEdit.Parent:= FPanel;
  FRichEdit.Left:= 0;
  FRichEdit.Top:= 0;
  FRichEdit.Width:= FPanel.ClientWidth;

  FForm.Show;
  Application.ProcessMessages;
end;


procedure TTestRichEditResize.TearDown;
begin
  FreeAndNil(FRichEdit);
  FreeAndNil(FPanel);
  FreeAndNil(FForm);
end;


{ Basic Tests }

procedure TTestRichEditResize.TestDefaultMinHeight;
begin
  Assert.AreEqual(50, FRichEdit.MinHeight, 'Default MinHeight should be 50');
end;


procedure TTestRichEditResize.TestScrollBarsDisabled;
begin
  { ssNone is also the VCL default, so start from ssVertical: TCustomMemo.SetScrollBars recreates the window,
    and the new CreateWnd must switch the scroll bars off again }
  FRichEdit.ScrollBars:= TScrollStyle.ssVertical;
  FRichEdit.HandleNeeded;
  Assert.IsTrue(FRichEdit.HandleAllocated, 'Precondition: the window was recreated');
  Assert.AreEqual(TScrollStyle.ssNone, FRichEdit.ScrollBars, 'CreateWnd must set ScrollBars to ssNone');
end;


{ MinHeight Property Tests }

procedure TTestRichEditResize.TestMinHeight_SetGet;
begin
  { One line is far shorter than either minimum, so the control must take exactly MinHeight }
  FRichEdit.MinHeight:= 100;
  FRichEdit.Lines.Clear;
  FRichEdit.Lines.Add('One line');
  Application.ProcessMessages;
  Assert.AreEqual(100, FRichEdit.Height, 'Height must equal MinHeight=100');

  FRichEdit.MinHeight:= 25;
  FRichEdit.Lines.Clear;
  FRichEdit.Lines.Add('One line');      { Change triggers the resize }
  Application.ProcessMessages;
  Assert.AreEqual(25, FRichEdit.Height, 'Height must follow a lower MinHeight=25');
end;


procedure TTestRichEditResize.TestMinHeight_RespectsMinimum;
begin
  { 150 is above the default height of the control (89), so only the MinHeight clamp can produce it }
  FRichEdit.MinHeight:= 150;
  FRichEdit.Lines.Add('One line');
  FRichEdit.Lines.Clear;
  Application.ProcessMessages;

  Assert.AreEqual(150, FRichEdit.Height, 'Empty content must take exactly MinHeight=150');
end;


{ Resize Behavior Tests }

procedure TTestRichEditResize.TestResize_SingleLine;
begin
  FRichEdit.Lines.Clear;
  FRichEdit.Lines.Add('Single line of text');
  Application.ProcessMessages;

  { Height should be at least MinHeight for single line }
  Assert.IsTrue(FRichEdit.Height >= FRichEdit.MinHeight, 'Height should be at least MinHeight');
end;


procedure TTestRichEditResize.TestResize_MultipleLines;
var
  HeightAfterOneLine, HeightAfterMultipleLines: Integer;
  i: Integer;
begin
  FRichEdit.MinHeight:= 10; { Set low MinHeight to see actual content height }
  FRichEdit.Lines.Clear;
  FRichEdit.Lines.Add('Line 1');
  Application.ProcessMessages;
  HeightAfterOneLine:= FRichEdit.Height;

  { Add more lines }
  for i:= 2 to 10 do
    FRichEdit.Lines.Add('Line ' + IntToStr(i));
  Application.ProcessMessages;
  HeightAfterMultipleLines:= FRichEdit.Height;

  Assert.IsTrue(HeightAfterMultipleLines > HeightAfterOneLine,
    'Height should increase with more lines');
end;


procedure TTestRichEditResize.TestResize_ParentHeightAdjusted;
var
  InitialPanelHeight, FinalPanelHeight: Integer;
  i: Integer;
begin
  FRichEdit.MinHeight:= 10;
  FRichEdit.Lines.Clear;
  FRichEdit.Lines.Add('Line 1');
  Application.ProcessMessages;
  InitialPanelHeight:= FPanel.Height;

  { Add many lines to force resize }
  for i:= 2 to 20 do
    FRichEdit.Lines.Add('Line ' + IntToStr(i));
  Application.ProcessMessages;
  FinalPanelHeight:= FPanel.Height;

  Assert.IsTrue(FinalPanelHeight > InitialPanelHeight,
    'Parent panel height should increase with content');
end;


procedure TTestRichEditResize.TestResize_HeightIncreasesWithContent;
var
  Heights: array[1..5] of Integer;
  i, j: Integer;
begin
  FRichEdit.MinHeight:= 10;
  FRichEdit.Lines.Clear;

  for i:= 1 to 5 do
  begin
    for j:= 1 to 5 do
      FRichEdit.Lines.Add('Line ' + IntToStr((i-1)*5 + j));
    Application.ProcessMessages;
    Heights[i]:= FRichEdit.Height;
  end;

  { Height = LineHeight * LineCount, so every batch of 5 lines adds the same step }
  Assert.IsTrue(Heights[2] > Heights[1], 'Height must grow after the second batch');
  for i:= 2 to 5 do
    Assert.AreEqual(Heights[2] - Heights[1], Heights[i] - Heights[i-1],
      'Batch ' + IntToStr(i) + ' must add the same height as batch 2');
end;


{ Edge Cases }

procedure TTestRichEditResize.TestResize_EmptyText;
var
  i: Integer;
begin
  { Grow the control first, then clear it: it must shrink back to the default MinHeight (50), and its parent with it }
  for i:= 1 to 20 do
    FRichEdit.Lines.Add('Line ' + IntToStr(i));
  Application.ProcessMessages;
  Assert.IsTrue(FRichEdit.Height > 50, 'Precondition: 20 lines are higher than MinHeight');

  FRichEdit.Lines.Clear;
  Application.ProcessMessages;

  Assert.AreEqual(50, FRichEdit.Height, 'Empty RichEdit must shrink to MinHeight=50');
  Assert.AreEqual(FRichEdit.Top + 50 + 5, FPanel.Height, 'Parent = Top + MinHeight + 5 pixels padding');
end;


procedure TTestRichEditResize.TestResize_LongText;
var
  i: Integer;
begin
  FRichEdit.MinHeight:= 10;
  FRichEdit.Lines.Clear;

  { Add 100 lines }
  for i:= 1 to 100 do
    FRichEdit.Lines.Add('This is line number ' + IntToStr(i) + ' with some extra text');
  Application.ProcessMessages;

  { Height should be substantial }
  Assert.IsTrue(FRichEdit.Height > 500,
    'Height should be large for 100 lines of text');

  { Parent should also be resized }
  Assert.IsTrue(FPanel.Height > 500,
    'Parent height should be large for 100 lines of text');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichEditResize);

end.
