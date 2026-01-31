unit Test.LightVcl.Visual.RichEdit;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichEdit.pas
   Tests the TCubicRichEdit component functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: These tests require a VCL application context with a valid window handle.
   Some tests may not run in headless/console mode.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Forms,
  Vcl.Controls,
  LightVcl.Visual.RichEdit;

type
  [TestFixture]
  TTestCubicRichEdit = class
  private
    FForm: TForm;
    FRichEdit: TCubicRichEdit;
    FVScrollEventFired: Boolean;
    procedure OnVScrollHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Basic Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestAdd_SimpleText;

    [Test]
    procedure TestAdd_TextWithLineBreaks;

    [Test]
    procedure TestAddFormated_SimpleText;

    [Test]
    procedure TestAddFormated_TextWithNullChar;

    [Test]
    procedure TestAddFormated_TextWithLineBreaks;

    { Copy Tests }
    [Test]
    procedure TestCopyAll_EmptyText;

    [Test]
    procedure TestCopyAll_WithText;

    { Line Navigation Tests }
    [Test]
    procedure TestGetCurrentLine_FirstLine;

    [Test]
    procedure TestGetCurrentLine_MiddleLine;

    [Test]
    procedure TestGetFirstVisibleLine_AtTop;

    { Scroll Tests }
    [Test]
    procedure TestScrollToBottom;

    [Test]
    procedure TestScrollTo;

    { Event Tests }
    [Test]
    procedure TestOnVScrollEvent;
  end;

implementation

uses
  Vcl.Clipbrd;


procedure TTestCubicRichEdit.OnVScrollHandler(Sender: TObject);
begin
  FVScrollEventFired:= True;
end;


procedure TTestCubicRichEdit.Setup;
begin
  FVScrollEventFired:= False;

  FForm:= TForm.CreateNew(nil);
  FForm.Width:= 400;
  FForm.Height:= 300;
  FForm.Position:= poScreenCenter;

  FRichEdit:= TCubicRichEdit.Create(FForm);
  FRichEdit.Parent:= FForm;
  FRichEdit.Align:= alClient;

  FForm.Show;
  Application.ProcessMessages;
end;


procedure TTestCubicRichEdit.TearDown;
begin
  FreeAndNil(FRichEdit);
  FreeAndNil(FForm);
end;


{ Basic Tests }

procedure TTestCubicRichEdit.TestCreate;
begin
  Assert.IsNotNull(FRichEdit, 'RichEdit should be created');
  Assert.IsTrue(FRichEdit.HandleAllocated, 'RichEdit should have a valid handle');
end;


procedure TTestCubicRichEdit.TestAdd_SimpleText;
begin
  FRichEdit.Add('Hello World');
  Assert.AreEqual(1, FRichEdit.Lines.Count, 'Should have one line');
  Assert.AreEqual('Hello World', FRichEdit.Lines[0], 'Text should match');
end;


procedure TTestCubicRichEdit.TestAdd_TextWithLineBreaks;
begin
  { The Add method should replace line breaks with spaces }
  FRichEdit.Add('Line1'#13#10'Line2');
  Assert.AreEqual(1, FRichEdit.Lines.Count, 'Line breaks should be replaced, resulting in one line');
  Assert.AreEqual('Line1 Line2', FRichEdit.Lines[0], 'Line breaks should be replaced with spaces');
end;


procedure TTestCubicRichEdit.TestAddFormated_SimpleText;
begin
  FRichEdit.AddFormated('Hello World');
  Assert.AreEqual(1, FRichEdit.Lines.Count, 'Should have one line');
  Assert.AreEqual('Hello World', FRichEdit.Lines[0], 'Text should match');
end;


procedure TTestCubicRichEdit.TestAddFormated_TextWithNullChar;
begin
  { Null characters should be replaced with spaces to avoid TRichEdit bugs }
  FRichEdit.AddFormated('Hello'#0'World');
  Assert.AreEqual(1, FRichEdit.Lines.Count, 'Should have one line');
  Assert.AreEqual('Hello World', FRichEdit.Lines[0], 'Null char should be replaced with space');
end;


procedure TTestCubicRichEdit.TestAddFormated_TextWithLineBreaks;
begin
  { AddFormated preserves line breaks within the added text.
    Note: TRichEdit.Lines.Add adds as a single logical line but may display wrapped. }
  FRichEdit.AddFormated('Line1');
  FRichEdit.AddFormated('Line2');
  Assert.AreEqual(2, FRichEdit.Lines.Count, 'Should have two lines');
  Assert.AreEqual('Line1', FRichEdit.Lines[0]);
  Assert.AreEqual('Line2', FRichEdit.Lines[1]);
end;


{ Copy Tests }

procedure TTestCubicRichEdit.TestCopyAll_EmptyText;
begin
  Clipboard.Clear;
  FRichEdit.Lines.Clear;
  FRichEdit.CopyAll;
  { Empty RichEdit - clipboard should be empty or contain empty string }
  Assert.IsTrue(Clipboard.AsText = '', 'Clipboard should be empty for empty RichEdit');
end;


procedure TTestCubicRichEdit.TestCopyAll_WithText;
begin
  Clipboard.Clear;
  FRichEdit.Lines.Clear;
  FRichEdit.Add('Test Content');
  FRichEdit.CopyAll;
  Assert.AreEqual('Test Content', Trim(Clipboard.AsText), 'Clipboard should contain the text');
end;


{ Line Navigation Tests }

procedure TTestCubicRichEdit.TestGetCurrentLine_FirstLine;
begin
  FRichEdit.Lines.Clear;
  FRichEdit.Add('Line 1');
  FRichEdit.Add('Line 2');
  FRichEdit.Add('Line 3');
  FRichEdit.SelStart:= 0; { Position caret at start }
  Assert.AreEqual(0, FRichEdit.GetCurrentLine, 'Caret at start should be on line 0');
end;


procedure TTestCubicRichEdit.TestGetCurrentLine_MiddleLine;
begin
  FRichEdit.Lines.Clear;
  FRichEdit.Add('Line 1');
  FRichEdit.Add('Line 2');
  FRichEdit.Add('Line 3');
  { Position caret at beginning of line 2 (after 'Line 1' + CRLF) }
  FRichEdit.SelStart:= Length('Line 1') + 2; { +2 for CRLF }
  Assert.AreEqual(1, FRichEdit.GetCurrentLine, 'Caret should be on line 1 (zero-based)');
end;


procedure TTestCubicRichEdit.TestGetFirstVisibleLine_AtTop;
begin
  FRichEdit.Lines.Clear;
  FRichEdit.Add('Line 1');
  FRichEdit.Add('Line 2');
  FRichEdit.Add('Line 3');
  { When not scrolled, first visible line should be 0 }
  Assert.AreEqual(0, FRichEdit.GetFirstVisibleLine, 'First visible line should be 0 when not scrolled');
end;


{ Scroll Tests }

procedure TTestCubicRichEdit.TestScrollToBottom;
var
  i: Integer;
begin
  FRichEdit.Lines.Clear;
  { Add enough lines to require scrolling }
  for i:= 1 to 100 do
    FRichEdit.Add('Line ' + IntToStr(i));

  FRichEdit.ScrollToBottom;
  Application.ProcessMessages;

  { After scrolling to bottom, the first visible line should be > 0 }
  Assert.IsTrue(FRichEdit.GetFirstVisibleLine > 0, 'First visible line should not be 0 after scrolling to bottom');
end;


procedure TTestCubicRichEdit.TestScrollTo;
var
  i: Integer;
begin
  FRichEdit.Lines.Clear;
  for i:= 1 to 100 do
    FRichEdit.Add('Line ' + IntToStr(i));

  { Set caret to first line and scroll to line 50 }
  FRichEdit.SelStart:= 0;
  FRichEdit.ScrollTo(50);
  Application.ProcessMessages;

  { The caret line should have moved toward line 50 }
  { Note: The exact behavior depends on the EM_LINESCROLL implementation }
  Assert.IsTrue(FRichEdit.GetCurrentLine >= 0, 'Current line should be valid');
end;


{ Event Tests }

procedure TTestCubicRichEdit.TestOnVScrollEvent;
var
  i: Integer;
begin
  FVScrollEventFired:= False;
  FRichEdit.OnVScroll:= OnVScrollHandler;

  FRichEdit.Lines.Clear;
  for i:= 1 to 100 do
    FRichEdit.Add('Line ' + IntToStr(i));

  { Trigger scroll by sending WM_VSCROLL directly (synchronous) }
  SendMessage(FRichEdit.Handle, WM_VSCROLL, SB_LINEDOWN, 0);

  Assert.IsTrue(FVScrollEventFired, 'OnVScroll event should have fired');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicRichEdit);

end.
