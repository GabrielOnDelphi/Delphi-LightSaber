unit Test.FrameExplorer;

{=============================================================================================================
   Unit tests for FrameExplorer.pas
   Tests TFrameWinExplorer - the Windows Explorer-like file browser frame.

   Note: These tests focus on frame creation and component existence.
   Full file browsing testing requires actual file system access.

   The frame uses custom LightSaber components (TCubicFileList, TCubicPathEdit, TCubicFilterBox)
   which must be properly registered for all tests to pass.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.FileCtrl;

type
  [TestFixture]
  TTestFrameExplorer = class
  private
    FTestFrame: TObject;
    FParentForm: TForm;
    procedure CleanupFrame;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Frame Creation Tests }
    [Test]
    procedure TestFrameClassExists;

    [Test]
    procedure TestFrameCreate_Succeeds;

    [Test]
    procedure TestFrameCreate_WithParentForm;

    { Component Tests }
    [Test]
    procedure TestFrameHasRefreshButton;

    [Test]
    procedure TestFrameHasDirectoryListBox;

    [Test]
    procedure TestFrameHasFileList;

    [Test]
    procedure TestFrameHasFilter;

    [Test]
    procedure TestFrameHasTopLabel;

    [Test]
    procedure TestFrameHasPanel;

    [Test]
    procedure TestFrameHasPath;

    [Test]
    procedure TestFrameHasSplitter;

    { Button Tests }
    [Test]
    procedure TestRefreshButton_IsButton;

    [Test]
    procedure TestRefreshButton_HasClickHandler;

    [Test]
    procedure TestRefreshButton_CaptionIsRef;

    [Test]
    procedure TestRefreshButton_IsDefault;

    { DirectoryListBox Tests }
    [Test]
    procedure TestDirectory_IsDirectoryListBox;

    [Test]
    procedure TestDirectory_HasChangeHandler;

    [Test]
    procedure TestDirectory_LinkedToFileList;

    { Label Tests }
    [Test]
    procedure TestTopLabel_IsLabel;

    [Test]
    procedure TestTopLabel_CaptionIsFileExplorer;

    [Test]
    procedure TestTopLabel_AlignIsTop;

    { Splitter Tests }
    [Test]
    procedure TestSplitter_IsSplitter;

    [Test]
    procedure TestSplitter_AlignIsTop;

    [Test]
    procedure TestSplitter_CursorIsVSplit;

    { Panel Tests }
    [Test]
    procedure TestPanel_IsPanel;

    [Test]
    procedure TestPanel_BevelOuterIsNone;

    { Frame Attribute Tests }
    [Test]
    procedure TestFrameName_IsFrameWinExplorer;

    [Test]
    procedure TestFrameShowHint_IsEnabled;

    { Event Handler Tests }
    [Test]
    procedure TestDirectoryChange_NoException;

    [Test]
    procedure TestBtnRefreshClick_NoException;

    { Component Linking Tests }
    [Test]
    procedure TestFileList_AlignIsClient;

    [Test]
    procedure TestFilter_LinkedToFileList;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  FrameExplorer;


procedure TTestFrameExplorer.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
  FTestFrame:= NIL;
  FParentForm:= NIL;
end;


procedure TTestFrameExplorer.TearDown;
begin
  CleanupFrame;
end;


procedure TTestFrameExplorer.CleanupFrame;
var
  Frame: TFrameWinExplorer;
begin
  if FTestFrame <> NIL then
  begin
    Frame:= TFrameWinExplorer(FTestFrame);
    FreeAndNil(Frame);
    FTestFrame:= NIL;
  end;

  if FParentForm <> NIL then
    FreeAndNil(FParentForm);
end;


{ Frame Creation Tests }

procedure TTestFrameExplorer.TestFrameClassExists;
begin
  Assert.IsNotNull(TFrameWinExplorer, 'TFrameWinExplorer class should exist');
end;


procedure TTestFrameExplorer.TestFrameCreate_Succeeds;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame, 'Frame creation should succeed');
end;


procedure TTestFrameExplorer.TestFrameCreate_WithParentForm;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  Frame.Parent:= FParentForm;
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.Parent = FParentForm, 'Parent should be the form');
end;


{ Component Tests }

procedure TTestFrameExplorer.TestFrameHasRefreshButton;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.btnRefresh, 'Frame should have btnRefresh component');
end;


procedure TTestFrameExplorer.TestFrameHasDirectoryListBox;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.Directory, 'Frame should have Directory component');
end;


procedure TTestFrameExplorer.TestFrameHasFileList;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.FileList, 'Frame should have FileList component');
end;


procedure TTestFrameExplorer.TestFrameHasFilter;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.Filter, 'Frame should have Filter component');
end;


procedure TTestFrameExplorer.TestFrameHasTopLabel;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.lblTop, 'Frame should have lblTop component');
end;


procedure TTestFrameExplorer.TestFrameHasPanel;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.Panel2, 'Frame should have Panel2 component');
end;


procedure TTestFrameExplorer.TestFrameHasPath;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.Path, 'Frame should have Path component');
end;


procedure TTestFrameExplorer.TestFrameHasSplitter;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsNotNull(Frame.Splitter, 'Frame should have Splitter component');
end;


{ Button Tests }

procedure TTestFrameExplorer.TestRefreshButton_IsButton;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.btnRefresh is TButton, 'btnRefresh should be a TButton');
end;


procedure TTestFrameExplorer.TestRefreshButton_HasClickHandler;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Assigned(Frame.btnRefresh.OnClick),
    'btnRefresh should have OnClick handler assigned');
end;


procedure TTestFrameExplorer.TestRefreshButton_CaptionIsRef;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual('Ref', Frame.btnRefresh.Caption,
    'btnRefresh caption should be "Ref"');
end;


procedure TTestFrameExplorer.TestRefreshButton_IsDefault;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.btnRefresh.Default,
    'btnRefresh should be the default button');
end;


{ DirectoryListBox Tests }

procedure TTestFrameExplorer.TestDirectory_IsDirectoryListBox;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.Directory is TDirectoryListBox,
    'Directory should be a TDirectoryListBox');
end;


procedure TTestFrameExplorer.TestDirectory_HasChangeHandler;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Assigned(Frame.Directory.OnChange),
    'Directory should have OnChange handler assigned');
end;


procedure TTestFrameExplorer.TestDirectory_LinkedToFileList;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.FileList = Frame.Directory.FileList,
    'Directory.FileList should be linked to FileList');
end;


{ Label Tests }

procedure TTestFrameExplorer.TestTopLabel_IsLabel;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.lblTop is TLabel, 'lblTop should be a TLabel');
end;


procedure TTestFrameExplorer.TestTopLabel_CaptionIsFileExplorer;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual('FILE EXPLORER', Frame.lblTop.Caption,
    'lblTop caption should be "FILE EXPLORER"');
end;


procedure TTestFrameExplorer.TestTopLabel_AlignIsTop;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual(alTop, Frame.lblTop.Align,
    'lblTop should align to top');
end;


{ Splitter Tests }

procedure TTestFrameExplorer.TestSplitter_IsSplitter;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.Splitter is TSplitter, 'Splitter should be a TSplitter');
end;


procedure TTestFrameExplorer.TestSplitter_AlignIsTop;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual(alTop, Frame.Splitter.Align,
    'Splitter should align to top');
end;


procedure TTestFrameExplorer.TestSplitter_CursorIsVSplit;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.Splitter.Cursor = crVSplit,
    'Splitter cursor should be crVSplit');
end;


{ Panel Tests }

procedure TTestFrameExplorer.TestPanel_IsPanel;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.Panel2 is TPanel, 'Panel2 should be a TPanel');
end;


procedure TTestFrameExplorer.TestPanel_BevelOuterIsNone;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual(bvNone, Frame.Panel2.BevelOuter,
    'Panel2 BevelOuter should be bvNone');
end;


{ Frame Attribute Tests }

procedure TTestFrameExplorer.TestFrameName_IsFrameWinExplorer;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual('FrameWinExplorer', Frame.Name,
    'Frame name must be "FrameWinExplorer"');
end;


procedure TTestFrameExplorer.TestFrameShowHint_IsEnabled;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.ShowHint,
    'ShowHint should be enabled');
end;


{ Event Handler Tests }

procedure TTestFrameExplorer.TestDirectoryChange_NoException;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  { DirectoryChange should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Frame.DirectoryChange(Frame);
    end);
end;


procedure TTestFrameExplorer.TestBtnRefreshClick_NoException;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  { btnRefreshClick should not raise exception }
  Assert.WillNotRaise(
    procedure
    begin
      Frame.btnRefreshClick(Frame);
    end);
end;


{ Component Linking Tests }

procedure TTestFrameExplorer.TestFileList_AlignIsClient;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.AreEqual(alClient, Frame.FileList.Align,
    'FileList should align to client');
end;


procedure TTestFrameExplorer.TestFilter_LinkedToFileList;
var
  Frame: TFrameWinExplorer;
begin
  FParentForm:= TForm.Create(NIL);
  Frame:= TFrameWinExplorer.Create(FParentForm);
  FTestFrame:= Frame;

  Assert.IsTrue(Frame.FileList = Frame.Filter.FileList,
    'Filter.FileList should be linked to FileList');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFrameExplorer);

end.
