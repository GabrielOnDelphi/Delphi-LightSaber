unit Test.LightVcl.Visual.MinimalPathLabel;

{=============================================================================================================
   2026.10.08
   Unit tests for LightVcl.Visual.MinimalPathLabel.pas
   Tests the TMinimalPathLabel component that truncates file paths to fit within label width.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Graphics;

type
  [TestFixture]
  TTestMinimalPathLabel = class
  private
    FForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultShowHint;

    [Test]
    procedure TestCreate_DefaultShowFullTextAsHint;

    [Test]
    procedure TestCreate_DefaultCaption;

    { CaptionMin Property Tests }
    [Test]
    procedure TestCaptionMin_SetShortPath;

    [Test]
    procedure TestCaptionMin_SetLongPath;

    [Test]
    procedure TestCaptionMin_EmptyString;

    [Test]
    procedure TestCaptionMin_StoresFullPath;

    { ShowFullTextAsHint Tests }
    [Test]
    procedure TestShowFullTextAsHint_True_SetsHint;

    [Test]
    procedure TestShowFullTextAsHint_False_NoHint;

    { Resize Tests }
    [Test]
    procedure TestResize_UpdatesCaption;

    [Test]
    procedure TestResize_NarrowWidth_TruncatesPath;

    [Test]
    procedure TestResize_WideWidth_ShowsFullPath;

    { Path Format Tests }
    [Test]
    procedure TestCaptionMin_WindowsPath;

    [Test]
    procedure TestCaptionMin_UNCPath;

    [Test]
    procedure TestCaptionMin_RelativePath;

    { Edge Cases }
    [Test]
    procedure TestCaptionMin_VeryLongPath;

    [Test]
    procedure TestCaptionMin_PathWithSpaces;

    [Test]
    procedure TestCaptionMin_PathWithUnicode;

    [Test]
    procedure TestCaptionMin_JustFilename;

    [Test]
    procedure TestCaptionMin_ZeroWidth_NoException;

    [Test]
    procedure TestCaptionMin_BeforeParented;
  end;

implementation

uses
  LightVcl.Visual.MinimalPathLabel;


{ Turns AutoSize off and makes the label exactly as wide as Shown, measured on the label's own canvas.
  Vcl.FileCtrl.MinimizeName (c:\Delphi\Delphi 13\source\vcl\Vcl.FileCtrl.pas:603-634) cuts one directory
  at a time from the front of the path and stops at the first form whose width is not above the label's.
  Every form before Shown holds more text than Shown, so at this width the label must show exactly Shown. }
procedure SetWidthToText(Lbl: TMinimalPathLabel; CONST Shown: string);
begin
  Lbl.AutoSize:= FALSE;
  Lbl.Width:= Lbl.Canvas.TextWidth(Shown);
end;


procedure TTestMinimalPathLabel.Setup;
begin
  FForm:= TForm.CreateNew(nil);
  FForm.Width:= 800;
  FForm.Height:= 600;
end;


procedure TTestMinimalPathLabel.TearDown;
begin
  FreeAndNil(FForm);
end;


{ Constructor Tests }

procedure TTestMinimalPathLabel.TestCreate_DefaultShowHint;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  try
    Assert.IsTrue(Lbl.ShowHint, 'ShowHint should be TRUE by default');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCreate_DefaultShowFullTextAsHint;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  try
    Assert.IsTrue(Lbl.ShowFullTextAsHint, 'ShowFullTextAsHint should be TRUE by default');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCreate_DefaultCaption;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  try
    Assert.AreEqual('Minimized text', Lbl.CaptionMin, 'Default CaptionMin should be "Minimized text"');
  finally
    FreeAndNil(Lbl);
  end;
end;


{ CaptionMin Property Tests }

procedure TTestMinimalPathLabel.TestCaptionMin_SetShortPath;
var
  Lbl: TMinimalPathLabel;
  ShortPath: string;
begin
  ShortPath:= 'C:\Test.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  { AutoSize must go off FIRST. TLabel autosizes by default, so TCustomLabel.AdjustBounds would
    shrink the label back to the width of its text and the 400 below would never take effect. }
  Lbl.AutoSize:= FALSE;
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= ShortPath;

    Assert.AreEqual(ShortPath, Lbl.Caption, 'Short path should not be truncated');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_SetLongPath;
var
  Lbl: TMinimalPathLabel;
  LongPath: string;
begin
  LongPath:= 'C:\Very\Long\Path\That\Goes\On\And\On\Forever\Until\It\Cannot\Fit\In\The\Label\Width\Anymore\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    SetWidthToText(Lbl, 'C:\...\Width\Anymore\File.txt');  { Narrow: the drive, the ellipsis, the last two folders and the file name }
    Lbl.CaptionMin:= LongPath;

    Assert.AreEqual('C:\...\Width\Anymore\File.txt', Lbl.Caption, 'The long path must be cut in the middle, keeping the drive and the file name');
    Assert.AreEqual(LongPath, Lbl.CaptionMin, 'CaptionMin should store the full path');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_EmptyString;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    Lbl.CaptionMin:= '';

    Assert.AreEqual('', Lbl.CaptionMin, 'Empty string should be accepted');
    Assert.AreEqual('', Lbl.Caption, 'Caption should be empty');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_StoresFullPath;
var
  Lbl: TMinimalPathLabel;
  TestPath: string;
begin
  TestPath:= 'C:\Users\TestUser\Documents\Projects\MyProject\Source\Units\MyUnit.pas';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.Width:= 100; // Force truncation
  try
    Lbl.CaptionMin:= TestPath;

    Assert.AreEqual(TestPath, Lbl.CaptionMin, 'CaptionMin should always return the full path');
  finally
    FreeAndNil(Lbl);
  end;
end;


{ ShowFullTextAsHint Tests }

procedure TTestMinimalPathLabel.TestShowFullTextAsHint_True_SetsHint;
var
  Lbl: TMinimalPathLabel;
  TestPath: string;
begin
  TestPath:= 'C:\Test\Path\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    Lbl.ShowFullTextAsHint:= TRUE;
    Lbl.CaptionMin:= TestPath;

    Assert.AreEqual(TestPath, Lbl.Hint, 'Hint should contain the full path when ShowFullTextAsHint is TRUE');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestShowFullTextAsHint_False_NoHint;
var
  Lbl: TMinimalPathLabel;
  TestPath: string;
begin
  TestPath:= 'C:\Test\Path\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    Lbl.ShowFullTextAsHint:= FALSE;
    Lbl.Hint:= ''; // Clear any existing hint
    Lbl.CaptionMin:= TestPath;

    Assert.AreEqual('', Lbl.Hint, 'Hint should not be set when ShowFullTextAsHint is FALSE');
  finally
    FreeAndNil(Lbl);
  end;
end;


{ Resize Tests }

procedure TTestMinimalPathLabel.TestResize_UpdatesCaption;
var
  Lbl: TMinimalPathLabel;
  LongPath: string;
  CaptionBefore: string;
begin
  LongPath:= 'C:\Very\Long\Path\That\Goes\On\And\On\Forever\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.AutoSize:= FALSE;   { Otherwise TLabel changes the width itself after every caption change }
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= LongPath;
    CaptionBefore:= Lbl.Caption;
    Assert.AreEqual(LongPath, CaptionBefore, 'Precondition: the path fits into 400 pixels');

    Lbl.Width:= 100; // Resize to much narrower

    Assert.IsTrue(Length(Lbl.Caption) < Length(LongPath), 'Resize must shorten the caption: ' + Lbl.Caption);
    Assert.IsTrue(Pos('...', Lbl.Caption) > 0, 'The shortened caption must hold the ellipsis: ' + Lbl.Caption);
    Assert.AreEqual(LongPath, Lbl.CaptionMin, 'The full path must be kept');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestResize_NarrowWidth_TruncatesPath;
var
  Lbl: TMinimalPathLabel;
  LongPath: string;
begin
  LongPath:= 'C:\Users\SomeUser\AppData\Local\Programs\MyApplication\Data\Configuration\Settings.xml';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    { Start wide: the whole path fits }
    SetWidthToText(Lbl, LongPath);
    Lbl.CaptionMin:= LongPath;
    Assert.AreEqual(LongPath, Lbl.Caption, 'Precondition: the whole path fits before the resize');

    { Then narrow: only Resize can shorten the caption now }
    SetWidthToText(Lbl, 'C:\...\Configuration\Settings.xml');

    Assert.AreEqual('C:\...\Configuration\Settings.xml', Lbl.Caption, 'Resize to a narrow width must cut the path in the middle, keeping the file name');
    Assert.AreEqual(LongPath, Lbl.CaptionMin, 'The full path must be kept');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestResize_WideWidth_ShowsFullPath;
var
  Lbl: TMinimalPathLabel;
  ShortPath: string;
begin
  ShortPath:= 'C:\Test.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    { Start narrow: only the file name fits. MinimizeName drops the folder, then the drive (Vcl.FileCtrl.pas:621-632). }
    SetWidthToText(Lbl, 'Test.txt');
    Lbl.CaptionMin:= ShortPath;
    Assert.AreEqual('Test.txt', Lbl.Caption, 'Precondition: the narrow label shows only the file name');

    { Then wide: only Resize can bring the full path back }
    Lbl.Width:= 500;

    Assert.AreEqual(ShortPath, Lbl.Caption, 'Full path should be shown when width is sufficient');
  finally
    FreeAndNil(Lbl);
  end;
end;


{ Path Format Tests }

procedure TTestMinimalPathLabel.TestCaptionMin_WindowsPath;
var
  Lbl: TMinimalPathLabel;
  WinPath: string;
begin
  WinPath:= 'D:\Projects\Delphi\MyApp\Source\MainForm.pas';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    SetWidthToText(Lbl, 'D:\...\Source\MainForm.pas');  { Narrow, so the shown text and the stored text differ }
    Lbl.CaptionMin:= WinPath;

    Assert.AreEqual('D:\...\Source\MainForm.pas', Lbl.Caption, 'The Windows path must be cut in the middle, keeping the drive and the file name');
    Assert.AreEqual(WinPath, Lbl.CaptionMin, 'Windows path should be stored correctly');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_UNCPath;
var
  Lbl: TMinimalPathLabel;
  UNCPath: string;
begin
  UNCPath:= '\\ServerName\SharedFolder\SubFolder\Document.docx';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    { A UNC path has no drive, so MinimizeName keeps one leading backslash before the ellipsis (CutFirstDirectory, Vcl.FileCtrl.pas:572-601) }
    SetWidthToText(Lbl, '\...\SubFolder\Document.docx');
    Lbl.CaptionMin:= UNCPath;

    Assert.AreEqual('\...\SubFolder\Document.docx', Lbl.Caption, 'The UNC path must be cut in the middle, keeping the file name');
    Assert.AreEqual(UNCPath, Lbl.CaptionMin, 'UNC path should be stored correctly');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_RelativePath;
var
  Lbl: TMinimalPathLabel;
  RelPath: string;
begin
  RelPath:= '..\Data\Config\Settings.ini';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    { The first cut drops '..\Data\': CutFirstDirectory deletes 4 characters from a folder that starts with a dot, then the rest up to the next backslash (Vcl.FileCtrl.pas:588-595) }
    SetWidthToText(Lbl, '...\Config\Settings.ini');
    Lbl.CaptionMin:= RelPath;

    Assert.AreEqual('...\Config\Settings.ini', Lbl.Caption, 'The relative path must be cut at the front, keeping the file name');
    Assert.AreEqual(RelPath, Lbl.CaptionMin, 'Relative path should be stored correctly');
  finally
    FreeAndNil(Lbl);
  end;
end;


{ Edge Cases }

procedure TTestMinimalPathLabel.TestCaptionMin_VeryLongPath;
var
  Lbl: TMinimalPathLabel;
  VeryLongPath: string;
begin
  VeryLongPath:= 'C:\' + StringOfChar('A', 50) + '\' + StringOfChar('B', 50) + '\' +
                 StringOfChar('C', 50) + '\' + StringOfChar('D', 50) + '\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    SetWidthToText(Lbl, 'C:\...\' + StringOfChar('D', 50) + '\File.txt');
    Lbl.CaptionMin:= VeryLongPath;

    Assert.AreEqual(VeryLongPath, Lbl.CaptionMin, 'Very long path should be stored correctly');
    Assert.AreEqual('C:\...\' + StringOfChar('D', 50) + '\File.txt', Lbl.Caption, 'Very long path must be cut in the middle, keeping the last folder and the file name');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_PathWithSpaces;
var
  Lbl: TMinimalPathLabel;
  SpacePath: string;
begin
  SpacePath:= 'C:\Program Files\My Application\User Data\Config File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  try
    SetWidthToText(Lbl, 'C:\...\User Data\Config File.txt');
    Lbl.CaptionMin:= SpacePath;

    Assert.AreEqual('C:\...\User Data\Config File.txt', Lbl.Caption, 'Spaces must not split a folder name: the cut keeps "User Data" and "Config File.txt" whole');
    Assert.AreEqual(SpacePath, Lbl.CaptionMin, 'Path with spaces should be stored correctly');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_PathWithUnicode;
var
  Lbl: TMinimalPathLabel;
  UnicodePath: string;
begin
  { Latin (U-umlaut), Cyrillic and Chinese characters, written as code points so the file encoding cannot change them }
  UnicodePath:= 'C:\Users\' + #$00DC + 'ser\' + #$0414#$043E#$043A + '\' + #$6587#$4EF6 + '.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.AutoSize:= FALSE;
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= UnicodePath;

    Assert.AreEqual(UnicodePath, Lbl.CaptionMin, 'Unicode path should be stored correctly');
    Assert.AreEqual(UnicodePath, Lbl.Caption, 'A short Unicode path must be shown unchanged');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_JustFilename;
var
  Lbl: TMinimalPathLabel;
  Filename: string;
begin
  Filename:= 'Document.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= Filename;

    Assert.AreEqual(Filename, Lbl.Caption, 'Just filename should be displayed as-is');
    Assert.AreEqual(Filename, Lbl.CaptionMin, 'Just filename should be stored correctly');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_ZeroWidth_NoException;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.AutoSize:= FALSE;   { Otherwise TLabel widens itself to the text and the zero width never reaches UpdateMinimizedCaption }
  Lbl.Width:= 0; // Zero width edge case
  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        Lbl.CaptionMin:= 'C:\Some\Path\File.txt';
      end);

    { MinimizeName with MaxLen 0 raises nothing and cuts the path down to 'File.txt' (Vcl.FileCtrl.pas:603-634).
      Only the Width > 0 guard keeps the full text. }
    Assert.AreEqual(0, Lbl.Width, 'Precondition: the label is still 0 pixels wide');
    Assert.AreEqual('C:\Some\Path\File.txt', Lbl.Caption, 'At zero width the label must fall back to the full path');
  finally
    FreeAndNil(Lbl);
  end;
end;


procedure TTestMinimalPathLabel.TestCaptionMin_BeforeParented;
var
  Lbl: TMinimalPathLabel;
  TestPath: string;
begin
  TestPath:= 'C:\Test\Path\File.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  // Note: NOT setting Parent here
  try
    Assert.WillNotRaiseAny(
      procedure
      begin
        Lbl.CaptionMin:= TestPath;
      end,
      'Setting CaptionMin before parenting should not raise exception');

    Assert.AreEqual(TestPath, Lbl.CaptionMin, 'Path should be stored even before parenting');
  finally
    FreeAndNil(Lbl);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestMinimalPathLabel);

end.
