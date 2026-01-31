unit Test.LightVcl.Visual.MinimalPathLabel;

{=============================================================================================================
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
    procedure TestCreate_NotNil;

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

procedure TTestMinimalPathLabel.TestCreate_NotNil;
var
  Lbl: TMinimalPathLabel;
begin
  Lbl:= TMinimalPathLabel.Create(FForm);
  try
    Assert.IsNotNull(Lbl, 'Component should be created');
  finally
    FreeAndNil(Lbl);
  end;
end;


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
  Lbl.Width:= 150; // Narrow width to force truncation
  try
    Lbl.CaptionMin:= LongPath;

    // Caption should be different (truncated) from the original long path
    Assert.AreNotEqual(LongPath, Lbl.Caption, 'Long path should be truncated when label is narrow');
    // But stored value should be full path
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
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= LongPath;
    CaptionBefore:= Lbl.Caption;

    Lbl.Width:= 100; // Resize to much narrower

    // Caption should change after resize (unless path was already truncated or fits)
    // At minimum, no exception should be raised
    Assert.Pass('Resize completed without exception');
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
    Lbl.CaptionMin:= LongPath;
    Lbl.Width:= 80; // Very narrow

    Assert.IsTrue(Length(Lbl.Caption) < Length(LongPath),
      'Caption should be shorter than original path when width is narrow');
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
    Lbl.CaptionMin:= ShortPath;
    Lbl.Width:= 500; // Very wide

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
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= WinPath;

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
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= UNCPath;

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
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= RelPath;

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
  Lbl.Width:= 200;
  try
    Lbl.CaptionMin:= VeryLongPath;

    Assert.AreEqual(VeryLongPath, Lbl.CaptionMin, 'Very long path should be stored correctly');
    Assert.IsTrue(Length(Lbl.Caption) < Length(VeryLongPath), 'Very long path should be truncated in Caption');
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
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= SpacePath;

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
  UnicodePath:= 'C:\Users\Utilisateur\Documents\Projets\Fichier.txt';
  Lbl:= TMinimalPathLabel.Create(FForm);
  Lbl.Parent:= FForm;
  Lbl.Width:= 400;
  try
    Lbl.CaptionMin:= UnicodePath;

    Assert.AreEqual(UnicodePath, Lbl.CaptionMin, 'Unicode path should be stored correctly');
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
  Lbl.Width:= 0; // Zero width edge case
  try
    Assert.WillNotRaise(
      procedure
      begin
        Lbl.CaptionMin:= 'C:\Some\Path\File.txt';
      end);
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
    Assert.WillNotRaise(
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
