unit Test.LightCore.TextFile;

{=============================================================================================================
   Unit tests for LightCore.TextFile
   Tests file read/write, UTF8/ANSI handling, BOM operations
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes;

type
  [TestFixture]
  TTestLightCoreTextFile = class
  private
    FTestFolder: string;
    FTestFile: string;
    procedure CleanupTestFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { String Write/Read Tests }
    [Test]
    procedure TestStringToFile_AndReadBack;

    [Test]
    procedure TestStringToFile_Overwrite;

    [Test]
    procedure TestStringToFile_Append;

    [Test]
    procedure TestStringFromFile_Empty;

    [Test]
    procedure TestStringFromFileExists_FileExists;

    [Test]
    procedure TestStringFromFileExists_FileNotExists;

    [Test]
    procedure TestStringFromFileTSL;

    { ANSI Tests }
    [Test]
    procedure TestStringToFileA_AndReadBack;

    [Test]
    procedure TestStringFromFileStart;

    { Line Operations Tests }
    [Test]
    procedure TestFirstLineFromFile;

    [Test]
    procedure TestCountLines_SingleLine;

    [Test]
    procedure TestCountLines_MultipleLines;

    [Test]
    procedure TestCountLines_EmptyFile;

    { UTF8/BOM Tests }
    [Test]
    procedure TestFileHasBOM_WithBOM;

    [Test]
    procedure TestFileHasBOM_WithoutBOM;

    [Test]
    procedure TestContainsUnicodeChars_True;

    [Test]
    procedure TestContainsUnicodeChars_False;

    [Test]
    procedure TestForceAddBOM;

    [Test]
    procedure TestForceRemoveBOM;

    { Encoding Detection Tests }
    [Test]
    procedure TestDetectFileEncoding_UTF8;

    [Test]
    procedure TestDetectFileEncoding_ANSI;

    { Random File Generation Tests }
    [Test]
    procedure TestGenerateRandomTextFile;
  end;

implementation

uses
  LightCore.TextFile;

procedure TTestLightCoreTextFile.Setup;
begin
  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'LightTextFileTest_' + IntToStr(Random(100000)));
  ForceDirectories(FTestFolder);
  FTestFile:= TPath.Combine(FTestFolder, 'TestFile.txt');
end;

procedure TTestLightCoreTextFile.TearDown;
begin
  CleanupTestFiles;
end;

procedure TTestLightCoreTextFile.CleanupTestFiles;
begin
  if DirectoryExists(FTestFolder)
  then TDirectory.Delete(FTestFolder, True);
end;

{ String Write/Read Tests }

procedure TTestLightCoreTextFile.TestStringToFile_AndReadBack;
var
  Content, ReadContent: string;
begin
  Content:= 'Hello, World!';
  StringToFile(FTestFile, Content, woOverwrite);
  ReadContent:= StringFromFile(FTestFile);
  Assert.AreEqual(Content, ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringToFile_Overwrite;
var
  Content1, Content2, ReadContent: string;
begin
  Content1:= 'First content';
  Content2:= 'Second content';

  StringToFile(FTestFile, Content1, woOverwrite);
  StringToFile(FTestFile, Content2, woOverwrite);
  ReadContent:= StringFromFile(FTestFile);

  Assert.AreEqual(Content2, ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringToFile_Append;
var
  Content1, Content2, ReadContent: string;
begin
  Content1:= 'First ';
  Content2:= 'Second';

  StringToFile(FTestFile, Content1, woOverwrite, wpOff);
  StringToFile(FTestFile, Content2, woAppend, wpOff);
  ReadContent:= StringFromFile(FTestFile);

  Assert.AreEqual('First Second', ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringFromFile_Empty;
var
  ReadContent: string;
begin
  StringToFile(FTestFile, '', woOverwrite, wpOff);
  ReadContent:= StringFromFile(FTestFile);
  Assert.AreEqual('', ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringFromFileExists_FileExists;
var
  Content, ReadContent: string;
begin
  Content:= 'Test content';
  StringToFile(FTestFile, Content, woOverwrite, wpOff);
  ReadContent:= StringFromFileExists(FTestFile);
  Assert.AreEqual(Content, ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringFromFileExists_FileNotExists;
var
  ReadContent: string;
begin
  ReadContent:= StringFromFileExists(TPath.Combine(FTestFolder, 'NonExistent.txt'));
  Assert.AreEqual('', ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringFromFileTSL;
var
  TSL: TStringList;
begin
  StringToFile(FTestFile, 'Line1'#13#10'Line2'#13#10'Line3', woOverwrite, wpOff);
  TSL:= StringFromFileTSL(FTestFile);
  try
    Assert.AreEqual(3, TSL.Count);
    Assert.AreEqual('Line1', TSL[0]);
    Assert.AreEqual('Line2', TSL[1]);
    Assert.AreEqual('Line3', TSL[2]);
  finally
    TSL.Free;
  end;
end;

{ ANSI Tests }

procedure TTestLightCoreTextFile.TestStringToFileA_AndReadBack;
var
  Content: AnsiString;
  ReadContent: AnsiString;
begin
  Content:= 'ANSI Content';
  StringToFileA(FTestFile, Content, woOverwrite);
  ReadContent:= StringFromFileA(FTestFile);
  Assert.AreEqual(Content, ReadContent);
end;

procedure TTestLightCoreTextFile.TestStringFromFileStart;
var
  Content: AnsiString;
  ReadContent: AnsiString;
begin
  Content:= 'Hello World, this is a longer text';
  StringToFileA(FTestFile, Content, woOverwrite);
  ReadContent:= StringFromFileStart(FTestFile, 5);
  Assert.AreEqual(AnsiString('Hello'), ReadContent);
end;

{ Line Operations Tests }

procedure TTestLightCoreTextFile.TestFirstLineFromFile;
var
  FirstLine: string;
begin
  StringToFile(FTestFile, 'First Line'#13#10'Second Line', woOverwrite, wpOff);
  FirstLine:= FirstLineFromFile(FTestFile);
  Assert.AreEqual('First Line', FirstLine);
end;

procedure TTestLightCoreTextFile.TestCountLines_SingleLine;
var
  Count: Int64;
begin
  StringToFile(FTestFile, 'Single line without newline', woOverwrite, wpOff);
  Count:= CountLines(FTestFile);
  Assert.AreEqual(Int64(1), Count);
end;

procedure TTestLightCoreTextFile.TestCountLines_MultipleLines;
var
  Count: Int64;
begin
  StringToFile(FTestFile, 'Line1'#10'Line2'#10'Line3', woOverwrite, wpOff);
  Count:= CountLines(FTestFile);
  Assert.AreEqual(Int64(3), Count);
end;

procedure TTestLightCoreTextFile.TestCountLines_EmptyFile;
var
  Count: Int64;
begin
  StringToFile(FTestFile, '', woOverwrite, wpOff);
  Count:= CountLines(FTestFile);
  Assert.AreEqual(Int64(0), Count);
end;

{ UTF8/BOM Tests }

procedure TTestLightCoreTextFile.TestFileHasBOM_WithBOM;
var
  FS: TFileStream;
begin
  { Create file with BOM }
  FS:= TFileStream.Create(FTestFile, fmCreate);
  try
    FS.WriteBuffer([$EF, $BB, $BF], 3);  { UTF-8 BOM }
    FS.WriteBuffer(AnsiString('Test')[1], 4);
  finally
    FS.Free;
  end;

  Assert.IsTrue(FileHasBOM(FTestFile));
end;

procedure TTestLightCoreTextFile.TestFileHasBOM_WithoutBOM;
begin
  StringToFile(FTestFile, 'No BOM here', woOverwrite, wpOff);
  Assert.IsFalse(FileHasBOM(FTestFile));
end;

procedure TTestLightCoreTextFile.TestContainsUnicodeChars_True;
begin
  Assert.IsTrue(ContainsUnicodeChars('Hello é'));
  Assert.IsTrue(ContainsUnicodeChars('Привет'));
  Assert.IsTrue(ContainsUnicodeChars('日本語'));
end;

procedure TTestLightCoreTextFile.TestContainsUnicodeChars_False;
begin
  Assert.IsFalse(ContainsUnicodeChars('Hello'));
  Assert.IsFalse(ContainsUnicodeChars('ABC123'));
  Assert.IsFalse(ContainsUnicodeChars(''));
end;

procedure TTestLightCoreTextFile.TestForceAddBOM;
var
  Result: Boolean;
begin
  { Create file without BOM }
  StringToFile(FTestFile, 'Test content', woOverwrite, wpOff);
  Assert.IsFalse(FileHasBOM(FTestFile));

  { Add BOM }
  Result:= ForceAddBOM(FTestFile);
  Assert.IsTrue(Result, 'ForceAddBOM should return True when BOM was added');
  Assert.IsTrue(FileHasBOM(FTestFile), 'File should now have BOM');
end;

procedure TTestLightCoreTextFile.TestForceRemoveBOM;
var
  Result: Boolean;
begin
  { Create file with BOM }
  StringToFile(FTestFile, 'Test content', woOverwrite, wpOn);
  Assert.IsTrue(FileHasBOM(FTestFile));

  { Remove BOM }
  Result:= ForceRemoveBOM(FTestFile);
  Assert.IsTrue(Result, 'ForceRemoveBOM should return True when BOM was removed');
  Assert.IsFalse(FileHasBOM(FTestFile), 'File should no longer have BOM');
end;

{ Encoding Detection Tests }

procedure TTestLightCoreTextFile.TestDetectFileEncoding_UTF8;
var
  Enc: TEncoding;
begin
  StringToFile(FTestFile, 'UTF8 content with BOM', woOverwrite, wpOn);
  Enc:= DetectFileEncoding(FTestFile);
  Assert.AreEqual(TEncoding.UTF8, Enc);
end;

procedure TTestLightCoreTextFile.TestDetectFileEncoding_ANSI;
var
  Enc: TEncoding;
begin
  { Write pure ASCII content without BOM - should be detected as ANSI }
  StringToFileA(FTestFile, 'Pure ASCII content', woOverwrite);
  Enc:= DetectFileEncoding(FTestFile);
  { Note: Pure ASCII is valid UTF-8, so this might return UTF8 }
  Assert.IsTrue((Enc = TEncoding.ANSI) OR (Enc = TEncoding.UTF8));
end;

{ Random File Generation Tests }

procedure TTestLightCoreTextFile.TestGenerateRandomTextFile;
var
  Lines: Int64;
begin
  GenerateRandomTextFile(FTestFile, 100);
  Assert.IsTrue(FileExists(FTestFile));
  Lines:= CountLines(FTestFile);
  Assert.AreEqual(Int64(100), Lines);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreTextFile);

end.
