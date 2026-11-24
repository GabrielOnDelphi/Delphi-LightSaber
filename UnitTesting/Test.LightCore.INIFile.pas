unit Test.LightCore.INIFile;

{=============================================================================================================
   Unit tests for LightCore.INIFile.pas
   Tests TIniFileEx extended INI file functionality
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.IniFiles,
  System.UITypes,
  Vcl.Graphics,
  LightCore.INIFile;

type
  [TestFixture]
  TTestIniFileEx = class
  private
    FRunFolder: string;
    FTestFile: string;
    FIniFile: TIniFileEx;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { String Tests }
    [Test]
    procedure TestWriteReadString;

    [Test]
    procedure TestWriteReadString_Empty;

    [Test]
    procedure TestWriteReadString_Unicode;

    { Integer Tests }
    [Test]
    procedure TestWriteReadInteger;

    [Test]
    procedure TestWriteReadInteger_Negative;

    [Test]
    procedure TestWriteReadInteger_Default;

    { Boolean Tests }
    [Test]
    procedure TestWriteReadBoolean_True;

    [Test]
    procedure TestWriteReadBoolean_False;

    [Test]
    procedure TestWriteReadBoolean_Default;

    { Float Tests }
    [Test]
    procedure TestWriteReadDouble;

    [Test]
    procedure TestWriteReadDouble_Negative;

    { Date Tests }
    [Test]
    procedure TestWriteReadDate;

    { ValueExists Tests }
    [Test]
    procedure TestValueExists_True;

    [Test]
    procedure TestValueExists_False;

    { Font Tests }
    [Test]
    procedure TestWriteReadFont;

    [Test]
    procedure TestWriteReadFont_WithMultipleStyles;

    [Test]
    procedure TestReadFont_NotFound_ReturnsDefaults;
  end;

  [TestFixture]
  TTestIniFileDt = class
  private
    FRunFolder: string;
    FTestFile: string;
    FIniFile: TIniFileDt;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestWriteReadDate;

    [Test]
    procedure TestWriteReadDate_DifferentSections;

    [Test]
    procedure TestReadDate_Default;
  end;

  { TIniFileEx.WriteString skips a value that the file already holds.
    Proof that a write was skipped: the modified time of the file, set to 2020 before the write, stays in 2020. }
  [TestFixture]
  TTestIniFileExSkipUnchanged = class
  private
    FRunFolder: string;
    FTestFile: string;
    FIniFile: TIniFileEx;
    procedure makeFileOld;
    function  fileIsOld: Boolean;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure UnchangedValue_FileNotRewritten;

    [Test]
    procedure ChangedValue_IsWritten;

    [Test]
    procedure UnchangedLongValue_FileNotRewritten;

    [Test]
    procedure WriteStringOverload_UnchangedValue_FileNotRewritten;

    [Test]
    procedure WriteIntegerOverload_UnchangedValue_FileNotRewritten;

    [Test]
    procedure MissingKey_EmptyValue_IsWritten;

    [Test]
    procedure ReadOnlyFile_UnchangedValue_RaisesNothing;

    [Test]
    procedure ReadOnlyFile_ChangedValue_Raises;

    [Test]
    procedure ReadStringOverload_LongValue_NotCut;
  end;

implementation

USES
  Test.RunFolder;

CONST
  RunPrefixEx   = 'TestLcIniFileEx';
  RunPrefixDt   = 'TestLcIniFileDt';
  RunPrefixSkip = 'TestLcIniFileSkip';


procedure TTestIniFileEx.Setup;
begin
  FRunFolder:= NewRunFolder(RunPrefixEx);
  FTestFile:= FRunFolder + 'TestIniFile.ini';
  FIniFile:= TIniFileEx.Create('TestSection', FTestFile);
end;


procedure TTestIniFileEx.TearDown;
begin
  FreeAndNil(FIniFile);
  DeleteRunFolder(FRunFolder, RunPrefixEx);
  FRunFolder:= '';
end;


{ String Tests }

procedure TTestIniFileEx.TestWriteReadString;
begin
  FIniFile.Write('Name', 'TestValue');
  Assert.AreEqual('TestValue', FIniFile.Read('Name', ''));
end;

procedure TTestIniFileEx.TestWriteReadString_Empty;
begin
  FIniFile.Write('Empty', '');
  Assert.AreEqual('', FIniFile.Read('Empty', 'Default'));
end;

procedure TTestIniFileEx.TestWriteReadString_Unicode;
begin
  FIniFile.Write('Unicode', 'Test éàü');
  Assert.AreEqual('Test éàü', FIniFile.Read('Unicode', ''));
end;


{ Integer Tests }

procedure TTestIniFileEx.TestWriteReadInteger;
begin
  FIniFile.Write('IntValue', 12345);
  Assert.AreEqual(12345, FIniFile.Read('IntValue', 0));
end;

procedure TTestIniFileEx.TestWriteReadInteger_Negative;
begin
  FIniFile.Write('NegValue', -999);
  Assert.AreEqual(-999, FIniFile.Read('NegValue', 0));
end;

procedure TTestIniFileEx.TestWriteReadInteger_Default;
begin
  Assert.AreEqual(42, FIniFile.Read('NonExistent', 42));
end;


{ Boolean Tests }

procedure TTestIniFileEx.TestWriteReadBoolean_True;
begin
  FIniFile.Write('BoolTrue', True);
  Assert.IsTrue(FIniFile.Read('BoolTrue', False));
end;

procedure TTestIniFileEx.TestWriteReadBoolean_False;
begin
  FIniFile.Write('BoolFalse', False);
  Assert.IsFalse(FIniFile.Read('BoolFalse', True));
end;

procedure TTestIniFileEx.TestWriteReadBoolean_Default;
begin
  Assert.IsTrue(FIniFile.Read('NonExistent', True));
  Assert.IsFalse(FIniFile.Read('NonExistent2', False));
end;


{ Float Tests }

procedure TTestIniFileEx.TestWriteReadDouble;
begin
  FIniFile.Write('FloatValue', 3.14159);
  Assert.AreEqual(3.14159, FIniFile.Read('FloatValue', 0.0), 0.00001);
end;

procedure TTestIniFileEx.TestWriteReadDouble_Negative;
begin
  FIniFile.Write('NegFloat', -123.456);
  Assert.AreEqual(-123.456, FIniFile.Read('NegFloat', 0.0), 0.001);
end;


{ Date Tests }

procedure TTestIniFileEx.TestWriteReadDate;
var
  TestDate, ReadDate: TDateTime;
begin
  { TIniFileEx inherits from TIniFile, which may lose time precision }
  { Test with date only to avoid precision issues }
  TestDate:= EncodeDate(2025, 6, 15);
  FIniFile.WriteDate('DateValue', TestDate);
  ReadDate:= FIniFile.ReadDate('DateValue', 0);
  Assert.AreEqual(Trunc(TestDate), Trunc(ReadDate), 'Date portion should match');
end;


{ ValueExists Tests }

procedure TTestIniFileEx.TestValueExists_True;
begin
  FIniFile.Write('ExistingKey', 'Value');
  Assert.IsTrue(FIniFile.ValueExists('ExistingKey'));
end;

procedure TTestIniFileEx.TestValueExists_False;
begin
  Assert.IsFalse(FIniFile.ValueExists('NonExistentKey'));
end;


{ Font Tests }

procedure TTestIniFileEx.TestWriteReadFont;
var
  WriteFont, ReadFont: FontStruct;
begin
  WriteFont.Name:= 'Arial';
  WriteFont.Size:= 12;
  WriteFont.Style:= [TFontStyle.fsBold];
  WriteFont.Color:= clRed;

  FIniFile.WriteFont('TestFont', WriteFont);
  ReadFont:= FIniFile.ReadFont('TestFont');

  Assert.AreEqual('Arial', ReadFont.Name);
  Assert.AreEqual(12, ReadFont.Size);
  Assert.IsTrue(TFontStyle.fsBold in ReadFont.Style, 'Should have fsBold');
  Assert.AreEqual(Integer(clRed), Integer(ReadFont.Color));
end;

procedure TTestIniFileEx.TestWriteReadFont_WithMultipleStyles;
var
  WriteFont, ReadFont: FontStruct;
begin
  WriteFont.Name:= 'Tahoma';
  WriteFont.Size:= 10;
  WriteFont.Style:= [TFontStyle.fsBold, TFontStyle.fsItalic, TFontStyle.fsUnderline];
  WriteFont.Color:= clBlue;

  FIniFile.WriteFont('StyledFont', WriteFont);
  ReadFont:= FIniFile.ReadFont('StyledFont');

  Assert.AreEqual('Tahoma', ReadFont.Name);
  Assert.AreEqual(10, ReadFont.Size);
  Assert.IsTrue(TFontStyle.fsBold in ReadFont.Style, 'Should have fsBold');
  Assert.IsTrue(TFontStyle.fsItalic in ReadFont.Style, 'Should have fsItalic');
  Assert.IsTrue(TFontStyle.fsUnderline in ReadFont.Style, 'Should have fsUnderline');
  Assert.IsFalse(TFontStyle.fsStrikeOut in ReadFont.Style, 'Should not have fsStrikeOut');
end;

procedure TTestIniFileEx.TestReadFont_NotFound_ReturnsDefaults;
var
  ReadFont: FontStruct;
begin
  ReadFont:= FIniFile.ReadFont('NonExistentFont');

  Assert.AreEqual('Arial', ReadFont.Name);
  Assert.AreEqual(8, ReadFont.Size);
  Assert.AreEqual(Integer(0), Integer(ReadFont.Color), 'Default color should be black (0)');
  Assert.AreEqual(Byte(0), Byte(ReadFont.Style), 'Default style should be empty');
end;


{ TTestIniFileDt }

procedure TTestIniFileDt.Setup;
begin
  FRunFolder:= NewRunFolder(RunPrefixDt);
  FTestFile:= FRunFolder + 'TestIniFileDt.ini';
  FIniFile:= TIniFileDt.Create(FTestFile);
end;

procedure TTestIniFileDt.TearDown;
begin
  FreeAndNil(FIniFile);
  DeleteRunFolder(FRunFolder, RunPrefixDt);
  FRunFolder:= '';
end;

procedure TTestIniFileDt.TestWriteReadDate;
var
  TestDate: TDateTime;
begin
  TestDate:= EncodeDate(2024, 12, 25) + EncodeTime(10, 30, 45, 0);
  FIniFile.WriteDate('Section1', 'DateKey', TestDate);
  Assert.AreEqual(TestDate, FIniFile.ReadDate('Section1', 'DateKey', 0), 0.0001);
end;

procedure TTestIniFileDt.TestWriteReadDate_DifferentSections;
var
  Date1, Date2: TDateTime;
begin
  Date1:= EncodeDate(2020, 1, 1);
  Date2:= EncodeDate(2025, 12, 31);

  FIniFile.WriteDate('SectionA', 'MyDate', Date1);
  FIniFile.WriteDate('SectionB', 'MyDate', Date2);

  { Verify each section has its own value - this tests the bug fix where Section param was ignored }
  Assert.AreEqual(Date1, FIniFile.ReadDate('SectionA', 'MyDate', 0), 0.0001);
  Assert.AreEqual(Date2, FIniFile.ReadDate('SectionB', 'MyDate', 0), 0.0001);
end;

procedure TTestIniFileDt.TestReadDate_Default;
var
  DefaultDate: TDateTime;
begin
  DefaultDate:= EncodeDate(1999, 6, 15);
  Assert.AreEqual(DefaultDate, FIniFile.ReadDate('NoSection', 'NoKey', DefaultDate), 0.0001);
end;




{ TTestIniFileExSkipUnchanged }

procedure TTestIniFileExSkipUnchanged.Setup;
begin
  FRunFolder:= NewRunFolder(RunPrefixSkip);
  FTestFile:= FRunFolder + 'TestIniFileSkip.ini';
  FIniFile:= TIniFileEx.Create('TestSection', FTestFile);
end;

procedure TTestIniFileExSkipUnchanged.TearDown;
begin
  FreeAndNil(FIniFile);
  if FileExists(FTestFile)
  then FileSetReadOnly(FTestFile, FALSE);   { A failed read-only test may leave the attribute set }
  DeleteRunFolder(FRunFolder, RunPrefixSkip);
  FRunFolder:= '';
end;

procedure TTestIniFileExSkipUnchanged.makeFileOld;
begin
  TFile.SetLastWriteTime(FTestFile, EncodeDate(2020, 1, 1));
end;

function TTestIniFileExSkipUnchanged.fileIsOld: Boolean;
begin
  Result:= TFile.GetLastWriteTime(FTestFile) < EncodeDate(2021, 1, 1);
end;


procedure TTestIniFileExSkipUnchanged.UnchangedValue_FileNotRewritten;
begin
  FIniFile.WriteString('TestSection', 'Key', 'abc');
  makeFileOld;
  FIniFile.WriteString('TestSection', 'Key', 'abc');
  FIniFile.UpdateFile;   { The flush that TIniFile.Destroy does must not rewrite the file either }
  Assert.IsTrue(fileIsOld, 'Writing the value that the file already holds must not rewrite the file');
  Assert.AreEqual('abc', FIniFile.ReadString('TestSection', 'Key', ''));
end;

procedure TTestIniFileExSkipUnchanged.ChangedValue_IsWritten;
begin
  FIniFile.WriteString('TestSection', 'Key', 'abc');
  makeFileOld;
  FIniFile.WriteString('TestSection', 'Key', 'xyz');
  Assert.IsFalse(fileIsOld, 'A changed value must rewrite the file');
  Assert.IsTrue(Pos('Key=xyz', TFile.ReadAllText(FTestFile)) > 0, 'The file must hold the new value');
end;

procedure TTestIniFileExSkipUnchanged.UnchangedLongValue_FileNotRewritten;
begin
  VAR LongValue:= StringOfChar('x', 5000);   { Longer than the 2047 characters that the RTL's TIniFile.ReadString returns }
  FIniFile.WriteString('TestSection', 'Long', LongValue);
  makeFileOld;
  FIniFile.WriteString('TestSection', 'Long', LongValue);
  Assert.IsTrue(fileIsOld, 'An unchanged value longer than 2047 characters must not rewrite the file');
end;

procedure TTestIniFileExSkipUnchanged.WriteStringOverload_UnchangedValue_FileNotRewritten;
begin
  FIniFile.Write('Key', 'abc');
  makeFileOld;
  FIniFile.Write('Key', 'abc');
  Assert.IsTrue(fileIsOld, 'Write(Ident, string) must skip an unchanged value too');
end;

procedure TTestIniFileExSkipUnchanged.WriteIntegerOverload_UnchangedValue_FileNotRewritten;
begin
  FIniFile.Write('Num', 42);
  makeFileOld;
  FIniFile.Write('Num', 42);
  Assert.IsTrue(fileIsOld, 'Write(Ident, Integer) must skip an unchanged value too');
end;

procedure TTestIniFileExSkipUnchanged.MissingKey_EmptyValue_IsWritten;
begin
  FIniFile.WriteString('TestSection', 'Other', '1');
  makeFileOld;
  FIniFile.WriteString('TestSection', 'Empty', '');
  Assert.IsFalse(fileIsOld, 'An empty value for a missing key must create the key');
  Assert.IsTrue(FIniFile.ValueExists('Empty'), 'The key must exist after the write');
end;

procedure TTestIniFileExSkipUnchanged.ReadOnlyFile_UnchangedValue_RaisesNothing;
begin
  FIniFile.WriteString('TestSection', 'Key', 'abc');
  FileSetReadOnly(FTestFile, TRUE);
  try
    Assert.WillNotRaise(
      procedure
      begin
        FIniFile.WriteString('TestSection', 'Key', 'abc');
      end,
      NIL, 'An unchanged value needs no write, so a read-only file must not raise');
  finally
    FileSetReadOnly(FTestFile, FALSE);
  end;
end;

procedure TTestIniFileExSkipUnchanged.ReadOnlyFile_ChangedValue_Raises;
begin
  FIniFile.WriteString('TestSection', 'Key', 'abc');
  FileSetReadOnly(FTestFile, TRUE);
  try
    Assert.WillRaise(
      procedure
      begin
        FIniFile.WriteString('TestSection', 'Key', 'xyz');
      end,
      EIniFileException, 'A changed value written to a read-only file must raise');
  finally
    FileSetReadOnly(FTestFile, FALSE);
  end;
end;

procedure TTestIniFileExSkipUnchanged.ReadStringOverload_LongValue_NotCut;
begin
  FIniFile.WriteString('TestSection', 'Long', StringOfChar('x', 5000));
  Assert.AreEqual(5000, Length(FIniFile.Read('Long', '')), 'Read(Ident, string) must return values longer than 2047 characters whole');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestIniFileEx);
  TDUnitX.RegisterTestFixture(TTestIniFileDt);
  TDUnitX.RegisterTestFixture(TTestIniFileExSkipUnchanged);

end.
