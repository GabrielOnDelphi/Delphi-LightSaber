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
  System.UITypes,
  LightCore.INIFile;

type
  [TestFixture]
  TTestIniFileEx = class
  private
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

implementation


procedure TTestIniFileEx.Setup;
begin
  FTestFile:= TPath.Combine(TPath.GetTempPath, 'TestIniFile_' + TGUID.NewGuid.ToString + '.ini');
  FIniFile:= TIniFileEx.Create('TestSection', FTestFile);
end;


procedure TTestIniFileEx.TearDown;
begin
  FreeAndNil(FIniFile);
  if FileExists(FTestFile)
  then DeleteFile(FTestFile);
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
  TestDate:= EncodeDate(2025, 6, 15) + EncodeTime(14, 30, 0, 0);
  FIniFile.WriteDate('DateValue', TestDate);
  ReadDate:= FIniFile.ReadDate('DateValue', 0);
  Assert.AreEqual(TestDate, ReadDate, 0.0001);
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
  WriteFont.Style:= [fsBold];
  WriteFont.Color:= clRed;

  FIniFile.Write('TestFont', WriteFont);
  ReadFont:= FIniFile.Read('TestFont');

  Assert.AreEqual('Arial', ReadFont.Name);
  Assert.AreEqual(12, ReadFont.Size);
  Assert.IsTrue(fsBold in ReadFont.Style, 'Should have fsBold');
  Assert.AreEqual(Integer(clRed), Integer(ReadFont.Color));
end;

procedure TTestIniFileEx.TestWriteReadFont_WithMultipleStyles;
var
  WriteFont, ReadFont: FontStruct;
begin
  WriteFont.Name:= 'Tahoma';
  WriteFont.Size:= 10;
  WriteFont.Style:= [fsBold, fsItalic, fsUnderline];
  WriteFont.Color:= clBlue;

  FIniFile.Write('StyledFont', WriteFont);
  ReadFont:= FIniFile.Read('StyledFont');

  Assert.AreEqual('Tahoma', ReadFont.Name);
  Assert.AreEqual(10, ReadFont.Size);
  Assert.IsTrue(fsBold in ReadFont.Style, 'Should have fsBold');
  Assert.IsTrue(fsItalic in ReadFont.Style, 'Should have fsItalic');
  Assert.IsTrue(fsUnderline in ReadFont.Style, 'Should have fsUnderline');
  Assert.IsFalse(fsStrikeOut in ReadFont.Style, 'Should not have fsStrikeOut');
end;

procedure TTestIniFileEx.TestReadFont_NotFound_ReturnsDefaults;
var
  ReadFont: FontStruct;
begin
  ReadFont:= FIniFile.Read('NonExistentFont');

  Assert.AreEqual('Arial', ReadFont.Name);
  Assert.AreEqual(8, ReadFont.Size);
  Assert.AreEqual(Integer(0), Integer(ReadFont.Color), 'Default color should be black (0)');
  Assert.AreEqual(Byte(0), Byte(ReadFont.Style), 'Default style should be empty');
end;


{ TTestIniFileDt }

procedure TTestIniFileDt.Setup;
begin
  FTestFile:= TPath.Combine(TPath.GetTempPath, 'TestIniFileDt_' + TGUID.NewGuid.ToString + '.ini');
  FIniFile:= TIniFileDt.Create(FTestFile);
end;

procedure TTestIniFileDt.TearDown;
begin
  FreeAndNil(FIniFile);
  if FileExists(FTestFile)
  then DeleteFile(FTestFile);
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


initialization
  TDUnitX.RegisterTestFixture(TTestIniFileEx);
  TDUnitX.RegisterTestFixture(TTestIniFileDt);

end.
