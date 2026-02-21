UNIT Test.FastJpegDec;

{-------------------------------------------------------------------------------------------------------------
   2026.02.21
   GabrielMoraru.com

   DUnitX tests for FastJpegDec.pas and FastJpegDecHelper.pas
   Tests the Pascal wrappers (input validation, error handling).
   Note: The assembly decoder is Win32-only (x86 SSE/SSE2).
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, System.IOUtils,
  Vcl.Graphics,
  DUnitX.TestFramework,
  FastJpegDec, FastJpegDecHelper;

TYPE
  [TestFixture]
  TTestJpegDecode = class
  public
    [Test]
    procedure Test_NilBuffer_ReturnsNil;
    [Test]
    procedure Test_ZeroLength_ReturnsNil;
    [Test]
    procedure Test_NegativeLength_ReturnsNil;
    [Test]
    procedure Test_InvalidData_ReturnsNil;
    [Test]
    procedure Test_InvalidData_ErrorCode;
    [Test]
    procedure Test_TooSmallBuffer_ReturnsNil;
  end;

  [TestFixture]
  TTestFastJpgDecHelper = class
  private
    FTestDir: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_NonExistentFile_ReturnsNil;
    [Test]
    procedure Test_NonExistentFile_ErrorType;
    [Test]
    procedure Test_EmptyFileName;
    [Test]
    procedure Test_TooSmallFile;
    [Test]
    procedure Test_InvalidJpegFile;
    [Test]
    procedure Test_SimpleOverload_ReturnsNil;
  end;


IMPLEMENTATION


{ TTestJpegDecode }

procedure TTestJpegDecode.Test_NilBuffer_ReturnsNil;
VAR Bmp: TBitmap;
begin
  Bmp:= JpegDecode(NIL, 100);
  Assert.IsNull(Bmp, 'Nil buffer should return nil');
end;


procedure TTestJpegDecode.Test_ZeroLength_ReturnsNil;
VAR
  Data: array[0..15] of Byte;
  Bmp: TBitmap;
begin
  FillChar(Data, SizeOf(Data), 0);
  Bmp:= JpegDecode(@Data, 0);
  Assert.IsNull(Bmp, 'Zero length should return nil');
end;


procedure TTestJpegDecode.Test_NegativeLength_ReturnsNil;
VAR
  Data: array[0..15] of Byte;
  Bmp: TBitmap;
begin
  FillChar(Data, SizeOf(Data), 0);
  Bmp:= JpegDecode(@Data, -1);
  Assert.IsNull(Bmp, 'Negative length should return nil');
end;


procedure TTestJpegDecode.Test_InvalidData_ReturnsNil;
VAR
  Data: array[0..255] of Byte;
  Bmp: TBitmap;
begin
  FillChar(Data, SizeOf(Data), $AA);
  Bmp:= JpegDecode(@Data, SizeOf(Data));
  Assert.IsNull(Bmp, 'Invalid data should return nil');
end;


procedure TTestJpegDecode.Test_InvalidData_ErrorCode;
VAR
  Data: array[0..255] of Byte;
  pImg: PJpegDecode;
  Err: TJpegDecodeError;
begin
  FillChar(Data, SizeOf(Data), $AA);
  Err:= JpegDecode(@Data, SizeOf(Data), pImg);
  Assert.AreNotEqual(Ord(JPEG_SUCCESS), Ord(Err), 'Invalid data should not return JPEG_SUCCESS');
end;


procedure TTestJpegDecode.Test_TooSmallBuffer_ReturnsNil;
VAR
  Data: array[0..3] of Byte;
  Bmp: TBitmap;
begin
  { JPEG files are at least ~20 bytes minimum (SOI + EOI markers) }
  Data[0]:= $FF; Data[1]:= $D8; { SOI marker }
  Data[2]:= $FF; Data[3]:= $D9; { EOI marker }
  Bmp:= JpegDecode(@Data, SizeOf(Data));
  Assert.IsNull(Bmp, 'Minimal SOI+EOI should not produce a bitmap');
end;



{ TTestFastJpgDecHelper }

procedure TTestFastJpgDecHelper.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'FastJpegTest_' + TGUID.NewGuid.ToString);
  ForceDirectories(FTestDir);
end;


procedure TTestFastJpgDecHelper.TearDown;
begin
  if DirectoryExists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


procedure TTestFastJpgDecHelper.Test_NonExistentFile_ReturnsNil;
VAR Bmp: TBitmap;
begin
  Bmp:= FastJpgDecode('C:\NonExistent_' + TGUID.NewGuid.ToString + '.jpg');
  Assert.IsNull(Bmp, 'Non-existent file should return nil');
end;


procedure TTestFastJpgDecHelper.Test_NonExistentFile_ErrorType;
VAR
  Bmp: TBitmap;
  ErrorType: string;
begin
  Bmp:= FastJpgDecode('C:\NonExistent_' + TGUID.NewGuid.ToString + '.jpg', ErrorType);
  Assert.IsNull(Bmp, 'Non-existent file should return nil');
  { The file open error is caught by the EFOpenError handler, result is nil }
end;


procedure TTestFastJpgDecHelper.Test_EmptyFileName;
VAR
  Bmp: TBitmap;
  ErrorType: string;
begin
  Bmp:= FastJpgDecode('', ErrorType);
  Assert.IsNull(Bmp, 'Empty filename should return nil');
end;


procedure TTestFastJpgDecHelper.Test_TooSmallFile;
VAR
  Bmp: TBitmap;
  ErrorType: string;
  FilePath: string;
  Stream: TFileStream;
begin
  FilePath:= TPath.Combine(FTestDir, 'tiny.jpg');
  Stream:= TFileStream.Create(FilePath, fmCreate);
  TRY
    Stream.WriteData(Byte($FF));
    Stream.WriteData(Byte($D8));
  FINALLY
    FreeAndNil(Stream);
  END;

  Bmp:= FastJpgDecode(FilePath, ErrorType);
  Assert.IsNull(Bmp, 'File smaller than 10 bytes should return nil');
  Assert.AreEqual('File is too small!', ErrorType);
end;


procedure TTestFastJpgDecHelper.Test_InvalidJpegFile;
VAR
  Bmp: TBitmap;
  ErrorType: string;
  FilePath: string;
  Stream: TFileStream;
  i: Integer;
begin
  { Create a file with random non-JPEG content, > 10 bytes }
  FilePath:= TPath.Combine(FTestDir, 'invalid.jpg');
  Stream:= TFileStream.Create(FilePath, fmCreate);
  TRY
    for i:= 0 to 99 do
      Stream.WriteData(Byte(i));
  FINALLY
    FreeAndNil(Stream);
  END;

  Bmp:= FastJpgDecode(FilePath, ErrorType);
  TRY
    Assert.IsNull(Bmp, 'Invalid JPEG content should return nil');
    Assert.IsNotEmpty(ErrorType, 'ErrorType should be populated on failure');
  FINALLY
    FreeAndNil(Bmp); { In case it somehow produced a result }
  END;
end;


procedure TTestFastJpgDecHelper.Test_SimpleOverload_ReturnsNil;
VAR Bmp: TBitmap;
begin
  { Test the single-parameter overload }
  Bmp:= FastJpgDecode('C:\NonExistent_' + TGUID.NewGuid.ToString + '.jpg');
  Assert.IsNull(Bmp, 'Simple overload with non-existent file should return nil');
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestJpegDecode);
  TDUnitX.RegisterTestFixture(TTestFastJpgDecHelper);

end.
