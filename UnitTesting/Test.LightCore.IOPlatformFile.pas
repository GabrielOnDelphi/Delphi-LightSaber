unit Test.LightCore.IOPlatformFile;

{=============================================================================================================
   Unit tests for LightCore.IOPlatformFile.pas
   Tests line ending detection and conversion between Windows, Unix, and Mac formats.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  LightCore.IOPlatformFile;

type
  [TestFixture]
  TTestIOPlatformFile = class
  private
    FTestDir: string;
    procedure CleanupTestDir;
    function CreateTestStream(const Content: TBytes): TMemoryStream;
    function StreamToBytes(Stream: TStream): TBytes;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { GetEnterType Tests }
    [Test]
    procedure TestGetEnterType_Windows;

    [Test]
    procedure TestGetEnterType_Unix;

    [Test]
    procedure TestGetEnterType_Mac;

    [Test]
    procedure TestGetEnterType_Unknown;

    [Test]
    procedure TestGetEnterType_EmptyStream;

    [Test]
    procedure TestGetEnterType_CRAtEndOfFile;

    { GetEnterTypeS Tests }
    [Test]
    procedure TestGetEnterTypeS_Windows;

    [Test]
    procedure TestGetEnterTypeS_Unix;

    [Test]
    procedure TestGetEnterTypeS_FileNotFound;

    { IsMacFile Tests }
    [Test]
    procedure TestIsMacFile_True;

    [Test]
    procedure TestIsMacFile_False_Windows;

    [Test]
    procedure TestIsMacFile_False_Unix;

    { WinToUnix Tests }
    [Test]
    procedure TestWinToUnix_Basic;

    [Test]
    procedure TestWinToUnix_MultipleLines;

    [Test]
    procedure TestWinToUnix_NoLineEndings;

    [Test]
    procedure TestWinToUnix_WithNotify;

    { UnixToWin Tests }
    [Test]
    procedure TestUnixToWin_Basic;

    [Test]
    procedure TestUnixToWin_MultipleLines;

    [Test]
    procedure TestUnixToWin_NoLineEndings;

    { MacToWin Tests }
    [Test]
    procedure TestMacToWin_Basic;

    [Test]
    procedure TestMacToWin_MultipleLines;

    { Round-trip Tests }
    [Test]
    procedure TestRoundTrip_WinToUnixToWin;

    [Test]
    procedure TestRoundTrip_UnixToWinToUnix;

    { File-based Tests }
    [Test]
    procedure TestWinToUnix_File;

    [Test]
    procedure TestUnixToWin_File;

    [Test]
    procedure TestMacToWin_File;

    [Test]
    procedure TestMacToWin_File_NotMac;
  end;

implementation


procedure TTestIOPlatformFile.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'IOPlatformTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
end;


procedure TTestIOPlatformFile.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestIOPlatformFile.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


function TTestIOPlatformFile.CreateTestStream(const Content: TBytes): TMemoryStream;
begin
  Result:= TMemoryStream.Create;
  if Length(Content) > 0
  then Result.WriteBuffer(Content[0], Length(Content));
  Result.Position:= 0;
end;


function TTestIOPlatformFile.StreamToBytes(Stream: TStream): TBytes;
begin
  Stream.Position:= 0;
  SetLength(Result, Stream.Size);
  if Stream.Size > 0
  then Stream.ReadBuffer(Result[0], Stream.Size);
end;


{ GetEnterType Tests }

procedure TTestIOPlatformFile.TestGetEnterType_Windows;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  { Windows: CR LF ($0D $0A) }
  Stream:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42));  { A<CRLF>B }
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etWin, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestGetEnterType_Unix;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  { Unix: LF only ($0A) }
  Stream:= CreateTestStream(TBytes.Create($41, $0A, $42));  { A<LF>B }
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etNix, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestGetEnterType_Mac;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  { Mac: CR only ($0D) not followed by LF }
  Stream:= CreateTestStream(TBytes.Create($41, $0D, $42));  { A<CR>B }
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etMac, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestGetEnterType_Unknown;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  { No line endings }
  Stream:= CreateTestStream(TBytes.Create($41, $42, $43));  { ABC }
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etUnknown, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestGetEnterType_EmptyStream;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  Stream:= CreateTestStream(TBytes.Create());
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etUnknown, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestGetEnterType_CRAtEndOfFile;
VAR
  Stream: TMemoryStream;
  EnterT: TEnterType;
begin
  { CR at end of file (edge case) }
  Stream:= CreateTestStream(TBytes.Create($41, $42, $0D));  { AB<CR> }
  TRY
    EnterT:= GetEnterType(Stream);
    Assert.AreEqual(etMac, EnterT);
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ GetEnterTypeS Tests }

procedure TTestIOPlatformFile.TestGetEnterTypeS_Windows;
VAR
  FilePath, Result: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'win.txt');
  TFile.WriteAllBytes(FilePath, TBytes.Create($41, $0D, $0A, $42));

  Result:= GetEnterTypeS(FilePath);
  Assert.AreEqual('Win', Result);
end;


procedure TTestIOPlatformFile.TestGetEnterTypeS_Unix;
VAR
  FilePath, Result: string;
begin
  FilePath:= TPath.Combine(FTestDir, 'unix.txt');
  TFile.WriteAllBytes(FilePath, TBytes.Create($41, $0A, $42));

  Result:= GetEnterTypeS(FilePath);
  Assert.AreEqual('Nix', Result);
end;


procedure TTestIOPlatformFile.TestGetEnterTypeS_FileNotFound;
begin
  Assert.WillRaise(
    procedure
    begin
      GetEnterTypeS(TPath.Combine(FTestDir, 'nonexistent.txt'));
    end,
    Exception
  );
end;


{ IsMacFile Tests }

procedure TTestIOPlatformFile.TestIsMacFile_True;
VAR
  Stream: TMemoryStream;
begin
  Stream:= CreateTestStream(TBytes.Create($41, $0D, $42, $0D, $43));  { A<CR>B<CR>C }
  TRY
    Assert.IsTrue(IsMacFile(Stream));
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestIsMacFile_False_Windows;
VAR
  Stream: TMemoryStream;
begin
  Stream:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42));  { A<CRLF>B }
  TRY
    Assert.IsFalse(IsMacFile(Stream));
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TTestIOPlatformFile.TestIsMacFile_False_Unix;
VAR
  Stream: TMemoryStream;
begin
  Stream:= CreateTestStream(TBytes.Create($41, $0A, $42));  { A<LF>B }
  TRY
    Assert.IsFalse(IsMacFile(Stream));
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ WinToUnix Tests }

procedure TTestIOPlatformFile.TestWinToUnix_Basic;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  { Input: A<CRLF>B -> Output: A<LF>B }
  InStream:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42));
  OutStream:= TMemoryStream.Create;
  TRY
    WinToUnix(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(3, Length(ResultBytes));
    Assert.AreEqual(Byte($41), ResultBytes[0]);  { A }
    Assert.AreEqual(Byte($0A), ResultBytes[1]);  { LF }
    Assert.AreEqual(Byte($42), ResultBytes[2]);  { B }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestWinToUnix_MultipleLines;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  { Input: A<CRLF>B<CRLF>C -> Output: A<LF>B<LF>C }
  InStream:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42, $0D, $0A, $43));
  OutStream:= TMemoryStream.Create;
  TRY
    WinToUnix(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(5, Length(ResultBytes));  { 3 chars + 2 LF }
    Assert.AreEqual(Byte($41), ResultBytes[0]);  { A }
    Assert.AreEqual(Byte($0A), ResultBytes[1]);  { LF }
    Assert.AreEqual(Byte($42), ResultBytes[2]);  { B }
    Assert.AreEqual(Byte($0A), ResultBytes[3]);  { LF }
    Assert.AreEqual(Byte($43), ResultBytes[4]);  { C }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestWinToUnix_NoLineEndings;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  InStream:= CreateTestStream(TBytes.Create($41, $42, $43));  { ABC }
  OutStream:= TMemoryStream.Create;
  TRY
    WinToUnix(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(3, Length(ResultBytes));
    Assert.AreEqual(Byte($41), ResultBytes[0]);
    Assert.AreEqual(Byte($42), ResultBytes[1]);
    Assert.AreEqual(Byte($43), ResultBytes[2]);
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestWinToUnix_WithNotify;
VAR
  InStream, OutStream: TMemoryStream;
  NotifyCount: Integer;
begin
  NotifyCount:= 0;
  InStream:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42));
  OutStream:= TMemoryStream.Create;
  TRY
    WinToUnix(InStream, OutStream,
      procedure(Kind: TConvertNotifyKind; Value: LongInt)
      begin
        Inc(NotifyCount);
      end
    );

    { Should have been called: 1 for nkMax + 4 for each byte + 1 for completion }
    Assert.IsTrue(NotifyCount > 0, 'Notify should have been called');
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


{ UnixToWin Tests }

procedure TTestIOPlatformFile.TestUnixToWin_Basic;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  { Input: A<LF>B -> Output: A<CRLF>B }
  InStream:= CreateTestStream(TBytes.Create($41, $0A, $42));
  OutStream:= TMemoryStream.Create;
  TRY
    UnixToWin(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(4, Length(ResultBytes));
    Assert.AreEqual(Byte($41), ResultBytes[0]);  { A }
    Assert.AreEqual(Byte($0D), ResultBytes[1]);  { CR }
    Assert.AreEqual(Byte($0A), ResultBytes[2]);  { LF }
    Assert.AreEqual(Byte($42), ResultBytes[3]);  { B }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestUnixToWin_MultipleLines;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  { Input: A<LF>B<LF>C -> Output: A<CRLF>B<CRLF>C }
  InStream:= CreateTestStream(TBytes.Create($41, $0A, $42, $0A, $43));
  OutStream:= TMemoryStream.Create;
  TRY
    UnixToWin(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(7, Length(ResultBytes));  { 3 chars + 2*CRLF }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestUnixToWin_NoLineEndings;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  InStream:= CreateTestStream(TBytes.Create($41, $42, $43));
  OutStream:= TMemoryStream.Create;
  TRY
    UnixToWin(InStream, OutStream, nil);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(3, Length(ResultBytes));
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


{ MacToWin Tests }

procedure TTestIOPlatformFile.TestMacToWin_Basic;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  { Input: A<CR>B -> Output: A<CRLF>B }
  InStream:= CreateTestStream(TBytes.Create($41, $0D, $42));
  OutStream:= TMemoryStream.Create;
  TRY
    MacToWin(InStream, OutStream);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(4, Length(ResultBytes));
    Assert.AreEqual(Byte($41), ResultBytes[0]);  { A }
    Assert.AreEqual(Byte($0D), ResultBytes[1]);  { CR }
    Assert.AreEqual(Byte($0A), ResultBytes[2]);  { LF }
    Assert.AreEqual(Byte($42), ResultBytes[3]);  { B }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


procedure TTestIOPlatformFile.TestMacToWin_MultipleLines;
VAR
  InStream, OutStream: TMemoryStream;
  ResultBytes: TBytes;
begin
  InStream:= CreateTestStream(TBytes.Create($41, $0D, $42, $0D, $43));
  OutStream:= TMemoryStream.Create;
  TRY
    MacToWin(InStream, OutStream);
    ResultBytes:= StreamToBytes(OutStream);

    Assert.AreEqual(7, Length(ResultBytes));  { 3 chars + 2*CRLF }
  FINALLY
    FreeAndNil(InStream);
    FreeAndNil(OutStream);
  END;
end;


{ Round-trip Tests }

procedure TTestIOPlatformFile.TestRoundTrip_WinToUnixToWin;
VAR
  Original, Step1, Step2: TMemoryStream;
  OrigBytes, FinalBytes: TBytes;
begin
  { Windows -> Unix -> Windows should preserve content }
  Original:= CreateTestStream(TBytes.Create($41, $0D, $0A, $42, $0D, $0A, $43));
  Step1:= TMemoryStream.Create;
  Step2:= TMemoryStream.Create;
  TRY
    OrigBytes:= StreamToBytes(Original);
    Original.Position:= 0;

    WinToUnix(Original, Step1, nil);
    Step1.Position:= 0;

    UnixToWin(Step1, Step2, nil);
    FinalBytes:= StreamToBytes(Step2);

    Assert.AreEqual(Length(OrigBytes), Length(FinalBytes), 'Round-trip should preserve size');
    Assert.IsTrue(CompareMem(@OrigBytes[0], @FinalBytes[0], Length(OrigBytes)),
      'Round-trip should preserve content');
  FINALLY
    FreeAndNil(Original);
    FreeAndNil(Step1);
    FreeAndNil(Step2);
  END;
end;


procedure TTestIOPlatformFile.TestRoundTrip_UnixToWinToUnix;
VAR
  Original, Step1, Step2: TMemoryStream;
  OrigBytes, FinalBytes: TBytes;
begin
  { Unix -> Windows -> Unix should preserve content }
  Original:= CreateTestStream(TBytes.Create($41, $0A, $42, $0A, $43));
  Step1:= TMemoryStream.Create;
  Step2:= TMemoryStream.Create;
  TRY
    OrigBytes:= StreamToBytes(Original);
    Original.Position:= 0;

    UnixToWin(Original, Step1, nil);
    Step1.Position:= 0;

    WinToUnix(Step1, Step2, nil);
    FinalBytes:= StreamToBytes(Step2);

    Assert.AreEqual(Length(OrigBytes), Length(FinalBytes), 'Round-trip should preserve size');
    Assert.IsTrue(CompareMem(@OrigBytes[0], @FinalBytes[0], Length(OrigBytes)),
      'Round-trip should preserve content');
  FINALLY
    FreeAndNil(Original);
    FreeAndNil(Step1);
    FreeAndNil(Step2);
  END;
end;


{ File-based Tests }

procedure TTestIOPlatformFile.TestWinToUnix_File;
VAR
  InPath, OutPath: string;
  ResultBytes: TBytes;
begin
  InPath:= TPath.Combine(FTestDir, 'win_input.txt');
  OutPath:= TPath.Combine(FTestDir, 'unix_output.txt');

  TFile.WriteAllBytes(InPath, TBytes.Create($41, $0D, $0A, $42));
  WinToUnix(InPath, OutPath, nil);

  Assert.IsTrue(FileExists(OutPath), 'Output file should exist');
  ResultBytes:= TFile.ReadAllBytes(OutPath);
  Assert.AreEqual(3, Length(ResultBytes));
  Assert.AreEqual(Byte($0A), ResultBytes[1]);  { LF, no CR }
end;


procedure TTestIOPlatformFile.TestUnixToWin_File;
VAR
  InPath, OutPath: string;
  ResultBytes: TBytes;
begin
  InPath:= TPath.Combine(FTestDir, 'unix_input.txt');
  OutPath:= TPath.Combine(FTestDir, 'win_output.txt');

  TFile.WriteAllBytes(InPath, TBytes.Create($41, $0A, $42));
  UnixToWin(InPath, OutPath, nil);

  Assert.IsTrue(FileExists(OutPath), 'Output file should exist');
  ResultBytes:= TFile.ReadAllBytes(OutPath);
  Assert.AreEqual(4, Length(ResultBytes));
  Assert.AreEqual(Byte($0D), ResultBytes[1]);  { CR }
  Assert.AreEqual(Byte($0A), ResultBytes[2]);  { LF }
end;


procedure TTestIOPlatformFile.TestMacToWin_File;
VAR
  InPath, OutPath: string;
  WasConverted: Boolean;
  ResultBytes: TBytes;
begin
  InPath:= TPath.Combine(FTestDir, 'mac_input.txt');
  OutPath:= TPath.Combine(FTestDir, 'win_output.txt');

  TFile.WriteAllBytes(InPath, TBytes.Create($41, $0D, $42));  { Mac format }
  WasConverted:= MacToWin(InPath, OutPath);

  Assert.IsTrue(WasConverted, 'Should return True for Mac file');
  Assert.IsTrue(FileExists(OutPath), 'Output file should exist');
  ResultBytes:= TFile.ReadAllBytes(OutPath);
  Assert.AreEqual(4, Length(ResultBytes));
end;


procedure TTestIOPlatformFile.TestMacToWin_File_NotMac;
VAR
  InPath, OutPath: string;
  WasConverted: Boolean;
begin
  InPath:= TPath.Combine(FTestDir, 'win_input.txt');
  OutPath:= TPath.Combine(FTestDir, 'output.txt');

  TFile.WriteAllBytes(InPath, TBytes.Create($41, $0D, $0A, $42));  { Windows format }
  WasConverted:= MacToWin(InPath, OutPath);

  Assert.IsFalse(WasConverted, 'Should return False for non-Mac file');
  Assert.IsFalse(FileExists(OutPath), 'Output file should NOT exist when not converted');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestIOPlatformFile);

end.
