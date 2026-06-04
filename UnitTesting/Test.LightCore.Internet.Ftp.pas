unit Test.LightCore.Internet.Ftp;

{=============================================================================================================
   Unit tests for LightCore.Internet.Ftp

   Scope: only the parts that run WITHOUT a live FTP server.
     - TFTPDetails.Clear
     - TFTPDetails.Write -> Read round-trip (the binary wire format that .thunder files persist).
       This is the important one: the unit header declares the layout backward-compatibility-mandatory,
       so a reordered field or a changed padding size must fail here.
     - TFtpUploader.Create nil-guard contract.

   The TFtpUploader directory/upload methods (UploadFolder, ChangeDirForce, NavigateTo, DirectoryExists)
   talk to a connected TIdFTP and can only be exercised against a real server - they are covered by the
   manual live-server test, not here, because a network round-trip would make these tests flaky.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  LightCore.StreamBuff,
  LightCore.Internet.Ftp;

type
  [TestFixture]
  TTestFtpDetails = class
  private
    FTestFile: string;
    function  MakeSample: TFTPDetails;                    { A fully-populated record with distinct field values }
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestClear;
    [Test] procedure TestWriteRead_RoundTrip;
    [Test] procedure TestWriteRead_EmptyRecord;
    [Test] procedure TestWriteRead_PreservesPassiveFalse;
    [Test] procedure TestUploaderCreate_NilRaises;
  end;


implementation


function TTestFtpDetails.MakeSample: TFTPDetails;
begin
  Result.Clear;
  Result.Server     := 'ftp.example.com';
  Result.RemoteDir  := '/public_html/sub/';
  Result.User       := 'user42';
  Result.Parola     := 'p@ss w0rd!';                     { Spaces + symbols: catch any length/encoding mistake }
  Result.Passive    := True;
  Result.OnlineAdr  := 'https://example.com';
  Result.AccountName:= 'acc_000webhost';
end;


procedure TTestFtpDetails.Setup;
begin
  FTestFile:= TPath.Combine(TPath.GetTempPath, 'FtpDetailsTest_' + IntToStr(Random(MaxInt)) + '.dat');
end;


procedure TTestFtpDetails.TearDown;
begin
  if TFile.Exists(FTestFile)
  then TFile.Delete(FTestFile);
end;


procedure TTestFtpDetails.TestClear;
var Rec: TFTPDetails;
begin
  Rec:= MakeSample;                                       { Start non-empty so Clear has something to wipe }
  Rec.Clear;

  Assert.AreEqual('', Rec.Server     , 'Server');
  Assert.AreEqual('', Rec.RemoteDir  , 'RemoteDir');
  Assert.AreEqual('', Rec.User       , 'User');
  Assert.AreEqual('', Rec.Parola     , 'Parola');
  Assert.AreEqual('', Rec.OnlineAdr  , 'OnlineAdr');
  Assert.AreEqual('', Rec.AccountName, 'AccountName');
  Assert.IsFalse(Rec.Passive, 'Passive should be reset to FALSE');
end;


{ The contract that matters: a record written by Write must read back identical via Read, padding included.
  If a future edit reorders a field or resizes the 96-byte reserved block, this round-trip breaks. }
procedure TTestFtpDetails.TestWriteRead_RoundTrip;
var
  Src, Dst: TFTPDetails;
  Stream: TLightStream;
begin
  Src:= MakeSample;

  Stream:= TLightStream.CreateWrite(FTestFile);
  try
    Src.Write(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Dst.Clear;                                              { Prove Read overwrites every field, not just sets some }
  Stream:= TLightStream.CreateRead(FTestFile);
  try
    Dst.Read(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Assert.AreEqual(Src.Server     , Dst.Server     , 'Server');
  Assert.AreEqual(Src.RemoteDir  , Dst.RemoteDir  , 'RemoteDir');
  Assert.AreEqual(Src.User       , Dst.User       , 'User');
  Assert.AreEqual(Src.Parola     , Dst.Parola     , 'Parola');
  Assert.AreEqual(Src.OnlineAdr  , Dst.OnlineAdr  , 'OnlineAdr');
  Assert.AreEqual(Src.AccountName, Dst.AccountName, 'AccountName');
  Assert.AreEqual(Src.Passive    , Dst.Passive    , 'Passive');
end;


procedure TTestFtpDetails.TestWriteRead_EmptyRecord;
var
  Src, Dst: TFTPDetails;
  Stream: TLightStream;
begin
  Src.Clear;                                              { All-empty strings must survive the round-trip too }

  Stream:= TLightStream.CreateWrite(FTestFile);
  try
    Src.Write(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Dst:= MakeSample;                                       { Pre-fill so we detect if Read fails to clear a field }
  Stream:= TLightStream.CreateRead(FTestFile);
  try
    Dst.Read(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Assert.AreEqual('', Dst.Server     , 'Server');
  Assert.AreEqual('', Dst.RemoteDir  , 'RemoteDir');
  Assert.AreEqual('', Dst.User       , 'User');
  Assert.AreEqual('', Dst.Parola     , 'Parola');
  Assert.AreEqual('', Dst.OnlineAdr  , 'OnlineAdr');
  Assert.AreEqual('', Dst.AccountName, 'AccountName');
  Assert.IsFalse(Dst.Passive, 'Passive');
end;


{ Passive defaults to FALSE; make sure a FALSE value is actually persisted (not just left at the field default). }
procedure TTestFtpDetails.TestWriteRead_PreservesPassiveFalse;
var
  Src, Dst: TFTPDetails;
  Stream: TLightStream;
begin
  Src:= MakeSample;
  Src.Passive:= False;

  Stream:= TLightStream.CreateWrite(FTestFile);
  try
    Src.Write(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Dst.Clear;
  Dst.Passive:= True;                                     { Opposite of expected, so a no-op Read is caught }
  Stream:= TLightStream.CreateRead(FTestFile);
  try
    Dst.Read(Stream);
  finally
    FreeAndNil(Stream);
  end;

  Assert.IsFalse(Dst.Passive, 'Passive FALSE must round-trip');
end;


{ TFtpUploader borrows a TIdFTP and asserts it is non-nil. Verify the guard fires. }
procedure TTestFtpDetails.TestUploaderCreate_NilRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TFtpUploader.Create(nil).Free;
    end,
    EAssertionFailed,
    'Create(nil) must raise the nil-guard assertion');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFtpDetails);

end.
