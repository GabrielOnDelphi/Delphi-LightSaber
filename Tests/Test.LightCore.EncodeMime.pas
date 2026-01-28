unit Test.LightCore.EncodeMime;

{=============================================================================================================
   Unit tests for LightCore.EncodeMime.pas
   Tests MIME encoding and decoding functions
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.EncodeMime;

type
  [TestFixture]
  TTestEncodeMime = class
  public
    { Unicode String Tests }
    [Test]
    procedure TestMimeString_Simple;

    [Test]
    procedure TestMimeString_Empty;

    [Test]
    procedure TestMimeString_Unicode;

    [Test]
    procedure TestMimeString_RoundTrip;

    [Test]
    procedure TestDeMimeString_Simple;

    { AnsiString Tests }
    [Test]
    procedure TestMimeStringA_Simple;

    [Test]
    procedure TestMimeStringA_Empty;

    [Test]
    procedure TestMimeStringA_RoundTrip;

    [Test]
    procedure TestDeMimeStringA_Simple;

    { Known Values Tests }
    [Test]
    procedure TestMimeString_KnownValue;

    [Test]
    procedure TestMimeStringA_KnownValue;
  end;

implementation


{ Unicode String Tests }

procedure TTestEncodeMime.TestMimeString_Simple;
var
  Encoded: string;
begin
  Encoded:= MimeString('Hello');
  Assert.IsNotEmpty(Encoded);
  Assert.AreNotEqual('Hello', Encoded);  { Should be encoded }
end;

procedure TTestEncodeMime.TestMimeString_Empty;
var
  Encoded: string;
begin
  Encoded:= MimeString('');
  Assert.AreEqual('', Encoded);
end;

procedure TTestEncodeMime.TestMimeString_Unicode;
var
  Original, Encoded, Decoded: string;
begin
  Original:= 'Hello éàü 世界';
  Encoded:= MimeString(Original);
  Decoded:= DeMimeString(Encoded);
  Assert.AreEqual(Original, Decoded);
end;

procedure TTestEncodeMime.TestMimeString_RoundTrip;
var
  Original, Decoded: string;
begin
  Original:= 'Test String 12345 !@#$%';
  Decoded:= DeMimeString(MimeString(Original));
  Assert.AreEqual(Original, Decoded);
end;

procedure TTestEncodeMime.TestDeMimeString_Simple;
var
  Encoded, Decoded: string;
begin
  Encoded:= MimeString('Test');
  Decoded:= DeMimeString(Encoded);
  Assert.AreEqual('Test', Decoded);
end;


{ AnsiString Tests }

procedure TTestEncodeMime.TestMimeStringA_Simple;
var
  Encoded: AnsiString;
begin
  Encoded:= MimeStringA('Hello');
  Assert.IsTrue(Length(Encoded) > 0);
  Assert.AreNotEqual(AnsiString('Hello'), Encoded);
end;

procedure TTestEncodeMime.TestMimeStringA_Empty;
var
  Encoded: AnsiString;
begin
  Encoded:= MimeStringA('');
  Assert.AreEqual(AnsiString(''), Encoded);
end;

procedure TTestEncodeMime.TestMimeStringA_RoundTrip;
var
  Original, Decoded: AnsiString;
begin
  Original:= 'Test AnsiString 12345';
  Decoded:= DeMimeStringA(MimeStringA(Original));
  Assert.AreEqual(Original, Decoded);
end;

procedure TTestEncodeMime.TestDeMimeStringA_Simple;
var
  Encoded, Decoded: AnsiString;
begin
  Encoded:= MimeStringA('Test');
  Decoded:= DeMimeStringA(Encoded);
  Assert.AreEqual(AnsiString('Test'), Decoded);
end;


{ Known Values Tests }

procedure TTestEncodeMime.TestMimeString_KnownValue;
var
  Encoded: string;
begin
  { Base64 of 'Hello' is 'SGVsbG8=' }
  Encoded:= MimeString('Hello');
  Assert.AreEqual('SGVsbG8=', Encoded);
end;

procedure TTestEncodeMime.TestMimeStringA_KnownValue;
var
  Encoded: AnsiString;
begin
  { Base64 of 'Hello' is 'SGVsbG8=' }
  Encoded:= MimeStringA('Hello');
  Assert.AreEqual(AnsiString('SGVsbG8='), Encoded);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEncodeMime);

end.
