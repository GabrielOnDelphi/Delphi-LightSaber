unit Test.LightCore.Win.DPAPI;

{=============================================================================================================
   Unit tests for LightCore.Win.DPAPI.pas
   2026.11.05

   ProtectString / TryUnprotectString round trip through the real Windows DPAPI of the current Windows user.
   Tampering: Microsoft's CryptUnprotectData page says the function "may return ERROR_INVALID_DATA,
   ERROR_INVALID_PARAMETER, or in some cases may succeed with corrupted output", so a damaged text must give FALSE
   or a text that differs from the original - never the original.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.Win.DPAPI;

type
  [TestFixture]
  TTestDPAPI = class
  private
  const
    Entropy = 'LightSaber.UnitTests.DPAPI';
  public
    [Test] procedure RoundTrip_Ascii;
    [Test] procedure RoundTrip_NonAscii;
    [Test] procedure Protected_DoesNotContainPlainText;
    [Test] procedure Protected_IsOneLine;
    [Test] procedure OtherEntropy_False;
    [Test] procedure OneCharacterChanged_NeverOriginal;
    [Test] procedure NotBase64_False;
    [Test] procedure Empty_False;
  end;
{$ENDIF}


implementation
{$IFDEF MSWINDOWS}


procedure TTestDPAPI.RoundTrip_Ascii;
VAR Plain: string;
begin
  Assert.IsTrue(TryUnprotectString(ProtectString('Hello World 123', Entropy), Entropy, Plain));
  Assert.AreEqual('Hello World 123', Plain, False);
end;


procedure TTestDPAPI.RoundTrip_NonAscii;
CONST Text = 'Pässwörd ß € Ωμέγα 日本';
VAR Plain: string;
begin
  Assert.IsTrue(TryUnprotectString(ProtectString(Text, Entropy), Entropy, Plain));
  Assert.AreEqual(Text, Plain, False);
end;


procedure TTestDPAPI.Protected_DoesNotContainPlainText;
VAR Encrypted: string;
begin
  Encrypted:= ProtectString('VisibleSecret', Entropy);
  Assert.IsTrue(Encrypted <> '');
  Assert.DoesNotContain(Encrypted, 'VisibleSecret');
end;


procedure TTestDPAPI.Protected_IsOneLine;
VAR Encrypted: string;
begin
  // A DPAPI blob is a few hundred bytes, so a line-breaking Base64 encoder (76 characters per line) would break it.
  Encrypted:= ProtectString(StringOfChar('x', 200), Entropy);
  Assert.IsTrue(Length(Encrypted) > 76, 'precondition: the text is longer than one Base64 line');
  Assert.AreEqual(0, Pos(#13, Encrypted));
  Assert.AreEqual(0, Pos(#10, Encrypted));
end;


procedure TTestDPAPI.OtherEntropy_False;
VAR Plain: string;
begin
  Assert.IsFalse(TryUnprotectString(ProtectString('Hello World 123', Entropy), 'Another.Program', Plain));
  Assert.AreEqual('', Plain);
end;


procedure TTestDPAPI.OneCharacterChanged_NeverOriginal;
VAR
  Encrypted, Plain: string;
  Mid: Integer;
  Ok: Boolean;
begin
  Encrypted:= ProtectString('Hello World 123', Entropy);
  Mid:= Length(Encrypted) div 2;
  if Encrypted[Mid] = 'A'
  then Encrypted[Mid]:= 'B'
  else Encrypted[Mid]:= 'A';

  Ok:= TryUnprotectString(Encrypted, Entropy, Plain);
  if Ok
  then Assert.AreNotEqual('Hello World 123', Plain, False, 'a damaged blob must never give back the original text')
  else Assert.AreEqual('', Plain);
end;


procedure TTestDPAPI.NotBase64_False;
VAR Plain: string;
begin
  Assert.IsFalse(TryUnprotectString('this is not a DPAPI blob!', Entropy, Plain));
  Assert.IsFalse(TryUnprotectString('%%%%', Entropy, Plain));
  Assert.AreEqual('', Plain);
end;


procedure TTestDPAPI.Empty_False;
VAR Plain: string;
begin
  Assert.AreEqual('', ProtectString('', Entropy));
  Assert.IsFalse(TryUnprotectString('', Entropy, Plain));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestDPAPI);
{$ENDIF}

end.
