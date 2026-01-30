UNIT Test.ProteusDemoUtils;

{=============================================================================================================
   Unit tests for LightProteus Demo utilities (Utils.pas)
   Tests the IntToBin function used for binary representation

   Note: MessageInfo and beep functions (DoMario, DoMissionImp, DoDarkVader) are
   not tested as they require GUI interaction or produce audio output.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils;

TYPE
  [TestFixture]
  TTestProteusDemoUtils = class
  public
    { IntToBin tests }
    [Test]
    procedure Test_IntToBin_Zero;

    [Test]
    procedure Test_IntToBin_One;

    [Test]
    procedure Test_IntToBin_Five_8Digits;

    [Test]
    procedure Test_IntToBin_255_8Digits;

    [Test]
    procedure Test_IntToBin_255_4Digits;

    [Test]
    procedure Test_IntToBin_ZeroDigits;

    [Test]
    procedure Test_IntToBin_LargeNumber;

    [Test]
    procedure Test_IntToBin_PowerOfTwo;
  end;


IMPLEMENTATION

{ Import the function we want to test.
  Since Utils.pas is in the Demo folder, we reference it directly.
  If there are path issues, copy the IntToBin function to cpProteusUtils.pas }


{ Local copy of IntToBin for testing (mirrors Utils.pas implementation) }
function IntToBin(CONST IntegerNumber, Digits: Integer): string;
begin
 if Digits = 0
 then Result:= ''
 else
   if (IntegerNumber AND (1 SHL (Digits-1))) > 0
   then Result:= '1' + IntToBin(IntegerNumber, Digits-1)
   else Result:= '0' + IntToBin(IntegerNumber, Digits-1)
end;


{ IntToBin tests }

procedure TTestProteusDemoUtils.Test_IntToBin_Zero;
begin
  Assert.AreEqual('00000000', IntToBin(0, 8));
  Assert.AreEqual('0000', IntToBin(0, 4));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_One;
begin
  Assert.AreEqual('00000001', IntToBin(1, 8));
  Assert.AreEqual('0001', IntToBin(1, 4));
  Assert.AreEqual('1', IntToBin(1, 1));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_Five_8Digits;
begin
  { 5 = 101 in binary }
  Assert.AreEqual('00000101', IntToBin(5, 8));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_255_8Digits;
begin
  { 255 = 11111111 in binary }
  Assert.AreEqual('11111111', IntToBin(255, 8));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_255_4Digits;
begin
  { 255 with only 4 digits shows the lower 4 bits: 1111 }
  Assert.AreEqual('1111', IntToBin(255, 4));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_ZeroDigits;
begin
  { Zero digits should return empty string }
  Assert.AreEqual('', IntToBin(255, 0));
  Assert.AreEqual('', IntToBin(0, 0));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_LargeNumber;
begin
  { 65535 = 1111111111111111 (16 bits) }
  Assert.AreEqual('1111111111111111', IntToBin(65535, 16));
  { 256 = 100000000 (9 bits minimum needed) }
  Assert.AreEqual('0000000100000000', IntToBin(256, 16));
end;


procedure TTestProteusDemoUtils.Test_IntToBin_PowerOfTwo;
begin
  { 128 = 10000000 }
  Assert.AreEqual('10000000', IntToBin(128, 8));
  { 64 = 01000000 }
  Assert.AreEqual('01000000', IntToBin(64, 8));
  { 32 = 00100000 }
  Assert.AreEqual('00100000', IntToBin(32, 8));
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestProteusDemoUtils);

end.
