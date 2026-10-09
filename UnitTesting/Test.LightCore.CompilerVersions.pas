unit Test.LightCore.CompilerVersions;

{=============================================================================================================
   Unit tests for LightCore.CompilerVersions.pas
   Tests compiler version constants for correctness and chronological ordering.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.CompilerVersions;

type
  [TestFixture]
  TTestCompiler = class
  public
    { Version Constants - Value Tests }
    [Test]
    procedure TestDelphiYukon;

    [Test]
    procedure TestDelphiAthens;

    [Test]
    procedure TestDelphiAlex;

    [Test]
    procedure TestDelphiSydney;

    [Test]
    procedure TestDelphiRio;

    [Test]
    procedure TestDelphiTokyo;

    { Version Order Tests }
    [Test]
    procedure TestVersionChronologicalOrder;

    [Test]
    procedure TestVersionStrictlyIncreasing;

    { RTLVersion Tests }
    [Test]
    procedure TestRTLVersionIsKnown;

    { Edge Cases }
    [Test]
    procedure TestVersionDifferencesAreSignificant;
  end;

implementation


{ Version Constants - Value Tests }

procedure TTestCompiler.TestDelphiYukon;
begin
  { Delphi 13 Yukon = RTLVersion 37.0 }
  Assert.AreEqual(37.0, Delphi13_Florence, 0.001, 'Delphi 13 Yukon should be 37.0');
end;


procedure TTestCompiler.TestDelphiAthens;
begin
  { Delphi 12 Athens = RTLVersion 36.0 }
  Assert.AreEqual(36.0, Delphi12_Athens, 0.001, 'Delphi 12 Athens should be 36.0');
end;


procedure TTestCompiler.TestDelphiAlex;
begin
  { Delphi 11 Alexandria = RTLVersion 35.0 }
  Assert.AreEqual(35.0, Delphi11_Alex, 0.001, 'Delphi 11 Alexandria should be 35.0');
end;


procedure TTestCompiler.TestDelphiSydney;
begin
  { Delphi 10.4 Sydney = RTLVersion 34.0 }
  Assert.AreEqual(34.0, Delphi10_Sydney, 0.001, 'Delphi 10.4 Sydney should be 34.0');
end;


procedure TTestCompiler.TestDelphiRio;
begin
  { Delphi 10.3 Rio = RTLVersion 33.0 }
  Assert.AreEqual(33.0, Delphi10_Rio, 0.001, 'Delphi 10.3 Rio should be 33.0');
end;


procedure TTestCompiler.TestDelphiTokyo;
begin
  { Delphi 10.2 Tokyo = RTLVersion 32.0 }
  Assert.AreEqual(32.0, Delphi10_Tokyo, 0.001, 'Delphi 10.2 Tokyo should be 32.0');
end;


{ Version Order Tests }

procedure TTestCompiler.TestVersionChronologicalOrder;
begin
  { Verify versions are in correct chronological/release order (newer > older) }
  Assert.IsTrue(Delphi13_Florence > Delphi12_Athens,   'Yukon should be newer than Athens');
  Assert.IsTrue(Delphi12_Athens > Delphi11_Alex,    'Athens should be newer than Alex');
  Assert.IsTrue(Delphi11_Alex > Delphi10_Sydney,    'Alex should be newer than Sydney');
  Assert.IsTrue(Delphi10_Sydney > Delphi10_Rio,     'Sydney should be newer than Rio');
  Assert.IsTrue(Delphi10_Rio > Delphi10_Tokyo,      'Rio should be newer than Tokyo');
  Assert.IsTrue(Delphi10_Tokyo > Delphi10_Berlin,   'Tokyo should be newer than Berlin');
  Assert.IsTrue(Delphi10_Berlin > Delphi10_Seattle, 'Berlin should be newer than Seattle');
  Assert.IsTrue(Delphi10_Seattle > Delphi_XE8,    'Seattle should be newer than XE8');
  Assert.IsTrue(Delphi_XE8 > Delphi_XE7,        'XE8 should be newer than XE7');
  Assert.IsTrue(Delphi_XE7 > Delphi_XE6,        'XE7 should be newer than XE6');
  Assert.IsTrue(Delphi_XE6 > Delphi_XE5,        'XE6 should be newer than XE5');
  Assert.IsTrue(Delphi_XE5 > Delphi_XE4,        'XE5 should be newer than XE4');
  Assert.IsTrue(Delphi_XE4 > Delphi_XE3,        'XE4 should be newer than XE3');
  Assert.IsTrue(Delphi_XE3 > Delphi_XE2,        'XE3 should be newer than XE2');
  Assert.IsTrue(Delphi_XE2 > Delphi_XE,         'XE2 should be newer than XE');
  Assert.IsTrue(Delphi_XE > Delphi_2010,        'XE should be newer than 2010');
  Assert.IsTrue(Delphi_2010 > Delphi_2009,      '2010 should be newer than 2009');
  Assert.IsTrue(Delphi_2009 > Delphi_2007,      '2009 should be newer than 2007');
  Assert.IsTrue(Delphi_2007 > Delphi_2006,      '2007 should be newer than 2006');
  Assert.IsTrue(Delphi_2006 > Delphi_2005,      '2006 should be newer than 2005');
  Assert.IsTrue(Delphi_2005 > Delphi_8Net,      '2005 should be newer than 8.Net');
  Assert.IsTrue(Delphi_8Net > Delphi_7,         '8.Net should be newer than 7');
end;


procedure TTestCompiler.TestVersionStrictlyIncreasing;
begin
  { Verify no two versions have the same value }
  Assert.AreNotEqual(Delphi13_Florence, Delphi12_Athens, 0.001);
  Assert.AreNotEqual(Delphi12_Athens, Delphi11_Alex, 0.001);
  Assert.AreNotEqual(Delphi11_Alex, Delphi10_Sydney, 0.001);
  Assert.AreNotEqual(Delphi10_Sydney, Delphi10_Rio, 0.001);
  Assert.AreNotEqual(Delphi10_Rio, Delphi10_Tokyo, 0.001);
end;


{ RTLVersion Tests }

{ The constant named for the compiler that builds this suite must equal that compiler's own System.RTLVersion
  (RTLVersion = 37.00 in c:\Delphi\Delphi 13\source\rtl\sys\System.pas). A compiler not listed here fails: its constant is missing. }
procedure TTestCompiler.TestRTLVersionIsKnown;
begin
  {$IF Defined(VER370)}
  Assert.AreEqual(RTLVersion, Delphi13_Florence, 0.001, 'Delphi13_Florence must equal the RTLVersion of Delphi 13');
  {$ELSEIF Defined(VER360)}
  Assert.AreEqual(RTLVersion, Delphi12_Athens, 0.001, 'Delphi12_Athens must equal the RTLVersion of Delphi 12');
  {$ELSEIF Defined(VER350)}
  Assert.AreEqual(RTLVersion, Delphi11_Alex, 0.001, 'Delphi11_Alex must equal the RTLVersion of Delphi 11');
  {$ELSEIF Defined(VER340)}
  Assert.AreEqual(RTLVersion, Delphi10_Sydney, 0.001, 'Delphi10_Sydney must equal the RTLVersion of Delphi 10.4');
  {$ELSEIF Defined(VER330)}
  Assert.AreEqual(RTLVersion, Delphi10_Rio, 0.001, 'Delphi10_Rio must equal the RTLVersion of Delphi 10.3');
  {$ELSE}
  Assert.Fail('No constant in LightCore.CompilerVersions for RTLVersion ' + FloatToStr(RTLVersion));
  {$ENDIF}
end;


{ Edge Cases }

procedure TTestCompiler.TestVersionDifferencesAreSignificant;
begin
  { Version differences should be >= 0.5 to avoid floating point comparison issues }
  Assert.IsTrue(Delphi13_Florence - Delphi12_Athens >= 0.5, 'Version gap should be significant');
  Assert.IsTrue(Delphi12_Athens - Delphi11_Alex >= 0.5, 'Version gap should be significant');
  Assert.IsTrue(Delphi_2007 - Delphi_2006 >= 0.5, 'Version gap should be significant');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCompiler);

end.
