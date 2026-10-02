unit Test.LightVcl.Internet.Common;

{=============================================================================================================
   Unit tests for LightVcl.Internet.Common
   The tests of ParseURL and GetLocalIP are in Test.LightCore.Internet.pas, beside the routines.

   Note: this unit is linked by no project (its line is commented out in Tests_LightVcl.Internet.dpr).

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestInternetCommon = class
  public
    { PathIsURL Tests }
    [Test]
    procedure TestPathIsURL_ValidHTTP;

    [Test]
    procedure TestPathIsURL_ValidHTTPS;

    [Test]
    procedure TestPathIsURL_InvalidPath;

    [Test]
    procedure TestPathIsURL_LocalPath;
  end;


implementation

uses
  LightVcl.Internet.Common;


{ PathIsURL Tests }

procedure TTestInternetCommon.TestPathIsURL_ValidHTTP;
begin
  Assert.IsTrue(PathIsURLW(PWideChar('http://www.example.com')), 'http:// should be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_ValidHTTPS;
begin
  Assert.IsTrue(PathIsURLW(PWideChar('https://www.example.com')), 'https:// should be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_InvalidPath;
begin
  { Note: PathIsURL only checks for scheme prefix, not full URL validity }
  Assert.IsFalse(PathIsURLW(PWideChar('not a url')), 'Plain text should not be recognized as URL');
end;


procedure TTestInternetCommon.TestPathIsURL_LocalPath;
begin
  Assert.IsFalse(PathIsURLW(PWideChar('C:\Windows\System32')), 'Local path should not be recognized as URL');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestInternetCommon);

end.
