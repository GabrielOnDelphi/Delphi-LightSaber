unit Test.LightCore.LogTypes;

{=============================================================================================================
   Unit tests for LightCore.LogTypes.pas
   Tests log verbosity types and conversion
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightCore.LogTypes;

type
  [TestFixture]
  TTestLogTypes = class
  public
    [Test]
    procedure TestVerbosity2String_Debug;

    [Test]
    procedure TestVerbosity2String_Verbose;

    [Test]
    procedure TestVerbosity2String_Hints;

    [Test]
    procedure TestVerbosity2String_Infos;

    [Test]
    procedure TestVerbosity2String_Important;

    [Test]
    procedure TestVerbosity2String_Warnings;

    [Test]
    procedure TestVerbosity2String_Errors;

    [Test]
    procedure TestVerbosityOrder;

    [Test]
    procedure TestDefaultVerbosity;
  end;

implementation


procedure TTestLogTypes.TestVerbosity2String_Debug;
begin
  Assert.AreEqual('Debug', Verbosity2String(lvDebug));
end;

procedure TTestLogTypes.TestVerbosity2String_Verbose;
begin
  Assert.AreEqual('Verbose', Verbosity2String(lvVerbose));
end;

procedure TTestLogTypes.TestVerbosity2String_Hints;
begin
  Assert.AreEqual('Hints', Verbosity2String(lvHints));
end;

procedure TTestLogTypes.TestVerbosity2String_Infos;
begin
  Assert.AreEqual('Info', Verbosity2String(lvInfos));
end;

procedure TTestLogTypes.TestVerbosity2String_Important;
begin
  Assert.AreEqual('Important', Verbosity2String(lvImportant));
end;

procedure TTestLogTypes.TestVerbosity2String_Warnings;
begin
  Assert.AreEqual('Warnings', Verbosity2String(lvWarnings));
end;

procedure TTestLogTypes.TestVerbosity2String_Errors;
begin
  Assert.AreEqual('Errors', Verbosity2String(lvErrors));
end;

procedure TTestLogTypes.TestVerbosityOrder;
begin
  { Verify that verbosity levels are ordered correctly }
  Assert.IsTrue(lvDebug < lvVerbose);
  Assert.IsTrue(lvVerbose < lvHints);
  Assert.IsTrue(lvHints < lvInfos);
  Assert.IsTrue(lvInfos < lvImportant);
  Assert.IsTrue(lvImportant < lvWarnings);
  Assert.IsTrue(lvWarnings < lvErrors);
end;

procedure TTestLogTypes.TestDefaultVerbosity;
begin
  Assert.AreEqual(lvInfos, DefaultVerbosity);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLogTypes);

end.
