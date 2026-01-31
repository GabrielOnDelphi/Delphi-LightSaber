unit Test.LightVcl.Visual.RichLogUtils;

{=============================================================================================================
   Unit tests for LightVcl.Visual.RichLogUtils.pas
   Tests utility functions and types for the RichLog system.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestRichLogUtils = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { TLogVerb Enum Tests }
    [Test]
    procedure TestTLogVerb_VerboseIsFirst;

    [Test]
    procedure TestTLogVerb_ErrorsIsLast;

    [Test]
    procedure TestTLogVerb_OrderIsCorrect;

    { Verbosity2String Tests }
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

    { DefaultVerbosity Tests }
    [Test]
    procedure TestDefaultVerbosity_IsInfos;

    { Color Constants Tests }
    [Test]
    procedure TestColorConstants_AreNotZero;
  end;

implementation

uses
  LightCore.AppData,
  LightVcl.Visual.AppData,
  LightVcl.Visual.RichLogUtils,
  Vcl.Graphics;


procedure TTestRichLogUtils.Setup;
begin
  Assert.IsNotNull(AppData, 'AppData must be initialized before running tests');
end;


procedure TTestRichLogUtils.TearDown;
begin
  { Nothing to clean up }
end;


{ TLogVerb Enum Tests }

procedure TTestRichLogUtils.TestTLogVerb_VerboseIsFirst;
begin
  Assert.AreEqual(0, Ord(lvrVerbose), 'lvrVerbose should be the first enum value (0)');
end;


procedure TTestRichLogUtils.TestTLogVerb_ErrorsIsLast;
begin
  Assert.AreEqual(5, Ord(lvrErrors), 'lvrErrors should be the last enum value (5)');
end;


procedure TTestRichLogUtils.TestTLogVerb_OrderIsCorrect;
begin
  Assert.IsTrue(Ord(lvrVerbose) < Ord(lvrHints), 'lvrVerbose should be less than lvrHints');
  Assert.IsTrue(Ord(lvrHints) < Ord(lvrInfos), 'lvrHints should be less than lvrInfos');
  Assert.IsTrue(Ord(lvrInfos) < Ord(lvrImportant), 'lvrInfos should be less than lvrImportant');
  Assert.IsTrue(Ord(lvrImportant) < Ord(lvrWarnings), 'lvrImportant should be less than lvrWarnings');
  Assert.IsTrue(Ord(lvrWarnings) < Ord(lvrErrors), 'lvrWarnings should be less than lvrErrors');
end;


{ Verbosity2String Tests }

procedure TTestRichLogUtils.TestVerbosity2String_Verbose;
begin
  Assert.AreEqual('Verbose', Verbosity2String(lvrVerbose), 'lvrVerbose should return "Verbose"');
end;


procedure TTestRichLogUtils.TestVerbosity2String_Hints;
begin
  Assert.AreEqual('Hints', Verbosity2String(lvrHints), 'lvrHints should return "Hints"');
end;


procedure TTestRichLogUtils.TestVerbosity2String_Infos;
begin
  Assert.AreEqual('Info', Verbosity2String(lvrInfos), 'lvrInfos should return "Info"');
end;


procedure TTestRichLogUtils.TestVerbosity2String_Important;
begin
  Assert.AreEqual('Important', Verbosity2String(lvrImportant), 'lvrImportant should return "Important"');
end;


procedure TTestRichLogUtils.TestVerbosity2String_Warnings;
begin
  Assert.AreEqual('Warnings', Verbosity2String(lvrWarnings), 'lvrWarnings should return "Warnings"');
end;


procedure TTestRichLogUtils.TestVerbosity2String_Errors;
begin
  Assert.AreEqual('Errors', Verbosity2String(lvrErrors), 'lvrErrors should return "Errors"');
end;


{ DefaultVerbosity Tests }

procedure TTestRichLogUtils.TestDefaultVerbosity_IsInfos;
begin
  Assert.AreEqual(lvrInfos, DefaultVerbosity, 'DefaultVerbosity should be lvrInfos');
end;


{ Color Constants Tests }

procedure TTestRichLogUtils.TestColorConstants_AreNotZero;
begin
  { Verify all color constants are defined and not black (0) except ctLogInfo which is black }
  Assert.AreNotEqual(TColor(0), ctLogVerb, 'ctLogVerb should not be black');
  Assert.AreNotEqual(TColor(0), ctLogHint, 'ctLogHint should not be black');
  Assert.AreEqual(clBlack, ctLogInfo, 'ctLogInfo should be black');
  Assert.AreNotEqual(TColor(0), ctLogImprt, 'ctLogImprt should not be black');
  Assert.AreNotEqual(TColor(0), ctLogWarn, 'ctLogWarn should not be black');
  Assert.AreEqual(clRed, ctLogError, 'ctLogError should be red');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRichLogUtils);

end.
