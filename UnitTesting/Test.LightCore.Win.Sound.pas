unit Test.LightCore.Win.Sound;

{=============================================================================================================
   Unit tests for LightCore.Win.Sound.pas
   Tests sound utility functions with focus on parameter validation.

   Only tests that make NO sound are kept: a test that would play a sound does not belong in this suite.

   The whole fixture is compiled only when MSWINDOWS is defined, because LightCore.Win.Sound is Windows-only.
   The tests of the other sound routines are in Test.LightCore.Sound.pas.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestWinSound = class
  public
    { PlayWinSound Tests }
    [Test]
    procedure TestPlayWinSound_EmptyString_NoException;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  LightCore.Win.Sound;


{ PlayWinSound Tests }

procedure TTestWinSound.TestPlayWinSound_EmptyString_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayWinSound('');
    end,
    'PlayWinSound with empty string should not raise exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinSound);
{$ENDIF}

end.
