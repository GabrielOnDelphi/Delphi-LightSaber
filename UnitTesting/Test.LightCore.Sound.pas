unit Test.LightCore.Sound;

{=============================================================================================================
   Unit tests for LightCore.Sound.pas
   Tests sound utility functions with focus on parameter validation.

   Only tests that make NO sound are kept: each one passes a parameter that makes the routine exit before it reaches the sound API.
   A test that would beep or play a tone does not belong in this suite.

   Every routine under test has an empty body off Windows, so the whole fixture is compiled only when MSWINDOWS is defined.
   The tests of PlayWinSound are in Test.LightCore.Win.Sound.pas.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestSound = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { PlaySoundFile Tests }
    [Test]
    procedure TestPlaySoundFile_EmptyString_NoException;

    [Test]
    procedure TestPlaySoundFile_NonexistentFile_NoException;

    { PlayResSound Tests }
    [Test]
    procedure TestPlayResSound_EmptyString_NoException;

    { PlayTone Tests }
    [Test]
    procedure TestPlayTone_ZeroFrequency_NoException;

    [Test]
    procedure TestPlayTone_NegativeFrequency_NoException;

    [Test]
    procedure TestPlayTone_ZeroDuration_NoException;

    [Test]
    procedure TestPlayTone_NegativeDuration_NoException;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  LightCore.Sound;


procedure TTestSound.Setup;
begin
  { No setup needed }
end;


procedure TTestSound.TearDown;
begin
  { No teardown needed }
end;


{ PlaySoundFile Tests }

procedure TTestSound.TestPlaySoundFile_EmptyString_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlaySoundFile('');
    end,
    'PlaySoundFile with empty string should not raise exception');
end;


procedure TTestSound.TestPlaySoundFile_NonexistentFile_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlaySoundFile('C:\NonExistent\File\That\Does\Not\Exist.wav');
    end,
    'PlaySoundFile with non-existent file should not raise exception');
end;


{ PlayResSound Tests }

procedure TTestSound.TestPlayResSound_EmptyString_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayResSound('', FALSE);
    end,
    'PlayResSound with empty string should not raise exception');
end;


{ PlayTone Tests }

procedure TTestSound.TestPlayTone_ZeroFrequency_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayTone(0, 100, 50);
    end,
    'PlayTone with zero frequency should exit without exception');
end;


procedure TTestSound.TestPlayTone_NegativeFrequency_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayTone(-100, 100, 50);
    end,
    'PlayTone with negative frequency should exit without exception');
end;


procedure TTestSound.TestPlayTone_ZeroDuration_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayTone(440, 0, 50);
    end,
    'PlayTone with zero duration should exit without exception');
end;


procedure TTestSound.TestPlayTone_NegativeDuration_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      PlayTone(440, -100, 50);
    end,
    'PlayTone with negative duration should exit without exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSound);
{$ENDIF}

end.
