unit Test.LightVcl.Common.Sound;

{=============================================================================================================
   Unit tests for LightVcl.Common.Sound.pas
   Tests sound utility functions with focus on parameter validation.

   Note: Many sound functions produce actual audio output, so tests focus on
   edge cases and parameter validation rather than verifying sound quality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

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

    { PlayWinSound Tests }
    [Test]
    procedure TestPlayWinSound_EmptyString_NoException;

    [Test]
    procedure TestPlayWinSound_ValidName_NoException;

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

    [Test]
    procedure TestPlayTone_VolumeClampedTo127;

    { Bip Tests - verify no exceptions with valid parameters }
    [Test]
    procedure TestBip_ValidParams_NoException;

    [Test]
    procedure TestBipError_NoException;

    [Test]
    procedure TestBipErrorShort_NoException;

    [Test]
    procedure TestBipConfirmation_NoException;

    [Test]
    procedure TestBipConfirmationShort_NoException;

    [Test]
    procedure TestBip30_NoException;

    [Test]
    procedure TestBip50_NoException;

    [Test]
    procedure TestBip100_NoException;

    [Test]
    procedure TestBip300_NoException;

    [Test]
    procedure TestBipCoconuts_NoException;
  end;

implementation

uses
  LightVcl.Common.Sound;


procedure TTestSound.Setup;
begin
  { No setup needed }
end;


procedure TTestSound.TearDown;
begin
  { No teardown needed }
end;


{ PlayWinSound Tests }

procedure TTestSound.TestPlayWinSound_EmptyString_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayWinSound('');
    end,
    Exception,
    'PlayWinSound with empty string should not raise exception');
end;


procedure TTestSound.TestPlayWinSound_ValidName_NoException;
begin
  { Using a system sound that should exist on all Windows installations }
  Assert.WillNotRaise(
    procedure
    begin
      PlayWinSound('SystemAsterisk');
    end,
    Exception,
    'PlayWinSound with valid system sound name should not raise exception');
end;


{ PlaySoundFile Tests }

procedure TTestSound.TestPlaySoundFile_EmptyString_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlaySoundFile('');
    end,
    Exception,
    'PlaySoundFile with empty string should not raise exception');
end;


procedure TTestSound.TestPlaySoundFile_NonexistentFile_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlaySoundFile('C:\NonExistent\File\That\Does\Not\Exist.wav');
    end,
    Exception,
    'PlaySoundFile with non-existent file should not raise exception');
end;


{ PlayResSound Tests }

procedure TTestSound.TestPlayResSound_EmptyString_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayResSound('', 0);
    end,
    Exception,
    'PlayResSound with empty string should not raise exception');
end;


{ PlayTone Tests }

procedure TTestSound.TestPlayTone_ZeroFrequency_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayTone(0, 100, 50);
    end,
    Exception,
    'PlayTone with zero frequency should exit without exception');
end;


procedure TTestSound.TestPlayTone_NegativeFrequency_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayTone(-100, 100, 50);
    end,
    Exception,
    'PlayTone with negative frequency should exit without exception');
end;


procedure TTestSound.TestPlayTone_ZeroDuration_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayTone(440, 0, 50);
    end,
    Exception,
    'PlayTone with zero duration should exit without exception');
end;


procedure TTestSound.TestPlayTone_NegativeDuration_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      PlayTone(440, -100, 50);
    end,
    Exception,
    'PlayTone with negative duration should exit without exception');
end;


procedure TTestSound.TestPlayTone_VolumeClampedTo127;
begin
  { Volume > 127 should be clamped, not cause overflow or exception }
  Assert.WillNotRaise(
    procedure
    begin
      PlayTone(440, 10, 255);  { Volume 255 should be clamped to 127 }
    end,
    Exception,
    'PlayTone with volume > 127 should clamp value without exception');
end;


{ Bip Tests }

procedure TTestSound.TestBip_ValidParams_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Bip(800, 10);  { Very short duration to minimize test time }
    end,
    Exception,
    'Bip with valid parameters should not raise exception');
end;


procedure TTestSound.TestBipError_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      BipError;
    end,
    Exception,
    'BipError should not raise exception');
end;


procedure TTestSound.TestBipErrorShort_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      BipErrorShort;
    end,
    Exception,
    'BipErrorShort should not raise exception');
end;


procedure TTestSound.TestBipConfirmation_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      BipConfirmation;
    end,
    Exception,
    'BipConfirmation should not raise exception');
end;


procedure TTestSound.TestBipConfirmationShort_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      BipConfirmationShort;
    end,
    Exception,
    'BipConfirmationShort should not raise exception');
end;


procedure TTestSound.TestBip30_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Bip30;
    end,
    Exception,
    'Bip30 should not raise exception');
end;


procedure TTestSound.TestBip50_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Bip50;
    end,
    Exception,
    'Bip50 should not raise exception');
end;


procedure TTestSound.TestBip100_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Bip100;
    end,
    Exception,
    'Bip100 should not raise exception');
end;


procedure TTestSound.TestBip300_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Bip300;
    end,
    Exception,
    'Bip300 should not raise exception');
end;


procedure TTestSound.TestBipCoconuts_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      BipCoconuts;
    end,
    Exception,
    'BipCoconuts should not raise exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestSound);

end.
