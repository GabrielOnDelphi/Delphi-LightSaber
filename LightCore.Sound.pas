UNIT LightCore.Sound;

{=============================================================================================================
   2026.09.15
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Sound and audio utilities.

   Includes:
     - WAV file playback
     - Resource-embedded sound playback
     - Programmatic tone generation
     - Various beep patterns for user feedback

   All 13 routines are declared on every platform. They produce sound only on Windows. Off Windows each body is empty, so the call still compiles and simply does nothing.

   PlayWinSound, which plays a Windows system sound by its name, is in LightCore.Win.Sound, a Windows-only unit of the package LightCore.Win.
=============================================================================================================}

INTERFACE
{$IFDEF MSWINDOWS}
USES
   Winapi.Windows, Winapi.MMSystem, System.SysUtils, System.Classes;
{$ENDIF}

{============================================================================================================
   SOUNDS
============================================================================================================}
 procedure PlaySoundFile(CONST FileName: string);
 procedure PlayResSound (CONST ResName: string; Async: Boolean= TRUE);

 procedure PlayTone(Frequency, Duration: Integer; Volume: Byte);   { Writes tone to memory and plays it } // Old name: MakeSound

{============================================================================================================
   BEEPS
============================================================================================================}
 procedure Bip(Frecv, Timp: integer);
 procedure BipConfirmation;
 procedure BipConfirmationShort;
 procedure BipError;
 procedure BipErrorShort;
 procedure Bip30;
 procedure Bip50;
 procedure Bip100;
 procedure Bip300;
 procedure BipCoconuts;



IMPLEMENTATION
{$IFDEF MSWINDOWS}
USES
  LightCore.AppData;


{ Plays a WAV file asynchronously. Does nothing if the file doesn't exist. }
procedure PlaySoundFile(CONST FileName: string);
begin
 if (FileName <> '') AND FileExists(FileName)
 then PlaySound(PChar(FileName), 0, SND_ASYNC or SND_FILENAME);
end;



{ Plays a sound embedded in application resources.
  Async = TRUE starts playing and returns at once. Async = FALSE returns only when the sound has finished.

  How to embed a sound in a resource:
    1. Create file 'SOUNDS.RC' with:
         #define WAVE WAVEFILE
         SOUND1 WAVE "updating.wav"
    2. Compile: BRCC32.EXE -foSOUND32.RES SOUNDS.RC

  Note: UnlockResource and FreeResource are deprecated since Windows 95 and do nothing.
  Resources are automatically freed when the module is unloaded. }
procedure PlayResSound(CONST ResName: string; Async: Boolean= TRUE);
VAR
  hResInfo, hRes: THandle;
  lpGlob: PChar;
  uFlags: Integer;
begin
 if ResName = ''
 then EXIT;

 hResInfo:= FindResource(HInstance, PChar(ResName), MAKEINTRESOURCE('WAVEFILE'));
 if hResInfo = 0 then
  begin
    { The WAV is compiled into our own EXE, so this is a build error and no user can cause it.
      It RAISES rather than asserting: an Assert is stripped from the Release build, which is exactly the build where a missing resource would then fail with no trace at all. }
    raise EResNotFound.Create('PlayResSound: resource not found: '+ ResName);
  end;

 hRes:= LoadResource(HInstance, hResInfo);
 if hRes = 0 then
  begin
    raise EResNotFound.Create('PlayResSound: cannot load resource: '+ ResName);
  end;

 lpGlob:= LockResource(hRes);
 if lpGlob = NIL then
  begin
    raise EResNotFound.Create('PlayResSound: cannot lock resource: '+ ResName);
  end;

 if Async
 then uFlags:= SND_ASYNC
 else uFlags:= SND_SYNC;
 uFlags:= SND_MEMORY or uFlags;
 SndPlaySound(lpGlob, uFlags);
end;





{ Generates and plays a pure sine wave tone.
    Frequency - Hz. Max ~6600 Hz, because the sample rate is 11025 Hz.
    Duration  - milliseconds.
    Volume    - 0 to 127. Anything higher is clamped to 127. }
procedure PlayTone(Frequency, Duration: Integer; Volume: Byte);
VAR
  WaveFormatEx: TWaveFormatEx;
  MS: TMemoryStream;
  i, TempInt, DataCount, RiffCount: Integer;
  SoundValue: Byte;
  Omega: Double;   { Angular frequency: 2 * Pi * Frequency }
CONST
  Mono: Word = $0001;
  SampleRate: Integer = 11025;   { Valid: 8000, 11025, 22050, or 44100 }
  { AnsiString, NOT string: these are written as raw bytes into the RIFF header.
    As UnicodeString, MS.Write(RiffId[1], 4) would emit 'R'#0'I'#0 (UTF-16) and corrupt
    the WAV magic numbers - PlaySound then fails silently and no tone is ever heard. }
  RiffId: AnsiString = 'RIFF';
  WaveId: AnsiString = 'WAVE';
  FmtId: AnsiString = 'fmt ';
  DataId: AnsiString = 'data';
begin
  if Frequency <= 0
  then EXIT;
  if Duration <= 0
  then EXIT;

  if Volume > 127
  then Volume:= 127;

  if Frequency > (0.6 * SampleRate) then
  begin
    AppDataCore.LogWarn('PlayTone: sample rate of '+ IntToStr(SampleRate)+ ' is too low to play a tone of '+ IntToStr(Frequency)+ 'Hz');
    EXIT;
  end;

  WaveFormatEx.wFormatTag     := WAVE_FORMAT_PCM;
  WaveFormatEx.nChannels      := Mono;
  WaveFormatEx.nSamplesPerSec := SampleRate;
  WaveFormatEx.wBitsPerSample := $0008;
  WaveFormatEx.nBlockAlign    := (WaveFormatEx.nChannels * WaveFormatEx.wBitsPerSample) div 8;
  WaveFormatEx.nAvgBytesPerSec:= WaveFormatEx.nSamplesPerSec * WaveFormatEx.nBlockAlign;
  WaveFormatEx.cbSize         := 0;

  MS:= TMemoryStream.Create;
  TRY
    { Calculate length of sound data and file data }
    DataCount:= (Duration * SampleRate) div 1000;
    RiffCount:= Length(WaveId) + Length(FmtId) + SizeOf(DWORD) +
                SizeOf(TWaveFormatEx) + Length(DataId) + SizeOf(DWORD) + DataCount;

    { Write wave header }
    MS.Write(RiffId[1], 4);
    MS.Write(RiffCount, SizeOf(DWORD));
    MS.Write(WaveId[1], Length(WaveId));
    MS.Write(FmtId[1], Length(FmtId));
    TempInt:= SizeOf(TWaveFormatEx);
    MS.Write(TempInt, SizeOf(DWORD));
    MS.Write(WaveFormatEx, SizeOf(TWaveFormatEx));
    MS.Write(DataId[1], Length(DataId));
    MS.Write(DataCount, SizeOf(DWORD));

    { Calculate and write tone signal }
    Omega:= 2 * Pi * Frequency;
    for i:= 0 to DataCount - 1 do
      begin
        SoundValue:= 127 + Trunc(Volume * Sin(i * Omega / SampleRate));
        MS.Write(SoundValue, 1);
      end;

    sndPlaySound(MS.Memory, SND_MEMORY or SND_SYNC);
  FINALLY
    FreeAndNil(MS);
  END;
end;








{ Frecv - Frequency in Hz
  Timp  - Duration in milliseconds
  The sound may not be heard if the duration is under ~35 ms. }
procedure Bip(Frecv, Timp: Integer);
begin
 WinApi.Windows.Beep(Frecv, Timp);
end;

{ Error sound - descending tones indicating failure }
procedure BipError;
begin
  WinApi.Windows.Beep(700, 70);
  Sleep(50);
  WinApi.Windows.Beep(300, 300);
end;

{ Confirmation sound - ascending tones indicating success }
procedure BipConfirmation;
begin
  WinApi.Windows.Beep(1100, 120);
  Sleep(10);
  WinApi.Windows.Beep(1900, 170);
end;

{ Short confirmation sound - quick ascending tones }
procedure BipConfirmationShort;
begin
  WinApi.Windows.Beep(1000, 55);
  Sleep(3);
  WinApi.Windows.Beep(1900, 135);
end;

{ Short error sound - quick descending tones }
procedure BipErrorShort;
begin
  WinApi.Windows.Beep(700, 50);
  Sleep(5);
  WinApi.Windows.Beep(400, 110);
end;

procedure Bip30;
begin
 WinApi.Windows.Beep(800, 30);
end;

procedure Bip50;
begin
 WinApi.Windows.Beep(800, 50);
end;

procedure Bip100;
begin
 WinApi.Windows.Beep(800, 100);
end;

procedure Bip300;
begin
 WinApi.Windows.Beep(800, 300);
end;

{ Fun coconut-style sound pattern }
procedure BipCoconuts;
begin
 Bip(1000, 30); Bip(1200, 40);
 Bip(890,  25); Bip(760,  40);
 Bip(1000, 30); Bip(1200, 40);
 Bip(890,  25); Bip(760,  40);
end;

{$ELSE}

{ Stubs for the platforms that are not Windows. Every routine is declared on every platform, so the call compiles everywhere; here it does nothing. }
procedure PlaySoundFile(CONST FileName: string);                  begin end;
procedure PlayResSound (CONST ResName: string; Async: Boolean= TRUE);  begin end;
procedure PlayTone(Frequency, Duration: Integer; Volume: Byte);   begin end;

procedure Bip(Frecv, Timp: integer);                              begin end;
procedure BipConfirmation;                                        begin end;
procedure BipConfirmationShort;                                   begin end;
procedure BipError;                                               begin end;
procedure BipErrorShort;                                          begin end;
procedure Bip30;                                                  begin end;
procedure Bip50;                                                  begin end;
procedure Bip100;                                                 begin end;
procedure Bip300;                                                 begin end;
procedure BipCoconuts;                                            begin end;

{$ENDIF}


end.
