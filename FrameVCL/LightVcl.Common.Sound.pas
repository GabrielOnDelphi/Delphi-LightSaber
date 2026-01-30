UNIT LightVcl.Common.Sound;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Sound and audio utilities for VCL applications.

   Includes:
     - Windows system sound playback
     - WAV file playback
     - Resource-embedded sound playback
     - Programmatic tone generation
     - Various beep patterns for user feedback
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.MMSystem, System.SysUtils, System.Classes;

{============================================================================================================
   SOUNDS
============================================================================================================}
 procedure PlayWinSound (CONST SystemSoundName: string);
 procedure PlaySoundFile(CONST FileName: string);
 procedure PlayResSound (CONST ResName: String; uFlags: Integer);

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
USES
  LightCore, LightVcl.Common.Dialogs;


{ Plays a Windows system sound by name.
  See the comment above PlaySoundFile for available system sound names. }
procedure PlayWinSound(CONST SystemSoundName: string);
begin
 if SystemSoundName = ''
 then EXIT;

 Winapi.MMSystem.PlaySound(PChar(SystemSoundName), 0, SND_ASYNC);
end;


{ All available constants are defined in the registry under the path HKEY_CURRENT_USER -> AppEvents -> Schemes -> Apps -> .Default. Here, depending on the installed applications and your Windows version, you can surely find the one or another sound file and the associated constant.
  System sounds:
    SystemEXCLAMATION        - Note
    SystemHAND               - Critical Stop
    SystemQUESTION           - Question
    SystemSTART              - Windows-Start
    SystemEXIT               - Windows-Shutdown
    SystemASTERIX            - played when a popup alert is displayed, like a warning message.
    RESTOREUP                - Enlarge
    RESTOREDOWN              - Shrink
    MENUCOMMAND              - Menu
    MENUPOPUP                - Pop-Up)
    MAXIMIZE                 - Maximize)
    MINIMIZE                 - Minimize)
    MAILBEEP                 - New Mail)
    OPEN                     - Open Application)
    CLOSE                    - Close Application)
    AppGPFAULT               - Program Error
    Notification             - played when a default notification from a program or app is displayed.
    -----
    Calendar Reminder        - played when a Calendar event is taking place.
    Critical Battery Alarm   - played when your battery reaches its critical level.
    Critical Stop            - played when a fatal error occurs.
    Default Beep             - played for multiple reasons, depending on what you do. For example, it will play if you try to select a parent window before closing the active one.
    Desktop Mail Notif       - played when you receive a message in your desktop email client.
    Device Connect           - played when you connect a device to your computer. For example, when you insert a memory stick.
    Device Disconnect        - played when you disconnect a device from your computer.
    Device Connect Failed    - played when something happened with the device that you were trying to connect.
    Exclamation              - played when you try to do something that is not supported by Windows.
    Instant Message Notif    - played when you receive an instant message.
    Low Battery Alarm        - played when the battery is running low.
    Message Nudge            - played when you receive a BUZZ in an instant message.
    New Fax Notification     - played when you receive a fax via your fax-modem.
    New Mail Notification    - played when you receive an email message.
    New Text Message Notif   - played when you receive a text message.
    NFP Completion           - played when the transfer of data via NFC between your Windows device and another device is completed.
    NFP Connection           - played when your Windows device is connecting to another device via NFC.
    System Notification      - played when a system notification is displayed.

 Flags are:
    SND_SYNC  =0 = Start playing, and wait for the sound to finish
    SND_ASYNC =1 = Start playing, and don't wait to return
    SND_LOOP  =8 = Keep looping the sound until another sound is played  }

{ Plays a WAV file asynchronously. Does nothing if the file doesn't exist. }
procedure PlaySoundFile(CONST FileName: string);
begin
 if (FileName <> '') AND FileExists(FileName)
 then PlaySound(PChar(FileName), 0, SND_ASYNC or SND_FILENAME);
end;



{ Plays a sound embedded in application resources.

  How to embed a sound in a resource:
    1. Create file 'SOUNDS.RC' with:
         #define WAVE WAVEFILE
         SOUND1 WAVE "updating.wav"
    2. Compile: BRCC32.EXE -foSOUND32.RES SOUNDS.RC

  Note: UnlockResource and FreeResource are deprecated since Windows 95 and do nothing.
  Resources are automatically freed when the module is unloaded. }
procedure PlayResSound(CONST ResName: String; uFlags: Integer);
VAR
  hResInfo, hRes: THandle;
  lpGlob: PChar;
begin
 if ResName = ''
 then EXIT;

 hResInfo:= FindResource(HInstance, PChar(ResName), MAKEINTRESOURCE('WAVEFILE'));
 if hResInfo = 0 then
  begin
    MessageError('Could not find resource' + CRLFw + ResName);
    EXIT;
  end;

 hRes:= LoadResource(HInstance, hResInfo);
 if hRes = 0 then
  begin
    MessageError('Could not load resource' + CRLFw + ResName);
    EXIT;
  end;

 lpGlob:= LockResource(hRes);
 if lpGlob = NIL then
  begin
    MessageError('Bad resource' + CRLFw + ResName);
    EXIT;
  end;

 uFlags:= SND_MEMORY or uFlags;
 SndPlaySound(lpGlob, uFlags);
 { Note: UnlockResource/FreeResource are no-ops in 32-bit Windows and later }
end;





{ Generates and plays a pure sine wave tone.
  Parameters:
    Frequency - Tone frequency in Hz (max ~6600 Hz due to sample rate)
    Duration  - Duration in milliseconds
    Volume    - Volume level 0-127 (values > 127 are clamped) }
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
  RiffId: string = 'RIFF';
  WaveId: string = 'WAVE';
  FmtId: string = 'fmt ';
  DataId: string = 'data';
begin
  if Frequency <= 0
  then EXIT;
  if Duration <= 0
  then EXIT;

  if Volume > 127
  then Volume:= 127;

  if Frequency > (0.6 * SampleRate) then
  begin
    MessageWarning(Format('Sample rate of %d is too low to play a tone of %dHz', [SampleRate, Frequency]));
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








{ Simple wrapper for Windows.Beep.
  Frecv - Frequency in Hz
  Timp  - Duration in milliseconds
  Note: The sound may not be heard if duration is too short (< ~35 ms) }
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

{ Quick beep - 30ms at 800Hz }
procedure Bip30;
begin
 WinApi.Windows.Beep(800, 30);
end;

{ Quick beep - 50ms at 800Hz }
procedure Bip50;
begin
 WinApi.Windows.Beep(800, 50);
end;

{ Standard beep - 100ms at 800Hz }
procedure Bip100;
begin
 WinApi.Windows.Beep(800, 100);
end;

{ Long beep - 300ms at 800Hz }
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


end.
