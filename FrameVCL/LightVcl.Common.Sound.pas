UNIT LightVcl.Common.Sound;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Beeps AND noises
=============================================================================================================}

INTERFACE
USES
   Winapi.Windows, Winapi.MMSystem, System.AnsiStrings, System.SysUtils,
   System.Classes, System.Types;

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


procedure PlayWinSound(CONST SystemSoundName: string);
begin
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
procedure PlaySoundFile(CONST FileName: string);
begin
 if FileExists(FileName)
 then PlaySound(pchar(FileName), 0, SND_ASYNC or SND_FILENAME);    { Also exists sndPlaySound but it is obsolete! } { Why 0 for the second parameter: hmod:  Handle to the Executeble file that contains the resource to be loaded. This parameter must be NULL unless SND_RESOURCE is specified in fdwSound. }
end;



{How to load a PlaySoundFile in a resource:
   FileName: 'SOUNDS.RC'
   Body    : #define WAVE WAVEFILE
             SOUND1 WAVE "updating.wav"
   Compiler: BRCC32.EXE -foSOUND32.RES SOUNDS.RC    }
procedure PlayResSound(CONST ResName: String; uFlags: Integer);
VAR hResInfo,hRes: Thandle;
    lpGlob: Pchar;
Begin
 hResInfo:= FindResource(HInstance,PChar(RESName),MAKEINTRESOURCE('WAVEFILE'));
 if hResInfo = 0 then
  begin
    MessageError('Could not find resource'+ CRLFw+ RESName);
    EXIT;
  end;

 hRes:=LoadResource(HInstance,hResinfo);
 if hRes = 0 then
  begin
    MessageError('Could not load resource'+ CRLFw+ RESName);
    EXIT;
  end;

 lpGlob:=LockResource(hRes);
 if lpGlob=Nil then
  begin
    MessageError('Bad resource'+ CRLFw+ RESName);
    EXIT;
  end;

 uFlags:= snd_Memory or uFlags;
 SndPlaySound(lpGlob,uFlags);
 UnlockResource(hRes);
 FreeResource(hRes);
End;





{ Writes tone to memory and plays it.   Hz/mSec }
procedure PlayTone(Frequency, Duration: Integer; Volume: Byte);
VAR
  WaveFormatEx: TWaveFormatEx;
  MS: TMemoryStream;
  i, TempInt, DataCount, RiffCount: integer;
  SoundValue: byte;
  w: double;   // omega ( 2 * pi * frequency)
CONST
  Mono: Word = $0001;
  SampleRate: Integer = 11025; // 8000, 11025, 22050, or 44100
  RiffId: string = 'RIFF';
  WaveId: string = 'WAVE';
  FmtId: string = 'fmt ';
  DataId: string = 'data';
begin
  if Volume> 127
  then Volume:= 127;

  if Frequency > (0.6 * SampleRate) then
  begin
    MessageWarning(Format('Sample rate of %d is too low to play a tone of %dHz', [SampleRate, Frequency]));
    EXIT;
  end;

  with WaveFormatEx do
  begin
    wFormatTag     := WAVE_FORMAT_PCM;
    nChannels      := Mono;
    nSamplesPerSec := SampleRate;
    wBitsPerSample := $0008;
    nBlockAlign    := (nChannels * wBitsPerSample) div 8;
    nAvgBytesPerSec:= nSamplesPerSec * nBlockAlign;
    cbSize         := 0;
  end;

  MS:= TMemoryStream.Create;
  TRY
    with MS do
    begin
      {Calculate length of sound data and of file data}
      DataCount := (Duration * SampleRate) div 1000;                              // sound data
      RiffCount := Length(WaveId)+ Length(FmtId) + SizeOf(DWORD)+
            SizeOf(TWaveFormatEx)+ Length(DataId)+ SizeOf(DWORD)+ DataCount;      // file data
      {write out the wave header}
      Write(RiffId[1], 4);                                                        // 'RIFF'
      Write(RiffCount, SizeOf(DWORD));                                            // file data size
      Write(WaveId[1], Length(WaveId));                                           // 'WAVE'
      Write(FmtId [1], Length(FmtId));                                            // 'fmt '
      TempInt := SizeOf(TWaveFormatEx);
      Write(TempInt, SizeOf(DWORD));                                              // TWaveFormat data size
      Write(WaveFormatEx, SizeOf(TWaveFormatEx));                                 // WaveFormatEx record
      Write(DataId[1], Length(DataId));                                           // 'data'
      Write(DataCount, SizeOf(DWORD));                                            // sound data size
      {calculate and write out the tone signal} // now the data values
      w := 2 * Pi * Frequency;                                                    // omega
      for i := 0 to DataCount - 1 do
       begin
         SoundValue := 127 + trunc(Volume * sin(i * w / SampleRate));              // wt = w * i / SampleRate
         Write(SoundValue, 1);
       end;
      sndPlaySound(MS.Memory, SND_MEMORY or SND_SYNC); {now play the sound}
    end;
  FINALLY
    FreeAndNil(MS);
  END;
end;








// Note! The sound is not heard if the time is too short (like 35 ms)
procedure Bip(Frecv, Timp: integer);
begin
 WinApi.Windows.Beep(Frecv, Timp);
end;

procedure BipError;
begin
  WinApi.Windows.Beep(700, 70);
  Sleep(50);
  WinApi.Windows.Beep(300, 300);
end;

procedure BipConfirmation;
begin
  WinApi.Windows.Beep(1100, 120);
  Sleep(10);
  WinApi.Windows.Beep(1900, 170);
end;

procedure BipConfirmationShort;
begin
  WinApi.Windows.Beep(1000, 55);
  Sleep(3);
  WinApi.Windows.Beep(1900, 135);
end;

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

procedure BipCoconuts;
begin
 bip(1000, 30); bip(1200, 40);
 bip(890 , 25); bip(760 , 40);
 bip(1000, 30); bip(1200, 40);
 bip(890 , 25); bip(760 , 40);
end;


end.
