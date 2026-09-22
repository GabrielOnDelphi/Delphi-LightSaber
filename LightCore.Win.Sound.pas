UNIT LightCore.Win.Sound;

{$IFNDEF MSWINDOWS}
  {$MESSAGE FATAL 'LightCore.Win.Sound is Windows-only. Its package LightCore.Win builds for Win32 and Win64 only.'}
{$ENDIF}

{=============================================================================================================
   2026.09.15
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Sound and audio utilities.

   Includes:
     - Windows system sound playback

   Windows-only unit, package LightCore.Win. PlayWinSound takes the name of a Windows system sound, a name space that exists only on Windows; the names it accepts are listed in this unit. A build for Android, macOS or iOS therefore stops at the top of this unit with a fatal compiler message. The other sound routines - WAV file, resource, tone and beeps - are in LightCore.Sound.
=============================================================================================================}

INTERFACE
USES
   Winapi.MMSystem;

{============================================================================================================
   SOUNDS
============================================================================================================}
 procedure PlayWinSound (CONST SystemSoundName: string);



IMPLEMENTATION


{ The system sound names this accepts are listed in the block below. }
procedure PlayWinSound(CONST SystemSoundName: string);
begin
 if SystemSoundName = ''
 then EXIT;

 Winapi.MMSystem.PlaySound(PChar(SystemSoundName), 0, SND_ASYNC);
end;


{ All the names below are registry values under HKEY_CURRENT_USER -> AppEvents -> Schemes -> Apps -> .Default.
  Which ones exist depends on the Windows version and on the installed applications.
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
    MENUPOPUP                - Pop-Up
    MAXIMIZE                 - Maximize
    MINIMIZE                 - Minimize
    MAILBEEP                 - New Mail
    OPEN                     - Open Application
    CLOSE                    - Close Application
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
    System Notification      - played when a system notification is displayed.  }


end.
