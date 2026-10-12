UNIT LightVcl.Internet.Common;

{-------------------------------------------------------------------------------------------------------------
   Gabriel Moraru
   2026.10.11
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Internet and URL utilities for VCL applications.

   Features:
     * URL validation with a message box (CheckURLStartMsg)
     * Internet connectivity test with a message box (TestProgramConnectionMsg)
     * URL shortcut file creation (CreateUrlOnDesktop)

   ParseURL, GetLocalIP, ResolveAddress, GenerateInternetRep, PCConnected2Internet, ProgramConnect2Internet(S) and IsPortOpened are in LightCore.Internet.pas.
   The Internet Explorer proxy, cache and session routines (IE_EnableProxy, IE_DisableProxy, IE_GetProxySettings, IE_DeleteCache, IE_EndSession, IE_SetProxy) are in LightCore.Win.Internet.pas.

   Related:
      Internet Status Detector: c:\Projects-3rd_Packages\Third party packages\_Out\_TEMPORARY EXCLUDED (Don't delete them yet!)\InternetStatusDetector.pas

   Tester:
      c:\Projects\LightSaber\Demo\Core\Demo Internet\
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
   Winapi.Windows,
   System.SysUtils, System.Win.Registry,
   LightCore,
   {LightCore.Internet,} LightVcl.Common.Dialogs;




{--------------------------------------------------------------------------------------------------
   URL VALIDATION
--------------------------------------------------------------------------------------------------}

 function  CheckURLStartMsg         (CONST URL: string): Boolean;                                 { Check if the URL starts with HTTP or with www }


 {--------------------------------------------------------------------------------------------------
   IS CONNECTED
--------------------------------------------------------------------------------------------------}
 function  TestProgramConnectionMsg(ShowMsgOnSuccess: Boolean= FALSE): Integer;   { The Msg suffix means: this one puts a modal box on screen. For a silent verdict call LightCore.Internet.ProgramConnect2Internet }


{--------------------------------------------------------------------------------------------------
   CREATE .URL FILES
--------------------------------------------------------------------------------------------------}
 Procedure CreateUrlOnDesktop (CONST ShortFileName, sFullURL: string);



IMPLEMENTATION

USES
   LightCore.IO,
   LightCore.Internet;




function CheckURLStartMsg(CONST URL: string): Boolean;
begin
 Result:= CheckURLStart(URL);

 if NOT Result
 then MessageWarning('Invalid URL:'+ CRLFw+ URL);
end;






{ Shows a message based on the connection test result.
  The Msg suffix is the library convention for "this routine puts a modal box on screen", so it must never be called from a thread, a service or a batch.
  ProgramConnect2Internet returns the same verdict silently. }
function TestProgramConnectionMsg(ShowMsgOnSuccess: Boolean= FALSE): Integer;
begin
 Result:= ProgramConnect2Internet;
 case Result of
  -1: MessageWarning(ComputerCannotAccessInet);
   0: MessageError(CheckYourFirewallMsg);
  +1: if ShowMsgOnSuccess
      then MessageInfo('Successfully connected to Internet');
 end;
end;












{==================================================================================================
   .URL
==================================================================================================}


Procedure CreateUrlOnDesktop(CONST ShortFileName, sFullURL: string);                               { create a URL file - The filename should end in .URL }
VAR sDesktop: string;
    MyReg   : TRegIniFile;
begin
 { Get desktop folder }
 MyReg:= TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
 TRY
   sDesktop:= MyReg.ReadString('Shell Folders','Desktop','');
 FINALLY
   FreeAndNil(MyReg);
 END;

 { Write URL file }
 CreateUrl(Trail(sDesktop)+ ShortFileName, sFullURL);
end;



end.
