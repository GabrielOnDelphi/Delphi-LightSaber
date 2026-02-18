UNIT LightVcl.Visual.RichLogUtils;

// OLD LOG based on RichEdit

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Utility functions and types for RichLog.
   Contains verbosity level definitions and color mappings.

   A simple but effective visual log control/library.
   The programmer can send messages to the log that will be shown or not, depending on the chosen verbosity level of the log (see Verbosity property).
   **Visual/Non-visual**
   There is a non-visual log (TRamLog) and a visual log (TRichLog). The idea is that your non-visual objects can send data to the non-visual log, for example in a batch job. At the end of the batch all collected messages can be shown in the visual log.
   Alternatively, if you connect the TRamLog to the visual log, the messages are shown in real time, as the batch job progresses.
   There is a pre-defined form that holds the log. To show it, call CreateLogForm in FormLog.pas
   The purpose is to have one single log window per application that will receive messages from the entire application.
   **Verbosity:**
     Supports several verbosity levels (verbose, info, warnings, errors, etc).
     Receives only messages that are above the specified verbosity threshold.
     For example, if the log is set to show only warnings and errors and you send a message marked as "verbose", then the message will not be shown.
     Each verbosity level has a predefined color.
   **I/O**
   The log can be saved to disk to a binary file so it can be restored when the next app starts up.
   Tester:
     c:\Projects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics, LightVcl.Common.Colors;

TYPE
  TLogVerb= (lvrVerbose, lvrHints, lvrInfos, lvrImportant, lvrWarnings, lvrErrors);

CONST
  ctLogVerb  = clGray;
  ctLogHint  = clDkGray;
  ctLogInfo  = clBlack;
  ctLogImprt = clPurpleDk;    // TColor($5E005E); 
  ctLogWarn  = clOrangeDark;  // TColor($0058DF);
  ctLogError = clRed;

CONST
  DefaultVerbosity= lvrInfos;

function Verbosity2String(Verbosity: TLogVerb): string;
function Verb2Color(Verb: TLogVerb; IsDark: Boolean = False): TColor;



IMPLEMENTATION


function Verbosity2String(Verbosity: TLogVerb): string;
begin
 case Verbosity of
   lvrVerbose   : Result := 'Verbose';
   lvrHints     : Result := 'Hints';
   lvrInfos     : Result := 'Info';      { This is the default level of verbosity }
   lvrImportant : Result := 'Important';
   lvrWarnings  : Result := 'Warnings';
   lvrErrors    : Result := 'Errors';
 else
   Raise Exception.Create('Invalid verbosity');
 end;
end;


{ Returns the display color for a given verbosity level.
  When IsDark is TRUE, returns lighter colors suitable for dark-background styles. }
function Verb2Color(Verb: TLogVerb; IsDark: Boolean): TColor;
begin
 if IsDark
 then
   case Verb of
     lvrVerbose  : Result:= clSilver;
     lvrHints    : Result:= TColor($B0B0B0);
     lvrInfos    : Result:= clWhite;
     lvrImportant: Result:= clPurpleFaded;
     lvrWarnings : Result:= clOrange;
     lvrErrors   : Result:= clRed;
   else
     raise Exception.Create('Verb2Color: Invalid verbosity');
   end
 else
   case Verb of
     lvrVerbose  : Result:= ctLogVerb;
     lvrHints    : Result:= ctLogHint;
     lvrInfos    : Result:= ctLogInfo;
     lvrImportant: Result:= ctLogImprt;
     lvrWarnings : Result:= ctLogWarn;
     lvrErrors   : Result:= ctLogError;
   else
     raise Exception.Create('Verb2Color: Invalid verbosity');
   end;
end;



end.
