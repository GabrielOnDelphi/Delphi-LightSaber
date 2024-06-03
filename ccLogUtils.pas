UNIT ccLogUtils;

{=============================================================================================================
   Gabriel Moraru
   2023.06
   See Copyright.txt

   A simple but effective visual log control/library.
   The programmer can send messages to the log that will be shown or not, depending on the chousen verbosity level of the log (see Verbosity property).
   **Visual/Non-visual**
   There is a non-visual log (TRamLog) and a visual log (TRichLog). The idea is that your non-visual objects can send data to the non-viusal log, for example in a batch job. At the end of the batch all collected messages can be shown in the visual log.
   Alternativelly, if you connect the TRamLog to the visual log, the messages are shown in real time, as the batch job progreeses.
   There is a pre-defined form that holds the log. To show it, call CreateLogForm in FormLog.pas
   The purpose is to have one single log window per application that will receive messages from the entire application.
   **Verbosity:**
     Supports several verbosity levels (verbose, info, warnings, errors, etc).
     Receives only messages that are above the specified verbosity threshold.
     For example, if the log is set to show only warnings and errors and you send a messages marked as "verbose", then the messages will not be shown.
     Each verbosity level has a predefined color.
   **I/O**
   The log can be save to disk to a binary file so it can be restored when the next app startup.
   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics;

TYPE
  TLogVerb= (lvVerbose, lvHints, lvInfos, lvImportant, lvWarnings, lvErrors);

CONST
  ctLogVerb  = clGray;
  ctLogHint  = clDkGray;
  ctLogInfo  = clBlack;
  ctLogImprt = TColor($5E005E); // Purple Dark
  ctLogWarn  = TColor($0058DF); // Orange Dark
  ctLogError = clRed;

CONST
  DefaultVerbosity= lvInfos;

function Verbosity2String(Verbosity: TLogVerb): string;



IMPLEMENTATION



function Verbosity2String(Verbosity: TLogVerb): string;
begin
 case Verbosity of
   lvVerbose   : Result := 'Verbose';
   lvHints     : Result := 'Hints';
   lvInfos     : Result := 'Info';      { This is the default level of verbosity }
   lvImportant : Result := 'Important';
   lvWarnings  : Result := 'Warnings';
   lvErrors    : Result := 'Errors';
 else
   Raise Exception.Create('Invalid verbosity');
 end;
end;


end.
