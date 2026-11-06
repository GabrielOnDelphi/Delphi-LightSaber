UNIT LightCore.LogTypes;

{=============================================================================================================
   www.GabrielMoraru.com
   2026.01
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

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
     For example, if the log is set to show only warnings and errors and you send a messages marked as "verbose", then the messages will not be shown.
     Each verbosity level has a predefined color.
   **I/O**
   The log can be saved to disk as a binary file so it can be restored on the next app startup.

   Tester:
     Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils;

TYPE
  TLogVerbLvl= (lvDebug, lvVerbose, lvHints, lvInfos, lvImportant, lvWarnings, lvErrors);

CONST
   DefaultVerbosity= lvInfos;   { Default verbosity threshold for new logs. }

function Verbosity2String(Verbosity: TLogVerbLvl): string;
function NormalizeLogBreaks(CONST Msg: string): string;


IMPLEMENTATION


{ Compiler-checked exhaustive mapping. If a future enum value (e.g. lvCritical) is
  added to TLogVerbLvl without extending this array, the compiler fails with
  E2072 ("Number of elements differs from declaration") — caught at build time
  rather than at runtime. The previous case-with-else version raised at runtime,
  potentially in a release build. }
const
  VerbosityNames: array[TLogVerbLvl] of string =
    ('Debug', 'Verbose', 'Hints', 'Info', 'Important', 'Warnings', 'Errors');

function Verbosity2String(Verbosity: TLogVerbLvl): string;
begin
  Result:= VerbosityNames[Verbosity];
end;



{ Turns a log message into the text a rich edit can take as it is.
    #0, #11 (vertical tab) and #12 (form feed) become spaces: a rich edit misbehaves on #0 ("EOutOfResources - RichEdit line insertion error") and draws #11 as a line break inside the paragraph.
    CRLF, a lone CR, a lone LF, U+2028 and U+2029 become one #13, the paragraph break of a rich edit, so one break is exactly one character.
    One trailing break is dropped, as the TStringList split of the old TRichLog did: AddMsg('Title' + CRLF) gives one line, not a line plus an empty row. }
function NormalizeLogBreaks(CONST Msg: string): string;
VAR
  i, n, Len: Integer;
  C: Char;
begin
  Len:= Length(Msg);
  SetLength(Result, Len);
  n:= 0;
  i:= 1;
  while i <= Len do
    begin
      C:= Msg[i];
      Inc(n);
      case C of
        #0, #11, #12:
          Result[n]:= ' ';
        #13:
          begin
            Result[n]:= #13;
            if (i < Len) AND (Msg[i+1] = #10)
            then Inc(i);   { CRLF is one break }
          end;
        #10, #$2028, #$2029:
          Result[n]:= #13;
      else
        Result[n]:= C;
      end;
      Inc(i);
    end;

  if (n > 0) AND (Result[n] = #13)
  then Dec(n);
  SetLength(Result, n);
end;



end.
