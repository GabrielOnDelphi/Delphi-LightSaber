UNIT LightCore.EnvironmentVar;

{=============================================================================================================
   SYSTEM - Environment Variables

   2026.09.15

   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  Lists the environment variables of the current process.

  Key features:
    - ListEnvironmentVars: Read all variables of the process environment block, one NAME=VALUE entry per line

   In this group:
     * LightVcl.Common.Shell.pas
     * LightVcl.Common.System.pas
     * LightVcl.Common.Window.pas
     * LightVcl.Common.WindowMetrics.pas
     * LightVcl.Common.ExecuteProc.pas
     * LightVcl.Common.ExecuteShell.pas

   ListEnvironmentVars returns a value and has a body only for Windows, so it is declared inside a MSWINDOWS conditional - interface and implementation both. Off Windows a FALSE or an empty list would read as "this process has no environment variables", which is a silent wrong answer. A call written in an Android, macOS or iOS build therefore fails to compile, and this unit compiles empty there. A POSIX body could read environ, declared in Posix.Unistd.

   The other three routines of this unit - ExpandEnvironmentStrings, SetEnvironmentVars and GetEnvironmentVars(Name, User) - moved to LightCore.Win.EnvironmentVar, a Windows-only unit of the package LightCore.Win. They read and write the environment keys of the Windows registry and expand the Windows %NAME% form.
=============================================================================================================}

INTERFACE
{$IFDEF MSWINDOWS}
USES
   Winapi.Windows, System.Classes,
   System.SysUtils;


{ Retrieves all environment variables from the current process environment block.
  Each entry is in NAME=VALUE format.
  Returns True if successful, False if GetEnvironmentStrings fails.
  TSL must not be nil. }
function ListEnvironmentVars(TSL: TStrings): Boolean;  overload;
{$ENDIF}



IMPLEMENTATION
{$IFDEF MSWINDOWS}


{---------------------------------------------------------------------------------------------------------------
   ListEnvironmentVars
---------------------------------------------------------------------------------------------------------------}
function ListEnvironmentVars(TSL: TStrings): Boolean;
VAR
  Vars: PChar;
  pS: PChar;
begin
  Assert(TSL <> NIL, 'ListEnvironmentVars: TSL parameter cannot be nil');

  TSL.BeginUpdate;
  TRY
    TSL.Clear;
    Vars:= WinApi.Windows.GetEnvironmentStrings;
    Result:= Vars <> NIL;
    if Result then
      TRY
        pS:= Vars;
        { Environment block: a sequence of null-terminated strings, ending with a double null. Format: VAR1=Value1#0VAR2=Value2#0#0 }
        while pS^ <> #0 do
          begin
            TSL.Add(pS);
            pS:= StrEnd(pS);  { Find end of current string }
            Inc(pS);          { Move past the null terminator to next string }
          end;
      FINALLY
        WinApi.Windows.FreeEnvironmentStrings(Vars);
      END;
  FINALLY
    TSL.EndUpdate;
  END;
end;

{$ENDIF}

end.
