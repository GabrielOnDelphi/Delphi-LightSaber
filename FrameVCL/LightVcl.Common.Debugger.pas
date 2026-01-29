UNIT LightVcl.Common.Debugger;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Debugging and anti-debugging utilities:
     - Output debug strings (viewable in SysInternals DebugView)
     - Debugger detection (IsDebuggerPresent)
     - Anti-debugging protection routines (32-bit assembly)
     - Windows API error message retrieval

   Tester: c:\Projects\LightSaber\Demo\VCL\Demo SystemReport\VCL_Demo_SystemReport.dpr
   Also see: QuickImageFX\Quick.Chrono.pas

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows,
   System.SysUtils, Vcl.Forms, Vcl.Dialogs;

 { ANTI-DEBUGGER PROTECTION }
 procedure AntiDebug; assembler;
 procedure AntiProcDump; assembler;
 procedure ExitIfUnderDebugger (ProjectFileName: string);
 procedure HaltApplication(UserMessage : string);

 function  IsDebuggerPresent: Boolean;


 { LOGGING }
 procedure OutputDebugStr (s: string);             overload;
 procedure OutputDebugStr (s: string; i: Integer); overload;
 procedure OutputDebugStr (s: string; r: Real);    overload;

 { ERROR HANDLING }
 function LastErrorMsgStr: string;

 { DELPHI SPECIFIC }
 function FixEmbarcaderoAtomTableMemLeak: Boolean; Deprecated 'Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)'


IMPLEMENTATION

USES
   LightCore, LightCore.AppData, LightCore.Platform, LightCore.Debugger,
   LightVcl.Common.Dialogs, LightVcl.Common.ExecuteProc, System.DateUtils;




{--------------------------------------------------------------------------------------------------
   PROTECTION
   Also see c:\MyProjects\Packages\Third party packages\uDebugger.pas
--------------------------------------------------------------------------------------------------}


{--------------------------------------------------------------------------------------------------
   Detects if the application is being run under a debugger.
   Uses the IsDebuggerPresent API function from kernel32.dll.
   Falls back to a heuristic check for Windows versions before Windows 98.
--------------------------------------------------------------------------------------------------}
function IsDebuggerPresent: Boolean;
VAR
  DbgPresentFunc: function: Boolean; stdcall;
  KernelHandle: THandle;
  Ptr: Pointer;
begin
  KernelHandle:= GetModuleHandle(kernel32);
  @DbgPresentFunc:= GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if Assigned(DbgPresentFunc)
  then Result:= DbgPresentFunc
  else
    begin
      { Fallback for Windows versions below Windows 98 }
      Ptr:= GetProcAddress(KernelHandle, 'GetProcAddress');
      Result:= LongWord(Ptr) < KernelHandle;
    end;
end;





TYPE
  { Custom exception class for application termination }
  EApplicationFail = class(Exception);


{--------------------------------------------------------------------------------------------------
   Terminates the application and raises an EApplicationFail exception.
   Use this for critical errors that require immediate shutdown.
   The exception message will be displayed to the user.
--------------------------------------------------------------------------------------------------}
procedure HaltApplication(UserMessage: string);
begin
  Application.Terminate;
  raise EApplicationFail.Create(UserMessage);
end;


{--------------------------------------------------------------------------------------------------
   Terminates the application if:
     1. A debugger is detected, AND
     2. The specified project file does NOT exist

   Use this to prevent running a release build under a debugger,
   while allowing development runs (where the project file exists).

   Parameters:
     ProjectFileName - Full path to the .dpr or .dproj file.
--------------------------------------------------------------------------------------------------}
procedure ExitIfUnderDebugger(ProjectFileName: string);
begin
  if IsDebuggerPresent AND NOT FileExists(ProjectFileName)
  then Application.Terminate;
end;


{--------------------------------------------------------------------------------------------------
   Anti-process dump protection (32-bit x86 assembly).
   Attempts to confuse process dumping tools by modifying the reported image size
   in the PE header. Works differently on Windows 9x vs NT-based systems.

   WARNING: This is 32-bit assembly code. It will NOT work on 64-bit applications.
   Note: This is a basic protection technique that can be bypassed by skilled attackers.
--------------------------------------------------------------------------------------------------}
procedure AntiProcDump; assembler;
asm
    MOV EAX, fs:[30h]
    TEST EAX, EAX
    JS @is9x

 @isNT:
    MOV EAX, [EAX+0Ch]
    MOV EAX, [EAX+0Ch]
    ADD DWORD PTR [EAX+20h], 2000h                                                             {increase size variable}
    JMP @finished

 @is9x:
    PUSH 0
    CALL GetModuleHandleA
    TEST EDX, EDX
    JNS @finished                                                                              {Most probably incompatible!!!}
    CMP DWORD PTR [EDX+8], -1
    JNE @finished                                                                              {Most probably incompatible!!!}
    MOV EDX, [EDX+4]                                                                           {get address of internaly used}
                                                                                               {PE header}
    ADD DWORD PTR [EDX+50h], 2000h                                                             {increase size variable}

 @finished:
end;


{--------------------------------------------------------------------------------------------------
   Anti-debugger protection (32-bit x86 assembly).
   Uses obfuscated jump instructions to confuse debuggers and disassemblers.
   The fake mov instruction and jump sequence creates an opaque predicate.

   WARNING: This is 32-bit assembly code. It will NOT work on 64-bit applications.
   Note: This is a basic protection technique that can be bypassed by skilled attackers.
--------------------------------------------------------------------------------------------------}
procedure AntiDebug; assembler;
asm
   jmp @jump
   db $b8                { Fake mov-instruction to confuse disassemblers }
  @fake1:
   jmp @ende
  @endlos:
   int 3                 { Breakpoint interrupt }
   xor ax, ax
   jmp @endlos
  @jump:
   jmp @fake1
  @ende:
end;





{--------------------------------------------------------------------------------------------------
   DEBUG OUTPUT
   These functions send debug strings to the Windows debug output.
   Use SysInternals DebugView to capture these messages when not running under a debugger.
--------------------------------------------------------------------------------------------------}

{ Outputs a debug string. Appends a Tab character for formatting. }
procedure OutputDebugStr(s: string);
begin
 s:= s + Tab;
 WinApi.Windows.OutputDebugString(PChar(s));
end;


{ Outputs a debug string with an integer value appended. }
procedure OutputDebugStr(s: string; i: Integer);
begin
 WinApi.Windows.OutputDebugString(PChar(s + IntToStr(i) + Tab));
end;


{ Outputs a debug string with a real value appended (3 decimal places). }
procedure OutputDebugStr(s: string; r: Real);
begin
 WinApi.Windows.OutputDebugString(PChar(s + Real2Str(r, 3) + Tab));
end;




{--------------------------------------------------------------------------------------------------
   WINDOWS ERROR MESSAGES
--------------------------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------------------------
   Returns the last Windows API error message as a string.
   Call this immediately after a Windows API function fails (returns False or INVALID_HANDLE_VALUE).
   Uses GetLastError() and FormatMessage() to retrieve the system error description.

   Note: GetLastError() value is only valid immediately after the failing API call.
         Subsequent API calls may overwrite it.
--------------------------------------------------------------------------------------------------}
function LastErrorMsgStr: string;
VAR
  szError: array[0..255] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NIL, GetLastError(), 0, szError, SizeOf(szError), NIL);
  Result:= string(szError);
end;



{--------------------------------------------------------------------------------------------------
   DEPRECATED: Atom Table Memory Leak Fix

   Workaround for Delphi/VCL atom table memory leak bug:
     EOsError - System Error. Code: 8. Not enough storage is available to process this command
     See: https://stackoverflow.com/questions/507853

   This function runs an external cleanup utility (AtomGarbageCollector.exe) periodically.
   It will only run if 15+ minutes have passed since the last cleanup.

   IMPORTANT: Use the in-program solution instead:
     3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)

   Note: Uses a unit-level variable for timing (deviation from coding convention).
         This is acceptable since the function is deprecated.
--------------------------------------------------------------------------------------------------}
VAR
  LastLeakFix: TDateTime = 0;  { Timestamp of last cleanup run }

function FixEmbarcaderoAtomTableMemLeak: Boolean;
begin
  if (System.DateUtils.MinutesBetween(Now, LastLeakFix) > 15)
  AND FileExists(AppDataCore.AppSysDir + 'AtomGarbageCollector.exe')
  then
    begin
      Result:= LightVcl.Common.ExecuteProc.ExecuteProc(AppDataCore.AppSysDir + 'AtomGarbageCollector.exe', SW_HIDE);
      LastLeakFix:= Now;
    end
  else
    Result:= TRUE;
end;



end.

