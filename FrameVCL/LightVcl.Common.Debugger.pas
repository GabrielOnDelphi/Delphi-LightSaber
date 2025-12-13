UNIT LightVcl.Common.Debugger;

{=============================================================================================================
   2025.12
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Functions to:
      Output debug strings
      Generate reports about the hardware
      Generate errors (for testing)

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

 {}
 function LastErrorMsgStr: string;

 { DELPHI SPECIFIC }
 function FixEmbarcaderoAtomTableMemLeak: Boolean; Deprecated 'Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)'


IMPLEMENTATION
USES
   LightCore, LightCore.AppData, LightCore.Platform,  LightCore.Debugger,
   LightVcl.Common.Dialogs, LightVcl.Common.ExecuteProc, System.DateUtils;



procedure EmptyDummy(i: Integer);
begin
 MessageInfo(IntToStr(i));
end;



{ The AppDataPath parameter lets the user provide the data path in case the app is not using the Default data path }
function GenerateCompilerReport: string;
begin
 TAppDataCore.AppDataFolder(True);
 Result:= ' [COMPILER]'+ CRLF;
 Result:= Result+'  RunningUnderDelphi: '  + Tab + BoolToStrYesNo(IsRunningUnderDelphiDebugger)+ CRLF;
 Result:= Result+'  IsDebuggerPresent: '   + Tab + BoolToStrYesNo(IsDebuggerPresent)+ CRLF;
 Result:= Result+'  AppBitnessEx: '        + Tab + AppBitnessEx+ CRLF;
 Result:= Result+'  CompilerOptimiz: '     + Tab + CompilerOptimizationS+ CRLF;
 //Result:= Result+'  Version: '           + Tab + Tab + LightVcl.Visual.AppData.TAppData.GetVersionInfo+ CRLF;   //not available at this level!
end;




{--------------------------------------------------------------------------------------------------
   PROTECTION
   Also see c:\MyProjects\Packages\Third party packages\uDebugger.pas
--------------------------------------------------------------------------------------------------}


{ Relies on the IsDebuggerPresent API function in kernel32. But first it tests if the function is available. }
function IsDebuggerPresent: Boolean;
var
  DbgPresentFunc: function: Boolean; stdcall;
  KernelHandle: THandle;
  Ptr: Pointer;
begin
  KernelHandle := GetModuleHandle(kernel32);       // also see  IsDebuggerPresent: BOOL; stdcall; external 'kernel32.dll';
  @DbgPresentFunc := GetProcAddress(KernelHandle, 'IsDebuggerPresent');
  if Assigned(DbgPresentFunc)
  then Result := DbgPresentFunc
  else
   begin
     // We are bellow Windows 98
     Ptr := GetProcAddress(KernelHandle, 'GetProcAddress');
     Result:= LongWord(Ptr) < KernelHandle;
   end;
end;





TYPE
   EApplicationFail = class(Exception);

procedure HaltApplication(UserMessage : string);
begin
   Application.Terminate;
   Raise EApplicationFail.Create(UserMessage);
end;


procedure ExitIfUnderDebugger(ProjectFileName: string);
begin
 if IsDebuggerPresent                 // IsDebuggerPresent is external 'kernel32.dll';
 AND NOT FileExists(ProjectFileName)
 then Application.Terminate;
end;


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


procedure Antidebug; assembler;
asm
   jmp @jump;
   db $b8; // fake mov-instruction
    @fake1: jmp @ende;
  @endlos:
   int 3
   xor ax,ax
   jmp @endlos;

  @jump:
    jmp @fake1
  @ende:
end;





{--------------------------------------------------------------------------------------------------
   LOGGING
--------------------------------------------------------------------------------------------------}
procedure OutputDebugStr(s: string);
begin
 s:= s+ Tab;
 WinApi.Windows.OutputDebugString( PChar(s) );   { Poate fi folosita cu SysInternals DebugView }
end;


procedure OutputDebugStr(s: string; i: Integer);
begin
 WinApi.Windows.OutputDebugString( PChar(s+ IntToStr(i)+ Tab) );
end;


procedure OutputDebugStr(s: string; r: Real);
begin
 WinApi.Windows.OutputDebugString( PChar(s+ Real2Str(r, 3)+ Tab) );
end;




{--------------------------------------------------------------------------------------------------
   DEBUGER
--------------------------------------------------------------------------------------------------}

{ Call it after a routine API function/procedure/call, to see what that procedure returned }
function LastErrorMsgStr: String;
VAR szError: array [0..255] of Char;
begin
  Result:= '';
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError(), 0, szError, sizeof(szError), nil);
  ShowMessage(String(szError));
end;



{--------------------------------------------------------------------------------------------------
   Fixes this Delphi bug:
    EOsError-System Error. Code:_8. Not enough storage is available to process this command
    Details: https://stackoverflow.com/questions/507853/system-error-code-8-not-enough-storage-is-available-to-process-this-command

    Set LastLeakFix:= Now; at program start up.
--------------------------------------------------------------------------------------------------}

VAR
   LastLeakFix: TDateTime= 0;   { The time when the fix ran for the last time }

function FixEmbarcaderoAtomTableMemLeak: Boolean;  { Deprecated "Use in-program leak fixing: 3rdPartyPkg.AtomGarbageCollector.pas.GarbageCollectAtoms(Log)" }
begin
 if (System.DateUtils.MinutesBetween(now, LastLeakFix) > 15)
 AND FileExists(Appdatacore.AppSysDir+ 'AtomGarbageCollector.exe')
 then
  begin
   Result:= LightVcl.Common.ExecuteProc.ExecuteProc(Appdatacore.AppSysDir+ 'AtomGarbageCollector.exe', SW_HIDE);
   LastLeakFix:= now;
  end
 else Result:= TRUE;
end;



end.

