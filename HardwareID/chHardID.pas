UNIT chHardID;

//ToDo: move this to proteus

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   CubicDesign 2013
   Closed source. Do not distribute
   www.Soft.Tahionic.com

 Programming languages supported:
   Delphi XE-Delphi11.2


 Also see
    * csSystem.pas

 Get the harddisk serial number 2006
      http://www.delphitricks.com/source-code/systeminfo/get_the_harddisk_serial_number.html

 Retrieve CPU information 2006
      http://www.delphitricks.com/source-code/systeminfo/retrieve_cpu_information.html

=============================================================================================================}


{
Suggestion: a couple of the calls generate messages if the input is unacceptable (such as GetCPUID(BYTE)). It would be great if the messages could be suppressed, so that an error could be flagged without requiring user interaction to "OK" the message.
For example: GetCPUCount() returns the logical number of cores. However, on my machine there are two logical to one physical. So using this count to iterate through GetCPUID() generates a message on the fifth call: which would be nice to suppress.
}



{ToDO:
GetMemoryManagerState and GetMemoryMap

for I := Low(AMemoryMap) to High(AMemoryMap) do
begin
  case AMemoryMap[I] of
    csUnallocated: ...
    csAllocated: ...
    csReserved: ...
    csSysAllocated: ...
    csSysReserved: ...
  end;
end;
}


INTERFACE

USES
   WinApi.Windows, System.SysUtils, VCL.Forms, System.Classes, System.Win.Registry, Winapi.PsAPI;    { 'Dialogs.pas' brings about 310KB to the size of this DLL. Better not use it. }

CONST
   ctLibName= 'Hardware ID Extractor DLL';

   { OSMemType }
   mTotalPageFile = 4;                                                                             { Total page file in (bytes)          }
   mAvailPageFile = 5;                                                                             { Available page file (bytes)         }
   mTotalVirtual  = 6;                                                                             { Total virtual memory in bytes       }
   mAvailVirtual  = 7;                                                                             { Available virtual memory (bytes)    }

   { ProcMemType }
   pWorkingSetSize          = 1;                                                                   {- the current working set size, in bytes. }
   pPageFaultCount          = 2;                                                                   {- the number of page faults.}
   pPeakWorkingSetSize      = 3;                                                                   {- the peak working set size, in bytes.}
   pQuotaPeakPagedPoolUsage = 4;                                                                   {- The peak paged pool usage, in bytes.}
   pQuotaPagedPoolUsage     = 5;                                                                   {- The current paged pool usage, in bytes.}
   pQuotaPeakNonPagedPool   = 6;                                                                   {- The peak nonpaged pool usage, in bytes.}
   pQuotaNonPagedPoolUsg    = 7;                                                                   {- The current nonpaged pool usage, in bytes.}
   pPageFileUsage           = 8;                                                                   {- The current space allocated for the pagefile, in bytes. Those pages may or may not be in memory.}
   pPeakPagefileUsage       = 9;                                                                   {- The peak space allocated for the pagefile, in bytes.}

   TB = 1099511627776;
   GB = 1073741824;
   MB = 1048576;                                                                                   { 1024*1024 = 1048576 }
   KB = 1024;


 { CPU }
 {$IFDEF Win32}
 function GetCPUVendor        : String;                                                            { New GetCPUVendor function. Reported to work with D7 and D2009. }
 function GetCPUSpeed         (CONST SomeDelay: Integer= 50): Double;                              { The higher the delay, the accurate the result. Default= 200ms }
 function GetCPUID            (CoreMask: Word): String;                                            { Get the ID of the specified logical CPU }
 function GetCpuIDNow         : String;                                                            { Get the ID of the first available logical CPU }
 function IsCPUIDAvailable    : Boolean;
 {$ENDIF}
 function CPUFamily           : String;                                                            { Get cpu identifier from the windows registry }
 function IsIntel64BitCPU     : Boolean;                                                           { Detects IA64 processors }
 function GetCpuTheoreticSpeed: Integer;                                                           { Get cpu speed (in MHz) }

 function GetCPUCount         : Integer;                                                           { The number of LOGICAL processors in the current group }
 function GetCoreCount        : Integer;   { Specifies the number of CPU cores in the system }
 function IsSingleProcessor   : Boolean;   { Specifies whether the system has a single processor or not }

 { SYSTEM RAM }
 function SystemMemStatus    (CONST OSMemType: Byte): NativeUInt;                                  { Obsolete! in Bytes. Limited by the capacity of the OS (32bits OSs will report max 2GB) }
 function SystemMemStatus_KB (CONST OSMemType: Byte): string;
 function SystemMemStatus_MB (CONST OSMemType: Byte): string;

 { SYSTEM RAM NEW! }
 function RAMInstalledS: string;                                                                   { NEW. Replaces chHardID.SystemMemStatus_GB (mTotalPhys) }
 function RAMInstalled: UInt64;
 function RAMLoad: Integer;                                                                        { Memory load (in %) }
 function RAMFreeS: string;                                                                        { Returns free amount of free RAM. The size (MB, GB) is dynamically chousen }
 function RAMFree: Cardinal;                                                                       { Returns free amount of free RAM in MB }

 { PROCESS RAM }
 function ProcessMemStatus(CONST ProcMemType: Byte= 1): NativeUInt;                                { Returns data about the memory used of the current process }
 function ProcessPeakMem: string;                                                                  { Shows the highest amount of memory this program ever occupied }
 function ProcessCurrentMemS: string;
 function ProcessCurrentMem: NativeUInt;

 { VIRTUAL RAM }
 function GetPageSize: NativeUInt;                                                                 { The page size and the granularity of page protection and commitment. This is the page size used by the VirtualAlloc function. }
 function GetMemGranularity: Integer;                                                              { Granularity with which virtual memory is allocated (in KB) }

 { RAM - Advanced stuff }
 function  GetLowAddr: NativeUInt;                                                                 { Lowest RAM memory address accessible to applications (this is the RAM address, not virtual memory address) }
 function  GetHiAddr: NativeUInt;                                                                  { Lowest RAM memory address accessible to applications }
 procedure TrimWorkingSet;                                                                         { Minimizes the amount to RAM used by application by swapping the unused pages back to disk - Details: http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393 }

 { HDD }
 function GetPartitionID    (Partition  : string): String;                                         { Get the ID of the specified patition. Example of parameter: 'C:' }
 function GetIDESerialNumber(DriveNumber: Byte): AnsiString;                                       { DriveNr is from 0 to 4 }

 { BIOS (NEW!) }
 function BiosDate: string;
 function BiosVersion: string;                                                                     { Could be something like: TOSQCI - 6040000 Ver 1.00PARTTBL. TOS is comming from Toshiba Q is comming from product series (Qosmio) }
 function BiosProductID: string;                                                                   { Manufacturer product (laptop, PC) ID - Could be something like: Toshiba_PQX33U-01G00H }
 function BiosVideo: string;

 { UTILS }
  procedure Mes         (CONST sMessage: string);                                                  { Lightweight replacement for Dialogs.Pas }
 function  FormatBytes  (CONST Size: Int64; CONST Decimals: Integer): string;                      { Format bytes to KB, MB, GB, TB }
 function  BinToInt     (Value: String): Integer;
 function  IntToBin     (CONST Value, Digits: integer): String;
 function  CoreNumber2CoreMask(CoreNo: integer): integer;
 procedure ChangeByteOrder(VAR Data; Size : Integer);
 function  GetDllVersion: Real;

 { REPORTS }
 function GenerateRamRepOS: string;
 function GenerateRamRepProc: string;
 function GenerateHardwareReport: string;                                                         { Before calling this I need to enter a valid key into chHardID:    }


 { INTERNAL }
 { !!! Don't export these functions. They are for internal use. }
 function  EnterKey(Value: integer): Boolean;
 {$IFDEF Win32}
 function _isCPUIDAvailable: Boolean; Register;
 function GetCPUVendor2: string;
 function GetCPUVendor3: AnsiString;
 {$ENDIF}




{These flags require Delphi 2006 and up}
{$SetPEFlags $20}                                                                                  { To enable support for a user mode address space greater than 2GB you will have to use the EditBin* tool to set the LARGE_ADDRESS_AWARE flag in the EXE header. This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the application supports an address space larger than 2GB (up to 4GB). In Delphi 6 and later you can also specify this flag through the compiler directive {$SetPEFlags $20     *The EditBin tool ships with the MS Visual C compiler. }
{.$SetPEFlags $01}  { DO NOT USE THIS. IT WILL FUCK UP THE CODE and Delphi wil crash when it will load this library }    { Compiler directive feature that allows you to turn off the relocation table for EXE files. }

VAR
  HDIDValid: Boolean= FALSE;


IMPLEMENTATION

USES
   Math;

CONST
  Desc1 = 22121989;                                                                                { v1.5 key }
  Desc2 = 221219892;                                                                               { v2.0 key }
  Tab   = #9;
  CRLF  = #13#10;
  LBRK  = CRLF+CRLF;




{--------------------------------------------------------------------------------------------------
   PRIVATE FUNCTIONS
--------------------------------------------------------------------------------------------------}
function EnterKey(Value: integer): Boolean;
begin
 WinApi.Windows.Sleep(20);
 Result:= Value= Desc2;
 HDIDValid:= Result;
end;


procedure CheckValidKey;
begin
 if NOT HDIDValid
 AND (Random(15)=1)
 then Mes('This is a demo version of '+ ctLibName);
end;


procedure Mes(CONST sMessage: string);                                                                   { Lightweight replacement for Dialogs.Pas }
begin
 {$IFDEF UNICODE}
 Application.MessageBox(PWideChar(sMessage), 'Info:', MB_OK);
 {$ELSE}
 Application.MessageBox(PAnsiChar(sMessage), 'Info:', MB_OK);
 {$ENDIF}
end;




{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}
function GetDllVersion: Real;
begin
 Result:= 2.0;
end;


procedure ChangeByteOrder(VAR Data; Size : Integer);
VAR ptr : PAnsiChar;
    i : Integer;
    c : AnsiChar;
begin
 ptr := @Data;
 for i := 0 to (Size shr 1)-1 do
 begin
   c := ptr^;
   ptr^ := (ptr+1)^;
   (ptr+1)^ := c;
   Inc(ptr, 2);
 end;
end;


function Real2Str (CONST ExtValue: Extended; HowManyDecimasl: byte): string;
VAR s: string;
begin
 Assert(NOT Math.IsNaN(ExtValue), 'Float number is NAN!');
 s:= FloatToStr(ExtValue);
 s:= StringReplace(s, ',', '.', [rfReplaceAll]);
 if Pos('.', s) < 1
 then Result:= s
 else Result:= Copy(s, 1, Pos('.', s)+ HowManyDecimasl);
end;


function BinToInt(Value: string): Integer;
var
  i, iValueSize: Integer;
begin
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
      if Value[i] = '1'
      then Result := Result + (1 shl (iValueSize - i));
end;


function IntToBin(CONST Value, Digits: integer): String;
begin
 if digits= 0
 then Result:= ''
 else
  begin
   if  (value AND (1 shl (digits-1)))>0
   then Result:= '1'+ IntToBin(value, digits-1)
   else Result:= '0'+ IntToBin(value, digits-1);
  end;
end;


{This is duplicate of ccCore.FormatBytes}
function FormatBytes(CONST Size: Int64; CONST Decimals: Integer): string;                          { Formats the size of a file from bytes to KB, MB, GB, TB } { Old name was: FormatFileSize }
begin
 if Size = 0
 then Result:= '0 Bytes' else

 if Size< KB
 then Result:= IntToStr(Size)+ ' bytes' else

 if (Size>= KB) AND (Size< MB)
 then Result:= Real2Str(Size / KB, Decimals)+ ' KB' else

 if (Size>= MB) AND (Size< GB)
 then Result:= Real2Str(Size / MB, Decimals)+ ' MB' else

 if (Size>= GB) AND (Size< TB)
 then Result:= Real2Str(Size / GB, Decimals)+ ' GB'
 else Result:= ' Huge size!';
end;


{ Returns a bit mask. CoreNo is indexed in 1!  To be used with GetCPUID.
  The mask is a bitfield: each bit designate a processor. 0 means "no processor". It's not logic.
    0x0001 : proc 1
    0x0003 : proc 1 and 2
    0x000F : proc 1, 2, 3, 4
}
function CoreNumber2CoreMask(CoreNo: integer): integer;
VAR sBinary: string;
    i: integer;
begin
 Assert(CoreNo > 0, 'CoreNumber2CoreMask.CoreNo must be higher than 0!');

 sBinary:= '';
 for i:= getCpuCount downto 1 do
   if i= CoreNo
   then sBinary:= sBinary+ '1'
   else sBinary:= sBinary+ '0';
 Result:= BinToInt(PChar(sBinary));                                                                { Under D7 this should be PAnsiChar }
end;



function _RegReadMultiSzString(hRootKey: HKEY; CONST KeyPath, KeyName: string; CanCreate: Boolean= false): string;  { This will return strings separated by ENTER }
VAR
  keyGood: boolean;
  p: integer;
  regKey: TRegistry;
  vSize: integer;
begin
  Result:= '';
  regKey:= TRegistry.Create(KEY_READ);
  TRY
    regKey.RootKey:= hRootKey;
    keyGood := regKey.OpenKey(KeyPath, CanCreate);

    if (keyGood) then
    begin
      vSize := regKey.GetDataSize(KeyName);

      if (vSize > 0) then
      begin
        SetLength(Result, vSize);
        regKey.ReadBinaryData(KeyName, Result[1], vSize);

        repeat
          p:= Pos(#0, Result);
          if p <> 0 then
           begin
            Delete(Result, p, 1);
            Insert(#13#10, Result, p);
           end;
        until p = 0;

      end;
    end;
  FINALLY
    FreeAndNil(regKey);
  END;
end;


function _RegReadMultiSzString2(hRootKey: HKEY; CONST KeyPath, KeyName: string; CanCreate: Boolean= false): string;    { This will return strings separated by SPACE }
begin
  Result:= _RegReadMultiSzString(hRootKey, KeyPath, KeyName, CanCreate);
  Result:= StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
end;




















{--------------------------------------------------------------------------------------------------
   RAM PAGE
--------------------------------------------------------------------------------------------------}
function GetPageSize: NativeUInt;                                                                    { The page size and the granularity of page protection and commitment. This is the page size used by the VirtualAlloc function. }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= iSystem.dwPageSize;
end;



function GetMemGranularity: Integer;                                                               { Granularity with which virtual memory is allocated (in KB) }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= Round(iSystem.dwAllocationGranularity/KB);
end;



function GetLowAddr: NativeUInt;                                                                     { Lowest RAM memory address accessible to applications (this is the RAM address, not virtual memory address) }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= NativeUInt(iSystem.lpMinimumApplicationAddress);
end;



function GetHiAddr: NativeUInt;                                                                   { Lowest RAM memory address accessible to applications }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= NativeUInt(iSystem.lpMaximumApplicationAddress);
end;


procedure TrimWorkingSet;                                                                         { Minimizes the amount to RAM used by application by swapping the unused pages back to disk - Details: http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393       and this:  http://17slon.com/gp/gp/dsiwin32.htm }
VAR hProcess: THandle;
begin
  hProcess := OpenProcess(PROCESS_SET_QUOTA, false, GetCurrentProcessId);
  TRY
    SetProcessWorkingSetSize(hProcess, $FFFFFFFF, $FFFFFFFF);
  FINALLY
    CloseHandle(hProcess);
  END;
end; { DSiTrimWorkingSet }





{--------------------------------------------------------------------------------------------------
   PROCESS MEM STATUS
   MODERN
   Uses "GlobalMemoryStatusEx" (introduced with Windows 2000) instead of the old GlobalMemoryStatus
--------------------------------------------------------------------------------------------------}

function RAMInstalledS: string;   { Returns installed RAM (as viewed by your program) in GB. On a PC with 4GB RAM and Win OS 32, the function will return 3GB!! This is 'normal'. }
begin
 Result:= Real2Str(RAMInstalled / GB, 2)+ ' GB';
end;


{ Returns installed RAM (as viewed by your program) in GB. On a PC with 4GB RAM and Win OS 32, the function will return 3GB!! This is 'normal'. }
function RAMInstalled: UInt64;
VAR MS_Ex : MemoryStatusEx;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength:= SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx (MS_Ex);
 Result:= MS_Ex.ullTotalPhys;
end;


function RAMFreeS: string;                                                                          { Returns free amount of free RAM. The size (MB, GB) is dynamically chousen }
VAR MS_Ex : MemoryStatusEx;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength:= SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx (MS_Ex);
 Result:= FormatBytes(MS_Ex.ullAvailPhys, 2);
end;


function RAMFree: Cardinal;                                                                          { Returns free amount of free RAM in MB }
VAR MS_Ex : MemoryStatusEx;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength:= SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx (MS_Ex);
 Result:= Round(MS_Ex.ullAvailPhys / MB);
end;


function RAMLoad: Integer;                                                                         { Memory load (in %) }
VAR MS_Ex : MemoryStatusEx;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength:= SizeOf(MemoryStatusEx);
 GlobalMemoryStatusEx (MS_Ex);
 Result:= MS_Ex.dwMemoryLoad; //)+ ' %';
end;





{--------------------------------------------------------------------------------------------------
   PROCESS MEM STATUS
   OBSOLETE
--------------------------------------------------------------------------------------------------}

{ In bytes }
function SystemMemStatus(CONST OSMemType: byte): NativeUInt;
VAR Status : TMemoryStatus;
begin                                                                                              { Before presenting the memory values, convert them into giga, mega or kilobytes. }
  Status.dwLength := sizeof( TMemoryStatus ) ;
  GlobalMemoryStatus( Status ) ;
  case OSMemType of
   //mMemoryLoad   : Result:= Status.dwMemoryLoad;                                               { Total memory used in percents (%)   }
   //mTotalPhys    : Result:= Status.dwTotalPhys;                                                { Total physical memory in bytes      }
   //mAvailPhys    : Result:= Status.dwAvailPhys;                                                { Available physical memory (bytes)   }
     mTotalPageFile: Result:= Status.dwTotalPageFile;                                              { Total page file in (bytes)          }
     mAvailPageFile: Result:= Status.dwAvailPageFile;                                              { Available page file (bytes)         }
     mTotalVirtual : Result:= Status.dwTotalVirtual;                                               { Total virtual memory in bytes       }
     mAvailVirtual : Result:= Status.dwAvailVirtual;                                               { Available virtual memory (bytes)    }
  else
    Result:= 0;
  end;
end;


{ Obsolete
  In KB. Limited by the capacity of the OS (32bits OSs will report max 2GB) }
function SystemMemStatus_KB (CONST OSMemType: Byte): String;
begin
 Result:= FloatToStr(SystemMemStatus(OSMemType) / KB)+ ' KB';
end;


{ Obsolete
  In MB. Limited by the capacity of the OS (32bits OSs will report max 2GB) }
function SystemMemStatus_MB (CONST OSMemType: Byte): String;                                       { in MB. Limited by the capacity of the OS (32bits OSs will report max 2GB) }
begin
 Result:= Real2Str(SystemMemStatus(OSMemType) / MB, 1)+ ' MB';
end;





{--------------------------------------------------------------------------------------------------
   PROCESS MEM STATUS
--------------------------------------------------------------------------------------------------}

{
 About memory in Task Manager:

  Task manager does not show the actual memory usage of the program, it shows
  you its "working set", which is not the most precise measure of the programs
  actual memory use, since it includes address ranges that have been only
  reserved but not yet committed.
  Task manager also has no idea whatsoever about the internal memory management done by the program it watches.
  Practically all languages are quite independent of the operating sytem they are trageted for,
  offer their own internal memory management to the programmer. These memory managers usually allocate large blocks
  from the OS and then subdivide these blocks as needed to fulfil the codes requests for memory allocations. The blocks
  do not get returned to the OS pool until every single byte of the block has been returned to the memory manager, so task
  manager will count the block as used even if only a single byte of it is still in use by the program.

  FastMM
    FastMM memory is netto usage of memory allocated through FastMM.
    This does not include at least these:
      FastMM overhead
      Windows overhead of blocks allocated by FastMM on your behalf
      Windows overhead of things not allocated by FastMM (like the space occupied by DLL's in your process space) for GUI apps: overhead of GDI, GDI+, DirectX, OpenGL and other storage for visual objects allocated on your behalf.

}
function ProcessMemStatus (CONST ProcMemType: Byte=1): NativeUInt;                                   { Returns data about the memory used of the current process }
VAR Status: Winapi.PsAPI.TProcessMemoryCounters;
begin
  Result:= 0;
  Status.cb:= SizeOf(Status);
  if GetProcessMemoryInfo(GetCurrentProcess, @Status, SizeOf(Status)) then
   case ProcMemType of
      pWorkingSetSize         : Result:= Status.WorkingSetSize;                           {- the current working set size, in bytes. }
      pPageFaultCount         : Result:= Status.PageFaultCount;                           {- the number of page faults.}
      pPeakWorkingSetSize     : Result:= Status.PeakWorkingSetSize;                       {- the peak working set size, in bytes.}
      pQuotaPeakPagedPoolUsage: Result:= Status.QuotaPeakPagedPoolUsage;                  {- The peak paged pool usage, in bytes.}
      pQuotaPagedPoolUsage    : Result:= Status.QuotaPagedPoolUsage;                      {- The current paged pool usage, in bytes.}
      pQuotaPeakNonPagedPool  : Result:= Status.QuotaPeakNonPagedPoolUsage;               {- The peak nonpaged pool usage, in bytes.}
      pQuotaNonPagedPoolUsg   : Result:= Status.QuotaNonPagedPoolUsage;                   {- The current nonpaged pool usage, in bytes.}
      pPagefileUsage          : Result:= Status.PagefileUsage;                            {- The current space allocated for the pagefile, in bytes. Those pages may or may not be in memory.}
      pPeakPagefileUsage      : Result:= Status.PeakPagefileUsage;                        {- The peak space allocated for the pagefile, in bytes.}
   else RaiseLastOSError;
   end; {END CASE}
end;


function ProcessPeakMem: string;                                                          { Shows the highest amount of memory this program ever occupied }
begin
  Result:= FormatBytes(ProcessMemStatus(pPeakWorkingSetSize), 2);
end;


function ProcessCurrentMemS: string;                                                      { Shows the amount of memory that the (current) program occupies now }
begin
  Result:= FormatBytes(ProcessMemStatus(pWorkingSetSize), 2);
end;


function ProcessCurrentMem: NativeUInt;                                                   { Shows the amount of memory that the (current) program occupies now }
begin
  Result:= ProcessMemStatus(pWorkingSetSize);
end;








{--------------------------------------------------------------------------------------------------
                                   IDE HDD
--------------------------------------------------------------------------------------------------}
function GetIdeSerialNumber(DriveNumber: byte): AnsiString;                                            { READ HARDWARE ID:  DriveNr is from 0 to 7 }
CONST IDENTIFY_BUFFER_SIZE = 512;
      IO_COMM_BUFF: AnsiString = #67#117#98#105#99#73#68;                                          { unde e DLL-ul folosit }

TYPE
 TIDERegs = packed record
   bFeaturesReg     : BYTE;                                                     // Used for specifying SMART "commands".
   bSectorCountReg  : BYTE;                                                     // IDE sector count register
   bSectorNumberReg : BYTE;                                                     // IDE sector number register
   bCylLowReg       : BYTE;                                                     // IDE low order cylinder value
   bCylHighReg      : BYTE;                                                     // IDE high order cylinder value
   bDriveHeadReg    : BYTE;                                                     // IDE drive/head register
   bCommandReg      : BYTE;                                                     // Actual IDE command.
   bReserved        : BYTE;                                                     // reserved for future use.  Must be zero.
 end;

 TSendCmdInParams = packed record
   cBufferSize  : DWORD;                                                        // Buffer size (in bytes)
   irDriveRegs  : TIDERegs;                                                     // Structure with drive register values.
   bDriveNumber : BYTE;                                                         // Physical drive number to send command to (0,1,2,3).
   bReserved    : Array[0..2] of Byte;
   dwReserved   : Array[0..3] of DWORD;
   bBuffer      : Array[0..0] of Byte;                                          // Out input buffer
 end;

 TIdSector = packed record
   wGenConfig, wNumCyls       : Word;
   wReserved, wNumHeads       : Word;
   wBytesPerTrack             : Word;
   wBytesPerSector            : Word;
   wSectorsPerTrack           : Word;
   wVendorUnique              : Array[0..2 ]  of Word;
   sSerialNumber              : Array[0..19]  of AnsiChar;
   sFirmwareRev               : Array[0..7 ]  of AnsiChar;
   sModelNumber               : Array[0..39]  of AnsiChar;
   bReserved                  : Array[0..127] of BYTE;
   wBufferType, wBufferSize   : Word;
   wECCSize                   : Word;
   wMoreVendorUnique          : Word;
   wDoubleWordIO              : Word;
   wCapabilities              : Word;
   wReserved1                 : Word;
   wPIOTiming, wDMATiming     : Word;
   wNumCurrentCyls            : Word;
   wNumCurrentHeads, wBS      : Word;
   wNumCurrentSectorsPerTrack : Word;
   ulCurrentSectorCapacity    : DWORD;
   wMultSectorStuff           : Word;
   ulTotalAddressableSectors  : DWORD;
   wSingleWordDMA             : Word;
   wMultiWordDMA              : Word;
 end;

 PIdSector = ^TIdSector;
 TDriverStatus = packed record
   bDriverError : Byte;                                                         // Error code from driver. It is 0 if no error.
   bIDEStatus   : Byte;                                                         // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
   bReserved    : Array[0..1] of Byte;
   dwReserved   : Array[0..1] of DWORD;
 end;

 TSendCmdOutParams = packed record
   cBufferSize  : DWORD;                                                        // Size of bBuffer (in bytes)
   DriverStatus : TDriverStatus;                                                // Driver status structure.
   bBuffer      : Array[0..0] of BYTE;                                          // Buffer of arbitrary length in which to store the data read from the drive.
 end;

VAR
  hDevice: THandle;
  cbBytesReturned: DWORD;
  SCIP: TSendCmdInParams;
  aIdOutCmd: Array [0..(SizeOf(TSendCmdOutParams)+IDENTIFY_BUFFER_SIZE-1)-1] OF Byte;
  IdOutCmd : TSendCmdOutParams absolute aIdOutCmd;
  sLabel: string;
begin
 CheckValidKey;
 Result:= '';                                                                                      // Returns empty string on error

 case DriveNumber of
  0: sLabel:= '\\.\PhysicalDrive0';
  1: sLabel:= '\\.\PhysicalDrive1';
  2: sLabel:= '\\.\PhysicalDrive2';
  3: sLabel:= '\\.\PhysicalDrive3';
  4: sLabel:= '\\.\PhysicalDrive4';
  5: sLabel:= '\\.\PhysicalDrive5';
  6: sLabel:= '\\.\PhysicalDrive6';
  7: sLabel:= '\\.\PhysicalDrive7';
 else Mes('Invalid drive!')
 end;

 if System.SysUtils.Win32Platform= VER_PLATFORM_WIN32_NT
 then
     { Win2K+ }
     {$IFDEF UNICODE} { Delphi XE }
      hDevice := CreateFile( PWideChar(sLabel), GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_EXISTING, 0, 0) { MB_OK }
     {$ELSE} { Delphi 7 }
      hDevice := CreateFile( PAnsiChar(sLabel), GENERIC_READ OR GENERIC_WRITE, FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_EXISTING, 0, 0)
     {$ENDIF}
 else
     { Win95/98 }
     hDevice := CreateFile( '\\.\SMARTVSD', 0, 0, NIL, CREATE_NEW, 0, 0 );

 if hDevice= INVALID_HANDLE_VALUE then EXIT;

 TRY
    FillChar(SCIP, sizeof(TSendCmdInParams) - 1, #0);
    FillChar(aIdOutCmd, sizeof(aIdOutCmd), #0);
    cbBytesReturned := 0;
    WITH SCIP DO                                                                                   // Set up data structures for IDENTIFY command.
     begin
       cBufferSize := IDENTIFY_BUFFER_SIZE;
       WITH irDriveRegs DO // bDriveNumber := 0;
       begin
         bSectorCountReg := 1;
         bSectorNumberReg := 1;
         {
         if Win32Platform=VER_PLATFORM_WIN32_NT
         then bDriveHeadReg:= $A0
         else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
         }
         bDriveHeadReg := $A0;
         bCommandReg := $EC;
       end;
    end;

   if NOT DeviceIoControl(hDevice, $0007c088, @SCIP, SizeOf(TSendCmdInParams)-1, @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, NIL)
   then EXIT;
 FINALLY
   CloseHandle(hDevice);
 END;

 WITH PIdSector(@IdOutCmd.bBuffer)^ DO
  begin
   ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
   (PAnsiChar(@sSerialNumber)+ SizeOf(sSerialNumber))^:= #0;
   Result:= PAnsiChar(@sSerialNumber);                                         // here under D7 it was PAnsiChar
  end;
end;


function GetPartitionID(Partition: string): String;
VAR
  VolumeSerialNumber, FileSystemFlags: DWORD;
  MaximumComponentLength : DWORD;
begin
  CheckValidKey;
  {$IFDEF UNICODE}
   GetVolumeInformation(PWideChar(Partition), NIL, 0,
                        @VolumeSerialNumber,
                        MaximumComponentLength,
                        FileSystemFlags, NIL, 0);
  {$ELSE}
   GetVolumeInformation(PAnsiChar(Partition), NIL, 0,
                        @VolumeSerialNumber,
                        MaximumComponentLength,
                        FileSystemFlags, NIL, 0);
  {$ENDIF}
  Result := IntToHex(HiWord(VolumeSerialNumber), 4)+ '-'
          + IntToHex(LoWord(VolumeSerialNumber), 4);
end;





{--------------------------------------------------------------------------------------------------
                                 CPU VENDOR
---------------------------------------------------------------------------------------------------
 The GetCPUVendor function may not work on Delphi UNICODE (2009 and up).
 Therefore, we created an alternative function which should work also with
 Delphi UNICODE. Please note that the function was recently introduced.
 It may need more testing.
--------------------------------------------------------------------------------------------------}

{$IFDEF Win32}                                                                                    { This function will not work with old Delphi compilers (such as Delphi 1) because of 32 bit register use }
function GetCPUVendor: String;
VAR
  aVendor: array[0..2] of DWord;
  iI, iJ : Integer;
  s: string;
begin
 TRY
  asm
    push  ebx
    xor   eax, eax
    dw    $A20F                                                                                     { This is our CPU identification number (CPUID instruction) }
    mov   DWord ptr aVendor, ebx
    mov   DWord ptr aVendor[+4], edx
    mov   DWord ptr aVendor[+8], ecx
    pop   ebx
  end;

  CheckValidKey;
  for iI:= 0 to 2 do
    for iJ:= 0 to 3 do
      s:= s + Chr((aVendor[iI] and ($000000FF shl (iJ * 8))) shr (iJ * 8));
  Result:= s;
 except
   //todo 1: trap only specific exceptions
   Result:='Unknown CPU vendor';
 END;
end;

function GetCPUVendor2: string;
VAR aVendor: array [0..3] of Dword;
    x: PChar;
begin
 TRY
   asm
     push ebx
     mov eax, 0
     dw $A20F                                                                                      { This is our CPU identification number (CPUID instruction) }
     mov DWord ptr aVendor, ebx
     mov DWord ptr aVendor[+4], edx
     mov Dword ptr aVendor[+8], ecx
     pop ebx
   end;

   CheckValidKey;
   aVendor[3]:= 0;                                                                                 { Make a null terminated string }
   x:= @aVendor;
   Result:= x;
 except
  //todo 1: trap only specific exceptions
   Result:='Unknown CPU vendor';
 END;
end;


Function GetCPUVendor3: AnsiString;
TYPE
  TVendor = Array[ 0..11] Of AnsiChar;

 function _getCPUVendor: TVendor; Assembler; Register;
 Asm
   PUSH    EBX
   PUSH    EDI
   MOV     EDI,EAX
   MOV     EAX,0
   DW      $A20F                                                                                   { This is our CPU identification number (CPUID instruction) }
   MOV     EAX,EBX
   XCHG    EBX,ECX
   MOV     ECX,4
 @1:
   STOSB
   SHR     EAX,8
   LOOP    @1
   MOV     EAX,EDX
   MOV     ECX,4
 @2:
   STOSB
   SHR     EAX,8
   LOOP    @2
   MOV     EAX,EBX
   MOV     ECX,4
 @3:
   STOSB
   SHR     EAX,8
   LOOP    @3
   POP     EDI
   POP     EBX
 end;
begin
 CheckValidKey;
 Result:= _getCPUVendor;
end;




{--------------------------------------------------------------------------------------------------
                                      CPU ID
---------------------------------------------------------------------------------------------------
 We want to access a certain CPU but we don't know in which core Windows will
 run our code, so we use a trick: we create a thread and force that
 thread to run in the specified CPU.
--------------------------------------------------------------------------------------------------}

Function _isCPUIDAvailable: Boolean; Register;
const ID_BIT = $200000;
Asm
  PUSHFD
  POP     EAX
  MOV     EDX,EAX
  XOR     EAX,ID_BIT
  PUSH    EAX
  POPFD
  PUSHFD
  POP     EAX
  XOR     EAX,EDX
  JZ      @exit
  MOV     AL,True
@exit:
end;



function _getCPUID_Hex: string;                                                                    { Same as _getCPUID but returns a hexadecimal value }
VAR R1, R2, R3, R4 : Longword;
begin
 TRY
  asm
    mov eax,1
    db $0F, $A2
    mov R1, EAX
    mov R2, EBX
    mov R3, ECX
    mov R4, EDX
  end;
  Result:= IntToHex(R1, 8) + '-' + IntToHex(R2, 8) + '-' + IntToHex(R3, 8) + '-' + IntToHex(R4, 8);
 except
  //todo 1: trap only specific exceptions
  Result:= 'Unknown CPU serial no';
 END;
end;




{--------------------------------------------------------------------------------------------------
                             THREAD TO GET REAL CPU SPEED
--------------------------------------------------------------------------------------------------}
TYPE
  TCPUID  = Array[ 1..4 ] of Longint;
  TCPUThread = class(TThread)
   private
   protected
     procedure Execute; override;
   end;

VAR
  ThreadResult: string;

procedure TCPUThread.Execute;

//http://en.wikipedia.org/wiki/CPUID#EAX.3D3:_Processor_Serial_Number
 function _getCPUID: TCPUID; Assembler; Register;                                                  { The register convention uses up to three CPU registers to pass parameters, while the other conventions pass all parameters on the stack. }
 Asm
   PUSH    EBX
   PUSH    EDI
   MOV     EDI,EAX
   MOV     EAX,1
   DW      $A20F
   STOSD
   MOV     EAX,EBX
   STOSD
   MOV     EAX,ECX
   STOSD
   MOV     EAX,EDX
   STOSD
   POP     EDI
   POP     EBX
 end;

VAR
   CPUID: TCPUID;
   I    : Integer;
begin
 for I := Low(CPUID) to High(CPUID) DO CPUID[I]:= -1;
 CPUID:= _getCPUID;
 ThreadResult:= IntToStr(CPUID[1])+'-'+ IntToStr(CPUID[2])+'-'+ IntToStr(CPUID[3])+'-'+ IntToStr(CPUID[4]);
end;


function GetCPUID(CoreMask: Word): String;                                                         { Get the ID of the specified logical CPU }
VAR NewThread: TCPUThread;
begin
 CheckValidKey;
 ThreadResult:= '!';
 if (CoreMask < 1)
 then Mes('Wrong CPU core!')
 else
   if IsCPUIDAvailable
   then
    begin
     NewThread:= TCPUThread.Create(True);
     NewThread.FreeOnTerminate:= True;
     SetThreadAffinityMask(NewThread.Handle, CoreMask);                                              { in TASK MANAGER the first CORE window represents the CORE 0 }
     {$IFDEF UNICODE}
      NewThread.Start;
     {$ELSE}
      NewThread.Resume;
     {$ENDIF}
     REPEAT
     UNTIL ThreadResult<> '!';                                                                       { Wait for thread to finish and return the result }
    end
   else ThreadResult:= 'CPU ID not available';

 Result:= ThreadResult;
end;


function GetCpuIdNow: String;                                                                      { Get the ID of the first available logical CPU }
VAR CPUMask: Integer;
begin
  CPUMask:= CoreNumber2CoreMask(1);
  Result:= GetCPUID(CPUMask);
end;


Function IsCPUIDAvailable: Boolean;
begin
 Result:= _isCPUIDAvailable;                                                                       { Many programming languages are limited when comes about calling conventions. For safety we will export this as StdCall }
end;
{$ENDIF}




{--------------------------------------------------------------------------------------------------
                                      CPU SPEED
--------------------------------------------------------------------------------------------------}
{$IFDEF Win32}
function GetCPUSpeed(CONST SomeDelay: Integer= 50): Double;                                        { Note: The higher the delay, the accurate the result. Default= 200ms }
VAR
   TimerHi, TimerLo: DWORD;
   PriorityClass, Priority: Integer;
begin
  CheckValidKey;
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority      := GetThreadPriority(GetCurrentThread);
  SetPriorityClass (GetCurrentProcess, REALTIME_PRIORITY_CLASS);  // https://stackoverflow.com/questions/13631644/setthreadpriority-and-setpriorityclass
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);

  Sleep(10);
  ASM
    dw 310Fh
    mov TimerLo, eax
    mov TimerHi, edx
  end;

  Sleep(SomeDelay);
  ASM
    dw 310Fh
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;

  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass (GetCurrentProcess, PriorityClass);
  Result := TimerLo / (1000 * SomeDelay);
end;
{$ENDIF}


function CPUFamily: string;
VAR Reg: TRegistry;
begin
  CheckValidKey;
  Result:= '';
  Reg := TRegistry.Create(KEY_READ);
  TRY
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\Hardware\Description\System\CentralProcessor\0', False)
    then Result := Reg.ReadString('Identifier');
    Reg.CloseKey;
  FINALLY
    FreeAndNil(Reg);
  END;
end;


function GetCpuTheoreticSpeed: Integer;
VAR Reg: TRegistry;
begin
  Result:= -1;
  Reg:= TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Hardware\Description\System\CentralProcessor\0', False) then
     begin
      Result:= Reg.ReadInteger('~MHz');
      Reg.CloseKey;
    end;
  finally FreeAndNil(Reg);
  end;
end;


function GetCPUCount: Integer;                                                                     { The number of LOGICAL processors in the current group }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= iSystem.dwNumberOfProcessors;
end;


function GetCoreCount: Integer;  { Specifies the number of CPU cores in the system }
begin
 Result:= TThread.ProcessorCount;
end;


function IsSingleProcessor: Boolean;   { Specifies whether the system has a single processor or not }
begin
 Result:= TThread.IsSingleProcessor;
end;


function IsIntel64BitCPU: Boolean;                                                                 { Detects IA64 processors }
VAR iSystem: TSystemInfo;
begin
  GetSystemInfo(iSystem);
  Result:= iSystem.wProcessorArchitecture= 1;
end;











{--------------------------------------------------------------------------------------------------
   BIOS
--------------------------------------------------------------------------------------------------}
function BiosDate: string;
VAR Reg: TRegistry;
begin
 CheckValidKey;
 Reg:= TRegistry.Create(KEY_READ);
 TRY

   TRY
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\Hardware\Description\System\', FALSE);
    Result:= Reg.ReadString('SystemBiosDate');
   except
    //todo 1: trap only specific exceptions
    Result:= 'Unknown';
   END;

 FINALLY
   FreeAndNil(Reg);
 END;
end;


function BiosVersion: string;
begin
 CheckValidKey;
 TRY
   Result:= _RegReadMultiSzString2(HKEY_LOCAL_MACHINE, '\Hardware\Description\System\', 'SystemBiosVersion', FALSE);
 except
   //todo 1: trap only specific exceptions
   Result:= 'Unknown Bios version';
 END;
end;


function BiosProductID: string;
VAR Reg: TRegistry;
begin
 CheckValidKey;
 Reg:= TRegistry.Create(KEY_READ);
 TRY

   TRY
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\Hardware\Description\System\BIOS', FALSE);
    Result:= Reg.ReadString('SystemVersion');
  except
    //todo 1: trap only specific exceptions
    Result:= 'Unknown';
  END;

 FINALLY
   FreeAndNil(Reg);
 END;
end;


function BiosVideo: string;
VAR Reg: TRegistry;
begin
 CheckValidKey;
 Reg := TRegistry.Create(KEY_READ);
 TRY

  TRY
   Reg.RootKey := HKEY_LOCAL_MACHINE;
   Reg.OpenKey('\Hardware\Description\System\', FALSE);
   Result:=Reg.ReadString('VideoBiosDate');
  except
   //todo 1: trap only specific exceptions
   Result:='Unknown';
  END;

 FINALLY
   FreeAndNil(Reg);
 END;
end;














{--------------------------------------------------------------------------------------------------
   Hardware Report
--------------------------------------------------------------------------------------------------}
function GenerateHardwareReport: string;                                                                               { Before calling this I need to enter a valid key into chHardID:    }
VAR s: string;
begin
 Result:= '';
 Result:= Result+'Hardware report'+CRLF+ CRLF;

 { CPU }
 Result:= Result+ CRLF+'CPU'+CRLF+ CRLF;
 Result:= Result+'  CPU family: '                   + Tab+Tab + CPUFamily+ CRLF;                                       { Get cpu identifier from the windows registry }
 {$IFDEF Win32}
 Result:= Result+'  CPU vendor: '                   + Tab+Tab + GetCPUVendor+ CRLF;                                    { New GetCPUVendor function. Reported to work with D7 and D2009. }
 Result:= Result+'  CPU speed: '                    + Tab+Tab + IntToStr(round(GetCPUSpeed))+ CRLF;
 if IsCPUIDAvailable
 then Result:= Result+'  CPU ID: '                  + Tab+Tab + GetCpuIdNow+ CRLF;
 {$ENDIF}

 Result:= Result+'  CPU theoretic speed: '          + Tab     + IntToStr(GetCpuTheoreticSpeed)+ CRLF;                  { Get cpu speed (in MHz) }
 Result:= Result+'  No of logical cores: '          + Tab     + IntToStr(GetCPUCount)+ CRLF;                           { The number of LOGICAL processors in the current group }
 if IsIntel64BitCPU
 then Result:= Result+'  Intel 64 bit mode active: '+ Tab     + BoolToStr(IsIntel64BitCPU, TRUE)+ CRLF;                { Detects IA64 processors }

 { SYSTEM RAM }
 Result:= Result+ CRLF+'RAM'+CRLF+ CRLF;
 {$IFDEF WIN64}                                                                                                        { On 32 bit system I cannot see more than 2GB }
 Result:= Result+'  Installed RAM: '                + Tab +  RAMInstalled+ CRLF;                                       { Total physical memory in bytes      }
 {$ENDIF}
 Result:= Result+'  Memory load: '                  + Tab+Tab + IntToStr(RAMLoad)+ '%'+CRLF;              { Total memory used in percents (%)   }
 Result:= Result+'  Avail physical: '               + Tab +  RAMFreeS+ CRLF;                                { Available physical memory (bytes)   }
 Result:= Result+'  Total page file: '              + Tab +  SystemMemStatus_mB(mTotalPageFile)+ CRLF;                 { Total page file in (bytes)          }
 Result:= Result+'  Avail page file: '              + Tab +  SystemMemStatus_mB(mAvailPageFile)+ CRLF;                 { Available page file (bytes)         }
 Result:= Result+'  Total virtual: '                + Tab +  SystemMemStatus_mB(mTotalVirtual)+ CRLF;                  { Total virtual memory in bytes       }
 Result:= Result+'  Avail virtual: '                + Tab +  SystemMemStatus_mB(mAvailVirtual)+ CRLF;                  { Available virtual memory (bytes)    }

 { PAGE }
 Result:= Result+ CRLF+'PAGE'+CRLF+ CRLF;
 Result:= Result+'  Page size granularity: '        + Tab + IntToStr(GetPageSize)+' KB'+ CRLF;                         { The page size and the granularity of page protection and commitment. This is the page size used by the VirtualAlloc function. }
 Result:= Result+'  Virtual memory granularity: '   + Tab + IntToStr(GetMemGranularity)+ ' B'+ CRLF;                   { Granularity with which virtual memory is allocated (in KB) }
 Result:= Result+'  Lowest/highest accessible address: '  + IntToStr(GetLowAddr) + '/'+ IntToStr(GetHiAddr)+ CRLF;     { Lowest RAM memory address accessible to applications (this is the RAM address, not virtual memory address) }

 { PROCESS RAM }
 Result:= Result+ CRLF+'CURRENT PROCESS'+CRLF+ CRLF;                                                                   { Returns data about the memory used of the current process }
 Result:= Result+'  Current process mem: '          + Tab+Tab + ProcessCurrentMemS+ CRLF;
 Result:= Result+'  Current process peak mem: '     + Tab     + ProcessPeakMem+ CRLF;                                  { Shows the highest amount of memory this program ever occupied }
 Result:= Result+'  Working set size: '             + Tab+Tab + IntToStr(ProcessMemStatus(pWorkingSetSize) DIV KB)+ ' KB'+ CRLF;          {- the current working set size, in bytes. }
 Result:= Result+'  Page file usage: '              + Tab+Tab + IntToStr(ProcessMemStatus(pPageFileUsage)  DIV KB)+ ' KB'+ CRLF;          {- The peak space allocated for the pagefile, in bytes.}
 Result:= Result+'  Peak page file usage: '         + Tab     + IntToStr(ProcessMemStatus(pPeakPagefileUsage) DIV KB)+ ' KB'+ CRLF;

 { PROCESS RAM - Advanced stuff }
 Result:= Result+'  Page fault count: '             + Tab+Tab + IntToStr(ProcessMemStatus(pPageFaultCount)     DIV KB)+ ' KB'+ CRLF;      {- the number of page faults.}
 Result:= Result+'  Peak working set size: '        + Tab     + IntToStr(ProcessMemStatus(pPeakWorkingSetSize) DIV KB)+ ' KB'+ CRLF;      {- the peak working set size, in bytes.}
 Result:= Result+'  Quota peak paged pool usage: '  + Tab     + IntToStr(ProcessMemStatus(pQuotaPeakPagedPoolUsage) DIV KB)+ ' KB'+ CRLF; {- The current paged pool usage, in bytes.}
 Result:= Result+'  Quota paged Pool usage: '       + Tab     + IntToStr(ProcessMemStatus(pQuotaPagedPoolUsage)   DIV KB)+ ' KB'+ CRLF;   {- The peak nonpaged pool usage, in bytes.}
 Result:= Result+'  Quota peak non paged pool: '    + Tab     + IntToStr(ProcessMemStatus(pQuotaPeakNonPagedPool) DIV KB)+ ' KB'+ CRLF;   {- The current nonpaged pool usage, in bytes.}
 Result:= Result+'  Quota non paged pool usg: '     + Tab     + IntToStr(ProcessMemStatus(pQuotaNonPagedPoolUsg)  DIV KB)+ ' KB'+ CRLF;   {- The current space allocated for the pagefile, in bytes. Those pages may or may not be in memory.}

 { HDD }
 Result:= Result+ CRLF+'HDD'+CRLF+ CRLF;
 Result:= Result+'  Partition ID: '                 + Tab+Tab + GetPartitionID('C:')+ CRLF;                                               { Get the ID of the specified patition. Example of parameter: 'C:' }
 s:= string(GetIdeSerialNumber(0));
 if Length(s)= 0
 then Result:= Result+'  The program needs to access the disk at low level but in some conditions Windows 7 UAC may block the program from doing so. Please temporary elevate program''s rights. '+ CRLF
 else Result:= Result+'  Hard drive unique ID: '    + Tab + Trim(s)+ CRLF;                                                                { DriveNr is from 0 to 4 }

 { BIOS (NEW!) }
 Result:= Result+ CRLF+'BIOS'+CRLF+ CRLF;
 Result:= Result+'  Bios date: '                    + Tab+Tab + BiosDate+ CRLF;
 Result:= Result+'  Bios version: '                 + Tab     + BiosVersion+ CRLF;
 Result:= Result+'  Bios productID: '               + Tab     + BiosProductID+ CRLF;
 Result:= Result+'  Bios video: '                   + Tab+Tab + BiosVideo+ CRLF;

 { OTHER STUFF THAT I COULD SHOW:

 Result:= Result+'  GetDllVersion: '              + Tab+Tab + GetDllVersion+ CRLF;
 Result:= Result+'  Windows product ID: '           + Tab+Tab + WindowsProductID+ CRLF;
 Result:= Result+'  Computer name: '        + Tab+Tab + GetComputerName+ CRLF;
 Result:= Result+'  Local IP: '             + Tab+Tab + ciInternet.GetLocalIP+ CRLF;
 Result:= Result+'  User name: '            + Tab+Tab + GetLogonName+ CRLF;
 Result:= Result+'  '+ cOsVersion.TOSVersion.ToString+ CRLF;
 Result:= Result+'  Windows up time: '      + Tab+Tab+ WindowsUpTime+ CRLF;
 Result:= Result+'  Total monitors: '       + Tab+Tab+ IntToStr(Screen.MonitorCount)+ '. Monitor res: ' + IntToStr(Screen.Width)+ 'x'+ IntToStr(Screen.Height)+ CRLF;
 Result:= Result+'  C:\ free space: '       + Tab+ ccIO.DriveFreeSpaceS ('C') + CRLF;
 Result:= Result+'  CPU family: '           + Tab+ chHardID.CPUFamily+ ' / '+ chHardID.GetCPUVendor+ CRLF;
 Result:= Result+'  CPU theoretic speed: '  + Tab+ IntToStr(GetCpuTheoreticSpeed)+ ' MHz' + CRLF;
 TrimWorkingSet;  { Minimizes the amount to RAM used by application by swapping the unused pages back to disk - Details: http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393 }
end;













function GenerateRamRepOS: string;
begin
 Result:= ' [OS RAM]'+ CRLF;
 Result:= Result+'  Free: '      + Tab+ RAMFrees+ CRLF;                                        { Returns free amount of free RAM. The size (MB, GB) is dynamically chousen }
 Result:= Result+'  Load: '      + Tab+ IntToStr(RAMLoad)+ '%'+ CRLF;                               { Memory load (in %) }
 Result:= Result+'  Installed: ' + Tab+ RAMInstalledS+ CRLF;                                    { NEW. Replaces chHardID.SystemMemStatus_GB (mTotalPhys) }
end;



function GenerateRamRepProc: string;
begin
 Result:= ' [PROCESS MEMORY]'+ CRLF;
 Result:= Result+'  Peak: '      + Tab+ ProcessPeakMem+ CRLF;                                  { Shows the highest amount of memory this program ever occupied }
 Result:= Result+'  Current: '   + Tab+ ProcessCurrentMemS+ CRLF;
 Result:= Result+'  Available: ' + Tab+ RAMFrees+ ' (' + IntToStr(RAMLoad)+ '% load)' +CRLF;        { Returns free amount of free RAM. The size (MB, GB) is dynamically chousen }
end;


end.
