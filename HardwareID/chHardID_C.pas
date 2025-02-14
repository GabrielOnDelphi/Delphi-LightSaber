UNIT chHardID_C;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
{
ULTIMATE GUIDE:
http://rvelthuis.de/articles/articles-dlls.html

Delphi

In Delphi, this looks like it could be some work, but it isn’t. Newer versions of Delphi have project options in the IDE that allow you to generate object files and headers:

Project menu ? Options ? Delphi Compiler ? Output – C/C++ ? C/C++ output file generation

The last item is a combobox that allows you to create a number of output files to be consumed by C and/or C++. Just choose an option that includes headers.

Note that the header has the extension .hpp and may contain some macros or defines that are specific to Delphi and C++ Builder. Be sure to look through the file and remove such stuff. Also, there will be a .hpp file for each unit, not for a DLL. So you may have to combine them into one single header file.

C and C++ usually use .h as file extension. You can just rename the file(s) generated.

But even in older versions of Delphi, you can tell the compiler to create C++ headers, it is just not as convenient as in the newer versions. Just specifiy the -JHPNI command line option. That will make the Delphi compiler generate, among other files, a .hpp file.
}

{===============================================================================

 UNIT
   chHardID
   Version 2.0
   2013.11.3

 COPYRIGHT
   CubicDesign 2013
   All rights reserved
   Closed source. Do not distribute
   www.Soft.Tahionic.com

 Programming languages supported:
    Pascal, C++, VB, .Net, etc


 IMPORTANT NOTE
    Call ReleaseMemory after you call a function that returns dynamic data (strings).
    Else you will leak memory.

=============================================================================================================}


INTERFACE

USES
  chHardID;

  { CPU }
  function CPUFamily           : PAnsiChar;                           stdcall;
 {$IFDEF Win32}
  function GetCPUVendor        : PAnsiChar;                           stdcall;
  function GetCPUSpeed         : Double;                              stdcall;
  function GetCPUID (CoreMask: Word): PAnsiChar;                      stdcall;
  function GetCpuIdNow         : PAnsiChar;                           stdcall;
  function IsCPUIDAvailable    : Boolean;                             stdcall;
   {$ENDIF}
  function IsIntel64BitCPU     : Boolean;                             stdcall;
  function GetCpuTheoreticSpeed: Integer;                             stdcall;
  function GetCPUCount         : Integer;                             stdcall;

  { PROCESS RAM }
  function ProcessMemStatus   (CONST ProcMemType: Byte= 1): Cardinal; stdcall;                     { Returns data about the memory used of the current process }
  function ProcessPeakMem   : PAnsiChar;                              stdcall;                     { Shows the highest amount of memory this program ever occupied }
  function ProcessCurrentMem: PAnsiChar;                              stdcall;

  { VIRTUAL RAM }
  function GetPageSize      : Cardinal;                               stdcall;                     { The page size and the granularity of page protection and commitment. This is the page size used by the VirtualAlloc function. }
  function GetMemGranularity: Integer;                                stdcall;                     { Granularity with which virtual memory is allocated (in KB) }

  { RAM - Advanced stuff }
  function GetLowAddr       : Cardinal;                               stdcall;                     { Lowest RAM memory address accessible to applications (this is the RAM address, not virtual memory address) }
  function GetHiAddr        : Cardinal;                               stdcall;                     { Lowest RAM memory address accessible to applications }
  procedure TrimWorkingSet;                                           stdcall;                     { Minimizes the amount to RAM used by application by swapping the unused pages back to disk - Details: http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393 }

  { HDD }
  function GetPartitionID    (Partition  : PAnsiChar): PAnsiChar;     stdcall;
  function GetIDESerialNumber(DriveNumber: Byte ): PAnsiChar;         stdcall;

  { BIOS (NEW!) }
  function BiosDate     : PAnsiChar;                                  stdcall;
  function BiosVersion  : PAnsiChar;                                  stdcall;
  function BiosProductID: PAnsiChar;                                  stdcall;
  function BiosVideo    : PAnsiChar;                                  stdcall;

  { UTILS }
  function  GenerateHardwareReport: PAnsiChar;                                                     { Before calling this I need to enter a valid key into chHardID:    }
  function  FormatBytes  (CONST Size: Int64; CONST Decimals: Integer): PAnsiChar; stdcall;         { Format bytes to KB, MB, GB, TB }
  function  BinToInt     (Value: PAnsiChar): Integer;                 stdcall;
  function  IntToBin     (CONST Value, Digits: integer): PAnsiChar;   stdcall;
  function  CoreNumber2CoreMask(CoreNo: integer): integer;            stdcall;
  procedure ReleaseMemory (P: PAnsiChar);                             stdcall;
  function  GetDllVersion: Real;                                      stdcall;

  { PRIVATE }
  function  EnterKey(Value: integer): Boolean;                        stdcall;



IMPLEMENTATION
USES AnsiStrings;




{--------------------------------------------------------------------------------------------------
   PRIVATE FUNCTIONS
--------------------------------------------------------------------------------------------------}
function EnterKey(Value: integer): Boolean;
begin
 Result:= chHardID.EnterKey(Value);
end;


{--------------------------------------------------------------------------------------------------
   UTILS
--------------------------------------------------------------------------------------------------}   
function GetDllVersion: Real;
begin
 Result:= chHardID.GetDllVersion;
end;


function BinToInt(Value: PAnsiChar): Integer;
VAR s: String;
begin
 s:= string(Value);
 Result:= chHardID.BinToInt(s);
end;


function IntToBin(CONST Value, Digits: integer): PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.IntToBin(Value, Digits));                                                      { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function FormatBytes(CONST Size: Int64; CONST Decimals: Integer): PAnsiChar;                       { Formats the size of a file from bytes to KB, MB, GB, TB } { Old name was: FormatFileSize }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.FormatBytes(Size, Decimals));                                                      { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function CoreNumber2CoreMask(CoreNo: integer): integer;
begin
 Result:= chHardID.CoreNumber2CoreMask(CoreNo)
end;


procedure ReleaseMemory(P: PAnsiChar);
begin
 FreeMem(P);
end;








{--------------------------------------------------------------------------------------------------
   RAM PAGE
--------------------------------------------------------------------------------------------------}
function GetPageSize: Cardinal;                                                                    { The page size and the granularity of page protection and commitment. This is the page size used by the VirtualAlloc function. }
begin
 Result:= chHardID.GetPageSize;
end;


function GetMemGranularity: Integer;                                                               { Granularity with which virtual memory is allocated (in KB) }
begin
 Result:= chHardID.GetMemGranularity;
end;


function GetLowAddr: Cardinal;
begin
 Result:= chHardID.GetLowAddr;
end;


function GetHiAddr: Cardinal;                                                                      { Lowest RAM memory address accessible to applications }
begin
 Result:= chHardID.GetHiAddr;
end;


procedure TrimWorkingSet;                                                                          { Minimizes the amount to RAM used by application by swapping the unused pages back to disk - Details: http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393       and this:  http://17slon.com/gp/gp/dsiwin32.htm }
begin
 chHardID.TrimWorkingSet;
end;


function ProcessMemStatus(CONST ProcMemType: Byte=1): Cardinal;                                     { Returns data about the memory used of the current process }
begin
  Result:= chHardID.ProcessMemStatus(ProcMemType);
end;


function ProcessPeakMem: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.ProcessPeakMem);                                                       { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function ProcessCurrentMem: PAnsiChar;                                                             { Shows the amount of memory that the (current) program occupies now }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.ProcessCurrentMem);                                                    { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;







{--------------------------------------------------------------------------------------------------
   HDD
--------------------------------------------------------------------------------------------------}
function GetIdeSerialNumber(DriveNumber: Byte): PAnsiChar;                                         { READ HARDWARE ID:  DriveNr is from 0 to 3 }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.GetIdeSerialNumber(DriveNumber));                                      { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function GetPartitionID(Partition: PAnsiChar): PAnsiChar;
VAR
   s: AnsiString;
   sPartition: string;
begin
 sPartition:= string(Partition);
 s:= AnsiString(chHardID.GetPartitionID(sPartition));                                           { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;






{--------------------------------------------------------------------------------------------------
   CPU
--------------------------------------------------------------------------------------------------}
{$IFDEF Win32}
function GetCPUVendor: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.GetCPUVendor);                                                         { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


Function IsCPUIDAvailable: Boolean; stdcall;
begin
 Result:= chHardID.IsCPUIDAvailable;                                                            { Many programming languages are limited when comes about calling conventions. For safety we will export this as StdCall }
end;


function GetCpuIdNow: PAnsiChar;                                                                   { Get the ID of the first available logical CPU }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.GetCpuIdNow);                                                          { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;



function GetCPUID(CoreMask: Word): PAnsiChar;                                                      { Get the ID of the specified logical CPU }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.GetCPUID(CoreMask));                                                   { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function GetCPUSpeed: Double;                                                                      { Note: call the GetCPUSpeed function more than one time to get a good result. }
begin
 Result:= chHardID.GetCPUSpeed;
end;
{$ENDIF}




function CPUFamily: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.CPUFamily);                                                            { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function GetCpuTheoreticSpeed: Integer;
begin
  Result:= chHardID.GetCpuTheoreticSpeed;
end;


function GetCPUCount: Integer;                                                                     { The number of LOGICAL processors in the current group }
begin
 Result:= chHardID.GetCPUCount;
end;


function IsIntel64BitCPU: Boolean;                                                                 { Detects IA64 processors }
begin
  Result:= chHardID.IsIntel64BitCPU;
end;






{--------------------------------------------------------------------------------------------------
   BIOS
--------------------------------------------------------------------------------------------------}{ NEW IN THIS VERSION }
function BiosDate: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.BiosDate);                                                             { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function BiosVersion: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.BiosVersion);                                                          { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function BiosProductID: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.BiosProductID);                                                        { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


function BiosVideo: PAnsiChar;
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.BiosVideo);                                                            { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;







{--------------------------------------------------------------------------------------------------
   Hardware Report
--------------------------------------------------------------------------------------------------}
function GenerateHardwareReport: PAnsiChar;                                                        { Before calling this I need to enter a valid key into chHardID:    }
VAR s: AnsiString;
begin
 s:= AnsiString(chHardID.GenerateHardwareReport);                                               { Call the funtion to find out how many bytes of RAM I need to reserve }
 GetMem(Result, Length(s) +1);                                                                     { +1 because we need to add the NULL termninator }
 StrPCopy(Result, s);                                                                              { Copy the Delphi string into the returned PAnsiChar }
end;


end.
