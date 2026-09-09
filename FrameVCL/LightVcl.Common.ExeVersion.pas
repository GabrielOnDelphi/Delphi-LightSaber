UNIT LightVcl.Common.ExeVersion;

{=============================================================================================================
   2026.01.29

   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Features:
     Retrieves version information (version number, build number) from executable files.

   Functions:
     GetVersionInfoFile - Low-level function returning raw TVSFixedFileInfo structure
     GetVersionInfo     - High-level function returning formatted version string

   Tester:
       c:\Projects\LightSaber\Demo\VCL\Demo WinVersion\

   Also see:
       LightVcl.Common.WinVersion
       LightVcl.Common.WinVersionApi
=============================================================================================================}

INTERFACE

USES
   WinApi.Windows, System.SysUtils;


{ Retrieves version info structure from an executable file.
  Raises an exception if FileName is empty. }
function GetVersionInfoFile(CONST FileName: string; OUT FixedInfo: TVSFixedFileInfo): Boolean;

{ Returns formatted version string from executable file.
  Format: "Major.Minor.Release" or "Major.Minor.Release.Build" if ShowBuildNo=True.
  Raises exception if FileName is empty or file has no version info. }
function GetVersionInfo(CONST FileName: string; ShowBuildNo: Boolean = False): string;


IMPLEMENTATION


{---------------------------------------------------------------------------------------------------------------
   GetVersionInfoFile

   The TVSFixedFileInfo structure contains:
     dwFileVersionMS - High 32 bits: Major (high word), Minor (low word)
     dwFileVersionLS - Low 32 bits: Release (high word), Build (low word)

   Source: JCL
---------------------------------------------------------------------------------------------------------------}
function GetVersionInfoFile(CONST FileName: string; OUT FixedInfo: TVSFixedFileInfo): Boolean;
VAR
  Buffer: string;
  DummyHandle: DWORD;
  InfoSize, FixInfoLen: DWORD;
  FixInfoBuf: PVSFixedFileInfo;
begin
  if FileName = ''
  then raise Exception.Create('GetVersionInfoFile: FileName parameter cannot be empty');

  Result:= False;
  InfoSize:= GetFileVersionInfoSize(PChar(FileName), DummyHandle);

  if InfoSize > 0 then
    begin
      FixInfoLen:= 0;
      FixInfoBuf:= NIL;

      SetLength(Buffer, InfoSize);
      if GetFileVersionInfo(PChar(FileName), DummyHandle, InfoSize, Pointer(Buffer))
      AND VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen)
      AND (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
        begin
          Result:= True;
          FixedInfo:= FixInfoBuf^;
        end;
    end;
end;


{---------------------------------------------------------------------------------------------------------------
   GetVersionInfo

   Examples:
     GetVersionInfo('C:\Windows\explorer.exe')       -> "10.0.26100"
     GetVersionInfo('C:\Windows\explorer.exe', True) -> "10.0.26100.8655"
---------------------------------------------------------------------------------------------------------------}
function GetVersionInfo(CONST FileName: string; ShowBuildNo: Boolean = False): string;
VAR
  FixedInfo: TVSFixedFileInfo;
begin
  if FileName = ''
  then raise Exception.Create('GetVersionInfo: FileName parameter cannot be empty');

  FillChar(FixedInfo, SizeOf(FixedInfo), 0);

  if NOT GetVersionInfoFile(FileName, FixedInfo)
  then raise Exception.Create('GetVersionInfo: Cannot retrieve version info from file: ' + FileName);

  Result:= IntToStr(HiWord(FixedInfo.dwFileVersionMS)) + '.' +
           IntToStr(LoWord(FixedInfo.dwFileVersionMS)) + '.' +
           IntToStr(HiWord(FixedInfo.dwFileVersionLS));

  if ShowBuildNo
  then Result:= Result + '.' + IntToStr(LoWord(FixedInfo.dwFileVersionLS));
end;


end.
