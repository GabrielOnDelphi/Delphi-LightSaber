UNIT LightCore.ExeVersion;

{=============================================================================================================
   2026.09.15

   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Features:
     Retrieves version information (version number, build number) from executable files.

   Functions:
     GetVersionInfoFile - Low-level function returning the four file version numbers in a TFileVersion record
     GetVersionInfo     - High-level function returning formatted version string

   GetVersionInfoFile and GetVersionInfo exist only on Windows: they read the version resource through version.dll. Off Windows the unit declares nothing, so a call made in an Android, macOS or iOS build fails to compile instead of returning a silent wrong answer.

   Tester:
       c:\Projects\LightSaber\Demo\VCL\Demo WinVersion\

   Also see:
       LightCore.WinVersion
       LightVcl.Common.WinVersionApi
=============================================================================================================}

INTERFACE

{$IFDEF MSWINDOWS}
USES
   WinApi.Windows, System.SysUtils;

TYPE
  TFileVersion = record
    Major, Minor, Release, Build: Word;
  end;

{ Retrieves the file version numbers of an executable file.
  Returns False (and a zeroed record) if the file has no version resource.
  Raises an exception if FileName is empty. }
function GetVersionInfoFile(CONST FileName: string; OUT Version: TFileVersion): Boolean;

{ Returns formatted version string from executable file.
  Format: "Major.Minor.Release" or "Major.Minor.Release.Build" if ShowBuildNo=True.
  Raises exception if FileName is empty or file has no version info. }
function GetVersionInfo(CONST FileName: string; ShowBuildNo: Boolean = False): string;
{$ENDIF}


IMPLEMENTATION
{$IFDEF MSWINDOWS}


{---------------------------------------------------------------------------------------------------------------
   GetVersionInfoFile

   The Windows TVSFixedFileInfo structure contains:
     dwFileVersionMS - High 32 bits: Major (high word), Minor (low word)
     dwFileVersionLS - Low 32 bits: Release (high word), Build (low word)

   Source: JCL
---------------------------------------------------------------------------------------------------------------}
function GetVersionInfoFile(CONST FileName: string; OUT Version: TFileVersion): Boolean;
VAR
  Buffer: string;
  DummyHandle: DWORD;
  InfoSize, FixInfoLen: DWORD;
  FixInfoBuf: PVSFixedFileInfo;
begin
  if FileName = ''
  then raise Exception.Create('GetVersionInfoFile: FileName parameter cannot be empty');

  Result:= False;
  Version:= Default(TFileVersion);
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
          Version.Major  := HiWord(FixInfoBuf.dwFileVersionMS);
          Version.Minor  := LoWord(FixInfoBuf.dwFileVersionMS);
          Version.Release:= HiWord(FixInfoBuf.dwFileVersionLS);
          Version.Build  := LoWord(FixInfoBuf.dwFileVersionLS);
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
  Version: TFileVersion;
begin
  if FileName = ''
  then raise Exception.Create('GetVersionInfo: FileName parameter cannot be empty');

  if NOT GetVersionInfoFile(FileName, Version)
  then raise Exception.Create('GetVersionInfo: Cannot retrieve version info from file: ' + FileName);

  Result:= IntToStr(Version.Major) + '.' + IntToStr(Version.Minor) + '.' + IntToStr(Version.Release);

  if ShowBuildNo
  then Result:= Result + '.' + IntToStr(Version.Build);
end;
{$ENDIF}


end.
