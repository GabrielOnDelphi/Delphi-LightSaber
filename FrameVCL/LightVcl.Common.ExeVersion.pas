UNIT LightVcl.Common.ExeVersion;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Features:
     Returns info (version no) about an exe file.
==============================================================================================================

   Tester:
       c:\Projects\LightSaber\Demo\Demo Detect WinVer\

   Also see:
       LightVcl.Common.WinVersion
       LightVcl.Common.ExeVersion
       LightVcl.Common.WinVersionApi
=============================================================================================================}

INTERFACE

USES
   WinApi.Windows, System.SysUtils;


function GetVersionInfoFile(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;
function GetVersionInfo    (CONST FileName: string; ShowBuildNo: Boolean= False): string;


IMPLEMENTATION


CONST
   TAB  = #9;
   CRLF = #13#10;
   Win10FirstRel = 10240;  //July 29, 2015
   Win11FirstRel = 22000;  //October 4, 2021



{ Returns information (including version no) about an EXE file.
  Source: JCL }
function GetVersionInfoFile(CONST FileName: string; VAR FixedInfo: TVSFixedFileInfo): Boolean;
VAR
  Buffer: string;
  DummyHandle: DWORD;    { The DummyHandle parameter is ignored by GetFileVersionInfo }
  InfoSize, FixInfoLen: DWORD;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  InfoSize := GetFileVersionInfoSize(PChar(FileName), DummyHandle);
  if InfoSize > 0 then
   begin
    FixInfoLen := 0;
    FixInfoBuf := Nil;

    SetLength(Buffer, InfoSize);
    if  GetFileVersionInfo(PChar(FileName), DummyHandle, InfoSize, Pointer(Buffer))
    AND VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen)
    AND (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
      begin
        Result := True;
        FixedInfo := FixInfoBuf^;
      end;
  end;
end;


function GetVersionInfo(CONST FileName: string; ShowBuildNo: Boolean= False): string;
VAR FixedInfo: TVSFixedFileInfo;
begin
  FixedInfo.dwSignature:= 0;
  if LightVcl.Common.ExeVersion.GetVersionInfoFile(FileName, FixedInfo)
  then
     begin
      Result:= IntToStr(HiWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(LoWord(FixedInfo.dwFileVersionMS))+'.'+ IntToStr(HiWord(FixedInfo.dwFileVersionLS));
      if ShowBuildNo
      then Result:= Result+ '.'+ IntToStr(LoWord(FixedInfo.dwFileVersionLS));
     end
  else Result:= 'N/A';
end;



end.
