@echo off
call "c:\Delphi\Delphi 13\bin\rsvars.bat"
"c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" "c:\Projects\LightSaber\Demo\FMX\Demo LightLog\FMX_Demo_Log.dproj" /t:Build /p:platform=Win32 /p:Configuration=Debug
