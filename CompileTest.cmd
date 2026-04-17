@echo off
call "c:\Delphi\Delphi 13\bin\rsvars.bat"
echo Building LightFmxVisual...
"c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" "c:\Projects\LightSaber\FrameFMX\LightFmxVisual.dproj" /t:Build /p:platform=Win32 /p:Configuration=Debug

if errorlevel 1 (
    echo.
    echo BUILD FAILED - LightFmxVisual
    exit /b 1
) else (
    echo.
    echo BUILD OK - LightFmxVisual
)
