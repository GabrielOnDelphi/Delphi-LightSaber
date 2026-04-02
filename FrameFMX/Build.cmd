@echo off
call "c:\Delphi\Delphi 13\bin\rsvars.bat"
echo Compiling LightFmxVisual package...
"c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" "C:\Projects\LightSaber\FrameFMX\LightFmxVisual.dproj" /t:Build /p:platform=Win32 /p:Configuration=Debug /p:DCC_Define="DEBUG"

if errorlevel 1 (
    echo.
    echo BUILD FAILED
    exit /b 1
) else (
    echo.
    echo BUILD OK
)
