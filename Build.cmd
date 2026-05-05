REM Build LightCore package

@echo off
setlocal enabledelayedexpansion
prompt $p
cls

echo Compiling LightCore...

call "c:\Delphi\Delphi 13\bin\rsvars.bat"

set "Program=c:\Projects\LightSaber\LightCore.dproj"
set "LogFile=c:\Projects\LightSaber\build_output.log"

if not exist "!Program!" (
    echo.
    echo ERROR: Project file not found: !Program!
    echo.
    exit /b 1
)

echo Build started %time% >> !LogFile!

"c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" "!Program!" /t:Clean;Build /p:platform=Win32 /p:Config=Debug >> !LogFile! 2>&1

echo Build finished %time% >> !LogFile!
echo Exit code: %errorlevel% >> !LogFile!

if errorlevel 1 (
    echo.
    echo BUILD FAILED
    exit /b 1
) else (
    echo.
    echo BUILD OK
)
