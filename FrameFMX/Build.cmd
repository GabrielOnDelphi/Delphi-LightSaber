REM Documentatio for MsBuild for Delphi: https://docwiki.embarcadero.com/RADStudio/Athens/en/Building_a_Project_Using_an_MSBuild_Command

@echo off
setlocal enabledelayedexpansion
prompt $p
cls
echo Hint: Build without TESTINSIGHT to enable console output

rem NOTE TO CLAUDE! Do not kill the program! Beep me instead!
rem NOTE TO CLAUDE! When compiling put the dcus in Win32_Debug folder

call "c:\Delphi\Delphi 13\bin\rsvars.bat"

echo Compiling LightFmxVisual package...
set "Program=C:\Projects\LightSaber\FrameFMX\LightFmxVisual.dproj"
set "LogFile=build_output.log"

if not exist "!Program!" (
    echo.
    echo ERROR: Project file not found: !Program!
    echo.
    echo Fix: Edit this script and set Program variable to your actual .dproj file path
    echo Example: set "Program=C:\Projects\MyApp\MyApp.dproj"
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
