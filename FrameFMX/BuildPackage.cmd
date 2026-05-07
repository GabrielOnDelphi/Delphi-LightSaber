REM Documentation for MsBuild for Delphi: https://docwiki.embarcadero.com/RADStudio/Athens/en/Building_a_Project_Using_an_MSBuild_Command

@echo off
setlocal enabledelayedexpansion
prompt $p
cls
echo Hint: Build without TESTINSIGHT to enable console output

rem NOTE TO CLAUDE! Do not kill the program! Beep me instead!
rem NOTE TO CLAUDE! When compiling put the dcus in Win32_Debug folder

call "c:\Delphi\Delphi 13\bin\rsvars.bat"

set "MSBuild=c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
set "LogFile=build_output.log"

REM Build packages in dependency order: LightCore -> LightFmxCommon -> LightFmxVisual
set "Project1=C:\Projects\LightSaber\LightCore.dproj"
set "Project2=C:\Projects\LightSaber\FrameFMX\LightFmxCommon.dproj"
set "Project3=C:\Projects\LightSaber\FrameFMX\LightFmxVisual.dproj"

echo Build started %time% > !LogFile!

for %%P in ("!Project1!" "!Project2!" "!Project3!") do (
    if not exist %%P (
        echo.
        echo ERROR: Project file not found: %%P
        echo.
        exit /b 1
    )
    echo.
    echo Compiling %%~nxP ...
    echo. >> !LogFile!
    echo === %%~nxP === >> !LogFile!
    "!MSBuild!" %%P /t:Clean;Build /p:platform=Win32 /p:Config=Debug >> !LogFile! 2>&1
    if errorlevel 1 (
        echo.
        echo BUILD FAILED at %%~nxP
        echo See !LogFile! for details
        echo Exit code: !errorlevel! >> !LogFile!
        exit /b 1
    )
)

echo Build finished %time% >> !LogFile!
echo Exit code: 0 >> !LogFile!

echo.
echo BUILD OK
