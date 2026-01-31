@echo off
call "c:\Delphi\Delphi 13\bin\rsvars.bat"
REM Build without TESTINSIGHT to enable console output
"c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" "c:\Projects\LightSaber\Tests\Tests_LightVcl.Forms.dproj" /t:Build /p:platform=Win32 /p:Configuration=Debug /p:DCC_Define="DEBUG;FRAMEWORK_VCL"
