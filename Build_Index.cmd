@echo off
REM Build_Index.cmd - regenerate _Index.md (LightSaber public API map).
REM Run after adding/renaming units or public routines. _Index.md is a DERIVED
REM file; do not edit it by hand. See Build_Index.ps1 for details.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0Build_Index.ps1"
