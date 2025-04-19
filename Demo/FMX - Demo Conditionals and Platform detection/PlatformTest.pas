unit PlatformTest;

// This demonstrates Platform and Framework detection

interface

implementation


// Framework Detection: VCL
{$IFDEF FRAMEWORK_VCL}
  {$MESSAGE 'FRAMEWORK_VCL: ON'}
{$ELSE}
  {$MESSAGE 'FRAMEWORK_VCL: OFF'}
{$ENDIF}

// Framework Detection: FMX
{$IFDEF FRAMEWORK_FMX}
  {$MESSAGE 'FRAMEWORK_FMX: ON'}
{$ELSE}
  {$MESSAGE 'FRAMEWORK_FMX: OFF'}
{$ENDIF}

// Platform Detection: Windows
{$IFDEF MSWINDOWS}
  {$MESSAGE 'MSWINDOWS: ON'}
{$ELSE}
  {$MESSAGE 'MSWINDOWS: OFF'}
{$ENDIF}

// Platform Detection: Windows 32-bit
{$IFDEF WIN32}
  {$MESSAGE 'WIN32: ON'}
{$ELSE}
  {$MESSAGE 'WIN32: OFF'}
{$ENDIF}

// Platform Detection: Windows 64-bit
{$IFDEF WIN64}
  {$MESSAGE 'WIN64: ON'}
{$ELSE}
  {$MESSAGE 'WIN64: OFF'}
{$ENDIF}

// Platform Detection: Linux
{$IFDEF LINUX}
  {$MESSAGE 'LINUX: ON'}
{$ELSE}
  {$MESSAGE 'LINUX: OFF'}
{$ENDIF}

// Platform Detection: Linux 32-bit
{$IFDEF LINUX32}
  {$MESSAGE 'LINUX32: ON'}
{$ELSE}
  {$MESSAGE 'LINUX32: OFF'}
{$ENDIF}

// Platform Detection: Linux 64-bit
{$IFDEF LINUX64}
  {$MESSAGE 'LINUX64: ON'}
{$ELSE}
  {$MESSAGE 'LINUX64: OFF'}
{$ENDIF}

// Platform Detection: macOS (all versions)
{$IFDEF MACOS}
  {$MESSAGE 'MACOS: ON'}
{$ELSE}
  {$MESSAGE 'MACOS: OFF'}
{$ENDIF}

// Platform Detection: macOS 32-bit
{$IFDEF MACOS32}
  {$MESSAGE 'MACOS32: ON'}
{$ELSE}
  {$MESSAGE 'MACOS32: OFF'}
{$ENDIF}

// Platform Detection: macOS 64-bit
{$IFDEF MACOS64}
  {$MESSAGE 'MACOS64: ON'}
{$ELSE}
  {$MESSAGE 'MACOS64: OFF'}
{$ENDIF}

// Platform Detection: Android
{$IFDEF ANDROID}
  {$MESSAGE 'ANDROID: ON'}
{$ELSE}
  {$MESSAGE 'ANDROID: OFF'}
{$ENDIF}

// Platform Detection: Android 32-bit
{$IFDEF ANDROID32}
  {$MESSAGE 'ANDROID32: ON'}
{$ELSE}
  {$MESSAGE 'ANDROID32: OFF'}
{$ENDIF}

// Platform Detection: Android 64-bit
{$IFDEF ANDROID64}
  {$MESSAGE 'ANDROID64: ON'}
{$ELSE}
  {$MESSAGE 'ANDROID64: OFF'}
{$ENDIF}

// CPU Architecture Detection: Intel 386
{$IFDEF CPU386}
  {$MESSAGE 'CPU386: ON'}
{$ELSE}
  {$MESSAGE 'CPU386: OFF'}
{$ENDIF}

// CPU Architecture Detection: x86 (32-bit)
{$IFDEF CPUX86}
  {$MESSAGE 'CPUX86: ON'}
{$ELSE}
  {$MESSAGE 'CPUX86: OFF'}
{$ENDIF}

// CPU Architecture Detection: x64 (64-bit)
{$IFDEF CPUX64}
  {$MESSAGE 'CPUX64: ON'}
{$ELSE}
  {$MESSAGE 'CPUX64: OFF'}
{$ENDIF}

// CPU Architecture Detection: ARM
{$IFDEF CPUARM}
  {$MESSAGE 'CPUARM: ON'}
{$ELSE}
  {$MESSAGE 'CPUARM: OFF'}
{$ENDIF}

// CPU Architecture Detection: ARM 32-bit
{$IFDEF CPUARM32}
  {$MESSAGE 'CPUARM32: ON'}
{$ELSE}
  {$MESSAGE 'CPUARM32: OFF'}
{$ENDIF}

// CPU Architecture Detection: ARM 64-bit
{$IFDEF CPUARM64}
  {$MESSAGE 'CPUARM64: ON'}
{$ELSE}
  {$MESSAGE 'CPUARM64: OFF'}
{$ENDIF}

// Feature Detection: Unicode
{$IFDEF UNICODE}
  {$MESSAGE 'UNICODE: ON'}
{$ELSE}
  {$MESSAGE 'UNICODE: OFF'}
{$ENDIF}

// Feature Detection: POSIX (Portable Operating System Interface)
{$IFDEF POSIX}
  {$MESSAGE 'POSIX: ON'}
{$ELSE}
  {$MESSAGE 'POSIX: OFF'}
{$ENDIF}

// Feature Detection: NextGen ARC (Automatic Reference Counting)
{$IFDEF NEXTGEN}
  {$MESSAGE 'NEXTGEN: ON'}
{$ELSE}
  {$MESSAGE 'NEXTGEN: OFF'}
{$ENDIF}

// Feature Detection: Native Code Compilation
{$IFDEF NATIVECODE}
  {$MESSAGE 'NATIVECODE: ON'}
{$ELSE}
  {$MESSAGE 'NATIVECODE: OFF'}
{$ENDIF}

// Feature Detection: External Linker
{$IFDEF EXTERNALLINKER}
  {$MESSAGE 'EXTERNALLINKER: ON'}
{$ELSE}
  {$MESSAGE 'EXTERNALLINKER: OFF'}
{$ENDIF}

// Feature Detection: ELF Format
{$IFDEF ELF}
  {$MESSAGE 'ELF: ON'}
{$ELSE}
  {$MESSAGE 'ELF: OFF'}
{$ENDIF}

// Feature Detection: Position-Independent Code
{$IFDEF PIC}
  {$MESSAGE 'PIC: ON'}
{$ELSE}
  {$MESSAGE 'PIC: OFF'}
{$ENDIF}

// Feature Detection: Conditional Expressions
{$IFDEF CONDITIONALEXPRESSIONS}
  {$MESSAGE 'CONDITIONALExp: ON'}
{$ELSE}
  {$MESSAGE 'CONDITIONALEXPRESSIONS: OFF'}
{$ENDIF}

end.

