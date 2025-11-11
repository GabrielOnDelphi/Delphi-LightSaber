{.$DEFINE FMX}                //*** remove the dot to assume FMX on Windows ***

{$IFNDEF MSWINDOWS}           //no VCL outside of Windows
  {$DEFINE FMX}
{$ENDIF}

{$IF CompilerVersion >= 25}
  {$LEGACYIFEND ON}
  {$ZEROBASEDSTRINGS OFF}
{$IFEND}

{$IF CompilerVersion < 23}    //no FMX before XE2
  {$UNDEF FMX}
{$IFEND}

{$IFDEF FMX}
  {$UNDEF VCL}
{$ELSE}
  {$IFDEF MSWINDOWS}
    {$DEFINE VCL}             //if not FMX, assume VCL on Windows
  {$ENDIF}
{$ENDIF}

{$IF CompilerVersion >= 18.5}
  {$DEFINE CanInline}         //inlining record methods is unstable before D2007
{$IFEND}

{$IF CompilerVersion >= 20}
  {$DEFINE DepCom}            //deprecated directive allows comments from D2009
  {$DEFINE HasToString}
{$IFEND}

{$IF CompilerVersion >= 21}
  {$DEFINE NoRecEnumBug}      //QC 72213
  {$DEFINE HasIOUtils}
{$IFEND}

{$IF CompilerVersion >= 22}
  {$DEFINE HasTTimeZone}
  {$DEFINE HasGenerics}
  {$DEFINE HasFormatSettings}
{$IFEND}

{$IF (CompilerVersion = 26) AND Defined(NEXTGEN)}
  {$DEFINE NeedHausladenByteStringsFix}
{$IFEND}

{$IF DEFINED(VER240) AND DEFINED(MACOS)}
  {$DEFINE BrokenFMXJpegExport}
{$IFEND}

{$IFOPT Q+}
  {$DEFINE OverflowCheckingOn}
{$ENDIF}

{$IFOPT R-}
  {$DEFINE RangeCheckingOff}
{$ENDIF}
