UNIT ccCompiler;

{=============================================================================================================
   CubicDesign
   2022.11

   Compiler version definitions
   https://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions
=============================================================================================================}

INTERFACE

CONST
  CompilerVers_Alexa   = 35.0;  // ?
  CompilerVers_Sydney  = 34.0;  // {$IFDEF VER340}
  CompilerVers_Rio     = 33.0;  // {$IFDEF VER330}
  CompilerVers_Tokyo   = 32.0;  // {$IFDEF VER320}
  CompilerVers_Berlin  = 31.0;  // {$IFDEF VER310}
  CompilerVers_Seattle = 30.0;  // {$IFDEF VER300}
  CompilerVers_XE8     = 29.0;
  CompilerVers_XE7     = 28.0;
  CompilerVers_XE6     = 27.0;
  CompilerVers_XE5     = 26.0;
  CompilerVers_XE4     = 25.0;
  CompilerVers_XE3     = 24.0;
  CompilerVers_XE2     = 23.0;
  CompilerVers_XE      = 22.0;
  CompilerVers_2010    = 21.0;
  CompilerVers_2009    = 20.0;
  CompilerVers_2007    = 18.5;
  CompilerVers_2006    = 18.0;
  CompilerVers_2005    = 17.0;
  CompilerVers_8Net    = 16.0;
  CompilerVers_7       = 15.0;

CONST
  RTLVers_Alexa   = 35.0;   // ?
  RTLVers_Sydney  = 34.0;   // Product Version: 27
  RTLVers_Rio     = 33.0;   // Product Version: 26
  RTLVers_Tokyo   = 32.0;   // Product Version: 25
  RTLVers_Berlin  = 31.0;   // Product Version: 24
  RTLVers_Seattle = 30.0;   // Product Version: 23
  RTLVers_XE8     = 29.0;   // Product Version: 22
  RTLVers_XE7     = 28.0;   // Product Version: 21
  RTLVers_XE6     = 27.0;   // Product Version: 20
  RTLVers_XE5     = 26.0;   // Product Version: 19
  RTLVers_XE4     = 25.0;   // Product Version: 18
  RTLVers_XE3     = 24.0;   // Product Version: 17
  RTLVers_XE2     = 23.0;   // Product Version: 16
  RTLVers_XE      = 22.0;   // Product Version: 15
  RTLVers_2010    = 21.0;
  RTLVers_2009    = 20.0;
  RTLVers_2007Net = 19.0;
  RTLVers_2007    = 18.5;
  RTLVers_2006    = 18.0;
  RTLVers_2005    = 17.0;
  RTLVers_8Net    = 16.0;
  RTLVers_7       = 15.0;

IMPLEMENTATION


(*
  Example of usage:

  {$IF RTLVersion > RTLVersionXE7}
    Result := 'Modern Delphi';
  {$ELSE}
    Result := 'Old Delphi';
  {$ENDIF}

*)
end.
