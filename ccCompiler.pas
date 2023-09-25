UNIT ccCompiler;

{=============================================================================================================
   CubicDesign
   2022.11

   Compiler version definitions
   https://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions
=============================================================================================================}

INTERFACE

CONST
  CompilerVers_Alexa   = 35.0;  //
  CompilerVers_Sydney  = 34.0;  // Product Version: 27
  CompilerVers_Rio     = 33.0;  // Product Version: 26
  CompilerVers_Tokyo   = 32.0;  // Product Version: 25
  CompilerVers_Berlin  = 31.0;  // Product Version: 24
  CompilerVers_Seattle = 30.0;  // Product Version: 23
  CompilerVers_XE8     = 29.0;  // Product Version: 22
  CompilerVers_XE7     = 28.0;  // Product Version: 21
  CompilerVers_XE6     = 27.0;  // Product Version: 20
  CompilerVers_XE5     = 26.0;  // Product Version: 19
  CompilerVers_XE4     = 25.0;  // Product Version: 18
  CompilerVers_XE3     = 24.0;  // Product Version: 17
  CompilerVers_XE2     = 23.0;  // Product Version: 16
  CompilerVers_XE      = 22.0;  // Product Version: 15
  CompilerVers_2010    = 21.0;
  CompilerVers_2009    = 20.0;
  CompilerVers_2007    = 18.5;
  CompilerVers_2006    = 18.0;
  CompilerVers_2005    = 17.0;
  CompilerVers_8Net    = 16.0;
  CompilerVers_7       = 15.0;



IMPLEMENTATION


(*
  Example of usage:

  {$IF RTLVersion > CompilerVers_7}
    Result := 'Modern Delphi';
  {$ELSE}
    Result := 'Old Delphi';
  {$ENDIF}



   {$IFDEF VER340}
   {$IFDEF VER330}
   {$IFDEF VER320}
   {$IFDEF VER310}
   {$IFDEF VER300}

*)
end.
