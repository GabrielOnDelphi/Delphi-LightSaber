UNIT ccCompiler;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Compiler version definitions
    https://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions
=============================================================================================================}

INTERFACE

CONST
  Delphi_Athenes = 36.0;  // Product Version: 29    Delphi 12
  Delphi_Alexa   = 35.0;  // Product Version: 28    Delphi 11
  Delphi_Sydney  = 34.0;  // Product Version: 27    Delphi 10.4
  Delphi_Rio     = 33.0;  // Product Version: 26    Delphi 10.3
  Delphi_Tokyo   = 32.0;  // Product Version: 25    Delphi 10.2
  Delphi_Berlin  = 31.0;  // Product Version: 24    Delphi 10.1
  Delphi_Seattle = 30.0;  // Product Version: 23    Delphi 10
  Delphi_XE8     = 29.0;  // Product Version: 22
  Delphi_XE7     = 28.0;  // Product Version: 21
  Delphi_XE6     = 27.0;  // Product Version: 20
  Delphi_XE5     = 26.0;  // Product Version: 19
  Delphi_XE4     = 25.0;  // Product Version: 18
  Delphi_XE3     = 24.0;  // Product Version: 17
  Delphi_XE2     = 23.0;  // Product Version: 16
  Delphi_XE      = 22.0;  // Product Version: 15
  Delphi_2010    = 21.0;
  Delphi_2009    = 20.0;
  Delphi_2007    = 18.5;
  Delphi_2006    = 18.0;
  Delphi_2005    = 17.0;
  Delphi_8Net    = 16.0;
  Delphi_7       = 15.0;

(*
  Example of usage:

  {$IF RTLVersion > Delphi_7}
    Result := 'Modern Delphi';
  {$ELSE}
    Result := 'Old Delphi';
  {$ENDIF}
*)


IMPLEMENTATION

(*
  Other alternatives:

  {$IFDEF VER350}
    Result := 'Delphi Alexa';
  {$ELSE}
    Result := 'Older Delphi';
  {$ENDIF}

  https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation_(Delphi)
*)

end.
