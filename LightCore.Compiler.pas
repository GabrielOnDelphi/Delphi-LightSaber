UNIT LightCore.Compiler;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Compiler version definitions

   Maps RTLVersion to human-readable Delphi version names.
   Use these constants with {$IF RTLVersion >= Delphi_XXX for version-specific code.

   Reference: https://docwiki.embarcadero.com/RADStudio/Athens/en/Compiler_Versions
=============================================================================================================}

INTERFACE

{ $I Frameworks.inc}

CONST
  { Modern Delphi versions (supported) }
  Delphi_Yukon   = 37.0;  // Product Version: 30    Delphi 13 (Florence)
  Delphi_Athens  = 36.0;  // Product Version: 29    Delphi 12 (Athens)
  Delphi_Alex    = 35.0;  // Product Version: 28    Delphi 11 (Alexandria)
  Delphi_Sydney  = 34.0;  // Product Version: 27    Delphi 10.4 (Sydney)
  Delphi_Rio     = 33.0;  // Product Version: 26    Delphi 10.3 (Rio) - Minimum supported version

  { Legacy Delphi versions (compatibility only) }
  Delphi_Tokyo   = 32.0;  // Product Version: 25    Delphi 10.2 (Tokyo)
  Delphi_Berlin  = 31.0;  // Product Version: 24    Delphi 10.1 (Berlin)
  Delphi_Seattle = 30.0;  // Product Version: 23    Delphi 10.0 (Seattle)
  Delphi_XE8     = 29.0;  // Product Version: 22
  Delphi_XE7     = 28.0;  // Product Version: 21
  Delphi_XE6     = 27.0;  // Product Version: 20
  Delphi_XE5     = 26.0;  // Product Version: 19
  Delphi_XE4     = 25.0;  // Product Version: 18
  Delphi_XE3     = 24.0;  // Product Version: 17
  Delphi_XE2     = 23.0;  // Product Version: 16
  Delphi_XE      = 22.0;  // Product Version: 15
  Delphi_2010    = 21.0;  // Product Version: 14
  Delphi_2009    = 20.0;  // Product Version: 12
  Delphi_2007    = 18.5;  // Product Version: 11
  Delphi_2006    = 18.0;  // Product Version: 10
  Delphi_2005    = 17.0;  // Product Version: 9
  Delphi_8Net    = 16.0;  // Product Version: 8
  Delphi_7       = 15.0;  // Product Version: 7

  { Backward compatibility aliases }
  Delphi_Alexa   = Delphi_Alex;     

(*
  Example usage:

  {$IF RTLVersion >= Delphi_Rio}
    Result := 'Supported Delphi version';
  {$ELSE}
    {$MESSAGE ERROR 'Delphi version too old. Minimum required: Delphi Rio (10.3)'}
  {$ENDIF}

  {$IF RTLVersion >= Delphi_Athens}
    // Use Delphi 12+ features
  {$ENDIF}
*)


IMPLEMENTATION

(*
  Alternative approach using VERxxx symbols:

  {$IFDEF VER370}
    Result := 'Delphi 13 Yukon';
  {$ELSEIF DEFINED(VER360)}
    Result := 'Delphi 12 Athens';
  {$ELSEIF DEFINED(VER350)}
    Result := 'Delphi 11 Alexandria';
  {$ELSE}
    Result := 'Older Delphi';
  {$ENDIF}

  Reference: https://docwiki.embarcadero.com/RADStudio/Athens/en/Conditional_compilation_(Delphi)
*)

end.
