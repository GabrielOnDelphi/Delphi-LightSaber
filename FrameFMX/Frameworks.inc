// Define FRAMEWORK_VCL and FRAMEWORK_FMX for Delphi versions prior to 11.1
  {$IF CompilerVersion < 35.0} // Delphi 11.1 = CompilerVersion 35.0
    {$IFDEF MSWINDOWS}
      {$DEFINE FRAMEWORK_VCL}
    {$ELSE}
      {$DEFINE FRAMEWORK_FMX}
    {$ENDIF}
  {$ENDIF}

// For Delphi 11.1 and later, FRAMEWORK_VCL and FRAMEWORK_FMX are already defined
