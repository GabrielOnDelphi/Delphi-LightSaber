program VCL_TemplateMicro;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}

  // madExcept is a commercial third-party library that does NOT ship with LightSaber.
  // The gate is a FEATURE symbol, not a build-configuration name, so that anyone who
  // downloads LightSaber can still build this template in Release: ($IFDEF madshi) is
  // false unless you own madExcept and add "madshi" to DCC_Define in your own .dproj.
  // Gated on ($IFDEF RELEASE) instead - as it was until 2026-08-01 - Release fails to
  // compile for everybody else with "File not found: madExcept.dcu".
  {$IFDEF madshi}
  madExcept, madLinkDisAsm, madListModules, {$ENDIF}
  
  Vcl.Themes,
  Vcl.Styles,
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  CONST
     MultiThreaded= FALSE;                 // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Micro';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);

  AppData.Run;
end.
