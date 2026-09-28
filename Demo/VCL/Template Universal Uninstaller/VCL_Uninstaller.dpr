program VCL_Uninstaller;

{--------------------------------------------------------------------------------------------------
  GabrielMoraru.com
  2026.09.25
  Universal Uninstaller. Shipped as <install folder>\System\Uninstall.exe.
  How it works: see the header of UninstallerForm.pas
--------------------------------------------------------------------------------------------------}

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  Vcl.Themes,
  Vcl.Styles,
  UninstallerForm in 'UninstallerForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.Shell in '..\..\..\FrameVCL\LightVcl.Common.Shell.pas',
  Vcl.Forms
  {$IFDEF AUTOPILOT}
  , Autopilot.Bridge.Vcl   { AI automation bridge. Debug-only: AUTOPILOT is defined only in the Debug config. Search path: see .dproj DCC_UnitSearchPath. }
  {$ENDIF}
  ;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  { The exe the user started lives in the install folder, which the uninstaller must delete. It only starts a copy of itself from the Temp folder, then ends. It never creates AppData. }
  if UninstallerForm.StartCopy then EXIT;

  { From here on this is the copy (or a build run from the IDE). Its settings stay next to its own exe, never in the settings folder of the product it removes. }
  UninstallerForm.PrepareOwnFolder;
  AppData:= TAppData.Create(UninstallerAppName);   // stackoverflow.com/questions/75449673/is-it-ok-to-create-an-object-before-application-initialize

  Application.MainFormOnTaskbar:= TRUE;
  { asPosOnly, never asFull: TLightForm.LoadForm restores the saved control states AFTER FormCreate, so with asFull the folder of a PREVIOUS run comes back out of the INI and replaces the folder this exe was started from }
  AppData.CreateMainForm(TfrmMain, frmMain, asPosOnly);

  {$IFDEF AUTOPILOT}
  StartBridge;                        { Open the named pipe so the AI automation client can drive this instance. Debug-only. }
  {$ENDIF}

  AppData.Run;

  {$IFDEF AUTOPILOT}
  StopBridge;
  {$ENDIF}
end.
