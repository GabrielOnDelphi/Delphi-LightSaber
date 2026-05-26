program UpdateDemo;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Vcl.Themes,
  Vcl.Styles,
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  ciUpdater in '..\..\..\ciUpdater.pas',
  ciUpdaterRec in '..\..\..\ciUpdaterRec.pas',
  FormUpdaterNotifier in '..\..\..\FrameVCL\FormUpdaterNotifier.pas';

{$R *.res}

begin
  CONST
     MultiThreaded= FALSE;                // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Demo Updater';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);

  Updater:= TUpdater.Create(UpdaterDemoURL);
  AppData.CreateMainForm(TFrmUpdater, TRUE, TRUE, asFull);
  Updater.CheckForNews;

  AppData.Run;
end.


