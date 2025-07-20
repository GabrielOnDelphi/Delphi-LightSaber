program VCL_Demo_Internet;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  WinApi.Windows,
  VCL.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Internet'); // This name is absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.
  AppData.CreateMainForm(TMainForm, MainForm, True, True, asFull);
  AppData.Run;
end.
