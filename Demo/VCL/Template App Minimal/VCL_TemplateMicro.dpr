program VCL_TemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightVcl.Common.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
