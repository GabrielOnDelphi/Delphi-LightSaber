program VCL_TemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
