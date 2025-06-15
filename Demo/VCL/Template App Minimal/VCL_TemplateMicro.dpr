program VCL_TemplateMicro;

uses
  {$IFDEF DEBUG}FastMM4, {$ENDIF }
  MainForm in 'MainForm.pas' {frmMain},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;
  
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
