program VCL_Demo_VisualControls;

uses
  {$IFDEF DEBUG} FastMM4, {$ENDIF }
  MainForm in 'MainForm.pas' {frmMain},
  ccAppData in '..\..\..\ccAppData.pas',
  LightCom.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightCom.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightCom.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Visual Controls');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
