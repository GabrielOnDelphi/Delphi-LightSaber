program VCL_Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\..\FrameVCL\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\..\FrameVCL\FormTranslSelector.pas' {frmTranslSelector},
  LighCore.AppData in '..\..\..\LighCore.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightCom.AppDataForm.pas',
  LightVcl.Common.IniFile in '..\..\..\FrameVCL\LightCom.IniFile.pas',
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightCom.AppData.pas',
  LightVcl.LogForm in '..\..\..\FrameVCL\LightVcl.LogForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
