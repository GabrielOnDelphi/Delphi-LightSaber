program VCL_Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\..\FrameVCL\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\..\FrameVCL\FormTranslSelector.pas' {frmTranslSelector},
  LightVcl.Common.AppData in '..\..\..\FrameVCL\LightVcl.Common.AppData.pas',
  LightVcl.Common.AppDataForm in '..\..\..\FrameVCL\LightVcl.Common.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
