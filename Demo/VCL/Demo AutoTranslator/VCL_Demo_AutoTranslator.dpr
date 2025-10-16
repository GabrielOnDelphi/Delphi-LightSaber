program VCL_Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\..\FrameVCL\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\..\FrameVCL\FormTranslSelector.pas' {frmTranslSelector},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
