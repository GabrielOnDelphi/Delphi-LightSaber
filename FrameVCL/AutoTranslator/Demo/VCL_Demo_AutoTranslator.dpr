program VCL_Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslDeepL in '..\FormTranslDeepL.pas',
  FormTranslEditor in '..\FormTranslEditor.pas',
  FormTranslSelector in '..\FormTranslSelector.pas',
  LightVcl.Translate in '..\LightVcl.Translate.pas',
  LightVcl.TranslatorAPI in '..\LightVcl.TranslatorAPI.pas',
  LightCore.AppData in '..\..\LightCore.AppData.pas',
  LightVcl.Visual.AppData in '..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  FormTranslatorIniEditor in '..\FormTranslatorIniEditor.pas' {frmTranslatorIniEditor},
  LightVcl.Visual.AppDataForm in '..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
