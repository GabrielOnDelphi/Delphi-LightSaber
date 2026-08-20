program VCL_Demo_AutoTranslator;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslatorIniEditor in '..\FormTranslatorIniEditor.pas',
  FormTranslDeepL in '..\FormTranslDeepL.pas',
  FormTranslEditor in '..\FormTranslEditor.pas',
  FormTranslSelector in '..\FormTranslSelector.pas',
  LightVcl.TranslatorAPI in '..\LightVcl.TranslatorAPI.pas',
  LightVcl.Visual.AppData in '..\..\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  FormUniversalEula in '..\..\FormUniversalEula.pas',
  Vcl.Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= TRUE;

  AppData:= TAppData.Create('Light Demo Translator');
  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TMainForm, MainForm, asFull);
  AppData.Run;
end.
