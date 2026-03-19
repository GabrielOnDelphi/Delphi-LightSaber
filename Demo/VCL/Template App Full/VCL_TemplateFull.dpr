program VCL_TemplateFull;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}
  {$IFDEF RELEASE}madExcept, madLinkDisAsm, madListModules, {$ENDIF}
  
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,

  FormMain in 'FormMain.pas' {MainForm},
  FormSettings in 'FormSettings.pas',
  uInitialization in 'uInitialization.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  ciUpdater in '..\..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\..\Updater\ciUpdaterRec.pas',
  FormTranslSelector in '..\..\..\FrameVCL\AutoTranslator\FormTranslSelector.pas',
  FormTranslEditor in '..\..\..\FrameVCL\AutoTranslator\FormTranslEditor.pas',
  LightVcl.TranslatorAPI in '..\..\..\FrameVCL\AutoTranslator\LightVcl.TranslatorAPI.pas',
  FormTranslDeepL in '..\..\..\FrameVCL\AutoTranslator\FormTranslDeepL.pas',
  FormTranslatorIniEditor in '..\..\..\FrameVCL\AutoTranslator\FormTranslatorIniEditor.pas';

{$R *.res}

begin
  Application.Initialize;                  // Required by IDE, otherwise the Appearance and Orientation pages do not appear in Project Options.
  
  CONST
     MultiThreaded= FALSE;                 // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);
  AppData.CreateMainForm(TMainForm, MainForm, FALSE, TRUE, asFull);

  // Warning: Don't call TrySetStyle until the main form is visible.
  //TStyleManager.TrySetStyle('Amakrits');

  AppData.Run;
end.
