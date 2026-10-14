program VCL_TemplateFull;

uses
  {$IFDEF DEBUG}FastMM4, {$ENDIF}
  
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,

  FormMain in 'FormMain.pas' {MainForm},
  FormSettings in 'FormSettings.pas',
  uInitialization in 'uInitialization.pas',
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  FormSkinsDisk in '..\..\..\FrameVCL\FormSkinsDisk.pas',
  ciUpdater in '..\..\..\Updater\ciUpdater.pas',
  ciUpdaterRec in '..\..\..\Updater\ciUpdaterRec.pas',
  FormTranslSelector in '..\..\..\FrameVCL\AutoTranslator\FormTranslSelector.pas',
  FormTranslEditor in '..\..\..\FrameVCL\AutoTranslator\FormTranslEditor.pas',
  LightVcl.TranslatorAPI in '..\..\..\FrameVCL\AutoTranslator\LightVcl.TranslatorAPI.pas',
  FormTranslDeepL in '..\..\..\FrameVCL\AutoTranslator\FormTranslDeepL.pas',
  {$IFDEF AUTOPILOT}Autopilot.Bridge.Vcl in 'c:\Projects\Projects AI\Autopilot for Delphi\Source\Bridge\Autopilot.Bridge.Vcl.pas',{$ENDIF}{Optional}
  FormTranslatorIniEditor in '..\..\..\FrameVCL\AutoTranslator\FormTranslatorIniEditor.pas';

{$R *.res}

begin
  Application.Initialize;                  // Required by IDE, otherwise the Appearance and Orientation pages do not appear in Project Options.
  
  CONST
     MultiThreaded= FALSE;                 // True => Only if we need to use multithreading in the Log.
  CONST
     AppName= 'Light Template Full';       // Absolutelly critical if you use the SaveForm/LoadForm functionality. This string will be used as the name of the INI file.

  AppData:= TAppData.Create(AppName, '', MultiThreaded);

  { Skins. MUST run BEFORE CreateMainForm - LoadLastStyle raises if the main form already exists.
    Applying a style with the main form alive leaks its TMainMenuBarStyleHook (AV on the next menu click)
    and replaces the form's window handle. See FormSkinsDisk.pas. }
  if AppData.RunningFirstTime
  AND NOT AppData.RunningHome
  then LoadLastStyle('Light AmethystKamri.vsf') // 'Light AmethystKamri.vsf' has too much blue
  else LoadLastStyle();

  Application.MainFormOnTaskbar:= FALSE;   { Explicit: this app must NOT own a taskbar button }
  AppData.CreateMainForm(TMainForm, MainForm, asFull);

  { Optional: Via Autopilot, Claude can run/debug/test this program for you. For example, <press a button of this program and check the result>.
    Don't have AUTOPILOT? Undefine $AUTOPILOT or get it from www.GabrielMoraru.com }
  {$IFDEF AUTOPILOT}
   Autopilot.Bridge.Vcl.StartBridge;
  {$ENDIF}

  AppData.Run;
end.
