program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  Application.Initialize;                  // Required by IDE, otherwise the Appearance and Orientation pages do not appear in Project Options.

  AppData:= TAppData.Create('Light VCL Log Demo');

  // Warning: set the style HERE, before CreateMainForm. Once the main form exists, SetStyle/TrySetStyle
  // leaks its TMainMenuBarStyleHook (AV on the next menu click) and replaces the form handle. See FormSkinsDisk.pas.
  //TStyleManager.TrySetStyle('Auric');

  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  AppData.Run;
end.
