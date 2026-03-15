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
  AppData.CreateMainForm(TMainForm, MainForm, TRUE, TRUE, asFull);
  // Warning: Don't call TrySetStyle until the main form is visible.
  //TStyleManager.TrySetStyle('Auric');
  AppData.Run;
end.
