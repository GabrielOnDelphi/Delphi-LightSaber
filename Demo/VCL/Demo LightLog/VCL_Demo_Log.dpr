program VCL_Demo_Log;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  FormMain in 'FormMain.pas' {MainForm},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  {$IFDEF AUTOPILOT}Autopilot.Bridge.Vcl in 'c:\Projects\Projects AI\Autopilot for Delphi\Source\Bridge\Autopilot.Bridge.Vcl.pas',{$ENDIF}{Optional}
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  Application.Initialize;                  // Required by IDE, otherwise the Appearance and Orientation pages do not appear in Project Options.

  AppData:= TAppData.Create('Light VCL Log Demo');

  // Warning: set the style HERE, before CreateMainForm. Once the main form exists, SetStyle/TrySetStyle
  // leaks its TMainMenuBarStyleHook (AV on the next menu click) and replaces the form handle. See FormSkinsDisk.pas.
  //TStyleManager.TrySetStyle('Auric');

  Application.MainFormOnTaskbar:= TRUE;
  AppData.CreateMainForm(TMainForm, MainForm, asFull);

  { Optional: Via Autopilot, Claude can run/debug/test this program for you.
    Don't have AUTOPILOT? Undefine $AUTOPILOT or get it from www.GabrielMoraru.com }
  {$IFDEF AUTOPILOT}
   Autopilot.Bridge.Vcl.StartBridge;
  {$ENDIF}

  AppData.Run;

  {$IFDEF AUTOPILOT}
   Autopilot.Bridge.Vcl.StopBridge;
  {$ENDIF}
end.
