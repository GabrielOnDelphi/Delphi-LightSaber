program DemoScreenCapture;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  FormScreenCapture in '..\..\..\FrameFMX\FormScreenCapture.pas',
  LightFmx.Visual.ScreenCapture in '..\..\..\FrameFMX\LightFmx.Visual.ScreenCapture.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo ScreenShot');
  AppData.CreateMainForm(TForm1, Form1, asPosOnly);
  AppData.Run;
end.
