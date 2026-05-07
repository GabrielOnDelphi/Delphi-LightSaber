program FMX_Demo_PlatformDetection;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form3},
  PlatformTest in '..\PlatformTest.pas',
  LightFmx.Common.AppData.Form in '..\..\..\FrameFMX\LightFmx.Common.AppData.Form.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo FMX PlatformDetection');
  AppData.CreateMainForm(TForm3, Form3, asPosOnly);
  AppData.Run;
end.
