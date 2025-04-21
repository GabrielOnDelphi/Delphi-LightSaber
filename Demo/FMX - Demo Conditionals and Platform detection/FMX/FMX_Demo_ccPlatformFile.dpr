program FMX_Demo_ccPlatformFile;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form3},
  PlatformTest in '..\PlatformTest.pas',
  LightFMX.AppData.Form in '..\..\..\FrameFMX\LightFMX.AppData.Form.pas',
  LightFMX.IniFile in '..\..\..\FrameFMX\LightFMX.IniFile.pas',
  LightFMX.AppData in '..\..\..\FrameFMX\LightFMX.AppData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  AppData.Run;
end.
