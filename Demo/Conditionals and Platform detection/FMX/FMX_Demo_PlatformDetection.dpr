program FMX_Demo_PlatformDetection;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form3},
  PlatformTest in '..\PlatformTest.pas',
  LightFMX.lbAppData.Form in '..\..\..\FrameFMX\LightFMX.lbAppData.Form.pas',
  LightFMX.lbIniFile in '..\..\..\FrameFMX\LightFMX.lbIniFile.pas',
  LightFMX.lbAppData in '..\..\..\FrameFMX\LightFMX.lbAppData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  AppData.Run;
end.
