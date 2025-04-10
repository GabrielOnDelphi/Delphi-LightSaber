program FmxTemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.AppData in '..\..\..\FMX\LightFMX.AppData.pas',
  LightFMX.AppDataForm in '..\..\..\FMX\LightFMX.AppDataForm.pas',
  cbIniFileFMX in '..\..\..\FMX\cbIniFileFMX.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX micro demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
