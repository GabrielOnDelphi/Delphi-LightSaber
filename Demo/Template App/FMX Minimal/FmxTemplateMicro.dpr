program FmxTemplateMicro;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  cbAppDataFMX in '..\..\..\FMX\cbAppDataFMX.pas',
  cbAppDataFmxForm in '..\..\..\FMX\cbAppDataFmxForm.pas',
  cbIniFileFMX in '..\..\..\FMX\cbIniFileFMX.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX micro demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
