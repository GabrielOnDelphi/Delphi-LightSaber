program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  cbAppDataFMX in '..\..\..\FMX\cbAppDataFMX.pas',
  cbAppDataFmxForm in '..\..\..\FMX\cbAppDataFmxForm.pas',
  cbIniFileFMX in '..\..\..\FMX\cbIniFileFMX.pas';
  //ccINIFile in '..\..\..\ccINIFile.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX micro demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
