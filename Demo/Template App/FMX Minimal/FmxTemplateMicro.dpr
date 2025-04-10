program FmxTemplateMicro;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.AppData in '..\..\..\FrameFMX\LightFMX.AppData.pas',
  LightFMX.AppData.Forms in '..\..\..\FrameFMX\LightFMX.AppData.Forms.pas',
  LightFmx.DialogsDesktop in '..\..\..\FrameFMX\LightFmx.DialogsDesktop.pas',
  LightFMX.IniFile in '..\..\..\FrameFMX\LightFMX.IniFile.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX micro demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
