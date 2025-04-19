program TemplateMicro_Fmx;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.AppData in '..\..\..\FrameFMX\LightFMX.AppData.pas',
  LightFmx.DialogsDesktop in '..\..\..\FrameFMX\LightFMX.DialogsDesktop.pas',
  LightFMX.IniFile in '..\..\..\FrameFMX\LightFMX.IniFile.pas',
  LightFMX.AppData.Form in '..\..\..\FrameFMX\LightFMX.AppData.Form.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX micro demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
