program ProjectColorPalette;

uses
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  MainForm in 'MainForm.pas' {FormPalette},
  LightFmx.Visual.ColorPalette in '..\..\..\FrameFMX\LightFmx.Visual.ColorPalette.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightCore.INIFile in '..\..\..\LightCore.INIFile.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo FMX Visual Log');
  AppData.CreateMainForm(TFormPalette, FormPalette, asFull);
  AppData.Run;
end.
