program FMX_Demo_MessageBoxes;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFMX.AppData in '..\..\..\FrameFMX\LightFMX.AppData.pas',
  LightFMX.AppData.Form in '..\..\..\FrameFMX\LightFMX.AppData.Form.pas',
  LightFMX.DropDownSearch in '..\..\..\FrameFMX\LightFMX.DropDownSearch.pas',
  LightFMX.IniFile in '..\..\..\FrameFMX\LightFMX.IniFile.pas',
  ccINIFile in '..\..\..\ccINIFile.pas',
  LightFMX.Dialogs in '..\..\..\FrameFMX\LightFMX.Dialogs.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light FMX Messages demo');
  AppData.CreateMainForm(TForm1, Form1, TRUE);
  AppData.Run;
end.
