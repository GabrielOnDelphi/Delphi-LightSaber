program FMX_Demo_TDropDownSearchBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Visual.DropDownSearch in '..\..\..\FrameFMX\LightFmx.Visual.DropDownSearch.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo FMX TDropDownSearchBox');
  AppData.CreateMainForm(TForm1, Form1, asFull);  // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.Run;
end.
