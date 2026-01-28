program FMX_Demo_SearchBoxes;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Visual.DropDownSearch in '..\..\..\FrameFMX\LightFmx.Visual.DropDownSearch.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Common.IniFile in '..\..\..\FrameFMX\LightFmx.Common.IniFile.pas',
  LightFmx.Visual.SearchListBox in '..\..\..\FrameFMX\LightFmx.Visual.SearchListBox.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Demo FMX TDropDownSearchBox');
  AppData.CreateMainForm(TForm1, Form1, asFull);  // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.Run;
end.
