program FMX_Demo_AutoSizeRect;

uses
  {$IF Defined(MSWINDOWS)}
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  FormAutoSize in 'FormAutoSize.pas' {Form1},
  LightCore.AppData in '..\..\..\LightCore.AppData.pas',
  LightFmx.Common.AppData in '..\..\..\FrameFMX\LightFmx.Common.AppData.pas',
  LightFmx.Visual.AutosizeBoxText in '..\..\..\FrameFMX\LightFmx.Visual.AutosizeBoxText.pas',
  LightFmx.Visual.AutoSizeBox in '..\..\..\FrameFMX\LightFmx.Visual.AutoSizeBox.pas',
  LightFmx.Visual.AutoSizeBoxImg in '..\..\..\FrameFMX\LightFmx.Visual.AutoSizeBoxImg.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Template FMX Demo AutoSizeRect');
  AppData.CreateMainForm(TForm1, Form1, asFull);  // Change AutoState from asFull to asNone if you don't want to save form's state to disk.
  AppData.Run;
end.
