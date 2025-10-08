program FMX_Demo_AutoSizeRect;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  LightFmx.Visual.AutoSizeBoxTxt in '..\..\..\FrameFMX\LightFmx.Visual.AutoSizeBoxTxt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
