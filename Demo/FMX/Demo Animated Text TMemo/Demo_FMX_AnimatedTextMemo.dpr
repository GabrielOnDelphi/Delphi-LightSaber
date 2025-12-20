program Demo_FMX_AnimatedTextMemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightFmx.Visual.AnimatedMemo in 'C:\Projects\LightSaber\FrameFMX\LightFmx.Visual.AnimatedMemo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
