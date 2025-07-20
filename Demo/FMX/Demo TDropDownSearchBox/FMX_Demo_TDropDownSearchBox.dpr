program FMX_Demo_TDropDownSearchBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LightFmx.Visual.DropDownSearch in '..\..\..\FrameFMX\LightFmx.Visual.DropDownSearch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
