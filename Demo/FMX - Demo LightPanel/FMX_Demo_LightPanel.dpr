program FMX_Demo_LightPanel;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormTest in 'FormTest.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
