program VCL_Demo_PlatformDetection;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form3},
  ccPlatformFile in '..\..\..\ccPlatformFile.pas',
  PlatformTest in '..\PlatformTest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
