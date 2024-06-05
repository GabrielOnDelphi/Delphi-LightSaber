program Tester_ccIO;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form6},
  ccAppData,
  ccIniFileVCL in '..\..\..\ccIniFileVCL.pas';

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Cubic Tester IO');
  AppData.CreateMainForm(TForm6, Form6, TRUE);
  Application.Run;
end.
