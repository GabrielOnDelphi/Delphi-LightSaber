program Tester_ccIO;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form6},
  cbAppData;

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Light Tester IO');
  AppData.CreateMainForm(TForm6, Form6, TRUE);
  Application.Run;
end.
