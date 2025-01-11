program Tester_ccIO;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmTestIO},
  cbAppData;

{$R *.res}

begin
  Application.Initialize;
  AppData:= TAppData.Create('Light Tester IO');
  AppData.CreateMainForm(TfrmTestIO, frmTestIO, TRUE);
  Application.Run;
end.
