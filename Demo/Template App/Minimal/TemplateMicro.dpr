program TemplateMicro;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  cbAppData,
  cbIniFile,
  ccINIFile;

{$R *.res}

begin
  AppData:= TAppData.Create('Light Template Micro');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, TRUE, asFull);
  AppData.Run;
end.
