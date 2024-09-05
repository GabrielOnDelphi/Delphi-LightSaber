program Tester_FileStream;

uses
  FastMM4,
    VCL.Forms,
    cbAppData,
    FormMain in 'FormMain.pas' {MainForm};

{$R *.res}

begin
  AppData:= TAppData.Create('BigSearch');
  AppData.CreateMainForm(TMainForm, MainForm, True, True);    // Main form
  Application.Run;

  Application.Run;
end.

