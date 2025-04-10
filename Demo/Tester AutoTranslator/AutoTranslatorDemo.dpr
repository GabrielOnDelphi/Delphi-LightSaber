program AutoTranslatorDemo;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF}
  cbAppDataVCL,
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\FormTranslSelector.pas' {frmTranslSelector};

{$R *.res}

begin
  AppData:= TAppData.Create('Light Translator Demo');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  AppData.Run;
end.
