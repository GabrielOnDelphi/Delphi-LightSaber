program AutoTranslatorDemo;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Forms,
  FormMain in 'FormMain.pas' {MainForm},
  FormTranslEditor in '..\..\FormTranslEditor.pas' {frmTranslEditor},
  FormTranslSelector in '..\..\FormTranslSelector.pas' {frmTranslSelector},
  cbAppData in '..\..\cbAppData.pas',
  cbTranslate in '..\..\cbTranslate.pas';

{$R *.res}

begin
  AppData:= TAppData.Create('Light Translator Demo');
  AppData.CreateMainForm(TMainForm, MainForm, TRUE);
  AppData.Run;
end.
