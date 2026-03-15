program VCL_TemplateMicro;

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF}
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LightVcl.Visual.AppData in '..\..\..\FrameVCL\LightVcl.Visual.AppData.pas',
  LightVcl.Visual.AppDataForm in '..\..\..\FrameVCL\LightVcl.Visual.AppDataForm.pas',
  LightCore.AppData in '..\..\..\LightCore.AppData.pas';

{$R *.res}

begin
  Application.Initialize;                  // Required by IDE, otherwise the Appearance and Orientation pages do not appear in Project Options.

  CONST
     MultiThreaded= FALSE;                 // True => Only if we need to use multithreading in the Log.
  AppData:= TAppData.Create('Light Template Micro', '', MultiThreaded);
  AppData.CreateMainForm(TfrmMain, TRUE, TRUE, asFull);
  // Warning: Don't call TrySetStyle until the main form is visible.
  //TStyleManager.TrySetStyle('Auric');

  AppData.Run;
end.
