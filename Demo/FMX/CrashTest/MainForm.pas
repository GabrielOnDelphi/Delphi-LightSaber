unit MainForm;

interface

uses
  System.Classes, System.SysUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    btnCrashPascal: TButton;
    btnCrashAV: TButton;
    lblIniFile: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCrashPascalClick(Sender: TObject);
    procedure btnCrashAVClick(Sender: TObject);
  private
    LogViewer: TLogViewer;
  public
    procedure FormPreRelease; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION

{$R *.fmx}

USES
   LightCore.AppData, LightCore.LogTypes, LightFmx.Common.AppData;


procedure TForm1.FormCreate(Sender: TObject);
begin
  // Visual log viewer, wired to the global RamLog (bottom strip).
  LogViewer:= TLogViewer.Create(Self);
  LogViewer.Parent:= Self;
  LogViewer.Align:= TAlignLayout.Bottom;
  LogViewer.Verbosity:= LightCore.LogTypes.lvDebug;
  LogViewer.Height:= 90;
  LogViewer.ObserveAppDataLog;

  AppData.LogInfo('CrashTest started. Exceptions -> Documents\CrashTest-Exceptions.log');
  lblIniFile.Text:= 'INI: ' + AppData.IniFile;
end;


procedure TForm1.btnCrashPascalClick(Sender: TObject);
begin
  AppData.LogError('Button: raising a deliberate Pascal exception');
  raise Exception.Create('Deliberate Pascal test crash - light-bug-Android acceptance 2026-07-22');
end;


procedure TForm1.btnCrashAVClick(Sender: TObject);
var
  P: PInteger;
begin
  AppData.LogError('Button: dereferencing NIL (deliberate access violation)');
  P:= nil;
  P^:= 42;   // EAccessViolation on Windows; SIGSEGV -> EAccessViolation or native tombstone on Android
end;


procedure TForm1.FormPreRelease;
begin
  inherited;
end;

end.
