unit MainForm;

interface

uses
  System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService, FMX.Grid,
  LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    btnShowErr: TButton;
    chkSome: TCheckBox;
    RadioButton1: TRadioButton;
    lblIniFile: TLabel;
    Label1: TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure btnShowErrClick(Sender: TObject);
  private
    LogViewer: TLogViewer;
  public
    procedure FormPostInitialize; override;
    procedure FormPreRelease;     override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES
   LightCore, LightCore.Types, LightCore.AppData, LightCore.LogTypes, LightCore.LogRam, LightFmx.Common.AppData;



procedure TForm1.FormCreate(Sender: TObject);
begin
  // FormPostInitialize is a much better place to initialize your code!
end;


procedure TForm1.FormPostInitialize;
begin
  inherited; // This will load the form's state from disk
  
  // Here we create a log viewer dynamically but we could also just drag and drop a component
  LogViewer:= TLogViewer.Create(Self);
  LogViewer.Parent:= Self;
  LogViewer.Align:= TAlignLayout.Bottom;
  LogViewer.Verbosity:= LightCore.LogTypes.lvDebug;
  LogViewer.Height:= 300;
  LogViewer.ObserveAppDataLog;   // This connects the LogViewer to the RamLog

  // Now we can show something in the log...
  LogViewer.RamLog.AddWarn('Warning 1');       // ...via the visual log viewer
  AppData.LogWarn('Warning 2');                // ...or by sending the message directly to AppData object
  
  lblIniFile.Text:= 'Your INI file is here: ' + CRLF+ AppData.IniFile;  
end;


procedure TForm1.FormPreRelease;
begin
  Caption:= 'Bye Bye!'; // This place is a better than FormClose/FormDestroy to deinitialize code.
  inherited;
end;


procedure TForm1.btnShowErrClick(Sender: TObject);
begin
  AppData.LogError('Some fancy error!');
end;

end.
