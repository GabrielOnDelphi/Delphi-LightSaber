unit MainForm;

interface

uses
  System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService, FMX.Grid,
  LightCore.LogRam, LightFmx.Common.AppData, LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer;

TYPE
  TForm1 = class(TLightForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    procedure FormCreate  (Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

USES LightCore.AppData, LightCore.LogTypes;



procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;


procedure TForm1.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!

  //Optional: assign a viewer to the log
  LogViewer:= TLogViewer.Create(Self);
  LogViewer.Parent:= Self;
  LogViewer.Align:= TAlignLayout.Bottom;
  LogViewer.Verbosity:= LightCore.LogTypes.lvDebug;
  LogViewer.Height:= 300;
  LogViewer.ObserveAppDataLog;   // This connects the LogViewer to the RamLog

  LogViewer.RamLog.AddWarn('Hi1');
  LogViewer.RamLog.AddWarn('Hi2');
  AppData  .RamLog.AddWarn('Hi3');
  inherited;           // This will load the form's state from disk
end;


procedure TForm1.FormPreRelease;
begin
  Caption:= 'Bye Bye!';
  inherited;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  AppData.LogImpo('Something');
end;

end.
