unit FormMain;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Controls.Presentation, FMX.Layouts, FMX.StdCtrls, FMX.DialogService,
  LightCore.LogRam,
  LightFmx.Common.AppData, LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer, LightFmx.Common.LogFilter,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox;

TYPE
  TMainForm = class(TLightForm)
    btnClear       : TButton;
    btnLoad        : TButton;
    btnSave        : TButton;
    Button1        : TButton;
    chkScrollDown  : TCheckBox;
    chkShowDate    : TCheckBox;
    chkShowTime    : TCheckBox;
    layBottom      : TLayout;
    laySettings    : TLayout;
    LogFilter      : TLogVerbFilter;
    LogViewer      : TLogViewer;
    procedure Button1Click        (Sender: TObject);
    procedure btnSaveClick        (Sender: TObject);
    procedure btnLoadClick        (Sender: TObject);
    procedure chkShowDateChange   (Sender: TObject);
    procedure chkShowTimeChange   (Sender: TObject);
    procedure chkScrollDownChange (Sender: TObject);
    procedure btnClearClick       (Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

VAR
  MainForm: TMainForm;


IMPLEMENTATION
{$R *.fmx}

USES LightCore.AppData;



procedure TMainForm.FormCreate(Sender: TObject);
begin
  LogViewer.ObserveAppDataLog;
  {
  if AppData.RunningHome
  then LogViewer.TrackBar.Min:= 0    // We allow us to see the "Debug" lines
  else LogViewer.TrackBar.Min:= 1;   // We don't let uset see the "Debug" lines;  }

 {In case we want to auto-create it:
    LogViewer: TLogViewer;
    LogViewer:= TLogViewer.Create(Self);
    LogViewer.Parent:= Self;
    LogViewer.Align:= TAlignLayout.Client;
    LogViewer.Visible:= TRUE;}
end;



{-------------------------------------------------------------------------------------------------------------
   GRID LOG
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.Button1Click(Sender: TObject);
begin
 //LogVis.TrackBar.Position:= 0;

 LogViewer.RamLog.AddImpo('This is TLogViewer based on TStringGrid');
 LogViewer.RamLog.AddEmptyRow;

 LogViewer.RamLog.AddDebug('AddDebug');
 LogViewer.RamLog.AddVerb ('AddVerb');
 LogViewer.RamLog.AddHint ('AddHint');
 LogViewer.RamLog.AddInfo ('AddInfo');
 LogViewer.RamLog.AddImpo ('AddImpo');
 LogViewer.RamLog.AddWarn ('AddWarn');
 LogViewer.RamLog.AddError('AddError');

 LogViewer.RamLog.AddEmptyRow;

 LogViewer.RamLog.AddBold  ('AddBold');
 LogViewer.RamLog.AddMsg   ('AddMsg');
 LogViewer.RamLog.AddMsgInt('AddMsgInt', 42);
end;



procedure TMainForm.chkScrollDownChange(Sender: TObject);
begin
  LogViewer.AutoScroll:= chkScrollDown.isChecked;
end;



procedure TMainForm.btnClearClick(Sender: TObject);
begin
  LogViewer.Clear;
end;


procedure TMainForm.chkShowDateChange(Sender: TObject);
begin
  LogViewer.ShowDate:= chkShowDate.isChecked;
end;


procedure TMainForm.chkShowTimeChange(Sender: TObject);
begin
  LogViewer.ShowTime:= chkShowTime.isChecked;
end;



procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  LogViewer.RamLog.SaveToFile(Appdata.AppFolder+ 'LogFile.log');
end;


procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  LogViewer.RamLog.Clear;
  LogViewer.RamLog.LoadFromFile(Appdata.AppFolder+ 'LogFile.log');
end;


end.
