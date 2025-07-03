unit FormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Layouts, FMX.StdCtrls, FMX.DialogService, FMX.Grid.Style, FMX.Presentation.Factory, FMX.Presentation.Style, FMX.ScrollBox, FMX.Grid,
  LightCore.INIFile, LightCore.LogRam,
  LightFmx.Common.AppData, LightFmx.Common.AppData.Form, LightFmx.Common.LogViewer, LightFmx.Common.LogFilter;

TYPE
  TMainForm = class(TLightForm)
    Layout1: TLayout;
    Button1: TButton;
    laySettings: TLayout;
    chkShowDate: TCheckBox;
    chkShowTime: TCheckBox;
    btnClear: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    LogViewer: TLogViewer;
    LogVerbFilter1: TLogVerbFilter;
    chkScrollDown: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure chkShowDateChange(Sender: TObject);
    procedure chkShowTimeChange(Sender: TObject);
    procedure chkScrollDownChange(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure Layout1Click(Sender: TObject);
  private
  public
   procedure FormPostInitialize; override;
  end;

VAR
  MainForm: TMainForm;


IMPLEMENTATION
{$R *.fmx}

USES LightCore.AppData;




procedure TMainForm.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited FormPostInitialize;           // This will load the form's state from disk
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




procedure TMainForm.Layout1Click(Sender: TObject);
begin

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
  LogViewer.RamLog.SaveToFile(AppData.ExeFolder+ 'LogFile.log');
end;


procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  LogViewer.RamLog.Clear;
  LogViewer.RamLog.LoadFromFile(AppData.ExeFolder+ 'LogFile.log');
end;


end.
