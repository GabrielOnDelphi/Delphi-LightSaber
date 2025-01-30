unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cbLogRam, Vcl.Grids, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  llRichLogTrack, llRichLogUtils, llRichLog, cvLog, cvLogFilter, cbAppDataForm;

type
  TMainForm = class(TLightForm)
    Panel1: TPanel;
    Panel2: TPanel;
    RichLog: TRichLog;
    Panel3: TPanel;
    Button1: TButton;
    RichLogTrckbr1: TRichLogTrckbr;
    Panel4: TPanel;
    Button2: TButton;
    LogVis: TLogVerbFilter;
    Button3: TButton;
    Button4: TButton;
    Panel5: TPanel;
    chkShowDate: TCheckBox;
    chkShowTime: TCheckBox;
    VisLog: TLogGrid;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkShowDateClick(Sender: TObject);
    procedure chkShowTimeClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
    procedure LoadForm; override;
    procedure SaveForm; override;
    procedure LateInitialize; override;
  end;

var
  MainForm: TMainForm;

implementation {$R *.dfm}
Uses cbAppData, cbLogUtils, cbLogTypes, ccINIFile, cvINIFile, ccIO, ccTextFile, cmIO, cmIO.Win, ccCore, csSystem, cbDialogs;


procedure TMainForm.FormCreate(Sender: TObject);
begin
 RichLog.Clear;
end;


procedure TMainForm.LateInitialize;
begin
  inherited;
  if AppData.RunningHome
  then LogVis.TrackBar.Min:= 0    // We allow us to see the "Debug" lines
  else LogVis.TrackBar.Min:= 1;   // We don't let uset see the "Debug" lines;
end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
 RichLog.Clear;

 RichLog.AddVerb ('AddVerb');
 RichLog.AddHint ('AddHint');
 RichLog.AddInfo ('AddInfo');
 RichLog.AddImpo ('AddImpo');
 RichLog.AddWarn ('AddWarn');
 RichLog.AddError('AddError');

 RichLog.AddEmptyRow;

 RichLog.AddBold  ('AddBold');
 RichLog.AddMsg   ('AddMsg');
 RichLog.AddMsgInt('AddMsgInt', 42);

 RichLog.AddEmptyRow;

 RichLog.AddInteger(42);
 RichLog.AddFromFile(AppData.CurFolder+ 'Test file.txt', lvrImportant);
end;



procedure TMainForm.Button2Click(Sender: TObject);
begin
 LogVis.TrackBar.Position:= 0;

 VisLog.Clear;
 VisLog.RamLog.AddDebug('AddDebug');
 VisLog.RamLog.AddVerb ('AddVerb');
 VisLog.RamLog.AddHint ('AddHint');
 VisLog.RamLog.AddInfo ('AddInfo');
 VisLog.RamLog.AddImpo ('AddImpo');
 VisLog.RamLog.AddWarn ('AddWarn');
 VisLog.RamLog.AddError('AddError');

 VisLog.RamLog.AddEmptyRow;

 VisLog.RamLog.AddBold  ('AddBold');
 VisLog.RamLog.AddMsg   ('AddMsg');
 VisLog.RamLog.AddMsgInt('AddMsgInt', 42);

 VisLog.RamLog.AddEmptyRow;

 //VisLog.Grid.ScrollBars:= ssnone;
end;



procedure TMainForm.chkShowDateClick(Sender: TObject);
begin
 VisLog.ShowDate:= chkShowDate.Checked;
end;

procedure TMainForm.chkShowTimeClick(Sender: TObject);
begin
 VisLog.ShowTime:= chkShowTime.Checked;
end;




procedure TMainForm.Button3Click(Sender: TObject);
begin
 VisLog.RamLog.SaveToFile(AppData.CurFolder+ 'LogFile.log');
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
 VisLog.RamLog.LoadFromFile(AppData.CurFolder+ 'LogFile.log');
end;




procedure TMainForm.Button5Click(Sender: TObject);
begin
 VAR VisLog2:= TLogGrid.Create(Self);
 VisLog2.Parent:= Self;
 //VisLog2.ScrollBars      := ssNone;   // Exception Notification - Project raised exception class EOSError with message 'A call to an OS function failed'.
end;





procedure TMainForm.SaveForm;
begin
  Assert(AppData <> NIL, 'AppData is gone already!');
  inherited SaveForm;

  // Save form position
  if NOT cbAppData.AppData.Initializing
  then cvINIFile.SaveForm(Self); // We don't save anything if the start up was improper!

  // Save Log verbosity
  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);
    IniFile.Write('ShowTime'   , VisLog.ShowTime);
    IniFile.Write('ShowDate'   , VisLog.ShowDate);
    IniFile.Write('Verbosity'  , Ord(VisLog.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TMainForm.LoadForm;
begin
  inherited LoadForm;

  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError := IniFile.Read('ShowOnError', TRUE);
    VisLog.ShowDate            := IniFile.Read('ShowDate', TRUE);
    VisLog.ShowTime            := IniFile.Read('ShowTime', TRUE);
    VisLog.Verbosity           := TLogVerbLvl(IniFile.Read('Verbosity', Ord(lvHints)));

    chkShowDate.Checked:= VisLog.ShowDate;
    chkShowTime.Checked:= VisLog.ShowTime;
  finally
    FreeAndNil(IniFile);
  end;
end;



end.
