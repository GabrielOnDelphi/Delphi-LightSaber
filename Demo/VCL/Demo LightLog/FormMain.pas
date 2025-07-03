UNIT FormMain;

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Grids,
  LightVcl.Visual.RichLogTrack, LightVcl.Visual.RichLogUtils, LightVcl.Visual.RichLog, LightVcl.Common.LogViewer, LightVcl.Common.LogFilter, LightVcl.Common.AppDataForm, LightCore.LogRam;

TYPE
  TMainForm = class(TLightForm)
    Panel1: TPanel;
    Panel2: TPanel;
    RichLog: TRichLog;
    Panel3: TPanel;
    Button1: TButton;
    RichLogTrckbr1: TRichLogTrckbr;
    Panel4: TPanel;
    btnTest2: TButton;
    LogVis: TLogVerbFilter;
    btnSave: TButton;
    btnLoad: TButton;
    Panel5: TPanel;
    chkShowDate: TCheckBox;
    chkShowTime: TCheckBox;
    btnLoopTest: TButton;
    GridLog: TLogViewer;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkShowDateClick(Sender: TObject);
    procedure chkShowTimeClick(Sender: TObject);
    procedure btnLoopTestClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
  public
    procedure LoadForm; override;
    procedure SaveForm; override;
    procedure FormPostInitialize; override;
  end;

var
  MainForm: TMainForm;

IMPLEMENTATION 
{$R *.dfm}

USES
  LightCore.AppData, LightVcl.Common.AppData, LightCore.LogTypes, LightCore.INIFile, LightVcl.Common.Dialogs;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  //RichLog.Clear;
end;


procedure TMainForm.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited FormPostInitialize;           // This will load the form's state from disk
  if AppData.RunningHome
  then LogVis.TrackBar.Min:= 0    // We allow us to see the "Debug" lines
  else LogVis.TrackBar.Min:= 1;   // We don't let uset see the "Debug" lines;
end;




procedure TMainForm.SaveForm;
begin
  Assert(AppData <> NIL, 'AppData is gone already!');
  inherited SaveForm;

  // Save Log verbosity
  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);
    IniFile.Write('ShowTime'   , GridLog.ShowTime);
    IniFile.Write('ShowDate'   , GridLog.ShowDate);
    IniFile.Write('Verbosity'  , Ord(GridLog.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TMainForm.LoadForm;
begin
  inherited LoadForm;

  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError:= IniFile.Read('ShowOnError', TRUE);
    GridLog.ShowDate          := IniFile.Read('ShowDate', TRUE);
    GridLog.ShowTime          := IniFile.Read('ShowTime', TRUE);
    GridLog.Verbosity         := TLogVerbLvl(IniFile.Read('Verbosity', Ord(lvHints)));

    chkShowDate.Checked:= GridLog.ShowDate;
    chkShowTime.Checked:= GridLog.ShowTime;
  finally
    FreeAndNil(IniFile);
  end;
end;




{-------------------------------------------------------------------------------------------------------------
   RICH LOG
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.Button1Click(Sender: TObject);
begin
 RichLog.Clear;
 RichLog.AddImpo ('This is TRichLog based on TRichEdit');
 RichLog.AddEmptyRow;

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
 RichLog.AddFromFile(AppData.ExeFolder+ 'Test file.txt', lvrImportant);
end;




{-------------------------------------------------------------------------------------------------------------
   GRID LOG
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.btnTest2Click(Sender: TObject);
begin
 LogVis.TrackBar.Position:= 0;

 GridLog.RamLog.AddImpo('This is TLogViewer based on TStringGrid');
 GridLog.RamLog.AddEmptyRow;

 GridLog.RamLog.AddDebug('AddDebug');
 GridLog.RamLog.AddVerb ('AddVerb');
 GridLog.RamLog.AddHint ('AddHint');
 GridLog.RamLog.AddInfo ('AddInfo');
 GridLog.RamLog.AddImpo ('AddImpo');
 GridLog.RamLog.AddWarn ('AddWarn');
 GridLog.RamLog.AddError('AddError');

 GridLog.RamLog.AddEmptyRow;

 GridLog.RamLog.AddBold  ('AddBold');
 GridLog.RamLog.AddMsg   ('AddMsg');
 GridLog.RamLog.AddMsgInt('AddMsgInt', 42);
end;


procedure TMainForm.btnLoopTestClick(Sender: TObject);
begin
  GridLog.RamLog.UnregisterLogObserver; // Prevent updating the GUI until all lines are put in the RamLog

  VAR Count:= GridLog.RamLog.Lines.Count;
  for VAR i:= 1 to 1000000 DO
   begin
     GridLog.RamLog.AddInfo('Item '+ IntToStr(i+Count));
   end;

  GridLog.RamLog.RegisterLogObserver(GridLog);
  GridLog.Populate;
  GridLog.ChangeScrollBarVisibility(True);
end;


procedure TMainForm.btnClearClick(Sender: TObject);
begin
  GridLog.RamLog.Clear;
end;


procedure TMainForm.chkShowDateClick(Sender: TObject);
begin
 GridLog.ShowDate:= chkShowDate.Checked;
end;


procedure TMainForm.chkShowTimeClick(Sender: TObject);
begin
 GridLog.ShowTime:= chkShowTime.Checked;
end;



procedure TMainForm.btnSaveClick(Sender: TObject);
begin
 GridLog.RamLog.SaveToFile(AppData.ExeFolder+ 'LogFile.log');
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
 GridLog.RamLog.Clear;
 GridLog.RamLog.LoadFromFile(AppData.ExeFolder+ 'LogFile.log');
end;


end.
