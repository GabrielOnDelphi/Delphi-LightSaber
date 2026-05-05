UNIT FormMain;

{=============================================================================================================
   2026.05.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Demo for the LightSaber log subsystem.

   Showcases:
     - TRichLog            (RTF-based log, on left panel)
     - TRichLogTrckbr      (verbosity filter for the RichLog)
     - TLogViewer          (TStringGrid-based log, on right panel; can hold 1M+ entries)
     - TLogVerbFilter      (verbosity filter for the LogViewer)
     - AssignExternalRamLog: wires the LogViewer to AppData.RamLog so any
       application-wide log message (Log.AddInfo / AppData.RamLog.AddInfo / etc.)
       is rendered live in the grid. The window also pops up automatically on
       warnings/errors when AppData.RamLog.ShowOnError = TRUE.

   The Save/Load buttons demonstrate persisting the log to disk via the binary
   format (LogFile.log). The "Test 1000000" button stress-tests the grid by
   appending one million entries with the observer temporarily detached, then
   reattaching and triggering a single Populate.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Grids,
  LightVcl.Visual.RichLogTrack, LightVcl.Visual.RichLogUtils, LightVcl.Visual.RichLog,
  LightVcl.Common.LogFilter, LightVcl.Visual.AppDataForm, LightCore.LogRam, LightVcl.Common.LogViewer;

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
  LightCore.AppData, LightVcl.Visual.AppData, LightCore.LogTypes, LightCore.INIFile, LightVcl.Common.Dialogs;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  //RichLog.Clear;
end;


procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;

  // Wire the grid-based log viewer to the application-wide RamLog.
  // After this call, any AppData.RamLog.AddXxx (or Log.AddXxx) message lands in GridLog.
  // Note: LoadForm has already run by this point; the verbosity/ShowDate/ShowTime
  // settings persisted in the INI are applied by AssignExternalRamLog -> Populate.
  GridLog.AssignExternalRamLog(AppData.RamLog);

  if AppData.RunningHome
  then LogVis.TrackBar.Min:= 0      // Dev/home build: allow viewing the "Debug" level
  else LogVis.TrackBar.Min:= 1;     // Production build: hide the "Debug" level
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
 RichLog.AddFromFile(Appdata.AppFolder+ 'Test file.txt', lvrImportant);
end;




{-------------------------------------------------------------------------------------------------------------
   GRID LOG
   GridLog has been wired to AppData.RamLog (see FormPostInitialize), so
   logging through AppData.RamLog (or the global Log alias) shows up in the grid.
-------------------------------------------------------------------------------------------------------------}
procedure TMainForm.btnTest2Click(Sender: TObject);
begin
 LogVis.TrackBar.Position:= 0;

 AppData.RamLog.AddImpo('This is TLogViewer based on TStringGrid (wired to AppData.RamLog)');
 AppData.RamLog.AddEmptyRow;

 AppData.RamLog.AddDebug('AddDebug');
 AppData.RamLog.AddVerb ('AddVerb');
 AppData.RamLog.AddHint ('AddHint');
 AppData.RamLog.AddInfo ('AddInfo');
 AppData.RamLog.AddImpo ('AddImpo');
 AppData.RamLog.AddWarn ('AddWarn');
 AppData.RamLog.AddError('AddError');

 AppData.RamLog.AddEmptyRow;

 AppData.RamLog.AddBold  ('AddBold');
 AppData.RamLog.AddMsg   ('AddMsg');
 AppData.RamLog.AddMsgInt('AddMsgInt', 42);
end;


procedure TMainForm.btnLoopTestClick(Sender: TObject);
begin
  // Detach the observer so the grid is not refreshed for every single AddInfo call.
  // We re-attach after the loop and trigger one Populate.
  AppData.RamLog.UnregisterLogObserver;

  VAR Count:= AppData.RamLog.Lines.Count;
  for VAR i:= 1 to 1000000 DO
     AppData.RamLog.AddInfo('Item '+ IntToStr(i+Count));

  AppData.RamLog.RegisterLogObserver(GridLog);
  GridLog.Populate;
  GridLog.ChangeScrollBarVisibility(TRUE);
end;


procedure TMainForm.btnClearClick(Sender: TObject);
begin
  AppData.RamLog.Clear;
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
 AppData.RamLog.SaveToFile(AppData.AppFolder+ 'LogFile.log');
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
 AppData.RamLog.Clear;
 AppData.RamLog.LoadFromFile(AppData.AppFolder+ 'LogFile.log');
end;


end.
