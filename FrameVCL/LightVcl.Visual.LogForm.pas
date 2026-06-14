UNIT LightVcl.Visual.LogForm;

{=============================================================================================================
   2026.05.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Visual log window.
   Reads the content of a TRamLog.

   It is automatically created by TAppDataVCL.pas
   Manual creation is possible BUT AppData must exist (LoadSettings/FormPostInitialize read AppData.RamLog)
   and you must call FormPostInitialize yourself - it is what creates the Log viewer.
   User's prefferences are managed via LoadSettings/SaveSettings.

   TLogViewer tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

// Fixed 2026.03: trackbar persistence was broken because WriteComponentsOf/ReadComponentsOf
// in IniFile only recursed into TFrame descendants, missing TLogVerbFilter's sub-components.

INTERFACE

USES
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  LightCore.AppData, LightVcl.Visual.AppDataForm, LightCore.LogRam, LightVcl.Common.LogViewer, LightVcl.Common.LogFilter;

TYPE
  TfrmRamLog = class(TLightForm)
    btnClear       : TButton;
    chkLogOnError  : TCheckBox;
    chkShowDate    : TCheckBox;
    chkShowTime    : TCheckBox;
    Container      : TPanel;
    mnuCopy        : TMenuItem;
    mnuCopyAll     : TMenuItem;
    mnuCopyFiltered: TMenuItem;
    mnuCopySelected: TMenuItem;
    pnlBottom      : TPanel;
    PopupMenu      : TPopupMenu;
    chkScrollDown: TCheckBox;
    procedure btnClearClick      (Sender: TObject);
    procedure chkLogOnErrorClick (Sender: TObject);
    procedure chkShowDateClick   (Sender: TObject);
    procedure chkShowTimeClick   (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy        (Sender: TObject);
    procedure mnuCopyAllClick      (Sender: TObject);
    procedure mnuCopyClick         (Sender: TObject);
    procedure mnuCopyFilteredClick (Sender: TObject);
    procedure mnuCopySelectedClick (Sender: TObject);
    procedure chkScrollDownClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
    Log: TLogViewer;
    LogFilter: TLogVerbFilter;
    procedure FormPostInitialize; override; // Called after the main form was fully initialized
  end;


IMPLEMENTATION {$R *.DFM}


USES
   LightCore.LogTypes, LightCore.INIFile, LightVcl.Visual.AppData;


{-------------------------------------------------------------------------------------------------------------
  FORM CREATION

  Wiring contract:
    TLogViewer.Create assigns Log a private, empty TRamLog (FOwnRamLog=TRUE).
    To display the application-wide log, the CALLER must invoke Log.AssignExternalRamLog(AppData.RamLog) after CreateForm returns.
    Today this is done by TAppData.getGlobalLog (LightVcl.Visual.AppData.pas).
    If anything else creates TfrmRamLog directly without that call, the form will display its private empty log instead of the application's RamLog.
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.FormPostInitialize;
begin
  inherited FormPostInitialize;

  // Must be created dynamically because the component might not be installed at this point
  Log:= TLogViewer.Create(Self);
  Log.Parent:= Container;
  Log.Align:= alClient;
  Log.PopupMenu:= PopupMenu;

  LogFilter:= TLogVerbFilter.Create(Self);
  LogFilter.Parent:= pnlBottom;
  LogFilter.Align:= alRight;
  LogFilter.Log:= Log;

  LoadSettings;
  chkLogOnError.Checked:= AppData.RamLog.ShowOnError;
  chkScrollDown.Checked:= Log.AutoScroll;
end;


procedure TfrmRamLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caHide;  // This window is global (tied to AppData). We cannot close it.
end;


// Triggered by application shutdown
procedure TfrmRamLog.FormDestroy(Sender: TObject);
begin
  Assert(Owner = NIL); // This form should have no owner. If it has an owner (like Application), it will destroy the form. We want AppData to destroy the form!

  // Log is created in FormPostInitialize. If the form is destroyed before that ran (startup failed
  // mid-initialization, or the form was created manually without calling FormPostInitialize),
  // there is nothing to unhook and nothing to save. Dereferencing Log here would AV and mask the original error.
  if Log = NIL then EXIT;

  // Order matters:
  //   1. FormDestroying:=TRUE so any closures already in TThread.Queue bail out cleanly
  //   2. UnregisterLogObserver so no NEW notifications get queued
  // Together these close the late-callback window for background-thread log appends.
  Log.FormDestroying:= TRUE;
  Log.RamLog.UnregisterLogObserver;
  SaveSettings;
end;




{-------------------------------------------------------------------------------------------------------------
   SETTINGS
   Verbosity persistence: saves Log.Verbosity (the model) directly.
   On restore, setting Log.Verbosity syncs the trackbar automatically via setVerbFilter.
   Compare with TMainForm (FMX demo) which takes the opposite approach: saving the trackbar UI control via auto-state.
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.SaveSettings;
begin
  if AppData = NIL then EXIT;  // AppData already freed during shutdown (valid lifecycle state — finalization order is non-deterministic)

  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);

    IniFile.Write('ShowTime'   , Log.ShowTime);
    IniFile.Write('ShowDate'   , Log.ShowDate);
    IniFile.Write('AutoScroll' , Log.AutoScroll);
    IniFile.Write('Verbosity'  , Ord(Log.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.LoadSettings;
begin
  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError:= IniFile.Read('ShowOnError', TRUE);

    Log.ShowTime              := IniFile.Read('ShowTime', TRUE);
    Log.ShowDate              := IniFile.Read('ShowDate', TRUE);
    Log.AutoScroll            := IniFile.Read('AutoScroll', TRUE);
    Log.Verbosity             := TLogVerbLvl(IniFile.Read('Verbosity', Ord(lvHints)));

    chkShowDate.Checked:= Log.ShowDate;
    chkShowTime.Checked:= Log.ShowTime;
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.btnClearClick(Sender: TObject);
begin
  Log.Clear;
end;





{-------------------------------------------------------------------------------------------------------------
   GUI
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.chkLogOnErrorClick(Sender: TObject);
begin
  AppData.RamLog.ShowOnError:= chkLogOnError.Checked;
end;


procedure TfrmRamLog.chkScrollDownClick(Sender: TObject);
begin
  Log.AutoScroll:= chkScrollDown.Checked;
end;


procedure TfrmRamLog.chkShowDateClick(Sender: TObject);
begin
  Log.ShowDate:= chkShowDate.Checked;
end;


procedure TfrmRamLog.chkShowTimeClick(Sender: TObject);
begin
  Log.ShowTime:= chkShowTime.Checked;
end;


procedure TfrmRamLog.mnuCopyAllClick(Sender: TObject);
begin
  Log.CopyAll;
end;


procedure TfrmRamLog.mnuCopyClick(Sender: TObject);
begin
  Log.CopyCurLine;
end;


procedure TfrmRamLog.mnuCopyFilteredClick(Sender: TObject);
begin
  Log.CopyVisible;
end;


procedure TfrmRamLog.mnuCopySelectedClick(Sender: TObject);
begin
  ///Log.CopySelected;  //todo: do not delete. put it back later
end;


end.
