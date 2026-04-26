UNIT LightFmx.Common.LogForm;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Visual log window for displaying TRamLog content.

   Creation:
     - Automatically created by TAppDataFMX when using AppData
     - Can be created manually if not using AppData

   User preferences are managed via LoadSettings/SaveSettings and persisted to INI file.

   Tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Menus,
  FMX.ScrollBox, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Presentation.Factory,
  LightFmx.Common.AppData.Form, LightFMX.Common.LogFilter, LightFMX.Common.LogViewer,
  LightCore.AppData, LightCore.LogRam;

TYPE
  TfrmRamLog = class(TLightForm)
    btnClear       : TButton;
    chkLogOnError  : TCheckBox;
    chkScrollDown  : TCheckBox;
    chkShowTime    : TCheckBox;
    Container      : TLayout;
    LogViewer      : TLogViewer;
    mnuCopy        : TMenuItem;
    mnuCopyAll     : TMenuItem;
    mnuCopyFiltered: TMenuItem;
    pnlBottom      : TPanel;
    chkShowDate    : TCheckBox;
    LogFilter      : TLogVerbFilter;
    btnReport: TButton;
    procedure btnClearClick       (Sender: TObject);
    procedure chkScrollDownChange (Sender: TObject);
    procedure FormClose           (Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy         (Sender: TObject);
    procedure mnuCopyAllClick     (Sender: TObject);
    procedure mnuCopyClick        (Sender: TObject);
    procedure mnuCopyFilteredClick(Sender: TObject);
    procedure chkLogOnErrorChange (Sender: TObject);
    procedure chkShowTimeChange   (Sender: TObject);
    procedure chkShowDateChange   (Sender: TObject);
    procedure FormCreate          (Sender: TObject);
    procedure btnReportClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;


IMPLEMENTATION {$R *.FMX}


USES
   LightCore.LogTypes, LightCore.INIFile, LightFMX.Common.AppData, LightFmx.Common.Styles,
   FormSystemReport, LightFmx.Common.Screen;


{-------------------------------------------------------------------------------------------------------------
  FORM CREATION
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.FormCreate(Sender: TObject);
begin
  LogViewer.AssignExternalRamLog(AppData.RamLog);   // FormLog will display data from AppData's RAM log

  LoadSettings;
  chkLogOnError.IsChecked:= AppData.RamLog.ShowOnError;
  // chkShowTime is already synced inside LoadSettings — no second assignment needed.

  // Phone Visibility
  btnClear.Visible     := NOT IsPhoneScreen;
  chkScrollDown.Visible:= NOT IsPhoneScreen;
  chkShowTime.Visible  := NOT IsPhoneScreen;
  chkShowDate.Visible  := NOT IsPhoneScreen;

  // Phone: collapse the Time/Date column — narrow screens don't have room for both columns.
  // Sync the (hidden) checkboxes too so their IsChecked state matches LogViewer.ShowDate/ShowTime.
  if IsPhoneScreen then
    begin
      LogViewer.ShowDate:= FALSE;
      LogViewer.ShowTime:= FALSE;
      chkShowDate.IsChecked:= FALSE;
      chkShowTime.IsChecked:= FALSE;
    end;

  Assert(Application.MainForm <> TCommonCustomForm(Self), 'Sanity check! The Log should not be the MainForm!'); { Make sure this is not the first form created }
end;


procedure TfrmRamLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caHide;  // This window is global (tied to AppData). We cannot close it.
end;


// Triggered by application shutdown
procedure TfrmRamLog.FormDestroy(Sender: TObject);
begin
  // Mark the viewer as destroying BEFORE we unregister, so any TThread.Queue
  // closure already in flight (posted by a background-thread log append) sees
  // the flag and bails out instead of touching this viewer once Application
  // has freed it. The form is owned by Application (see TAppData.getLogForm),
  // not by us — Application will run our destructor and then free the
  // component memory. The flag closes the race window.
  LogViewer.Destroying:= TRUE;
  LogViewer.RamLog.UnregisterLogObserver;  // Prevent NEW notifications.
  SaveSettings;
end;






{-------------------------------------------------------------------------------------------------------------
   SETTINGS
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.SaveSettings;
begin
  Assert(AppData <> NIL, 'AppData is gone already!');

  VAR IniFile:= TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);
    // Phone forces ShowDate/ShowTime to FALSE — don't persist that, or it would clobber the desktop preference.
    if NOT IsPhoneScreen then
      begin
        IniFile.Write('ShowTime', LogViewer.ShowTime);
        IniFile.Write('ShowDate', LogViewer.ShowDate);
      end;
    IniFile.Write('Verbosity', Ord(LogViewer.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.LoadSettings;
begin
  VAR IniFile:= TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError:= IniFile.Read('ShowOnError', TRUE);

    LogViewer.ShowTime:= IniFile.Read('ShowTime', TRUE);
    LogViewer.ShowDate:= IniFile.Read('ShowDate', TRUE);
    LogViewer.Verbosity:= TLogVerbLvl(IniFile.Read('Verbosity', Ord(lvHints)));

    chkShowDate.IsChecked:= LogViewer.ShowDate;
    chkShowTime.IsChecked:= LogViewer.ShowTime;
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.btnClearClick(Sender: TObject);
begin
  LogViewer.Clear;
end;





{-------------------------------------------------------------------------------------------------------------
   GUI
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.chkLogOnErrorChange(Sender: TObject);
begin
  AppData.RamLog.ShowOnError:= chkLogOnError.IsChecked;
end;


procedure TfrmRamLog.chkScrollDownChange(Sender: TObject);
begin
  LogViewer.AutoScroll:= chkScrollDown.IsChecked;
end;


procedure TfrmRamLog.chkShowDateChange(Sender: TObject);
begin
  LogViewer.ShowDate:= chkShowDate.IsChecked;
end;


procedure TfrmRamLog.chkShowTimeChange(Sender: TObject);
begin
  LogViewer.ShowTime:= chkShowTime.IsChecked;
end;


procedure TfrmRamLog.mnuCopyAllClick(Sender: TObject);
begin
  LogViewer.CopyAll;
end;


procedure TfrmRamLog.mnuCopyClick(Sender: TObject);
begin
  LogViewer.CopyCurLine;
end;


procedure TfrmRamLog.mnuCopyFilteredClick(Sender: TObject);
begin
  LogViewer.CopyVisible;
end;


procedure TfrmRamLog.btnReportClick(Sender: TObject);
begin
  AppData.CreateFormModal(TfrmSystemReport);
end;

end.
