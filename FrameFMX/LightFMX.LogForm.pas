UNIT LightFmx.LogForm;

{=============================================================================================================
   2026.01.31
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
    Container      : TPanel;
    LogViewer      : TLogViewer;
    mnuCopy        : TMenuItem;
    mnuCopyAll     : TMenuItem;
    mnuCopyFiltered: TMenuItem;
    pnlBottom      : TPanel;
    chkShowDate    : TCheckBox;
    LogFilter      : TLogVerbFilter;
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
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;


IMPLEMENTATION {$R *.FMX}


USES
   LightCore.LogTypes, LightCore.INIFile, LightFMX.Common.AppData;


{-------------------------------------------------------------------------------------------------------------
  FORM CREATION
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.FormCreate(Sender: TObject);
begin
  LogViewer.AssignExternalRamLog(AppData.RamLog);   // FormLog will display data from AppData's RAM log

  LoadSettings;
  chkLogOnError.IsChecked:= AppData.RamLog.ShowOnError;
  chkShowTime.IsChecked:= LogViewer.ShowTime;

  Assert(Application.MainForm <> TCommonCustomForm(Self), 'Sanity check! The Log should not be the MainForm!'); { Make sure this is not the first form created }
end;


procedure TfrmRamLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caHide;  // This window is global (tied to AppData). We cannot close it.
end;


// Triggered by application shutdown
procedure TfrmRamLog.FormDestroy(Sender: TObject);
begin
  Assert(Owner = NIL); // This form should have no owner. If it has an owner (like Application), it will destroy the form. We want AppData to destroy the form!

  LogViewer.RamLog.UnregisterLogObserver;  // I need this so I don't send messages to the LogViewer.
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
    IniFile.Write('ShowTime', LogViewer.ShowTime);
    IniFile.Write('ShowDate', LogViewer.ShowDate);
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


end.
