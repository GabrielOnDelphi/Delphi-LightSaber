UNIT FormRamLog;

{=============================================================================================================
   2024.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Visual log window.
   Reads the content of a TRamLog.

   It is automatically created by TAppDataVCL.pas
   But can be also created manually if you don't use AppData.
   User's prefferences are managed via LoadSettings/SaveSettings.

   TLogGrid tester:
     c:\Projects\LightSaber\Demo\Demo LightLog\Demo_Log.dpr
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Grids,
  ccAppData, cbAppDataForm, ccLogRam, cbLog, cbLogFilter;

TYPE
  TfrmRamLog = class(TLightForm)
    btnClear       : TButton;
    chkLogOnError  : TCheckBox;
    chkShowDate    : TCheckBox;
    chkShowTime    : TCheckBox;
    Container      : TPanel;    { We use a container for all controls on this form so we can reparent them easily to another form }
    Log            : TLogGrid;
    mnuCopy        : TMenuItem;
    mnuCopyAll     : TMenuItem;
    mnuCopyFiltered: TMenuItem;
    pnlBottom      : TPanel;
    PopupMenu      : TPopupMenu;
    trkLogVerb     : TLogVerbFilter;
    procedure btnClearClick      (Sender: TObject);
    procedure chkLogOnErrorClick (Sender: TObject);
    procedure chkShowDateClick   (Sender: TObject);
    procedure chkShowTimeClick   (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy        (Sender: TObject);
    procedure mnuCopyAllClick    (Sender: TObject);
    procedure mnuCopyClick       (Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
    procedure FormPostInitialize; override; // Called after the main form was fully initilized
  end;


IMPLEMENTATION {$R *.dfm}


USES
   ccLogTypes, ccINIFile, cbAppDataVCL;




{-------------------------------------------------------------------------------------------------------------
  FORM CREATION
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.FormPostInitialize;
begin
  inherited FormPostInitialize;

  LoadSettings;
  chkLogOnError.Checked:= AppData.RamLog.ShowOnError;
  chkShowTime.Checked  := Log.ShowTime;
end;


procedure TfrmRamLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caHide;  // This window is global (tied to AppData). We cannot close it.
end;


// Triggered by application shutdown
procedure TfrmRamLog.FormDestroy(Sender: TObject);
begin
  Assert(Owner = NIL); // This form should have no owner. If it has an owner (like Application), it will destroy the form. We want AppData to destroy the form!

  Log.RamLog.UnregisterLogObserver;  //ToDo: do I need this?
  SaveSettings;
  Container.Parent:= Self;
end;






{-------------------------------------------------------------------------------------------------------------
   SETTINGS
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRamLog.SaveSettings;
begin
  Assert(AppData <> NIL, 'AppData is gone already!');

  // Save Log verbosity
  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);

    IniFile.Write('ShowTime'   , Log.ShowTime);
    IniFile.Write('ShowDate'   , Log.ShowDate);
    IniFile.Write('Verbosity'  , Ord(Log.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.LoadSettings;
begin
  //del cvINIFile.LoadForm(Self);

  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError:= IniFile.Read('ShowOnError', TRUE);

    Log.ShowTime              := IniFile.Read('ShowTime', TRUE);
    Log.ShowDate              := IniFile.Read('ShowDate', TRUE);
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


end.
