UNIT FormRamLog;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

   Visual log (window).
   More details in llRichLogUtils.pas

   Usage:
     It is CRITICAL to create the AppData object as soon as the application starts.
     Prefferably in the DPR file before creating the main form!
       DPR:
          AppData:= TAppData.Create('MyCollApp');
       OnLateInitialize:
          AppData.Initilizing:= False;

     AppDataEx is automatically destroyed by the Finalization section of this unit.

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Forms, cbAppDataForm,Vcl.StdCtrls, Vcl.ExtCtrls,
  ccAppData, cbAppDataVCL, ccLogRam, cvLog, cvLogFilter, Vcl.Menus, Vcl.Grids;

TYPE
  TfrmRamLog = class(TLightForm)
    Log           : TLogGrid;
    Container     : TPanel;    { We use a container for all controls on this form so we can reparent them easily to another form }
    btnClear      : TButton;
    chkLogOnError : TCheckBox;
    chkShowTime   : TCheckBox;
    pnlBottom     : TPanel;
    trkLogVerb    : TLogVerbFilter;
    chkShowDate   : TCheckBox;
    PopupMenu: TPopupMenu;
    mnuCopy: TMenuItem;
    mnuCopyAll: TMenuItem;
    mnuCopyFiltered: TMenuItem;
    procedure btnClearClick      (Sender: TObject);
    procedure FormDestroy        (Sender: TObject);
    procedure chkLogOnErrorClick (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure chkShowTimeClick   (Sender: TObject);
    procedure chkShowDateClick   (Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuCopyAllClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
    class procedure CreateGlobalLog; static; // Would be nice to make this protected but we can't. All event handlers must be accesible/visible
    procedure FormPostInitialize; override; // Called after the main form was fully initilized
  end;




IMPLEMENTATION {$R *.dfm}


USES
   ccLogTypes, ccINIFile;



{-------------------------------------------------------------------------------------------------------------
   FORM

   Create the Log form globally (to be used by the entire application)
-------------------------------------------------------------------------------------------------------------}
VAR
  FormLog: TfrmRamLog= NIL;  // Accessible via AppData only


{ToDo 5: Find a way to create it automatically from AppData (interfaces?). We cannot do it from AppData itself because this form depends on my LightVisControls.dpk which is not available at this compilation point.
  We could use something like:
  TAppData = class
    class procedure RegisterFormCreator(AFormCreator: TFunc<TForm>); static;  // This class procedure would be called when we initialize the high-level package that contains the FormLog.
  But this method seems to brittle. }
class procedure TfrmRamLog.CreateGlobalLog;
begin
  Assert(FormLog = NIL, 'Form log already created!');

  VAR CreateBeforeMainForm:= Application.MainForm = NIL;
  AppData.CreateForm(TfrmRamLog, FormLog, FALSE, asPosOnly, NIL, FALSE, CreateBeforeMainForm);
  FormLog.Log.AssignExternalRamLog(AppData.RamLog);   // We will read data from AppData's log

  Assert(Application.MainForm <> FormLog, 'The Log should not be the MainForm!'); { Just in case: Make sure this is not the first form created }
end;


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
  Log.RamLog.UnregisterLogObserver;  //ToDo: do I need this?
  SaveSettings;
  Container.Parent:= Self;
end;




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


