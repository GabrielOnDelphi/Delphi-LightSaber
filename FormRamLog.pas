UNIT FormRamLog;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
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
  Winapi.Messages, System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  cbAppData, cbLogRam, cvLog, cvLogFilter;

TYPE
  TfrmRamLog = class(TForm)
    Log           : TLogGrid;
    Container     : TPanel;    { We use a container for all controls on this form so we can reparent them easily to another form }
    btnClear      : TButton;
    chkLogOnError : TCheckBox;
    chkShowTime   : TCheckBox;
    pnlBottom     : TPanel;
    trkLogVerb    : TLogVerbFilter;
    procedure btnClearClick      (Sender: TObject);
    procedure FormDestroy        (Sender: TObject);
    procedure chkLogOnErrorClick (Sender: TObject);
    procedure FormClose          (Sender: TObject; var Action: TCloseAction);
    procedure chkShowTimeClick   (Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
    class procedure CreateGlobalLog; static; // Would be nice to make this protected but we can't. All event handlers must be accesible/visible
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit; // Called after the main form was fully initilized
  end;



IMPLEMENTATION {$R *.dfm}


USES
   cbLogUtils, cvINIFile, ccINIFile;



{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}
VAR FormLog: TfrmRamLog= NIL;  // Accessible via AppData only


//ToDo: find a way to create it automatically from AppData (interfaces?). We cannot do it from AppData itself because this form depends on my LightVisControls.dpk which is not available at this compilation point.
class procedure TfrmRamLog.CreateGlobalLog;
begin
  Assert(FormLog = NIL, 'Form log already created!!!');

  AppData.CreateForm(TfrmRamLog, FormLog, FALSE, flPosOnly);
  FormLog.Log.AssignExternalRamLog(AppData.RamLog);   // We will read data from AppData's log

  Assert(Application.MainForm <> FormLog, 'The Log should not be the MainForm!'); { Just in case: Make sure this is not the first form created }
end;


procedure TfrmRamLog.LateInitialize;
begin
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

  // Save form position
  if NOT cbAppData.AppData.Initializing
  then cvINIFile.SaveForm(Self, flPosOnly); // We don't save anything if the start up was improper!

  // Save Log verbosity
  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    IniFile.Write('ShowOnError', AppData.RamLog.ShowOnError);
    IniFile.Write('ShowTime', Log.ShowTime);
    IniFile.Write('Verbosity', Ord(Log.Verbosity));
  finally
    FreeAndNil(IniFile);
  end;
end;


procedure TfrmRamLog.LoadSettings;
begin
  cvINIFile.LoadForm(FormLog, flPosOnly);

  VAR IniFile := TIniFileEx.Create('Log Settings', AppData.IniFile);
  try
    AppData.RamLog.ShowOnError:= IniFile.Read('ShowOnError', True);
    Log.ShowTime              := IniFile.Read('ShowTime', TRUE);
    Log.Verbosity             := TLogVerbLvl(IniFile.Read('Verbosity', Ord(lvHints)));
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


procedure TfrmRamLog.chkShowTimeClick(Sender: TObject);
begin
  Log.ShowTime:= chkShowTime.Checked;
end;



end.



