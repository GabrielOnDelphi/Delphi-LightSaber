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
  Winapi.Messages, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  cbLogRam, cvLog, cbAppData, cvLogFilter, Vcl.Grids;

TYPE
  TfrmRamLog = class(TForm)
    Container  : TPanel;    { We use a container for all controls on this form so we can reparent them easily to another form }
    pnlBottom  : TPanel;
    btnClear   : TButton;
    chkLogOnError: TCheckBox;
    Log: TLogGrid;
    trkLogVerb: TLogVerbFilter;
    chkShowTime: TCheckBox;
    procedure btnClearClick(Sender: TObject);
//    procedure LogError     (Sender: TObject);
    procedure FormCreate   (Sender: TObject);
    procedure FormDestroy  (Sender: TObject);
    procedure chkLogOnErrorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkShowTimeClick(Sender: TObject);
  private
  public
    class procedure CreateFormAppData; static; // Would be nice to make this protected but we can't. All event handlers must be accesible/visible
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit; // Called after the main form was fully initilized
  end;



IMPLEMENTATION {$R *.dfm}


USES
   cvINIFile, ccINIFile;



{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}
VAR FormLog: TfrmRamLog= NIL;  // Accessible via AppData only


//ToDo: find a way to create it automatically from AppData, when I have to show an error.
class procedure TfrmRamLog.CreateFormAppData;
begin
  Assert(FormLog = NIL, 'Form log already created!!!');

  AppData.CreateForm(TfrmRamLog,  FormLog, FALSE, flPositionOnly);
  FormLog.Log.AssignExternalRamLog(AppData.RamLog);
  cvINIFile.LoadForm(FormLog, flFull);

  Assert(Application.MainForm <> FormLog, 'The Log should not be the MainForm!'); { Just in case: Make sure this is not the first form created }
end;


procedure TfrmRamLog.FormCreate(Sender: TObject);
begin
  chkLogOnError.Checked:= AppData.RamLog.ShowOnError;
end;




procedure TfrmRamLog.LateInitialize;
begin
end;


procedure TfrmRamLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caHide;  // This window is global (tied to AppData). We cannot close it.
end;


// Triggered by application shutdown
procedure TfrmRamLog.FormDestroy(Sender: TObject);
begin
  Log.RamLog.UnregisterLogObserver;  //ToDo: do I need this?

  Assert(AppData <> NIL, 'AppData is gone already!');
  Container.Parent:= Self;
  if NOT cbAppData.AppData.Initializing
  then cvINIFile.SaveForm(Self); // We don't save anything if the start up was improper!

  //ToDo: Save Log verbosity
end;


procedure TfrmRamLog.btnClearClick(Sender: TObject);
begin
  Log.Clear;
end;










 {
// Log errors AND also warnings
procedure TfrmRamLog.LogError(Sender: TObject);
begin
  if AppData.PopUpLogWindowOnError
  then Show;
end; }


procedure TfrmRamLog.chkLogOnErrorClick(Sender: TObject);
begin
  AppData.RamLog.ShowOnError:= chkLogOnError.Checked;
end;


procedure TfrmRamLog.chkShowTimeClick(Sender: TObject);
begin
  Log.ShowTime:= TRUE;
end;



end.



