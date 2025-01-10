UNIT FormRichLog;

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
  Winapi.Windows, Winapi.Messages, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  llRichLogTrack, llRichLog, cbAppDataForm;

TYPE
  TfrmRichLog = class(TLightForm)
    Log        : TRichLog;
    Container  : TPanel;    { We use a container for all controls on this form so we can reparent them easily to another form }
    pnlBottom  : TPanel;
    btnClear   : TButton;
    chkAutoOpen: TCheckBox;
    trkLogVerb : TRichLogTrckbr;
    procedure btnClearClick(Sender: TObject);
    procedure LogError     (Sender: TObject);
    procedure FormCreate   (Sender: TObject);
    procedure FormDestroy  (Sender: TObject);  // Would be nice to make this protected but we can't. All event handlers must be accesible/visible
  public
    procedure LateInitialize; override; // Called after the main form was fully initilized
  end;



IMPLEMENTATION {$R *.dfm}


USES
   cvINIFile, cbAppData;




{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}
procedure TfrmRichLog.FormCreate(Sender: TObject);
begin
 Log.Onwarn := LogError;               // Auto show form if we send an error msg to the log
 Log.OnError:= LogError;
 PostMessage(Self.Handle, WM_APP + 4712, 0, 0);
end;


procedure TfrmRichLog.LateInitialize;
begin
 LoadForm(Self);
end;



// This is called automatically by "Finalization"
procedure TfrmRichLog.FormDestroy(Sender: TObject);
begin
 Assert(AppData <> NIL, 'AppData is gone already!');
 Container.Parent:= Self;
 if NOT cbAppData.AppData.Initializing
 then SaveForm(Self); // We don't save anything if the start up was improper!
end;


procedure TfrmRichLog.btnClearClick(Sender: TObject);
begin
 Log.Clear;
end;


procedure TfrmRichLog.LogError(Sender: TObject);
begin
 if chkAutoOpen.Checked
 then Show;
end;



end.



