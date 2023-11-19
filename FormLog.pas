UNIT FormLog;

{=============================================================================================================
   CubicDesign
   2022-04-02
   See Copyright.txt

   Visual log (window).
   More details in ccLogUtils.pas

   Usage:
     It is CRITICAL to create the AppDataEx object as soon as the application starts. Prefferably in the DPR file before creating the main form!
       DPR:
          AppData:= TAppData.Create('MyCollApp');
       OnLateInitialize:
          AppData.Initilizing:= False;

     AppDataEx is automatically destroyed by the Finalization section of this unit.

   Tester:
     c:\Myprojects\Packages\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE
{.$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  ccRichLogTrack, ccRichLog;

TYPE
  TfrmLog = class(TForm)
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
  end;



IMPLEMENTATION {$R *.dfm}

USES
   ccIniFileVCL, ccAppData;




{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}
procedure TfrmLog.FormCreate(Sender: TObject);
begin
 Log.Onwarn := LogError;               // Auto show form if we send an error msg to the log
 Log.OnError:= LogError;
end;


// This is called automatically by "Finalization"
procedure TfrmLog.FormDestroy(Sender: TObject);
begin
 Container.Parent:= Self;
 if NOT ccAppdata.AppData.Initializing // We don't save anything if the start up was improper!
 then SaveForm(Self);
end;


procedure TfrmLog.btnClearClick(Sender: TObject);
begin
 Log.Clear;
end;


procedure TfrmLog.LogError(Sender: TObject);
begin
 if chkAutoOpen.Checked
 then Show;
end;



end.
