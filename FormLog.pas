UNIT FormLog;

{=============================================================================================================
   CubicDesign
   2022-04-02
   See Copyright.txt

   Visual log (window).
   More details in clLogUtils.pas

   Usage:
     It is CRITICAL to create the AppDataEx object as soon as the application starts. Prefferably in the DPR file before creating the main form!
       DPR:
          AppData:= TAppDataEx.Create('MyCollApp');
       OnLateInitialize:
          AppData.Initilizing:= False;

     AppDataEx is automatically destroyed by the Finalization section of this unit.

   Tester:
     c:\Myprojects\Packages\CubicCommonControls\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  clRichLogTrack, clRichLog, ccAppData;

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
    procedure FormDestroy  (Sender: TObject);
    procedure FormCreate   (Sender: TObject);
  end;

TYPE
  TAppDataEx= class(TAppData)
  public
    constructor Create(CONST aAppName: string; CONST WindowClassName: string= ''); override;
    destructor Destroy; override;
  end;


{ Quick Utils }
procedure LogAddVerb (Mesaj: string);
procedure LogAddHint (Mesaj: string);
procedure LogAddInfo (Mesaj: string);
procedure LogAddImpo (Mesaj: string);
procedure LogAddWarn (Mesaj: string);
procedure LogAddError(Mesaj: string);
procedure LogAddMsg  (Mesaj: string);
procedure LogAddBold (Mesaj: string);

procedure LogAddEmptyRow;
procedure LogClear;
procedure LogSaveAsRtf(CONST FileName: string);

procedure ShowLog(Center: Boolean= FALSE);




IMPLEMENTATION {$R *.dfm}

USES
   ccCore, ccIniFileVCL;

VAR
   frmLog: TfrmLog = NIL; // Keep this private



{-------------------------------------------------------------------------------------------------------------
   MAIN FUNCTIONS
-------------------------------------------------------------------------------------------------------------}
procedure ShowLog(Center: Boolean= FALSE);
begin
 frmLog.Show;
 if Center
 then CenterForm(frmLog{, Application.MainForm});
end;








{-------------------------------------------------------------------------------------------------------------
   FORM
-------------------------------------------------------------------------------------------------------------}
procedure TfrmLog.FormCreate(Sender: TObject);
begin
 Log.Onwarn := LogError;               // Auto show form if we send an error msg to the log
 Log.OnError:= LogError;
end;


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






{--------------------------------------------------------------------------------------------------
   TAppDataEx

   Tester: c:\Myprojects\Packages\CubicCommonControls\Demo\CubicCore\GUI Autosave\DemoCore.dpr
--------------------------------------------------------------------------------------------------}
constructor TAppDataEx.Create(CONST aAppName: string; CONST WindowClassName: string= '');
begin
  inherited Create(aAppName, WindowClassName);

 { Call this as soon as possible so it can catch all Log messages generated during app start up. A good place might be in your DPR file before Application.CreateForm(TMainForm, frmMain) }
 Assert(frmLog = NIL, 'frmLog already created!');
 frmLog:= TfrmLog.Create(NIL);
 LoadForm(frmLog);
 Assert(Application.MainForm <> frmLog, 'The Log should not be the MainForm'); { Just in case: Make sure this is not the first form created }
end;





destructor TAppDataEx.Destroy;
begin
  FreeAndNil(frmLog); { Call this as late as possible, on application shutdown }
  inherited;
end;









{--------------------------------------------------------------------------------------------------
   UTILS
   SEND MESSAGES DIRECTLY TO LOG WND
--------------------------------------------------------------------------------------------------}
procedure LogAddVerb(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddVerb(Mesaj);
end;


procedure LogAddHint(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddHint(Mesaj);
end;


procedure LogAddInfo(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddInfo(Mesaj);
end;


procedure LogAddImpo(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddImpo(Mesaj);
end;


procedure LogAddWarn(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddWarn(Mesaj);
end;


procedure LogAddError(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddError(Mesaj);

 if frmLog.chkAutoOpen.Checked
 then ShowLog;
end;


procedure LogAddMsg(Mesaj: string);  { Always show this message, no matter the verbosity of the log. Equivalent to Log.AddError but the msg won't be shown in red. }
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddMsg(Mesaj);
end;


procedure LogAddBold(Mesaj: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddBold(Mesaj);
end;






procedure LogClear;
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddEmptyRow;
end;


procedure LogAddEmptyRow;
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.AddEmptyRow;
end;

procedure LogSaveAsRtf(CONST FileName: string);
begin
 Assert(frmLog <> NIL, 'The log window is not ready yet!');
 frmLog.Log.SaveAsRtf(FileName);
end;



INITIALIZATION

FINALIZATION
  FreeAndNil(AppData); // This will release also the frmLog.

end.
