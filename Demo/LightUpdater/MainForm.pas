UNIT MainForm;

INTERFACE

USES
   Winapi.Windows, WinApi.Messages, System.SysUtils, System.Classes, VCL.Forms, Vcl.Controls,
   ccCore, ciUpdater;

TYPE
 TfrmDemoStarter = class(TForm)
    procedure FormCreate            (Sender: TObject);
    procedure FormDestroy           (Sender: TObject);
  private
    procedure LateInitialize(VAR message: TMessage);  message MSG_LateInitialize;
  public
 end;

VAR
   frmDemoStarter: TfrmDemoStarter;



IMPLEMENTATION  {$R *.dfm}
USES
   ccAppData, FormUpdaterNotifier, BxConstants;

CONST
   wwwUpdaterBinTest = wwwUpdaterBin;



procedure TfrmDemoStarter.FormCreate(Sender: TObject);
begin
 AppData.Initializing:= FALSE;
 PostMessage(Self.Handle, MSG_LateInitialize, 0, 0);         { This will call LateInitialize }
end;


procedure TfrmDemoStarter.LateInitialize;
begin
 Updater:= TUpdater.Create(wwwUpdaterBinTest);
 Updater.URLDownload    := wwwDwnldPage;
 Updater.URLRelHistory  := wwwReleaseHistory;

 { Create updater form }
 TFrmUpdater.ShowUpdater(TRUE);
 Close;
end;


procedure TfrmDemoStarter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Updater);
 FreeAndNil(AppData-);
end;











end.

