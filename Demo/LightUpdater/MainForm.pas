UNIT MainForm;

INTERFACE

USES
   Winapi.Windows, WinApi.Messages, System.SysUtils, System.Classes, VCL.Forms, Vcl.Controls,
   ccCore, ccAppData, ciUpdater;

TYPE
 TfrmDemoStarter = class(TForm)
    procedure FormCreate            (Sender: TObject);
    procedure FormDestroy           (Sender: TObject);
  private
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateAppInit; // Called after the main form was fully created
  public
 end;

VAR
   frmDemoStarter: TfrmDemoStarter;



IMPLEMENTATION  {$R *.dfm}
USES
   FormUpdaterNotifier, BxConstants;

CONST
   wwwUpdaterBinTest = wwwUpdaterBin;



procedure TfrmDemoStarter.FormCreate(Sender: TObject);
begin
  //
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
end;











end.

