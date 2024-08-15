UNIT MainForm;

INTERFACE

USES
   Winapi.Windows, WinApi.Messages, System.SysUtils, System.Classes, VCL.Forms, Vcl.Controls,
   ccCore, csSystem, cbDialogs, cbAppData, ciUpdater;

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
   FormUpdaterNotifier;

CONST
   // For testing purposes
   wwwBioniXWall     = 'http://www.BionixWallpaper.com/';
   wwwUpdaterBinTest = 'downloads/Bionix%20Desktop%20Wallpaper%20Changer/OnlineNews.bin';



procedure TfrmDemoStarter.FormCreate(Sender: TObject);
begin
  //
end;


procedure TfrmDemoStarter.LateInitialize;
begin
 Updater:= TUpdater.Create(wwwUpdaterBinTest);
 Updater.URLDownload    := wwwBioniXWall+ '/downloads/index.html#soft'; // wwwDwnldPage;
 Updater.URLRelHistory  := wwwBioniXWall+ '/downloads/Bionix%20Desktop%20Wallpaper%20Changer/release-history.html#soft'; // wwwReleaseHistory;

 { Create updater form }
 TFrmUpdater.ShowUpdater(TRUE);
 Close;
end;


procedure TfrmDemoStarter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Updater);
end;











end.

