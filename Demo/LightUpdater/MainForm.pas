UNIT MainForm;

INTERFACE

USES
   Winapi.Windows, WinApi.Messages, System.SysUtils, System.Classes, VCL.Forms, Vcl.Controls,
   ccCore, csSystem, cbDialogs, cbAppData, ciUpdater;

TYPE
 TfrmDemoStarter = class(TForm)
    procedure FormDestroy           (Sender: TObject);
  private
    procedure LateInitialize(VAR Msg: TMessage); message MSG_LateFormInit; // Called after the main form was fully created
  public
 end;

VAR
   frmDemoStarter: TfrmDemoStarter;



IMPLEMENTATION  {$R *.dfm}
USES
   IdURI, FormUpdaterNotifier;

CONST
   // For testing purposes
   wwwBioniXWall     = 'http://www.BionixWallpaper.com';
   wwwUpdaterBinTest = '/downloads/Bionix%20Desktop%20Wallpaper%20Changer/OnlineNews.bin';



procedure TfrmDemoStarter.LateInitialize;
begin
 Updater:= TUpdater.Create(wwwBioniXWall+ wwwUpdaterBinTest);
 Updater.URLDownload   := wwwBioniXWall+ '/downloads/index.html#soft'; // wwwDwnldPage;
 Updater.URLRelHistory := wwwBioniXWall+ '/downloads/Bionix%20Desktop%20Wallpaper%20Changer/release-history.html#soft'; // wwwReleaseHistory;
 Updater.CheckForNews;

 { Create updater form }
 TFrmUpdater.ShowUpdater(TRUE);
 Close;
end;


procedure TfrmDemoStarter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(Updater);
end;




end.

