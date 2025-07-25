UNIT FormMain;

{=============================================================================================================
   2025.05
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE

USES
  windows, System.SysUtils, System.Classes, System.Actions, System.Net.URLClient,
  VCL.Menus, Vcl.AppEvnts, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.ActnList,
  LightVcl.Common.SystemTime, LightVcl.Common.Clipboard, LightVcl.Common.AppDataForm, LightVcl.Internet.Download.Thread;

TYPE
  TMainForm = class(TLightForm)
    mmoIP: TMemo;
    pgCtrl      : TPageControl;
    pnlRight    : TPanel;
    tabMain     : TTabSheet;
    btnIntConnection: TButton;
    tabSecondary: TTabSheet;
    btnGetExtIp: TButton;
    mmoDown2: TMemo;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    btnDownloadFileRTL: TButton;
    btnDownloadStreamRTL: TButton;
    Button1: TButton;
    btnDownload2: TButton;
    btnDownload1: TButton;
    btnDownloadThreaded: TButton;
    mmoDown3: TMemo;
    btnAsStringRTL: TButton;
    procedure btnIntConnectionClick (Sender: TObject);
    procedure btnGetExtIpClick(Sender: TObject);
    procedure btnDownload2Click(Sender: TObject);
    procedure btnDownload1Click(Sender: TObject);
    procedure OnDownloadDone(Sender: TObject);
    procedure btnDownloadThreadedClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnDownloadFileRTLClick(Sender: TObject);
    procedure btnDownloadStreamRTLClick(Sender: TObject);
    procedure btnAsStringRTLClick(Sender: TObject);
  protected
  private
    c: Cardinal;
    TestURL: string;
    myDownload: TWinInetObj;
  public
    procedure FormPostInitialize; override;
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

USES
   LightVcl.Common.Sound, LightCore, LightVcl.Common.System, LightVcl.Common.Shell, LightVcl.Common.ExecuteShell, LightCore.AppData, LightVcl.Common.AppData, LightVcl.Common.CenterControl,
   LightCore.TextFile, LightVcl.Internet, LightCore.Internet, LightCore.Download, LightCore.IO,LightVcl.Internet.Download.WinInet;



{--------------------------------------------------------------------------------------------------
   APP START
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormPostInitialize;
begin
  inherited FormPostInitialize;
  AppData.CompanyName:= 'SciVance Technologies';

  //TestURL:= AppData.CompanyHome;
  //TestURL:= 'https://www.google.com/';
  TestURL:= 'https://api.github.com/repos/yt-dlp/yt-dlp/releases/latest';
  //TestURL:= 'https://github.com/yt-dlp/yt-dlp/releases';
  //TestURL:= 'https://github.com/yt-dlp/yt-dlp/releases/download/'
end;





{--------------------------------------------------------------------------------------------------
   IP
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnIntConnectionClick(Sender: TObject);
begin
  LightVcl.Common.System.CursorBusy;
  TRY
    Caption:= 'Connecting...';
   LightVcl.Internet.TestProgramConnection(TRUE);
  FINALLY
    CursorNotBusy;
  END;
end;


procedure TMainForm.btnGetExtIpClick(Sender: TObject);
begin
  mmoIP.Text:= GenerateInternetRep;
end;





{--------------------------------------------------------------------------------------------------
    DOWNLOAD Delphi RTL
--------------------------------------------------------------------------------------------------}

procedure TMainForm.btnDownloadStreamRTLClick(Sender: TObject);
VAR
  Stream: TMemoryStream;
  RetCode: string;
  Headers: System.Net.URLClient.TNetHeaders;
begin
 mmoDown2.Text:= 'DownloadToStream [System.Net.HttpClient]...';
 mmoDown2.Update;

 c:= GetTickCount;

 // Headers
  SetLength(Headers, 1);
  Headers[0].Name := 'Referer';
  Headers[0].Value:= 'http://example.com';

 Stream:= LightCore.Download.DownloadToStream(TestURL, RetCode);
 TRY
   stream.SaveToFile(AppData.ExeFolder+ 'DownloadToStreamRTL.BIN');
   if RetCode= ''
   then mmoDown2.Lines.Add('OK!')
   else mmoDown2.Lines.Add(RetCode);
 FINALLY
   FreeAndNil(Stream);
 END;

 mmoDown2.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;


procedure TMainForm.btnDownloadFileRTLClick(Sender: TObject);
VAR RetCode: string;
begin
 mmoDown2.Text:= 'DownloadToFile [System.Net.HttpClient]...';
 mmoDown2.Update;

 c:= GetTickCount;
 LightCore.Download.DownloadToFile(TestURL, AppData.ExeFolder+ 'DownloadToFileRTL.BIN', RetCode);
 if RetCode= ''
 then mmoDown2.Lines.Add('OK!')
 else mmoDown2.Lines.Add(RetCode);

 mmoDown2.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;


procedure TMainForm.btnAsStringRTLClick(Sender: TObject);
VAR s: string;
begin
 mmoDown2.Text:= 'DownloadAsString [System.Net.HttpClient]...';
 mmoDown2.Update;

 c:= GetTickCount;
 s:= LightCore.Download.DownloadAsString(TestURL);
 if s> ''
 then mmoDown2.Lines.Add('OK!')
 else mmoDown2.Lines.Add('Failed!');
 mmoDown2.Lines.Add(CRLF);
 mmoDown2.Lines.Add(s);

 mmoDown2.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;






{--------------------------------------------------------------------------------------------------
   DOWNLOAD FILE
   WinInet
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnDownload1Click(Sender: TObject);
VAR
   BinData: TBytes;
   HttpRetCode: Integer;
begin
 mmoDown3.Text:= 'DownloadBytes [WinInet]...';
 mmoDown3.Update;

 c:= GetTickCount;
 HttpRetCode:=LightVcl.Internet.Download.WinInet.DownloadBytes(TestURL, '', BinData);

 if HttpRetCode = 0
 then
  begin
    mmoDown3.Lines.Add('OK!');
    mmoDown3.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
    LightCore.IO.BytesToFile(AppData.ExeFolder+ 'DownloadBytes.BIN', BinData, TRUE);
  end
 else
   mmoDown3.Text:= 'Failed! '+ GetWin32ErrorString(HttpRetCode);

 BipConfirmation;
end;



procedure TMainForm.btnDownload2Click(Sender: TObject);
begin
 mmoDown3.Text:= 'DownloadToFile [WinInet]...';
 mmoDown3.Update;

 c:= GetTickCount;
 if LightVcl.Internet.Download.WinInet.DownloadToFile(TestURL, '', AppData.ExeFolder+ 'DownloadToFile.BIN') = 0
 then mmoDown3.Lines.Add('OK!')
 else mmoDown3.Lines.Add('Failed!');

 mmoDown3.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;






{--------------------------------------------------------------------------------------------------
   DOWNLOAD FILE
   WinInet
   Async
--------------------------------------------------------------------------------------------------}

// Silent. Does not raises exceptions on download error
procedure TMainForm.btnDownloadThreadedClick(Sender: TObject);
begin
 mmoDown3.Text:= 'TWinInetObj - Threaded download...';
 mmoDown3.Update;

 FreeAndNil(myDownload);

 c:= GetTickCount;
 myDownload:=LightVcl.Internet.Download.Thread.TWinInetObj.Create(TRUE);  { You can't restart a thread once it is finished/terminated. https://stackoverflow.com/questions/6719949/restart-delphi-tthread-that-lives-the-entire-app-lifetime }
 myDownload.OnDownloadDone:= OnDownloadDone;  // WARNING! THIS IS CALLED EVEN IF THE DOWNLOAD FAILS!
 myDownload.UserAgent:= 'IEXPLORE';
 myDownload.Referer:= '';
 myDownload.URL := TestURL;
 myDownload.Start;
end;


procedure TMainForm.OnDownloadDone(Sender: TObject);
begin
 if myDownload.Data.Size > 0
 then myDownload.Data.SaveToFile(AppData.ExeFolder+ 'THREADED.BIN')  //LightCore.IO.BytesToFile(AppData.ExeFolder+ 'THREADED.BIN', myDownload.Data, TRUE)
 else mmoDown3.Lines.Add('Failure!');

 mmoDown3.Lines.Add('Returned in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
 mmoDown3.Text:= 'DownloadAsString [WinInet]...';
 mmoDown3.Update;

 c:= GetTickCount;
 VAR s:=LightVcl.Internet.Download.WinInet.DownloadAsString(TestURL);
 StringToFile(AppData.ExeFolder+ 'DownloadAsString.txt', s, woOverwrite, wpOff);
 mmoDown3.Lines.Add('Done in ' + Real2Str((GetTickCount-c) / 1000)+ 'sec');
 BipConfirmation;
end;



end.
