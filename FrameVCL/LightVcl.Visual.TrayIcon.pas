UNIT LightVcl.Visual.TrayIcon;

{=============================================================================================================
   2026.11.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  Features:
       PutIconInTaskbar
       PutIconInSystray

  How to use it:

   procedure TfrmTester.ApplicationEventsMinimize(Sender: TObject);
   begin
     TrayIcon.PutIconInSystray;
   end;

   procedure TfrmTester.TrayIconClick(Sender: TObject);
   begin
     TrayIcon.PutIconInTaskbar;
   end;


  IMPORTANT
    Set Application.ShowMainFormOnTaskbar := False to keep the application's button from appearing on the Windows Taskbar.

   Use balloon like this:
     BalloonTitle := 'Restoring the window.';
     BalloonHint  := 'Click the system tray icon to restore the window.';
     BalloonFlags := bfInfo;

  FocusFormOnShow (default TRUE): PutIconInTaskbar ends with Application.BringToFront, which calls SetForegroundWindow.
  Set it to FALSE in an AUTOPILOT build, so a test that restores the window does not take the keyboard from the user.
  Same field and same purpose as in TCoolTrayIcon.
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.Classes,
  Vcl.Forms, Vcl.ExtCtrls;

TYPE
  TCubicTrayIcon = class(TTrayIcon)
  private
  public
    FocusFormOnShow: Boolean;
    constructor Create(Owner: TComponent); override;
    procedure PutIconInTaskbar;
    procedure PutIconInSystray;
    procedure PutIconInSystrayBalloon;              { This will also show the balloon IF BalloonHint is not empty. }
  end;

procedure Register;

IMPLEMENTATION


constructor TCubicTrayIcon.Create(Owner: TComponent);
begin
 inherited Create(Owner);
 FocusFormOnShow:= TRUE;
end;


procedure TCubicTrayIcon.PutIconInTaskbar;
begin
 Self.Visible := FALSE;                              { Hide the tray icon and show the window, setting its state property to wsNormal. }
 Application.MainForm.Show;
 Application.MainForm.WindowState:= wsNormal;
 ShowWindow(Application.Handle, SW_SHOW);            { This is mandatory with D2007 and up else the main form won't show when Application.MainFormOnTaskBar:= false;                http://delphi.about.com/od/delphitips2008/qt/hide_taskbutton.htm }
 Application.Restore;
 if FocusFormOnShow
 then Application.BringToFront;
end;



procedure TCubicTrayIcon.PutIconInSystray;
begin
{ ShowWindow(Application.Handle, SW_HIDE); }
 Application.MainForm.Hide;
 Application.MainForm.WindowState:= wsMinimized;
 Self.Visible:= TRUE;                                { Show the animated tray icon }
end;



procedure TCubicTrayIcon.PutIconInSystrayBalloon;   { This will also show the balloon IF BalloonHint is not empty. }
begin
 PutIconInSystray;
 ShowBalloonHint;
end;










procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicTrayIcon]);
end;


end.
