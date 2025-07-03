UNIT LightVcl.Visual.TrayIcon;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
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
  Set Application.ShowMainFormOnTaskbar := False;' to keep the application's button from appearing on the Windows Taskbar.

 Use ballon like this:
   BalloonTitle := 'Restoring the window.';
   BalloonHint  := 'Click the system tray icon to restore the window.';
   BalloonFlags := bfInfo;
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.Classes,
  Vcl.Forms, Vcl.ExtCtrls;

TYPE
  TCubicTrayIcon = class(TTrayIcon)
  private
  public
    procedure PutIconInTaskbar;
    procedure PutIconInSystray;
    procedure PutIconInSystrayBallon;               { This will also show the ballon IF BalloonHint is not empty. }
  end;

procedure Register;

IMPLEMENTATION


procedure TCubicTrayIcon.PutIconInTaskbar;
begin
 Self.Visible := FALSE;                              { Hide the tray icon and show the window, setting its state property to wsNormal. }
 Application.MainForm.Show;
 Application.MainForm.WindowState:= wsNormal;
 ShowWindow(Application.Handle, SW_SHOW);            { This is mandatory with D2007 and up else the main form won't show when Application.MainFormOnTaskBar:= false;                http://delphi.about.com/od/delphitips2008/qt/hide_taskbutton.htm }
 Application.Restore;
 Application.BringToFront;
end;



procedure TCubicTrayIcon.PutIconInSystray;
begin
{ ShowWindow(Application.Handle, SW_HIDE); }
 Application.MainForm.Hide;
 Application.MainForm.WindowState:= wsMinimized;
 Self.Visible:= TRUE;                                { Show the animated tray icon }
end;



procedure TCubicTrayIcon.PutIconInSystrayBallon;    { This will also show the ballon IF BalloonHint is not empty. }
begin
 PutIconInSystray;
 ShowBalloonHint;
end;










procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicTrayIcon]);
end;




end.
