UNIT FormSplashScreen;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   SPLASH SCREEN

   Displays a fade-in/fade-out splash screen during application startup.
   The splash screen loads a PNG image with transparency support.

   WARNING:
     * DON'T ADD THIS UNIT TO ANY DPK! (enforced by $DENYPACKAGEUNIT)
     * fsStayOnTop interferes with the IDE when debugging during startup
     * It is not advisable to change FormStyle (fsStayOnTop) at runtime

   USAGE:
     1. Place 'Splash.png' in AppData.AppSysDir folder
     2. Set PNG transparency to match background color
     3. Call: ShowSplashScreen(SizeOfImage)

   NOTES:
     - The splash screen is NOT the MainForm (created with TfrmSplash.Create, not Application.CreateForm)
     - Create a file 'NoSplashScreen' in AppSysDir to disable the splash
     - Splash is not shown on first run (when setup wizard appears)
     - Click the image to close the splash immediately

   TESTER:
     c:\MyProjects\Project Testers\SplashScreen.pas\SplashTester.dpr
=============================================================================================================}

INTERFACE
{$DENYPACKAGEUNIT ON}

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, LightVcl.Visual.AppDataForm,Vcl.ExtCtrls, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;

TYPE
  TfrmSplash = class(TLightForm)
    imgSplash: TImage;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure imgSplashClick(Sender: TObject);
  private
    FIncrement: Integer;  { Alpha step: positive=fade in, negative=fade out }
    FCurrAlpha: Integer;  { Current alpha blend value (0-255) }
  end;

{ Shows the splash screen with fade-in/fade-out animation.
  ImgFileSize: Expected size of Splash.png for integrity check. Pass 0 to skip the size check. }
procedure ShowSplashScreen(ImgFileSize: Integer);


IMPLEMENTATION {$R *.dfm}
USES LightCore.IO, LightVcl.Common.CenterControl, LightCore.AppData, LightVcl.Visual.AppData;

CONST
  { Alpha animation constants }
  ALPHA_STEP_UP   = 30;   { Fade-in step (higher = faster) }
  ALPHA_STEP_DOWN = -12;  { Fade-out step (negative value) }
  ALPHA_START     = 40;   { Initial alpha value when splash appears }
  ALPHA_END       = 90;   { Close splash when alpha falls below this during fade-out }
  SPLASH_FILENAME = 'Splash.png';




procedure ShowSplashScreen(ImgFileSize: Integer);
var
  frmSplash: TfrmSplash;
  SplashPath: string;
begin
  { Check if splash is disabled or should be skipped }
  if FileExists(Appdata.AppSysDir + 'NoSplashScreen')
  then EXIT;

  { Don't show splash on first run - setup wizard will appear instead }
  if AppData.RunningFirstTime
  then EXIT;

  SplashPath:= Appdata.AppSysDir + SPLASH_FILENAME;

  { Verify splash image exists and has expected size (integrity check) }
  if NOT FileExists(SplashPath)
  then EXIT;

  if (ImgFileSize > 0) AND (GetFileSize(SplashPath) <> ImgFileSize)
  then EXIT;

  { Create the splash form }
  frmSplash:= TfrmSplash.Create(Application);

  { Form name must not change - other forms check for it to skip opacity changes }
  Assert(frmSplash.Name = 'frmSplash',
    'Do not change form name - TfrmSettings.spnOpacityChange depends on it!');

  { Load the splash image }
  try
    frmSplash.imgSplash.Picture.LoadFromFile(SplashPath);
  except
    on E: Exception do
    begin
      MessageWarning('The program cannot load its logo/graphics. ' +
        'Your antivirus may be blocking access.' + CRLF +
        '(If using IOBit, consider switching to a better program.)');
      FreeAndNil(frmSplash);
      EXIT;
    end;
  end;

  { Initialize alpha animation }
  frmSplash.AlphaBlendValue:= ALPHA_START;
  frmSplash.FCurrAlpha:= frmSplash.AlphaBlendValue;
  frmSplash.FIncrement:= ALPHA_STEP_UP;  { Start with fade-in }

  { Show the splash }
  CenterForm(frmSplash);
  frmSplash.Show;
  { Note: Cannot center to main form - INI not loaded yet, position unknown }

  { Trigger first animation step immediately }
  frmSplash.TimerTimer(NIL);
  frmSplash.Timer.Enabled:= TRUE;
end;


{ Handles the fade-in/fade-out animation.
  Called by timer at regular intervals to update alpha transparency. }
procedure TfrmSplash.TimerTimer(Sender: TObject);
begin
  BringToFront;
  Refresh;

  { Check if fade-out is complete - time to close }
  if (FIncrement < 0) 
  AND (FCurrAlpha < ALPHA_END) then
  begin
    Timer.Enabled:= FALSE;
    Close;
    EXIT;
  end;

  { Update alpha value }
  FCurrAlpha:= FCurrAlpha + FIncrement;

  { Check if fade-in is complete - switch to fade-out }
  if FCurrAlpha >= 255 then
  begin
    FIncrement:= ALPHA_STEP_DOWN;
    FCurrAlpha:= 255;
    BringToFront;
  end;

  { Clamp alpha to valid range }
  if FCurrAlpha < 0
  then FCurrAlpha:= 0;

  AlphaBlendValue:= FCurrAlpha;
end;


procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
end;


{ Allows user to close splash immediately by clicking on it }
procedure TfrmSplash.imgSplashClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Must use caFree because splash is created before MainForm.
    See: https://stackoverflow.com/questions/45611162/how-to-create-a-form-before-the-mainform
    Note: caFree bug (RSP-33140) was fixed in Delphi 11 (Alexandria) }
  Action:= caFree;
end;


end.
