UNIT FormSplashScreen;
{-------------------------------------------------------------------------------------------------------------
 How to use it:
   Load PNG. Set its transparency to match background's color.
   Set form transparency to the same color.
   ShowSplashScreen(SizeOfImage);

   Main Form:
   Use this if you want to create first frmSplash but don't want to use it as MainForm:
     frmSplash:= TfrmSplash.Create(Application);
   The splash screen it is not the Main form because it is not created with 'Application.CreateForm'.

   Running home:
     fsStayOnTop interferes with the IDE, when I debug BioniX during its startup. So, I better don't load the SplashScreen at all!
     Warning (EmbarcaderoHelp): It is not advisable to change FormStyle (fsStayOnTop) at runtime.


 DON'T ADD IT TO ANY DPK!

 Tester:
     c:\MyProjects\Project Testers\SplashScreen.pas\SplashTester.dpr
-------------------------------------------------------------------------------------------------------------}
INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.ExtCtrls, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;

TYPE
  TfrmSplash = class(TLightForm)
    imgSplash: TImage;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure imgSplashClick(Sender: TObject);
  private
    Increment: Integer; { First we make the splash visible }
    CurrAlpha: Integer;
  public
  end;

procedure ShowSplashScreen(ImgFileSize: integer);


IMPLEMENTATION  {$R *.dfm}
USES LightCore.IO, LightVcl.Common.CenterControl, LightCore.AppData, LightVcl.Common.AppData
;

CONST
  StepUp    = 30;  { The higher the number the faster the speed (less time on screen) }
  StepDown  = -12;
  StartValue= 40;  { This is the alpha value from which we start }
  EndValue  = 90;  { When alpha is under this value we simply close the splash screen }




procedure ShowSplashScreen(ImgFileSize: integer);
VAR frmSplash: TfrmSplash;
begin
 if FileExists(AppData.SysDir+ 'NoSplashScreen') then EXIT;
 if AppData.RunningFirstTime then EXIT; { It is confusing for the user to see this splash screen when the program starts for the first time AND the "setup" wizard is presented. }

 frmSplash:= TfrmSplash.Create(Application);
 Assert(frmSplash.Name= 'frmSplash', 'Dont change form name because of TfrmSettings.spnOpacityChange!'); { We don't apply custom transparency to the splash screen. It handles the transparency by its own. }

 { Protection }
 if NOT FileExists(AppData.SysDir+'Splash.png')
 AND (GetFileSize(AppData.SysDir+'Splash.png') = ImgFileSize) then
  begin
    FreeAndNil(frmSplash);
    EXIT;
  end;

 { Load logo img }
 TRY
   frmSplash.imgSplash.Picture.LoadFromFile(AppData.SysDir+'Splash.png');
 EXCEPT
   MessageWarning('The program cannot load its logo/graphics. Your antivirus is probably a bit to overzealous!'+ CRLF+'(Are you using IOBit? Consider switching to a better program.)');
   FreeAndNil(frmSplash);
   EXIT;
 END;

 { Form setup }
 frmSplash.AlphaBlendValue:= StartValue;
 frmSplash.CurrAlpha:= frmSplash.AlphaBlendValue;
 frmSplash.Increment:= StepUp;   { First we make the splash visible }
 CenterForm(frmSplash);
 frmSplash.Show;                 { Cannot center splash screen to BioniX main form because at this point we don't know the position of the main form yet (INI not loaded). }
 frmSplash.TimerTimer(NIL);      { Show the first step now }
 frmSplash.Timer.Enabled:= TRUE;
end;


procedure TfrmSplash.TimerTimer(Sender: TObject);
begin
 BringToFront;
 Refresh;

 { Time to close the splash? }
 if  (Increment < 0)
 AND (CurrAlpha < EndValue) then
  begin
   Timer.Enabled:= FALSE;
   Close;
   EXIT;
  end;

 CurrAlpha:= CurrAlpha+ Increment;

 { Fade out }
 if CurrAlpha >= 255 then
  begin
   Increment:= StepDown;
   CurrAlpha:= 255;
   BringToFront;
  end;

 if CurrAlpha < 0
 then CurrAlpha:= 0;
 AlphaBlendValue:= CurrAlpha;
end;


procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
 EmptyDummy;
end;


procedure TfrmSplash.imgSplashClick(Sender: TObject);
begin
 Close;
end;


procedure TfrmSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:= caFree;      // mandatory. reason: https://stackoverflow.com/questions/45611162/how-to-create-a-form-before-the-mainform
 {Action:= caFree; WARNING: Delphi bug: https://quality.embarcadero.com/browse/RSP-33140. Fixed in Alexandria }
end;


end.
