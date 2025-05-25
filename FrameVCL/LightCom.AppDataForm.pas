UNIT LightCom.AppDataForm;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Motivation - Where to initialize own code?

      VCL forms offer no good place where to execute your initialization/finalization code.
      OnCreate is too early, and OnShow may never be called (or called too late) or called multiple times.

      The TLightForm provides two places that (in conjunction with TAppData) offer you two methods that are guaranteed to be executed:
      TMyForm = class(TLightForm)
          protected
            procedure FormPostInitialize; override; // Called after the main form was fully initialized.
            procedure FormPreRelease;
         end;



   How to use it

      Change the declaration of your form from TForm to TLightForm.
      Optionally, if you want to execute your own initialization code, override the LateInitialize (don't forget to call inherited).

      uses LightCom.AppDataForm;
      Type
        TYourForm = class(TLightForm)
        protected
        public
          procedure FormPostInitialize;  override;    // Optional
        end;

       procedure TYourForm.FormPostInitialize;
       begin
         inherited FormPostInitialize;

         // Initialize your own code here
       end;

--------------------------------------------------------------------------------------------------------------

  Extra features

      FormPreRelease

         Optionally you can use the FormPreRelease event to execute your code on application shutdown.
         Unlike other events, FormPreRelease is always called, and it is guaranteed to be called once and only once!

         Execution order: FormPreRelease -> FormClose -> FormDestroy

      Self saving forms

         Using SaveForm/LoadForm, a form can save its status (including checkboxes/radio buttons/etc on it))
         to disk on shutdown and resume exaclty from where it left on application startup.

         LoadForm is automatically called by TAppData.CreateForm(). Therefore, you must create all your forms with this method.
         The TLightForm.SaveForm is called automatically when the form closes.

         Override SaveForm/LoadForm if you want to do your own loading/saving (in this case, don't call inherited)!

=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.IniFiles, Vcl.Controls, Vcl.Forms,
  LightCom.Dialogs, LightCom.CenterControl, LightCom.IniFile, ccAppData; // Do not add dependencies higher than "cb" level

TYPE
  TLightForm = class(TForm)
  private
    FCloseOnEscape: Boolean;
    FAutoSaveForm: TAutoState;
  protected
    Saved: Boolean;
    procedure saveBeforeExit;

    procedure DoDestroy; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure WMEndSession(var Msg: TWMEndSession);
    procedure FormKeyPress(Sender: TObject; var Key: Char);  // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
  public
    constructor Create(AOwner: TComponent; AutoSaveForm: TAutoState); reintroduce; overload; virtual;
    function  CloseQuery: boolean; override;

    procedure FormPostInitialize; virtual;   // Takes place after the form was fully created
    procedure FormPreRelease; virtual;       // Takes place before the form is destroyed. It is guaranteed to be called excetly once.

    procedure LoadForm; virtual;
    procedure SaveForm; virtual;
  published
    property AutoState: TAutoState   read FAutoSaveForm   write FAutoSaveForm;
    property CloseOnEscape: Boolean  read FCloseOnEscape  write FCloseOnEscape;    // Close this form when the Esc key is pressed
  end;


IMPLEMENTATION
USES
  LightCom.AppData;


constructor TLightForm.Create(AOwner: TComponent; AutoSaveForm: TAutoState);
begin
  inherited Create(AOwner);

  ScreenSnap:= TRUE;
  Position  := poDesigned;  // Without this we cannot restore form's position on screen!
  Showhint  := TRUE;
  Saved     := FALSE;

  FAutoSaveForm := AutoSaveForm; // Default value. Can be overriden by AppData.CreateForm
end;


procedure TLightForm.FormPostInitialize;
begin
  // This can be overridden by the user to implement initialization after the form is ready
end;


procedure TLightForm.WMEndSession(var Msg: TWMEndSession);
begin
  saveBeforeExit;
  inherited;
end;

procedure TLightForm.DoDestroy;
begin
  saveBeforeExit;
  inherited;
end;

procedure TLightForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if Action = TCloseAction.caFree then saveBeforeExit;
end;


function TLightForm.CloseQuery: Boolean;  // Correct method name
begin
  saveBeforeExit;
  Result:= inherited CloseQuery;
end;


{ This code is guaranteed to be called ONLY once.

  Tech details: It is enough to put SaveBeforeExit in these two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TLightForm.saveBeforeExit;
begin
  if NOT Saved
  AND NOT AppData.Initializing then
  begin
    try
      FormPreRelease;

      if AutoState > asNone  // Give the user the option not to save the form
      then SaveForm;
    finally
      Saved:= TRUE;             // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


{ Called ONLY once, when Saved = False }
procedure TLightForm.FormPreRelease;
begin
  // Give user a chance to call its own finalization code (guaranteed once)
end;


procedure TLightForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if CloseOnEscape
  AND (Ord(Key) = VK_ESCAPE)
  then Close;
end;









{-----------------------------------------------------------------------------------------------------------------------
   MAIN

   Load/Save all controls on this form to their initial state.

   Parameters:
         OnlyFormPos=False  ->  Save all supported controls on this form
         OnlyFormPos=True   ->  It will only save the position of the form (only Left/Top, no width/height/WndState)


   Also see LoadForm/SaveForm in cvINIFile.pas
-----------------------------------------------------------------------------------------------------------------------}

procedure TLightForm.SaveForm;
VAR
   IniFile: TIniFileApp;
begin
  if TAppDataCore.Initializing
  AND (Self= Application.MainForm) then
   begin
     if TAppDataCore.RunningHome
     then MessageError('Closing application while still initializing!');
     Exit; // We don't save anything if the start up was improper!
   end;

  IniFile:= TIniFileApp.Create(Self.Name);
  TRY
   TRY
     IniFile.SaveForm(Self, AutoState);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot save INI file: '+ IniFile.FileName);
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;
end;



{ Override this method if you want to do your own loading from INI file. In this case, don't call inherited!

  LoadForm also does:
    * Set the font for all forms to be the same as the font of the MainForm.
    * If the form is out of screen, LoadForm will also bring the form back to screen. }
procedure TLightForm.LoadForm;
VAR
   IniFile: TIniFileApp;
begin
  IniFile:= TIniFileApp.Create(Self.Name);
  TRY
   TRY
     IniFile.LoadForm(Self, AutoState);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot load INI file: '+ IniFile.FileName);
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;

  CorrectFormPositionDesktop(Self); // Form is off screen?

  { Set font for secondary forms }
  {moved to TAppData.setGuiProperties
  if Self <> Application.MainForm
  then Self.Font:= AppData.Font;  }
end;



end.
