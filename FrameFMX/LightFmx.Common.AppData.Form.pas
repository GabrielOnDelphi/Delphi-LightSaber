UNIT LightFmx.Common.AppData.Form;

{=============================================================================================================
   2025.08
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   Motivation - Where to initialize own code?

   FormPreRelease

      The TLightForm provides the FormPreRelease method that, unlike other events, is guaranteed to be executed once and only once when the form is closed.

      Execution order: FormPreRelease -> FormClose -> FormDestroy

      How to use it

         Change the declaration of your form from TForm to TLightForm.
         Optionally, if you want to execute your own initialization code, override the LateInitialize (don't forget to call inherited).
         See: c:\Projects\LightSaber\Demo\Template App\FMX Minimal\TemplateMicro_Fmx.dpr

         uses Light_FMX.Common.AppDataForm;
         Type
           TYourForm = class(TLightForm)
           public
             procedure FormPreRelease; override;
           end;

           procedure TYourForm.FormPreRelease;
           begin
             // Free your stuff here
             inherited FormPreRelease;
           end;


--------------------------------------------------------------------------------------------------------------

      Self saving forms

         Using SaveForm/LoadForm, a form can save its status (including checkboxes/radio buttons/etc on it))
         to disk on shutdown and resume exaclty from where it left on application startup.

         LoadForm is automatically called by TAppData.CreateForm(). Therefore, you must create all your forms with this method.
         The TLightForm.SaveForm is called automatically when the form closes.

         Override SaveForm/LoadForm if you want to do your own loading/saving (in this case, don't call inherited)!

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.UITypes, System.IniFiles,
  FMX.Forms, FMX.Types,
  LightFmx.Common.IniFile, LightCore.AppData;

TYPE
  TLightForm = class(TForm)
  private
    FCloseOnEscape: Boolean;
    FAutoState: TAutoState;
    procedure SetGuiProperties(Form: TForm);
  protected
    Saved: Boolean;
    procedure saveBeforeExit;

    procedure FormKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState); // We can use this later in the destructor to know how to save the form: asPosOnly/asFull
    procedure Loaded; override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent; aAutoState: TAutoState); reintroduce; overload; virtual;
    function CloseQuery: Boolean; override;

    procedure FormPreRelease; virtual;

    procedure LoadForm; virtual;
    procedure SaveForm; virtual;
  published
    property AutoState: TAutoState   read FAutoState;                              // The user needs to set this property if they want to auto save/load the form.
    property CloseOnEscape: Boolean  read FCloseOnEscape  write FCloseOnEscape;    // Close this form when the Esc key is pressed
  end;


IMPLEMENTATION
USES
  LightFmx.Common.AppData, LightFmx.Common.CenterControl, LightCore;


constructor TLightForm.Create(AOwner: TComponent; aAutoState: TAutoState);
begin
  inherited Create(AOwner);

  Showhint:= TRUE;
  Saved   := FALSE;
  FAutoState:= aAutoState;

  FAutoState:= asUndefined; // Default value. Can be overriden by AppData.CreateForm
end;


// In FMX, Loaded is called before FormCreate!
procedure TLightForm.Loaded;
begin
  Position:= TFormPosition.Designed;
  AppData.MainFormCaption('Initializing...');

  inherited Loaded;

  // Font, snap, alpha
  SetGuiProperties(Self);

  // Off-screen?
  if AppData.RunningFirstTime
  then EnsureFormVisibleOnScreen(Self);

  // Show app name
  AppData.MainFormCaption('');

  // Load form
  // Limitation: At this point we can only load "standard" Delphi components. Loading of our Light components can only be done in Light_FMX.Visual.INIFile.pas -> TIniFileVCL

  if FAutoState = asUndefined  // Only check queue if AutoState wasn’t set in Create
  then FAutoState:= AppData.GetAutoState;

  if AutoState = asUndefined
  then RAISE Exception.Create('The user must set the AutoState via AppData.CreateForm()' + CRLF + 'Form: '+ Name+ ' / '+ ClassName)
  else
    if AutoState <> asNone
    then LoadForm;

  // Ignore the "Show" parameter if "StartMinimized" is active
  // Note: FMX: CreateForm does not create the given form immediately. It just adds a request to the pending list. RealCreateForms creates the real forms.
  if NOT AppData.StartMinim
  AND Visible
  then Show;
end;


// Font, snap, alpha
procedure TLightForm.SetGuiProperties(Form: TForm);
begin
  {$IFDEF FullAppData}
   // Font
   if Form = Application.MainForm
   then Self.Font:= Form.Font   // We TAKE the font from the main form. Then we apply it to all existing and all future windows.
   else
     if Self.Font <> nil
     then Form.Font:= Self.Font;  // We set the same font for secondary forms

   // Form transparency
   Form.AlphaBlendValue := Opacity;
   Form.AlphaBlend:= Opacity< 255;
  {$ENDIF}
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


procedure TLightForm.FormKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if CloseOnEscape
  and (Key = vkEscape) 
  then Close;
end;









{-----------------------------------------------------------------------------------------------------------------------
   MAIN

   Load/Save all controls on this form to their initial state.

   Parameters:
         OnlyFormPos=False  ->  Save all supported controls on this form
         OnlyFormPos=True   ->  It will only save the position of the form (only Left/Top, no width/height/WndState)


   Also see LoadForm/SaveForm in Light_FMX.Visual.INIFile.pas
-----------------------------------------------------------------------------------------------------------------------}

procedure TLightForm.SaveForm;
VAR
   IniFile: TIniFileApp;
begin
  if TAppData.Initializing
  AND (Self= Application.MainForm) then
   begin
    //if TAppDataCore.RunningHome
    //then MessageError('Closing application while still initializing!');
    Exit; // We don't save anything if the start up was improper!
   end;

  Assert(AppData <> NIL, '!!!');
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
  (*if AppData = NIL then                { If AppData exists, let it deal with the font }
    if (Application.MainForm <> NIL)     { Set font only for secondary forms }
    AND (Self <> Application.MainForm)
    then Self.Font:= Application.MainForm.Font; *)

  IniFile:= TIniFileApp.Create(Self.Name);
  TRY
   TRY
     IniFile.LoadForm(Self, AutoState);
     //CorrectFormPositionScreen [or] CorrectFormPositionDesktop(Self);
   EXCEPT
     ON EIniFileException DO
       if AppData <> NIL
       then AppData.LogWarn('Cannot load INI file: '+ IniFile.FileName);
   END;
  FINALLY
    FreeAndNil(IniFile);
  END;
end;



end.
