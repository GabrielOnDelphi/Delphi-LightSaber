UNIT cbAppDataForm;

{=============================================================================================================
   Gabriel Moraru
   2025.01.26
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Motivation - Where to initialize own code?

      VCL forms offer no good place where to execute your initialization/finalization code.
      OnCreate is too early, and OnShow may never be called (or called too late) or called multiple times.

      The TLightForm provides two places that (in conjunction with TAppData) offer you two methods that are guaranteed to be executed:
      TMyForm = class(TLightForm)
          protected
            procedure FormInitialize; override; // Called after the main form was fully initialized.
            property OnBeforeRelease;
         end;



   How to use it

      Change the declaration of your form from TForm to TLightForm.
      Optionally, if you want to execute your own initialization code, override the LateInitialize (don't forget to call inherited).

      uses cbAppDataForm;
      Type
        TYourForm = class(TLightForm)
        protected
        public
          procedure FormInitialize;  override;    // Optional
        end;

       procedure TYourForm.FormInitialize;
       begin
         inherited FormInitialize;

         // Initialize your own code here
       end;

--------------------------------------------------------------------------------------------------------------

  Extra features

      OnBeforeRelease

         Optionally you can use the OnBeforeRelease event to execute your code on application shutdown.
         Unlike other events, OnBeforeRelease is always called, and it is guaranteed to be called once and only once!


      Self saving forms

         Using SaveForm/LoadForm, a form can save its status (including checkboxes/radio buttons/etc on it))
         to disk on shutdown and resume exaclty from where it left on application startup.

         LoadForm is automatically called by TAppData.CreateForm(). Therefore, you must create all your forms with this method.
         The TLightForm.SaveForm is called automatically when the form closes.

         Override SaveForm/LoadForm if you want to do your own loading/saving (in this case, don't call inherited)!

=============================================================================================================}

INTERFACE

USES
  Winapi.Messages,
  System.SysUtils, System.Classes, System.IniFiles,
  Vcl.Controls, Vcl.Forms,
  cbDialogs,
  cbCenterControl,
  cbIniFile,
  ccINIFile; // Do not add dependencies higher than "cb" level

type
  TLightForm = class(TForm)
  private
    FAutoSaveForm: TAutoState;  // We can use this later in the destructor to know how to save the form: flPosOnly/flFull
    //FOnRelease: TNotifyEvent;
  protected
    Saved: Boolean;
    procedure saveBeforeExit;

    procedure DoDestroy; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure WMEndSession(var Msg: TWMEndSession);
  public
    constructor Create(AOwner: TComponent; AutoSaveForm: TAutoState); reintroduce; overload; virtual;
    function CloseQuery: boolean; override;

    procedure FormInitialize; virtual;
    procedure FormRelease; virtual;

    procedure LoadForm; virtual;
    procedure SaveForm; virtual;
  published
    property AutoSaveForm   : TAutoState   read FAutoSaveForm  write FAutoSaveForm;
    // Events
    //del property OnRelease: TNotifyEvent read FOnRelease write FOnRelease;
  end;


IMPLEMENTATION
USES
  cbAppData;


constructor TLightForm.Create(AOwner: TComponent; AutoSaveForm: TAutoState);
begin
  inherited Create(AOwner);

  ScreenSnap:= TRUE;
  Position  := poDesigned;
  Showhint  := TRUE;
  Saved     := FALSE;

  FAutoSaveForm := AutoSaveForm; // Default value. Can be overriden by AppData.CreateForm
end;


procedure TLightForm.FormInitialize;
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
      FormRelease;

      if AutoSaveForm > asNone  // Give the user the option not to save the form
      then SaveForm;
    finally
      Saved:= TRUE;             // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


procedure TLightForm.FormRelease;
begin
  // Give user a chance to call its own finalization code (guaranteed once)
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
 if TAppData.Initializing
 AND (Self= Application.MainForm) then
  begin
   if TAppData.RunningHome
   then MesajError('Closing application while still initializing!');
   Exit; // We don't save anything if the start up was improper!
  end;

 Assert(AppData <> NIL, '!!!');
 IniFile:= TIniFileApp.Create(Self.Name);
 TRY
  TRY
    IniFile.SaveForm(Self);
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
 if AppData = NIL then                { If AppData exists, let it deal with the font }
   if (Application.MainForm <> NIL)     { Set font only for secondary forms }
   AND (Self <> Application.MainForm)
   then Self.Font:= Application.MainForm.Font;

 IniFile:= TIniFileApp.Create(Self.Name);
 TRY
  TRY
    IniFile.LoadForm(Self);
    CorrectFormPositionScreen(Self);
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



