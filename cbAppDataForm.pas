UNIT cbAppDataForm;

{=============================================================================================================
   Gabriel Moraru
   2025.01.10
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   Motivation

      OnFormCreate and OnFormShow is the worst place to initialize your code. FormCreate is too early and OnShow may never be called (or called too late) or called multiple times.
      Instead, your form can implement the LateInitialize message handler.
      This will be called after the form was fully created and the application finished initializing.
      Example:
         TfrmMain = class(TLightForm)
          protected
            procedure LateInitialize; override; // Called after the main form was fully initilized. Don't forget to call "Inherited LateInitialize"
         end;


   How to use it

      Change the declaration of your form to TLightForm and override the LateInitialize.

      uses cbAppDataForm;
      Type
        TYourForm = class(TLightForm)
        protected
        public
          procedure LateInitialize;  override;    // Optional
        end;

       procedure TYourForm.LateInitialize;
       begin
         inherited LateInitialize;
         // Intialize your code here
       end;

       Optionally set the BeforeRelease event hanlder


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
    FAutoSaveForm: TFormLoading;  // We can use this later in the destructor to know how to save the form: flPosOnly/flFull
    FBeforeRelease: TNotifyEvent;
  protected
    Saved: Boolean;
    procedure SaveBeforeExit;

    procedure DoDestroy; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure WMEndSession(var Msg: TWMEndSession);
  public
    procedure LateInitialize; virtual;

    function CloseQuery: boolean; override;
    constructor Create(AOwner: TComponent; AutoSaveForm: TFormLoading); reintroduce; overload; virtual;

    procedure LoadForm;
    procedure SaveForm;
  published
    property AutoSaveForm: TFormLoading    read FAutoSaveForm  write FAutoSaveForm;
    // Events
    property OnBeforeRelease: TNotifyEvent read FBeforeRelease write FBeforeRelease;
  end;


IMPLEMENTATION
USES
  cbAppData;


constructor TLightForm.Create(AOwner: TComponent; AutoSaveForm: TFormLoading);
begin
  inherited Create(AOwner);

  ScreenSnap:= TRUE;
  Position  := poDesigned;
  Showhint  := TRUE;
  Saved     := FALSE;

  FAutoSaveForm := AutoSaveForm; // Default value. Can be overriden by AppData.CreateForm
end;


procedure TLightForm.LateInitialize;
begin
  // This can be overridden by the user to implement initialization after the form is ready
end;


procedure TLightForm.WMEndSession(var Msg: TWMEndSession);
begin
  SaveBeforeExit;
  inherited;
end;

procedure TLightForm.DoDestroy;
begin
  SaveBeforeExit;
  inherited;
end;

procedure TLightForm.DoClose(var Action: TCloseAction);
begin
  SaveBeforeExit;
  inherited DoClose(Action);
end; 


function TLightForm.CloseQuery: Boolean;  // Correct method name
begin
  SaveBeforeExit;
  Result:= inherited CloseQuery;
end;


{ It is enough to put SaveBeforeExit in thse two places only: OnCloseQueryand & OnDestroy.
  Details: https://groups.google.com/forum/#!msg/borland.public.delphi.objectpascal/82AG0_kHonU/ft53lAjxWRMJ }
procedure TLightForm.SaveBeforeExit;
begin
  if NOT Saved
  AND NOT AppData.Initializing then
  begin
    try
      if Assigned(FBeforeRelease)
      then FBeforeRelease(Self); { Called ONLY once! }

      if AutoSaveForm <> flNone
      then SaveForm;
    finally
      Saved:= TRUE;  // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
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


{ It also does:
    * LoadForm will also set the font for all forms to be the same as the font of the MainForm.
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
      if appdata <> NIL
      then appdata.LogWarn('Cannot load INI file: '+ IniFile.FileName);
  END;
 FINALLY
   FreeAndNil(IniFile);
 END;
end;







end.



