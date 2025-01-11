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
         TfrmMain = class(TForm)
          protected
            procedure LateInitialize; override; // Called after the main form was fully initilized
         end;


   How to use it

      Change the declaration of your form to TLightForm and override the LateInitialize.

      uses cbAppDataForm;
      Type
        TYourForm = class(TLightForm)
        protected
          procedure BeforeRelease;  override;    // Optional
        public
          procedure LateInitialize; override;    // Optional
        end;

       procedure TYourForm.LateInitialize;
       begin
         inherited LateInitialize;
         // Intialize your code here
       end;

       procedure TYourForm.BeforeRelease;
       begin
         // Release your resources here
         inherited BeforeRelease;
       end;

=============================================================================================================}

INTERFACE

USES
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  ccINIFile, cbIniFile; // Do not add dependencies higher than "cb" level

type
  TLightForm = class(TForm)
  private
    procedure SaveBeforeExit;
  protected
    Saved: Boolean;
    procedure DoDestroy; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure WMEndSession(var Msg: TWMEndSession);
    procedure BeforeRelease; virtual;
  public
    Loading: TFormLoading;

    procedure LateInitialize; virtual;

    function CloseQuery: boolean; override;
    constructor Create(AOwner: TComponent); override;
  published
    //property OnLateInitialize: TNotifyEvent read FOnLateInitialize write FOnLateInitialize;
  end;


IMPLEMENTATION

USES cbAppData;


constructor TLightForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Position:= poDesigned;
  showhint:= TRUE;
  screensnap:= TRUE;
  snapbuffer:= 4;

  Saved:= FALSE;
  Loading:= flPosOnly; // Default value. Can be overriden by AppData.CreateForm
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
      BeforeRelease;
    finally
      Saved:= TRUE;  // Make sure it is put to true even on accidents, otherwise we might call it multiple times.
    end;
  end;
end;


{ Called ONLY once, when Saved = False }
procedure TLightForm.BeforeRelease;
begin
  Assert(NOT Saved);
  cbIniFile.SaveFormBase(Self);
end;



end.



