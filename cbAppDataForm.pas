UNIT cbAppDataForm;

{=============================================================================================================
   Gabriel Moraru
   2025.01.10
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   How to use it:

   Change the declaration of your form to TLightForm and override the LateInitialize.

   uses cbAppDataForm;

   Type
     TYourForm = class(TLightForm)
     public
       procedure LateInitialize; override;
     end;

    procedure TYourForm.LateInitialize;
    begin
      inherited LateInitialize;
      // You code here
    end;


=============================================================================================================}

INTERFACE

USES
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  ccINIFile, cbIniFile; // Do not add dependencies higher than "cb" level

type
  TLightForm = class(TForm)
  private
  protected
    Saved: Boolean;
    procedure DoDestroy; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure WMEndSession(var Msg: TWMEndSession);
  public
    Loading: TFormLoading;
    procedure LateInitialize; virtual;
    procedure SaveBeforeExit;
    function  CloseQuery: boolean; override;
    constructor Create(AOwner: TComponent); override;
  published
    //property OnLateInitialize: TNotifyEvent read FOnLateInitialize write FOnLateInitialize;
  end;


IMPLEMENTATION


constructor TLightForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Saved:= FALSE;
  Loading:= flPosOnly;
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
  if NOT Saved then
  begin
    Saved := TRUE;
    cbIniFile.SaveFormBase(Self);
  end;
end;


end.



