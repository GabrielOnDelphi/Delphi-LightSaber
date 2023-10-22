UNIT cvStatusBar;

{
  v1.1
   allows other controls to be inserted into it.

// todo: https://zarko-gajic.iz.hr/implementing-custom-hint-for-each-panel-on-a-status-bar/#more-640
}

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.ComCtrls;

TYPE
  TcubicStatusBar = class(TStatusBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }



procedure Register;
begin
  RegisterComponents('LightSaber', [TcubicStatusBar]);
end;


constructor TcubicStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  SimplePanel:= TRUE;
end;


end.

