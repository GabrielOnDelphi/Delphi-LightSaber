UNIT LightVcl.Visual.StrongPasswordEdit;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt

   An edit box where you can enter a password.
   The strength of the password is color coded (green for strong password, red for poor password)
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Messages, Winapi.Windows, vcl.Dialogs,  { vcl.Dialogs appears unused }
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

type
  TStrongPasswordEdit = class(TEdit)
  private
    procedure UpdateBackgroundColor;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

IMPLEMENTATION

USES
  System.Math, LightVcl.Common.SystemSecurity, LightVcl.Common.System;  { LightVcl.Common.System appears unused - verify and remove if not needed }



constructor TStrongPasswordEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clRed;   // Default background color
  Hint:= 'It is recommended to use uppercase, lowercase, numbers & signs.';
end;


{ CM_TEXTCHANGED handles programmatic text changes (e.g., Text := 'x') }
procedure TStrongPasswordEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateBackgroundColor;
end;

{ Change handles user input (EN_CHANGE notification).
  Note: Both handlers call UpdateBackgroundColor to ensure coverage of all change scenarios.
  The function is idempotent, so duplicate calls are harmless. }
procedure TStrongPasswordEdit.Change;
begin
  inherited Change;
  UpdateBackgroundColor;
end;



procedure TStrongPasswordEdit.UpdateBackgroundColor;
var
  Strength: Integer;
  GreenValue: Byte;
begin
  Strength := CalculatePasswordStrength(Text);

  // Map strength (0-5) to green component (0-255). Each strength level adds 51 to green.
  GreenValue := Min(255, Strength * 51);

  // Set the background color from red to green based on strength
  Color := RGB(255 - GreenValue, GreenValue, 0);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TStrongPasswordEdit]);
end;


end.
