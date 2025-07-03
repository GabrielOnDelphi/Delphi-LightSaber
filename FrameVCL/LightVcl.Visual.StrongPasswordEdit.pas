UNIT LightVcl.Visual.StrongPasswordEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file

   An edit box where you can enter a password.
   The strength of the password is color coded (green for strong password, red for poor password)
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Messages, Winapi.Windows, vcl.Dialogs,
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
  System.Math, LightVcl.Common.SystemSecurity, LightVcl.Common.System;



constructor TStrongPasswordEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clRed;   // Default background color
  Hint:= 'It is reccomended to use uppercase, lowercase, numbers & signs.'
end;


procedure TStrongPasswordEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateBackgroundColor;
end;


procedure TStrongPasswordEdit.Change;
begin
  inherited Change;
  UpdateBackgroundColor;  // Update the background color based on the new text
end;



procedure TStrongPasswordEdit.UpdateBackgroundColor;
var
  Strength: Integer;
  GreenValue: Byte;
begin
  Strength := CalculatePasswordStrength(Text);

  // Map strength to a green value (0 to 255)
  GreenValue := Min(255, Strength * 51);  // 5 levels (0..5), 51 increment each level

  // Set the background color from red to green based on strength
  Color := RGB(255 - GreenValue, GreenValue, 0);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TStrongPasswordEdit]);
end;


end.
