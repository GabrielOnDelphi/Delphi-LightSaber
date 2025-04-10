
UNIT cvGradientPanel;

{--------------------------------------------------------------------------------------------------
   2018.02.16
   TPanel with gradient
   Source: http://embarcadero.newsgroups.archived.at/public.delphi.vcl.components.using/201001/1001154053.html


   Also see:
      c:\MyProjects\Packages\Third party packages\GradientPanel.pas
--------------------------------------------------------------------------------------------------}
{todo 2: draw 3D frame on it }

INTERFACE
{$D-}                 { NoDebugInfo switch }
{$WARN GARBAGE OFF}    { Silent the: 'W1011 Text after final END' warning }

USES
  System.SysUtils, System.Classes, Vcl.ExtCtrls, Vcl.Graphics;


TYPE
  TGradPanel = class(TPanel)
    private
      FShowGrad: Boolean;
      FColor1: TColor;
      FColor2: TColor;
      procedure SetGradientFill(const Value: Boolean);
      procedure SetColor1(const Value: TColor);
      procedure SetColor2(const Value: TColor);
    protected
      procedure Paint; override;
    public
      property Canvas;                                                                       // if you want the panels canvas available elsewhere
      constructor Create(AOwner: TComponent); override;
    published
      property ShowGradient: Boolean read FShowGrad   write SetGradientFill default TRUE;
      property Color1    : TColor  read FColor1 write SetColor1   default $00D6D6D6;
      property Color2    : TColor  read FColor2 write SetColor2   default $00FFA4A4;
  end;



procedure Register;

IMPLEMENTATION
USES cGraphFX.Gradient;










constructor TGradPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);                                                        //del  Parent:= TWinControl(AOwner);  Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
  FShowGrad := TRUE;
  FColor1   := $00D6D6D6;
  FColor2   := $00FFA4A4;
end;


procedure TGradPanel.Paint;
begin
  inherited;
  if FShowGrad
  then GradientFill(Self, Color2, Color1);
end;


procedure TGradPanel.SetGradientFill(const Value: Boolean);
begin
  if FShowGrad <> Value then
   begin
    FShowGrad := Value;
    Invalidate;
   end;
end;


procedure TGradPanel.SetColor1(const Value: TColor);
begin
  if FColor1 <> Value then
   begin
    FColor1 := Value;
    Invalidate;
   end;
end;


procedure TGradPanel.SetColor2(const Value: TColor);
begin
  if FColor2 <> Value then
   begin
    FColor2 := Value;
    Invalidate;
   end;
end;





procedure Register;
begin
  RegisterComponents('LightSaber', [TGradPanel]);
end;


end.
