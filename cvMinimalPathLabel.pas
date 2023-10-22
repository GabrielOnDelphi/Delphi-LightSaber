UNIT cvMinimalPathLabel;

{--------------------------------------------------------------------------------------------------
  CubicDesign
  2013.09

  When displaying a path, truncates the middle of the text if the entire path cannot fit into the visible area of the control.
  It ONLY works wot filenames and paths because of MinimizeName!
  If ShowFullTextAsHint is true then the entire text will be shown into the Hint.

--------------------------------------------------------------------------------------------------}

INTERFACE
{.$D-}    { 'NoDebugInfo' switch  }
{$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.FileCtrl;

TYPE
  TMinimalPathLabel = class(TLabel)
   private
    FFulCaption: string;
    FShowFull: Boolean;
    procedure SetCaption(Value: string);
   public
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
   published
    property CaptionMin: string read FFulCaption write SetCaption;
    property ShowFullTextAsHint: Boolean read FShowFull write FShowFull default TRUE;
  end;

procedure Register;

IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }







constructor TMinimalPathLabel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                                                         // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
 ShowHint:= TRUE;
 FShowFull:= TRUE;
 FFulCaption:= 'Minimized text';
end;


procedure TMinimalPathLabel.SetCaption(Value: string);
begin
 FFulCaption:= Value;
 Caption:= Vcl.FileCtrl.MinimizeName(Value, Canvas, Width);
 if FShowFull
 then Hint:= Value;
end;


procedure TMinimalPathLabel.Resize;
begin
 inherited;
 Caption:= MinimizeName(FFulCaption, Canvas, Width);
end;














procedure Register;
begin
  RegisterComponents('LightSaber', [TMinimalPathLabel]);
end;

end.{============================================================================
