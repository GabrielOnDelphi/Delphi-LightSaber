UNIT LightVcl.Visual.MinimalPathLabel;

{=============================================================================================================
   2026.01
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  TMinimalPathLabel - A TLabel descendant that truncates the middle of file paths when they don't fit.

  When displaying a path, truncates the middle of the text if the entire path cannot fit into the
  visible area of the control. Uses Vcl.FileCtrl.MinimizeName internally.

  IMPORTANT: This component ONLY works with filenames and paths because MinimizeName is path-aware!
  For general text ellipsis, use LightVcl.Common.EllipsisText instead.

  If ShowFullTextAsHint is TRUE then the entire untruncated path will be shown in the Hint.

  Usage:
    MinimalPathLabel1.CaptionMin:= 'C:\Very\Long\Path\To\Some\File.txt';

--------------------------------------------------------------------------------------------------}

INTERFACE
{.$D-}    { 'NoDebugInfo' switch  }
{.$WARN UNIT_PLATFORM OFF}   {Silence the 'W1005 Unit Vcl.FileCtrl is specific to a platform' warning }

USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.FileCtrl;

TYPE
  TMinimalPathLabel = class(TLabel)
   private
    FFullCaption: string;
    FShowFull: Boolean;
    procedure SetCaption(Value: string);
    procedure UpdateMinimizedCaption;
   public
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
   published
    property CaptionMin: string read FFullCaption write SetCaption;
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
 FFullCaption:= 'Minimized text';
end;


{ Updates the minimized caption based on the current width.
  Called internally after Width changes or when CaptionMin is set. }
procedure TMinimalPathLabel.UpdateMinimizedCaption;
begin
 if Width > 0
 then Caption:= Vcl.FileCtrl.MinimizeName(FFullCaption, Canvas, Width)
 else Caption:= FFullCaption;  // Fallback when control not yet sized
end;


procedure TMinimalPathLabel.SetCaption(Value: string);
begin
 FFullCaption:= Value;
 UpdateMinimizedCaption;
 if FShowFull
 then Hint:= Value;
end;


procedure TMinimalPathLabel.Resize;
begin
 inherited;
 UpdateMinimizedCaption;
end;














procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TMinimalPathLabel]);
end;

end.{============================================================================
