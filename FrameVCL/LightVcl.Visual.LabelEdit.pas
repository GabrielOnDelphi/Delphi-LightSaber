UNIT LightVcl.Visual.LabelEdit;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Features:
     OnPressEnter event - triggered when the user presses Enter
     CheckFileExistence - when true, shows green for existing files, red for non-existing files
     Valid property - allows manual control of validity state (vaNone/vaValid/vaInvalid)

  Note: Unlike TCubicEdit, empty text is treated as invalid when CheckFileExistence is enabled.
        This component provides positive feedback (green) for valid files, not just red for invalid.

  See also: TCubicEdit (LightVcl.Visual.Edit.pas) for a simpler TEdit-based version.

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, Winapi.Windows, System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls;
  {$WARN GARBAGE OFF}   {Silent the: 'W1011 Text after final END' warning }

TYPE
  TValidity= (vaNone, vaValid, vaInvalid);        { Normal / Green / Red color }

  TCubicLabelEdit = class(TCustomLabeledEdit)
   private
     FCheckFileEx: Boolean;
     FPressEnter: TNotifyEvent;
     FValid: TValidity;
    procedure setValid(const Value: TValidity);
   protected
     procedure Change; override;
     procedure KeyPress (VAR Key: Char); override;
   public
     constructor Create(AOwner: TComponent); override;
   published
     property OnPressEnter: TNotifyEvent  read FPressEnter  write FPressEnter;
     property CheckFileExistence: Boolean read FCheckFileEx write FCheckFileEx default FALSE;
     property Valid: TValidity            read FValid       write setValid     default vaNone;

     property Alignment;
     property Anchors;
     property AutoSelect;
     property AutoSize;
     property BevelEdges;
     property BevelInner;
     property BevelKind;
     property BevelOuter;
     property BiDiMode;
     property BorderStyle;
     property CharCase;
     property Color;
     property Constraints;
     property Ctl3D;
     property DoubleBuffered;
     property DragCursor;
     property DragKind;
     property DragMode;
     property EditLabel;
     property Enabled;
     property Font;
     property HideSelection;
     property ImeMode;
     property ImeName;
     property LabelPosition;
     property LabelSpacing;
     property MaxLength;
     property OEMConvert;
     property NumbersOnly;
     property ParentBiDiMode;
     property ParentColor;
     property ParentCtl3D;
     property ParentDoubleBuffered;
     property ParentFont;
     property ParentShowHint;
     property PasswordChar;
     property PopupMenu;
     property ReadOnly;
     property ShowHint;
     property TabOrder;
     property TabStop;
     property Text;
     property TextHint;
     property Touch;
     property Visible;
     property StyleElements;
     property OnChange;
     property OnClick;
     property OnContextPopup;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnEndDock;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnGesture;
     property OnKeyDown;
     property OnKeyPress;
     property OnKeyUp;
     property OnMouseActivate;
     property OnMouseDown;
     property OnMouseEnter;
     property OnMouseLeave;
     property OnMouseMove;
     property OnMouseUp;
     property OnStartDock;
     property OnStartDrag;
  end;



procedure Register;

IMPLEMENTATION
USES LightVcl.Common.Colors;



constructor TCubicLabelEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FCheckFileEx:= FALSE;
 FValid:= vaNone;
end;



{ Updates validity state based on file existence check.
  Note: Empty text is treated as invalid when CheckFileExistence is enabled. }
procedure TCubicLabelEdit.Change;
begin
 if CheckFileExistence
 then
   if FileExists(Text)
   then Valid:= vaValid
   else Valid:= vaInvalid
 else
   Valid:= vaNone;

 inherited;  { Run standard OnChange handler }
end;



{ Intercepts key presses to detect Enter key and fire OnPressEnter event }
procedure TCubicLabelEdit.KeyPress(VAR Key: Char);
begin
 inherited;
 if (Ord(Key) = VK_RETURN)
 AND Assigned(FPressEnter)
 then FPressEnter(Self);
end;





procedure TCubicLabelEdit.setValid(const Value: TValidity);
begin
 if FValid = Value then Exit;
 FValid:= Value;
 case FValid of
   vaNone   : Color:= clWindow;
   vaValid  : Color:= clGreenFade;
   vaInvalid: Color:= clRedFade;
 end;
end;












procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicLabelEdit]);
end;



end.============================================================================



 //if csCreating in ControlState then exit;
