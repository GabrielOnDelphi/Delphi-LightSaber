UNIT cvLabelEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  Features:
     Added OnPressEnter event - Event is triggered when the user pressed Enter
     If CheckFileExistence=true then the control gets red if the text entered is not an existent file

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
USES LightCom.Colors;





procedure TCubicLabelEdit.Change;
begin
 if CheckFileExistence
 then
   if FileExists(Text)
   then Valid:= vaValid
   else Valid:= vaInvalid
 else
   Valid:= vaNone;

 inherited;  // if you want to run standard handler
end;



procedure TCubicLabelEdit.KeyPress(VAR Key: Char);
begin
 inherited;
 if (Ord(Key) = VK_RETURN)
 AND Assigned(FPressEnter)
 then FPressEnter(Self);
end;





procedure TCubicLabelEdit.setValid(const Value: TValidity);
begin
 FValid:= Value;
 case FValid of
   vaNone: Color:= clWindow;
   vaValid: Color:= clGreenFade;
   vaInvalid: Color:= clRedFade;
 end;
end;












procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicLabelEdit]);
end;



end.============================================================================



 //if csCreating in ControlState then exit;
