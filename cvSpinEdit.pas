UNIT cvSpinEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  TCubicSpinEdit
    Features:
      Checks the value entered by the user. If the value is not in the Min/Max range, then the OnChange is NOT called.
    ToDo:
      If the user deleted the value (not the editor is empty), OnLoseFocus, put the value back in the editor.


  TCubicSpinEditSplit
     Allows us to have a SpinEdit with two lables, one to the left, one to the right.
     Example:  Maximum zoom: 20%
     With this control you don't need to manually create 3 controls: a label, a spinedit and then another label.
     Also, this control will automatically resize to fit the text in it, when the text in the first label is getting longer or the font is getting smaller/bigger/

  Tester
     c:\Myprojects\Project Testers\Cubic VCL SpinEdits\Tester.dpr
=============================================================================================================}

INTERFACE

USES
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Samples.Spin, Vcl.Controls, Vcl.StdCtrls, vcl.Graphics, Vcl.ExtCtrls;


TYPE
 TCubicSpinEdit = class(TSpinEdit)
  private
  public
   procedure Change; override;
 end;

 TCubicSpinEditSplit = class(TPanel)
  private
    FSpin: TCubicSpinEdit;
    FLabelFront: TLabel;
    FLabelEnd: TLabel;
    Initialized: Boolean;     { Unused }
    function  getValue: Integer;
    procedure setValue   (const Value: Integer);
    procedure setCaption1(const Value: string);
    procedure setCaption2(const Value: string);
    function  getCaption1: string;
    function  getCaption2: string;
    procedure SetWidth;
  protected
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CreateWnd; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure SetParent(aParent: TWinControl); override;     { SetParent is called during construction AND also during destruction with aParent=nil }
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Spin: TCubicSpinEdit read FSpin;
    property Caption1: string  read getCaption1 write setCaption1;
    property Caption2: string  read getCaption2 write setCaption2;
    property Value   : Integer read getValue    write setValue;
 end;


procedure Register;


IMPLEMENTATION
USES cGraphUtil;




constructor TCubicSpinEditSplit.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);// Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 //Width := 134;
 Height:= 22;

 AutoSize    := FALSE;
 BevelOuter  := bvNone;
 ShowCaption := FALSE;
 ParentFont  := TRUE;
 ParentColor := TRUE;
 ParentBackground:= TRUE; // Mandatory

 FLabelFront:= TLabel.Create(Self);
 FLabelFront.Name:= 'LabelFront';
 FLabelFront.Parent := Self; // Here I can set the parent
 FLabelFront.Caption:= 'Front label';  {  Cannot be moved to CreateWnd because if it is there, (when skins are loading) we will override the text set at design time }
 FLabelFront.Layout := tlCenter;
 FLabelFront.Margins.Left:= 0;
 FLabelFront.Margins.Right:= 6;
 FLabelFront.AlignWithMargins := TRUE;
 FLabelFront.Align  := alLeft;
 FLabelFront.SetSubComponent(TRUE);

 FSpin:= TCubicSpinEdit.Create(Self);
 FSpin.Name:= 'Spin';
 FSpin.SetSubComponent(TRUE);
 FSpin.Parent:= Self;
 FSpin.Width:= 50;
 FSpin.AlignWithMargins := TRUE;
 FSpin.Margins.Top:= 0;
 FSpin.Margins.Bottom:= 0;
 FSpin.MinValue:= 0;            { Cannot be moved to CreateWnd because if it is there, (when skins are loading) we will override the value set at design time }
 FSpin.MaxValue:= 10;
 FSpin.Align:= alLeft;
 FSpin.SetSubComponent(TRUE);

 FLabelEnd:= TLabel.Create(Self);
 FLabelEnd.Name:= 'LabelEnd';
 FLabelEnd.Parent := Self;
 FLabelEnd.Caption:= '%';
 FLabelEnd.Layout := tlCenter;
 FLabelEnd.AlignWithMargins := TRUE;
 FLabelEnd.Margins.Top:= 0;
 FLabelEnd.Margins.Bottom:= 0;
 FLabelEnd.Align:= alLeft;
 FLabelEnd.SetSubComponent(TRUE);
end;





procedure TCubicSpinEditSplit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;

  if VclStylesEnabled
  then ParentBackground:= FALSE; { Without this, the canvas is corrupted when the control is places in a TCubicGroupBox and skins are active. The problem might be with TCubicGroupBox }
 // else ParentBackground:= TRUE;

  AlignWithMargins:= TRUE;
end;


procedure TCubicSpinEditSplit.CreateWnd;
begin
 inherited CreateWnd;
 SetWidth;

 if NOT Initialized then
  begin
   Initialized:= TRUE;
   {
   Caption:= 'Don''t set here!';
   ShowCaption := FALSE; // https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040
   ParentColor:= FALSE;
   ParentBackground:= FALSE;
   BevelOuter:= bvNone;
   DoubleBuffered:= FALSE;
   //Height:= SpinEdit.Height;

   //AutoSize:= TRUE;  }
  end;
end;




function TCubicSpinEditSplit.getValue: Integer;
begin
 Result:= FSpin.Value;
end;

procedure TCubicSpinEditSplit.setValue(const Value: Integer);
begin
 FSpin.Value:= Value;
end;



procedure TCubicSpinEditSplit.setCaption1(const Value: string);
begin
 FLabelFront.Caption:= Value;
 SetWidth;
end;

procedure TCubicSpinEditSplit.setCaption2(const Value: string);
begin
 FLabelEnd.Caption:= Value;
 SetWidth;
end;


function TCubicSpinEditSplit.getCaption1: string;
begin
 Result:= FLabelFront.Caption;
end;

function TCubicSpinEditSplit.getCaption2: string;
begin
 Result:= FLabelEnd.Caption;
end;



procedure TCubicSpinEditSplit.SetWidth;
begin
 VAR NewWidth:= FLabelEnd.Left+ FLabelEnd.Width+ 2;
 if Width <> NewWidth then
  begin
   Width:= NewWidth;
   Canvas.Font:= Font; //?

   { Make sure the controls are shown in the correct order }
   FLabelFront.Left:= 0;
   FSpin.Left      := FLabelFront.Left+ FLabelFront.Width+ 1;
   FLabelEnd.Left  := FSpin.Left+ FSpin.Width+ 1;
  end;
end;


// Recalculate width when the font was changed!
procedure TCubicSpinEditSplit.CMFontChanged(var Message: TMessage);
begin
  SetWidth;
end;


procedure TCubicSpinEditSplit.SetParent(aParent: TWinControl);  { SetParent is called during construction AND also during destruction with aParent=nil }
begin
  inherited;
  if aParent <> NIL
  then SetWidth;
end;





{------------------------------------------------------------------------------------------------------------}
procedure TCubicSpinEdit.Change;
begin
 {
 DEL - This is not really what I want, because the user won't be able to enter ANY value. He should be able to do that - for example enter 3 digits and later remove the last digit.
 If the value entered by the user is not in the Min/Max range, it is forced to the correct (min/max) value.

 if (Value > MaxValue) then Value:= MaxValue;
 if (Value < MinValue) then Value:= MinValue; }

 if (Value > MaxValue) OR (Value < MinValue) then
  begin
    Color:= clRed;
    EXIT;
  end;

 Color:= clWindow;
 inherited;
end;







procedure Register;
begin
 RegisterComponents('LightSaber', [TCubicSpinEdit]);
 RegisterComponents('LightSaber', [TCubicSpinEditSplit]);
end;


end.

