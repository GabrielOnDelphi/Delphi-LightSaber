UNIT LightVcl.Visual.FloatSpinEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  FLOAT SPIN EDIT - v1.8
  Allows user to enter a float number and increase/decrease its value (via spin buttons) or manually edit it.

  !!!!!!!!!!!!!!!!!!!!!!!!
  Replaced in Delphi 10.4.2 by TNumberBox: See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_VCL_TNumberBox_Control
  !!!!!!!!!!!!!!!!!!!!!!!!

  Features:
      MinValue   - same as TSpinEdit
      MaxValue   - same as TSpinEdit
      Value      - same as TSpinEdit
      Increment  - same as TSpinEdit
      Decimals   - number fo decimals to show in control
      Real2Str   - converts real number to 'nice' string

  This project:
    No dependencies
    No other external files required
    Works with all Delphi editions

  Pas file
    Code & comments
    506 Lines (12.3KB)

  DCU file
    18KB
    + icon file

  //ToDo: Add another up/down spin ctrl in from of the control. This will increment ONLY the integer part, while the "normal" spinedit ctrl will control the decimal part.

  Tester
     c:\Myprojects\Project Testers\Cubic VCL SpinEdits\Tester.dpr
=============================================================================================================}

INTERFACE

USES
 System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, WinApi.Messages, WinApi.Windows;

TYPE
 TMyEdit = class(TEdit)
  protected
   procedure KeyPress(var Key: Char); override;
 end;


 TFloatSpinEdit = class(TWinControl)
  private
   FValue    : Real;                                            // 8 byte real
   FMinValue : Real;
   FMaxValue : Real;
   FIncrement: Real;
   FDecimals : Integer;
   procedure setValue(aValue: Real);
   function  getHint: string;
   procedure setHint(aValue: string);
   function  getShowHint: boolean;
   procedure setShowHint(aValue: boolean);
   function  checkMinMax(NewValue: Real): Real;
   function  getFilteredText: string;
   procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
   UpDown: TUpDown;
   EditBox: TMyEdit;
   FOnChange: TNotifyEvent;
   procedure UpdateEditorValue;
   procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure ControlEnter(Sender: TObject);
   function  GetEnabled: boolean;                        override;
   procedure SetEnabled(aValue: boolean);                override;
   procedure WMSetFocus(VAR Message: TWMSetFocus); message WM_SetFocus;
   procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
   procedure EditChanged(Sender: TObject);

  public
   constructor Create(aOwner: TComponent);                  override;
   function ValueAsString(HowManyDecimals: Byte): string;   overload;
   function ValueAsString: string;                          overload;

  published
   property MinValue  : Real         read FMinValue   write FMinValue;
   property MaxValue  : Real         read FMaxValue   write FMaxValue;
   property Increment : Real         read FIncrement  write FIncrement;
   property Decimals  : Integer      read FDecimals   write FDecimals default 1;
   property ShowHint  : Boolean      read GetShowHint write SetShowHint;
   property Hint      : string       read GetHint     write SetHint;
   property Enabled   : Boolean      read GetEnabled  write SetEnabled;
   property OnChange  : TNotifyEvent read FOnChange   write FOnChange;
   property Value     : Real         read FValue      write SetValue;   { This must be streamed after all the other properies (like FDecimals) have been set }
   property Anchors;
   property Align;
   property Color;
   property Constraints;
   property Ctl3D;
   property DragCursor;
   property DragMode;
   property Font;
   property ParentColor;
   property ParentCtl3D;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property TabOrder;
   property TabStop;
   property Visible;
   property OnClick;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnStartDrag;
  end;

procedure Register;


function Real2Str(CONST ExtValue: Extended; Decimals: Byte = 1): string;


IMPLEMENTATION
{$R LightVcl.Visual.FloatSpinEdit.dcr}

USES
   System.Math;





{-----------------------------------
   CREATE/DESTROY
-----------------------------------}

constructor TFloatSpinEdit.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);

 FIncrement          := 0.1;
 FMinValue           := -10;                          { Type real is the same as double }
 FMaxValue           := +10;
 FValue              := 0.00;
 FDecimals           := 2;                            { Put a big number otherwise there will be an odd (but correct) behavior when the user type a number with more decimals then FDecimals }

 Width               := 75;
 Height              := 21;
 OnEnter             := ControlEnter;

 EditBox             := TMyEdit.Create(Self);         { Do not destroy it manually. let the owner do it }
 //EditBox.SetSubComponent(True);
 EditBox.Parent      := Self;                         { Here we can set the parent  }
 EditBox.Text        := '0.0';
 //EditBox.Alignment   := taRightJustify;
 EditBox.Align       := alClient;
 EditBox.AutoSize    := FALSE;
 EditBox.OnChange    := EditChanged;
 EditBox.OnKeyDown   := EditKeyDown;
 EditBox.OnEnter     := ControlEnter;

 UpDown              := TUpDown.Create(Self);         { Do not destroy it manually. let the owner do it }
 //UpDown.SetSubComponent(True);
 UpDown.Parent       := Self;                         { Here we can set the parent  }
 UpDown.Orientation  := udVertical;
 UpDown.Align        := alRight;
 UpDown.Height       := Height;
 UpDown.Min          := -MaxInt;
 UpDown.Max          := +MaxInt;
 UpDown.Position     := 0;                            { We need to make sure the position never reaches the UpDown.MinValue or UpDown.MaxValue }
 UpDown.OnChangingEx := UpDownChangingEx;
end;







{-----------------------------------
  CONTROL STATE
-----------------------------------}

procedure TFloatSpinEdit.WMSetFocus(VAR Message: TWMSetFocus);
begin
 inherited;
 SetFocus;
end;



procedure TFloatSpinEdit.ControlEnter(Sender: TObject);
begin
 if NOT (csDesignInteractive in ControlStyle) then
  begin
   EditBox.SelStart := 0;
   EditBox.SelLength:= Length(EditBox.Text);
  end;
end;








{-----------------------------------
   VALUE CHANGED
-----------------------------------}

procedure TFloatSpinEdit.UpdateEditorValue;
VAR SelStart: Integer;
begin
 EditBox.OnChange:= NIL;
 SelStart        := EditBox.SelStart;
 EditBox.Text    := Real2Str(FValue, FDecimals);  { Set value in the editor }
 EditBox.SelStart:= SelStart;                     { Restore selection }
 EditBox.OnChange:= EditChanged;                  { Restore event. Triggered when the user edited the text value }

 if NOT (csLoading in ComponentState) then
   if Assigned(FOnChange)
   then FOnChange(Self);
end;


procedure TFloatSpinEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
  VK_UP:
   begin
    if (ssCtrl in Shift)
    OR (ssShift in Shift)
    then Value := Value + FIncrement * 10     { Fast increment }
    else Value := Value + FIncrement;         { Normal increment }
   end;

  VK_DOWN:
   begin
    if (ssCtrl in Shift)
    OR (ssShift in Shift)
    then Value := Value - FIncrement * 10     { Fast decrement }
    else Value := Value - FIncrement;         { Normal decrement }
   end;

  VK_Next:
    Value := Value + FIncrement * 10;         { Pg Up = Fast increment }

  VK_PRIOR:
    Value := Value - FIncrement * 10;         { Pg Down = Fast decrement }

  VK_END:
   begin
    Value := MinValue;
    Key   := 0;
   end;

  VK_HOME:
   begin
    Value := MaxValue;
    Key   := 0;
   end;
 end;
end;



function CharIsValidDecimal(C: Char): Boolean;
begin
 Result:= (CharInSet(c, ['+', '-', FormatSettings.DecimalSeparator, '0'..'9', #0..#31] ));
 (*Other possible checks:
    OR NOT ((c = FormatSettings.DecimalSeparator) AND (Pos(FormatSettings.DecimalSeparator, text) > 0))     { Dot already exists? }
    OR NOT (((c = '+') OR (c = '-')) AND ((Pos('+',text)=1) OR (Pos('-',text)=1)));                         { Check if  + or - is first }
 *)
end;


procedure TMyEdit.KeyPress(var Key: Char);
begin
 if CharIsValidDecimal(Key)
 then inherited KeyPress(Key)                                                   { Accept key }
 else
  begin
   Key := #0;                                                                   { Do Nothing }
   MessageBeep(MB_ICONEXCLAMATION);
  end;
end;


{ Triggered when the control loses focus }
procedure TFloatSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if checkMinMax (Value) <> Value
  then SetValue (Value);
end;


{ Returns the text enterd by user, after all invalid characters have been removed }
function TFloatSpinEdit.getFilteredText: string;
VAR
  i: Integer;
begin
 Result:= '';
 for i := 1 to Length(EditBox.Text) DO
   if CharIsValidDecimal(EditBox.Text[i])
   OR ((EditBox.Text[i] = '-') AND (i = 1))  // Keep the minus char (-)
   then Result := Result + EditBox.Text[i];
end;


{ The user edited the text value }
procedure TFloatSpinEdit.EditChanged(Sender: TObject);
VAR
 r: Real;
 sOut: string;
begin
 sOut := getFilteredText;

 if  (sOut <> '')
 AND (sOut <> '-') then
  begin
   r := StrToFloatDef(EditBox.Text, 0);

   { Did the user entered a new value? }
   if r <> FValue then
     begin
      FValue := r;
      UpdateEditorValue;
     end;
  end;
end;



function TFloatSpinEdit.checkMinMax(NewValue: Real): Real;
begin
 if NewValue < FMinValue
 then
  begin
   //Vcl.Dialogs.ShowMessage('Value must be greater than '+ RealToStr(FMinValue));
   Result := FMinValue;
  end
 else
   if NewValue > FMaxValue
   then Result:= FMaxValue
   else Result:= NewValue;
end;


procedure TFloatSpinEdit.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);  { Increase/decrease value }
begin
 case Direction of
  updUp:
    if Value+ Increment <= FMaxValue         { Increment number in EditBox }
    then Value:= Value + Increment
    else Value:= fMaxValue;
  updDown:
    if Value- Increment >= FMinValue         { Decrement number in EditBox }
    then Value := Value - Increment
    else Value := fMinValue;
  updNone: ;//DoNothing
 end;
end;









{-----------------------------------
   SETTERS/GETTERS
-----------------------------------}

procedure TFloatSpinEdit.SetValue(aValue: Real);
begin
 if FValue = checkMinMax(aValue) then EXIT;
 FValue:= checkMinMax(aValue);
 UpdateEditorValue;
end;


function TFloatSpinEdit.ValueAsString(HowManyDecimals: Byte): string;
begin
 Result:= Real2Str(Value, HowManyDecimals);
end;


function TFloatSpinEdit.ValueAsString: string;
begin
 Result:= Real2Str(Value, FDecimals);
end;





function TFloatSpinEdit.GetEnabled: boolean;
begin
 Result := EditBox.Enabled;
end;


procedure TFloatSpinEdit.SetEnabled(aValue: boolean);
begin
 EditBox.Enabled:= aValue;
 UpDown.Enabled := aValue;
end;





function TFloatSpinEdit.GetHint: string;
begin
 Result := EditBox.Hint;
end;


procedure TFloatSpinEdit.SetHint(aValue: string);
begin
 EditBox.Hint:= aValue;
 UpDown.Hint := aValue;
end;





function TFloatSpinEdit.GetShowHint: boolean;
begin
 Result := EditBox.ShowHint;
end;



procedure TFloatSpinEdit.SetShowHint(aValue: boolean);
begin
 EditBox.ShowHint:= aValue;
 UpDown.ShowHint := aValue;
end;










{-----------------------------------
   UTILS
-----------------------------------}
{ This will hide the decimals after coma if they are '0'.
  Example: 3.0 returns 3
  Also see
      System.SysUtils.FloatToStrF
      http://www.delphibasics.co.uk/rtl.asp?name=floattostrf

  PS: Nice documentation about floating no:
      http://rvelthuis.de/articles/articles-floats.html   }
function Real2Str(CONST ExtValue: Extended; Decimals: Byte = 1): string;
VAR ComaPos, i: Integer;
begin
 if Decimals = 0                                                                { The user don't want to show any decimals }
 then Result:= IntToStr(Round(ExtValue))
 else
  begin
   Assert(NOT System.Math.IsNaN(ExtValue), 'Float is NAN!');
   Result:= FloatToStrF(ExtValue, ffFixed, 16, Decimals);

   ComaPos:= Pos(FormatSettings.DecimalSeparator, Result);
   Assert(ComaPos > 1, 'Decimal separator not found!');

   Result:= system.COPY(Result, 1, ComaPos+ Decimals);

   { Cut 0s from the end }
   ComaPos:= Length(Result);
   for i:= ComaPos downto 1 DO
    if Result[i] <> '0' then
     begin
      ComaPos:= i;
      Break;
     end;

   if Result[ComaPos]= FormatSettings.DecimalSeparator
   then Dec(ComaPos);

   Result:= System.COPY(Result, 1, ComaPos);
  end;
end;




procedure Register;
begin
 RegisterComponents('LightSaber VCL', [TFloatSpinEdit]);
end;

end.
