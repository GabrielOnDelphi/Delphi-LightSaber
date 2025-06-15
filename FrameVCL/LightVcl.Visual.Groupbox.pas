unit LightVcl.Visual.GroupBox;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
  A groupbox that has a bold caption

  Tester: c:\Myprojects\Project Testers\cubic VCL controls tester\
=============================================================================================================}

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.StdCtrls, Vcl.Themes, VCL.Controls,
  System.Types, System.UITypes, Vcl.Graphics;

Type
  TCubicGroupBox = class(TGroupBox)
    private
      FBoldCaption: Boolean;
    protected
      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
    published
      property BoldCaption: Boolean read FBoldCaption write FBoldCaption default true;
  end;

procedure Register;


IMPLEMENTATION


constructor TCubicGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoldCaption:= TRUE;
end;


{$WARN SYMBOL_DEPRECATED OFF}
{ Copied from procedure TCustomGroupBox.Paint }
procedure TCubicGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  TextFormat: TTextFormat;
  CaptionRect, OuterRect: TRect; Size: TSize;
  Box: TThemedButton;
  Details: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  with Canvas do
   begin
    Font := Self.Font;
    if FBoldCaption
    then Font.Style := [fsBold];

    // Styles enabled
    if ThemeControl(Self)
    then
     begin
      LStyle := StyleServices(Self);
      if Text <> ''
      then
        begin
         GetTextExtentPoint32(Handle, Text, Length(Text), Size);
         CaptionRect := Rect(0, 0, Size.cx, Size.cy);
         if not UseRightToLeftAlignment
         then OffsetRect(CaptionRect, 8, 0)
         else OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
        end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(Handle, Left, Top, Right, Bottom);

      if Enabled
      then Box := tbGroupBoxNormal
      else Box := tbGroupBoxDisabled;
      Details := LStyle.GetElementDetails(Box);
      LStyle.DrawElement(Handle, Details, OuterRect);

      SelectClipRgn(Handle, 0);
      Brush.Style := bsClear;
      if Text <> ''
      then
        if IsRightToLeft
        then
         begin
          Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
          TextFormat:= [tfSingleLine];    //new
          LStyle.DrawText(Handle, Details, Text, CaptionRect, Flags, 0);
         end
        else
          LStyle.DrawText(Handle, Details, Text, CaptionRect, [tfLeft]);
      end
    else

     // Styles disabled
     begin
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D
      then
       begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
       end
      else
        Brush.Color := clWindowFrame;

      FrameRect(R);
      if Text <> '' then
       begin
        if not UseRightToLeftAlignment
        then R := Rect(8, 0, 0, H)
        else R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);

        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        DrawText(Handle, Text, Length(Text), R, Flags OR DT_CALCRECT);
        Brush.Color := Color;
        DrawText(Handle, Text, Length(Text), R, Flags);
       end;
     end;
   end;
end;
{$WARN SYMBOL_DEPRECATED ON}

procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicGroupBox]);
end;

end.
