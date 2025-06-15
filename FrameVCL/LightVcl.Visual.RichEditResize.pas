UNIT LightVcl.Visual.RichEditResize;

{=============================================================================================================
   Gabriel Moraru
   2024.09
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  This component resizes itself as we add lines to it (instead of showing the vert scrollbars)

  Tester:
    C:\Projects\LightSaber\Demo\Tester All Visual Controls

  Also see:
    LightVcl.Visual.RichEdit.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.UITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Forms, LightVcl.Common.AppDataForm,Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs;


TYPE
  TRichEditResize = class(TRichEdit)
  private
    FMinHeight: Integer;
    procedure ResizeRichEdit;
  protected
    procedure CreateWnd; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinHeight: Integer read FMinHeight write FMinHeight;
  end;

procedure Register;



IMPLEMENTATION


constructor TRichEditResize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinHeight:= 50;
end;


procedure TRichEditResize.CreateWnd;
begin
  inherited CreateWnd;
  ScrollBars := ssNone;  // Now that the handle is created, we can safely set the ScrollBars property
end;


procedure TRichEditResize.Change;
begin
  inherited;
  ResizeRichEdit;
end;


procedure TRichEditResize.ResizeRichEdit;
var
  LineHeight, NewHeight: Integer;
  DC: HDC;
  TextMetric: TTextMetric;
  LineCount: Integer;
begin
  if (csDesigning in ComponentState) then EXIT;
  if Parent = NIL then
  begin
    ShowMessage('TRichEditResize - Parent not assigned'); // Temporary. Take it out later
    EXIT;
  end;

  // Calc single line height
  DC := GetDC(Handle);
  try
    GetTextMetrics(DC, TextMetric);
    LineHeight := TextMetric.tmHeight- TextMetric.tmInternalLeading;
  finally
    ReleaseDC(Handle, DC);
  end;

  // Calculate number of lines
  LineCount := Perform(EM_LINEFROMCHAR, WPARAM(MaxInt), 0) + 1;

  // Calculate heigh
  NewHeight := LineHeight * LineCount;
  if NewHeight < FMinHeight
  then NewHeight:= FMinHeight;

  // Prepare the parent
  if (Parent is TPanel)
  AND (TPanel(Parent).AutoSize)
  then ShowMessage('TRichEditResize - Parent panel cannot be set to autosize!');

  NewHeight:= Top+ NewHeight + 5; // Add padding to the panel
  Parent.Height := NewHeight;

  // Self height
  Height := NewHeight;
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TRichEditResize]);
end;



end.
