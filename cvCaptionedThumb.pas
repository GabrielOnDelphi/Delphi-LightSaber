UNIT cvCaptionedThumb;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   A panel that shows the thumbnail of an image and draws a caption over it

   Tester:
      c:\Myprojects\Project Testers\gr TCationedThumbnail\TCationedThumbnail_Tester.dpr
=============================================================================================================}

{
Thumbnails:

Mouse over thumbnail
  Mouse over in same color as pushed buttons is confusing, take different color/effect.
  Decided: Mouse over should be different from currently selected and from inactive (i.e. default)
  Implemented: Now we use clActiveCaption instead of clHighlight.



Active selected thumbnail
  After selecting/loading a scene I would like to have this scene remain highlighted that I can see which scene is selected (Mats, Thomas)
  The thumbnail should to some visual feedback when the user clicks it.
  Decided: Colored frame around thumbnail from predefined, theme-specific color palette.
  Thomas->Mats: (slightly) lighter and larger.
}



INTERFACE
USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.ExtCtrls, VCL.Themes;

TYPE
  TCationedThumbnail = class(TPanel)
  private
    ThumbStretched: TBitmap;
    FThumbBMP: TBitmap;
    Highlight: Boolean;
    FCaption: string;
    FSelected: Boolean;
    Procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure setCaption(const Value: string);
    procedure setSelected(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    ShadowBox: Boolean;           // True to draw a fuzzy regrangle/box arround the shadow. False to show a "vista" like shadow under text (no box)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ThumbBMP: TBitmap read FThumbBMP;
    property Caption: string read FCaption write setCaption;
    property Selected: Boolean read FSelected write setSelected;
    function GetClientHeight: Integer;
    function GetClientWidth : Integer;
  end;

procedure Register;
function CreateThumbnail(Owner, Parent: TWinControl; FileName: string): TCationedThumbnail;


IMPLEMENTATION

{.$DEFINE HOERTECH}

USES
   ShadowText, cGraphText, cGraphUtil, {$IFDEF HOERTECH}Common,{$else}ccIO{$ENDIF};





function CreateThumbnail(Owner, Parent: TWinControl; FileName: string): TCationedThumbnail;
begin
  Result:= TCationedThumbnail.Create(Owner);
  Result.Parent:= Parent;
  Result.Width:= 141;
  Result.AlignWithMargins:= True;
  Result.Align:= alLeft;
  Result.ThumbBMP.LoadFromFile(FileName); //ToDo: for speed reasons, we might want to load the image in a temp bitmap then resize it to have the EXACT size as the panel (minus borders)
  Result.Caption:= ExtractOnlyName(FileName);
end;










{-------------------------------------------------------------------------------------------------------------
   TCationedThumbnail
-------------------------------------------------------------------------------------------------------------}
constructor TCationedThumbnail.Create(AOwner: TComponent);
begin
  inherited;
  Highlight:= FALSE;
  Width := 141;
  AlignWithMargins:= True;
  ShadowBox:= False;
  ThumbStretched:= TBitmap.Create;
  ThumbStretched.SetSize(Width, Height);

  FThumbBMP:= TBitmap.Create;
  FThumbBMP.PixelFormat:= pf24bit;
  FThumbBMP.Canvas.Font.Size:= 10;
  FThumbBMP.Canvas.Font.Color:= clWhite;
  FThumbBMP.Canvas.Font.Style:= [fsBold];
end;


destructor TCationedThumbnail.Destroy;
begin
  FreeAndNil(ThumbStretched);
  FreeAndNil(FThumbBMP);
  inherited;
end;







CONST
   BorderSizeV= 8;  // Vertical   (top/btm)    shadow
   BorderSizeH= 6;  // Horizontal (left/right) shadow
   Effect= 1;       // How much we enlarge the image on mouse over


function TCationedThumbnail.GetClientWidth: Integer;  { Returns the size of the thumbnail. The user should resize his thumbnail to fit this size }
begin
 Result:= Width - BorderSizeV*2;
end;

function TCationedThumbnail.GetClientHeight: Integer; { Returns the size of the thumbnail. The user should resize his thumbnail to fit this size }
begin
 Result:= Height - BorderSizeH*2;
end;





procedure TCationedThumbnail.Paint;
VAR
   R: TRect;
   CurColor: TColor;
begin
  //inherited; // We don't let the control paint itself. We do all the painting!

  { Decide which color }
  R := Rect(0, 0, Width, Height);
  if Selected
  then CurColor:= TStyleManager.ActiveStyle.GetSystemColor(clHotLight)
  else
    { Color for mouse highlight. We don't define the color. Instead we use the color of the theme/style. }
    if Highlight
    then CurColor:= TStyleManager.ActiveStyle.GetSystemColor(clHighlight) // clHighlight clActiveCaption
    else CurColor:= TStyleManager.ActiveStyle.GetSystemColor(clBackground);

  if Highlight
  then CurColor:= LightenColor(CurColor, 80);

  { Fill with bkg color }
  Canvas.Brush.Color:= CurColor;
  Canvas.FillRect(R);

  { Paint the thumbnail }
  if Highlight
  then R := Rect(BorderSizeV-Effect, BorderSizeH-Effect, Width-BorderSizeV+Effect, Height-BorderSizeH+Effect)
  else R := Rect(BorderSizeV, BorderSizeH, Width-BorderSizeV, Height-BorderSizeH);

  {
  //ToDo: we need a smoother resizing. See JanFX or use the thumbnails of the same size as the panel so no resize is needed
  //Idea: we could pre-scale the images in an external program, and keep that second image in memory

  if Highlight
  then
   begin
    VCL.GraphUtil.ScaleImage(ThumbBMP, ThumbStretched, 1.2);
    Canvas.StretchDraw(R, ThumbStretched);
   end
  else
   Canvas.Draw(BorderSizeV, BorderSizeH, ThumbBMP);  }

   Canvas.Draw(BorderSizeV, BorderSizeH, ThumbBMP);
end;















{ MOUSE }
procedure TCationedThumbnail.Click;
VAR
   i: Integer;
   Control: TControl;
begin
  inherited;
  Selected:= TRUE;
  Paint;

  { Deselect all other thumbnails }
  for i:= 0 to Parent.ControlCount-1 DO      { Don't confuse the Controls property with the Components property. The Controls property lists all the controls that are child windows of the control, while the Components property lists all components that it owns. }
   begin
     Control:= Parent.Controls[i];
     if (Control is TCationedThumbnail)
     AND (Control <> Self)
     then TCationedThumbnail(Control).Selected:= FALSE;
   end;
end;

procedure TCationedThumbnail.CMMouseEnter(var msg: TMessage);
begin
  Cursor:= crHandPoint;
  Highlight:= TRUE;
  Invalidate;
end;

procedure TCationedThumbnail.CMMouseLeave(var msg: TMessage);
begin
  Cursor:= crDefault;
  Highlight:= FALSE;
  Invalidate;
end;






procedure TCationedThumbnail.setCaption(const Value: string);
VAR
   iLeft: Integer;
   TextHeight: Integer;
begin
  if Value = '' then EXIT; //Vcl.dialogs.ShowMessage('TCationedThumbnail. Caption is empty!');

  FCaption := Value;
  if ShadowBox
  then
    cGraphText.DrawTextShadowBox(ThumbBMP, FCaption, FALSE, clDkGray, 10, 1)
  else
    begin
     iLeft:= ThumbBMP.Canvas.TextWidth(FCaption);
     iLeft:= (ThumbBMP.Width - iLeft) DIV 2;                    //current size: 164x116
     TextHeight:= ThumbBMP.Canvas.TextHeight(FCaption);
     ShadowText.DrawShadowText (ThumbBMP.Canvas, FCaption, iLeft, ThumbBMP.Height-TextHeight-BorderSizeV, clWhite, clDkGray, 2);
    end;
end;


procedure TCationedThumbnail.setSelected(const Value: Boolean);
begin
  FSelected := Value;
  Paint;
end;


procedure Register;
begin
  // This will register the control into the IDE (Tool Palette) so we canuse it at Design time.
  // But of course, the component must be installed first.
  // This is be a bit pointless anyway, because we need to create x tbumnails (x= unknown at design time) dynamically.
  RegisterComponents('LightSaber', [TCationedThumbnail]);
end;





end.
