UNIT FormDemo;

{=============================================================================================================
   2025.10
   www.GabrielMoraru.com
==============================================================================================================

  Demo 1:
    Draws shadow under text using DrawShadowText API from ComCtl32.dll.
    If the API is not available the shadow is drawn manually.

  Demo 2:
    Shortens a text so it can fix in the specified canvas.
    Example: 'This is a long text that will be truncated' could be shortened to something like "This is a...runcated'.

=============================================================================================================}


INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.ComCtrls, LightCore, Vcl.FileCtrl, LightVcl.Common.Dialogs, Vcl.ExtCtrls,  Vcl.WinXCtrls,  vcl.Graphics,
  LightVcl.Visual.AppDataForm, LightVcl.Visual.SpinEdit, LightVcl.Visual.Panel, LightVcl.Visual.FileListBox, LightVcl.Visual.RichLog, LightVcl.Visual.AppData,
  GR32_ColorPicker;

TYPE
 TfrmDemoShadow = class(TLightForm)
    btnAPI           : TButton;
    btnOutline       : TButton;
    btnShadow3D      : TButton;
    btnShadowBox     : TButton;
    btnShadowBoxRect : TButton;
    btnShadowSimple  : TButton;
    btnTest1         : TButton;
    btnTest2         : TButton;
    btnXor           : TButton;
    CubicPanel1      : TCubicPanel;
    CubicPanel2      : TCubicPanel;
    FileList         : TCubicFileList;
    imgShadow        : TImage;
    Label2           : TLabel;
    lblCoord         : TLabel;
    lblResizeWarn    : TLabel;
    PageControl1     : TPageControl;
    Panel1           : TPanel;
    spnBlur          : TSpinEdit;
    spnShadowOpac    : TCubicSpinEdit;
    StatusBar1       : TStatusBar;
    StatusBar2       : TStatusBar;
    StatusBar3       : TStatusBar;
    tabOthers        : TTabSheet;
    tabShadow        : TTabSheet;
    tglTopBtm        : TToggleSwitch;
    procedure FormDestroy             (Sender: TObject);
    procedure btnShadowBoxClick       (Sender: TObject);
    procedure btnShadowSimpleClick    (Sender: TObject);
    procedure btnShadow3DClick        (Sender: TObject);
    procedure btnOutlineClick         (Sender: TObject);
    procedure btnXorClick             (Sender: TObject);
    procedure FileListDblClick        (Sender: TObject);
    procedure imgShadowMouseDown      (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormResize              (Sender: TObject);
    procedure btnAPIClick(Sender: TObject);
    procedure ColorPickerChanged(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
    procedure btnShadowBoxRectClick(Sender: TObject);
    procedure ParametersChanged(Sender: TObject);
    procedure btnTest1Click(Sender: TObject);
    procedure btnTest3Click(Sender: TObject);
  private
    BMP: TBitmap;
    LastAlgorithm: TNotifyEvent;
    ShadowX, ShadowY: Integer;
    function GetSelectedColor: TColor;
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;

VAR
   frmDemoShadow: TfrmDemoShadow;

IMPLEMENTATION  {$R *.dfm}

USES
   LightVcl.Graph.Text,
   LightVcl.Common.IO, LightVcl.Graph.Loader,
   LightVcl.Graph.ShadowText,
   LightVcl.Common.EllipsisText;

VAR OrigBitmap: TBitmap= NIL;


{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TfrmDemoShadow.FormPostInitialize;
begin
 inherited FormPostInitialize;

 Assert(imgShadow.Stretch     = FALSE, 'Stretch must be false otherwise the XY are relative to the Timage control and not also to the contained bitmap!');
 Assert(imgShadow.Proportional= FALSE, 'Proportional must be false otherwise the XY are relative to the Timage control and not also to the contained bitmap!');

 BMP:= TBitmap.Create;
 BMP.PixelFormat:= pf24bit;
 BMP.Canvas.Font.Size:= 10;
 BMP.Canvas.Font.Color:= clWhite;
 BMP.Canvas.Font.Style:= [fsBold];

 FileList.Directory:= Appdata.AppFolder+ '..\..\Resources\';
 ShadowX:= 30;
 ShadowY:= 50;
 //ColorPicker.SelectedColor:= Color32(clGray);

 if FileList.Count > 0
 then
  begin
    FileList.SelectFirstItem;
    FileListDblClick(Self);
    //btnShadowBoxRectClick(Self);
  end
 else
  MessageError('No images found in '+ Appdata.AppFolder+ 'Test images\');
end;


procedure TfrmDemoShadow.FormDestroy(Sender: TObject);
begin
 FreeAndNil(BMP);
 FreeAndNil(OrigBitmap);
end;






{--------------------------------------------------------------------------------------------------
   GUI
--------------------------------------------------------------------------------------------------}
procedure TfrmDemoShadow.imgShadowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 ShadowX:= X;
 ShadowY:= Y;
 lblCoord.Caption:= 'X: '+ i2s(ShadowX)+ ' / Y: '+ i2s(ShadowY);

 if Assigned(LastAlgorithm)
 then LastAlgorithm(Self)
 else MessageWarning('Choose an algorithm first!');
 lblCoord.Transparent:= TRUE;
end;


procedure TfrmDemoShadow.FileListDblClick(Sender: TObject);
begin
 FreeAndNil(OrigBitmap);
 if FileExistsMsg(FileList.FileName) then
  begin
   OrigBitmap:= LoadGraph(FileList.FileName, TRUE);
   ///LightVcl.Graph.Resize.StretchProportMax(OrigBitmap, 0, imgShadow.Width, imgShadow.Height);
   OrigBitmap.PixelFormat:= pf24bit;
   imgShadow.Picture.Assign(OrigBitmap);
  end;

 lblResizeWarn.Visible:= FALSE;
end;


procedure TfrmDemoShadow.FormResize(Sender: TObject);
begin
 lblResizeWarn.Visible:= TRUE;
end;


procedure TfrmDemoShadow.ParametersChanged(Sender: TObject);
begin
 if NOT AppData.Initializing
 then LastAlgorithm(Self);
end;



{--------------------------------------------------------------------------------------------------
   TEXT SHADOW
--------------------------------------------------------------------------------------------------}
procedure TfrmDemoShadow.btnShadowBoxClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 LightVcl.Graph.Text.DrawTextShadowBox(BMP.Canvas, 'ShadowBox demo text!', ShadowX, ShadowY, GetSelectedColor, spnShadowOpac.Value);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnShadowBoxClick;
end;



procedure TfrmDemoShadow.btnShadowBoxRectClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);
 BMP.PixelFormat:= pf24bit;
 OrigBitmap.PixelFormat:= pf24bit;

 LightVcl.Graph.Text.DrawTextShadowBox(BMP, 'ShadowBox demo text!', tglTopBtm.State= tssOn, GetSelectedColor, spnShadowOpac.Value, spnBlur.Value);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnShadowBoxRectClick;
end;



procedure TfrmDemoShadow.btnShadowSimpleClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 LightVcl.Graph.Text.DrawTextShadow3DSoft(BMP.Canvas, 'DrawShadow3DSoft demo text!', ShadowX, ShadowY, GetSelectedColor);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnShadowSimpleClick;
end;



procedure TfrmDemoShadow.btnShadow3DClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 LightVcl.Graph.Text.DrawTextShadow3DHard(BMP.Canvas, 'DrawShadow3DHard demo text', ShadowX, ShadowY, GetSelectedColor);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnShadow3DClick;
end;



procedure TfrmDemoShadow.btnOutlineClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 LightVcl.Graph.Text.DrawTextOutline(BMP.Canvas, 'DrawTextOutline / solid middle true' , ShadowX, ShadowY, TRUE);
 LightVcl.Graph.Text.DrawTextOutline(BMP.Canvas, 'DrawTextOutline / solid middle false', ShadowX, ShadowY+25, FALSE);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnOutlineClick;
end;



procedure TfrmDemoShadow.btnAPIClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 DrawShadowText(BMP.Canvas, 'API ShadowText', ShadowX, ShadowY, clWhite, GetSelectedColor, 2);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnAPIClick;
end;



procedure TfrmDemoShadow.btnTest3Click(Sender: TObject);
begin
 {BMP.Assign(OrigBitmap);

 s:= 'API ShadowText Low';
 R := Rect(ShadowX, ShadowY, ShadowX + BMP.Canvas.TextWidth(s), ShadowY + BMP.Canvas.TextHeight(s));
 ShadowText.DrawShadowTextLow(BMP.Canvas, R, s, clWhite, GetSelectedColor, 1, 1, DT_LEFT or DT_END_ELLIPSIS or DT_MODIFYSTRING);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= Button1Click;  }
end;


procedure TfrmDemoShadow.btnXorClick(Sender: TObject);
begin
 BMP.Assign(OrigBitmap);

 LightVcl.Graph.Text.DrawTextXOR(BMP.Canvas.Handle, Font, 'XOR shadow demo text!', ShadowX, ShadowY);
 imgShadow.Picture.Assign(BMP);
 LastAlgorithm:= btnXorClick;
end;





{ Color picker }
procedure TfrmDemoShadow.ColorPickerChanged(Sender: TObject);
begin
 if NOT AppData.Initializing
 then LastAlgorithm(Self);
end;

function TfrmDemoShadow.GetSelectedColor: TColor;
begin
 Result:= clred; // Gr32.Wincolor(ColorPicker.SelectedColor);
end;








CONST
   LongText= 'c:\Projects\LightSaber\LightVcl.Graph.Loader.Resolution.pas';

procedure TfrmDemoShadow.btnTest1Click(Sender: TObject);
begin
  StatusBar1.SimpleText:= LightVcl.Common.EllipsisText.GetEllipsisText(LongText, StatusBar1.Canvas, StatusBar1.Width);
end;


procedure TfrmDemoShadow.btnTest2Click(Sender: TObject);
begin
  LightVcl.Common.EllipsisText.DrawStringEllipsis(
           LongText,
           StatusBar3.Canvas,
           StatusBar3.ClientRect);
end;

end.


