UNIT LightVcl.Graph.ResizeParamFrame;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

   GUI frame where the user can edit the parameters of the resizing algorithm (LightVcl.Graph.Resize.pas).
   Works on a RResizeParams record (LightVcl.Graph.ResizeParams.pas).

   !!! Don't add dependencies to LightVisControls.dpk because that package is compiled after this one !!!

   TESTER:
      c:\Projects\LightSaber ImageResampler Test\ResamplerTester.dpr
      c:\MyProjects\Projects GRAPH Resamplers\Tester for LightVcl.Graph.ResizeParam\


   How to make a quick program that resizes an image:

      procedure btnResizeClick(Sender: TObject);
      VAR
         Params: RResizeParams;
      begin
       ResizeParamsFrame.ObjectFromGUI(@Params);
       Params.MaxWidth := Monitor.Width;
       Params.MaxHeight:= Monitor.Height;

       VAR BMP:= LoadAndStretch(FileName, Params);
       if BMP <> NIL
       then Bmp2Jpg(BMP, LightCore.IO.IncrementFileName(FileName), 95);
      end;
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.NumberBox, Vcl.Samples.Spin,
  LightVcl.Graph.ResizeParams;

TYPE
  TResizeParameters = class(TFrame)
    numForceHeight : TNumberBox;
    numForceWidth  : TNumberBox;
    radForceHeight : TRadioButton;
    radForceWidth  : TRadioButton;
    radStretch     : TRadioButton;
    radZoomAuto    : TRadioButton;
    radZoomCustom  : TRadioButton;
    radZoomFill    : TRadioButton;
    radZoomFit     : TRadioButton;
    radZoomNone    : TRadioButton;
    spnZoomCustom  : TNumberBox;
    spnZoomMax     : TSpinEdit;
    procedure GUIChanged (Sender: TObject);
  public
    procedure GUIFromObject(ResizeParams: PResizeParams);  { Set the GUI according to the values stored in the ResizeParams record }
    procedure ObjectFromGUI(ResizeParams: PResizeParams);  { Load values from the GUI into the ResizeParams record }
  published
    chkZoomMax: TCheckBox;
    //Also we can add published properties in order to use them in the Object Inspector later.
  end;


procedure Register;

IMPLEMENTATION {$R *.dfm}





{ Reads values from GUI controls into the ResizeParams record.
  Note: If no radio button is checked (unexpected state), ResizeOpp defaults to roAutoDetect. }
procedure TResizeParameters.ObjectFromGUI(ResizeParams: PResizeParams);
begin
  Assert(ResizeParams <> NIL, 'ObjectFromGUI: ResizeParams cannot be nil');

  { Determine resize operation from radio buttons }
  if radZoomNone.Checked         then ResizeParams.ResizeOpp:= roNone
  else if radZoomAuto.Checked    then ResizeParams.ResizeOpp:= roAutoDetect
  else if radZoomFit.Checked     then ResizeParams.ResizeOpp:= roFit
  else if radZoomFill.Checked    then ResizeParams.ResizeOpp:= roFill
  else if radZoomCustom.Checked  then ResizeParams.ResizeOpp:= roCustom
  else if radForceWidth.Checked  then ResizeParams.ResizeOpp:= roForceWidth
  else if radForceHeight.Checked then ResizeParams.ResizeOpp:= roForceHeight
  else if radStretch.Checked     then ResizeParams.ResizeOpp:= roStretch
  else ResizeParams.ResizeOpp:= roAutoDetect;  { Default fallback - should never happen in normal use }

  ResizeParams.MaxZoomUse  := chkZoomMax.Checked;
  ResizeParams.MaxZoomVal  := spnZoomMax.Value;
  ResizeParams.ForcedWidth := numForceWidth.ValueInt;
  ResizeParams.ForcedHeight:= numForceHeight.ValueInt;
  ResizeParams.CustomZoom  := spnZoomCustom.Value;
end;


{ Sets GUI controls according to the values stored in the ResizeParams record.
  Warning: Double-buffering on the parent form can cause visual glitches with skins active. }
procedure TResizeParameters.GuiFromObject(ResizeParams: PResizeParams);
begin
  Assert(ResizeParams <> NIL, 'GUIFromObject: ResizeParams cannot be nil');

  { Warning about double-buffering - only check if parented }
  Assert((Parent = NIL) OR NOT Parent.DoubleBuffered,
    'TResizeParameters frame may have visual issues when its parent is double-buffered and skins are active');

  case ResizeParams.ResizeOpp of
    roNone       : radZoomNone.Checked   := TRUE;
    roAutoDetect : radZoomAuto.Checked   := TRUE;
    roFit        : radZoomFit.Checked    := TRUE;
    roFill       : radZoomFill.Checked   := TRUE;
    roCustom     : radZoomCustom.Checked := TRUE;
    roForceWidth : radForceWidth.Checked := TRUE;
    roForceHeight: radForceHeight.Checked:= TRUE;
    roStretch    : radStretch.Checked    := TRUE;
  end;

  chkZoomMax.Checked     := ResizeParams.MaxZoomUse;
  spnZoomMax.Value       := ResizeParams.MaxZoomVal;
  numForceWidth.ValueInt := ResizeParams.ForcedWidth;
  numForceHeight.ValueInt:= ResizeParams.ForcedHeight;
  spnZoomCustom.Value    := ResizeParams.CustomZoom;
end;








procedure TResizeParameters.GUIChanged(Sender: TObject);
begin
 chkZoomMax.Enabled:= radZoomAuto.Checked;
 spnZoomMax.Enabled:= radZoomAuto.Checked;
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TResizeParameters]); //register the frame in the desired component category
end;


end.
