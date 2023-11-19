UNIT cGraphResizeParamFrame;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   GUI where the user can edit the paramaters of the resizing algorithm (cGraphResize.pas).
   Works on a RResizeParams record (cGraphResizeParams.pas)

   !!! Don't add dependencies to LightVisControls.dpk because that package is compiled after this one !!!

   TESTER:
      c:\MyProjects\Projects GRAPH Resamplers\GLOBAL Tester\
      c:\MyProjects\Projects GRAPH Resamplers\Tester for cGraphResizeParam\


   How to make a quick program that resizes an image:

      procedure btnResizeClick(Sender: TObject);
      VAR
         Params: RResizeParams;
      begin
       Params:= ResizeParams.ObjectFromGUI;
       Params.MaxWidth := Monitor.Width;
       Params.MaxHeight:= Monitor.Height;

       VAR BMP:= LoadAndStretch(FileName, Params);
       if BMP <> NIL
       then Bmp2Jpg(BMP, ccIO.IncrementFileName(FileName), 95);
      end;
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.NumberBox, Vcl.Samples.Spin,
  cGraphResizeParams;

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
    procedure ObjectFromGUI(ResizeParams: PResizeParams);  { Load values from the GUI into the ResizeParams recod }
  published
    chkZoomMax: TCheckBox;
    //Also we can add published properties in order to use them in the Object Inspector later.
  end;


procedure Register;

IMPLEMENTATION {$R *.dfm}





procedure TResizeParameters.ObjectFromGUI(ResizeParams: PResizeParams);
begin
 Assert(ResizeParams<>NIL);
 if radZoomNone.Checked    then ResizeParams.ResizeOpp:= roNone else
  if radZoomAuto.Checked    then ResizeParams.ResizeOpp:= roAutoDetect else
   if radZoomFit.Checked     then ResizeParams.ResizeOpp:= roFit else
    if radZoomFill.Checked    then ResizeParams.ResizeOpp:= roFill else
     if radZoomCustom.Checked  then ResizeParams.ResizeOpp:= roCustom else
      if radForceWidth.Checked  then ResizeParams.ResizeOpp:= roForceWidth else
       if radForceHeight.Checked then ResizeParams.ResizeOpp:= roForceHeight else
        if radStretch.Checked     then ResizeParams.ResizeOpp:= roStretch;

 ResizeParams.MaxZoomUse  := chkZoomMax.Checked;
 ResizeParams.MaxZoomVal  := spnZoomMax.Value;
 ResizeParams.ForcedWidth := numForceWidth.ValueInt;
 ResizeParams.ForcedHeight:= numForceHeight.ValueInt;
 ResizeParams.CustomZoom  := spnZoomCustom.Value;
end;


procedure TResizeParameters.GuiFromObject(ResizeParams: PResizeParams);
begin
 Assert(Parent.DoubleBuffered = FALSE, 'TResizeParameters frame does not like when its parent is double buffered and skins are active!');
 Assert(ResizeParams<>NIL);

 case ResizeParams.ResizeOpp of
  roNone       : radZoomNone.Checked   := true;
  roAutoDetect : radZoomAuto.Checked   := true;
  roFit        : radZoomFit.Checked    := true;
  roFill       : radZoomFill.Checked   := true;
  roCustom     : radZoomCustom.Checked := true;
  roForceWidth : radForceWidth.Checked := true;
  roForceHeight: radForceHeight.Checked:= true;
  roStretch    : radStretch.Checked    := true;
 end;

 chkZoomMax.Checked     := ResizeParams.MaxZoomUse;   { 0 means that we don't want to use the MaxZoom feature }
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
  RegisterComponents('LightSaber', [TResizeParameters]); //register the frame in the desired component category
end;


end.
