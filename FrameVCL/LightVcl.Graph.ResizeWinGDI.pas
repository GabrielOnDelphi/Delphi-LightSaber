UNIT LightVcl.Graph.ResizeWinGDI;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Tester: c:\MyProjects\Projects GRAPHICS Resamplers\GLOBAL Tester\
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
  Winapi.Windows,
  System.SysUtils, System.Classes, Vcl.Graphics, System.Types,
  LightCore;

procedure ResizeBitmapGDI(Source, Dest: TBitmap; OutWidth, OutHeight: integer);


IMPLEMENTATION

uses
  GDIPAPI,
  GDIPOBJ;

procedure ResizeBitmapGDI(Source, Dest: TBitmap; OutWidth, OutHeight: integer);
var
  src, dst: TGPBitmap;
  g: TGPGraphics;
  h: HBITMAP;
begin
  src := TGPBitmap.Create(Source.Handle, 0);
  try
    dst := TGPBitmap.Create(OutWidth, OutHeight);
    try
      g := TGPGraphics.Create(dst);
      try
        g.SetInterpolationMode(InterpolationModeHighQuality);
        g.SetPixelOffsetMode(PixelOffsetModeHighQuality);
        g.SetSmoothingMode(SmoothingModeHighQuality);
        g.DrawImage(src, 0, 0, dst.GetWidth, dst.GetHeight);
      finally
        g.Free;
      end;
      dst.GetHBITMAP(0, h);
      Dest.Handle := h;
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;


end.



