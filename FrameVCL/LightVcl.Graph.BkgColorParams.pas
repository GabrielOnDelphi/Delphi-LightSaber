UNIT LightVcl.Graph.BkgColorParams;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Parameters for background color/fill effects.
   Used by LightVcl.Graph.BkgColor.pas for applying border fading and color fill effects to images.
   The DFM editor for these parameters is in: LightVcl.Graph.BkgColorEditor.pas

   Features:
     - Multiple fill types (solid color, gradient fade)
     - Effect shapes (rectangles, triangles, single color)
     - Color detection modes (border detection, image average, user-specified)
     - Configurable fade speed and edge smearing

   Stream versioning: CurrentVersion=1. Older BioniX v13 format is auto-converted on read.

   Tester:
       c:\MyProjects\Project Testers\gr cGraphBorder.pas tester\TesterFadeBrd.dpr
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics, LightCore.StreamBuff;

TYPE
   { Border positions for selective edge effects }
   TBorderType = (btTop, btBottom, btLeft, btRight);

   { Set of borders to apply effects to }
   TBorderSet  = set of TBorderType;

   { Fill/fade mode for background effect }
   TFillType   = (ftSolid,             { Solid color fill, no fading }
                  ftFade);             { Gradient fade from image edge to border color. Slower but smoother }

   { Shape used when rendering the border effect }
   TEffectShape= (esRectangles,        { Fill with rectangular blocks }
                  esTriangles,         { Fill with triangular patterns }
                  esOneColor);         { Single solid color fill }

   { Method to determine the fill color }
   TEffectColor= (ecAutoDetBorder,     { Auto-detect dominant color from image border pixels }
                  ecImageAverage,      { Calculate average color of entire image }
                  ecUserColor);        { Use Color field specified by user }

   PBkgColorParams= ^RBkgColorParams;
   RBkgColorParams= record
     FillType        : TFillType;
     EffectShape     : TEffectShape;
     EffectColor     : TEffectColor;
     FadeSpeed       : Integer;        { How fast the margins of the image will fade to background color. This is multiplied with 100 so I don't have to show to the user or store to disk decimal values}
     EdgeSmear       : Byte;           { Makes the edge of the image to smear towards the borders of the screen. Default: 0 }
     NeighborWeight  : Integer;        { Factor applied to neighbour pixels when computing the 3-pixel average. Give more intens colors and longer trails (fade) for higher values. This is multiplied with 100 so I don't have to show to the user or store to disk decimal values}
     NeighborDist    : Integer;        { Fuzzyness}
     Tolerance       : Integer;        { Border detection/tolerance }
     Color           : TColor;
     procedure Reset;
   private
     CONST CurrentVersion: Integer= 1;
   public
     procedure ReadFromStream(IOStream: TLightStream);
     procedure WriteToStream (IOStream: TLightStream);
   end;


IMPLEMENTATION


{ Initialize all fields to sensible defaults.
  Call this before using the record or to restore defaults. }
procedure RBkgColorParams.Reset;
begin
  FillType       := ftSolid;
  EffectShape    := esOneColor;
  EffectColor    := ecImageAverage;
  FadeSpeed      := 200;                     { Default: 2.0x multiplier (stored as percentage * 100) }
  EdgeSmear      := 0;                       { No smearing by default }
  NeighborWeight := 100;                     { Default: 1.0x weight (stored as percentage * 100) }
  NeighborDist   := 2;                       { 2 pixel neighbor distance }
  Tolerance      := 8;                       { Border detection tolerance }
  Color          := TColor($218F42);         { Default: Dark green (R=66, G=143, B=33) }
end;


{ Load parameters from stream.
  Handles both current format (Version=1) and legacy BioniX v13 format.
  Raises exception if stream is nil or contains invalid enum values. }
procedure RBkgColorParams.ReadFromStream(IOStream: TLightStream);
VAR
  Version: Integer;
  FillByte, ShapeByte, ColorByte: Byte;
begin
  if IOStream = NIL
  then raise Exception.Create('RBkgColorParams.ReadFromStream: IOStream parameter cannot be nil');

  Version:= IOStream.ReadInteger;

  if Version > CurrentVersion
  then raise Exception.Create('RBkgColorParams.ReadFromStream: Unknown version ' + IntToStr(Version) + '. Expected <= ' + IntToStr(CurrentVersion));

  if Version = CurrentVersion then
   begin
    { Current format (Version 1) }
    Color    := IOStream.ReadInteger;
    FillByte := IOStream.ReadByte;
    ShapeByte:= IOStream.ReadByte;
    ColorByte:= IOStream.ReadByte;

    { Validate enum ranges before casting }
    if Integer(FillByte) > Ord(High(TFillType))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid FillType value: ' + IntToStr(FillByte));
    if Integer(ShapeByte) > Ord(High(TEffectShape))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid EffectShape value: ' + IntToStr(ShapeByte));
    if Integer(ColorByte) > Ord(High(TEffectColor))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid EffectColor value: ' + IntToStr(ColorByte));

    FillType      := TFillType(FillByte);
    EffectShape   := TEffectShape(ShapeByte);
    EffectColor   := TEffectColor(ColorByte);
    EdgeSmear     := IOStream.ReadByte;
    NeighborDist  := IOStream.ReadInteger;
    Tolerance     := IOStream.ReadInteger;
    FadeSpeed     := IOStream.ReadInteger;
    NeighborWeight:= IOStream.ReadInteger;
    IOStream.ReadPadding;
   end
  else
   begin
    { Legacy format: BioniX v13 and earlier (Version < 1 or Version = 0) }
    Color    := IOStream.ReadInteger;
    FillByte := IOStream.ReadByte;
    ShapeByte:= IOStream.ReadByte;
    ColorByte:= IOStream.ReadByte;

    { Validate enum ranges before casting }
    if Integer(FillByte) > Ord(High(TFillType))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid FillType value in legacy format: ' + IntToStr(FillByte));
    if Integer(ShapeByte) > Ord(High(TEffectShape))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid EffectShape value in legacy format: ' + IntToStr(ShapeByte));
    if Integer(ColorByte) > Ord(High(TEffectColor))
    then raise Exception.Create('RBkgColorParams.ReadFromStream: Invalid EffectColor value in legacy format: ' + IntToStr(ColorByte));

    FillType      := TFillType(FillByte);
    EffectShape   := TEffectShape(ShapeByte);
    EffectColor   := TEffectColor(ColorByte);
    EdgeSmear     := IOStream.ReadByte;
    NeighborDist  := Round(IOStream.ReadSingle * 100);  { Convert from float to percentage*100 }
    Tolerance     := IOStream.ReadInteger;
    FadeSpeed     := Round(IOStream.ReadSingle * 100);  { Convert from float to percentage*100 }
    NeighborWeight:= IOStream.ReadInteger;
   end;
end;


{ Save parameters to stream.
  Always writes current version format. Use ReadFromStream to load.
  Raises exception if stream is nil. }
procedure RBkgColorParams.WriteToStream(IOStream: TLightStream);
begin
  if IOStream = NIL
  then raise Exception.Create('RBkgColorParams.WriteToStream: IOStream parameter cannot be nil');

  IOStream.WriteInteger (CurrentVersion);
  IOStream.WriteInteger (Color);
  IOStream.WriteByte    (Ord(FillType));
  IOStream.WriteByte    (Ord(EffectShape));
  IOStream.WriteByte    (Ord(EffectColor));
  IOStream.WriteByte    (EdgeSmear);
  IOStream.WriteInteger (NeighborDist);
  IOStream.WriteInteger (Tolerance);
  IOStream.WriteInteger (FadeSpeed);
  IOStream.WriteInteger (NeighborWeight);
  IOStream.WritePadding;
end;


end.
