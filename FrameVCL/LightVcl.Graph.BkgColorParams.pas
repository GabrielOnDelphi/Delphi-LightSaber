UNIT LightVcl.Graph.BkgColorParams;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Used by LightVcl.Graph.BkgColor.pas
  The DFM editor for these parameters is in: LightVcl.Graph.BkgColorEditor.pas

  Tester:
       c:\MyProjects\Project Testers\gr cGraphBorder.pas tester\TesterFadeBrd.dpr
--------------------------------------------------------------------------------------------------}

INTERFACE

USES
   System.SysUtils, Vcl.Graphics, LightCore.StreamBuff;

TYPE
   TBorderType = (btTop, btBottom, btLeft, btRight);

   TBorderSet  = set of TBorderType;

   TFillType   = (ftSolid,             { Don't fade. Use color }
                  ftFade);             { Fade borders to color. Super slow }

   TEffectShape= (esRectangles, esTriangles, esOneColor);

   TEffectColor= (ecAutoDetBorder,     { Detect border color }
                  ecImageAverage,      { Use average image color }
                  ecUserColor);        { Use user-provided color }

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


procedure RBkgColorParams.Reset;
begin
  FillType       := ftSolid;
  EffectShape    := esOneColor;
  EffectColor    := ecImageAverage;
  FadeSpeed      := 200;
  EdgeSmear      := 0;
  NeighborWeight := 100;
  NeighborDist   := 2;
  Tolerance      := 8;                       { Border detection }
  Color          := TColor($218F42);         { Albastru+gri+negru }
end;


procedure RBkgColorParams.ReadFromStream(IOStream: TLightStream);
begin
  VAR Version:= IOStream.ReadInteger;
  if Version = CurrentVersion then
   begin
    { Current }
    Color         :=              IOStream.ReadInteger;
    FillType      := TFillType   (IOStream.ReadByte);
    EffectShape   := TEffectShape(IOStream.ReadByte);
    EffectColor   := TEffectColor(IOStream.ReadByte);
    EdgeSmear     :=              IOStream.ReadByte;
    NeighborDist  :=              IOStream.ReadInteger;
    Tolerance     :=              IOStream.ReadInteger;
    FadeSpeed     :=              IOStream.ReadInteger;
    NeighborWeight:=              IOStream.ReadInteger;
    IOStream.ReadPadding;
   end
  else
   begin
    { Up to BioniX v13 inclusive }
    Color         :=              IOStream.ReadInteger;
    FillType      := TFillType   (IOStream.ReadByte);
    EffectShape   := TEffectShape(IOStream.ReadByte);
    EffectColor   := TEffectColor(IOStream.ReadByte);
    EdgeSmear     :=              IOStream.ReadByte;
    NeighborDist  :=              Round(IOStream.ReadSingle * 100);
    Tolerance     :=              IOStream.ReadInteger;
    FadeSpeed     :=              Round(IOStream.ReadSingle * 100);
    NeighborWeight:=              IOStream.ReadInteger;
   end
end;


procedure RBkgColorParams.WriteToStream(IOStream: TLightStream);
begin
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
