UNIT ccColors;

{=============================================================================================================
   Gabriel Moraru
   2024.06
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------}

INTERFACE

{$I Frameworks.inc}

USES System.UiTypes;

{ COLORS (in BGR format) }
CONST
   clBlueBkg     = TColor($15100F);           { To be used as backgound when I display images }
   clBlueAlmost  = Tcolor($F03030);
   clBlueBleo    = Tcolor($FEF800);
   clBlueNight   = Tcolor($3F0000);
   clBlueDark    = TColor($770000);
   clBlueGreen   = TColor($CCCC00);
   clBlueLight   = Tcolor($F6B0B0);
   clBlueLt      = TColor($FFCC99);
   clBlueNaval   = TColor($CC9933);
   clBlueSky     = TColor($F0CAA6);
   clBluePale    = TColor($FFFFCC);
   clBlueSlab    = TColor($B3B67E);           { Faded blue }
   clBlueSea     = TColor($B90F0B);
   clBlueSeaGreen= TColor($CCFF00);           { Bleo but a bit more green in it }
   clPlum        = TColor($B16778);           { Plum blue }
   clPlumLt      = TColor($BD85C7);

   clLimeDark    = TColor($00CF00);
   clGreenOk     = TColor($BBEEBB);           { To be used as background for controls to singnal that data (a path in a teditbox) is ok }
   clGreenDark   = Tcolor($005F00);
   clGreenFade   = TColor($79FF91);
   clKhaki       = TColor($669999);
   clMustard     = TColor($00DDDD);           { Dark yellow }
   clGreenWashed = TColor($CCFFCC);           { A very very fade green }
   clOliveGreen  = TColor($009966);

   clOrange      = TColor($0078FF);
   clOrangeDark  = TColor($0058DF);
   clOrangeDk    = TColor($0099CC);
   clOrangeGray  = TColor($80A8F0);
   clOrangeLt    = TColor($99CCFF);

   clPink        = TColor($F255FF);           { A bit lighter than clPurpleLight }
   clPinkLight   = TColor($F793FF);
   clPurpleDk    = TColor($5E005E);
   clPurpleLight = TColor($AA22CC);           { A bit lighter than Purple }
   clPurpleFaded = TColor($CA62FC);           { This is almost pink }
   clPurpleWashed= TColor($cAa2eC);           { This is almost pink }
   clPumpkin     = TColor($0099FF);
   clButterfly   = TColor($EF10B8);           { Purple with some extra red in it }

   clRedBrick    = TColor($003399);
   clRedBaron    = TColor($0033FF);
   clRedBright   = Tcolor($4040FF);           { A bit brighter than the classic clRed }
   clRedDark     = Tcolor($00003F);
   clRedFade     = Tcolor($D0D0FF);           { Frez }
   clRose        = TColor($5E24F4);           { Intense }
   clRoseLt      = TColor($9966FF);           { Frez frez }
   clBrown       = TColor($6058A0);
   clBrownLt     = TColor($688FB0);
   clBurntSienna = TColor($000088);           { Almost coagulated blood }

   clTealDk      = TColor($999933);
   clViolet      = TColor($FF33FF);
   clVioletDk    = TColor($993399);
   clVioletLt    = TColor($FFCCFF);
   clCyanLt      = TColor($FFFF99);

   clYellowGreen = TColor($00FFCC);
   clYellowLight = TColor($99ffff);
   clYellowPale  = TColor($CCFFFF);
   clHoney       = TColor($1CAEE6);           { Dark yellow with some red in it }

   clCream       = TColor($F0FBFF);
   clPeach       = TColor($647EF9);
   clSilverDark  = TColor($a0a0a0);
   clSilverLight = TColor($F3F2F2);           { Very light silver }
   clGrayMedium  = TColor($A4A0A0);

   {$IFDEF FRAMEWORK_VCL}
   {$ELSE FRAMEWORK_FMX}
   clBlack         = TAlphaColors.Black;
   clGray          = TAlphaColors.Gray;
   clRed           = TAlphaColors.Red;
   {$Endif}

IMPLEMENTATION

end.

