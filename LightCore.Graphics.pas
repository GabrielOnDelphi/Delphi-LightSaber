UNIT LightCore.Graphics;

{=============================================================================================================
   2026.04.25
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  RTL-only graphics utilities. No VCL/FMX dependencies — safe to use from any platform layer.

  Detects the format of a graphics file by inspecting its magic-byte signature. Falls back to
  file-extension matching for formats without a reliable magic-byte signature (J2K, WB1, RainDrop).

  Two callable forms:
    DetectGraphFormatBySig — returns the canonical file extension as a string (with leading dot).
                             Superset: also detects PDF, TIFF, WEBP for general MIME-style sniffing.
    DetectGraphSignature   — legacy integer API used by LoadGraph's case statement.
                             1=BMP, 2=PNG, 3=GIF, 4=JPG, 5=JP2, 6=WB1, 7=RainDrop, 0=unknown/error.
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes,
   LightCore.IO, LightCore.AppData;



function DetectGraphFormatBySig(CONST FileName: string): string;        { Returns canonical extension ('.jpg', '.png', '.gif', '.bmp', '.pdf', '.tif', '.webp', '.jp2', '.wb1', '.RainDrop') or '' on unknown/error. }
function DetectGraphSignature  (CONST FileName: string): Integer;       { Legacy integer code. 0=unknown (incl. PDF/TIFF/WEBP not in legacy range), 1=BMP, 2=PNG, 3=GIF, 4=JPG, 5=JP2, 6=WB1, 7=RainDrop. }


IMPLEMENTATION


function DetectGraphFormatBySig(CONST FileName: string): string;
VAR
  FS  : TFileStream;
  Buf : array[0..11] of Byte;
  Read: Integer;
begin
  Result:= '';
  if NOT FileExists(FileName) then EXIT;

  { Extension-based formats first — no reliable magic-byte signature. }
  if IsJp2(FileName) then EXIT('.jp2');
  if IsWB1(FileName) then EXIT('.wb1');
  if isRainDrop(FileName) then EXIT(RainDropExt);

  { Magic-byte detection for the rest. }
  Read:= 0;
  TRY
    FS:= TFileStream.Create(FileName, fmOpenRead OR fmShareDenyNone);
    TRY
      Read:= FS.Read(Buf, SizeOf(Buf));
    FINALLY
      FreeAndNil(FS);
    END;
  EXCEPT
    on E: Exception do
      begin
        AppDataCore.LogError(E.ClassName + ': ' + E.Message + ' - ' + FileName);
        EXIT;
      end;
  END;

  if Read < 4 then EXIT;

  // BMP: 42 4D ('BM')
  if (Buf[0] = $42) AND (Buf[1] = $4D)                                              then EXIT('.bmp');
  // PNG: 89 50 4E 47
  if (Buf[0] = $89) AND (Buf[1] = $50) AND (Buf[2] = $4E) AND (Buf[3] = $47)        then EXIT('.png');
  // GIF: 47 49 46 38 ('GIF8')
  if (Buf[0] = $47) AND (Buf[1] = $49) AND (Buf[2] = $46) AND (Buf[3] = $38)        then EXIT('.gif');
  // JPEG: FF D8 FF
  if (Buf[0] = $FF) AND (Buf[1] = $D8) AND (Buf[2] = $FF)                           then EXIT('.jpg');
  // PDF: 25 50 44 46  ('%PDF')
  if (Buf[0] = $25) AND (Buf[1] = $50) AND (Buf[2] = $44) AND (Buf[3] = $46)        then EXIT('.pdf');
  // TIFF: 49 49 2A 00 (little-endian) or 4D 4D 00 2A (big-endian)
  if ((Buf[0] = $49) AND (Buf[1] = $49) AND (Buf[2] = $2A) AND (Buf[3] = $00))
  OR ((Buf[0] = $4D) AND (Buf[1] = $4D) AND (Buf[2] = $00) AND (Buf[3] = $2A))      then EXIT('.tif');
  // WEBP: 'RIFF' .... 'WEBP'
  if (Read >= 12)
  AND (Buf[0] = $52) AND (Buf[1] = $49) AND (Buf[2] = $46) AND (Buf[3] = $46)
  AND (Buf[8] = $57) AND (Buf[9] = $45) AND (Buf[10]= $42) AND (Buf[11]= $50)       then EXIT('.webp');
end;


function DetectGraphSignature(CONST FileName: string): Integer;
VAR Ext: string;
begin
  Ext:= DetectGraphFormatBySig(FileName);
  if Ext = ''           then EXIT(0);
  if Ext = '.bmp'       then EXIT(1);
  if Ext = '.png'       then EXIT(2);
  if Ext = '.gif'       then EXIT(3);
  if Ext = '.jpg'       then EXIT(4);
  if Ext = '.jp2'       then EXIT(5);
  if Ext = '.wb1'       then EXIT(6);
  if Ext = RainDropExt  then EXIT(7);
  Result:= 0;   { PDF/TIFF/WEBP recognized but not in legacy 1..7 codespace — caller falls back to WIC. }
end;


end.
