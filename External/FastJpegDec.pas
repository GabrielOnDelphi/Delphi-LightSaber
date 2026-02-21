UNIT FastJpegDec;

{-------------------------------------------------------------------------------------------------------------
   JPEG fast decoder for Delphi, using SSE/SSE2
   synopse.info/forum/viewtopic.php?pid=31079

   Pros:
     Damn Fast
     Can be used multithreaded apps (see examples in '\Demo' folder )

   Cons:
     Is not able to decode some of jpeg files; this issue comes from original jpegdec code, not our Delphi conversion; in such cases, the original libjpeg library must be used instead.
     Win32 only (x86 assembly with SSE/SSE2). No Win64 or pure Pascal fallback exists.

   Hint:
     Direct access to the picture bitmap without creating any TBitmap resource is allowed via the TJpegDecode.DrawTo() methods: so you can use very big pictures, without any resource limitations (under Win 2K or XP, allocating big TBitmap instances raises errors)

-------------------------------------------------------------------------------------------------------------  
   Updated: 2026.02.21
   Gabriel Moraru
   Tester: c:\projects\Project Testers\gr LoadGraph\
-------------------------------------------------------------------------------------------------------------
   
   Source
     www.marktg.com/jpegdec

   Arnaud Bouchez notes:
     Most of the hack was to include the SSE/SSE2 assembly code into a true Delphi unit. Conversion was made difficult because the Delphi compiler doesn't allow to align code or data at 16 bytes boundaries, which is required by the SSE/SSE2 operations. This is a well known limitation of the Delphi compiler. See @http://qc.embarcadero.com/wc/qcmain.aspx?d=1116  A solution was found by copying the whole used tables into memory-allocated buffer, and by creating the TBL_64 table by code.
     Since we use Win32 VirtualAlloc API for memory allocation (which is always 16 bytes aligned and set to zero, as expected by the code), the TJpegDecode object instance has a not-common creator, as the JpegDecode() function: don't try allocate any TJpegDecode object on the stack or via Delphi heap
     There is no TPicture descendent implementation yet, since it should be more usefull to use a resulting TBitmap in your code
     Tested under Delphi 7, 2009, (XE7)

   Mark Griffiths: it probably won't work with interlaced images or anything that uses uncommon features of the JPG format. I haven't got any plans for improvements to it at this stage.

--------------------------------------------------------------------------------------------------------------
  LICENSE BLOCK
    Copyright (C) 2004 Dr. Manhattan    Initial SSE and SSE2 assembly code - http://sourceforge.net/projects/jpegdec
    Copyright (C) 2010 Arnaud Bouchez   Synopse Informatique - http://synopse.info
    Copyright (C) 2017 Mark Griffiths   http://www.marktg.com/jpegdec

    Released under MPL/GPL/LGPL tri-license; version 1.18
    Version: MPL 1.1/GPL 2.0/LGPL 2.1

    The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL
    Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights and limitations under the License.
    The Original Code is jpegdec. The Initial Developer of the Original Code is Dr. Manhattan.  Portions created by the Initial Developer are Copyright (C) 2004 the Initial Developer. All Rights Reserved.
    Contributor(s):
      March 2010: Arnaud Bouchez for Delphi integration http://synopse.info
      2017: Mark Griffiths made the code fully thread safe. http://www.marktg.com/jpegdec
    Alternatively, the contents of this file may be used under the terms of either the GNU General Public License Version 2 or later (the "GPL"), or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which case the provisions of the GPL or the LGPL are applicable instead of those above. If you wish to allow use of your version of this file only under the terms of either the GPL or the LGPL, and not to allow others to use your version of this file under the terms of the MPL, indicate your decision by deleting the provisions above and replace them with the notice and other provisions required by the GPL or the LGPL. If you do not delete the provisions above, a recipient may use your version of this file under the terms of any one of the MPL, the GPL or the LGPL.
-------------------------------------------------------------------------------------------------------------}


INTERFACE

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

USES
  Winapi.Windows, system.SysUtils, VCL.Graphics;

TYPE
  /// error codes returned by JpegDecode()
  TJpegDecodeError = (JPEG_EOF, JPEG_OUTOFMEM, JPEG_SUCCESS, JPEG_CPUNOTSUPPORTED, JPEG_BADFILE, JPEG_FORMATNOTSUPPORTED);

{$A-}
  /// the memory header of a decoded image, as returned by JpegDecode() must be allocated by JpegDecode() function, not on stack nor heap
  // - code example to draw a picture on a canvas (taken from JpegDraw procedure):
  // var Img: PJpegDecode;
  // begin
  //   if JpegDecode(Buffer,BufferLen,Img)=JPEG_SUCCESS then
  //   try
  //     Img.DrawTo(Canvas,X,Y);
  //   finally
  //     Img.Free;
  //   end;
  // end;


  { If you get E1025 "Unsupported language feature Object":
      Go to "Project Objects > Delphi Compiler > Output - C/C++ > C/C++ Output file generation",
      change the value to "Generate DCUs Only" instead of the default value "Generate all C++Builder files (including package libs)"
      https://synopse.info/forum/viewtopic.php?pid=31281#p31281 }
  TJpegDecode = object //class(TObject)
    width: integer;
    height: integer;
    scanlength: cardinal;          /// picture scan length
    pRGB: PAnsiChar;               /// pointer to RGB data
    bitsPixel: cardinal;           /// picture  bits per pixel
    ComponentsCount: cardinal;     /// picture number of components
    procedure ToBMI(var BMI: TBitmapInfo);
    /// convert the resulting image to a Win32 bitmap context
    // - return nil on error
    // - you can then call TJpegDecode.Free to release the decoded memory
    function ToBitmap: TBitmap;
    /// direct draw of the picture to a canvas
    // - use Win32 fast API for fast drawing, without any TBitmap conversion
    // and/or resource allocation (usefull for very big pictures)
    procedure DrawTo(Canvas: TCanvas; X,Y: integer); overload;
    /// direct stretch draw of the picture to a canvas
    // - use Win32 fast API for fast drawing, without any TBitmap conversion
    // and/or resource allocation (usefull for very big pictures)
    procedure DrawTo(Canvas: TCanvas; const Dest: TRect); overload;
    /// direct stretch draw of a source rectangle of the picture to a canvas
    // - use Win32 fast API for fast drawing, without any TBitmap conversion
    // and/or resource allocation (usefull for very big pictures)
    procedure DrawTo(Canvas: TCanvas; const Source, Dest: TRect); overload;

    /// release used bitmap memory and the corresponding PJpegDecode instance
    // - use this method, and not any freemem() nor stack-allocated instance
    function Free: boolean;
  end;

{$A+}
  PJpegDecode = ^TJpegDecode;    /// reference pointer to a jpeg picture content, as returned by JpegDecode()

/// decode a .JPEG buffer into a bitmap array
// - written entirely in assembly with SSE and SSE2 optimizations. Uses floating point internally for maximum precision and image quality. Assembly code integrated from @http://sourceforge.net/projects/jpegdec into Delphi
// - very fast, but need a SSE or SSE2 compatible CPU
// - this function will allocate the bitmap image into pImg parameter
// - return an error code, or JPEG_SUCCESS on sucess
// - all memory must be freed by a pImg^.Free call (in a try...finally block)
function JpegDecode(Buffer: pointer; BufferLen: Cardinal; var pImg: PJpegDecode): TJpegDecodeError; stdcall; overload;

/// helper function which creates a TBitmap from raw .JPEG memory buffer
function JpegDecode(Buffer: pointer; BufferLen: integer): TBitmap; overload;

/// helper function which draw raw .JPEG memory buffer into a Canvas
// - perform the JPEG decompression at every call: if you want to draw a JPEG in a WM_PAINT event (i.e. Delphi OnPaint event), don't use this procedure
// but perform the JPEG decompression once, save the PJpegDecode instance,
// and draw the picture at request by using the fast TJpegDecode.DrawTo() methods;
// another possibility (more VCLish) is to use a temporary TBitmap for the drawing
procedure JpegDraw(Buffer: pointer; BufferLen: integer; Canvas: TCanvas; X,Y: integer);


IMPLEMENTATION





function AllocMem(Size: integer): pointer;
asm // use Windows heap for memory allocation (16 bytes align + set to zero)
        push        PAGE_READWRITE
        push        MEM_COMMIT
        push        eax
        push        0
        call        VirtualAlloc
end;


function ReleaseMem(buf: pointer): integer;
asm
        test        eax, eax
        jz          @Done
        push        MEM_RELEASE
        push        0
        push        eax
        call        VirtualFree
        sub         eax, eax
@Done:
end;


CONST
  TBLSize = $B00;                        // size of data to be copied from TBL into 16 bytes aligned memory (otherwize SSE2 will fail)
  TBLOffset = $1B10;                     // normal PJpegDecode needed data size -> TBL is copied after this
  TBL64Offset = TBLOffset+TBLSize;       // TBL_64 offset in PJpegDecode
  TBL64Size = 65*8;                      // TBL_64 size


procedure TBL;
asm
// _TBL_MultCR;
        db 00H, 00H, 00H, 00H, 0E1H, 0D1H, 36H, 0BFH
        db 0BCH, 74H, 0B3H, 3FH, 00H, 00H, 00H, 00H
// _TBL_MultCB: TBL+010H
        db 0E5H, 0D0H, 0E2H, 3FH, 1EH, 33H, 0B0H, 0BEH
        db 00H, 00H, 00H, 00H, 00H, 00H, 00H, 00H
// _TBL_tg1_16; TBL+020H
        db 0AFH, 0AFH, 4BH, 3EH, 0AFH, 0AFH, 4BH, 3EH
        db 0AFH, 0AFH, 4BH, 3EH, 0AFH, 0AFH, 4BH, 3EH
// _TBL_tg2_16; TBL+030H
        db 0CDH, 13H, 0D4H, 3EH, 0CDH, 13H, 0D4H, 3EH
        db 0CDH, 13H, 0D4H, 3EH, 0CDH, 13H, 0D4H, 3EH
// _TBL_tg3_16; TBL+040H
        db 0C1H, 0DH, 2BH, 3FH, 0C1H, 0DH, 2BH, 3FH
        db 0C1H, 0DH, 2BH, 3FH, 0C1H, 0DH, 2BH, 3FH
// _TBL_tg1_32; TBL+050H
        db 0DCH, 0B5H, 0C9H, 3DH, 0DCH, 0B5H, 0C9H, 3DH
        db 0DCH, 0B5H, 0C9H, 3DH, 0DCH, 0B5H, 0C9H, 3DH
// _TBL_tg3_32; TBL+060H
        db 42H, 50H, 9BH, 3EH, 42H, 50H, 9BH, 3EH
        db 42H, 50H, 9BH, 3EH, 42H, 50H, 9BH, 3EH
// _TBL_tg5_32; TBL+070H
        db 0B9H, 0D5H, 08H, 3FH, 0B9H, 0D5H, 08H, 3FH
        db 0B9H, 0D5H, 08H, 3FH, 0B9H, 0D5H, 08H, 3FH
// _TBL_tg7_32; TBL+080H
        db 01H, 18H, 52H, 3FH, 01H, 18H, 52H, 3FH
        db 01H, 18H, 52H, 3FH, 01H, 18H, 52H, 3FH
// _TBL_cos2_16; TBL+090H
        db 5EH, 83H, 6CH, 3FH, 5EH, 83H, 6CH, 3FH
        db 5EH, 83H, 6CH, 3FH, 5EH, 83H, 6CH, 3FH
// _TBL_cos4_16; TBL+0A0H
        db 0F3H, 04H, 35H, 3FH, 0F3H, 04H, 35H, 3FH
        db 0F3H, 04H, 35H, 3FH, 0F3H, 04H, 35H, 3FH
// _TBL_MultRow8x8; TBL+0B0H
        dd offset @TBL_8x8_04 // offsets will be recalculated in JpegDecode()
        dd offset @TBL_8x8_17
        dd offset @TBL_8x8_26
        dd offset @TBL_8x8_35
        dd offset @TBL_8x8_04
        dd offset @TBL_8x8_35
        dd offset @TBL_8x8_26
        dd offset @TBL_8x8_17
//_TBL_MultRow16x16: _TBL_MultRow8x8+020H = TBL+0D0H
        dd offset @TBL_16x16_08 // offsets will be recalculated in JpegDecode()
        dd offset @TBL_16x16_1F
        dd offset @TBL_16x16_2E
        dd offset @TBL_16x16_3D
        dd offset @TBL_16x16_4C
        dd offset @TBL_16x16_5B
        dd offset @TBL_16x16_6A
        dd offset @TBL_16x16_79
@TBL_8x8_04: 
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 75H, 3DH, 27H, 3EH, 0D4H, 8BH, 8AH, 3DH
        db 0D4H, 8BH, 8AH, 0BDH, 75H, 3DH, 27H, 0BEH
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 0BEH
        db 00H, 00H, 00H, 0BEH, 00H, 00H, 00H, 3EH
        db 0D4H, 8BH, 8AH, 3DH, 75H, 3DH, 27H, 0BEH
        db 75H, 3DH, 27H, 3EH, 0D4H, 8BH, 8AH, 0BDH
        db 86H, 8AH, 31H, 3EH, 17H, 83H, 16H, 3EH
        db 4EH, 23H, 0C9H, 3DH, 0AFH, 42H, 0DH, 3DH
        db 17H, 83H, 16H, 3EH, 0AFH, 42H, 0DH, 0BDH
        db 86H, 8AH, 31H, 0BEH, 4EH, 23H, 0C9H, 0BDH
        db 4EH, 23H, 0C9H, 3DH, 86H, 8AH, 31H, 0BEH
        db 0AFH, 42H, 0DH, 3DH, 17H, 83H, 16H, 3EH
        db 0AFH, 42H, 0DH, 3DH, 4EH, 23H, 0C9H, 0BDH
        db 17H, 83H, 16H, 3EH, 86H, 8AH, 31H, 0BEH
@TBL_8x8_17:
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 0F8H, 0F7H, 67H, 3EH, 4AH, 2BH, 0C0H, 3DH
        db 4AH, 2BH, 0C0H, 0BDH, 0F8H, 0F7H, 67H, 0BEH
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 0BEH
        db 86H, 8AH, 31H, 0BEH, 86H, 8AH, 31H, 3EH
        db 4AH, 2BH, 0C0H, 3DH, 0F8H, 0F7H, 67H, 0BEH
        db 0F8H, 0F7H, 67H, 3EH, 4AH, 2BH, 0C0H, 0BDH
        db 0AFH, 41H, 76H, 3EH, 29H, 0C4H, 50H, 3EH
        db 3FH, 7EH, 0BH, 3EH, 15H, 0EFH, 43H, 3DH
        db 29H, 0C4H, 50H, 3EH, 15H, 0EFH, 43H, 0BDH
        db 0AFH, 41H, 76H, 0BEH, 3FH, 7EH, 0BH, 0BEH
        db 3FH, 7EH, 0BH, 3EH, 0AFH, 41H, 76H, 0BEH
        db 15H, 0EFH, 43H, 3DH, 29H, 0C4H, 50H, 3EH
        db 15H, 0EFH, 43H, 3DH, 3FH, 7EH, 0BH, 0BEH
        db 29H, 0C4H, 50H, 3EH, 0AFH, 41H, 76H, 0BEH
@TBL_8x8_26:
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH
        db 7AH, 82H, 5AH, 3EH, 0F3H, 04H, 0B5H, 3DH
        db 0F3H, 04H, 0B5H, 0BDH, 7AH, 82H, 5AH, 0BEH
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 0BEH
        db 75H, 3DH, 27H, 0BEH, 75H, 3DH, 27H, 3EH
        db 0F3H, 04H, 0B5H, 3DH, 7AH, 82H, 5AH, 0BEH
        db 7AH, 82H, 5AH, 3EH, 0F3H, 04H, 0B5H, 0BDH
        db 0F8H, 0F7H, 67H, 3EH, 4CH, 0A7H, 44H, 3EH
        db 51H, 66H, 03H, 3EH, 0D3H, 90H, 38H, 3DH
        db 4CH, 0A7H, 44H, 3EH, 0D3H, 90H, 38H, 0BDH
        db 0F8H, 0F7H, 67H, 0BEH, 51H, 66H, 03H, 0BEH
        db 51H, 66H, 03H, 3EH, 0F8H, 0F7H, 67H, 0BEH
        db 0D3H, 90H, 38H, 3DH, 4CH, 0A7H, 44H, 3EH
        db 0D3H, 90H, 38H, 3DH, 51H, 66H, 03H, 0BEH
        db 4CH, 0A7H, 44H, 3EH, 0F8H, 0F7H, 67H, 0BEH
@TBL_8x8_35:
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH
        db 4CH, 0A7H, 44H, 3EH, 0C1H, 0E9H, 0A2H, 3DH
        db 0C1H, 0E9H, 0A2H, 0BDH, 4CH, 0A7H, 44H, 0BEH
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 0BEH
        db 17H, 83H, 16H, 0BEH, 17H, 83H, 16H, 3EH
        db 0C1H, 0E9H, 0A2H, 3DH, 4CH, 0A7H, 44H, 0BEH
        db 4CH, 0A7H, 44H, 3EH, 0C1H, 0E9H, 0A2H, 0BDH
        db 29H, 0C4H, 50H, 3EH, 0C5H, 0FBH, 30H, 3EH
        db 5EH, 83H, 0ECH, 3DH, 0D1H, 1AH, 26H, 3DH
        db 0C5H, 0FBH, 30H, 3EH, 0D1H, 1AH, 26H, 0BDH
        db 29H, 0C4H, 50H, 0BEH, 5EH, 83H, 0ECH, 0BDH
        db 5EH, 83H, 0ECH, 3DH, 29H, 0C4H, 50H, 0BEH
        db 0D1H, 1AH, 26H, 3DH, 0C5H, 0FBH, 30H, 3EH
        db 0D1H, 1AH, 26H, 3DH, 5EH, 83H, 0ECH, 0BDH
        db 0C5H, 0FBH, 30H, 3EH, 29H, 0C4H, 50H, 0BEH
@TBL_16x16_08:
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 86H, 8AH, 31H, 3EH, 17H, 83H, 16H, 3EH
        db 4EH, 23H, 0C9H, 3DH, 0AFH, 42H, 0DH, 3DH
        db 75H, 3DH, 27H, 3EH, 0D4H, 8BH, 8AH, 3DH
        db 0D4H, 8BH, 8AH, 0BDH, 75H, 3DH, 27H, 0BEH
        db 17H, 83H, 16H, 3EH, 0AFH, 42H, 0DH, 0BDH
        db 86H, 8AH, 31H, 0BEH, 4EH, 23H, 0C9H, 0BDH
        db 0CEH, 25H, 34H, 3EH, 86H, 39H, 2DH, 3EH
        db 12H, 0A5H, 1FH, 3EH, 0AH, 0EEH, 0BH, 3EH
        db 86H, 39H, 2DH, 3EH, 0C6H, 0ACH, 0E5H, 3DH
        db 0A9H, 0F1H, 8DH, 3CH, 0F2H, 0A9H, 0AAH, 0BDH
        db 12H, 0A5H, 1FH, 3EH, 0A9H, 0F1H, 8DH, 3CH
        db 0AH, 0EEH, 0BH, 0BEH, 86H, 39H, 2DH, 0BEH
        db 0AH, 0EEH, 0BH, 3EH, 0F2H, 0A9H, 0AAH, 0BDH
        db 86H, 39H, 2DH, 0BEH, 0A9H, 0F1H, 8DH, 3CH
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 00H, 00H, 00H, 3EH, 00H, 00H, 00H, 3EH
        db 0AFH, 42H, 0DH, 0BDH, 4EH, 23H, 0C9H, 0BDH
        db 17H, 83H, 16H, 0BEH, 86H, 8AH, 31H, 0BEH
        db 75H, 3DH, 27H, 0BEH, 0D4H, 8BH, 8AH, 0BDH
        db 0D4H, 8BH, 8AH, 3DH, 75H, 3DH, 27H, 3EH
        db 4EH, 23H, 0C9H, 3DH, 86H, 8AH, 31H, 3EH
        db 0AFH, 42H, 0DH, 3DH, 17H, 83H, 16H, 0BEH
        db 0C6H, 0ACH, 0E5H, 3DH, 0F2H, 0A9H, 0AAH, 3DH
        db 45H, 30H, 52H, 3DH, 0A9H, 0F1H, 8DH, 3CH
        db 12H, 0A5H, 1FH, 0BEH, 0CEH, 25H, 34H, 0BEH
        db 0AH, 0EEH, 0BH, 0BEH, 45H, 30H, 52H, 0BDH
        db 45H, 30H, 52H, 0BDH, 0C6H, 0ACH, 0E5H, 3DH
        db 0CEH, 25H, 34H, 3EH, 0F2H, 0A9H, 0AAH, 3DH
        db 0CEH, 25H, 34H, 3EH, 45H, 30H, 52H, 3DH
        db 12H, 0A5H, 1FH, 0BEH, 0C6H, 0ACH, 0E5H, 0BDH
@TBL_16x16_1F:
        db 0CEH, 25H, 34H, 3EH, 0CEH, 25H, 34H, 3EH
        db 0CEH, 25H, 34H, 3EH, 0CEH, 25H, 34H, 3EH
        db 3CH, 0DFH, 79H, 3EH, 0CDH, 0D4H, 53H, 3EH
        db 87H, 8AH, 0DH, 3EH, 7FH, 0CFH, 46H, 3DH
        db 0D1H, 5FH, 6BH, 3EH, 8EH, 0FDH, 0C2H, 3DH
        db 8EH, 0FDH, 0C2H, 0BDH, 0D1H, 5FH, 6BH, 0BEH
        db 0CDH, 0D4H, 53H, 3EH, 7FH, 0CFH, 46H, 0BDH
        db 3CH, 0DFH, 79H, 0BEH, 87H, 8AH, 0DH, 0BEH
        db 5FH, 8AH, 7DH, 3EH, 0EH, 0CCH, 73H, 3EH
        db 48H, 0AFH, 60H, 3EH, 12H, 0F0H, 44H, 3EH
        db 0EH, 0CCH, 73H, 3EH, 67H, 9FH, 21H, 3EH
        db 0C2H, 0C5H, 0C7H, 3CH, 64H, 31H, 0F0H, 0BDH
        db 48H, 0AFH, 60H, 3EH, 0C2H, 0C5H, 0C7H, 3CH
        db 12H, 0F0H, 44H, 0BEH, 0EH, 0CCH, 73H, 0BEH
        db 12H, 0F0H, 44H, 3EH, 64H, 31H, 0F0H, 0BDH
        db 0EH, 0CCH, 73H, 0BEH, 0C2H, 0C5H, 0C7H, 3CH
        db 0CEH, 25H, 34H, 3EH, 0CEH, 25H, 34H, 3EH
        db 0CEH, 25H, 34H, 3EH, 0CEH, 25H, 34H, 3EH
        db 7FH, 0CFH, 46H, 0BDH, 87H, 8AH, 0DH, 0BEH
        db 0CDH, 0D4H, 53H, 0BEH, 3CH, 0DFH, 79H, 0BEH
        db 0D1H, 5FH, 6BH, 0BEH, 8EH, 0FDH, 0C2H, 0BDH
        db 8EH, 0FDH, 0C2H, 3DH, 0D1H, 5FH, 6BH, 3EH
        db 87H, 8AH, 0DH, 3EH, 3CH, 0DFH, 79H, 3EH
        db 7FH, 0CFH, 46H, 3DH, 0CDH, 0D4H, 53H, 0BEH
        db 67H, 9FH, 21H, 3EH, 64H, 31H, 0F0H, 3DH
        db 0FBH, 0E8H, 93H, 3DH, 0C2H, 0C5H, 0C7H, 3CH
        db 48H, 0AFH, 60H, 0BEH, 5FH, 8AH, 7DH, 0BEH
        db 12H, 0F0H, 44H, 0BEH, 0FBH, 0E8H, 93H, 0BDH
        db 0FBH, 0E8H, 93H, 0BDH, 67H, 9FH, 21H, 3EH
        db 5FH, 8AH, 7DH, 3EH, 64H, 31H, 0F0H, 3DH
        db 5FH, 8AH, 7DH, 3EH, 0FBH, 0E8H, 93H, 3DH
        db 48H, 0AFH, 60H, 0BEH, 67H, 9FH, 21H, 0BEH
@TBL_16x16_2E:
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 0AFH, 41H, 76H, 3EH, 29H, 0C4H, 50H, 3EH
        db 3FH, 7EH, 0BH, 3EH, 15H, 0EFH, 43H, 3DH
        db 0F8H, 0F7H, 67H, 3EH, 4AH, 2BH, 0C0H, 3DH
        db 4AH, 2BH, 0C0H, 0BDH, 0F8H, 0F7H, 67H, 0BEH
        db 29H, 0C4H, 50H, 3EH, 15H, 0EFH, 43H, 0BDH
        db 0AFH, 41H, 76H, 0BEH, 3FH, 7EH, 0BH, 0BEH
        db 3CH, 0DFH, 79H, 3EH, 02H, 45H, 70H, 3EH
        db 07H, 6FH, 5DH, 3EH, 98H, 16H, 42H, 3EH
        db 02H, 45H, 70H, 3EH, 0BCH, 48H, 1FH, 3EH
        db 0C8H, 0E1H, 0C4H, 3CH, 0B2H, 0B7H, 0ECH, 0BDH
        db 07H, 6FH, 5DH, 3EH, 0C8H, 0E1H, 0C4H, 3CH
        db 98H, 16H, 42H, 0BEH, 02H, 45H, 70H, 0BEH
        db 98H, 16H, 42H, 3EH, 0B2H, 0B7H, 0ECH, 0BDH
        db 02H, 45H, 70H, 0BEH, 0C8H, 0E1H, 0C4H, 3CH
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 86H, 8AH, 31H, 3EH, 86H, 8AH, 31H, 3EH
        db 15H, 0EFH, 43H, 0BDH, 3FH, 7EH, 0BH, 0BEH
        db 29H, 0C4H, 50H, 0BEH, 0AFH, 41H, 76H, 0BEH
        db 0F8H, 0F7H, 67H, 0BEH, 4AH, 2BH, 0C0H, 0BDH
        db 4AH, 2BH, 0C0H, 3DH, 0F8H, 0F7H, 67H, 3EH
        db 3FH, 7EH, 0BH, 3EH, 0AFH, 41H, 76H, 3EH
        db 15H, 0EFH, 43H, 3DH, 29H, 0C4H, 50H, 0BEH
        db 0BCH, 48H, 1FH, 3EH, 0B2H, 0B7H, 0ECH, 3DH
        db 1CH, 0C5H, 91H, 3DH, 0C8H, 0E1H, 0C4H, 3CH
        db 07H, 6FH, 5DH, 0BEH, 3CH, 0DFH, 79H, 0BEH
        db 98H, 16H, 42H, 0BEH, 1CH, 0C5H, 91H, 0BDH
        db 1CH, 0C5H, 91H, 0BDH, 0BCH, 48H, 1FH, 3EH
        db 3CH, 0DFH, 79H, 3EH, 0B2H, 0B7H, 0ECH, 3DH
        db 3CH, 0DFH, 79H, 3EH, 1CH, 0C5H, 91H, 3DH
        db 07H, 6FH, 5DH, 0BEH, 0BCH, 48H, 1FH, 0BEH
@TBL_16x16_3D:
        db 86H, 39H, 2DH, 3EH, 86H, 39H, 2DH, 3EH
        db 86H, 39H, 2DH, 3EH, 86H, 39H, 2DH, 3EH
        db 02H, 45H, 70H, 3EH, 0D2H, 0B0H, 4BH, 3EH     
        db 0EH, 1AH, 08H, 3EH, 9CH, 2BH, 3FH, 3DH       
        db 38H, 54H, 62H, 3EH, 40H, 7FH, 0BBH, 3DH      
        db 40H, 7FH, 0BBH, 0BDH, 38H, 54H, 62H, 0BEH    
        db 0D2H, 0B0H, 4BH, 3EH, 9CH, 2BH, 3FH, 0BDH    
        db 02H, 45H, 70H, 0BEH, 0EH, 1AH, 08H, 0BEH     
        db 0EH, 0CCH, 73H, 3EH, 99H, 6DH, 6AH, 3EH      
        db 0D9H, 0CH, 58H, 3EH, 9CH, 5EH, 3DH, 3EH      
        db 99H, 6DH, 6AH, 3EH, 5EH, 69H, 1BH, 3EH       
        db 68H, 18H, 0C0H, 3CH, 64H, 0F6H, 0E6H, 0BDH
        db 0D9H, 0CH, 58H, 3EH, 68H, 18H, 0C0H, 3CH     
        db 9CH, 5EH, 3DH, 0BEH, 99H, 6DH, 6AH, 0BEH     
        db 9CH, 5EH, 3DH, 3EH, 64H, 0F6H, 0E6H, 0BDH    
        db 99H, 6DH, 6AH, 0BEH, 68H, 18H, 0C0H, 3CH     
        db 86H, 39H, 2DH, 3EH, 86H, 39H, 2DH, 3EH       
        db 86H, 39H, 2DH, 3EH, 86H, 39H, 2DH, 3EH       
        db 9CH, 2BH, 3FH, 0BDH, 0EH, 1AH, 08H, 0BEH     
        db 0D2H, 0B0H, 4BH, 0BEH, 02H, 45H, 70H, 0BEH
        db 38H, 54H, 62H, 0BEH, 40H, 7FH, 0BBH, 0BDH    
        db 40H, 7FH, 0BBH, 3DH, 38H, 54H, 62H, 3EH
        db 0EH, 1AH, 08H, 3EH, 02H, 45H, 70H, 3EH       
        db 9CH, 2BH, 3FH, 3DH, 0D2H, 0B0H, 4BH, 0BEH
        db 5EH, 69H, 1BH, 3EH, 64H, 0F6H, 0E6H, 3DH     
        db 0DAH, 39H, 8EH, 3DH, 68H, 18H, 0C0H, 3CH     
        db 0D9H, 0CH, 58H, 0BEH, 0EH, 0CCH, 73H, 0BEH   
        db 9CH, 5EH, 3DH, 0BEH, 0DAH, 39H, 8EH, 0BDH    
        db 0DAH, 39H, 8EH, 0BDH, 5EH, 69H, 1BH, 3EH     
        db 0EH, 0CCH, 73H, 3EH, 64H, 0F6H, 0E6H, 3DH    
        db 0EH, 0CCH, 73H, 3EH, 0DAH, 39H, 8EH, 3DH     
        db 0D9H, 0CH, 58H, 0BEH, 5EH, 69H, 1BH, 0BEH    
@TBL_16x16_4C:
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH       
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH       
        db 0F8H, 0F7H, 67H, 3EH, 4CH, 0A7H, 44H, 3EH    
        db 51H, 66H, 03H, 3EH, 0D3H, 90H, 38H, 3DH      
        db 7AH, 82H, 5AH, 3EH, 0F3H, 04H, 0B5H, 3DH
        db 0F3H, 04H, 0B5H, 0BDH, 7AH, 82H, 5AH, 0BEH   
        db 4CH, 0A7H, 44H, 3EH, 0D3H, 90H, 38H, 0BDH    
        db 0F8H, 0F7H, 67H, 0BEH, 51H, 66H, 03H, 0BEH   
        db 0D1H, 5FH, 6BH, 3EH, 38H, 54H, 62H, 3EH
        db 03H, 96H, 50H, 3EH, 0C0H, 0D3H, 36H, 3EH
        db 38H, 54H, 62H, 3EH, 0D8H, 0AH, 16H, 3EH      
        db 70H, 75H, 0B9H, 3CH, 0AAH, 0FBH, 0DEH, 0BDH
        db 03H, 96H, 50H, 3EH, 70H, 75H, 0B9H, 3CH      
        db 0C0H, 0D3H, 36H, 0BEH, 38H, 54H, 62H, 0BEH   
        db 0C0H, 0D3H, 36H, 3EH, 0AAH, 0FBH, 0DEH, 0BDH 
        db 38H, 54H, 62H, 0BEH, 70H, 75H, 0B9H, 3CH     
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH
        db 75H, 3DH, 27H, 3EH, 75H, 3DH, 27H, 3EH       
        db 0D3H, 90H, 38H, 0BDH, 51H, 66H, 03H, 0BEH    
        db 4CH, 0A7H, 44H, 0BEH, 0F8H, 0F7H, 67H, 0BEH
        db 7AH, 82H, 5AH, 0BEH, 0F3H, 04H, 0B5H, 0BDH   
        db 0F3H, 04H, 0B5H, 3DH, 7AH, 82H, 5AH, 3EH     
        db 51H, 66H, 03H, 3EH, 0F8H, 0F7H, 67H, 3EH
        db 0D3H, 90H, 38H, 3DH, 4CH, 0A7H, 44H, 0BEH
        db 0D8H, 0AH, 16H, 3EH, 0AAH, 0FBH, 0DEH, 3DH   
        db 0F2H, 4FH, 89H, 3DH, 70H, 75H, 0B9H, 3CH     
        db 03H, 96H, 50H, 0BEH, 0D1H, 5FH, 6BH, 0BEH    
        db 0C0H, 0D3H, 36H, 0BEH, 0F2H, 4FH, 89H, 0BDH
        db 0F2H, 4FH, 89H, 0BDH, 0D8H, 0AH, 16H, 3EH    
        db 0D1H, 5FH, 6BH, 3EH, 0AAH, 0FBH, 0DEH, 3DH   
        db 0D1H, 5FH, 6BH, 3EH, 0F2H, 4FH, 89H, 3DH     
        db 03H, 96H, 50H, 0BEH, 0D8H, 0AH, 16H, 0BEH    
@TBL_16x16_5B:
        db 12H, 0A5H, 1FH, 3EH, 12H, 0A5H, 1FH, 3EH     
        db 12H, 0A5H, 1FH, 3EH, 12H, 0A5H, 1FH, 3EH     
        db 07H, 6FH, 5DH, 3EH, 0F1H, 0B8H, 3BH, 3EH     
        db 3EH, 0DDH, 0FAH, 3DH, 01H, 2FH, 30H, 3DH     
        db 03H, 96H, 50H, 3EH, 5DH, 0CCH, 0ACH, 3DH     
        db 5DH, 0CCH, 0ACH, 0BDH, 03H, 96H, 50H, 0BEH   
        db 0F1H, 0B8H, 3BH, 3EH, 01H, 2FH, 30H, 0BDH    
        db 07H, 6FH, 5DH, 0BEH, 3EH, 0DDH, 0FAH, 0BDH   
        db 48H, 0AFH, 60H, 3EH, 0D9H, 0CH, 58H, 3EH     
        db 0EDH, 1CH, 47H, 3EH, 25H, 86H, 2EH, 3EH      
        db 0D9H, 0CH, 58H, 3EH, 67H, 3AH, 0FH, 3EH      
        db 3DH, 09H, 0B1H, 3CH, 31H, 0DBH, 0D4H, 0BDH   
        db 0EDH, 1CH, 47H, 3EH, 3DH, 09H, 0B1H, 3CH     
        db 25H, 86H, 2EH, 0BEH, 0D9H, 0CH, 58H, 0BEH    
        db 25H, 86H, 2EH, 3EH, 31H, 0DBH, 0D4H, 0BDH
        db 0D9H, 0CH, 58H, 0BEH, 3DH, 09H, 0B1H, 3CH    
        db 12H, 0A5H, 1FH, 3EH, 12H, 0A5H, 1FH, 3EH
        db 12H, 0A5H, 1FH, 3EH, 12H, 0A5H, 1FH, 3EH     
        db 01H, 2FH, 30H, 0BDH, 3EH, 0DDH, 0FAH, 0BDH
        db 0F1H, 0B8H, 3BH, 0BEH, 07H, 6FH, 5DH, 0BEH   
        db 03H, 96H, 50H, 0BEH, 5DH, 0CCH, 0ACH, 0BDH   
        db 5DH, 0CCH, 0ACH, 3DH, 03H, 96H, 50H, 3EH
        db 3EH, 0DDH, 0FAH, 3DH, 07H, 6FH, 5DH, 3EH     
        db 01H, 2FH, 30H, 3DH, 0F1H, 0B8H, 3BH, 0BEH    
        db 67H, 3AH, 0FH, 3EH, 31H, 0DBH, 0D4H, 3DH     
        db 83H, 13H, 83H, 3DH, 3DH, 09H, 0B1H, 3CH      
        db 0EDH, 1CH, 47H, 0BEH, 48H, 0AFH, 60H, 0BEH   
        db 25H, 86H, 2EH, 0BEH, 83H, 13H, 83H, 0BDH     
        db 83H, 13H, 83H, 0BDH, 67H, 3AH, 0FH, 3EH      
        db 48H, 0AFH, 60H, 3EH, 31H, 0DBH, 0D4H, 3DH    
        db 48H, 0AFH, 60H, 3EH, 83H, 13H, 83H, 3DH      
        db 0EDH, 1CH, 47H, 0BEH, 67H, 3AH, 0FH, 0BEH    
@TBL_16x16_6A:
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH       
        db 29H, 0C4H, 50H, 3EH, 0C5H, 0FBH, 30H, 3EH    
        db 5EH, 83H, 0ECH, 3DH, 0D1H, 1AH, 26H, 3DH
        db 4CH, 0A7H, 44H, 3EH, 0C1H, 0E9H, 0A2H, 3DH
        db 0C1H, 0E9H, 0A2H, 0BDH, 4CH, 0A7H, 44H, 0BEH 
        db 0C5H, 0FBH, 30H, 3EH, 0D1H, 1AH, 26H, 0BDH
        db 29H, 0C4H, 50H, 0BEH, 5EH, 83H, 0ECH, 0BDH   
        db 0CDH, 0D4H, 53H, 3EH, 0D2H, 0B0H, 4BH, 3EH   
        db 0F1H, 0B8H, 3BH, 3EH, 43H, 8AH, 24H, 3EH     
        db 0D2H, 0B0H, 4BH, 3EH, 0D9H, 08H, 07H, 3EH    
        db 91H, 0E8H, 0A6H, 3CH, 0F1H, 0ADH, 0C8H, 0BDH
        db 0F1H, 0B8H, 3BH, 3EH, 91H, 0E8H, 0A6H, 3CH
        db 43H, 8AH, 24H, 0BEH, 0D2H, 0B0H, 4BH, 0BEH   
        db 43H, 8AH, 24H, 3EH, 0F1H, 0ADH, 0C8H, 0BDH   
        db 0D2H, 0B0H, 4BH, 0BEH, 91H, 0E8H, 0A6H, 3CH  
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH       
        db 17H, 83H, 16H, 3EH, 17H, 83H, 16H, 3EH       
        db 0D1H, 1AH, 26H, 0BDH, 5EH, 83H, 0ECH, 0BDH   
        db 0C5H, 0FBH, 30H, 0BEH, 29H, 0C4H, 50H, 0BEH  
        db 4CH, 0A7H, 44H, 0BEH, 0C1H, 0E9H, 0A2H, 0BDH 
        db 0C1H, 0E9H, 0A2H, 3DH, 4CH, 0A7H, 44H, 3EH   
        db 5EH, 83H, 0ECH, 3DH, 29H, 0C4H, 50H, 3EH
        db 0D1H, 1AH, 26H, 3DH, 0C5H, 0FBH, 30H, 0BEH   
        db 0D9H, 08H, 07H, 3EH, 0F1H, 0ADH, 0C8H, 3DH   
        db 0D5H, 27H, 77H, 3DH, 91H, 0E8H, 0A6H, 3CH    
        db 0F1H, 0B8H, 3BH, 0BEH, 0CDH, 0D4H, 53H, 0BEH 
        db 43H, 8AH, 24H, 0BEH, 0D5H, 27H, 77H, 0BDH    
        db 0D5H, 27H, 77H, 0BDH, 0D9H, 08H, 07H, 3EH    
        db 0CDH, 0D4H, 53H, 3EH, 0F1H, 0ADH, 0C8H, 3DH  
        db 0CDH, 0D4H, 53H, 3EH, 0D5H, 27H, 77H, 3DH    
        db 0F1H, 0B8H, 3BH, 0BEH, 0D9H, 08H, 07H, 0BEH  
@TBL_16x16_79:
        db 0AH, 0EEH, 0BH, 3EH, 0AH, 0EEH, 0BH, 3EH     
        db 0AH, 0EEH, 0BH, 3EH, 0AH, 0EEH, 0BH, 3EH     
        db 98H, 16H, 42H, 3EH, 43H, 8AH, 24H, 3EH       
        db 64H, 0E2H, 0DBH, 3DH, 1DH, 6DH, 1AH, 3DH     
        db 0C0H, 0D3H, 36H, 3EH, 7FH, 75H, 97H, 3DH     
        db 7FH, 75H, 97H, 0BDH, 0C0H, 0D3H, 36H, 0BEH
        db 43H, 8AH, 24H, 3EH, 1DH, 6DH, 1AH, 0BDH      
        db 98H, 16H, 42H, 0BEH, 64H, 0E2H, 0DBH, 0BDH   
        db 12H, 0F0H, 44H, 3EH, 9CH, 5EH, 3DH, 3EH      
        db 25H, 86H, 2EH, 3EH, 0B8H, 0F8H, 18H, 3EH
        db 9CH, 5EH, 3DH, 3EH, 0BEH, 14H, 0FBH, 3DH
        db 66H, 2CH, 9BH, 3CH, 0EEH, 91H, 0BAH, 0BDH
        db 25H, 86H, 2EH, 3EH, 66H, 2CH, 9BH, 3CH       
        db 0B8H, 0F8H, 18H, 0BEH, 9CH, 5EH, 3DH, 0BEH
        db 0B8H, 0F8H, 18H, 3EH, 0EEH, 91H, 0BAH, 0BDH
        db 9CH, 5EH, 3DH, 0BEH, 66H, 2CH, 9BH, 3CH
        db 0AH, 0EEH, 0BH, 3EH, 0AH, 0EEH, 0BH, 3EH
        db 0AH, 0EEH, 0BH, 3EH, 0AH, 0EEH, 0BH, 3EH
        db 1DH, 6DH, 1AH, 0BDH, 64H, 0E2H, 0DBH, 0BDH
        db 43H, 8AH, 24H, 0BEH, 98H, 16H, 42H, 0BEH
        db 0C0H, 0D3H, 36H, 0BEH, 7FH, 75H, 97H, 0BDH
        db 7FH, 75H, 97H, 3DH, 0C0H, 0D3H, 36H, 3EH
        db 64H, 0E2H, 0DBH, 3DH, 98H, 16H, 42H, 3EH
        db 1DH, 6DH, 1AH, 3DH, 43H, 8AH, 24H, 0BEH
        db 0BEH, 14H, 0FBH, 3DH, 0EEH, 91H, 0BAH, 3DH
        db 4DH, 0C7H, 65H, 3DH, 66H, 2CH, 9BH, 3CH
        db 25H, 86H, 2EH, 0BEH, 12H, 0F0H, 44H, 0BEH
        db 0B8H, 0F8H, 18H, 0BEH, 4DH, 0C7H, 65H, 0BDH
        db 4DH, 0C7H, 65H, 0BDH, 0BEH, 14H, 0FBH, 3DH
        db 12H, 0F0H, 44H, 3EH, 0EEH, 91H, 0BAH, 3DH
        db 12H, 0F0H, 44H, 3EH, 4DH, 0C7H, 65H, 3DH
        db 25H, 86H, 2EH, 0BEH, 0BEH, 14H, 0FBH, 0BDH
end;


procedure RGB_GrayConv_SSE2;
asm
        push    esi
        mov     ecx, dword ptr [ebp + 000000C8H]
        mov     edx, dword ptr [ebp + 000000B8H]
        mov     esi, dword ptr [ebp + 000000D4H]
        shr     edx, 2
        movaps  xmm7, dqword ptr [ebp + 000000E0H]
        neg     edx
@@016:  mov     eax, dword ptr [edx + esi]
        movaps  xmm0, dqword ptr [eax]                 
        movaps  xmm1, dqword ptr [eax + 10H]           
        addps   xmm0, xmm7                              
        addps   xmm1, xmm7                              
        cvtps2dq xmm0, xmm0
        cvtps2dq xmm1, xmm1                             
        pshufd  xmm2, xmm0, -1                          
        pshufd  xmm3, xmm0, -86                         
        pshufd  xmm4, xmm0, 85                          
        pshufd  xmm0, xmm0, 0                           
        packssdw xmm3, xmm2                             
        packssdw xmm0, xmm4
        pshufd  xmm2, xmm1, -1                          
        packuswb xmm0, xmm3                             
        pshufd  xmm3, xmm1, -86                         
        pshufd  xmm4, xmm1, 85                          
        pshufd  xmm1, xmm1, 0
        packssdw xmm3, xmm2
        packssdw xmm1, xmm4                             
        packuswb xmm1, xmm3                             
        movdqa  dqword ptr [ecx], xmm0                 
        movdqa  dqword ptr [ecx + 10H], xmm1           
        add     ecx, dword ptr [edx + esi + 04H]        
        add     edx, 8                                  
        jnz     @@016                                   
        dec     dword ptr [ebp + 000000D0H]             
        jz      @@018
@@017:  mov     dword ptr [ebp + 000000C8H], ecx        
        pop     esi
        ret                                             

@@018:  add     ecx, dword ptr [ebp + 000000D8H]        
        mov     eax, dword ptr [ebp + 000000CCH]        
        mov     dword ptr [ebp + 000000D0H], eax        
        jmp     @@017
end;


procedure RGB_GrayConv_SSE;
asm
        push    esi                                     
        mov     ecx, dword ptr [ebp + 000000C8H]        
        mov     edx, dword ptr [ebp + 000000B8H]        
        mov     esi, dword ptr [ebp + 000000D4H]        
        shr     edx, 2
        movaps  xmm7, dqword ptr [ebp + 000000E0H]     
        neg     edx                                     
@@019:  mov     eax, dword ptr [edx + esi]              
        movaps  xmm0, dqword ptr [eax]                 
        movaps  xmm1, dqword ptr [eax + 10H]           
        addps   xmm0, xmm7                              
        addps   xmm1, xmm7
        cvtps2pi mm1, xmm0                              
        cvtps2pi mm2, xmm1                              
        movhlps xmm0, xmm0                              
        movhlps xmm1, xmm1                              
        cvtps2pi mm3, xmm0
        cvtps2pi mm4, xmm1
        packssdw mm1, mm3                               
        packssdw mm2, mm4                               
        pshufw  mm5, mm1, -1                            
        pshufw  mm6, mm1, -86                           
        pshufw  mm7, mm1, 85                            
        pshufw  mm1, mm1, 0                             
        packuswb mm6, mm5
        packuswb mm1, mm7
        pshufw  mm3, mm2, -1                            
        pshufw  mm4, mm2, -86
        pshufw  mm5, mm2, 85
        pshufw  mm2, mm2, 0
        packuswb mm4, mm3
        packuswb mm2, mm5
        movq    qword ptr [ecx], mm1
        movq    qword ptr [ecx + 08H], mm6
        movq    qword ptr [ecx + 10H], mm2
        movq    qword ptr [ecx + 18H], mm4
        add     ecx, dword ptr [edx + esi + 04H]        
        add     edx, 8
        jnz     @@019                                   
        dec     dword ptr [ebp + 000000D0H]
        jz      @@021                                   
@@020:  mov     dword ptr [ebp + 000000C8H], ecx
        pop     esi                                     
        ret

@@021:  add     ecx, dword ptr [ebp + 000000D8H]
        mov     eax, dword ptr [ebp + 000000CCH]
        mov     dword ptr [ebp + 000000D0H], eax
        jmp     @@020
end;


procedure RGB_YCbCrConv_SSE2;
asm
        push    esi
        push    edi
        push    ebx
        mov     edx, dword ptr [ebp + 000000C8H]
        mov     eax, dword ptr [ebp + 000000B8H]
        mov     esi, dword ptr [ebp + 000000D4H]
        shr     eax, 1
        neg     eax
        movaps  xmm7, dqword ptr [ebp+TBLOffset]
@@010:  mov     edi, dword ptr [eax + esi]
        mov     ebx, dword ptr [eax + esi + 04H]        
        mov     ecx, dword ptr [eax + esi + 08H]        
        movaps  xmm6, dqword ptr [ebp+TBLOffset+16]
        movaps  xmm0, dqword ptr [edi]                 
        movaps  xmm1, dqword ptr [ebx]                 
        movaps  xmm2, dqword ptr [ecx]                 
        addps   xmm0, dqword ptr [ebp + 000000E0H]     
        pshufd  xmm3, xmm0, 0                           
        pshufd  xmm4, xmm1, 0                           
        pshufd  xmm5, xmm2, 0                           
        mulps   xmm4, xmm6                              
        mulps   xmm5, xmm7                              
        addps   xmm3, xmm4                              
        addps   xmm3, xmm5                              
        pshufd  xmm4, xmm0, 85                          
        pshufd  xmm5, xmm1, 85                          
        pshufd  xmm6, xmm2, 85                          
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        cvtps2dq xmm3, xmm3                             
        cvtps2dq xmm4, xmm4                             
        packssdw xmm3, xmm4                             
        pshufd  xmm4, xmm0, -86                         
        pshufd  xmm5, xmm1, -86
        pshufd  xmm6, xmm2, -86                         
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        pshufd  xmm0, xmm0, -1                          
        pshufd  xmm1, xmm1, -1                          
        pshufd  xmm2, xmm2, -1                          
        mulps   xmm1, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm2, xmm7                              
        addps   xmm0, xmm1                              
        addps   xmm0, xmm2                              
        cvtps2dq xmm4, xmm4                             
        cvtps2dq xmm0, xmm0                             
        packssdw xmm4, xmm0                             
        packuswb xmm3, xmm4                             
        movdqa  dqword ptr [edx], xmm3                 
        movaps  xmm6, dqword ptr [ebp+TBLOffset+16]
        movaps  xmm0, dqword ptr [edi + 10H]           
        movaps  xmm1, dqword ptr [ebx + 10H]           
        movaps  xmm2, dqword ptr [ecx + 10H]           
        addps   xmm0, dqword ptr [ebp + 000000E0H]     
        pshufd  xmm3, xmm0, 0
        pshufd  xmm4, xmm1, 0                           
        pshufd  xmm5, xmm2, 0
        mulps   xmm4, xmm6                              
        mulps   xmm5, xmm7                              
        addps   xmm3, xmm4                              
        addps   xmm3, xmm5                              
        pshufd  xmm4, xmm0, 85                          
        pshufd  xmm5, xmm1, 85                          
        pshufd  xmm6, xmm2, 85
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        cvtps2dq xmm3, xmm3                             
        cvtps2dq xmm4, xmm4                             
        packssdw xmm3, xmm4                             
        pshufd  xmm4, xmm0, -86                         
        pshufd  xmm5, xmm1, -86                         
        pshufd  xmm6, xmm2, -86                         
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm6, xmm7
        addps   xmm4, xmm5
        addps   xmm4, xmm6
        shufps  xmm0, xmm0, -1
        shufps  xmm1, xmm1, -1
        shufps  xmm2, xmm2, -1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm2, xmm7                              
        addps   xmm0, xmm1                              
        addps   xmm0, xmm2                              
        cvtps2dq xmm4, xmm4                             
        cvtps2dq xmm0, xmm0
        packssdw xmm4, xmm0                             
        packuswb xmm3, xmm4                             
        movdqa  dqword ptr [edx + 10H], xmm3           
        add     edx, dword ptr [eax + esi + 0CH]        
        add     eax, 16                                 
        jne     @@010                                   
        dec     dword ptr [ebp + 000000D0H]             
        jz      @@012
@@011:  mov     dword ptr [ebp + 000000C8H], edx
        pop     ebx
        pop     edi
        pop     esi
        ret

@@012:  add     edx, dword ptr [ebp + 000000D8H]
        mov     eax, dword ptr [ebp + 000000CCH]
        mov     dword ptr [ebp + 000000D0H], eax
        jmp     @@011
end;


procedure RGB_YCbCrConv_SSE;
asm
        push    esi                                     
        push    edi                                     
        push    ebx                                     
        mov     edx, dword ptr [ebp + 000000C8H]        
        mov     eax, dword ptr [ebp + 000000B8H]        
        mov     esi, dword ptr [ebp + 000000D4H]        
        shr     eax, 1
        neg     eax                                     
        movaps  xmm7, dqword ptr [ebp+TBLOffset]
@@013:  mov     edi, dword ptr [eax + esi]              
        mov     ebx, dword ptr [eax + esi + 04H]        
        mov     ecx, dword ptr [eax + esi + 08H]        
        movaps  xmm6, dqword ptr [ebp+TBLOffset+16]
        movaps  xmm0, dqword ptr [edi]                 
        movaps  xmm1, dqword ptr [ebx]                 
        movaps  xmm2, dqword ptr [ecx]                 
        addps   xmm0, dqword ptr [ebp + 000000E0H]     
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        movaps  xmm5, xmm2                              
        shufps  xmm3, xmm3, 0                           
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, 0                           
        mulps   xmm4, xmm6                              
        mulps   xmm5, xmm7                              
        addps   xmm3, xmm4                              
        addps   xmm3, xmm5                              
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm1                              
        movaps  xmm6, xmm2                              
        shufps  xmm4, xmm4, 85                          
        shufps  xmm5, xmm5, 85                          
        shufps  xmm6, xmm6, 85
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        cvtps2pi mm1, xmm3                              
        cvtps2pi mm2, xmm4                              
        movhlps xmm3, xmm3                              
        movhlps xmm4, xmm4                              
        cvtps2pi mm3, xmm3                              
        cvtps2pi mm4, xmm4                              
        packssdw mm1, mm3                               
        packssdw mm2, mm4
        packuswb mm1, mm2                               
        movq    qword ptr [edx], mm1                    
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm1                              
        movaps  xmm6, xmm2
        shufps  xmm4, xmm4, -86                         
        shufps  xmm5, xmm5, -86                         
        shufps  xmm6, xmm6, -86                         
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]         
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        shufps  xmm0, xmm0, -1                          
        shufps  xmm1, xmm1, -1                          
        shufps  xmm2, xmm2, -1                          
        mulps   xmm1, dqword ptr [ebp+TBLOffset+16]         
        mulps   xmm2, xmm7                              
        addps   xmm0, xmm1                              
        addps   xmm0, xmm2                              
        cvtps2pi mm1, xmm4                              
        cvtps2pi mm2, xmm0                              
        movhlps xmm4, xmm4                              
        movhlps xmm0, xmm0
        cvtps2pi mm3, xmm4                              
        cvtps2pi mm4, xmm0
        packssdw mm1, mm3                               
        packssdw mm2, mm4                               
        packuswb mm1, mm2                               
        movq    qword ptr [edx + 08H], mm1              
        movaps  xmm6, dqword ptr [ebp+TBLOffset+16]         
        movaps  xmm0, dqword ptr [edi + 10H]           
        movaps  xmm1, dqword ptr [ebx + 10H]           
        movaps  xmm2, dqword ptr [ecx + 10H]           
        addps   xmm0, dqword ptr [ebp + 000000E0H]     
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        movaps  xmm5, xmm2                              
        shufps  xmm3, xmm3, 0                           
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, 0                           
        mulps   xmm4, xmm6                              
        mulps   xmm5, xmm7                              
        addps   xmm3, xmm4
        addps   xmm3, xmm5                              
        movaps  xmm4, xmm0
        movaps  xmm5, xmm1                              
        movaps  xmm6, xmm2                              
        shufps  xmm4, xmm4, 85                          
        shufps  xmm5, xmm5, 85                          
        shufps  xmm6, xmm6, 85                          
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]         
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        cvtps2pi mm1, xmm3                              
        cvtps2pi mm2, xmm4
        movhlps xmm3, xmm3                              
        movhlps xmm4, xmm4                              
        cvtps2pi mm3, xmm3                              
        cvtps2pi mm4, xmm4                              
        packssdw mm1, mm3                               
        packssdw mm2, mm4                               
        packuswb mm1, mm2                               
        movq    qword ptr [edx + 10H], mm1              
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm1                              
        movaps  xmm6, xmm2                              
        shufps  xmm4, xmm4, -86                         
        shufps  xmm5, xmm5, -86                         
        shufps  xmm6, xmm6, -86                         
        mulps   xmm5, dqword ptr [ebp+TBLOffset+16]         
        mulps   xmm6, xmm7                              
        addps   xmm4, xmm5                              
        addps   xmm4, xmm6                              
        shufps  xmm0, xmm0, -1                          
        shufps  xmm1, xmm1, -1                          
        shufps  xmm2, xmm2, -1                          
        mulps   xmm1, dqword ptr [ebp+TBLOffset+16]
        mulps   xmm2, xmm7                              
        addps   xmm0, xmm1                              
        addps   xmm0, xmm2                              
        cvtps2pi mm1, xmm4                              
        cvtps2pi mm2, xmm0
        movhlps xmm4, xmm4
        movhlps xmm0, xmm0                              
        cvtps2pi mm3, xmm4
        cvtps2pi mm4, xmm0                              
        packssdw mm1, mm3                               
        packssdw mm2, mm4                               
        packuswb mm1, mm2
        movq    qword ptr [edx + 18H], mm1              
        add     edx, dword ptr [eax + esi + 0CH]        
        add     eax, 16                                 
        jne     @@013                                   
        dec     dword ptr [ebp + 000000D0H]             
        jz      @@015                                   
@@014:  mov     dword ptr [ebp + 000000C8H], edx        
        pop     ebx
        pop     edi                                     
        pop     esi                                     
        ret                                             

@@015:  add     edx, dword ptr [ebp + 000000D8H]        
        mov     eax, dword ptr [ebp + 000000CCH]        
        mov     dword ptr [ebp + 000000D0H], eax        
        jmp     @@014
end;


procedure _RGB_Init;
asm
        mov     ecx, dword ptr [@TBL_RGBConv - 00000008H + eax*4]
        mov     dword ptr [ebp + 00000100H], ecx
        ret
        nop; nop
@TBL_RGBConv:
        dd offset @TBL_RGBConv_SSE
        dd offset @TBL_RGBConv_SSE2
        dd offset @TBL_RGBConv_SSE2
@TBL_RGBConv_SSE2:
        dd offset RGB_GrayConv_SSE2
        dd 00000000H
        dd offset RGB_YCbCrConv_SSE2
@TBL_RGBConv_SSE:
        dd offset RGB_GrayConv_SSE
        dd 00000000H
        dd offset RGB_YCbCrConv_SSE
end;


procedure _RGB_GenPointer;
asm
        sub     esp, 56
        mov     dword ptr [esp + 24H], edi
        mov     dword ptr [esp + 28H], ebx
        shr     edx, 2
        movzx   eax, byte ptr [@TBL_C0 + edx]
        mov     dword ptr [esp], eax
        movzx   ecx, byte ptr [@TBL_C1 + edx]
        mov     dword ptr [esp + 04H], ecx
        imul    eax, ecx
        mov     dword ptr [esp + 08H], eax              
        mov     ecx, dword ptr [ebp + 08H]
        shl     ecx, 2
        mov     eax, dword ptr [ebp + 000000B0H]
        mov     dword ptr [esp + 2CH], eax
        dec     eax                                     
        shl     eax, 5
        neg     eax                                     
        add     eax, ecx                                
        mov     dword ptr [esp + 0CH], eax
        mov     eax, dword ptr [ebp + 000000B4H]
        mov     dword ptr [esp + 30H], eax
        mov     eax, 32
        sub     eax, dword ptr [ebp + 000000D8H]        
        mov     dword ptr [esp + 10H], eax              
        mov     eax, dword ptr [ebp + 14H]              
        lea     eax, [eax*4 + 00000004H]                
        mov     dword ptr [esp + 34H], eax              
        cmp     dword ptr [esp + 3CH], 1                
        jz      @@001                                   
        mov     eax, dword ptr [esi]
        mov     dword ptr [esp + 2CH], eax              
        mov     eax, dword ptr [esi + 04H]              
        mov     dword ptr [esp + 30H], eax              
@@001:  mov     eax, dword ptr [esp + 30H]              
        mov     dword ptr [esp + 14H], eax              
@@002:  mov     eax, dword ptr [esp]                    
        mov     dword ptr [esp + 18H], eax              
@@003:  mov     dword ptr [esp + 1CH], ebx              
        mov     edx, dword ptr [esp + 2CH]              
@@004:  mov     ecx, dword ptr [esp + 04H]              
        mov     eax, ebx                                
@@005:  mov     dword ptr [edi], 32                     
        cmp     dword ptr [esp + 3CH], 1                
        jz      @@006                                   
        mov     dword ptr [edi], eax                    
        add     eax, 32                                 
@@006:  add     edi, dword ptr [esp + 34H]              
        sub     ecx, 32
        jnz     @@005                                   
        add     ebx, dword ptr [esp + 08H]              
        dec     edx
        jnz     @@004                                   
        mov     eax, dword ptr [esp]                    
        cmp     dword ptr [esp + 18H], eax              
        jnz     @@007                                   
        mov     dword ptr [esp + 20H], ebx              
@@007:  mov     eax, dword ptr [esp + 04H]              
        add     eax, dword ptr [esp + 1CH]              
        mov     ebx, eax                                
        cmp     dword ptr [esp + 3CH], 1
        jnz     @@008
        mov     ecx, dword ptr [esp + 34H]
        neg     ecx
        mov     eax, dword ptr [esp + 0CH]
        mov     dword ptr [ecx + edi], eax
@@008:  dec     dword ptr [esp + 18H]
        jnz     @@003
        mov     ebx, dword ptr [esp + 20H]
        dec     dword ptr [esp + 14H]
        jnz     @@002
        cmp     dword ptr [esp + 3CH], 1
        jnz     @@009
        mov     ecx, dword ptr [esp + 34H]
        neg     ecx
        mov     eax, dword ptr [esp + 10H]
        mov     dword ptr [ecx + edi], eax
@@009:  mov     edi, dword ptr [esp + 24H]
        mov     ebx, dword ptr [esp + 28H]
        add     esp, 56
        ret     4
        nop; nop; nop // dword align
@TBL_C0:db 10H, 08H, 10H, 08H
@TBL_C1:db 20H, 40H, 40H, 20H, 00H, 00H, 00H, 00H
end;


procedure _IDCT_8x8_SSE2;
asm
        mov     ecx, -32
@@001:  mov     edx, dword ptr [ebp+TBLOffset+0B0H + 00000020H + ecx]
        cvtdq2ps xmm0, dqword ptr [esp + ecx*8 + 00000100H]
        cvtdq2ps xmm1, dqword ptr [esp + ecx*8 + 00000110H]
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        pshufd  xmm2, xmm0, 0
        pshufd  xmm3, xmm0, -86
        pshufd  xmm4, xmm1, 0
        pshufd  xmm5, xmm1, -86
        mulps   xmm2, dqword ptr [edx]
        mulps   xmm3, dqword ptr [edx + 10H]
        mulps   xmm4, dqword ptr [edx + 20H]
        mulps   xmm5, dqword ptr [edx + 30H]
        addps   xmm2, xmm3
        addps   xmm4, xmm5
        pshufd  xmm3, xmm0, 85
        addps   xmm2, xmm4
        pshufd  xmm0, xmm0, -1
        pshufd  xmm4, xmm1, 85
        pshufd  xmm1, xmm1, -1
        mulps   xmm3, dqword ptr [edx + 40H]
        mulps   xmm0, dqword ptr [edx + 50H]
        mulps   xmm4, dqword ptr [edx + 60H]
        mulps   xmm1, dqword ptr [edx + 70H]
        addps   xmm3, xmm0
        addps   xmm4, xmm1
        addps   xmm3, xmm4
        movaps  xmm4, xmm2
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [edi + ecx*8 + 00000100H], xmm2
        movaps  dqword ptr [edi + ecx*8 + 00000110H], xmm4
        add     ecx, 4
        jne     @@001                                   
        mov     ecx, -32                                
@@002:  movaps  xmm0, dqword ptr [ecx + edi + 40H]     
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, xmm0
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]
        movaps  xmm3, xmm1                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+020H]         
        subps   xmm0, xmm3                              
        addps   xmm2, xmm1                              
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm4, xmm1                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm5, xmm3
        mulps   xmm3, dqword ptr [ebp+TBLOffset+040H]         
        subps   xmm5, xmm1
        addps   xmm3, xmm4                              
        movaps  xmm1, xmm2                              
        movaps  xmm4, xmm0                              
        addps   xmm2, xmm3                              
        subps   xmm1, xmm3
        addps   xmm0, xmm5
        subps   xmm4, xmm5                              
        movaps  xmm3, xmm4                              
        addps   xmm4, xmm1                              
        subps   xmm1, xmm3                              
        mulps   xmm4, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm1, dqword ptr [ebp+TBLOffset+0A0H]        
        movaps  dqword ptr [ecx + edi + 40H], xmm4     
        movaps  dqword ptr [ecx + edi + 00000080H], xmm0
        movaps  xmm5, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm3, dqword ptr [ecx + edi + 20H]     
        movaps  xmm6, xmm5
        addps   xmm5, xmm3                              
        subps   xmm3, xmm6                              
        movaps  xmm0, dqword ptr [ecx + edi + 60H]     
        movaps  xmm4, dqword ptr [ecx + edi + 000000E0H]
        movaps  xmm6, xmm0                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+030H]         
        movaps  xmm7, xmm4                              
        mulps   xmm4, dqword ptr [ebp+TBLOffset+030H]         
        subps   xmm0, xmm7                              
        addps   xmm4, xmm6                              
        movaps  xmm6, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm3, xmm6                              
        movaps  xmm6, xmm4                              
        addps   xmm4, xmm5                              
        subps   xmm5, xmm6
        movaps  xmm6, xmm2
        addps   xmm2, xmm4                              
        subps   xmm4, xmm6                              
        movaps  xmm7, xmm1                              
        addps   xmm1, xmm3                              
        subps   xmm3, xmm7                              
        movaps  xmm6, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm7, xmm5                              
        subps   xmm5, xmm6                              
        addps   xmm7, xmm6                              
        movaps  dqword ptr [ecx + edi + 20H], xmm2     
        movaps  dqword ptr [ecx + edi + 00000100H], xmm4
        movaps  dqword ptr [ecx + edi + 60H], xmm1     
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm3
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000080H], xmm7
        movaps  xmm2, dqword ptr [ecx + edi + 40H]     
        movaps  xmm3, xmm0                              
        addps   xmm0, xmm2                              
        subps   xmm3, xmm2                              
        movaps  dqword ptr [ecx + edi + 40H], xmm0     
        movaps  dqword ptr [ecx + edi + 000000E0H], xmm3
        add     ecx, 16                                 
        jne     @@002                                   
        add     edi, 256                                
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_16x16_SSE2;
asm
        movd    mm1, ebp
        mov     ecx, -32
        mov     ebp, -512
@@003:  //mov     edx, dword ptr [ebp+TBLOffset+0D0H + 00000020H + ecx]
        movd edx,mm1
        mov edx,[edx+TBLOffset+0D0H + 00000020H + ecx]
        cvtdq2ps xmm0, dqword ptr [esp + ecx*8 + 00000100H]
        cvtdq2ps xmm1, dqword ptr [esp + ecx*8 + 00000110H]
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        pshufd  xmm2, xmm0, 0
        pshufd  xmm3, xmm0, -86
        pshufd  xmm4, xmm1, 0
        pshufd  xmm5, xmm1, -86
        movaps  xmm6, xmm0
        movaps  xmm7, xmm1
        mulps   xmm2, dqword ptr [edx]
        mulps   xmm3, dqword ptr [edx + 10H]
        mulps   xmm4, dqword ptr [edx + 20H]
        mulps   xmm5, dqword ptr [edx + 30H]
        addps   xmm2, xmm3
        addps   xmm4, xmm5
        pshufd  xmm3, xmm0, 85
        addps   xmm2, xmm4
        pshufd  xmm0, xmm0, -1
        pshufd  xmm4, xmm1, 85
        pshufd  xmm1, xmm1, -1
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [ebp + edi + 00000200H], xmm2
        movaps  dqword ptr [ebp + edi + 00000230H], xmm4
        pshufd  xmm2, xmm6, 0                           
        pshufd  xmm3, xmm6, -86                         
        pshufd  xmm4, xmm7, 0                           
        pshufd  xmm5, xmm7, -86                         
        mulps   xmm2, dqword ptr [edx + 00000080H]     
        mulps   xmm3, dqword ptr [edx + 00000090H]     
        mulps   xmm4, dqword ptr [edx + 000000A0H]
        mulps   xmm5, dqword ptr [edx + 000000B0H]     
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        pshufd  xmm3, xmm6, 85                          
        addps   xmm2, xmm4                              
        pshufd  xmm6, xmm6, -1                          
        pshufd  xmm4, xmm7, 85                          
        pshufd  xmm7, xmm7, -1                          
        mulps   xmm3, dqword ptr [edx + 000000C0H]     
        mulps   xmm6, dqword ptr [edx + 000000D0H]
        mulps   xmm4, dqword ptr [edx + 000000E0H]     
        mulps   xmm7, dqword ptr [edx + 000000F0H]     
        addps   xmm3, xmm6                              
        addps   xmm4, xmm7
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [ebp + edi + 00000210H], xmm2
        movaps  dqword ptr [ebp + edi + 00000220H], xmm4
        add     ebp, 64
        add     ecx, 4
        jne     @@003
        mov     ecx, -64
        movd    ebp, mm1
@@004:  movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm2, xmm3                              
        addps   xmm4, xmm1                              
        subps   xmm5, xmm1
        movaps  dqword ptr [ecx + edi + 40H], xmm0
        movaps  xmm0, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm1, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm3, xmm0                              
        movaps  xmm6, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]         
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm6, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm7, xmm0                              
        mulps   xmm1, xmm0                              
        movaps  xmm0, xmm4                              
        addps   xmm4, xmm7                              
        subps   xmm0, xmm7                              
        movaps  xmm7, xmm5                              
        addps   xmm5, xmm1                              
        subps   xmm7, xmm1                              
        movaps  xmm1, xmm2                              
        addps   xmm2, xmm6                              
        subps   xmm1, xmm6
        movaps  dqword ptr [ecx + edi + 00000280H], xmm0
        movaps  dqword ptr [ecx + edi + 000002C0H], xmm7
        movaps  dqword ptr [ecx + edi + 00000300H], xmm1
        movaps  dqword ptr [ecx + edi + 00000340H], xmm2
        movaps  dqword ptr [ecx + edi + 00000380H], xmm5
        movaps  dqword ptr [ecx + edi + 000003C0H], xmm4
        movaps  xmm0, dqword ptr [ecx + edi + 40H]     
        movaps  xmm1, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm1, xmm3
        movaps  dqword ptr [ecx + edi + 00000400H], xmm0
        movaps  dqword ptr [ecx + edi + 00000240H], xmm1
        movaps  xmm0, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+050H]
        mulps   xmm1, dqword ptr [ebp+TBLOffset+080H]         
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3
        movaps  xmm5, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 40H], xmm2
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm3, xmm1
        movaps  xmm6, xmm2                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+060H]         
        mulps   xmm2, dqword ptr [ebp+TBLOffset+070H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm2                              
        subps   xmm6, xmm2                              
        movaps  xmm2, xmm4                              
        addps   xmm4, xmm1                              
        subps   xmm2, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm0, xmm6                              
        movaps  xmm6, xmm3                              
        addps   xmm3, dqword ptr [ecx + edi + 40H]     
        subps   xmm6, dqword ptr [ecx + edi + 40H]     
        movaps  dqword ptr [ecx + edi + 40H], xmm3     
        movaps  xmm3, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm3, xmm6                              
        mulps   xmm5, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm3, dqword ptr [ebp+TBLOffset+0A0H]
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000100H], xmm3
        movaps  xmm3, dqword ptr [ebp+TBLOffset+030H]         
        movaps  xmm5, xmm4                              
        mulps   xmm4, xmm3                              
        movaps  xmm6, xmm7                              
        mulps   xmm7, xmm3                              
        subps   xmm4, xmm6                              
        addps   xmm5, xmm7                              
        movaps  xmm6, xmm1                              
        mulps   xmm1, xmm3
        movaps  xmm7, xmm2                              
        mulps   xmm2, xmm3                              
        addps   xmm7, xmm1                              
        movaps  xmm3, dqword ptr [ebp+TBLOffset+090H]        
        subps   xmm2, xmm6
        mulps   xmm5, xmm3
        mulps   xmm7, xmm3                              
        mulps   xmm2, xmm3                              
        mulps   xmm4, xmm3                              
        movaps  xmm1, dqword ptr [ecx + edi + 40H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000400H]
        movaps  xmm6, xmm3                              
        addps   xmm3, xmm1                              
        subps   xmm6, xmm1                              
        movaps  dqword ptr [ecx + edi + 40H], xmm3
        movaps  dqword ptr [ecx + edi + 00000400H], xmm6
        movaps  xmm1, dqword ptr [ecx + edi + 00000380H]
        movaps  xmm3, dqword ptr [ecx + edi + 000003C0H]
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm7                              
        subps   xmm6, xmm7                              
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm5                              
        subps   xmm7, xmm5
        movaps  dqword ptr [ecx + edi + 00000080H], xmm3
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm1
        movaps  dqword ptr [ecx + edi + 00000380H], xmm6
        movaps  dqword ptr [ecx + edi + 000003C0H], xmm7
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000300H]
        movaps  xmm6, dqword ptr [ecx + edi + 00000340H]
        movaps  xmm7, xmm5                              
        subps   xmm5, xmm3                              
        addps   xmm7, xmm3                              
        movaps  xmm3, xmm6                              
        addps   xmm6, xmm1                              
        subps   xmm3, xmm1
        movaps  dqword ptr [ecx + edi + 00000100H], xmm6
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000300H], xmm7
        movaps  dqword ptr [ecx + edi + 00000340H], xmm3
        movaps  xmm1, dqword ptr [ecx + edi + 00000240H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000280H]
        movaps  xmm5, dqword ptr [ecx + edi + 000002C0H]
        movaps  xmm6, xmm3                              
        addps   xmm3, xmm4                              
        subps   xmm6, xmm4                              
        movaps  xmm4, xmm5                              
        addps   xmm5, xmm2
        subps   xmm4, xmm2
        movaps  xmm7, xmm1
        addps   xmm1, xmm0
        subps   xmm7, xmm0
        movaps  dqword ptr [ecx + edi + 00000180H], xmm5
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm3
        movaps  dqword ptr [ecx + edi + 00000200H], xmm1
        movaps  dqword ptr [ecx + edi + 00000240H], xmm7
        movaps  dqword ptr [ecx + edi + 00000280H], xmm6
        movaps  dqword ptr [ecx + edi + 000002C0H], xmm4
        add     ecx, 16
        jne     @@004
        add     edi, 1024
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_8x16_SSE2;
asm
        mov     ecx, -32
@@005:  mov     edx, dword ptr [ebp+TBLOffset+0B0H + 00000020H + ecx]
        cvtdq2ps xmm0, dqword ptr [esp + ecx*8 + 00000100H]
        cvtdq2ps xmm1, dqword ptr [esp + ecx*8 + 00000110H]
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        pshufd  xmm2, xmm0, 0                           
        pshufd  xmm3, xmm0, -86                         
        pshufd  xmm4, xmm1, 0                           
        pshufd  xmm5, xmm1, -86                         
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        pshufd  xmm3, xmm0, 85                          
        addps   xmm2, xmm4                              
        pshufd  xmm0, xmm0, -1                          
        pshufd  xmm4, xmm1, 85                          
        pshufd  xmm1, xmm1, -1                          
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [edi + ecx*8 + 00000100H], xmm2
        movaps  dqword ptr [edi + ecx*8 + 00000110H], xmm4
        add     ecx, 4                                  
        jne     @@005                                   
        mov     ecx, -32                                
@@006:  movaps  xmm0, dqword ptr [ecx + edi + 20H]
        movaps  xmm1, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm2, xmm0                              
        movaps  xmm3, xmm1                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+030H]         
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm0
        addps   xmm0, xmm3
        subps   xmm2, xmm3                              
        addps   xmm4, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm0     
        movaps  xmm0, dqword ptr [ecx + edi + 60H]
        movaps  xmm1, dqword ptr [ecx + edi + 000000E0H]
        movaps  xmm3, xmm0                              
        movaps  xmm6, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]         
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm0
        addps   xmm0, xmm1                              
        subps   xmm6, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm7, xmm0                              
        mulps   xmm1, xmm0                              
        movaps  xmm0, xmm4                              
        addps   xmm4, xmm7                              
        subps   xmm0, xmm7                              
        movaps  xmm7, xmm5                              
        addps   xmm5, xmm1                              
        subps   xmm7, xmm1                              
        movaps  xmm1, xmm2                              
        addps   xmm2, xmm6                              
        subps   xmm1, xmm6
        movaps  dqword ptr [ecx + edi + 00000140H], xmm0
        movaps  dqword ptr [ecx + edi + 00000160H], xmm7
        movaps  dqword ptr [ecx + edi + 00000180H], xmm1
        movaps  dqword ptr [ecx + edi + 000001A0H], xmm2
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm5
        movaps  dqword ptr [ecx + edi + 000001E0H], xmm4
        movaps  xmm0, dqword ptr [ecx + edi + 20H]
        movaps  xmm1, xmm0
        addps   xmm0, xmm3                              
        subps   xmm1, xmm3
        movaps  dqword ptr [ecx + edi + 00000200H], xmm0
        movaps  dqword ptr [ecx + edi + 00000120H], xmm1
        movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, xmm0                              
        movaps  xmm3, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+050H]         
        mulps   xmm1, dqword ptr [ebp+TBLOffset+080H]         
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        movaps  xmm5, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm2
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm2, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm3, xmm1                              
        movaps  xmm6, xmm2                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+060H]         
        mulps   xmm2, dqword ptr [ebp+TBLOffset+070H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm2                              
        subps   xmm6, xmm2                              
        movaps  xmm2, xmm4                              
        addps   xmm4, xmm1                              
        subps   xmm2, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, xmm5
        addps   xmm5, xmm6                              
        subps   xmm0, xmm6                              
        movaps  xmm6, xmm3                              
        addps   xmm3, dqword ptr [ecx + edi + 20H]     
        subps   xmm6, dqword ptr [ecx + edi + 20H]     
        movaps  dqword ptr [ecx + edi + 20H], xmm3     
        movaps  xmm3, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm3, xmm6                              
        mulps   xmm5, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm3, dqword ptr [ebp+TBLOffset+0A0H]
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000080H], xmm3
        movaps  xmm3, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm5, xmm4
        mulps   xmm4, xmm3                              
        movaps  xmm6, xmm7                              
        mulps   xmm7, xmm3                              
        subps   xmm4, xmm6                              
        addps   xmm5, xmm7                              
        movaps  xmm6, xmm1                              
        mulps   xmm1, xmm3                              
        movaps  xmm7, xmm2                              
        mulps   xmm2, xmm3                              
        addps   xmm7, xmm1                              
        movaps  xmm3, dqword ptr [ebp+TBLOffset+090H]        
        subps   xmm2, xmm6                              
        mulps   xmm5, xmm3                              
        mulps   xmm7, xmm3                              
        mulps   xmm2, xmm3                              
        mulps   xmm4, xmm3                              
        movaps  xmm1, dqword ptr [ecx + edi + 20H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm6, xmm3                              
        addps   xmm3, xmm1                              
        subps   xmm6, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm3
        movaps  dqword ptr [ecx + edi + 00000200H], xmm6
        movaps  xmm1, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm3, dqword ptr [ecx + edi + 000001E0H]
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm7                              
        subps   xmm6, xmm7                              
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm5                              
        subps   xmm7, xmm5                              
        movaps  dqword ptr [ecx + edi + 40H], xmm3     
        movaps  dqword ptr [ecx + edi + 60H], xmm1
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm6
        movaps  dqword ptr [ecx + edi + 000001E0H], xmm7
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm6, dqword ptr [ecx + edi + 000001A0H]
        movaps  xmm7, xmm5                              
        subps   xmm5, xmm3                              
        addps   xmm7, xmm3                              
        movaps  xmm3, xmm6                              
        addps   xmm6, xmm1                              
        subps   xmm3, xmm1
        movaps  dqword ptr [ecx + edi + 00000080H], xmm6
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000180H], xmm7
        movaps  dqword ptr [ecx + edi + 000001A0H], xmm3
        movaps  xmm1, dqword ptr [ecx + edi + 00000120H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000160H]
        movaps  xmm6, xmm3                              
        addps   xmm3, xmm4                              
        subps   xmm6, xmm4                              
        movaps  xmm4, xmm5                              
        addps   xmm5, xmm2                              
        subps   xmm4, xmm2                              
        movaps  xmm7, xmm1                              
        addps   xmm1, xmm0                              
        subps   xmm7, xmm0
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm5
        movaps  dqword ptr [ecx + edi + 000000E0H], xmm3
        movaps  dqword ptr [ecx + edi + 00000100H], xmm1
        movaps  dqword ptr [ecx + edi + 00000120H], xmm7
        movaps  dqword ptr [ecx + edi + 00000140H], xmm6
        movaps  dqword ptr [ecx + edi + 00000160H], xmm4
        add     ecx, 16                                 
        jne     @@006                                   
        add     edi, 512                                
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_16x8_SSE2;
asm
        movd    mm1, ebp
        mov     ecx, -32
        mov     ebp, -512
@@007:  //mov     edx, dword ptr [ebp+TBLOffset+0D0H + 00000020H + ecx]
        movd edx,mm1
        mov edx,[edx+TBLOffset+0D0H + 00000020H + ecx]
        cvtdq2ps xmm0, dqword ptr [esp + ecx*8 + 00000100H]
        cvtdq2ps xmm1, dqword ptr [esp + ecx*8 + 00000110H]
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        pshufd  xmm2, xmm0, 0                           
        pshufd  xmm3, xmm0, -86                         
        pshufd  xmm4, xmm1, 0                           
        pshufd  xmm5, xmm1, -86                         
        movaps  xmm6, xmm0                              
        movaps  xmm7, xmm1                              
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        pshufd  xmm3, xmm0, 85                          
        addps   xmm2, xmm4                              
        pshufd  xmm0, xmm0, -1                          
        pshufd  xmm4, xmm1, 85                          
        pshufd  xmm1, xmm1, -1                          
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1
        addps   xmm3, xmm4
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [ebp + edi + 00000200H], xmm2
        movaps  dqword ptr [ebp + edi + 00000230H], xmm4
        pshufd  xmm2, xmm6, 0                           
        pshufd  xmm3, xmm6, -86                         
        pshufd  xmm4, xmm7, 0                           
        pshufd  xmm5, xmm7, -86                         
        mulps   xmm2, dqword ptr [edx + 00000080H]     
        mulps   xmm3, dqword ptr [edx + 00000090H]     
        mulps   xmm4, dqword ptr [edx + 000000A0H]     
        mulps   xmm5, dqword ptr [edx + 000000B0H]     
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        pshufd  xmm3, xmm6, 85                          
        addps   xmm2, xmm4                              
        pshufd  xmm6, xmm6, -1                          
        pshufd  xmm4, xmm7, 85                          
        pshufd  xmm7, xmm7, -1                          
        mulps   xmm3, dqword ptr [edx + 000000C0H]     
        mulps   xmm6, dqword ptr [edx + 000000D0H]     
        mulps   xmm4, dqword ptr [edx + 000000E0H]     
        mulps   xmm7, dqword ptr [edx + 000000F0H]     
        addps   xmm3, xmm6                              
        addps   xmm4, xmm7                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [ebp + edi + 00000210H], xmm2
        movaps  dqword ptr [ebp + edi + 00000220H], xmm4
        add     ebp, 64
        add     ecx, 4
        jne     @@007
        mov     ecx, -64
        movd ebp,mm1
@@008:  movaps  xmm0, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm2, xmm0
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]
        movaps  xmm3, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+020H]
        subps   xmm0, xmm3                              
        addps   xmm2, xmm1
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm4, xmm1                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm5, xmm3                              
        mulps   xmm3, dqword ptr [ebp+TBLOffset+040H]         
        subps   xmm5, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm1, xmm2                              
        movaps  xmm4, xmm0                              
        addps   xmm2, xmm3                              
        subps   xmm1, xmm3                              
        addps   xmm0, xmm5                              
        subps   xmm4, xmm5                              
        movaps  xmm3, xmm4
        addps   xmm4, xmm1                              
        subps   xmm1, xmm3                              
        mulps   xmm4, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm1, dqword ptr [ebp+TBLOffset+0A0H]
        movaps  dqword ptr [ecx + edi + 00000080H], xmm4
        movaps  dqword ptr [ecx + edi + 00000100H], xmm0
        movaps  xmm5, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm3, dqword ptr [ecx + edi + 40H]     
        movaps  xmm6, xmm5                              
        addps   xmm5, xmm3                              
        subps   xmm3, xmm6
        movaps  xmm0, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm4, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm6, xmm0                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm7, xmm4
        mulps   xmm4, dqword ptr [ebp+TBLOffset+030H]
        subps   xmm0, xmm7                              
        addps   xmm4, xmm6                              
        movaps  xmm6, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm3, xmm6                              
        movaps  xmm6, xmm4                              
        addps   xmm4, xmm5                              
        subps   xmm5, xmm6                              
        movaps  xmm6, xmm2                              
        addps   xmm2, xmm4                              
        subps   xmm4, xmm6                              
        movaps  xmm7, xmm1                              
        addps   xmm1, xmm3                              
        subps   xmm3, xmm7
        movaps  xmm6, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm7, xmm5                              
        subps   xmm5, xmm6                              
        addps   xmm7, xmm6                              
        movaps  dqword ptr [ecx + edi + 40H], xmm2
        movaps  dqword ptr [ecx + edi + 00000200H], xmm4
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm1
        movaps  dqword ptr [ecx + edi + 00000180H], xmm3
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000100H], xmm7
        movaps  xmm2, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, xmm0                              
        addps   xmm0, xmm2                              
        subps   xmm3, xmm2
        movaps  dqword ptr [ecx + edi + 00000080H], xmm0
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm3
        add     ecx, 16                                 
        jne     @@008                                   
        add     edi, 512
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_8x8_SSE;
asm
        mov     ecx, -32
@@009:  mov     edx, dword ptr [ebp+TBLOffset+0B0H + 00000020H + ecx]
        cvtpi2ps xmm0, qword ptr [esp + ecx*8 + 00000100H]
        cvtpi2ps xmm2, qword ptr [esp + ecx*8 + 00000108H]
        cvtpi2ps xmm1, qword ptr [esp + ecx*8 + 00000110H]
        cvtpi2ps xmm3, qword ptr [esp + ecx*8 + 00000118H]
        shufps  xmm0, xmm2, 68                          
        shufps  xmm1, xmm3, 68
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        movaps  xmm2, xmm0                             
        movaps  xmm3, xmm0                             
        movaps  xmm4, xmm1                             
        movaps  xmm5, xmm1                             
        shufps  xmm2, xmm2, 0                          
        shufps  xmm3, xmm3, -86                        
        shufps  xmm4, xmm4, 0                          
        shufps  xmm5, xmm5, -86                        
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                             
        addps   xmm4, xmm5                             
        addps   xmm2, xmm4                             
        movaps  xmm3, xmm0                             
        movaps  xmm4, xmm1                             
        shufps  xmm3, xmm3, 85                         
        shufps  xmm0, xmm0, -1                         
        shufps  xmm4, xmm4, 85                         
        shufps  xmm1, xmm1, -1
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                             
        addps   xmm4, xmm1                             
        addps   xmm3, xmm4                             
        movaps  xmm4, xmm2                             
        addps   xmm2, xmm3                             
        subps   xmm4, xmm3                             
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [edi + ecx*8 + 00000100H], xmm2
        movaps  dqword ptr [edi + ecx*8 + 00000110H], xmm4
        add     ecx, 4                                  
        jne     @@009                                   
        mov     ecx, -32                                
@@010:  movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, xmm0
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]
        movaps  xmm3, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+020H]
        subps   xmm0, xmm3
        addps   xmm2, xmm1
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm4, xmm1                             
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm5, xmm3                             
        mulps   xmm3, dqword ptr [ebp+TBLOffset+040H]         
        subps   xmm5, xmm1                             
        addps   xmm3, xmm4                             
        movaps  xmm1, xmm2                             
        movaps  xmm4, xmm0                             
        addps   xmm2, xmm3
        subps   xmm1, xmm3                             
        addps   xmm0, xmm5                             
        subps   xmm4, xmm5                             
        movaps  xmm3, xmm4                             
        addps   xmm4, xmm1                             
        subps   xmm1, xmm3                             
        mulps   xmm4, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm1, dqword ptr [ebp+TBLOffset+0A0H]        
        movaps  dqword ptr [ecx + edi + 40H], xmm4
        movaps  dqword ptr [ecx + edi + 00000080H], xmm0
        movaps  xmm5, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm3, dqword ptr [ecx + edi + 20H]     
        movaps  xmm6, xmm5                             
        addps   xmm5, xmm3                             
        subps   xmm3, xmm6                             
        movaps  xmm0, dqword ptr [ecx + edi + 60H]
        movaps  xmm4, dqword ptr [ecx + edi + 000000E0H]
        movaps  xmm6, xmm0                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm7, xmm4
        mulps   xmm4, dqword ptr [ebp+TBLOffset+030H]
        subps   xmm0, xmm7
        addps   xmm4, xmm6
        movaps  xmm6, xmm0
        addps   xmm0, xmm3
        subps   xmm3, xmm6
        movaps  xmm6, xmm4
        addps   xmm4, xmm5
        subps   xmm5, xmm6
        movaps  xmm6, xmm2
        addps   xmm2, xmm4
        subps   xmm4, xmm6
        movaps  xmm7, xmm1
        addps   xmm1, xmm3
        subps   xmm3, xmm7
        movaps  xmm6, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm7, xmm5
        subps   xmm5, xmm6
        addps   xmm7, xmm6
        movaps  dqword ptr [ecx + edi + 20H], xmm2
        movaps  dqword ptr [ecx + edi + 00000100H], xmm4
        movaps  dqword ptr [ecx + edi + 60H], xmm1
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm3
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000080H], xmm7
        movaps  xmm2, dqword ptr [ecx + edi + 40H]
        movaps  xmm3, xmm0                              
        addps   xmm0, xmm2                              
        subps   xmm3, xmm2                              
        movaps  dqword ptr [ecx + edi + 40H], xmm0
        movaps  dqword ptr [ecx + edi + 000000E0H], xmm3
        add     ecx, 16
        jne     @@010
        add     edi, 256
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_16x16_SSE;
asm
        movd    mm1, ebp
        mov     ecx, -32
        mov     ebp, -512
@@011:  //mov     edx, dword ptr [ebp+TBLOffset+0D0H + 00000020H + ecx]
        movd edx,mm1
        mov edx,[edx+TBLOffset+0D0H + 00000020H + ecx]
        cvtpi2ps xmm0, qword ptr [esp + ecx*8 + 00000100H]
        cvtpi2ps xmm2, qword ptr [esp + ecx*8 + 00000108H]
        cvtpi2ps xmm1, qword ptr [esp + ecx*8 + 00000110H]
        cvtpi2ps xmm3, qword ptr [esp + ecx*8 + 00000118H]
        shufps  xmm0, xmm2, 68                          
        shufps  xmm1, xmm3, 68
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        movaps  xmm2, xmm0                              
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        movaps  xmm5, xmm1                              
        shufps  xmm2, xmm2, 0                           
        shufps  xmm3, xmm3, -86                         
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, -86                         
        movaps  xmm6, xmm0                              
        movaps  xmm7, xmm1                              
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        addps   xmm2, xmm4                              
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        shufps  xmm3, xmm3, 85                          
        shufps  xmm0, xmm0, -1                          
        shufps  xmm4, xmm4, 85                          
        shufps  xmm1, xmm1, -1                          
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27                          
        movaps  dqword ptr [ebp + edi + 00000200H], xmm2
        movaps  dqword ptr [ebp + edi + 00000230H], xmm4
        movaps  xmm2, xmm6                              
        movaps  xmm3, xmm6                              
        movaps  xmm4, xmm7                              
        movaps  xmm5, xmm7                              
        shufps  xmm2, xmm2, 0                           
        shufps  xmm3, xmm3, -86                         
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, -86                         
        mulps   xmm2, dqword ptr [edx + 00000080H]     
        mulps   xmm3, dqword ptr [edx + 00000090H]     
        mulps   xmm4, dqword ptr [edx + 000000A0H]
        mulps   xmm5, dqword ptr [edx + 000000B0H]     
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        addps   xmm2, xmm4
        movaps  xmm3, xmm6                              
        movaps  xmm4, xmm7                              
        shufps  xmm3, xmm3, 85                          
        shufps  xmm6, xmm6, -1                          
        shufps  xmm4, xmm4, 85                          
        shufps  xmm7, xmm7, -1                          
        mulps   xmm3, dqword ptr [edx + 000000C0H]     
        mulps   xmm6, dqword ptr [edx + 000000D0H]     
        mulps   xmm4, dqword ptr [edx + 000000E0H]     
        mulps   xmm7, dqword ptr [edx + 000000F0H]     
        addps   xmm3, xmm6                              
        addps   xmm4, xmm7                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27
        movaps  dqword ptr [ebp + edi + 00000210H], xmm2
        movaps  dqword ptr [ebp + edi + 00000220H], xmm4
        add     ebp, 64
        add     ecx, 4
        jne     @@011
        mov     ecx, -64
        movd ebp,mm1
@@012:  movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm2, xmm3                              
        addps   xmm4, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 40H], xmm0     
        movaps  xmm0, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm1, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm3, xmm0
        movaps  xmm6, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]         
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm6, xmm1
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm7, xmm0                              
        mulps   xmm1, xmm0                              
        movaps  xmm0, xmm4                              
        addps   xmm4, xmm7                              
        subps   xmm0, xmm7                              
        movaps  xmm7, xmm5                              
        addps   xmm5, xmm1                              
        subps   xmm7, xmm1                              
        movaps  xmm1, xmm2                              
        addps   xmm2, xmm6
        subps   xmm1, xmm6                              
        movaps  dqword ptr [ecx + edi + 00000280H], xmm0
        movaps  dqword ptr [ecx + edi + 000002C0H], xmm7
        movaps  dqword ptr [ecx + edi + 00000300H], xmm1
        movaps  dqword ptr [ecx + edi + 00000340H], xmm2
        movaps  dqword ptr [ecx + edi + 00000380H], xmm5
        movaps  dqword ptr [ecx + edi + 000003C0H], xmm4
        movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm1, xmm3                              
        movaps  dqword ptr [ecx + edi + 00000400H], xmm0
        movaps  dqword ptr [ecx + edi + 00000240H], xmm1
        movaps  xmm0, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+050H]
        mulps   xmm1, dqword ptr [ebp+TBLOffset+080H]         
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        movaps  xmm5, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 40H], xmm2     
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm3, xmm1
        movaps  xmm6, xmm2                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+060H]         
        mulps   xmm2, dqword ptr [ebp+TBLOffset+070H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm2
        subps   xmm6, xmm2                              
        movaps  xmm2, xmm4                              
        addps   xmm4, xmm1                              
        subps   xmm2, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm0, xmm6                              
        movaps  xmm6, xmm3                              
        addps   xmm3, dqword ptr [ecx + edi + 40H]     
        subps   xmm6, dqword ptr [ecx + edi + 40H]     
        movaps  dqword ptr [ecx + edi + 40H], xmm3     
        movaps  xmm3, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm3, xmm6                              
        mulps   xmm5, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm3, dqword ptr [ebp+TBLOffset+0A0H]
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000100H], xmm3
        movaps  xmm3, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm5, xmm4                              
        mulps   xmm4, xmm3
        movaps  xmm6, xmm7                              
        mulps   xmm7, xmm3                              
        subps   xmm4, xmm6                              
        addps   xmm5, xmm7                              
        movaps  xmm6, xmm1                              
        mulps   xmm1, xmm3                              
        movaps  xmm7, xmm2                              
        mulps   xmm2, xmm3                              
        addps   xmm7, xmm1                              
        movaps  xmm3, dqword ptr [ebp+TBLOffset+090H]        
        subps   xmm2, xmm6                              
        mulps   xmm5, xmm3                              
        mulps   xmm7, xmm3
        mulps   xmm2, xmm3                              
        mulps   xmm4, xmm3                              
        movaps  xmm1, dqword ptr [ecx + edi + 40H]     
        movaps  xmm3, dqword ptr [ecx + edi + 00000400H]
        movaps  xmm6, xmm3
        addps   xmm3, xmm1                              
        subps   xmm6, xmm1                              
        movaps  dqword ptr [ecx + edi + 40H], xmm3     
        movaps  dqword ptr [ecx + edi + 00000400H], xmm6
        movaps  xmm1, dqword ptr [ecx + edi + 00000380H]
        movaps  xmm3, dqword ptr [ecx + edi + 000003C0H]
        movaps  xmm6, xmm1
        addps   xmm1, xmm7                              
        subps   xmm6, xmm7
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm5                              
        subps   xmm7, xmm5                              
        movaps  dqword ptr [ecx + edi + 00000080H], xmm3
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm1
        movaps  dqword ptr [ecx + edi + 00000380H], xmm6
        movaps  dqword ptr [ecx + edi + 000003C0H], xmm7
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000300H]
        movaps  xmm6, dqword ptr [ecx + edi + 00000340H]
        movaps  xmm7, xmm5
        subps   xmm5, xmm3                              
        addps   xmm7, xmm3                              
        movaps  xmm3, xmm6                              
        addps   xmm6, xmm1                              
        subps   xmm3, xmm1                              
        movaps  dqword ptr [ecx + edi + 00000100H], xmm6
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000300H], xmm7
        movaps  dqword ptr [ecx + edi + 00000340H], xmm3
        movaps  xmm1, dqword ptr [ecx + edi + 00000240H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000280H]
        movaps  xmm5, dqword ptr [ecx + edi + 000002C0H]
        movaps  xmm6, xmm3
        addps   xmm3, xmm4                              
        subps   xmm6, xmm4                              
        movaps  xmm4, xmm5                              
        addps   xmm5, xmm2                              
        subps   xmm4, xmm2                              
        movaps  xmm7, xmm1                              
        addps   xmm1, xmm0                              
        subps   xmm7, xmm0                              
        movaps  dqword ptr [ecx + edi + 00000180H], xmm5
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm3
        movaps  dqword ptr [ecx + edi + 00000200H], xmm1
        movaps  dqword ptr [ecx + edi + 00000240H], xmm7
        movaps  dqword ptr [ecx + edi + 00000280H], xmm6
        movaps  dqword ptr [ecx + edi + 000002C0H], xmm4
        add     ecx, 16
        jne     @@012                                  
        add     edi, 1024
        jmp     dword ptr [ebp + 000000FCH]            
end;


procedure _IDCT_8x16_SSE;
asm
        mov     ecx, -32
@@013:  mov     edx, dword ptr [ebp+TBLOffset+0B0H + 00000020H + ecx]
        cvtpi2ps xmm0, qword ptr [esp + ecx*8 + 00000100H]
        cvtpi2ps xmm2, qword ptr [esp + ecx*8 + 00000108H]
        cvtpi2ps xmm1, qword ptr [esp + ecx*8 + 00000110H]
        cvtpi2ps xmm3, qword ptr [esp + ecx*8 + 00000118H]
        shufps  xmm0, xmm2, 68
        shufps  xmm1, xmm3, 68                          
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        movaps  xmm5, xmm1                              
        shufps  xmm2, xmm2, 0                           
        shufps  xmm3, xmm3, -86                         
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, -86                         
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        addps   xmm2, xmm4                              
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        shufps  xmm3, xmm3, 85                          
        shufps  xmm0, xmm0, -1                          
        shufps  xmm4, xmm4, 85                          
        shufps  xmm1, xmm1, -1                          
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]           
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27                          
        movaps  dqword ptr [edi + ecx*8 + 00000100H], xmm2
        movaps  dqword ptr [edi + ecx*8 + 00000110H], xmm4
        add     ecx, 4
        jne     @@013                                   
        mov     ecx, -32
@@014:  movaps  xmm0, dqword ptr [ecx + edi + 20H]     
        movaps  xmm1, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+030H]         
        movaps  xmm4, xmm0                              
        movaps  xmm5, xmm0
        addps   xmm0, xmm3                              
        subps   xmm2, xmm3                              
        addps   xmm4, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm0     
        movaps  xmm0, dqword ptr [ecx + edi + 60H]     
        movaps  xmm1, dqword ptr [ecx + edi + 000000E0H]
        movaps  xmm3, xmm0
        movaps  xmm6, xmm1
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]         
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]         
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm0                              
        addps   xmm0, xmm1                              
        subps   xmm6, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm7, xmm0                              
        mulps   xmm1, xmm0                              
        movaps  xmm0, xmm4                              
        addps   xmm4, xmm7                              
        subps   xmm0, xmm7                              
        movaps  xmm7, xmm5                              
        addps   xmm5, xmm1                              
        subps   xmm7, xmm1                              
        movaps  xmm1, xmm2                              
        addps   xmm2, xmm6                              
        subps   xmm1, xmm6                              
        movaps  dqword ptr [ecx + edi + 00000140H], xmm0
        movaps  dqword ptr [ecx + edi + 00000160H], xmm7
        movaps  dqword ptr [ecx + edi + 00000180H], xmm1
        movaps  dqword ptr [ecx + edi + 000001A0H], xmm2
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm5
        movaps  dqword ptr [ecx + edi + 000001E0H], xmm4
        movaps  xmm0, dqword ptr [ecx + edi + 20H]
        movaps  xmm1, xmm0                              
        addps   xmm0, xmm3                              
        subps   xmm1, xmm3                              
        movaps  dqword ptr [ecx + edi + 00000200H], xmm0
        movaps  dqword ptr [ecx + edi + 00000120H], xmm1
        movaps  xmm0, dqword ptr [ecx + edi + 40H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm1                              
        mulps   xmm0, dqword ptr [ebp+TBLOffset+050H]
        mulps   xmm1, dqword ptr [ebp+TBLOffset+080H]
        movaps  xmm4, xmm2
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        movaps  xmm5, xmm0
        addps   xmm0, xmm1                              
        subps   xmm5, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm2     
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm2, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm3, xmm1
        movaps  xmm6, xmm2                              
        mulps   xmm1, dqword ptr [ebp+TBLOffset+060H]
        mulps   xmm2, dqword ptr [ebp+TBLOffset+070H]
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm6                              
        subps   xmm7, xmm6                              
        movaps  xmm6, xmm1                              
        addps   xmm1, xmm2                              
        subps   xmm6, xmm2                              
        movaps  xmm2, xmm4                              
        addps   xmm4, xmm1                              
        subps   xmm2, xmm1                              
        movaps  xmm1, xmm7                              
        addps   xmm7, xmm0                              
        subps   xmm1, xmm0                              
        movaps  xmm0, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm0, xmm6                              
        movaps  xmm6, xmm3                              
        addps   xmm3, dqword ptr [ecx + edi + 20H]     
        subps   xmm6, dqword ptr [ecx + edi + 20H]     
        movaps  dqword ptr [ecx + edi + 20H], xmm3     
        movaps  xmm3, xmm5                              
        addps   xmm5, xmm6                              
        subps   xmm3, xmm6
        mulps   xmm5, dqword ptr [ebp+TBLOffset+0A0H]        
        mulps   xmm3, dqword ptr [ebp+TBLOffset+0A0H]        
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000080H], xmm3
        movaps  xmm3, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm5, xmm4                              
        mulps   xmm4, xmm3                              
        movaps  xmm6, xmm7                              
        mulps   xmm7, xmm3                              
        subps   xmm4, xmm6                              
        addps   xmm5, xmm7                              
        movaps  xmm6, xmm1                              
        mulps   xmm1, xmm3                              
        movaps  xmm7, xmm2                              
        mulps   xmm2, xmm3                              
        addps   xmm7, xmm1                              
        movaps  xmm3, dqword ptr [ebp+TBLOffset+090H]
        subps   xmm2, xmm6                              
        mulps   xmm5, xmm3                              
        mulps   xmm7, xmm3                              
        mulps   xmm2, xmm3                              
        mulps   xmm4, xmm3                              
        movaps  xmm1, dqword ptr [ecx + edi + 20H]     
        movaps  xmm3, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm6, xmm3
        addps   xmm3, xmm1                              
        subps   xmm6, xmm1                              
        movaps  dqword ptr [ecx + edi + 20H], xmm3     
        movaps  dqword ptr [ecx + edi + 00000200H], xmm6
        movaps  xmm1, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm3, dqword ptr [ecx + edi + 000001E0H]
        movaps  xmm6, xmm1
        addps   xmm1, xmm7
        subps   xmm6, xmm7                              
        movaps  xmm7, xmm3                              
        addps   xmm3, xmm5                              
        subps   xmm7, xmm5                              
        movaps  dqword ptr [ecx + edi + 40H], xmm3     
        movaps  dqword ptr [ecx + edi + 60H], xmm1     
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm6
        movaps  dqword ptr [ecx + edi + 000001E0H], xmm7
        movaps  xmm1, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, dqword ptr [ecx + edi + 000000A0H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm6, dqword ptr [ecx + edi + 000001A0H]
        movaps  xmm7, xmm5
        subps   xmm5, xmm3                              
        addps   xmm7, xmm3                              
        movaps  xmm3, xmm6                              
        addps   xmm6, xmm1                              
        subps   xmm3, xmm1                              
        movaps  dqword ptr [ecx + edi + 00000080H], xmm6
        movaps  dqword ptr [ecx + edi + 000000A0H], xmm5
        movaps  dqword ptr [ecx + edi + 00000180H], xmm7
        movaps  dqword ptr [ecx + edi + 000001A0H], xmm3
        movaps  xmm1, dqword ptr [ecx + edi + 00000120H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm5, dqword ptr [ecx + edi + 00000160H]
        movaps  xmm6, xmm3
        addps   xmm3, xmm4                              
        subps   xmm6, xmm4                              
        movaps  xmm4, xmm5                              
        addps   xmm5, xmm2                              
        subps   xmm4, xmm2                              
        movaps  xmm7, xmm1                              
        addps   xmm1, xmm0                              
        subps   xmm7, xmm0                              
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm5
        movaps  dqword ptr [ecx + edi + 000000E0H], xmm3
        movaps  dqword ptr [ecx + edi + 00000100H], xmm1
        movaps  dqword ptr [ecx + edi + 00000120H], xmm7
        movaps  dqword ptr [ecx + edi + 00000140H], xmm6
        movaps  dqword ptr [ecx + edi + 00000160H], xmm4
        add     ecx, 16
        jne     @@014                                   
        add     edi, 512                                
        jmp     dword ptr [ebp + 000000FCH]             
end;


procedure _IDCT_16x8_SSE;
asm
        movd    mm1, ebp
        mov     ecx, -32                                
        mov     ebp, -512
@@015:  //mov     edx, dword ptr [ebp+TBLOffset+0D0H + 00000020H + ecx]
        movd edx,mm1
        mov edx,[edx+TBLOffset+0D0H + 00000020H + ecx]
        cvtpi2ps xmm0, qword ptr [esp + ecx*8 + 00000100H]
        cvtpi2ps xmm2, qword ptr [esp + ecx*8 + 00000108H]
        cvtpi2ps xmm1, qword ptr [esp + ecx*8 + 00000110H]
        cvtpi2ps xmm3, qword ptr [esp + ecx*8 + 00000118H]
        shufps  xmm0, xmm2, 68
        shufps  xmm1, xmm3, 68
        mulps   xmm0, dqword ptr [eax + ecx*8 + 00000100H]
        mulps   xmm1, dqword ptr [eax + ecx*8 + 00000110H]
        movaps  xmm2, xmm0
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        movaps  xmm5, xmm1                              
        shufps  xmm2, xmm2, 0                           
        shufps  xmm3, xmm3, -86                         
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, -86                         
        movaps  xmm6, xmm0                              
        movaps  xmm7, xmm1                              
        mulps   xmm2, dqword ptr [edx]                 
        mulps   xmm3, dqword ptr [edx + 10H]           
        mulps   xmm4, dqword ptr [edx + 20H]           
        mulps   xmm5, dqword ptr [edx + 30H]           
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        addps   xmm2, xmm4                              
        movaps  xmm3, xmm0                              
        movaps  xmm4, xmm1                              
        shufps  xmm3, xmm3, 85                          
        shufps  xmm0, xmm0, -1                          
        shufps  xmm4, xmm4, 85                          
        shufps  xmm1, xmm1, -1                          
        mulps   xmm3, dqword ptr [edx + 40H]           
        mulps   xmm0, dqword ptr [edx + 50H]           
        mulps   xmm4, dqword ptr [edx + 60H]
        mulps   xmm1, dqword ptr [edx + 70H]           
        addps   xmm3, xmm0                              
        addps   xmm4, xmm1                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27                          
        movaps  dqword ptr [ebp + edi + 00000200H], xmm2
        movaps  dqword ptr [ebp + edi + 00000230H], xmm4
        movaps  xmm2, xmm6
        movaps  xmm3, xmm6                              
        movaps  xmm4, xmm7                              
        movaps  xmm5, xmm7                              
        shufps  xmm2, xmm2, 0                           
        shufps  xmm3, xmm3, -86                         
        shufps  xmm4, xmm4, 0                           
        shufps  xmm5, xmm5, -86                         
        mulps   xmm2, dqword ptr [edx + 00000080H]     
        mulps   xmm3, dqword ptr [edx + 00000090H]     
        mulps   xmm4, dqword ptr [edx + 000000A0H]     
        mulps   xmm5, dqword ptr [edx + 000000B0H]
        addps   xmm2, xmm3                              
        addps   xmm4, xmm5                              
        addps   xmm2, xmm4                              
        movaps  xmm3, xmm6                              
        movaps  xmm4, xmm7                              
        shufps  xmm3, xmm3, 85                          
        shufps  xmm6, xmm6, -1                          
        shufps  xmm4, xmm4, 85                          
        shufps  xmm7, xmm7, -1                          
        mulps   xmm3, dqword ptr [edx + 000000C0H]     
        mulps   xmm6, dqword ptr [edx + 000000D0H]
        mulps   xmm4, dqword ptr [edx + 000000E0H]     
        mulps   xmm7, dqword ptr [edx + 000000F0H]     
        addps   xmm3, xmm6                              
        addps   xmm4, xmm7                              
        addps   xmm3, xmm4                              
        movaps  xmm4, xmm2                              
        addps   xmm2, xmm3                              
        subps   xmm4, xmm3                              
        shufps  xmm4, xmm4, 27                          
        movaps  dqword ptr [ebp + edi + 00000210H], xmm2
        movaps  dqword ptr [ebp + edi + 00000220H], xmm4
        add     ebp, 64
        add     ecx, 4
        jne     @@015
        mov     ecx, -64
        movd ebp,mm1
@@016:  movaps  xmm0, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm1, dqword ptr [ecx + edi + 00000200H]
        movaps  xmm2, xmm0
        mulps   xmm0, dqword ptr [ebp+TBLOffset+020H]
        movaps  xmm3, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+020H]
        subps   xmm0, xmm3
        addps   xmm2, xmm1
        movaps  xmm1, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm3, dqword ptr [ecx + edi + 00000180H]
        movaps  xmm4, xmm1
        mulps   xmm1, dqword ptr [ebp+TBLOffset+040H]
        movaps  xmm5, xmm3
        mulps   xmm3, dqword ptr [ebp+TBLOffset+040H]
        subps   xmm5, xmm1
        addps   xmm3, xmm4
        movaps  xmm1, xmm2
        movaps  xmm4, xmm0
        addps   xmm2, xmm3
        subps   xmm1, xmm3
        addps   xmm0, xmm5
        subps   xmm4, xmm5
        movaps  xmm3, xmm4
        addps   xmm4, xmm1
        subps   xmm1, xmm3
        mulps   xmm4, dqword ptr [ebp+TBLOffset+0A0H]
        mulps   xmm1, dqword ptr [ebp+TBLOffset+0A0H]
        movaps  dqword ptr [ecx + edi + 00000080H], xmm4
        movaps  dqword ptr [ecx + edi + 00000100H], xmm0
        movaps  xmm5, dqword ptr [ecx + edi + 00000140H]
        movaps  xmm3, dqword ptr [ecx + edi + 40H]
        movaps  xmm6, xmm5
        addps   xmm5, xmm3
        subps   xmm3, xmm6
        movaps  xmm0, dqword ptr [ecx + edi + 000000C0H]
        movaps  xmm4, dqword ptr [ecx + edi + 000001C0H]
        movaps  xmm6, xmm0
        mulps   xmm0, dqword ptr [ebp+TBLOffset+030H]
        movaps  xmm7, xmm4
        mulps   xmm4, dqword ptr [ebp+TBLOffset+030H]
        subps   xmm0, xmm7
        addps   xmm4, xmm6
        movaps  xmm6, xmm0
        addps   xmm0, xmm3
        subps   xmm3, xmm6
        movaps  xmm6, xmm4
        addps   xmm4, xmm5
        subps   xmm5, xmm6
        movaps  xmm6, xmm2
        addps   xmm2, xmm4
        subps   xmm4, xmm6
        movaps  xmm7, xmm1
        addps   xmm1, xmm3
        subps   xmm3, xmm7
        movaps  xmm6, dqword ptr [ecx + edi + 00000100H]
        movaps  xmm7, xmm5
        subps   xmm5, xmm6
        addps   xmm7, xmm6
        movaps  dqword ptr [ecx + edi + 40H], xmm2
        movaps  dqword ptr [ecx + edi + 00000200H], xmm4
        movaps  dqword ptr [ecx + edi + 000000C0H], xmm1
        movaps  dqword ptr [ecx + edi + 00000180H], xmm3
        movaps  dqword ptr [ecx + edi + 00000140H], xmm5
        movaps  dqword ptr [ecx + edi + 00000100H], xmm7
        movaps  xmm2, dqword ptr [ecx + edi + 00000080H]
        movaps  xmm3, xmm0
        addps   xmm0, xmm2
        subps   xmm3, xmm2
        movaps  dqword ptr [ecx + edi + 00000080H], xmm0
        movaps  dqword ptr [ecx + edi + 000001C0H], xmm3
        add     ecx, 16
        jne     @@016
        add     edi, 512
        jmp     dword ptr [ebp + 000000FCH]
end;


procedure _IDCT_Init;
asm
        mov     ecx, dword ptr [@TBL_IDCT - 00000008H + eax*4]
        mov     dword ptr [ebp + 000000F8H], ecx
        ret
        nop; nop
@TBL_IDCT:
        dd offset @TBL_IDCT_SSE
        dd offset @TBL_IDCT_SSE2
        dd offset @TBL_IDCT_SSE2
        dd 00000000H
        dd 00000000H
        dd 00000000H
@TBL_IDCT_SSE2:
        dd offset _IDCT_8x16_SSE2
        dd offset _IDCT_16x8_SSE2
        dd offset _IDCT_16x16_SSE2
        dd offset _IDCT_8x8_SSE2
@TBL_IDCT_SSE:
        dd offset _IDCT_8x16_SSE
        dd offset _IDCT_16x8_SSE
        dd offset _IDCT_16x16_SSE
        dd offset _IDCT_8x8_SSE
end;


procedure _MEM_Set;
asm
        mov     eax, edx
        test    dl, 07H
        jnz     @@007
@@002:  mov     edx, ecx
        and     ecx, 0FFFFFFE0H
        jz      @@004
        add     eax, ecx
        neg     ecx
@@003:  movq    qword ptr [ecx + eax], mm1
        movq    qword ptr [ecx + eax + 08H], mm1
        movq    qword ptr [ecx + eax + 10H], mm1
        movq    qword ptr [ecx + eax + 18H], mm1
        add     ecx, 32
        jl      @@003
@@004:  and     edx, 0000001FH
        jz      @@006
        add     eax, edx
        movd    ecx, mm1
        neg     edx
@@005:  mov     byte ptr [edx + eax], cl
        inc     edx
        jnz     @@005
@@006:  ret

@@007:  movd    edx, mm1
        jmp     @@009

@@008:  mov     byte ptr [eax], dl
        inc     eax
        test    al, 07H
        jz      @@002
@@009:  dec     ecx
        jge     @@008
end;


procedure _HUF_GenTable;
asm
        sub     esp, 20
        mov     dword ptr [esp], ebx
@@001:  movzx   eax, byte ptr [esi]
        inc     esi
        mov     edx, eax
        shr     eax, 4
        and     edx, 00000003H
        and     eax, 00000001H
        lea     eax, [eax + edx*2]
        imul    eax, eax, 704
        dec     dword ptr [esp]
        lea     ebx, [eax + ebp + 00000510H]
        mov     dword ptr [esp + 04H], ebp
        lea     edx, [ebx + 000000C0H]
        mov     ecx, 512
        pcmpeqd mm1, mm1
        call    _MEM_Set
        mov     ebp, -16                                
        mov     dword ptr [esp + 10H], 0                
        lea     edi, [esi + 10H]                        
@@002:  movzx   eax, byte ptr [ebp + esi + 10H]         
        dec     eax                                     
        mov     dword ptr [ebx + ebp*4 + 000000C0H], eax
        js      @@006                                   
        mov     dword ptr [esp + 0CH], eax              
        mov     ecx, dword ptr [esp + 10H]              
        mov     dword ptr [ebx + ebp*4 + 00000080H], ecx
        mov     dword ptr [ebx + ebp*4 + 40H], edi      
        cmp     ebp, -8                                 
        jge     @@005                                   
@@003:  lea     eax, [ebp + 11H]                        
        shl     eax, 8                                  
        movzx   ecx, byte ptr [edi]                     
        or      eax, ecx                                
        mov     dword ptr [esp + 08H], eax              
        mov     ecx, -9                                 
        sub     ecx, ebp                                
        mov     edx, dword ptr [esp + 10H]              
        shl     edx, cl                                 
        mov     eax, 1                                  
        shl     eax, cl
        lea     edx, [ebx + edx*2 + 000000C0H]
        mov     cx, word ptr [esp + 08H]
@@004:  mov     word ptr [edx + eax*2 - 02H], cx
        dec     eax
        jnz     @@004
        inc     edi
        inc     dword ptr [esp + 10H]
        dec     dword ptr [esp + 0CH]
        jns     @@003
        dec     eax
@@005:  mov     edx, dword ptr [esp + 10H]
        add     edx, eax
        lea     edi, [eax + edi + 01H]
        mov     dword ptr [ebx + ebp*4 + 000000C0H], edx
        inc     edx
        mov     dword ptr [esp + 10H], edx
@@006:  shl     dword ptr [esp + 10H], 1
        inc     ebp
        jne     @@002
        mov     ebp, dword ptr [esp + 04H]
        sub     dword ptr [esp], edi
        add     dword ptr [esp], esi
        mov     esi, edi
        jg      @@001
        add     esp, 20
end;


procedure _BS_GetBitsECS;
asm
        sub     dword ptr [ebp + 38H], eax
        jl      @@006
        movd    mm1, eax
        movq    mm2, mm0
        psrlq   mm2, qword ptr [ebp+TBL64Offset + eax*8]
        psllq   mm0, mm1
        movd    eax, mm2
        ret
@@006:  mov     edx, dword ptr [ebp + 40H]
        sub     dword ptr [ebp + 44H], 8
        jl      @@007                                   
        test    edx, 00000007H                          
        jnz     @@007                                   
        pcmpeqd mm1, mm1                                
        pcmpeqb mm1, qword ptr [edx]                    
        pmovmskb ecx, mm1                               
        test    ecx, ecx                                
        jnz     @@007                                   
        add     dword ptr [ebp + 40H], 8                
        mov     ecx, dword ptr [edx]                    
        mov     edx, dword ptr [edx + 04H]              
        bswap   ecx                                     
        bswap   edx                                     
        movd    mm1, ecx                                
        movd    mm2, edx                                
        psllq   mm1, 32                                 
        mov     ecx, dword ptr [ebp + 38H]              
        por     mm1, mm2                                
        add     ecx, 64                                 
        movq    mm2, mm1                                
        mov     dword ptr [ebp + 38H], ecx              
        movq    mm3, qword ptr [ebp+TBL64Offset + ecx*8]
        lea     ecx, [eax + ecx - 40H]
        movd    mm4, ecx
        psrlq   mm1, mm4
        por     mm0, mm1
        psrlq   mm0, qword ptr [ebp+TBL64Offset + eax*8]
        movd    eax, mm0                                
        psllq   mm2, mm3                                
        sub     ecx, ecx                                
        movq    mm0, mm2                                
        ret                                             

@@007:  sub     esp, 16                                 
        mov     dword ptr [esp + 04H], ebx              
        mov     dword ptr [esp], eax                    
        mov     ecx, dword ptr [ebp + 38H]
        mov     ebx, 7                                  
        add     eax, ecx                                
        mov     dword ptr [esp + 08H], eax              
        sub     ecx, ebx
        and     ebx, edx                                
        sar     ecx, 3                                  
        sub     ebx, 8                                  
        sub     ebx, ecx
        sbb     eax, eax                                
        and     eax, ebx                                
        add     ecx, eax                                
        lea     ebx, [ebp + 30H]                        
        pxor    mm1, mm1                                
        movq    qword ptr [ebx], mm1                    
        sub     ebx, ecx                                
        add     dword ptr [ebp + 44H], 8                
@@008:  cmp     dword ptr [ebp + 44H], 0                
        jz      @@010                                   
        mov     al, byte ptr [edx]                      
        inc     edx                                     
        mov     byte ptr [ecx + ebx], al                
        dec     dword ptr [ebp + 44H]                   
        cmp     al, -1                                  
        jnz     @@009
        cmp     dword ptr [ebp + 44H], 0                
        jz      @@010                                   
        mov     al, byte ptr [edx]                      
        test    al, al                                  
        jnz     @@011                                   
        inc     edx                                     
        dec     dword ptr [ebp + 44H]                   
@@009:  add     dword ptr [ebp + 38H], 8                
        inc     ecx                                     
        jnz     @@008                                   
@@010:  sub     ecx, ecx                                
        mov     ebx, dword ptr [esp + 04H]              
        mov     dword ptr [esp + 04H], ecx              
        mov     dword ptr [esp + 0CH], ecx              
        mov     dword ptr [ebp + 40H], edx              
        cmp     dword ptr [ebp + 38H], ecx              
        jl      @@012                                   
        mov     eax, dword ptr [ebp + 30H]              
        mov     ecx, dword ptr [ebp + 34H]              
        bswap   eax                                     
        bswap   ecx                                     
        movd    mm1, eax                                
        movd    mm2, ecx
        psllq   mm1, 32                                 
        por     mm1, mm2
        movq    mm2, mm1                                
        psrlq   mm1, qword ptr [esp + 08H]              
        por     mm0, mm1                                
        mov     eax, dword ptr [esp]                    
        psrlq   mm0, qword ptr [ebp+TBL64Offset + eax*8]
        sub     eax, dword ptr [esp + 08H]
        movd    mm3, eax                                
        psllq   mm2, mm3                                
        movd    eax, mm0                                
        movq    mm0, mm2
        add     esp, 16                                 
        sub     ecx, ecx                                
        ret                                             

@@011:  dec     edx                                     
        inc     dword ptr [ebp + 44H]                   
        mov     byte ptr [ecx + ebx], 0                 
        jmp     @@010                                   

@@012:  mov     dword ptr [ebp + 38H], 0
        add     esp, 16                                 
        stc                                             
end;


procedure _HUF_Decode;
asm
        cmp     dword ptr [ebp + 38H], 8
        jl      @@007
        movq    mm2, mm0
        psrlq   mm2, 56
        movd    ecx, mm2
        movzx   ecx, word ptr [eax + ecx*2 + 000000C0H]
        cmp     ecx, 65535
        jz      @@007
        mov     eax, ecx
        shr     ecx, 8
        movd    mm1, ecx
        and     eax, 000000FFH
        sub     dword ptr [ebp + 38H], ecx
        psllq   mm0, mm1
        ret

@@007:  push    ebx
        push    esi
        push    edi
        mov     ebx, eax
        mov     esi, -1
        sub     edi, edi
@@008:  inc     esi
        mov     eax, 1
        call    _BS_GetBitsECS
        jc      @@009
        lea     edi, [eax + edi*2]
        cmp     edi, dword ptr [ebx + esi*4 + 00000080H]
        jg      @@008
        mov     eax, dword ptr [ebx + esi*4]
        sub     edi, dword ptr [ebx + esi*4 + 40H]
        movzx   eax, byte ptr [edi + eax]
@@009:  pop     edi
        pop     esi                                     
        pop     ebx
end;


procedure _JPG_InitDecoder;
asm
        mov     dword ptr [ebp + 28H], eax
        push    ebx     // me: I got an "Invalid floating point op" here!!
        mov     eax, 1
        cpuid
        mov     eax, 4
        shr     ecx, 1
        jc      @000
        dec     eax
        shl     edx, 6
        jc      @000
        dec     eax
        shl     edx, 1
        jc      @000
        shl     edx, 1
        adc     eax, -2
@000:   pop     ebx
        cmp     eax, 2
        jl      @@002
        mov     dword ptr [ebp + 2CH], eax
        call    _IDCT_Init
        call    _RGB_Init
@@001:  ret

@@002:  mov     byte ptr [ebp + 1CH], 3
        sub     eax, eax
end;


procedure _BS_GetBits;
asm
        sub     dword ptr [ebp + 38H], eax
        jl      @@001                                   
        movd    mm1, eax
        movq    mm2, mm0
        psrlq   mm2, qword ptr [ebp+TBL64Offset + eax*8]
        psllq   mm0, mm1                                
        movd    eax, mm2                                
        ret                                             

@@001:  mov     edx, dword ptr [ebp + 40H]              
        sub     dword ptr [ebp + 44H], 8                
        jl      @@002                                   
        add     dword ptr [ebp + 40H], 8                
        mov     ecx, dword ptr [edx]                    
        mov     edx, dword ptr [edx + 04H]              
        bswap   ecx                                     
        bswap   edx                                     
        movd    mm1, ecx
        movd    mm2, edx                                
        psllq   mm1, 32                                 
        mov     ecx, dword ptr [ebp + 38H]              
        por     mm1, mm2
        add     ecx, 64                                 
        movq    mm2, mm1                                
        mov     dword ptr [ebp + 38H], ecx              
        movq    mm3, qword ptr [ebp+TBL64Offset + ecx*8]
        lea     ecx, [eax + ecx - 40H]                  
        movd    mm4, ecx                                
        psrlq   mm1, mm4                                
        por     mm0, mm1
        psrlq   mm0, qword ptr [ebp+TBL64Offset + eax*8]
        movd    eax, mm0                                
        psllq   mm2, mm3                                
        sub     ecx, ecx                                
        movq    mm0, mm2                                
        ret                                             

@@002:  sub     esp, 16                                 
        pxor    mm1, mm1                                
        movq    qword ptr [esp], mm1                    
        movq    qword ptr [esp + 08H], mm1              
        mov     dword ptr [esp], eax                    
        mov     ecx, dword ptr [ebp + 38H]              
        add     ecx, eax                                
        mov     dword ptr [esp + 08H], ecx              
        sub     ecx, ecx                                
@@003:  cmp     edx, dword ptr [ebp + 48H]              
        jnc     @@004                                   
        mov     al, byte ptr [edx]                      
        mov     byte ptr [ecx + ebp + 30H], al          
        inc     ecx                                     
        inc     edx                                     
        jmp     @@003                                   

@@004:  mov     dword ptr [ebp + 44H], 0                
        mov     dword ptr [ebp + 40H], edx              
        shl     ecx, 3                                  
        add     dword ptr [ebp + 38H], ecx              
        jl      @@005                                   
        mov     eax, dword ptr [ebp + 30H]              
        mov     ecx, dword ptr [ebp + 34H]              
        bswap   eax                                     
        bswap   ecx                                     
        movd    mm1, eax
        movd    mm2, ecx                                
        psllq   mm1, 32                                 
        por     mm1, mm2                                
        movq    mm2, mm1                                
        psrlq   mm1, qword ptr [esp + 08H]
        por     mm0, mm1                                
        mov     eax, dword ptr [esp]                    
        psrlq   mm0, qword ptr [ebp+TBL64Offset + eax*8]
        sub     eax, dword ptr [esp + 08H]              
        movd    mm3, eax                                
        psllq   mm2, mm3                                
        movd    eax, mm0                                
        movq    mm0, mm2
        add     esp, 16                                 
        sub     ecx, ecx                                
        ret                                             

@@005:  mov     dword ptr [ebp + 38H], 0
        add     esp, 16
        stc
end;

procedure _JPG_GetMarker;
asm
        mov     eax, 8
        call    _BS_GetBits
        jc      @@007
        cmp     al, -1
        jnz     _JPG_GetMarker
@@003:  mov     eax, 8
        call    _BS_GetBits
        jc      @@007
        cmp     al, -1
        jz      @@003                                   
        cmp     al, -65                                 
        jbe     _JPG_GetMarker
        cmp     al, -32                                 
        jc      @@004                                   
        mov     al, -32                                 
@@004:  cmp     al, -48                                 
        jc      @@005                                   
        cmp     al, -41                                 
        ja      @@005                                   
        mov     al, -48                                 
@@005:  mov     dword ptr [ebp + 24H], eax              
        mov     al, 1                                   
@@006:  ret                                             

@@007:  mov     byte ptr [ebp + 1CH], 0                 
        sub     eax, eax                                
        jmp     @@006                                   
end;


procedure _BS_Test;
asm
        mov     edx, eax
        mov     ecx, dword ptr [ebp + 38H]
        add     edx, dword ptr [ebp + 40H]
        shr     ecx, 3
        sub     edx, ecx
        cmp     edx, dword ptr [ebp + 48H]
end;


procedure _JPG_TestLength;
asm
        mov     eax, 16
        call    _BS_GetBits
        jc      @@010
        sub     eax, 2
        call    _BS_Test
@@010:
end;


procedure _JPG_SOI;
asm
        mov     al, 1
end;


procedure _JPG_SOF;
asm
        sub     esp, 4                                  
        call    _JPG_TestLength
        jg      @@017                                   
        mov     eax, dword ptr [ebp + 000000F0H]
        test    eax,eax
        jz @1
        call    ReleaseMem
        mov     dword ptr [ebp + 000000F0H], eax
@1:     mov     eax, 32
        mov     dword ptr [ebp + 10H], eax
        call    _BS_GetBits
        mov     ecx, eax                                
        mov     esi, eax                                
        shr     ecx, 24                                 
        shr     eax, 8
        and     esi, 000000FFH                          
        and     eax, 0000FFFFH                          
        je      @@015                                   
        mov     dword ptr [ebp + 04H], eax              
        sub     cl, 8                                   
        test    cl, 0FBH                                
        jne     @@015                                   
        add     ecx, 7                                  
        sub     eax, eax                                
        bts     eax, ecx                                
        cvtsi2ss xmm0, eax                              
        shufps  xmm0, xmm0, 0                           
        movaps  dqword ptr [ebp + 000000E0H], xmm0
        mov     eax, 16
        call    _BS_GetBits
        mov     edi, eax                                
        shl     esi, 8                                  
        and     edi, 00000003H                          
        shr     eax, 8                                  
        bt      edi, 0                                  
        jae     @@015                                   
        or      eax, esi                                
        je      @@015                                   
        mov     dword ptr [ebp], eax                    
        mov     dword ptr [ebp + 14H], edi              
        mov     eax, dword ptr [ebp + 00000100H]        
        mov     eax, dword ptr [eax + edi*4 - 04H]
        mov     dword ptr [ebp + 000000C4H], eax
        mov     dword ptr [esp], edi                    
        sub     edi, edi                                
        mov     ebx, edi                                
        lea     esi, [ebp + 50H]                        
@@011:  mov     eax, 24
        call    _BS_GetBits                           
        mov     ecx, eax
        mov     edx, eax                                
        and     eax, 00000003H                          
        shr     ecx, 8                                  
        shr     edx, 12                                 
        and     ecx, 0000000FH                          
        je      @@015                                   
        and     edx, 0000000FH                          
        je      @@015                                   
        mov     dword ptr [esi + 04H], ecx              
        mov     dword ptr [esi], edx                    
        cmp     ecx, ebx                                
        cmova   ebx, ecx                                
        cmp     edx, edi                                
        cmova   edi, edx                                
        shl     eax, 8
        lea     eax, [eax + ebp + 00000110H]            
        mov     dword ptr [esi + 14H], eax              
        imul    ecx, edx                                
        mov     dword ptr [esi + 08H], ecx              
        add     esi, 32
        dec     dword ptr [esp]                         
        jnz     @@011                                   
        mov     dword ptr [ebp + 000000B0H], edi        
        mov     dword ptr [ebp + 000000B4H], ebx        
        cmp     edi, 4
        ja      @@015                                   
        cmp     ebx, 4                                  
        ja      @@015                                   
        shl     edi, 3                                  
        mov     eax, dword ptr [ebp]
        sub     edx, edx                                
        lea     eax, [edi + eax - 01H]                  
        div     edi                                     
        mov     dword ptr [ebp + 000000CCH], eax
        mov     dword ptr [ebp + 000000D0H], eax        
        imul    edi                                     
        mov     dword ptr [ebp + 08H], eax              
        shl     ebx, 3                                  
        lea     ecx, [ebx*4 - 00000004H]                
        imul    eax, ecx                                
        mov     dword ptr [ebp + 000000D8H], eax        
        mov     eax, dword ptr [ebp + 04H]              
        lea     eax, [ebx + eax - 01H]                  
        div     ebx                                     
        imul    ebx                                     
        imul    edi, ebx                                
        imul    eax, dword ptr [ebp + 08H]              
        jo      @@015
        mov     ecx, eax
        shl     ecx, 2                                  
        je      @@015                                   
        div     edi                                     
        mov     dword ptr [ebp + 000000BCH], eax
        mov     eax, edi                                
        shl     edi, 2                                  
        mov     dword ptr [ebp + 000000B8H], edi        
        mov     ebx, dword ptr [ebp + 14H]              
        imul    edi, ebx                                
        lea     ebx, [ebx*4 + 00000004H]                
        shr     eax, 3                                  
        imul    ebx, eax
        lea     eax, [edi + ebx + 0FH]                  
        add     eax, ecx                                
        jo      @@015
        call    AllocMem
        test    eax,eax
        jz      @@016
        mov     dword ptr [ebp + 000000F0H], eax        
        and     eax, 0FFFFFFF0H                         
        mov     dword ptr [ebp + 000000F4H], eax        
        lea     ecx, [ebx + eax]
        add     ecx, edi                                
        mov     dword ptr [ebp + 0CH], ecx              
        mov     dword ptr [ebp + 000000C8H], ecx        
        mov     dword ptr [ebp + 000000D4H], ecx        
        add     edi, eax
        mov     ebx, eax                                
        mov     dword ptr [esp], 0                      
        lea     esi, [ebp + 50H]                        
@@012:  mov     eax, dword ptr [ebp + 000000B0H]        
        div     byte ptr [esi]
        bswap   eax                                     
        mov     ax, word ptr [ebp + 000000B4H]          
        div     byte ptr [esi + 04H]                    
        mov     ecx, 16777217                           
        mov     edx, 3                                  
        cmp     eax, ecx                                
        jz      @@013                                   
        shl     ecx, 1                                  
        dec     edx
        cmp     eax, ecx                                
        jz      @@013                                   
        dec     edx                                     
        dec     ecx                                     
        cmp     eax, ecx                                
        jz      @@013                                   
        dec     edx                                     
        bswap   ecx                                     
        cmp     eax, ecx                                
        jnz     @@015
@@013:  shl     edx, 2                                  
        mov     ecx, dword ptr [ebp + 000000F8H]        
        mov     ecx, dword ptr [edx + ecx]
        mov     dword ptr [esi + 1CH], ecx              
        push    0                                       
        call    _RGB_GenPointer
        add     ebx, dword ptr [ebp + 000000B8H]        
        add     edi, 4                                  
        add     esi, 32                                 
        mov     eax, dword ptr [esp]                    
        inc     eax                                     
        mov     dword ptr [esp], eax                    
        cmp     eax, dword ptr [ebp + 14H]              
        jnz     @@012
        mov     edx, 12                                 
        push    1
        call    _RGB_GenPointer
        mov     al, 1
@@014:  add     esp, 4
        ret

@@015:  mov     al, 5
        jmp     @@018

@@016:  mov     al, 1
        jmp     @@018

@@017:  mov     al, 0
@@018:  mov     byte ptr [ebp + 1CH], al
        sub     eax, eax
        jmp     @@014
end;


procedure _BS_GetPtr;
asm
        mov     ecx, dword ptr [ebp + 38H]              
        mov     eax, dword ptr [ebp + 40H]
        shr     ecx, 3
        sub     eax, ecx
        mov     dword ptr [ebp + 38H], 0
        add     dword ptr [ebp + 44H], ecx
        mov     dword ptr [ebp + 40H], eax
end;


procedure TBL_jpeg_natural_order;
asm
        db 00H, 01H, 08H, 10H, 09H, 02H, 03H, 0AH
        db 11H, 18H, 20H, 19H, 12H, 0BH, 04H, 05H
        db 0CH, 13H, 1AH, 21H, 28H, 30H, 29H, 22H
        db 1BH, 14H, 0DH, 06H, 07H, 0EH, 15H, 1CH
        db 23H, 2AH, 31H, 38H, 39H, 32H, 2BH, 24H
        db 1DH, 16H, 0FH, 17H, 1EH, 25H, 2CH, 33H
        db 3AH, 3BH, 34H, 2DH, 26H, 1FH, 27H, 2EH
        db 35H, 3CH, 3DH, 36H, 2FH, 37H, 3EH, 3FH
        db 3FH, 3FH, 3FH, 3FH, 3FH, 3FH, 3FH, 3FH
        db 3FH, 3FH, 3FH, 3FH, 3FH, 3FH, 3FH, 3FH
end;


procedure _JPG_ECS;
asm
        // Code Modification by Mark Griffiths to make this library thread safe
        mov     edi, esp
        sub     esp, 4
        and     esp, 0FFFFFFF0H
        add     esp, 4
        push    edi
        sub     esp, 120H
        // end of new code - original code is commented out below.
{
        mov     dword ptr [espsav], esp
        sub     esp, 287
        and     esp, 0FFFFFFF0H
}
        sub     edx, edx
        mov     ecx, dword ptr [ebp + 000000BCH]
        add     edx, dword ptr [ebp + 000000C0H]
        jnz     @@033
        mov     edx, ecx
@@033:  sub     ecx, edx
        sbb     eax, eax
        and     eax, ecx
        add     edx, eax
        je      @@040
        mov     dword ptr [esp + 00000108H], edx
        sub     dword ptr [ebp + 000000BCH], edx
        call    _BS_GetPtr
        mov     dword ptr [ebp + 000000FCH], offset @RetIdct
@@034:  mov     eax, dword ptr [ebp + 14H]
        mov     dword ptr [esp + 00000100H], eax
        lea     esi, [ebp + 50H]
        mov     edi, dword ptr [ebp + 000000F4H]
@@035:  mov     eax, dword ptr [esi + 08H]
        mov     dword ptr [esp + 00000104H], eax
@@036:  lea     edx, [esp]
        mov     ecx, 256
        pxor    mm1, mm1
        call    _MEM_Set
        mov     eax, dword ptr [esi + 0CH]
        call    _HUF_Decode
        jb      @@042
        mov     ebx, dword ptr [esi + 18H]
        test    eax, eax
        jz      @@037
        mov     dword ptr [esp + 0000010CH], eax
        call    _BS_GetBitsECS
        jb      @@042
        mov     ecx, dword ptr [esp + 0000010CH]
        mov     edx, eax
        shr     eax, cl
        sbb     eax, eax
        not     eax
        shl     eax, cl
        adc     eax, edx
        add     ebx, eax
        mov     dword ptr [esi + 18H], ebx
@@037:  mov     dword ptr [esp], ebx
        mov     ebx, -62
@@038:  mov     eax, dword ptr [esi + 10H]
        call    _HUF_Decode
        jb      @@042
        mov     dword ptr [esp + 0000010CH], eax
        shr     eax, 4
        add     ebx, eax
        and     dword ptr [esp + 0000010CH], 0000000FH
        jz      @@039
        mov     eax, dword ptr [esp + 0000010CH]
        call    _BS_GetBitsECS
        jc      @@042
        mov     ecx, dword ptr [esp + 0000010CH]
        mov     edx, eax
        shr     eax, cl
        sbb     eax, eax
        not     eax
        shl     eax, cl
        adc     eax, edx
        movzx   ecx, byte ptr [TBL_jpeg_natural_order + 0000003FH + ebx]
        mov     dword ptr [esp + ecx*4], eax
        mov     eax, 15
@@039:  cmp     eax, 15
        sbb     eax, eax
        not     eax
        and     ebx, eax
        inc     ebx
        jle     @@038
        mov     eax, dword ptr [esi + 14H]
        jmp     dword ptr [esi + 1CH]

@RetIdct:
        dec     dword ptr [esp + 00000104H]
        jne     @@036
        add     esi, 32
        dec     dword ptr [esp + 00000100H]
        jne     @@035
        call    dword ptr [ebp + 000000C4H]
        dec     dword ptr [esp + 00000108H]
        jne     @@034
        call    _BS_GetPtr
@@040:  mov     al, 1
@@041:
        // Code Modification by Mark Griffiths to make this library thread safe
        add     esp, 120H
        pop     esp
        // end of new code - original code is commented out below.
//        mov     esp, dword ptr [espsav]
        ret

@@042:  mov     byte ptr [ebp + 1CH], 0
        sub     eax, eax
        jmp     @@041
end;


procedure _JPG_SOS;
asm
        call    _JPG_TestLength
        jg      @@021                                   
        mov     eax, 8                                  
        call    _BS_GetBits                           
        mov     esi, eax                                
        cmp     eax, dword ptr [ebp + 14H]              
        jnz     @@022
        lea     ebx, [ebp + 50H]                        
@@019:  mov     eax, 16                                 
        call    _BS_GetBits                           
        mov     ecx, eax                                
        shr     eax, 4                                  
        and     ecx, 00000003H                          
        and     eax, 00000003H                          
        imul    eax, eax, 1408                          
        imul    ecx, ecx, 1408                          
        lea     eax, [eax + ebp + 00000510H]            
        lea     ecx, [ecx + ebp + 000007D0H]            
        mov     dword ptr [ebx + 0CH], eax              
        mov     dword ptr [ebx + 10H], ecx              
        add     ebx, 32                                 
        dec     esi                                     
        jnz     @@019                                   
        mov     eax, 24
        call    _BS_GetBits                           
        call    _JPG_ECS
@@020:  ret                                             

@@021:  mov     al, 0                                   
        jmp     @@023                                   

@@022:  mov     al, 5                                   
@@023:  mov     byte ptr [ebp + 1CH], al                
        sub     eax, eax                                
        jmp     @@020                                   
end;


procedure _JPG_DQT;
asm
        sub     esp, 4
        call    _JPG_TestLength
        jg      @@027
        mov     dword ptr [esp], eax
@@024:  mov     eax, 8
        call    _BS_GetBits
        mov     ebx, eax
        shr     eax, 4
        and     ebx, 00000003H                          
        and     eax, 00000001H                          
        shl     ebx, 8                                  
        lea     esi, [eax*8 + 00000008H]                
        mov     edi, -64                                
        lea     ebx, [ebx + ebp + 00000110H]
@@025:  mov     eax, esi                                
        call    _BS_GetBits                           
        movzx   ecx, byte ptr [TBL_jpeg_natural_order + 00000040H + edi]
        cvtsi2ss xmm0, eax
        inc     edi
        movss   dword ptr [ebx + ecx*4], xmm0
        jnz     @@025                                   
        shl     esi, 3                                  
        inc     esi
        sub     dword ptr [esp], esi                    
        jg      @@024                                   
        mov     al, 1                                   
@@026:  add     esp, 4                                  
        ret                                             

@@027:  mov     al, 0                                   
        mov     byte ptr [ebp + 1CH], al                
        sub     eax, eax                                
        jmp     @@026                                   
end;


procedure _BS_Align;
asm
        push    esi                                     
        push    edi
        push    ebx
        call    _BS_GetPtr
        mov     esi, eax
        lea     edi, [ebp + 30H]
        pxor    mm0, mm0
        movq    qword ptr [edi], mm0
        lea     ecx, [eax + 07H]
        and     ecx, 0FFFFFFF8H
        sub     ecx, eax
        mov     edx, dword ptr [ebp + 44H]
        sub     edx, ecx
        sbb     eax, eax
        and     eax, edx
        add     ecx, eax
        mov     eax, ecx
        shl     eax, 3
        sub     dword ptr [ebp + 44H], ecx
        rep movsb
        mov     dword ptr [ebp + 38H], eax
        mov     dword ptr [ebp + 40H], esi              
        mov     eax, dword ptr [ebp + 30H]              
        mov     ecx, dword ptr [ebp + 34H]
        bswap   eax                                     
        bswap   ecx                                     
        movd    mm0, eax                                
        movd    mm1, ecx                                
        psllq   mm0, 32
        por     mm0, mm1                                
        pop     ebx
        pop     edi                                     
        pop     esi
end;


procedure _BS_SkipBytes;
asm
        sub     dword ptr [ebp + 44H], eax
        mov     ecx, dword ptr [ebp + 38H]
        add     eax, dword ptr [ebp + 40H]
        shr     ecx, 3
        sub     eax, ecx
        mov     dword ptr [ebp + 38H], 0
        mov     dword ptr [ebp + 40H], eax
        add     dword ptr [ebp + 44H], ecx
        call    _BS_Align
end;


procedure _JPG_DHT;
asm
        call    _JPG_TestLength
        jg      @@029
        mov     ebx, eax
        call    _BS_GetPtr
        mov     esi, eax
        mov     eax, ebx
        call    _BS_SkipBytes
        call    _HUF_GenTable
        mov     al, 1
@@028:  ret

@@029:  mov     byte ptr [ebp + 1CH], 0
        sub     eax, eax
end;


procedure _JPG_DRI;
asm
        call    _JPG_TestLength
        jg      @@031
        mov     eax, 16
        call    _BS_GetBits
        mov     dword ptr [ebp + 000000C0H], eax
        mov     al, 1
        ret
@@031:  mov     byte ptr [ebp + 1CH], 0
        sub     eax, eax
end;


procedure _JPG_RST;
asm
        sub     eax, eax
        lea     ecx, [ebp + 50H]
        mov     edx, dword ptr [ebp + 14H]
@@032:  mov     dword ptr [ecx + 18H], eax
        add     ecx, 32
        dec     edx
        jnz     @@032
        call    _JPG_ECS
end;


procedure _JPG_EOI;
asm
        mov     byte ptr [ebp + 1CH], 2
        sub     eax, eax
end;


procedure _JPG_SKIP;
asm
        call    _JPG_TestLength
        jg      @@044
        call    _BS_SkipBytes
        mov     al, 1
        ret
@@044:  mov     byte ptr [ebp + 1CH], 0
        sub     eax, eax
end;


procedure _JPG_UNSUPP;
asm
        mov     byte ptr [ebp + 1CH], 5
        sub     eax, eax
end;


procedure _JPG_ProcessMarker;
asm
        mov     eax, dword ptr [ebp + 20H]
        mov     eax, dword ptr [@TBL_TransList + eax*4]
        mov     edx, dword ptr [ebp + 24H]
@@008:  mov     ecx, dword ptr [eax]
        cmp     cl, dl
        jz      @@009
        cmp     cl, -1
        jz      @@009
        add     eax, 4
        jmp     @@008
        nop; nop; nop
@TBL_Trans_DEAD:
        db 0D8H, 01H, 09H, 00H, 0FFH, 00H, 0BH, 00H
@TBL_Trans_SOI:
        db 0DAH, 02H, 02H, 00H, 0C0H, 01H, 01H, 00H
        db 0C1H, 01H, 01H, 00H, 0DBH, 01H, 03H, 00H
        db 0C4H, 01H, 04H, 00H, 0DDH, 01H, 05H, 00H
        db 0E0H, 01H, 0AH, 00H, 0FFH, 00H, 0BH, 00H
@TBL_Trans_ECS:
        db 0D9H, 00H, 07H, 00H, 0D0H, 02H, 06H, 00H
        db 0FFH, 00H, 0BH, 00H
@TBL_TransList:
        dd offset @TBL_Trans_DEAD
        dd offset @TBL_Trans_SOI
        dd offset @TBL_Trans_ECS
        dd 00000000H
@TBL_TransFunc:
        dd offset _JPG_SOF
        dd offset _JPG_SOS
        dd offset _JPG_DQT
        dd offset _JPG_DHT
        dd offset _JPG_DRI
        dd offset _JPG_RST
        dd offset _JPG_EOI
        dd offset _JPG_ECS
        dd offset _JPG_SOI
        dd offset _JPG_SKIP
        dd offset _JPG_UNSUPP
@@009:  shr     ecx, 8
        mov     byte ptr [ebp + 20H], cl
        shr     ecx, 8
        call    dword ptr [@TBL_TransFunc - 00000004H + ecx*4]
end;


function JpegDecode(Buffer: pointer; BufferLen: Cardinal; var pImg: PJpegDecode): TJpegDecodeError; stdcall;
asm     pop ebp  // delphi created a push ebp
        sub     eax, eax
        pushad
        mov     eax, dword ptr [esp + 2CH] // pImg
        mov     dword ptr [eax], 0
        mov     eax,TBLOffset+TBLSize+TBL64Size
        call    AllocMem // SSE2 ops expect 16 bytes aligned data -> no GetMem()
        test    eax,eax
        jz      @@005
        mov     ebp, eax
        mov     dword ptr [eax + 18H], -1412571974 // magic
        lea edx,eax+TBLOffset
        mov eax,offset TBL
        mov ecx,TBLSize
        call move // move TBL content into pImg for 16 bytes align
        push ebp
        add ebp,TBLOffset
        lea eax,ebp+0B0H // first _TBL_MultRow8x8 dd offset
        sub ebp,offset TBL
        mov ecx,16   // dd count for _TBL_MultRow8x8 and _TBL_MultRow16x16
@next:  add dword ptr [eax],ebp // dd offset corection TBL -> pImg
        add eax,4 // next dd
        dec ecx
        jnz @next
        pop ebp
        lea eax,ebp+TBL64Offset // create TBL_64 from code
        mov ecx,040H
@64:    mov [eax],ecx // 64..0 stored as mmx register 
        add eax,8     // next mmx register
        dec ecx
        jnz @64
@end:   xor eax,eax // no options is implemented by now 
        call    _JPG_InitDecoder
        test    eax, eax
        jz      @@002
        mov     esi, dword ptr [esp + 24H] // Buffer
        mov     ecx, dword ptr [esp + 28H] // BufferLen
        push    esi
        mov     dword ptr [ebp + 40H], esi
        mov     dword ptr [ebp + 44H], ecx
        add     esi, ecx
        mov     dword ptr [ebp + 48H], esi
        call    _BS_Align
        pop     esi
@@001:  call    _JPG_GetMarker
        test    eax, eax
        jz      @@002
        call    _JPG_ProcessMarker
        test    eax, eax
        jnz     @@001
@@002:  mov     bl, byte ptr [ebp + 1CH]
        cmp     bl, 2
        jnz     @@004
        mov     ecx, dword ptr [esp + 2CH]
        mov     dword ptr [ecx], ebp       // update pImg var
@@003:  mov     byte ptr [esp + 1CH], bl   // error code will be poped to eax
        emms    // allow FPU usage in Delphi code
        popad
        ret     12

@@004:  mov     eax, dword ptr [ebp + 000000F0H]
        or      eax, eax
        jz      @@006
        call    ReleaseMem // release any bitmap area (to avoid memory leak)
@@006:  mov     eax,ebp
        call    ReleaseMem // release TJpegDecode instance
        jmp     @@003

@@005:  mov     bl, 1
        jmp     @@003
end;


function JpegDecode(Buffer: pointer; BufferLen: integer): TBitmap;
VAR pImg: PJpegDecode;
begin
  Result:= NIL;
  if (Buffer = NIL) OR (BufferLen <= 0) then EXIT;

  if JpegDecode(Buffer, BufferLen, pImg)= JPEG_SUCCESS then
  TRY
    Result:= pImg^.ToBitmap;
  FINALLY
    pImg^.Free;
  End;
end;


procedure JpegDraw(Buffer: pointer; BufferLen: integer; Canvas: TCanvas; X,Y: integer);
var pImg: PJpegDecode;
begin
  if (Buffer = NIL) OR (BufferLen <= 0) then EXIT;

  if JpegDecode(Buffer,BufferLen,pImg)=JPEG_SUCCESS then
  try
    pImg^.DrawTo(Canvas,X,Y);
  finally
    pImg^.Free;
  end;
end;












{-------------------------------------------------------------------------------------------------------------
    TJpegDecode
-------------------------------------------------------------------------------------------------------------}
function TJpegDecode.Free: boolean;
asm
 test    eax, eax
 jz      @z
 cmp     dword ptr [eax + 18H], -1412571974  // magic
 jnz     @z
 push    eax
 mov     eax, dword ptr [eax + 000000F0H] // main allocated memory block
 call    ReleaseMem
 pop     eax
 call    ReleaseMem // TJpegDecode instance
 xor     eax,eax
@z:
end;


procedure TJpegDecode.ToBMI(var BMI: TBitmapInfo);
begin
  fillchar(BMI,sizeof(BMI),0);
  if @self= nil then exit;

  BMI.bmiHeader.biSize := sizeof(BMI.bmiHeader);
  BMI.bmiHeader.biWidth := scanlength;
  BMI.bmiHeader.biHeight := -height;
  BMI.bmiHeader.biPlanes := 1;
  BMI.bmiHeader.biBitCount := bitsPixel;
  BMI.bmiHeader.biCompression := BI_RGB;
end;


function TJpegDecode.ToBitmap: TBitmap;
var BMI: TBitmapInfo;
    DC: HDC;
begin
  if @self= NIL then EXIT(NIL);

  Result := TBitmap.Create;
  TRY
    result.PixelFormat := pf24bit;
    result.Width       := width;
    result.Height      := height;
    ToBMI(BMI);
    DC := GetDC(0);
    TRY
      if SetDIBits(DC, result.Handle,0,height,pRGB,BMI,DIB_RGB_COLORS) <> height
      then FreeAndNil(result);
    FINALLY
      ReleaseDC(0,DC);
    END;
  EXCEPT
    FreeAndNil(Result);      // ADDED BY ME!
    raise;                   // ADDED BY ME!
  END;
end;


procedure TJpegDecode.DrawTo(Canvas: TCanvas; X, Y: integer);
var BMI: TBitmapInfo;
begin
  if @self=nil then
    exit;
  ToBMI(BMI);
  StretchDIBits(Canvas.Handle,X,Y,width,height,0,0,width,height,pRGB,
   BMI,DIB_RGB_COLORS,SrcCopy);
end;


procedure TJpegDecode.DrawTo(Canvas: TCanvas; const Dest: TRect);
var BMI: TBitmapInfo;
begin
  if @self=nil then
    exit;
  ToBMI(BMI);
  StretchDIBits(Canvas.Handle,
    Dest.Left,Dest.Top,Dest.Right-Dest.Left,Dest.Bottom-Dest.Top,
    0,0,width,height,pRGB,BMI,DIB_RGB_COLORS,SrcCopy);
end;


procedure TJpegDecode.DrawTo(Canvas: TCanvas; const Source, Dest: TRect);
var BMI: TBitmapInfo;
begin
  if @self=nil then
    exit;
  ToBMI(BMI);
  StretchDIBits(Canvas.Handle,
    Dest.Left,Dest.Top,Dest.Right-Dest.Left,Dest.Bottom-Dest.Top,
    Source.Left,Source.Top,Source.Right-Source.Left,Source.Bottom-Source.Top,
    pRGB,BMI,DIB_RGB_COLORS,SrcCopy);
end;


end.
