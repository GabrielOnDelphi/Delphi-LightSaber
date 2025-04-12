
UNIT cGraphLoader.WB1;

{=============================================================================================================
   Gabriel Moraru
   2023.08.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   Decoder for WB1 file format
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Classes, ccStreamMem, Vcl.Imaging.jpeg,
  System.AnsiStrings;

CONST
  WB1    = '*.WB1';
  WB1Ftl = 'WebShots|'+ WB1;

TYPE
 TWb1Obj = class(TObject)
  private
    FFileName: string;
  protected
    procedure JpegNeeded;
  public
    JpgStream: TCubicMemStream;
    InternalJPG: TJPEGImage;
    constructor Create;                                                                            { TObject is never directly instantiated. Although it does not use programming language features that prevent instantiation, TObject is an abstract class. }
    destructor  Destroy; override;
    procedure Clear;
    function  LoadFromFile(CONST FullFileName: string): Boolean;
    procedure SaveAsJpg(const FullFileName: string);
    property  FileName: string read FFileName;
 end;


IMPLEMENTATION
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }

CONST
  MAGIC_NUMBER0: AnsiString= 'WWBB0000';
  MAGIC_NUMBER1: AnsiString= 'WWBB1111';
  Key0 = $A4;
  Key1 = $F2;




procedure TWb1Obj.Clear;
begin
 {TODO: clear jpeg image here!!!!! }
 {InternalJPG.DoClear }
 JpgStream.Clear;
 FFileName:= '';
end;

constructor TWb1Obj.Create;
begin
 inherited Create;
 InternalJPG:= TJPEGImage.Create;
 JpgStream:= TCubicMemStream.Create;
end;

destructor TWb1Obj.Destroy;
begin
 FreeAndNil(InternalJPG);
 FreeAndNil(JpgStream);
 inherited Destroy;
end;





function TWb1Obj.LoadFromFile(CONST FullFileName: string): Boolean;
VAR
   WB1Stream: TCubicMemStream;
   MagicHeader: array[1..8] of AnsiChar;
   A100, B100, DecryptHeader: array[1..100] of Byte;
   Count, Key, I: Integer;
begin
 Clear;
 FFileName:= FullFileName;
 WB1Stream:= TCubicMemStream.Create;
 TRY
  { Read magic number }
  WB1Stream.LoadFromFile(FullFileName);
  WB1Stream.Position := 0;
  WB1Stream.ReadBuffer(MagicHeader, SizeOf(MagicHeader));

  { Magic number is ok? }
  Result:= SameText(MagicHeader, MAGIC_NUMBER0) OR SameText(MagicHeader, MAGIC_NUMBER1);
  if NOT Result then EXIT;

  { Find the right key }
  if SameText(MagicHeader, MAGIC_NUMBER0)
  then Key:= Key0
  else Key:= Key1;

  { Read the encrypted bytes (200 or 100?!?) }
  WB1Stream.ReadBuffer(A100, SizeOf(A100));
  WB1Stream.ReadBuffer(B100, SizeOf(A100));

  { Decode 100 bytes }
  for I:= 1 to Length(A100) DO                                                                     { Formula: B(n) = (B(n) XOR (NOT A(n)) XOR key  -> key = ‘F2’ (hex) }
   DecryptHeader[I]:= (B100[I] XOR (NOT A100[I])) XOR Key;

  { Build the final JPEG image }
  Count:= WB1Stream.Size- WB1Stream.Position;

  JpgStream.WriteBuffer(DecryptHeader, SizeOf(DecryptHeader));
  JpgStream.WriteBuffer(B100, SizeOf(B100));
  JpgStream.CopyFrom(WB1Stream, Count);                                                            { Copy the rest of the image }

  JpegNeeded;
 FINALLY
  FreeAndNil(WB1Stream);
 end;
end;


procedure TWb1Obj.SaveAsJpg(CONST FullFileName: string);
begin
 Assert(JpgStream<> NIL);
 JpgStream.SaveToFile(FullFileName);
end;


procedure TWb1Obj.JpegNeeded;                                                                      { Get a real JPEG from stream }
begin
 Assert(JpgStream<> NIL);
 JpgStream.Position:= 0;
 InternalJPG.LoadFromStream(JpgStream);
end;


end.






  (*
  var sTemp, Data: AnsiString;

  for I:= 1 to Length(DecryptHeader) DO
   Data:= Data+ AnsiChar(DecryptHeader[i]);

  for I:= 1 to Length(B100) DO
   Data:= Data+ AnsiChar(B100[i]);

  Count:= 20* MB;                                                                                  { I don't know how much I have to read so I TRY to read lots }
  SetLength(sTemp, Count);                                                                         { Make buffer large enough }
  Count:= WB1Stream.Read(sTemp[1], Count);                                                         { Try to read LOTS. The function will return the actual number of read bytes }
  SetLength(sTemp, Count);                                                                         { Now I know how much I read }
  Data:= Data+ sTemp;
  WriteToFileA(AppData.CurFolder+ 'tst.jpg', Data, TRUE, TRUE);
  IT WORKS!
  *)
