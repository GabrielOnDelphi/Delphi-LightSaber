UNIT FormMain;

{
 Tester for LightCore.StreamBuffer.pas
 Tester for other streams

 Memory usage:
   File2 size:  20MBB ->  RAM 200MB
   File2 size:  700MB ->  RAM 1540MB. Finished OK
   File2 size: 1.11GB ->  RAM 1600MB, then it showed OutOfMem
}

INTERFACE

USES
  System.SysUtils, System.Classes, System.Types, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  InternetLabel, LightVcl.Visual.PathEdit, LightVcl.Visual.SpinEdit,
  LightVcl.Visual.RichLogTrack, LightVcl.Visual.CheckBox, LightVcl.Visual.RichLog, LightCore.AppData,
  LightVcl.Visual.AppData, LightVcl.Visual.AppDataForm, chHardID;

TYPE
  TMainForm = class(TLightForm)
    pgCtrl             : TPageControl;
    edtFile2           : TCubicPathEdit;
    InternetLabel      : TInternetLabel;
    Label1             : TLabel;
    Label2             : TLabel;
    tabMain            : TTabSheet;
    spnCacheSize: TCubicSpinEdit;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    btnStreamWrite: TButton;
    btnStreamRead: TButton;
    GroupBox2: TGroupBox;
    btnReadChar2: TButton;
    btnNewDelphiStream: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure actExitExecute          (Sender: TObject);
    procedure btnNewDelphiStreamClick (Sender: TObject);
    procedure btnReadChar2Click       (Sender: TObject);
    procedure btnStreamWriteClick     (Sender: TObject);
    procedure btnStreamReadClick      (Sender: TObject);
  private
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;


IMPLEMENTATION {$R *.dfm}


USES
   System.Math, LightCore.Types, LightCore.StreamBuff, LightVcl.Common.Debugger;





{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormPostInitialize;
begin
 inherited FormPostInitialize;
 Show;
end;


procedure TMainForm.actExitExecute(Sender: TObject);
begin
 Close;
end;





{--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------}

{ Delphi's TBufferedFileStream
  Read characters one by one

  Almost two times faster! }
procedure TMainForm.btnReadChar2Click(Sender: TObject);
VAR
   DelphiStream: TBufferedFileStream;
   Count: Cardinal;
   CacheSize: Integer;
   Char: AnsiChar;
begin
 Count:= 0;
 CacheSize:= spnCacheSize.Value;

 DelphiStream:= TBufferedFileStream.Create(edtFile2.Path, fmOpenRead, CacheSize * KB);
 TRY
   DelphiStream.Position:= 0;                                      { Go to first line }

   TimerStart;
   WHILE DelphiStream.Read(Char, 1) > 0 DO
       if Char = #32
       then Inc(Count);

   Caption:= 'Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(Count)+ '.  RAM: '+ ProcessPeakMem;

 FINALLY
   FreeAndNil(DelphiStream);
 END;
end;



procedure TMainForm.btnNewDelphiStreamClick(Sender: TObject);
VAR
   DelphiStream: TBufferedFileStream;
   FileSize, LineCount2: Cardinal;
   Buff, i, CacheSize: Integer;
begin
 Randomize;
 LineCount2:= 0;
 CacheSize:= spnCacheSize.Value;

 DelphiStream:= TBufferedFileStream.Create(edtFile2.Path, fmOpenRead, CacheSize * KB);
 FileSize:= DelphiStream.Size;
 //DelphiStream.FirstLine;                                        { Go to first line }

 TimerStart;
 for i:= 1 to 100000 DO
  begin
   DelphiStream.Position:= Random(FileSize);
   DelphiStream.Read(Buff, 4);
   Inc(LineCount2)
  end;

 Caption:= 'Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(LineCount2)+ '.  RAM: '+ ProcessPeakMem;

 FreeAndNil(DelphiStream);
end;




{--------------------------------------------------------------------------------------------------
   READ/WRITE TEST
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnStreamWriteClick(Sender: TObject);
VAR
   TSL: TStringList;
   Stream: TLightStream;
   dDate: TDate;
   IntArr: TIntegerArray;
   DblArr: TDoubleArray;
   ByteArr: TBytes;
   TestRect: TRect;
   TestRectF: TRectF;
begin
  Caption:= 'Writing...';
  dDate:= EncodeDate(2024, 08, 10);
  TSL:= TStringList.Create;
  TSL.Add('List0');
  TSL.Add('List1');

  Stream:= TLightStream.CreateWrite('TLightStream.bin');
  TRY
   {----------------- HEADER -----------------}
   Stream.WriteHeader('MagicNo', 1);
   
   {----------------- BOOLEAN -----------------}
   Stream.WriteBoolean(TRUE);
   Stream.WriteBoolean(FALSE);
   
   {----------------- DATE -----------------}
   Stream.WriteDate(dDate);
   Stream.WriteDate(Now);
   
   {----------------- BYTE TYPES -----------------}
   Stream.WriteByte(0);
   Stream.WriteByte(255);
   Stream.WriteByte(128);
   
   {----------------- CARDINAL -----------------}
   Stream.WriteCardinal(0);
   Stream.WriteCardinal(High(Cardinal));
   Stream.WriteCardinal(1234567890);
   
   {----------------- INT64 -----------------}
   Stream.WriteInt64(Low(Int64));
   Stream.WriteInt64(High(Int64));
   Stream.WriteInt64(0);
   
   {----------------- INTEGER -----------------}
   Stream.WriteInteger(Low(Integer));
   Stream.WriteInteger(High(Integer));
   Stream.WriteInteger(0);
   Stream.WriteInteger(-12345);
   
   {----------------- SHORTINT -----------------}
   Stream.WriteShortInt(Low(ShortInt));
   Stream.WriteShortInt(High(ShortInt));
   Stream.WriteShortInt(0);
   
   {----------------- SMALLINT -----------------}
   Stream.WriteSmallInt(Low(SmallInt));
   Stream.WriteSmallInt(High(SmallInt));
   Stream.WriteSmallInt(0);
   
   {----------------- UINT64 -----------------}
   Stream.WriteUInt64(0);
   Stream.WriteUInt64(High(UInt64));
   Stream.WriteUInt64(9876543210);
   
   {----------------- WORD -----------------}
   Stream.WriteWord(0);
   Stream.WriteWord(High(Word));
   Stream.WriteWord(32768);
   
   {----------------- FLOAT TYPES -----------------}
   Stream.WriteDouble(0.0);
   Stream.WriteDouble(3.14159265358979);
   Stream.WriteDouble(-123.456);
   Stream.WriteDouble(1.7E+308);  // Near max double
   
   Stream.WriteSingle(0.0);
   Stream.WriteSingle(3.14);
   Stream.WriteSingle(-99.9);
   Stream.WriteSingle(3.4E+38);  // Near max single
   
   {----------------- ANSI STRINGS -----------------}
   Stream.WriteStringA('');  // Empty string
   Stream.WriteStringA('abba');
   Stream.WriteStringA('Test ANSI with special chars: ÄÖÜ');
   Stream.WriteStringA('A very long string: ' + StringOfChar('X', 1000));
   
   {----------------- UNICODE STRINGS -----------------}
   Stream.WriteString('');  // Empty string
   Stream.WriteString('abba');
   Stream.WriteString('Unicode test: ???? ??');
   Stream.WriteString('Long unicode: ' + StringOfChar('O', 500));
   
   {----------------- CHARS -----------------}
   Stream.WriteChars(AnsiString('x'));
   Stream.WriteChars(AnsiString('ABC'));
   Stream.WriteChars('x');
   Stream.WriteChars('XYZ');
   
   {----------------- ENTER -----------------}
   Stream.WriteEnter;
   
   {----------------- STRING LISTS -----------------}
   Stream.WriteStrings(TSL);
   TSL.Add('List2');
   Stream.WriteStrings(TSL);
   
   {----------------- COMPLEX STRUCTURES: TRect -----------------}
   TestRect.Left := 10;
   TestRect.Top := 20;
   TestRect.Right := 100;
   TestRect.Bottom := 200;
   Stream.WriteRect(TestRect);
   
   TestRect := TRect.Create(0, 0, 0, 0);
   Stream.WriteRect(TestRect);
   
   {----------------- COMPLEX STRUCTURES: TRectF -----------------}
   TestRectF.Left := 10.5;
   TestRectF.Top := 20.5;
   TestRectF.Right := 100.5;
   TestRectF.Bottom := 200.5;
   Stream.WriteRectF(TestRectF);
   
   TestRectF := TRectF.Create(0, 0, 0, 0);
   Stream.WriteRectF(TestRectF);
   
   {----------------- INTEGER ARRAYS -----------------}
   SetLength(IntArr, 0);
   Stream.WriteIntegers(IntArr);  // Empty array
   
   SetLength(IntArr, 5);
   IntArr[0] := -100;
   IntArr[1] := 0;
   IntArr[2] := 100;
   IntArr[3] := Low(Integer);
   IntArr[4] := High(Integer);
   Stream.WriteIntegers(IntArr);
   
   {----------------- DOUBLE ARRAYS -----------------}
   SetLength(DblArr, 0);
   Stream.WriteDoubles(DblArr);  // Empty array
   
   SetLength(DblArr, 4);
   DblArr[0] := -123.456;
   DblArr[1] := 0.0;
   DblArr[2] := 789.012;
   DblArr[3] := 3.14159265358979;
   Stream.WriteDoubles(DblArr);
   
   {----------------- RAW BYTES -----------------}
   SetLength(ByteArr, 10);
   for var i := 0 to 9 do
     ByteArr[i] := Byte(i * 25);
   Stream.PushBytesCnt(ByteArr);
   
   Stream.PushBytes(ByteArr);
   
   {----------------- PUSH STRING (no length) -----------------}
   Stream.PushString('RawUnicode');
   Stream.PushAnsi(AnsiString('RawAnsi'));
   
   {----------------- PADDING -----------------}
   Stream.WritePadding0(32);  // Zero padding
   Stream.WritePadding(64);   // Safety padding with signature
   Stream.WritePadding(128);  // Larger padding
   
   {----------------- CHECKPOINTS -----------------}
   Stream.WriteCheckPoint;
   Stream.WriteCheckPoint('TestPoint');
   Stream.WriteCheckPoint('FinalCheck');
   
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(TSL);
  END;

  Caption:= 'Write successful';
end;



procedure TMainForm.btnStreamReadClick(Sender: TObject);
VAR
   TSL: TStringList;
   Stream: TLightStream;
   dDate: TDate;
   IntArr: TIntegerArray;
   DblArr: TDoubleArray;
   ByteArr: TBytes;
   TestRect: TRect;
   TestRectF: TRectF;
   TestStr: string;
   TestAnsi: AnsiString;
   SavedPos: Int64;
begin
  if NOT FileExists('TLightStream.bin') then 
  begin
    Caption := 'Error: Test file not found!';
    EXIT;
  end;
  
  dDate:= EncodeDate(2024, 08, 10);
  Caption:= 'Reading...';

  Stream:= TLightStream.CreateRead('TLightStream.bin');
  TRY
   {----------------- HEADER -----------------}
   if NOT stream.TryReadHeader('MagicNo', 1)
   then RAISE Exception.Create('Cannot read header!');
   
   // Test alternative header reading
   SavedPos := Stream.Position;
   Stream.Position := 0;
   if stream.TryReadHeader('MagicNo') <> 1
   then RAISE Exception.Create('TryReadHeader (version) failed!');
   Stream.Position := SavedPos;
   
   {----------------- BOOLEAN -----------------}
   if NOT Stream.ReadBoolean then RAISE Exception.Create('Boolean TRUE failure!');
   if Stream.ReadBoolean then RAISE Exception.Create('Boolean FALSE failure!');
   
   {----------------- DATE -----------------}
   if Stream.ReadDate <> dDate then RAISE Exception.Create('Date failure!');
   if Stream.ReadDate = 0 then RAISE Exception.Create('Date Now failure!');
   
   {----------------- BYTE TYPES -----------------}
   if Stream.ReadByte <> 0 then RAISE Exception.Create('ReadByte 0 failure!');
   if Stream.ReadByte <> 255 then RAISE Exception.Create('ReadByte 255 failure!');
   if Stream.ReadByte <> 128 then RAISE Exception.Create('ReadByte 128 failure!');
   
   {----------------- CARDINAL -----------------}
   if Stream.ReadCardinal <> 0 then RAISE Exception.Create('ReadCardinal 0 failure!');
   if Stream.ReadCardinal <> High(Cardinal) then RAISE Exception.Create('ReadCardinal High failure!');
   if Stream.ReadCardinal <> 1234567890 then RAISE Exception.Create('ReadCardinal value failure!');
   
   {----------------- INT64 -----------------}
   if Stream.ReadInt64 <> Low(Int64) then RAISE Exception.Create('ReadInt64 Low failure!');
   if Stream.ReadInt64 <> High(Int64) then RAISE Exception.Create('ReadInt64 High failure!');
   if Stream.ReadInt64 <> 0 then RAISE Exception.Create('ReadInt64 0 failure!');
   
   {----------------- INTEGER -----------------}
   if Stream.ReadInteger <> Low(Integer) then RAISE Exception.Create('ReadInteger Low failure!');
   if Stream.ReadInteger <> High(Integer) then RAISE Exception.Create('ReadInteger High failure!');
   if Stream.ReadInteger <> 0 then RAISE Exception.Create('ReadInteger 0 failure!');
   if Stream.ReadInteger <> -12345 then RAISE Exception.Create('ReadInteger negative failure!');
   
   {----------------- SHORTINT -----------------}
   if Stream.ReadShortInt <> Low(ShortInt) then RAISE Exception.Create('ReadShortInt Low failure!');
   if Stream.ReadShortInt <> High(ShortInt) then RAISE Exception.Create('ReadShortInt High failure!');
   if Stream.ReadShortInt <> 0 then RAISE Exception.Create('ReadShortInt 0 failure!');
   
   {----------------- SMALLINT -----------------}
   if Stream.ReadSmallInt <> Low(SmallInt) then RAISE Exception.Create('ReadSmallInt Low failure!');
   if Stream.ReadSmallInt <> High(SmallInt) then RAISE Exception.Create('ReadSmallInt High failure!');
   if Stream.ReadSmallInt <> 0 then RAISE Exception.Create('ReadSmallInt 0 failure!');
   
   {----------------- UINT64 -----------------}
   if Stream.ReadUInt64 <> 0 then RAISE Exception.Create('ReadUInt64 0 failure!');
   if Stream.ReadUInt64 <> High(UInt64) then RAISE Exception.Create('ReadUInt64 High failure!');
   if Stream.ReadUInt64 <> 9876543210 then RAISE Exception.Create('ReadUInt64 value failure!');
   
   {----------------- WORD -----------------}
   if Stream.ReadWord <> 0 then RAISE Exception.Create('ReadWord 0 failure!');
   if Stream.ReadWord <> High(Word) then RAISE Exception.Create('ReadWord High failure!');
   if Stream.ReadWord <> 32768 then RAISE Exception.Create('ReadWord value failure!');
   
   {----------------- FLOAT TYPES -----------------}
   if NOT SameValue(Stream.ReadDouble, 0.0, 1E-10) then RAISE Exception.Create('ReadDouble 0 failure!');
   if NOT SameValue(Stream.ReadDouble, 3.14159265358979, 1E-10) then RAISE Exception.Create('ReadDouble Pi failure!');
   if NOT SameValue(Stream.ReadDouble, -123.456, 1E-10) then RAISE Exception.Create('ReadDouble negative failure!');
   if Stream.ReadDouble <= 0 then RAISE Exception.Create('ReadDouble large failure!');
   
   if NOT SameValue(Stream.ReadSingle, 0.0, 1E-5) then RAISE Exception.Create('ReadSingle 0 failure!');
   if NOT SameValue(Stream.ReadSingle, 3.14, 1E-5) then RAISE Exception.Create('ReadSingle Pi failure!');
   if NOT SameValue(Stream.ReadSingle, -99.9, 1E-5) then RAISE Exception.Create('ReadSingle negative failure!');
   if Stream.ReadSingle <= 0 then RAISE Exception.Create('ReadSingle large failure!');
   
   {----------------- ANSI STRINGS -----------------}
   if Stream.ReadStringA <> '' then RAISE Exception.Create('ReadStringA empty failure!');
   if Stream.ReadStringA <> 'abba' then RAISE Exception.Create('ReadStringA simple failure!');
   TestAnsi := Stream.ReadStringA;
   if Pos('ÄÖÜ', string(TestAnsi)) = 0 then RAISE Exception.Create('ReadStringA special chars failure!');
   TestAnsi := Stream.ReadStringA;
   if Length(TestAnsi) <> 1020 then RAISE Exception.Create('ReadStringA long string failure!');
   
   {----------------- UNICODE STRINGS -----------------}
   if Stream.ReadString <> '' then RAISE Exception.Create('ReadString empty failure!');
   if Stream.ReadString <> 'abba' then RAISE Exception.Create('ReadString simple failure!');
   TestStr := Stream.ReadString;
   if Pos('??', TestStr) = 0 then RAISE Exception.Create('ReadString unicode failure!');
   TestStr := Stream.ReadString;
   if Length(TestStr) < 500 then RAISE Exception.Create('ReadString long unicode failure!');
   
   {----------------- CHARS -----------------}
   if Stream.ReadCharsA(1) <> 'x' then RAISE Exception.Create('ReadCharsA single failure!');
   if Stream.ReadCharsA(3) <> 'ABC' then RAISE Exception.Create('ReadCharsA multiple failure!');
   if Stream.ReadChars(1) <> 'x' then RAISE Exception.Create('ReadChars single failure!');
   if Stream.ReadChars(3) <> 'XYZ' then RAISE Exception.Create('ReadChars multiple failure!');
   
   {----------------- ENTER -----------------}
   if NOT Stream.ReadEnter then RAISE Exception.Create('ReadEnter failure!');
   
   {----------------- STRING LISTS -----------------}
   TSL := Stream.ReadStrings;
   if TSL.Count <> 2 then RAISE Exception.Create('ReadStrings count failure!');
   if TSL[0] <> 'List0' then RAISE Exception.Create('ReadStrings [0] failure!');
   if TSL[1] <> 'List1' then RAISE Exception.Create('ReadStrings [1] failure!');
   FreeAndNil(TSL);
   
   TSL := TStringList.Create;
   Stream.ReadStrings(TSL);
   if TSL.Count <> 3 then RAISE Exception.Create('ReadStrings (TSL) count failure!');
   if TSL[2] <> 'List2' then RAISE Exception.Create('ReadStrings (TSL) [2] failure!');
   FreeAndNil(TSL);
   
   {----------------- COMPLEX STRUCTURES: TRect -----------------}
   TestRect := Stream.ReadRect;
   if (TestRect.Left <> 10) or (TestRect.Top <> 20) or 
      (TestRect.Right <> 100) or (TestRect.Bottom <> 200) 
   then RAISE Exception.Create('ReadRect values failure!');
   
   TestRect := Stream.ReadRect;
   if NOT TestRect.IsEmpty then RAISE Exception.Create('ReadRect empty failure!');
   
   {----------------- COMPLEX STRUCTURES: TRectF -----------------}
   TestRectF := Stream.ReadRectF;
   if NOT SameValue(TestRectF.Left, 10.5, 0.01) or 
      NOT SameValue(TestRectF.Top, 20.5, 0.01) or
      NOT SameValue(TestRectF.Right, 100.5, 0.01) or 
      NOT SameValue(TestRectF.Bottom, 200.5, 0.01)
   then RAISE Exception.Create('ReadRectF values failure!');
   
   TestRectF := Stream.ReadRectF;
   if NOT TestRectF.IsEmpty then RAISE Exception.Create('ReadRectF empty failure!');
   
   {----------------- INTEGER ARRAYS -----------------}
   Stream.ReadIntegers(IntArr);
   if Length(IntArr) <> 0 then RAISE Exception.Create('ReadIntegers empty array failure!');
   
   Stream.ReadIntegers(IntArr);
   if Length(IntArr) <> 5 then RAISE Exception.Create('ReadIntegers count failure!');
   if (IntArr[0] <> -100) or (IntArr[1] <> 0) or (IntArr[2] <> 100) or
      (IntArr[3] <> Low(Integer)) or (IntArr[4] <> High(Integer))
   then RAISE Exception.Create('ReadIntegers values failure!');
   
   {----------------- DOUBLE ARRAYS -----------------}
   Stream.ReadDoubles(DblArr);
   if Length(DblArr) <> 0 then RAISE Exception.Create('ReadDoubles empty array failure!');
   
   Stream.ReadDoubles(DblArr);
   if Length(DblArr) <> 4 then RAISE Exception.Create('ReadDoubles count failure!');
   if NOT SameValue(DblArr[0], -123.456, 1E-10) or 
      NOT SameValue(DblArr[1], 0.0, 1E-10) or
      NOT SameValue(DblArr[2], 789.012, 1E-10) or
      NOT SameValue(DblArr[3], 3.14159265358979, 1E-10)
   then RAISE Exception.Create('ReadDoubles values failure!');
   
   {----------------- RAW BYTES -----------------}
   ByteArr := Stream.ReadByteChunk;
   if Length(ByteArr) <> 10 then RAISE Exception.Create('ReadByteChunk length failure!');
   for var i := 0 to 9 do
     if ByteArr[i] <> Byte(i * 25) then RAISE Exception.Create('ReadByteChunk value failure!');
   
   SetLength(ByteArr, 10);
   Stream.ReadBuffer(ByteArr[0], 10);
   for var i := 0 to 9 do
     if ByteArr[i] <> Byte(i * 25) then RAISE Exception.Create('PushBytes/ReadBuffer failure!');
   
   {----------------- PUSH STRING (no length) -----------------}
   if Stream.ReadString(10) <> 'RawUnicode' then RAISE Exception.Create('ReadString (fixed len) failure!');
   if Stream.ReadStringA(7) <> 'RawAnsi' then RAISE Exception.Create('ReadStringA (fixed len) failure!');
   
   {----------------- PADDING -----------------}
   Stream.ReadPadding0(32);  // Zero padding - no validation
   Stream.ReadPadding(64);   // Safety padding with validation
   Stream.ReadPadding(128);  // Larger padding with validation
   
   {----------------- CHECKPOINTS -----------------}
   if NOT Stream.ReadCheckPoint then RAISE Exception.Create('ReadCheckPoint (empty) failure!');
   if NOT Stream.ReadCheckPoint('TestPoint') then RAISE Exception.Create('ReadCheckPoint (TestPoint) failure!');
   Stream.ReadCheckPointE('FinalCheck');  // Should not raise exception
   
   {----------------- END OF FILE CHECK -----------------}
   if Stream.Position <> Stream.Size 
   then RAISE Exception.Create('Stream position mismatch! Expected EOF but ' + 
                               IntToStr(Stream.Size - Stream.Position) + ' bytes remain.');
   
  FINALLY
    FreeAndNil(Stream);
  END;

  Caption:= 'Read successful - All tests passed!';
end;


end.
