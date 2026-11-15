unit Test.CCR.Exif.TiffUtils;

{=============================================================================================================
   Unit tests for External\Exif\CCR.Exif.TiffUtils.pas
   2026.11.14

   A TIFF file holds a chain of image file directories (IFD).
   Each IFD ends with the offset of the next IFD, and an offset of 0 ends the chain.
   ParseTiff returns the IFDs one by one (for Directory in ParseTiff(Stream)).
   A damaged file can hold a next-IFD offset that points back to an IFD already read.
   The chain must then stop there: one directory per offset, and no endless loop.

   Each test builds a small little-endian ("II") TIFF in memory, byte by byte.
   CountDirectories stops after MaxDirectories, so a chain that never ends fails on its assertion instead of hanging the run.
   [MaxTime(5000)] is the second guard: after 5 seconds DUnitX raises ETimedOut inside the test thread (DUnitX.Timeout.pas).

   TTestTiffRewriter: RewriteTiff builds one TTiffDirectoryRewriter per directory, and one per sub-directory
   (a tag whose value is the offset of another IFD, such as the Exif IFD pointer).
   A damaged file can point a sub-directory back to itself or to a parent.
   TCountingCallback counts the rewriters (AddNewTags is called once per rewriter) and raises ERewriteLoop
   past MaxRewriters, so an endless recursion fails on its assertion instead of overflowing the stack.

   TestSimpleCallback_*: TSimpleTiffRewriteCallbackImpl (the callback of TIPTCData and TXMPPacket) must drop only
   the old metadata tag of IFD 0, never the tags of a sub-directory or of a second page.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  [TestFixture]
  TTestTiffParser = class
  private
    function CountDirectories(const Data: TBytes; out LastIsLast: Boolean): Integer;
  public
    [Test]
    procedure TestParse_ChainEndsWithZero_TwoDirectories;

    [Test]
    [MaxTime(5000)]
    procedure TestParse_DirectoryPointsToItself_OneDirectory;

    [Test]
    [MaxTime(5000)]
    procedure TestParse_SecondDirectoryPointsBack_TwoDirectories;
  end;

  [TestFixture]
  TTestTiffRewriter = class
  private
    function Rewrite(const Data: TBytes; out Output: TBytes; out LoopDetected: Boolean): Integer;
  public
    [Test]
    [MaxTime(5000)]
    procedure TestRewrite_NormalSubDirectory_TwoRewritersSameBytes;

    [Test]
    [MaxTime(5000)]
    procedure TestRewrite_NilCallback_SameBytes;

    [Test]
    [MaxTime(5000)]
    procedure TestRewrite_SubDirectoryPointsToItsOwnDirectory_OneRewriter;

    [Test]
    [MaxTime(5000)]
    procedure TestRewrite_SubDirectoryPointsBackToParent_TwoRewriters;

    [Test]
    [MaxTime(5000)]
    procedure TestRewrite_SubDirectoriesNested100Deep_RecursionCapped;

    [Test]
    [MaxTime(5000)]
    procedure TestSimpleCallback_EmptyOwner_ExifSubDirectoryKeepsItsTag;

    [Test]
    [MaxTime(5000)]
    procedure TestSimpleCallback_EmptyOwner_SecondDirectoryKeepsItsTag;

    [Test]
    [MaxTime(5000)]
    procedure TestSimpleCallback_EmptyOwner_OldMetadataTagInFirstDirectoryDropped;
  end;

implementation

uses
  CCR.Exif.BaseUtils,
  CCR.Exif.TiffUtils;

CONST
  MaxDirectories = 100;   { Far more than any test file below holds }

  { Header: "II" (little endian), the TIFF magic number 42, the first IFD at offset 8 }
  TiffHeader: array[0..7] of Byte = ($49, $49, $2A, $00, $08, $00, $00, $00);


{ Builds a TIFF from the header and one IFD with 0 tags per entry of NextOffsets.
  The IFD number i sits at offset 8 + 6*i: 2 bytes of tag count (0), then 4 bytes of next-IFD offset. }
function BuildTiff(const NextOffsets: array of Cardinal): TBytes;
VAR
  i, Pos: Integer;
begin
  SetLength(Result, Length(TiffHeader) + 6 * Length(NextOffsets));
  Move(TiffHeader[0], Result[0], Length(TiffHeader));
  Pos:= Length(TiffHeader);
  for i:= 0 to High(NextOffsets) do
    begin
      Result[Pos]    := 0;                               { Tag count, low byte }
      Result[Pos + 1]:= 0;                               { Tag count, high byte }
      Result[Pos + 2]:= Byte(NextOffsets[i]);            { Next-IFD offset, little endian }
      Result[Pos + 3]:= Byte(NextOffsets[i] shr 8);
      Result[Pos + 4]:= Byte(NextOffsets[i] shr 16);
      Result[Pos + 5]:= Byte(NextOffsets[i] shr 24);
      Inc(Pos, 6);
    end;
end;


{ Returns how many directories ParseTiff gives for Data, but never more than MaxDirectories + 1.
  LastIsLast tells whether the last directory returned says it is the last one in the file. }
function TTestTiffParser.CountDirectories(const Data: TBytes; out LastIsLast: Boolean): Integer;
VAR
  Stream: TBytesStream;
  Directory: IFoundTiffDirectory;
begin
  Result:= 0;
  LastIsLast:= FALSE;
  Stream:= TBytesStream.Create(Data);
  TRY
    for Directory in ParseTiff(Stream) do
      begin
        Inc(Result);
        LastIsLast:= Directory.IsLastDirectoryInFile;
        if Result > MaxDirectories
        then Break;                                      { The chain does not end }
      end;
  FINALLY
    FreeAndNil(Stream);
  END;
end;


{ A normal file: IFD 0 at offset 8 points to IFD 1 at offset 14, which ends the chain with 0 }
procedure TTestTiffParser.TestParse_ChainEndsWithZero_TwoDirectories;
VAR
  LastIsLast: Boolean;
begin
  Assert.AreEqual(2, CountDirectories(BuildTiff([14, 0]), LastIsLast), 'A chain of 2 IFDs that ends with offset 0');
  Assert.IsTrue(LastIsLast, 'The second IFD is the last one in the file');
end;


{ A damaged file of 14 bytes: its only IFD, at offset 8, gives offset 8 as the next IFD }
procedure TTestTiffParser.TestParse_DirectoryPointsToItself_OneDirectory;
VAR
  LastIsLast: Boolean;
begin
  Assert.AreEqual(1, CountDirectories(BuildTiff([8]), LastIsLast), 'The IFD points to itself: the parser must return it once and stop');
  Assert.IsTrue(LastIsLast, 'The chain ends at the IFD that points back');
end;


{ A damaged file: IFD 0 at offset 8 points to IFD 1 at offset 14, which points back to offset 8 }
procedure TTestTiffParser.TestParse_SecondDirectoryPointsBack_TwoDirectories;
VAR
  LastIsLast: Boolean;
begin
  Assert.AreEqual(2, CountDirectories(BuildTiff([14, 8]), LastIsLast), 'IFD 1 points back to IFD 0: the parser must return each IFD once and stop');
  Assert.IsTrue(LastIsLast, 'The chain ends at the IFD that points back');
end;



{ TTestTiffRewriter }

CONST
  MaxRewriters = 1000;        { Far more than any test file below needs }

  { Tag IDs and the data type LONG, from the TIFF 6.0 and Exif 2.3 specifications }
  TagImageWidth    = $0100;   { TIFF ImageWidth: a plain number, never a directory offset }
  TagExifIFD       = $8769;   { Exif IFD pointer }
  TagInteropIFD    = $A005;   { Interoperability IFD pointer }
  TypeLong         = 4;

  IfdSize = 18;               { 2 bytes tag count + one 12-byte tag + 4 bytes next-IFD offset }

TYPE
  ERewriteLoop = class(Exception);

  { Counts the rewriters RewriteTiff builds: TTiffDirectoryRewriter.Create calls AddNewTags once per rewriter }
  TCountingCallback = class(TInterfacedObject, ITiffRewriteCallback)
  public
    Count: Integer;
    procedure AddNewTags(Rewriter: TTiffDirectoryRewriter);
    procedure RewritingOldTag(const Source: ITiffDirectory; TagID: TTiffTagID; DataType: TTiffDataType; var Rewrite: Boolean);
  end;


procedure TCountingCallback.AddNewTags(Rewriter: TTiffDirectoryRewriter);
begin
  Inc(Count);
  if Count > MaxRewriters
  then raise ERewriteLoop.Create('The rewriter recursion does not end');
end;


{ Leaves Rewrite as it is, so every tag of every directory is rewritten }
procedure TCountingCallback.RewritingOldTag(const Source: ITiffDirectory; TagID: TTiffTagID; DataType: TTiffDataType; var Rewrite: Boolean);
begin
end;


{ Builds a little-endian TIFF from the header and one IFD per entry of TagIDs.
  The IFD number i sits at offset 8 + 18*i and holds one LONG tag (TagIDs[i], value Values[i]); its next-IFD offset is 0,
  so the main chain holds only IFD 0, and the other IFDs are reached as sub-directories. }
function BuildOneTagIfds(const TagIDs: array of Word; const Values: array of Cardinal): TBytes;
VAR
  i, Pos: Integer;
  Buf: TBytes;

  procedure PutWord(Value: Word);
  begin
    Buf[Pos]    := Byte(Value);
    Buf[Pos + 1]:= Byte(Value shr 8);
    Inc(Pos, 2);
  end;

  procedure PutLong(Value: Cardinal);
  begin
    PutWord(Word(Value));
    PutWord(Word(Value shr 16));
  end;

begin
  SetLength(Buf, Length(TiffHeader) + IfdSize * Length(TagIDs));
  Move(TiffHeader[0], Buf[0], Length(TiffHeader));
  Pos:= Length(TiffHeader);
  for i:= 0 to High(TagIDs) do
    begin
      PutWord(1);              { Tag count }
      PutWord(TagIDs[i]);      { Tag ID }
      PutWord(TypeLong);       { Data type }
      PutLong(1);              { Element count }
      PutLong(Values[i]);      { The value, held in the tag itself because it fits in 4 bytes }
      PutLong(0);              { Next-IFD offset: none }
    end;
  Result:= Buf;
end;


function BytesToHex(const Data: TBytes): string;
VAR
  B: Byte;
begin
  Result:= '';
  for B in Data do
    Result:= Result + IntToHex(B, 2);
end;


{ Runs RewriteTiff on Data and returns how many rewriters it built.
  LoopDetected is True when the count passed MaxRewriters, and Output is then empty. }
function TTestTiffRewriter.Rewrite(const Data: TBytes; out Output: TBytes; out LoopDetected: Boolean): Integer;
VAR
  Counter: TCountingCallback;
  Callback: ITiffRewriteCallback;
  InStream, OutStream: TBytesStream;
begin
  Output:= NIL;
  LoopDetected:= FALSE;
  Counter:= TCountingCallback.Create;
  Callback:= Counter;                          { The interface reference owns the object }
  InStream:= TBytesStream.Create(Data);
  TRY
    OutStream:= TBytesStream.Create(NIL);
    TRY
      TRY
        RewriteTiff(InStream, OutStream, Callback);
        Output:= Copy(OutStream.Bytes, 0, OutStream.Size);
      EXCEPT
        on ERewriteLoop do LoopDetected:= TRUE;  { The guard of TCountingCallback fired: the recursion does not end }
      END;
    FINALLY
      FreeAndNil(OutStream);
    END;
  FINALLY
    FreeAndNil(InStream);
  END;
  Result:= Counter.Count;
end;


{ A normal file: IFD 0 (offset 8) holds the Exif IFD pointer to offset 26; the Exif IFD holds ImageWidth = 640.
  The rewrite builds 2 rewriters and, since the layout is already compact, writes the same bytes back. }
procedure TTestTiffRewriter.TestRewrite_NormalSubDirectory_TwoRewritersSameBytes;
VAR
  Data, Output: TBytes;
  LoopDetected: Boolean;
begin
  Data:= BuildOneTagIfds([TagExifIFD, TagImageWidth], [8 + IfdSize, 640]);
  Assert.AreEqual(2, Rewrite(Data, Output, LoopDetected), 'IFD 0 and its Exif sub-directory');
  Assert.IsFalse(LoopDetected, 'The rewrite must end');
  Assert.AreEqual(BytesToHex(Data), BytesToHex(Output), 'The sub-directory must be rewritten at offset 26, with its tag');
end;


{ The same normal file, rewritten with no callback at all (the sub-directory is still followed and written) }
procedure TTestTiffRewriter.TestRewrite_NilCallback_SameBytes;
VAR
  Data, Output: TBytes;
  InStream, OutStream: TBytesStream;
begin
  Data:= BuildOneTagIfds([TagExifIFD, TagImageWidth], [8 + IfdSize, 640]);
  InStream:= TBytesStream.Create(Data);
  TRY
    OutStream:= TBytesStream.Create(NIL);
    TRY
      RewriteTiff(InStream, OutStream, NIL);
      Output:= Copy(OutStream.Bytes, 0, OutStream.Size);
    FINALLY
      FreeAndNil(OutStream);
    END;
  FINALLY
    FreeAndNil(InStream);
  END;
  Assert.AreEqual(BytesToHex(Data), BytesToHex(Output), 'The sub-directory must be rewritten at offset 26, with its tag');
end;


{ A damaged file of 26 bytes: IFD 0, at offset 8, holds an Exif IFD pointer to offset 8, its own offset }
procedure TTestTiffRewriter.TestRewrite_SubDirectoryPointsToItsOwnDirectory_OneRewriter;
VAR
  Data, Output: TBytes;
  LoopDetected: Boolean;
begin
  Data:= BuildOneTagIfds([TagExifIFD], [8]);
  Assert.AreEqual(1, Rewrite(Data, Output, LoopDetected), 'The pointer back to IFD 0 must not start a second rewrite of IFD 0');
  Assert.IsFalse(LoopDetected, 'The rewrite must end');
  Assert.AreEqual(BytesToHex(Data), BytesToHex(Output), 'The tag must be kept, with its value 8');
end;


{ A damaged file: IFD 0 (offset 8) points to the Exif IFD at offset 26, whose Interop IFD pointer goes back to offset 8 }
procedure TTestTiffRewriter.TestRewrite_SubDirectoryPointsBackToParent_TwoRewriters;
VAR
  Data, Output: TBytes;
  LoopDetected: Boolean;
begin
  Data:= BuildOneTagIfds([TagExifIFD, TagInteropIFD], [8 + IfdSize, 8]);
  Assert.AreEqual(2, Rewrite(Data, Output, LoopDetected), 'One rewriter for IFD 0 and one for the Exif IFD, then stop');
  Assert.IsFalse(LoopDetected, 'The rewrite must end');
  Assert.AreEqual(BytesToHex(Data), BytesToHex(Output), 'Both IFDs rewritten once; the pointer back keeps its value 8');
end;


{ A crafted file: 100 IFDs, each one the Exif sub-directory of the one before it, the last one holding ImageWidth.
  Without a limit on the nesting, a long enough chain of this kind overflows the stack. }
procedure TTestTiffRewriter.TestRewrite_SubDirectoriesNested100Deep_RecursionCapped;
CONST
  Depth = 100;
VAR
  TagIDs: array of Word;
  Values: array of Cardinal;
  Data, Output: TBytes;
  i, Count: Integer;
  LoopDetected: Boolean;
begin
  SetLength(TagIDs, Depth);
  SetLength(Values, Depth);
  for i:= 0 to Depth - 2 do
    begin
      TagIDs[i]:= TagExifIFD;
      Values[i]:= 8 + IfdSize * Cardinal(i + 1);  { The offset of the next IFD }
    end;
  TagIDs[Depth - 1]:= TagImageWidth;
  Values[Depth - 1]:= 640;
  Data:= BuildOneTagIfds(TagIDs, Values);

  Count:= Rewrite(Data, Output, LoopDetected);
  Assert.IsFalse(LoopDetected, 'The rewrite must end');
  Assert.IsTrue(Count >= 3, 'Normal nesting (IFD 0 > Exif > Interop) must still be rewritten. Count: ' + IntToStr(Count));
  Assert.IsTrue(Count < Depth, 'The rewriter must stop following sub-directories long before 100 levels. Count: ' + IntToStr(Count));
  Assert.IsTrue(Length(Output) > 0, 'The rewrite must still write the file');
end;



{ TSimpleTiffRewriteCallbackImpl is the callback that TIPTCData and TXMPPacket give to RewriteTiff.
  With an empty owner its AddNewTags adds nothing, and its RewritingOldTag must drop only the old metadata tag
  of the first main directory (Parent = nil, Index = 0), the one place where AddNewTags writes the new one. }

CONST
  TagIPTC = $83BB;            { The IPTC tag ID, as in CCR.Exif.TagIDs (ttIPTC) }

TYPE
  { An owner whose metadata block is empty: Owner.Empty = TRUE }
  TEmptyMetadataOwner = class(TInterfacedObject, IStreamPersist, IStreamPersistEx)
  public
    function GetEmpty: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;


function TEmptyMetadataOwner.GetEmpty: Boolean;
begin
  Result:= TRUE;
end;


procedure TEmptyMetadataOwner.LoadFromStream(Stream: TStream);
begin
end;


procedure TEmptyMetadataOwner.SaveToStream(Stream: TStream);
begin
end;


{ Runs RewriteTiff on Data with a TSimpleTiffRewriteCallbackImpl for IPTC data whose owner is empty }
function RewriteWithEmptyIptc(const Data: TBytes): TBytes;
VAR
  Owner: IStreamPersistEx;
  Impl: TSimpleTiffRewriteCallbackImpl;
  Callback: ITiffRewriteCallback;
  InStream, OutStream: TBytesStream;
begin
  Owner:= TEmptyMetadataOwner.Create;
  Impl:= TSimpleTiffRewriteCallbackImpl.Create(Owner, TagIPTC);   { A TAggregatedObject: Owner counts its references }
  TRY
    Callback:= Impl;
    InStream:= TBytesStream.Create(Data);
    TRY
      OutStream:= TBytesStream.Create(NIL);
      TRY
        RewriteTiff(InStream, OutStream, Callback);
        Result:= Copy(OutStream.Bytes, 0, OutStream.Size);
      FINALLY
        FreeAndNil(OutStream);
      END;
    FINALLY
      FreeAndNil(InStream);
    END;
  FINALLY
    Callback:= NIL;            { Before Impl is freed }
    FreeAndNil(Impl);
  END;
end;


{ IFD 0 (offset 8) holds the Exif IFD pointer to offset 26; the Exif IFD holds ImageWidth = 640.
  The file holds no IPTC tag, so nothing may be dropped. A sub-directory gets its offset tag ID ($8769) as Index. }
procedure TTestTiffRewriter.TestSimpleCallback_EmptyOwner_ExifSubDirectoryKeepsItsTag;
VAR
  Data: TBytes;
begin
  Data:= BuildOneTagIfds([TagExifIFD, TagImageWidth], [8 + IfdSize, 640]);
  Assert.AreEqual(BytesToHex(Data), BytesToHex(RewriteWithEmptyIptc(Data)), 'The ImageWidth tag of the Exif sub-directory must be kept');
end;


{ Two main directories (two pages): IFD 0 at offset 8 holds ImageWidth = 640 and points to IFD 1 at offset 26,
  which holds ImageWidth = 320. The file holds no IPTC tag, so nothing may be dropped. }
procedure TTestTiffRewriter.TestSimpleCallback_EmptyOwner_SecondDirectoryKeepsItsTag;
CONST
  NextOffsetPos = 8 + 2 + 12;   { The next-IFD offset of IFD 0: after its tag count and its one tag }
VAR
  Data: TBytes;
begin
  Data:= BuildOneTagIfds([TagImageWidth, TagImageWidth], [640, 320]);
  Data[NextOffsetPos]:= 8 + IfdSize;
  Assert.AreEqual(BytesToHex(Data), BytesToHex(RewriteWithEmptyIptc(Data)), 'The ImageWidth tag of the second page must be kept');
end;


{ IFD 0 holds an old IPTC tag; the owner is empty, so the old tag must go and IFD 0 is written with 0 tags }
procedure TTestTiffRewriter.TestSimpleCallback_EmptyOwner_OldMetadataTagInFirstDirectoryDropped;
CONST
  { Header, then IFD 0: tag count 0, next-IFD offset 0 }
  Expected = '49492A0008000000' + '0000' + '00000000';
VAR
  Data: TBytes;
begin
  Data:= BuildOneTagIfds([TagIPTC], [640]);
  Assert.AreEqual(Expected, BytesToHex(RewriteWithEmptyIptc(Data)), 'The old IPTC tag of IFD 0 must be dropped');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestTiffParser);
  TDUnitX.RegisterTestFixture(TTestTiffRewriter);

end.
