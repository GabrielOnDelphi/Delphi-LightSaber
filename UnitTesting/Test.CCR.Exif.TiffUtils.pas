unit Test.CCR.Exif.TiffUtils;

{=============================================================================================================
   Unit tests for External\Exif\CCR.Exif.TiffUtils.pas
   2026.10.31

   A TIFF file holds a chain of image file directories (IFD).
   Each IFD ends with the offset of the next IFD, and an offset of 0 ends the chain.
   ParseTiff returns the IFDs one by one (for Directory in ParseTiff(Stream)).
   A damaged file can hold a next-IFD offset that points back to an IFD already read.
   The chain must then stop there: one directory per offset, and no endless loop.

   Each test builds a small little-endian ("II") TIFF in memory, byte by byte.
   CountDirectories stops after MaxDirectories, so a chain that never ends fails on its assertion instead of hanging the run.
   [MaxTime(5000)] is the second guard: after 5 seconds DUnitX raises ETimedOut inside the test thread (DUnitX.Timeout.pas).
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

implementation

uses
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


initialization
  TDUnitX.RegisterTestFixture(TTestTiffParser);

end.
