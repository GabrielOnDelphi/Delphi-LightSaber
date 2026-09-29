unit Test.LightVcl.Visual.ThumbViewerM;

{=============================================================================================================
   Unit tests for LightVcl.Visual.ThumbViewerM.pas (TCubicThumbs)
   Only AddPicture is tested: it is synchronous. LoadFolder needs a progress bar and the worker thread,
   which Test.LightVcl.Graph.Loader.Thread covers.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Graphics,
  LightVcl.Visual.ThumbViewerM;

type
  [TestFixture]
  TTestCubicThumbs = class
  private
    FTempDir: string;
    FForm: TForm;
    FThumbs: TCubicThumbs;
    function CreateBmpFile(CONST Name: string; W, H: Integer; PixelFormat: TPixelFormat): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAddPicture_StaysInsideBox;

    [Test]
    procedure TestAddPicture_Panorama_StaysInsideBox;

    [Test]
    procedure TestAddPicture_InvalidThumbSize_Raises;
  end;

implementation

uses
  LightCore.IO;


procedure TTestCubicThumbs.Setup;
begin
  FTempDir:= TPath.Combine(TPath.GetTempPath, 'TestCubicThumbs_' + TGUID.NewGuid.ToString);
  ForceDirectoriesE(FTempDir);

  { CreateNew: a form class declared in code has no DFM resource. The form is never shown. }
  FForm:= TForm.CreateNew(NIL);
  FForm.Width := 800;
  FForm.Height:= 600;

  FThumbs:= TCubicThumbs.Create(FForm);
  FThumbs.Parent:= FForm;
  FThumbs.Width := 700;
  FThumbs.Height:= 500;
  FThumbs.ThumbWidth := 100;
  FThumbs.ThumbHeight:= 100;
end;


procedure TTestCubicThumbs.TearDown;
begin
  FreeAndNil(FThumbs);
  FreeAndNil(FForm);
  if DirectoryExists(FTempDir)
  then TDirectory.Delete(FTempDir, TRUE);
end;


function TTestCubicThumbs.CreateBmpFile(CONST Name: string; W, H: Integer; PixelFormat: TPixelFormat): string;
var
  Bmp: TBitmap;
begin
  Result:= TPath.Combine(FTempDir, Name);
  Bmp:= TBitmap.Create;
  TRY
    Bmp.PixelFormat:= PixelFormat;
    Bmp.SetSize(W, H);
    Bmp.SaveToFile(Result);
  FINALLY
    FreeAndNil(Bmp);
  END;
end;


{ 400x300 into a 100x100 box must come out 100x75.
  The auto-detect resize mode returns 110x83 here, and TCubicThumbs.DrawCell then paints the thumbnail over the neighbouring cell. }
procedure TTestCubicThumbs.TestAddPicture_StaysInsideBox;
var
  BMP: TBitmap;
begin
  FThumbs.AddPicture(CreateBmpFile('img400x300.bmp', 400, 300, pf24bit));

  Assert.AreEqual(1, FThumbs.ThumbList.Count, 'One thumbnail expected');
  BMP:= FThumbs.ThumbList[0]^.BMP;
  Assert.IsNotNull(BMP, 'Thumbnail should have been generated');
  Assert.AreEqual(100, BMP.Width,  'Thumbnail width');
  Assert.AreEqual(75,  BMP.Height, 'Thumbnail height');
end;


{ 7000x1000 is panoramic for LightCore.Math.IsPanoramic. RResizeParams leaves it at full size unless ResizePanoram is TRUE. }
procedure TTestCubicThumbs.TestAddPicture_Panorama_StaysInsideBox;
var
  BMP: TBitmap;
begin
  FThumbs.AddPicture(CreateBmpFile('panorama.bmp', 7000, 1000, pf1bit));

  BMP:= FThumbs.ThumbList[0]^.BMP;
  Assert.IsNotNull(BMP, 'Thumbnail should have been generated');
  Assert.AreEqual(100, BMP.Width, 'Panorama thumbnail must be fitted to the box width');
  Assert.IsTrue(BMP.Height <= 100, 'Panorama thumbnail height should be <= 100 but is ' + IntToStr(BMP.Height));
end;


procedure TTestCubicThumbs.TestAddPicture_InvalidThumbSize_Raises;
begin
  FThumbs.ThumbWidth:= 0;
  Assert.WillRaise(
    procedure
    begin
      FThumbs.AddPicture(TPath.Combine(FTempDir, 'any.bmp'));
    end,
    Exception,
    'AddPicture must raise when ThumbWidth is 0');
  Assert.AreEqual(0, FThumbs.ThumbList.Count, 'Nothing may be added when the size is invalid');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCubicThumbs);

end.
