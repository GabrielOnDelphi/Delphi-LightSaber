unit Test.LightVcl.Graph.Cache;

{=============================================================================================================
   Unit tests for LightVcl.Graph.Cache
   Tests thumbnail caching system functionality.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.jpeg;

type
  [TestFixture]
  TTestGraphCache = class
  private
    FTestFolder: string;
    FCacheFolder: string;
    FTestImagePath: string;
    FTestImagePath2: string;
    procedure CreateTestImage(const APath: string; AWidth, AHeight: Integer);
    procedure CleanupTestFiles;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_ValidFolder;

    [Test]
    procedure TestCreate_EmptyFolder_RaisesException;

    { CacheFolder Property Tests }
    [Test]
    procedure TestSetCacheFolder_EmptyValue_RaisesException;

    [Test]
    procedure TestSetCacheFolder_ValidFolder_TrailsPath;

    { AddToCache / GetThumbFor Tests }
    [Test]
    procedure TestGetThumbFor_NewImage_CreatesThumb;

    [Test]
    procedure TestGetThumbFor_ExistingThumb_ReturnsCached;

    [Test]
    procedure TestGetThumbFor_NonExistentFile_ReturnsEmpty;

    [Test]
    procedure TestGetThumbFor_NonImageFile_ReturnsEmpty;

    [Test]
    procedure TestAddToCache_NonExistentFile_ReturnsEmpty;

    { ImagePosDB Tests }
    [Test]
    procedure TestImagePosDB_ExistingImage_ReturnsPosition;

    [Test]
    procedure TestImagePosDB_NonExistingImage_ReturnsMinusOne;

    [Test]
    procedure TestImagePosDB_WithOutParam_SetsShortPath;

    { ThumbPosDB Tests }
    [Test]
    procedure TestThumbPosDB_ExistingThumb_ReturnsPosition;

    [Test]
    procedure TestThumbPosDB_NonExistingThumb_ReturnsMinusOne;

    { DeleteThumb Tests }
    [Test]
    procedure TestDeleteThumb_ByPosition_DeletesFromDB;

    [Test]
    procedure TestDeleteThumb_InvalidPosition_RaisesException;

    [Test]
    procedure TestDeleteThumb_EmptyDB_RaisesException;

    [Test]
    procedure TestDeleteThumb_ByName_DeletesFromDB;

    [Test]
    procedure TestDeleteThumb_ByName_NotFound_RaisesException;

    { DeleteImage Tests }
    [Test]
    procedure TestDeleteImage_DeletesFileAndThumb;

    { ClearCache Tests }
    [Test]
    procedure TestClearCache_EmptiesDB;

    { MaintainCache Tests }
    [Test]
    procedure TestMaintainCache_RemovesOrphanThumbs;

    [Test]
    procedure TestMaintainCache_RemovesThumbsForDeletedImages;

    { SaveDB/LoadDB Tests }
    [Test]
    procedure TestSaveLoadDB_PersistsData;

    { ThumbWidth/ThumbHeight Tests }
    [Test]
    procedure TestSetThumbWidth_ClearsCache;

    [Test]
    procedure TestSetThumbHeight_ClearsCache;

    { FormatName Tests }
    [Test]
    procedure TestFormatName_GeneratesCorrectName;

    [Test]
    procedure TestFormatName_UsesCorrectExtension;
  end;

implementation

uses
  LightVcl.Graph.Cache,
  LightCore.IO;

type
  { Reaches the protected AddToCache / DeleteThumb / DeleteImage of TCacheObj }
  TCacheObjCracker = class(TCacheObj);

{ TTestGraphCache }

procedure TTestGraphCache.Setup;
begin
  FTestFolder:= TPath.Combine(TPath.GetTempPath, 'LightSaberTestCache');
  FCacheFolder:= TPath.Combine(FTestFolder, 'Cache');
  FTestImagePath:= TPath.Combine(FTestFolder, 'TestImage1.bmp');
  FTestImagePath2:= TPath.Combine(FTestFolder, 'TestImage2.bmp');

  ForceDirectoriesE(FTestFolder);
  ForceDirectoriesE(FCacheFolder);

  { Create test images }
  CreateTestImage(FTestImagePath, 800, 600);
  CreateTestImage(FTestImagePath2, 640, 480);
end;

procedure TTestGraphCache.TearDown;
begin
  CleanupTestFiles;
end;

procedure TTestGraphCache.CreateTestImage(const APath: string; AWidth, AHeight: Integer);
var
  BMP: TBitmap;
begin
  BMP:= TBitmap.Create;
  try
    BMP.Width:= AWidth;
    BMP.Height:= AHeight;
    BMP.PixelFormat:= pf24bit;
    BMP.Canvas.Brush.Color:= clRed;
    BMP.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
    BMP.SaveToFile(APath);
  finally
    FreeAndNil(BMP);
  end;
end;

procedure TTestGraphCache.CleanupTestFiles;
begin
  if DirectoryExists(FTestFolder)
  then TDirectory.Delete(FTestFolder, True);
end;

{ Constructor Tests }

procedure TTestGraphCache.TestCreate_ValidFolder;
var
  Cache: TCacheObj;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Assert.IsNotNull(Cache, 'Cache object should be created');
    Assert.AreEqual(Trail(FCacheFolder), Cache.CacheFolder, 'CacheFolder should match');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestCreate_EmptyFolder_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      TCacheObj.Create('').Free;
    end,
    Exception,
    'Empty folder should raise exception');
end;

{ CacheFolder Property Tests }

procedure TTestGraphCache.TestSetCacheFolder_EmptyValue_RaisesException;
var
  Cache: TCacheObj;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Assert.WillRaise(
      procedure
      begin
        Cache.CacheFolder:= '';
      end,
      Exception,
      'Empty value should raise exception');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestSetCacheFolder_ValidFolder_TrailsPath;
var
  Cache: TCacheObj;
  NewFolder: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    NewFolder:= TPath.Combine(FTestFolder, 'NewCache');
    Cache.CacheFolder:= NewFolder;
    Assert.AreEqual(Trail(NewFolder), Cache.CacheFolder, 'Path should be trailed');
  finally
    FreeAndNil(Cache);
  end;
end;

{ AddToCache / GetThumbFor Tests }

{ A new TCacheObj starts with an empty DB (the constructor does not call LoadDB), so the first thumbnail is number 1: '000000001.JPG'.
  ExtractThumbnail scales the 800x600 image to the default ThumbWidth of 128, so the thumbnail is 128x96 }
procedure TTestGraphCache.TestGetThumbFor_NewImage_CreatesThumb;
var
  Cache: TCacheObj;
  ThumbPath: string;
  Jpg: TJPEGImage;
  BMP: TBitmap;
  Pixel: Integer;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    ThumbPath:= Cache.GetThumbFor(FTestImagePath);
    Assert.AreEqual(Trail(FCacheFolder) + '000000001.JPG', ThumbPath, 'The first thumbnail must be 000000001.JPG in the cache folder');
    Assert.IsTrue(FileExists(ThumbPath), 'Thumbnail file should exist');
  finally
    FreeAndNil(Cache);
  end;

  Jpg:= TJPEGImage.Create;
  try
    Jpg.LoadFromFile(ThumbPath);
    Assert.AreEqual(128, Jpg.Width,  'The thumbnail must be ThumbWidth (128) wide');
    Assert.AreEqual(96,  Jpg.Height, 'The 4:3 image must keep its ratio: 96 high');

    BMP:= TBitmap.Create;
    try
      BMP.Assign(Jpg);
      Pixel:= ColorToRGB(BMP.Canvas.Pixels[64, 48]);
      { The source image is pure red; JPEG allows a few units of error per channel }
      Assert.AreEqual(Double(255), Double(Pixel AND $FF),          Double(8), 'The thumbnail must be red (red channel)');
      Assert.AreEqual(Double(0),   Double((Pixel SHR 8) AND $FF),  Double(8), 'The thumbnail must be red (green channel)');
      Assert.AreEqual(Double(0),   Double((Pixel SHR 16) AND $FF), Double(8), 'The thumbnail must be red (blue channel)');
    finally
      FreeAndNil(BMP);
    end;
  finally
    FreeAndNil(Jpg);
  end;
end;

procedure TTestGraphCache.TestGetThumbFor_ExistingThumb_ReturnsCached;
var
  Cache: TCacheObj;
  ThumbPath1, ThumbPath2: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    ThumbPath1:= Cache.GetThumbFor(FTestImagePath);
    ThumbPath2:= Cache.GetThumbFor(FTestImagePath);  { Request same image again }
    Assert.AreEqual(Trail(FCacheFolder) + '000000001.JPG', ThumbPath1, 'The first request must create thumbnail 1');
    Assert.AreEqual(ThumbPath1, ThumbPath2, 'Should return same cached thumbnail');
    Assert.IsFalse(FileExists(Trail(FCacheFolder) + '000000002.JPG'), 'The second request must not create a second thumbnail');
    Assert.AreEqual(0, Cache.ImagePosDB(FTestImagePath), 'The image must be in the DB once, at position 0');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestGetThumbFor_NonExistentFile_ReturnsEmpty;
var
  Cache: TCacheObj;
  ThumbPath: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    ThumbPath:= Cache.GetThumbFor(TPath.Combine(FTestFolder, 'NonExistent.bmp'));
    Assert.IsEmpty(ThumbPath, 'Should return empty for non-existent file');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestGetThumbFor_NonImageFile_ReturnsEmpty;
var
  Cache: TCacheObj;
  ThumbPath: string;
  TextFile: string;
begin
  TextFile:= TPath.Combine(FTestFolder, 'TextFile.txt');
  TFile.WriteAllText(TextFile, 'This is not an image');

  Cache:= TCacheObj.Create(FCacheFolder);
  try
    ThumbPath:= Cache.GetThumbFor(TextFile);
    Assert.IsEmpty(ThumbPath, 'Should return empty for non-image file');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestAddToCache_NonExistentFile_ReturnsEmpty;
var
  Cache: TCacheObjCracker;
  First, Second: string;
begin
  { AddToCache is called directly (GetThumbFor checks FileExists itself and never reaches it).
    A missing file must return '' and must not use up a thumbnail number. }
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    First:= Cache.AddToCache(FTestImagePath);
    Assert.IsNotEmpty(First, 'A real image must be added');

    Assert.IsEmpty(Cache.AddToCache(TPath.Combine(FTestFolder, 'NonExistent.bmp')), 'Should return empty for non-existent file');
    Assert.AreEqual(-1, Cache.ImagePosDB(TPath.Combine(FTestFolder, 'NonExistent.bmp')), 'A missing file must not enter the DB');

    Second:= Cache.AddToCache(FTestImagePath2);
    Assert.AreEqual(StrToInt(Copy(First, 1, 9)) + 1, StrToInt(Copy(Second, 1, 9)), 'The missing file must not consume a thumbnail number');
  finally
    FreeAndNil(Cache);
  end;
end;

{ ImagePosDB Tests }

procedure TTestGraphCache.TestImagePosDB_ExistingImage_ReturnsPosition;
var
  Cache: TCacheObj;
  Position: Integer;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);   { Position 0 }
    Cache.GetThumbFor(FTestImagePath2);  { Position 1 }
    Position:= Cache.ImagePosDB(LowerCase(FTestImagePath));
    Assert.AreEqual(0, Position, 'The first image added must be at position 0');
    Assert.AreEqual(1, Cache.ImagePosDB(LowerCase(FTestImagePath2)), 'The second image added must be at position 1');
    Assert.AreEqual(1, Cache.ImagePosDB(UpperCase(FTestImagePath2)), 'The lookup must ignore the case of the path');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestImagePosDB_NonExistingImage_ReturnsMinusOne;
var
  Cache: TCacheObj;
  Position: Integer;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Position:= Cache.ImagePosDB('c:\nonexistent\image.bmp');
    Assert.AreEqual(-1, Position, 'Should return -1 for non-existing image');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestImagePosDB_WithOutParam_SetsShortPath;
var
  Cache: TCacheObj;
  Position: Integer;
  ShortPath: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);   { Thumbnail 1, position 0 }
    Cache.GetThumbFor(FTestImagePath2);  { Thumbnail 2, position 1 }
    Position:= Cache.ImagePosDB(LowerCase(FTestImagePath2), ShortPath);
    Assert.AreEqual(1, Position, 'The second image must be at position 1');
    Assert.AreEqual('000000002.JPG', ShortPath, 'ShortPath must be the name of the second thumbnail');

    Position:= Cache.ImagePosDB(LowerCase(FTestImagePath), ShortPath);
    Assert.AreEqual(0, Position, 'The first image must be at position 0');
    Assert.AreEqual('000000001.JPG', ShortPath, 'ShortPath must be the name of the first thumbnail');
  finally
    FreeAndNil(Cache);
  end;
end;

{ ThumbPosDB Tests }

procedure TTestGraphCache.TestThumbPosDB_ExistingThumb_ReturnsPosition;
var
  Cache: TCacheObj;
  Position: Integer;
  ShortPath: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);   { 000000001.JPG, position 0 }
    Cache.GetThumbFor(FTestImagePath2);  { 000000002.JPG, position 1 }
    Cache.ImagePosDB(LowerCase(FTestImagePath2), ShortPath);  { Get short name }
    Assert.AreEqual('000000002.JPG', ShortPath, 'Precondition: the second thumbnail name');
    Position:= Cache.ThumbPosDB(LowerCase(ShortPath));
    Assert.AreEqual(1, Position, 'The second thumbnail must be at position 1 (lookup ignores case)');
    Assert.AreEqual(0, Cache.ThumbPosDB('000000001.JPG'), 'The first thumbnail must be at position 0');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestThumbPosDB_NonExistingThumb_ReturnsMinusOne;
var
  Cache: TCacheObj;
  Position: Integer;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Position:= Cache.ThumbPosDB('nonexistent.jpg');
    Assert.AreEqual(-1, Position, 'Should return -1 for non-existing thumb');
  finally
    FreeAndNil(Cache);
  end;
end;

{ DeleteThumb Tests }

procedure TTestGraphCache.TestDeleteThumb_ByPosition_DeletesFromDB;
var
  Cache: TCacheObjCracker;
  Position: Integer;
  ThumbPath1, ThumbPath2: string;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    ThumbPath1:= Cache.GetThumbFor(FTestImagePath);
    ThumbPath2:= Cache.GetThumbFor(FTestImagePath2);
    Position:= Cache.ImagePosDB(FTestImagePath);
    Assert.IsTrue(Position >= 0, 'Image should be in cache');

    Assert.IsTrue(Cache.DeleteThumb(Position), 'DeleteThumb must report the thumbnail file as deleted');
    Assert.AreEqual(-1, Cache.ImagePosDB(FTestImagePath), 'Image should no longer be in the DB');
    Assert.IsFalse(FileExists(ThumbPath1), 'The thumbnail file must be deleted from disk');

    { The other entry is untouched }
    Assert.IsTrue(Cache.ImagePosDB(FTestImagePath2) >= 0, 'The other image must stay in the DB');
    Assert.IsTrue(FileExists(ThumbPath2), 'The other thumbnail file must stay on disk');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestDeleteThumb_InvalidPosition_RaisesException;
var
  Cache: TCacheObjCracker;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);  { One entry: position 0 }
    Assert.WillRaise(
      procedure
      begin
        Cache.DeleteThumb(-1);
      end,
      Exception,
      'A negative position must raise');
    Assert.WillRaise(
      procedure
      begin
        Cache.DeleteThumb(1);
      end,
      Exception,
      'A position past the end must raise');
    Assert.IsTrue(Cache.ImagePosDB(FTestImagePath) >= 0, 'A refused delete must leave the entry in the DB');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestDeleteThumb_EmptyDB_RaisesException;
var
  Cache: TCacheObjCracker;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    Assert.WillRaise(
      procedure
      begin
        Cache.DeleteThumb(0);
      end,
      Exception,
      'DeleteThumb on an empty DB must raise');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestDeleteThumb_ByName_DeletesFromDB;
var
  Cache: TCacheObjCracker;
  ThumbPath, ShortPath: string;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    ThumbPath:= Cache.GetThumbFor(FTestImagePath);
    Assert.IsTrue(Cache.ImagePosDB(FTestImagePath, ShortPath) >= 0, 'Image should be in cache');

    Assert.IsTrue(Cache.DeleteThumb(ShortPath), 'DeleteThumb(name) must report the thumbnail file as deleted');
    Assert.AreEqual(-1, Cache.ImagePosDB(FTestImagePath), 'Should be removed from the DB');
    Assert.AreEqual(-1, Cache.ThumbPosDB(ShortPath), 'The thumb name should be removed from the DB');
    Assert.IsFalse(FileExists(ThumbPath), 'The thumbnail file must be deleted from disk');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestDeleteThumb_ByName_NotFound_RaisesException;
var
  Cache: TCacheObjCracker;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);
    Assert.WillRaise(
      procedure
      begin
        Cache.DeleteThumb('999999999.JPG');
      end,
      Exception,
      'DeleteThumb(name) must raise when the name is not in the DB');
    Assert.IsTrue(Cache.ImagePosDB(FTestImagePath) >= 0, 'A refused delete must leave the other entry in the DB');
  finally
    FreeAndNil(Cache);
  end;
end;

{ DeleteImage Tests }

procedure TTestGraphCache.TestDeleteImage_DeletesFileAndThumb;
var
  Cache: TCacheObjCracker;
  TempImage, ThumbPath: string;
begin
  { Create a temporary image that can be deleted }
  TempImage:= TPath.Combine(FTestFolder, 'TempToDelete.bmp');
  CreateTestImage(TempImage, 100, 100);

  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    ThumbPath:= Cache.GetThumbFor(TempImage);  { Add to cache }
    Assert.IsTrue(Cache.ImagePosDB(TempImage) >= 0, 'Should be in cache');
    Assert.IsTrue(FileExists(ThumbPath), 'The thumbnail must exist before the delete');

    Assert.IsTrue(Cache.DeleteImage(TempImage), 'DeleteImage must report the image as deleted');
    Assert.IsFalse(FileExists(TempImage), 'The original image must be deleted from disk');
    Assert.AreEqual(-1, Cache.ImagePosDB(TempImage), 'The image must be removed from the DB');
    Assert.IsFalse(FileExists(ThumbPath), 'The thumbnail file must be deleted from disk');
  finally
    FreeAndNil(Cache);
  end;
end;

{ ClearCache Tests }

procedure TTestGraphCache.TestClearCache_EmptiesDB;
var
  Cache: TCacheObj;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);
    Cache.GetThumbFor(FTestImagePath2);

    Assert.IsTrue(Cache.ImagePosDB(LowerCase(FTestImagePath)) >= 0, 'Image1 should be cached');
    Assert.IsTrue(Cache.ImagePosDB(LowerCase(FTestImagePath2)) >= 0, 'Image2 should be cached');

    Cache.ClearCache;

    Assert.AreEqual(-1, Cache.ImagePosDB(LowerCase(FTestImagePath)), 'Image1 should be removed');
    Assert.AreEqual(-1, Cache.ImagePosDB(LowerCase(FTestImagePath2)), 'Image2 should be removed');
  finally
    FreeAndNil(Cache);
  end;
end;

{ MaintainCache Tests }

procedure TTestGraphCache.TestMaintainCache_RemovesOrphanThumbs;
var
  Cache: TCacheObj;
  OrphanFile, ThumbPath: string;
  DeletedCount: Integer;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    { A real entry first, so MaintainCache takes its non-empty-DB branch, which compares each file on disk with the DB }
    ThumbPath:= Cache.GetThumbFor(FTestImagePath);
    Assert.IsTrue(FileExists(ThumbPath), 'Precondition: the real thumbnail exists');

    { Create an orphan thumbnail file that's not in DB }
    OrphanFile:= TPath.Combine(FCacheFolder, 'orphan.jpg');
    TFile.WriteAllText(OrphanFile, 'fake data');

    DeletedCount:= Cache.MaintainCache;
    Assert.AreEqual(1, DeletedCount, 'Exactly the one orphan file must be counted');
    Assert.IsFalse(FileExists(OrphanFile), 'Orphan file should be deleted');
    Assert.IsTrue(FileExists(ThumbPath), 'The thumbnail that is in the DB must stay on disk');
    Assert.AreEqual(0, Cache.ImagePosDB(FTestImagePath), 'The real entry must stay in the DB');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestMaintainCache_RemovesThumbsForDeletedImages;
var
  Cache: TCacheObj;
  TempImage: string;
  DeletedCount: Integer;
begin
  TempImage:= TPath.Combine(FTestFolder, 'ToBeDeleted.bmp');
  CreateTestImage(TempImage, 100, 100);

  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(TempImage);  { Add to cache }
    Assert.IsTrue(Cache.ImagePosDB(LowerCase(TempImage)) >= 0, 'Should be in cache');

    { Delete the original image }
    DeleteFile(TempImage);
    Assert.IsFalse(FileExists(TempImage), 'Original should be deleted');

    { Run maintenance }
    DeletedCount:= Cache.MaintainCache;
    Assert.IsTrue(DeletedCount >= 1, 'Should report deleted entries');
    Assert.AreEqual(-1, Cache.ImagePosDB(LowerCase(TempImage)), 'Entry should be removed from DB');
  finally
    FreeAndNil(Cache);
  end;
end;

{ SaveDB/LoadDB Tests }

procedure TTestGraphCache.TestSaveLoadDB_PersistsData;
var
  Cache1, Cache2: TCacheObj;
  Position: Integer;
begin
  { First cache instance - add data and save }
  Cache1:= TCacheObj.Create(FCacheFolder);
  try
    Cache1.GetThumbFor(FTestImagePath);
    Cache1.SaveDB;
  finally
    FreeAndNil(Cache1);  { Destructor also calls SaveDB }
  end;

  { Second cache instance - load and verify }
  Cache2:= TCacheObj.Create(FCacheFolder);
  try
    Cache2.LoadDB;
    Position:= Cache2.ImagePosDB(LowerCase(FTestImagePath));
    Assert.IsTrue(Position >= 0, 'Data should persist across instances');
  finally
    FreeAndNil(Cache2);
  end;
end;

{ ThumbWidth/ThumbHeight Tests }

procedure TTestGraphCache.TestSetThumbWidth_ClearsCache;
var
  Cache: TCacheObj;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);
    Assert.IsTrue(Cache.ImagePosDB(LowerCase(FTestImagePath)) >= 0, 'Should be cached');

    Cache.ThumbWidth:= 256;  { Change width - should clear cache }

    Assert.AreEqual(-1, Cache.ImagePosDB(LowerCase(FTestImagePath)),
      'Cache should be cleared when ThumbWidth changes');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestSetThumbHeight_ClearsCache;
var
  Cache: TCacheObj;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);
    Assert.IsTrue(Cache.ImagePosDB(LowerCase(FTestImagePath)) >= 0, 'Should be cached');

    Cache.ThumbHeight:= 192;  { Change height - should clear cache }

    Assert.AreEqual(-1, Cache.ImagePosDB(LowerCase(FTestImagePath)),
      'Cache should be cleared when ThumbHeight changes');
  finally
    FreeAndNil(Cache);
  end;
end;

{ FormatName Tests }

procedure TTestGraphCache.TestFormatName_GeneratesCorrectName;
var
  Cache: TCacheObjCracker;
  ShortPath: string;
begin
  Cache:= TCacheObjCracker.Create(FCacheFolder);
  try
    Cache.GetThumbFor(FTestImagePath);
    Cache.ImagePosDB(LowerCase(FTestImagePath), ShortPath);

    { 9 digits with leading zeros + extension }
    Assert.AreEqual('000000001.JPG', ShortPath, 'The first thumbnail name');
    Assert.AreEqual('000000042.JPG', Cache.FormatName(42, 9), 'The example of the routine header');
    Assert.AreEqual('123456789.JPG', Cache.FormatName(123456789, 9), 'A number of 9 digits needs no zeros');
    Assert.AreEqual('00007.JPG',     Cache.FormatName(7, 5), 'NameLength sets the number of digits');
  finally
    FreeAndNil(Cache);
  end;
end;

procedure TTestGraphCache.TestFormatName_UsesCorrectExtension;
var
  Cache: TCacheObj;
  ThumbPath, ShortPath: string;
begin
  Cache:= TCacheObj.Create(FCacheFolder);
  try
    { Default is JPEG }
    Cache.ThumbsAreBitmaps:= FALSE;
    Cache.ClearCache;
    ThumbPath:= Cache.GetThumbFor(FTestImagePath);
    Cache.ImagePosDB(LowerCase(FTestImagePath), ShortPath);
    Assert.IsTrue(ShortPath.EndsWith('.JPG'), 'Should use .JPG extension');

    { Switch to BMP }
    Cache.ThumbsAreBitmaps:= TRUE;
    Cache.ClearCache;
    ThumbPath:= Cache.GetThumbFor(FTestImagePath);
    Cache.ImagePosDB(LowerCase(FTestImagePath), ShortPath);
    Assert.IsTrue(ShortPath.EndsWith('.BMP'), 'Should use .BMP extension');
  finally
    FreeAndNil(Cache);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestGraphCache);

end.
