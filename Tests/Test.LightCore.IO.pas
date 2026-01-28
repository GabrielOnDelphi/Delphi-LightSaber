unit Test.LightCore.IO;

{=============================================================================================================
   Unit tests for LightCore.IO.pas
   Tests file/folder operations, path manipulation, and file type detection
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  LightCore.IO;

type
  [TestFixture]
  TTestLightCoreIO = class
  private
    FTestDir: string;
    FTestFile: string;
    procedure CleanupTestDir;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Path Conversion Tests }
    [Test]
    procedure TestConvert2LinuxPath;

    [Test]
    procedure TestConvert2DosPath;

    [Test]
    procedure TestTrailLinuxPath;

    { Trail Tests }
    [Test]
    procedure TestTrail;

    { Path Processing Tests }
    [Test]
    procedure TestExtractLastFolder;

    [Test]
    procedure TestExtractParentFolder;

    [Test]
    procedure TestTrimLastFolder;

    [Test]
    procedure TestSameFolder;

    { Filename Manipulation Tests }
    [Test]
    procedure TestAppendToFileName;

    [Test]
    procedure TestAppendBeforeName;

    [Test]
    procedure TestReplaceOnlyName;

    [Test]
    procedure TestExtractOnlyName;

    [Test]
    procedure TestRemoveLastExtension;

    [Test]
    procedure TestForceExtension;

    [Test]
    procedure TestExtractFileExtUp;

    { Auto-naming Tests }
    [Test]
    procedure TestIncrementFileName;

    [Test]
    procedure TestFileEndsInNumber;

    [Test]
    procedure TestAppendNumber2Filename;

    { Directory Operations }
    [Test]
    procedure TestForceDirectoriesB;

    [Test]
    procedure TestListFilesOf;

    [Test]
    procedure TestListDirectoriesOf;

    [Test]
    procedure TestFolderIsEmpty;

    { File Type Detection }
    [Test]
    procedure TestIsJpg;

    [Test]
    procedure TestIsPNG;

    [Test]
    procedure TestIsBMP;

    [Test]
    procedure TestIsText;

    [Test]
    procedure TestIsDelphi;

    [Test]
    procedure TestIsVideo;

    { Path Validity }
    [Test]
    procedure TestPathNameIsValid;

    [Test]
    procedure TestCorrectFilename;

    [Test]
    procedure TestCorrectFolder;

    { File Operations }
    [Test]
    procedure TestCopyFile;

    [Test]
    procedure TestGetFileSize;

    { Date/Time Formatting }
    [Test]
    procedure TestDateToStr_IO;

    [Test]
    procedure TestTimeToStr_IO;

    { Special Folders }
    [Test]
    procedure TestGetTempFolder;

    [Test]
    procedure TestGetMyDocuments;
  end;

implementation


procedure TTestLightCoreIO.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'LightCoreIOTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  FTestFile:= TPath.Combine(FTestDir, 'test.txt');
  TFile.WriteAllText(FTestFile, 'Test content');
end;


procedure TTestLightCoreIO.TearDown;
begin
  CleanupTestDir;
end;


procedure TTestLightCoreIO.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


{ Path Conversion Tests }

procedure TTestLightCoreIO.TestConvert2LinuxPath;
begin
  Assert.AreEqual('/test/path/file.txt', Convert2LinuxPath('c:\test\path\file.txt'));
  Assert.AreEqual('/folder/', Convert2LinuxPath('c:\folder\'));
end;

procedure TTestLightCoreIO.TestConvert2DosPath;
begin
  Assert.AreEqual('\test\path\file.txt', Convert2DosPath('/test/path/file.txt'));
end;

procedure TTestLightCoreIO.TestTrailLinuxPath;
begin
  Assert.AreEqual('/test/path/', TrailLinuxPath('/test/path'));
  Assert.AreEqual('/test/path/', TrailLinuxPath('/test/path/'));
end;


{ Trail Tests }

procedure TTestLightCoreIO.TestTrail;
begin
  Assert.AreEqual('c:\test\', Trail('c:\test'));
  Assert.AreEqual('c:\test\', Trail('c:\test\'));
end;


{ Path Processing Tests }

procedure TTestLightCoreIO.TestExtractLastFolder;
begin
  Assert.AreEqual('system', ExtractLastFolder('c:\windows\system\'));
  Assert.AreEqual('folder', ExtractLastFolder('c:\path\folder'));
end;

procedure TTestLightCoreIO.TestExtractParentFolder;
begin
  Assert.AreEqual('c:\windows\', ExtractParentFolder('c:\windows\system\'));
end;

procedure TTestLightCoreIO.TestTrimLastFolder;
begin
  Assert.AreEqual('c:\windows\', TrimLastFolder('c:\windows\system\'));
end;

procedure TTestLightCoreIO.TestSameFolder;
begin
  Assert.IsTrue(SameFolder('c:\Test\', 'c:\test\'));
  Assert.IsTrue(SameFolder('c:\Test', 'c:\Test\'));
  Assert.IsFalse(SameFolder('c:\Test1\', 'c:\Test2\'));
end;


{ Filename Manipulation Tests }

procedure TTestLightCoreIO.TestAppendToFileName;
begin
  Assert.AreEqual('c:\test\file_backup.txt', AppendToFileName('c:\test\file.txt', '_backup'));
  Assert.AreEqual('Shell32 backup.DLL', AppendToFileName('Shell32.DLL', ' backup'));
end;

procedure TTestLightCoreIO.TestAppendBeforeName;
begin
  Assert.AreEqual('c:\backup_Shell32.DLL', AppendBeforeName('c:\Shell32.DLL', 'backup_'));
end;

procedure TTestLightCoreIO.TestReplaceOnlyName;
begin
  Assert.AreEqual('c:\path\NewName.dll', ReplaceOnlyName('c:\path\OldName.dll', 'NewName'));
end;

procedure TTestLightCoreIO.TestExtractOnlyName;
begin
  Assert.AreEqual('file', ExtractOnlyName('c:\path\file.txt'));
  Assert.AreEqual('archive.tar', ExtractOnlyName('c:\path\archive.tar.gz'));
end;

procedure TTestLightCoreIO.TestRemoveLastExtension;
begin
  Assert.AreEqual('c:\path\file', RemoveLastExtension('c:\path\file.txt'));
  Assert.AreEqual('c:\path\archive.tar', RemoveLastExtension('c:\path\archive.tar.gz'));
end;

procedure TTestLightCoreIO.TestForceExtension;
begin
  Assert.AreEqual('file.txt', ForceExtension('file', '.txt'));
  Assert.AreEqual('file.txt', ForceExtension('file.doc', '.txt'));
end;

procedure TTestLightCoreIO.TestExtractFileExtUp;
begin
  Assert.AreEqual('.TXT', ExtractFileExtUp('file.txt'));
  Assert.AreEqual('.TXT', ExtractFileExtUp('file.TXT'));
end;


{ Auto-naming Tests }

procedure TTestLightCoreIO.TestIncrementFileName;
begin
  Assert.AreEqual('file02.txt', IncrementFileName('file01.txt'));
  Assert.AreEqual('test10.txt', IncrementFileName('test09.txt'));
end;

procedure TTestLightCoreIO.TestFileEndsInNumber;
begin
  Assert.IsTrue(FileEndsInNumber('file01.txt'));
  Assert.IsTrue(FileEndsInNumber('test99.doc'));
  Assert.IsFalse(FileEndsInNumber('file.txt'));
end;

procedure TTestLightCoreIO.TestAppendNumber2Filename;
begin
  Assert.AreEqual('Log1.txt', AppendNumber2Filename('Log.txt', 1, 1));
  Assert.AreEqual('Log001.txt', AppendNumber2Filename('Log.txt', 1, 3));
end;


{ Directory Operations }

procedure TTestLightCoreIO.TestForceDirectoriesB;
var
  TestPath: string;
begin
  TestPath:= TPath.Combine(FTestDir, 'subdir1\subdir2\subdir3');
  Assert.IsTrue(ForceDirectoriesB(TestPath));
  Assert.IsTrue(TDirectory.Exists(TestPath));
end;

procedure TTestLightCoreIO.TestListFilesOf;
var
  Files: TStringList;
begin
  { Create some test files }
  TFile.WriteAllText(TPath.Combine(FTestDir, 'file1.txt'), 'content1');
  TFile.WriteAllText(TPath.Combine(FTestDir, 'file2.txt'), 'content2');

  Files:= ListFilesOf(FTestDir, '*.txt', TRUE, FALSE);
  try
    Assert.IsTrue(Files.Count >= 2);
  finally
    Files.Free;
  end;
end;

procedure TTestLightCoreIO.TestListDirectoriesOf;
var
  Dirs: TStringList;
begin
  { Create some test subdirectories }
  TDirectory.CreateDirectory(TPath.Combine(FTestDir, 'subdir1'));
  TDirectory.CreateDirectory(TPath.Combine(FTestDir, 'subdir2'));

  Dirs:= ListDirectoriesOf(FTestDir, TRUE, FALSE);
  try
    Assert.AreEqual(2, Dirs.Count);
  finally
    Dirs.Free;
  end;
end;

procedure TTestLightCoreIO.TestFolderIsEmpty;
var
  EmptyDir: string;
begin
  EmptyDir:= TPath.Combine(FTestDir, 'emptydir');
  TDirectory.CreateDirectory(EmptyDir);

  Assert.IsTrue(FolderIsEmpty(EmptyDir));
  Assert.IsFalse(FolderIsEmpty(FTestDir));  { Has test.txt }
end;


{ File Type Detection }

procedure TTestLightCoreIO.TestIsJpg;
begin
  Assert.IsTrue(IsJpg('photo.jpg'));
  Assert.IsTrue(IsJpg('photo.jpeg'));
  Assert.IsTrue(IsJpg('photo.JPG'));
  Assert.IsFalse(IsJpg('photo.png'));
end;

procedure TTestLightCoreIO.TestIsPNG;
begin
  Assert.IsTrue(IsPNG('image.png'));
  Assert.IsTrue(IsPNG('image.PNG'));
  Assert.IsFalse(IsPNG('image.jpg'));
end;

procedure TTestLightCoreIO.TestIsBMP;
begin
  Assert.IsTrue(IsBMP('image.bmp'));
  Assert.IsTrue(IsBMP('image.BMP'));
  Assert.IsFalse(IsBMP('image.png'));
end;

procedure TTestLightCoreIO.TestIsText;
begin
  Assert.IsTrue(IsText('file.txt'));
  Assert.IsTrue(IsText('file.TXT'));
  Assert.IsFalse(IsText('file.exe'));
end;

procedure TTestLightCoreIO.TestIsDelphi;
begin
  Assert.IsTrue(IsDelphi('unit.pas'));
  Assert.IsTrue(IsDelphi('project.dpr'));
  Assert.IsTrue(IsDelphi('package.dpk'));
  Assert.IsTrue(IsDelphi('form.dfm'));
  Assert.IsFalse(IsDelphi('file.txt'));
end;

procedure TTestLightCoreIO.TestIsVideo;
begin
  Assert.IsTrue(IsVideoGeneric('movie.mp4'));
  Assert.IsTrue(IsVideoGeneric('movie.avi'));
  Assert.IsTrue(IsVideoGeneric('movie.mkv'));
  Assert.IsFalse(IsVideoGeneric('image.jpg'));
end;


{ Path Validity }

procedure TTestLightCoreIO.TestPathNameIsValid;
begin
  Assert.IsTrue(PathNameIsValid('c:\valid\path\'));
  Assert.IsFalse(PathNameIsValid('c:\invalid<path\'));
  Assert.IsFalse(PathNameIsValid('c:\invalid>path\'));
end;

procedure TTestLightCoreIO.TestCorrectFilename;
begin
  Assert.AreEqual('file name.txt', CorrectFilename('file<name.txt'));
  Assert.AreEqual('file name.txt', CorrectFilename('file>name.txt'));
  Assert.AreEqual('file name.txt', CorrectFilename('file:name.txt'));
end;

procedure TTestLightCoreIO.TestCorrectFolder;
begin
  Assert.AreEqual('folder name', CorrectFolder('folder<name'));
end;


{ File Operations }

procedure TTestLightCoreIO.TestCopyFile;
var
  DestFile: string;
begin
  DestFile:= TPath.Combine(FTestDir, 'copied.txt');
  Assert.IsTrue(LightCore.IO.CopyFile(FTestFile, DestFile));
  Assert.IsTrue(FileExists(DestFile));
end;

procedure TTestLightCoreIO.TestGetFileSize;
begin
  Assert.IsTrue(GetFileSize(FTestFile) > 0);
end;


{ Date/Time Formatting }

procedure TTestLightCoreIO.TestDateToStr_IO;
var
  TestDate: TDateTime;
  Result: string;
begin
  TestDate:= EncodeDate(2025, 6, 15);
  Result:= DateToStr_IO(TestDate);
  Assert.IsTrue(Pos('2025', Result) > 0);
  Assert.IsTrue(Pos('-', Result) > 0);  { Uses dashes, not slashes }
end;

procedure TTestLightCoreIO.TestTimeToStr_IO;
var
  TestTime: TDateTime;
  Result: string;
begin
  TestTime:= EncodeTime(14, 30, 45, 0);
  Result:= TimeToStr_IO(TestTime);
  Assert.IsFalse(Pos(':', Result) > 0);  { Uses safe characters }
end;


{ Special Folders }

procedure TTestLightCoreIO.TestGetTempFolder;
var
  TempFolder: string;
begin
  TempFolder:= GetTempFolder;
  Assert.IsNotEmpty(TempFolder);
  Assert.IsTrue(TDirectory.Exists(TempFolder));
end;

procedure TTestLightCoreIO.TestGetMyDocuments;
var
  DocsFolder: string;
begin
  DocsFolder:= GetMyDocuments;
  Assert.IsNotEmpty(DocsFolder);
  Assert.IsTrue(TDirectory.Exists(DocsFolder));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreIO);

end.
