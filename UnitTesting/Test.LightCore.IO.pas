unit Test.LightCore.IO;

{=============================================================================================================
   2026.01.30
   Unit tests for LightCore.IO.pas
   Tests file/folder operations, path manipulation, and file type detection

   Run with: TestInsight or DUnitX console runner
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

    [Test]
    procedure TestTrailLinuxPathEx;

    { Trail Tests }
    [Test]
    procedure TestTrail;

    [Test]
    procedure TestTrail_Empty;

    { Path Processing Tests }
    [Test]
    procedure TestExtractLastFolder;

    [Test]
    procedure TestExtractParentFolder;

    [Test]
    procedure TestTrimLastFolder;

    [Test]
    procedure TestSameFolder;

    [Test]
    procedure TestSameFolderFromFile;

    [Test]
    procedure TestIsSubfolder;

    [Test]
    procedure TestExtractFirstFolder;

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
    procedure TestForceExtension_NoLeadingDot;

    [Test]
    procedure TestExtractFileExtUp;

    [Test]
    procedure TestAppendFileExtension;

    [Test]
    procedure TestChangeFilePath;

    { Auto-naming Tests }
    [Test]
    procedure TestIncrementFileName;

    [Test]
    procedure TestIncrementFileNameEx;

    [Test]
    procedure TestFileEndsInNumber;

    [Test]
    procedure TestAppendNumber2Filename;

    [Test]
    procedure TestMakeUniqueFolderName;

    { Directory Operations }
    [Test]
    procedure TestForceDirectoriesB;

    [Test]
    procedure TestForceDirectories;

    [Test]
    procedure TestListFilesOf;

    [Test]
    procedure TestListDirectoriesOf;

    [Test]
    procedure TestListFilesAndFolderOf;

    [Test]
    procedure TestFolderIsEmpty;

    [Test]
    procedure TestDirectoryExists;

    [Test]
    procedure TestIsFolder;

    { File Type Detection }
    [Test]
    procedure TestIsJpg;

    [Test]
    procedure TestIsJp2;

    [Test]
    procedure TestIsPNG;

    [Test]
    procedure TestIsBMP;

    [Test]
    procedure TestIsGIF;

    [Test]
    procedure TestIsWebP;

    [Test]
    procedure TestIsImage;

    [Test]
    procedure TestIsText;

    [Test]
    procedure TestIsDocument;

    [Test]
    procedure TestIsDelphi;

    [Test]
    procedure TestIsExec;

    [Test]
    procedure TestIsVideo;

    [Test]
    procedure TestIsThisType;

    [Test]
    procedure TestExtensionToMimeType;

    [Test]
    procedure TestExtensionFromMimeType;

    { Path Validity }
    [Test]
    procedure TestPathNameIsValid;

    [Test]
    procedure TestCorrectFilename;

    [Test]
    procedure TestCorrectFolder;

    [Test]
    procedure TestIsUnicode;

    [Test]
    procedure TestCheckPathLength;

    [Test]
    procedure TestShortenFileName;

    { File Operations }
    [Test]
    procedure TestCopyFile;

    [Test]
    procedure TestFileCopyQuick;

    [Test]
    procedure TestFileMoveTo;

    [Test]
    procedure TestFileMoveToDir;

    [Test]
    procedure TestGetFileSize;

    [Test]
    procedure TestCompareFiles;

    [Test]
    procedure TestBytesToFile;

    { Backup Tests }
    [Test]
    procedure TestBackupFileBak;

    { Date/Time Formatting }
    [Test]
    procedure TestDateToStr_IO;

    [Test]
    procedure TestTimeToStr_IO;

    [Test]
    procedure TestDateTimeToStr_IO;

    { Drive Functions }
    [Test]
    procedure TestExtractDrive;

    [Test]
    procedure TestRemoveDrive;

    [Test]
    procedure TestValidDriveLetter;

    [Test]
    procedure TestDrive2Byte;

    [Test]
    procedure TestDrive2Char;

    { Special Folders }
    [Test]
    procedure TestGetTempFolder;

    [Test]
    procedure TestGetMyDocuments;

    [Test]
    procedure TestGetHomePath;

    { Utility Functions }
    [Test]
    procedure TestShortenText;

    [Test]
    procedure TestForcePathDelimiters;

    { Additional Special Folders }
    [Test]
    procedure TestGetLibraryPath;

    [Test]
    procedure TestGetCachePath;

    [Test]
    procedure TestGetPublicPath;

    { Time Extraction }
    [Test]
    procedure TestExtractTimeFromFileName;

    { Copy/Backup edge cases }
    [Test]
    procedure TestCopyFolder;

    [Test]
    procedure TestBackupFileIncrement;
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
  { Convert2LinuxPath only replaces \ with /, does not remove drive letter }
  Assert.AreEqual('c:/test/path/file.txt', Convert2LinuxPath('c:\test\path\file.txt'));
  Assert.AreEqual('c:/folder/', Convert2LinuxPath('c:\folder\'));
end;

procedure TTestLightCoreIO.TestConvert2DosPath;
begin
  Assert.AreEqual('\test\path\file.txt', Convert2DosPath('/test/path/file.txt'));
end;

procedure TTestLightCoreIO.TestTrailLinuxPath;
begin
  Assert.AreEqual('/test/path/', TrailLinuxPath('/test/path'));
  Assert.AreEqual('/test/path/', TrailLinuxPath('/test/path/'));
  Assert.AreEqual('', TrailLinuxPath(''));  { Empty returns empty }
end;

procedure TTestLightCoreIO.TestTrailLinuxPathEx;
begin
  Assert.AreEqual('/test/path/', TrailLinuxPathEx('test/path'));
  Assert.AreEqual('/test/path/', TrailLinuxPathEx('/test/path/'));
  Assert.AreEqual('', TrailLinuxPathEx(''));  { Empty returns empty }
end;


{ Trail Tests }

procedure TTestLightCoreIO.TestTrail;
begin
  Assert.AreEqual('c:\test\', Trail('c:\test'));
  Assert.AreEqual('c:\test\', Trail('c:\test\'));
end;

procedure TTestLightCoreIO.TestTrail_Empty;
begin
  Assert.AreEqual('', Trail(''));  { Empty returns empty, not crash }
end;


{ Path Processing Tests }

procedure TTestLightCoreIO.TestExtractLastFolder;
begin
  { ExtractLastFolder works on paths. 'c:\path\folder' is treated as a file, not folder }
  Assert.AreEqual('system', ExtractLastFolder('c:\windows\system\'));
  Assert.AreEqual('path', ExtractLastFolder('c:\path\folder'));  { 'folder' is treated as filename }
end;

procedure TTestLightCoreIO.TestExtractParentFolder;
begin
  { ExtractParentFolder uses TDirectory.GetParent which doesn't include trailing separator }
  Assert.AreEqual('c:\windows', ExtractParentFolder('c:\windows\system\'));
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

procedure TTestLightCoreIO.TestSameFolderFromFile;
begin
  Assert.IsTrue(SameFolderFromFile('c:\Test\file1.txt', 'c:\Test\file2.txt'));
  Assert.IsTrue(SameFolderFromFile('c:\Test\', 'c:\Test\file.txt'));
  Assert.IsFalse(SameFolderFromFile('c:\Test1\file.txt', 'c:\Test2\file.txt'));
end;

procedure TTestLightCoreIO.TestIsSubfolder;
begin
  Assert.IsTrue(IsSubfolder('c:\Test\Sub\', 'c:\Test\'));
  Assert.IsTrue(IsSubfolder('c:\Test\', 'c:\Test\Sub\'));  { Order doesn't matter }
  Assert.IsFalse(IsSubfolder('c:\Test1\', 'c:\Test2\'));
end;

procedure TTestLightCoreIO.TestExtractFirstFolder;
begin
  { ExtractFirstFolder returns the first folder after the drive letter }
  Assert.AreEqual('1\', ExtractFirstFolder('c:\1\2\3\'));
  Assert.AreEqual('', ExtractFirstFolder('c:\1'));  { Single folder }
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
  { RemoveLastExtension uses TPath.GetFileNameWithoutExtension which returns name only, no path }
  Assert.AreEqual('file', RemoveLastExtension('c:\path\file.txt'));
  Assert.AreEqual('archive.tar', RemoveLastExtension('c:\path\archive.tar.gz'));
end;

procedure TTestLightCoreIO.TestForceExtension;
begin
  Assert.AreEqual('file.txt', ForceExtension('file', '.txt'));
  Assert.AreEqual('file.txt', ForceExtension('file.doc', '.txt'));
end;

procedure TTestLightCoreIO.TestForceExtension_NoLeadingDot;
begin
  { ForceExtension accepts extension with or without leading dot }
  Assert.AreEqual('file.txt', ForceExtension('file', 'txt'));
end;

procedure TTestLightCoreIO.TestExtractFileExtUp;
begin
  Assert.AreEqual('.TXT', ExtractFileExtUp('file.txt'));
  Assert.AreEqual('.TXT', ExtractFileExtUp('file.TXT'));
  Assert.AreEqual('', ExtractFileExtUp('file'));  { No extension }
end;

procedure TTestLightCoreIO.TestAppendFileExtension;
begin
  Assert.AreEqual('file.txt.bak', AppendFileExtension('file.txt', '.bak'));
  Assert.AreEqual('file.bak', AppendFileExtension('file.bak', '.bak'));  { Already has extension }
end;

procedure TTestLightCoreIO.TestChangeFilePath;
begin
  Assert.AreEqual('d:\new\file.txt', ChangeFilePath('c:\old\file.txt', 'd:\new'));
  Assert.AreEqual('d:\new\file.txt', ChangeFilePath('c:\old\file.txt', 'd:\new\'));  { With trailing slash }
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
  { AppendNumber2Filename adds '_' before the number }
  Assert.AreEqual('Log_1.txt', AppendNumber2Filename('Log.txt', 1, 1));
  Assert.AreEqual('Log_001.txt', AppendNumber2Filename('Log.txt', 1, 3));
  Assert.AreEqual('Log_010.txt', AppendNumber2Filename('Log.txt', 10, 3));
end;

procedure TTestLightCoreIO.TestIncrementFileNameEx;
begin
  { IncrementFileNameEx adds number if doesn't exist, increments if it does }
  Assert.AreEqual('file_001.txt', IncrementFileNameEx('file.txt', 1, 3));  { Add number }
  Assert.AreEqual('file02.txt', IncrementFileNameEx('file01.txt', 1, 3)); { Increment existing }
end;

procedure TTestLightCoreIO.TestMakeUniqueFolderName;
var
  UniquePath: string;
begin
  { Should return base path if doesn't exist }
  UniquePath:= MakeUniqueFolderName(FTestDir, 'unique_folder');
  Assert.AreEqual(Trail(FTestDir) + 'unique_folder', UniquePath);
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

procedure TTestLightCoreIO.TestForceDirectories;
var
  TestPath: string;
begin
  TestPath:= TPath.Combine(FTestDir, 'newdir');
  Assert.AreEqual(1, ForceDirectories(TestPath));  { 1 = created successfully }
  Assert.AreEqual(0, ForceDirectories(TestPath));  { 0 = already exists }
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

procedure TTestLightCoreIO.TestListFilesAndFolderOf;
var
  Items: TStringList;
begin
  { Create test files and subdirectory }
  TFile.WriteAllText(TPath.Combine(FTestDir, 'file1.txt'), 'content');
  TDirectory.CreateDirectory(TPath.Combine(FTestDir, 'subdir'));

  Items:= ListFilesAndFolderOf(FTestDir, TRUE);
  try
    Assert.IsTrue(Items.Count >= 2);  { At least file1.txt and subdir }
  finally
    Items.Free;
  end;
end;

procedure TTestLightCoreIO.TestDirectoryExists;
begin
  Assert.IsTrue(LightCore.IO.DirectoryExists(FTestDir));
  Assert.IsFalse(LightCore.IO.DirectoryExists(FTestDir + ' '));  { Space at end - bug fix }
  Assert.IsFalse(LightCore.IO.DirectoryExists('c:\nonexistent_folder_xyz\'));
end;

procedure TTestLightCoreIO.TestIsFolder;
begin
  Assert.IsTrue(IsFolder(FTestDir));
  Assert.IsFalse(IsFolder(FTestFile));
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
  { IsDelphi checks for .pas, .dpr, .dpk only - NOT .dfm }
  Assert.IsTrue(IsDelphi('unit.pas'));
  Assert.IsTrue(IsDelphi('project.dpr'));
  Assert.IsTrue(IsDelphi('package.dpk'));
  Assert.IsFalse(IsDelphi('form.dfm'));  { .dfm is NOT included in IsDelphi }
  Assert.IsFalse(IsDelphi('file.txt'));
end;

procedure TTestLightCoreIO.TestIsVideo;
begin
  Assert.IsTrue(IsVideoGeneric('movie.mp4'));
  Assert.IsTrue(IsVideoGeneric('movie.avi'));
  Assert.IsTrue(IsVideoGeneric('movie.mkv'));
  Assert.IsTrue(IsVideoGeneric('movie.webm'));
  Assert.IsFalse(IsVideoGeneric('image.jpg'));
end;

procedure TTestLightCoreIO.TestIsJp2;
begin
  Assert.IsTrue(IsJp2('image.j2k'));
  Assert.IsTrue(IsJp2('image.JP2'));
  Assert.IsFalse(IsJp2('image.jpg'));
end;

procedure TTestLightCoreIO.TestIsGIF;
begin
  Assert.IsTrue(IsGIF('image.gif'));
  Assert.IsTrue(IsGIF('image.GIF'));
  Assert.IsFalse(IsGIF('image.png'));
end;

procedure TTestLightCoreIO.TestIsWebP;
begin
  Assert.IsTrue(IsWebP('image.webp'));
  Assert.IsTrue(IsWebP('image.WEBP'));
  Assert.IsFalse(IsWebP('image.jpg'));
end;

procedure TTestLightCoreIO.TestIsImage;
begin
  Assert.IsTrue(IsImage('photo.jpg'));
  Assert.IsTrue(IsImage('photo.png'));
  Assert.IsTrue(IsImage('photo.bmp'));
  Assert.IsTrue(IsImage('photo.gif'));
  Assert.IsTrue(IsImage('photo.webp'));
  Assert.IsFalse(IsImage('document.pdf'));
end;

procedure TTestLightCoreIO.TestIsDocument;
begin
  Assert.IsTrue(IsDocument('file.pdf'));
  Assert.IsTrue(IsDocument('file.doc'));
  Assert.IsTrue(IsDocument('file.docx'));
  Assert.IsFalse(IsDocument('file.txt'));
end;

procedure TTestLightCoreIO.TestIsExec;
begin
  Assert.IsTrue(IsExec('file.exe'));
  Assert.IsTrue(IsExec('file.bat'));
  Assert.IsTrue(IsExec('file.dll'));
  Assert.IsTrue(IsExec('file.ps1'));
  Assert.IsFalse(IsExec('file.txt'));
end;

procedure TTestLightCoreIO.TestIsThisType;
begin
  Assert.IsTrue(IsThisType('file.txt', 'txt'));
  Assert.IsTrue(IsThisType('file.TXT', 'txt'));  { Case insensitive }
  Assert.IsFalse(IsThisType('file.doc', 'txt'));
end;

procedure TTestLightCoreIO.TestExtensionToMimeType;
begin
  Assert.AreEqual('text/plain', ExtensionToMimeType('file.txt'));
  Assert.AreEqual('image/jpeg', ExtensionToMimeType('photo.jpg'));
  Assert.AreEqual('image/png', ExtensionToMimeType('image.png'));
  Assert.AreEqual('application/pdf', ExtensionToMimeType('doc.pdf'));
  Assert.AreEqual('application/octet-stream', ExtensionToMimeType('file.xyz'));
end;

procedure TTestLightCoreIO.TestExtensionFromMimeType;
begin
  Assert.AreEqual('.txt', ExtensionFromMimeType('text/plain'));
  Assert.AreEqual('.jpg', ExtensionFromMimeType('image/jpeg'));
  Assert.AreEqual('.png', ExtensionFromMimeType('image/png'));
  Assert.AreEqual('', ExtensionFromMimeType('application/unknown'));
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
  Assert.AreEqual('folder name', CorrectFolder('folder>name'));
end;

procedure TTestLightCoreIO.TestIsUnicode;
begin
  Assert.IsTrue(IsUnicode('\\?\c:\very\long\path'));
  Assert.IsFalse(IsUnicode('c:\normal\path'));
end;

procedure TTestLightCoreIO.TestCheckPathLength;
begin
  Assert.IsTrue(CheckPathLength('c:\short\path'));
  Assert.IsTrue(CheckPathLength('\\?\c:\very\long\path'));  { Extended prefix allowed }
end;

procedure TTestLightCoreIO.TestShortenFileName;
var
  ShortPath: string;
begin
  { Normal path should not be changed }
  Assert.AreEqual('c:\test\file.txt', ShortenFileName('c:\test\file.txt', 260));

  { Long path should be shortened }
  ShortPath:= ShortenFileName('c:\test\verylongfilename.txt', 20);
  Assert.IsTrue(Length(ShortPath) <= 20);
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
  Assert.AreEqual(Int64(-1), GetFileSize('c:\nonexistent_file.xyz'));  { Returns -1 for missing files }
end;

procedure TTestLightCoreIO.TestFileCopyQuick;
var
  SubDir, DestFile: string;
begin
  SubDir:= TPath.Combine(FTestDir, 'copydir');
  TDirectory.CreateDirectory(SubDir);

  Assert.IsTrue(FileCopyQuick(FTestFile, SubDir));
  DestFile:= TPath.Combine(SubDir, ExtractFileName(FTestFile));
  Assert.IsTrue(FileExists(DestFile));
end;

procedure TTestLightCoreIO.TestFileMoveTo;
var
  SrcFile, DestFile: string;
begin
  SrcFile:= TPath.Combine(FTestDir, 'move_src.txt');
  DestFile:= TPath.Combine(FTestDir, 'move_dest.txt');
  TFile.WriteAllText(SrcFile, 'content');

  Assert.IsTrue(FileMoveTo(SrcFile, DestFile));
  Assert.IsFalse(FileExists(SrcFile));
  Assert.IsTrue(FileExists(DestFile));
end;

procedure TTestLightCoreIO.TestFileMoveToDir;
var
  SrcFile, DestDir: string;
begin
  SrcFile:= TPath.Combine(FTestDir, 'movedir_src.txt');
  DestDir:= TPath.Combine(FTestDir, 'movedir_dest');
  TFile.WriteAllText(SrcFile, 'content');

  Assert.IsTrue(FileMoveToDir(SrcFile, DestDir));
  Assert.IsFalse(FileExists(SrcFile));
  Assert.IsTrue(TDirectory.Exists(DestDir));
end;

procedure TTestLightCoreIO.TestCompareFiles;
var
  File1, File2, File3: string;
begin
  File1:= TPath.Combine(FTestDir, 'compare1.txt');
  File2:= TPath.Combine(FTestDir, 'compare2.txt');
  File3:= TPath.Combine(FTestDir, 'compare3.txt');

  TFile.WriteAllText(File1, 'Same content');
  TFile.WriteAllText(File2, 'Same content');
  TFile.WriteAllText(File3, 'Different content');

  Assert.IsTrue(CompareFiles(File1, File2));
  Assert.IsFalse(CompareFiles(File1, File3));
end;

procedure TTestLightCoreIO.TestBytesToFile;
var
  TestPath: string;
  Data: TBytes;
begin
  TestPath:= TPath.Combine(FTestDir, 'bytes.bin');
  SetLength(Data, 4);
  Data[0]:= $41; Data[1]:= $42; Data[2]:= $43; Data[3]:= $44;  { 'ABCD' }

  Assert.IsTrue(BytesToFile(TestPath, Data));
  Assert.IsTrue(FileExists(TestPath));
  Assert.AreEqual(Int64(4), GetFileSize(TestPath));
end;

procedure TTestLightCoreIO.TestBackupFileBak;
var
  BakFile: string;
begin
  BakFile:= FTestFile + '.bak';

  Assert.IsTrue(BackupFileBak(FTestFile));
  Assert.IsTrue(FileExists(BakFile));
  Assert.IsTrue(FileExists(FTestFile));  { Original still exists }
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
  TimeResult: string;
begin
  TestTime:= EncodeTime(14, 30, 45, 0);
  TimeResult:= TimeToStr_IO(TestTime);
  Assert.IsFalse(Pos(':', TimeResult) > 0);  { Uses safe characters (periods instead) }
  Assert.IsTrue(Pos('.', TimeResult) > 0);   { Uses periods as separator }
end;

procedure TTestLightCoreIO.TestDateTimeToStr_IO;
var
  TestDateTime: TDateTime;
  DTResult: string;
begin
  TestDateTime:= EncodeDate(2025, 6, 15) + EncodeTime(14, 30, 0, 0);
  DTResult:= DateTimeToStr_IO(TestDateTime);
  Assert.IsTrue(Pos('2025', DTResult) > 0);
  Assert.IsTrue(Pos('-', DTResult) > 0);  { Uses dashes for date }
  Assert.IsTrue(Pos('.', DTResult) > 0);  { Uses periods for time }
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

procedure TTestLightCoreIO.TestGetHomePath;
var
  HomePath: string;
begin
  HomePath:= GetHomePath;
  Assert.IsNotEmpty(HomePath);
  Assert.IsTrue(TDirectory.Exists(HomePath));
end;


{ Drive Functions }

procedure TTestLightCoreIO.TestExtractDrive;
begin
  Assert.AreEqual('C', ExtractDrive('C:\Windows\System32'));
  Assert.AreEqual('D', ExtractDrive('D:\Data\Files'));
  Assert.AreEqual('', ExtractDrive('\\Server\Share'));  { UNC path has no drive letter }
end;

procedure TTestLightCoreIO.TestRemoveDrive;
begin
  Assert.AreEqual('Windows\System32', RemoveDrive('C:\Windows\System32'));
  Assert.AreEqual('Data\Files', RemoveDrive('D:\Data\Files'));
end;

procedure TTestLightCoreIO.TestValidDriveLetter;
begin
  Assert.IsTrue(ValidDriveLetter('C'));
  Assert.IsTrue(ValidDriveLetter('c'));  { Case insensitive }
  Assert.IsTrue(ValidDriveLetter('Z'));
  Assert.IsFalse(ValidDriveLetter('1'));
  Assert.IsFalse(ValidDriveLetter('?'));
end;

procedure TTestLightCoreIO.TestDrive2Byte;
begin
  Assert.AreEqual(Byte(1), Drive2Byte('A'));
  Assert.AreEqual(Byte(3), Drive2Byte('C'));
  Assert.AreEqual(Byte(26), Drive2Byte('Z'));
end;

procedure TTestLightCoreIO.TestDrive2Char;
begin
  Assert.AreEqual('A', Drive2Char(1));
  Assert.AreEqual('C', Drive2Char(3));
  Assert.AreEqual('Z', Drive2Char(26));
end;


{ Utility Functions }

procedure TTestLightCoreIO.TestShortenText;
var
  ShortenedResult: string;
begin
  { No change if already short }
  Assert.AreEqual('short', ShortenPath('short', 20));

  { Test shortening - result should have ellipsis in middle }
  ShortenedResult:= ShortenPath('c:\path\to\deep\folder', 16);
  Assert.IsTrue(Length(ShortenedResult) <= 16);
  Assert.IsTrue(Pos('...', ShortenedResult) > 0);  { Should contain ellipsis }

  { Test that first character is preserved (bug fix verification) }
  ShortenedResult:= ShortenPath('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 12);
  Assert.AreEqual('A', ShortenedResult[1]);  { First char should be 'A', not empty }
  Assert.IsTrue(Pos('Z', ShortenedResult) > 0);  { Last part should include 'Z' }
end;

procedure TTestLightCoreIO.TestForcePathDelimiters;
begin
  Assert.AreEqual('/path/', ForcePathDelimiters('path', '/', TRUE, TRUE));
  Assert.AreEqual('/path', ForcePathDelimiters('path', '/', TRUE, FALSE));
  Assert.AreEqual('path/', ForcePathDelimiters('path', '/', FALSE, TRUE));
  Assert.AreEqual('', ForcePathDelimiters('', '/', TRUE, TRUE));  { Empty stays empty }
end;


{ Additional Special Folders Tests }

procedure TTestLightCoreIO.TestGetLibraryPath;
var
  LibPath: string;
begin
  LibPath:= GetLibraryPath;
  Assert.IsNotEmpty(LibPath);
  { On Windows, this returns the application's folder }
  {$IFDEF MSWINDOWS}
  Assert.IsTrue(TDirectory.Exists(LibPath));
  {$ENDIF}
end;

procedure TTestLightCoreIO.TestGetCachePath;
var
  CachePath: string;
begin
  CachePath:= GetCachePath;
  Assert.IsNotEmpty(CachePath);
  Assert.IsTrue(TDirectory.Exists(CachePath));
end;

procedure TTestLightCoreIO.TestGetPublicPath;
var
  PublicPath: string;
begin
  PublicPath:= GetPublicPath;
  { On Windows, this should return something like C:\ProgramData }
  {$IFDEF MSWINDOWS}
  Assert.IsNotEmpty(PublicPath);
  Assert.IsTrue(TDirectory.Exists(PublicPath));
  {$ENDIF}
end;


{ Time Extraction Test }

procedure TTestLightCoreIO.TestExtractTimeFromFileName;
var
  ExtractedTime: TTime;
begin
  { Valid time at end of filename }
  ExtractedTime:= ExtractTimeFromFileName('MyPicture 20-00.jpg');
  Assert.AreEqual(EncodeTime(20, 0, 0, 0), ExtractedTime, 0.0001);

  ExtractedTime:= ExtractTimeFromFileName('Photo 14-30.png');
  Assert.AreEqual(EncodeTime(14, 30, 0, 0), ExtractedTime, 0.0001);

  { Invalid/missing time should return 0 }
  Assert.AreEqual(TTime(0), ExtractTimeFromFileName('NoTime.jpg'));
  Assert.AreEqual(TTime(0), ExtractTimeFromFileName('ab.jpg'));  { Too short }
end;


{ Copy/Backup edge cases }

procedure TTestLightCoreIO.TestCopyFolder;
var
  SrcDir, DestDir: string;
  FailCount: Integer;
begin
  SrcDir:= TPath.Combine(FTestDir, 'copysrc');
  DestDir:= TPath.Combine(FTestDir, 'copydest');
  TDirectory.CreateDirectory(SrcDir);

  { Create test files }
  TFile.WriteAllText(TPath.Combine(SrcDir, 'file1.txt'), 'content1');
  TFile.WriteAllText(TPath.Combine(SrcDir, 'file2.txt'), 'content2');

  FailCount:= CopyFolder(SrcDir, DestDir, True, '*.txt');
  Assert.AreEqual(0, FailCount);  { 0 = all succeeded }
  Assert.IsTrue(TFile.Exists(TPath.Combine(DestDir, 'file1.txt')));
  Assert.IsTrue(TFile.Exists(TPath.Combine(DestDir, 'file2.txt')));
end;

procedure TTestLightCoreIO.TestBackupFileIncrement;
var
  BakFile: string;
begin
  { Create a backup with increment }
  BakFile:= BackupFileIncrement(FTestFile, FTestDir, '.bak');
  Assert.IsNotEmpty(BakFile);
  Assert.IsTrue(TFile.Exists(BakFile));
  Assert.IsTrue(BakFile.EndsWith('.bak'));
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreIO);

end.
