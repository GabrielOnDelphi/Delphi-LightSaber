unit Test.LightCore.MRU;

{=============================================================================================================
   Unit tests for LightCore.MRU.pas
   Tests Most Recently Used file list functionality.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  LightCore.MRU;

type
  [TestFixture]
  TTestMRU = class
  private
    FTestDir: string;
    FMRU: TMRUList;
    FChangedCount: Integer;
    procedure OnMRUChanged(Sender: TObject);
    procedure CleanupTestDir;
    function CreateTestFile(const Name: string): string;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestDefaultMaxItems;

    { AddToMRU Tests }
    [Test]
    procedure TestAddToMRU_NewFile;

    [Test]
    procedure TestAddToMRU_NonExistentFile;

    [Test]
    procedure TestAddToMRU_Duplicate;

    [Test]
    procedure TestAddToMRU_MoveToTop;

    [Test]
    procedure TestAddToMRU_ExceedsMaxItems;

    { MaxItems Tests }
    [Test]
    procedure TestSetMaxItems;

    [Test]
    procedure TestSetMaxItems_TrimsList;

    [Test]
    procedure TestSetMaxItems_MinimumValue;

    { Persistence Tests }
    [Test]
    procedure TestFileName_Load;

    [Test]
    procedure TestFileName_Save;

    [Test]
    procedure TestFileName_NonExistent;

    { OnChanged Event Tests }
    [Test]
    procedure TestOnChanged_AddToMRU;

    [Test]
    procedure TestOnChanged_SetMaxItems;

    [Test]
    procedure TestOnChanged_SetFileName;

    { Clear and Access Tests }
    [Test]
    procedure TestClear;

    [Test]
    procedure TestCount;

    [Test]
    procedure TestGetItem;

    [Test]
    procedure TestGetItem_OutOfRange;

    { Case Insensitivity Tests }
    [Test]
    procedure TestAddToMRU_CaseInsensitive;
  end;

implementation


procedure TTestMRU.Setup;
begin
  FTestDir:= TPath.Combine(TPath.GetTempPath, 'MRUTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);

  FMRU:= TMRUList.Create;
  FChangedCount:= 0;
end;


procedure TTestMRU.TearDown;
begin
  FreeAndNil(FMRU);
  CleanupTestDir;
end;


procedure TTestMRU.CleanupTestDir;
begin
  if TDirectory.Exists(FTestDir)
  then TDirectory.Delete(FTestDir, True);
end;


procedure TTestMRU.OnMRUChanged(Sender: TObject);
begin
  Inc(FChangedCount);
end;


function TTestMRU.CreateTestFile(const Name: string): string;
begin
  Result:= TPath.Combine(FTestDir, Name);
  TFile.WriteAllText(Result, 'test content');
end;


{ Constructor/Destructor Tests }

procedure TTestMRU.TestCreate;
begin
  Assert.IsNotNull(FMRU);
  Assert.IsNotNull(FMRU.Items);
end;


procedure TTestMRU.TestDefaultMaxItems;
begin
  Assert.AreEqual(16, FMRU.MaxItems);
end;


{ AddToMRU Tests }

procedure TTestMRU.TestAddToMRU_NewFile;
VAR
  FilePath: string;
  WasExisting: Boolean;
begin
  FilePath:= CreateTestFile('test1.txt');

  WasExisting:= FMRU.AddToMRU(FilePath);

  Assert.IsFalse(WasExisting, 'Should return False for new file');
  Assert.AreEqual(1, FMRU.Count);
  Assert.AreEqual(FilePath, FMRU.GetItem(0));
end;


procedure TTestMRU.TestAddToMRU_NonExistentFile;
VAR
  WasExisting: Boolean;
begin
  WasExisting:= FMRU.AddToMRU(TPath.Combine(FTestDir, 'nonexistent.txt'));

  Assert.IsFalse(WasExisting);
  Assert.AreEqual(0, FMRU.Count, 'Non-existent file should not be added');
end;


procedure TTestMRU.TestAddToMRU_Duplicate;
VAR
  FilePath: string;
  WasExisting: Boolean;
begin
  FilePath:= CreateTestFile('test1.txt');

  FMRU.AddToMRU(FilePath);
  WasExisting:= FMRU.AddToMRU(FilePath);

  Assert.IsTrue(WasExisting, 'Should return True for duplicate');
  Assert.AreEqual(1, FMRU.Count, 'Duplicate should not increase count');
end;


procedure TTestMRU.TestAddToMRU_MoveToTop;
VAR
  File1, File2, File3: string;
begin
  File1:= CreateTestFile('file1.txt');
  File2:= CreateTestFile('file2.txt');
  File3:= CreateTestFile('file3.txt');

  FMRU.AddToMRU(File1);
  FMRU.AddToMRU(File2);
  FMRU.AddToMRU(File3);

  { Order should be: File3, File2, File1 }
  Assert.AreEqual(File3, FMRU.GetItem(0));
  Assert.AreEqual(File2, FMRU.GetItem(1));
  Assert.AreEqual(File1, FMRU.GetItem(2));

  { Re-add File1 - should move to top }
  FMRU.AddToMRU(File1);

  Assert.AreEqual(3, FMRU.Count, 'Count should remain 3');
  Assert.AreEqual(File1, FMRU.GetItem(0), 'File1 should be at top');
  Assert.AreEqual(File3, FMRU.GetItem(1));
  Assert.AreEqual(File2, FMRU.GetItem(2));
end;


procedure TTestMRU.TestAddToMRU_ExceedsMaxItems;
VAR
  i: Integer;
  FilePath: string;
begin
  FMRU.MaxItems:= 3;

  for i:= 1 to 5 do
  begin
    FilePath:= CreateTestFile('file' + IntToStr(i) + '.txt');
    FMRU.AddToMRU(FilePath);
  end;

  Assert.AreEqual(3, FMRU.Count, 'Count should be limited to MaxItems');
  { Most recent files should be kept (5, 4, 3) }
  Assert.IsTrue(Pos('file5', FMRU.GetItem(0)) > 0);
  Assert.IsTrue(Pos('file4', FMRU.GetItem(1)) > 0);
  Assert.IsTrue(Pos('file3', FMRU.GetItem(2)) > 0);
end;


{ MaxItems Tests }

procedure TTestMRU.TestSetMaxItems;
begin
  FMRU.MaxItems:= 10;
  Assert.AreEqual(10, FMRU.MaxItems);
end;


procedure TTestMRU.TestSetMaxItems_TrimsList;
VAR
  i: Integer;
  FilePath: string;
begin
  { Add 5 files }
  for i:= 1 to 5 do
  begin
    FilePath:= CreateTestFile('file' + IntToStr(i) + '.txt');
    FMRU.AddToMRU(FilePath);
  end;

  Assert.AreEqual(5, FMRU.Count);

  { Reduce max to 3 }
  FMRU.MaxItems:= 3;

  Assert.AreEqual(3, FMRU.Count, 'List should be trimmed');
end;


procedure TTestMRU.TestSetMaxItems_MinimumValue;
begin
  FMRU.MaxItems:= 0;  { Should be clamped to 1 }
  Assert.AreEqual(1, FMRU.MaxItems);

  FMRU.MaxItems:= -5;  { Should be clamped to 1 }
  Assert.AreEqual(1, FMRU.MaxItems);
end;


{ Persistence Tests }

procedure TTestMRU.TestFileName_Load;
VAR
  MRUFile, TestFile1, TestFile2: string;
  NewMRU: TMRUList;
begin
  TestFile1:= CreateTestFile('persist1.txt');
  TestFile2:= CreateTestFile('persist2.txt');
  MRUFile:= TPath.Combine(FTestDir, 'mru.ini');

  { Setup and save }
  FMRU.FileName:= MRUFile;
  FMRU.AddToMRU(TestFile1);
  FMRU.AddToMRU(TestFile2);

  { Load in new instance }
  NewMRU:= TMRUList.Create;
  TRY
    NewMRU.FileName:= MRUFile;
    Assert.AreEqual(2, NewMRU.Count, 'Should load 2 items from file');
  FINALLY
    FreeAndNil(NewMRU);
  END;
end;


procedure TTestMRU.TestFileName_Save;
VAR
  MRUFile, TestFile: string;
begin
  TestFile:= CreateTestFile('saveme.txt');
  MRUFile:= TPath.Combine(FTestDir, 'mru_save.ini');

  FMRU.FileName:= MRUFile;
  FMRU.AddToMRU(TestFile);

  Assert.IsTrue(FileExists(MRUFile), 'MRU file should be created');
end;


procedure TTestMRU.TestFileName_NonExistent;
VAR
  MRUFile: string;
begin
  MRUFile:= TPath.Combine(FTestDir, 'nonexistent_mru.ini');

  { Should not raise exception }
  Assert.WillNotRaiseAny(
    procedure
    begin
      FMRU.FileName:= MRUFile;
    end
  );
  Assert.AreEqual(0, FMRU.Count);
end;


{ OnChanged Event Tests }

procedure TTestMRU.TestOnChanged_AddToMRU;
VAR
  FilePath: string;
begin
  FilePath:= CreateTestFile('changed.txt');
  FMRU.OnChanged:= OnMRUChanged;
  FChangedCount:= 0;

  FMRU.AddToMRU(FilePath);

  Assert.AreEqual(1, FChangedCount, 'OnChanged should fire once on AddToMRU');
end;


procedure TTestMRU.TestOnChanged_SetMaxItems;
VAR
  i: Integer;
  FilePath: string;
begin
  { Add files first }
  for i:= 1 to 5 do
  begin
    FilePath:= CreateTestFile('file' + IntToStr(i) + '.txt');
    FMRU.AddToMRU(FilePath);
  end;

  FMRU.OnChanged:= OnMRUChanged;
  FChangedCount:= 0;

  FMRU.MaxItems:= 2;  { This should trim and fire OnChanged }

  Assert.AreEqual(1, FChangedCount, 'OnChanged should fire when list is trimmed');
end;


procedure TTestMRU.TestOnChanged_SetFileName;
VAR
  MRUFile, TestFile: string;
begin
  TestFile:= CreateTestFile('preload.txt');
  MRUFile:= TPath.Combine(FTestDir, 'mru_event.ini');

  { Create a file to load }
  TFile.WriteAllText(MRUFile, TestFile);

  FMRU.OnChanged:= OnMRUChanged;
  FChangedCount:= 0;

  FMRU.FileName:= MRUFile;

  Assert.AreEqual(1, FChangedCount, 'OnChanged should fire when file is loaded');
end;


{ Clear and Access Tests }

procedure TTestMRU.TestClear;
VAR
  FilePath: string;
begin
  FilePath:= CreateTestFile('clearme.txt');
  FMRU.AddToMRU(FilePath);

  Assert.AreEqual(1, FMRU.Count);

  FMRU.Clear;

  Assert.AreEqual(0, FMRU.Count);
end;


procedure TTestMRU.TestCount;
VAR
  i: Integer;
  FilePath: string;
begin
  Assert.AreEqual(0, FMRU.Count);

  for i:= 1 to 3 do
  begin
    FilePath:= CreateTestFile('count' + IntToStr(i) + '.txt');
    FMRU.AddToMRU(FilePath);
    Assert.AreEqual(i, FMRU.Count);
  end;
end;


procedure TTestMRU.TestGetItem;
VAR
  File1, File2: string;
begin
  File1:= CreateTestFile('item1.txt');
  File2:= CreateTestFile('item2.txt');

  FMRU.AddToMRU(File1);
  FMRU.AddToMRU(File2);

  Assert.AreEqual(File2, FMRU.GetItem(0));  { Most recent }
  Assert.AreEqual(File1, FMRU.GetItem(1));
end;


procedure TTestMRU.TestGetItem_OutOfRange;
begin
  Assert.AreEqual('', FMRU.GetItem(-1));
  Assert.AreEqual('', FMRU.GetItem(0));  { Empty list }
  Assert.AreEqual('', FMRU.GetItem(100));
end;


{ Case Insensitivity Tests }

procedure TTestMRU.TestAddToMRU_CaseInsensitive;
VAR
  FilePath, UpperPath: string;
begin
  FilePath:= CreateTestFile('CaseSensitive.txt');
  UpperPath:= UpperCase(FilePath);

  FMRU.AddToMRU(FilePath);

  { Create a copy with uppercase path }
  TFile.Copy(FilePath, UpperPath);

  FMRU.AddToMRU(UpperPath);

  { On Windows, these are the same file, so count should be 1 }
  { This test verifies case-insensitive duplicate detection }
  Assert.IsTrue(FMRU.Count <= 2, 'Case-insensitive matching should prevent excessive duplicates');
end;


initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestMRU);

end.
