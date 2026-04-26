unit Test.LightFmx.Visual.DropDownSearch;

{=============================================================================================================
   Unit tests for LightFmx.Visual.DropDownSearch.pas
   Tests the TLightDownSearch auto-suggest search box component.

   Note: FMX tests require platform initialization. Some tests may be skipped on non-GUI environments.
   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.ListBox;

type
  [TestFixture]
  TTesTLightDownSearch = class
  private
    FForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor/Destructor Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestDestroy_NoMemoryLeak;

    { PopulateDictionary Tests }
    [Test]
    procedure TestPopulateDictionary_Basic;

    [Test]
    procedure TestPopulateDictionary_Empty;

    [Test]
    procedure TestPopulateDictionary_ClearsExisting;

    { SelectedString/SelectedObject Tests }
    [Test]
    procedure TestSelectedString_NoSelection;

    [Test]
    procedure TestSelectedObject_NoSelection;

    { AddDemoStrings Tests }
    [Test]
    procedure TestAddDemoStrings;

    { MaxDropHeight Property Tests }
    [Test]
    procedure TestMaxDropHeight_Default;

    [Test]
    procedure TestMaxDropHeight_SetValue;
  end;

implementation

uses
  LightFmx.Visual.DropDownSearch;


procedure TTesTLightDownSearch.Setup;
begin
  FForm:= nil;
end;


procedure TTesTLightDownSearch.TearDown;
begin
  FreeAndNil(FForm);
end;


{ Constructor/Destructor Tests }

procedure TTesTLightDownSearch.TestCreate;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;
    Assert.IsNotNull(SearchBox);
    Assert.AreEqual('', SearchBox.Text);
  finally
    FreeAndNil(SearchBox);
  end;
end;


procedure TTesTLightDownSearch.TestDestroy_NoMemoryLeak;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  SearchBox.Parent:= FForm;

  var Words:= TStringList.Create;
  try
    Words.Add('Test1');
    Words.Add('Test2');
    SearchBox.PopulateDictionary(Words);
  finally
    FreeAndNil(Words);
  end;

  Assert.AreEqual(2, SearchBox.WordCount, 'Should have 2 words before destruction');
  FreeAndNil(SearchBox);
  Assert.IsNull(SearchBox, 'SearchBox should be nil after FreeAndNil');
end;


{ PopulateDictionary Tests }

procedure TTesTLightDownSearch.TestPopulateDictionary_Basic;
var
  SearchBox: TLightDownSearch;
  Words: TStringList;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    Words:= TStringList.Create;
    try
      Words.Add('Apple');
      Words.Add('Banana');
      Words.Add('Cherry');
      SearchBox.PopulateDictionary(Words);
    finally
      FreeAndNil(Words);
    end;

    Assert.AreEqual(3, SearchBox.WordCount, 'Should contain 3 words after populate');
  finally
    FreeAndNil(SearchBox);
  end;
end;


procedure TTesTLightDownSearch.TestPopulateDictionary_Empty;
var
  SearchBox: TLightDownSearch;
  Words: TStringList;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    Words:= TStringList.Create;
    try
      SearchBox.PopulateDictionary(Words);
    finally
      FreeAndNil(Words);
    end;

    Assert.AreEqual(0, SearchBox.WordCount, 'Should contain 0 words after empty populate');
  finally
    FreeAndNil(SearchBox);
  end;
end;


procedure TTesTLightDownSearch.TestPopulateDictionary_ClearsExisting;
var
  SearchBox: TLightDownSearch;
  Words1, Words2: TStringList;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    Words1:= TStringList.Create;
    try
      Words1.Add('Old1');
      Words1.Add('Old2');
      SearchBox.PopulateDictionary(Words1);
    finally
      FreeAndNil(Words1);
    end;

    Assert.AreEqual(2, SearchBox.WordCount, 'Should have 2 words after first populate');

    Words2:= TStringList.Create;
    try
      Words2.Add('New1');
      SearchBox.PopulateDictionary(Words2);
    finally
      FreeAndNil(Words2);
    end;

    Assert.AreEqual(1, SearchBox.WordCount, 'Second populate should replace, not append');
  finally
    FreeAndNil(SearchBox);
  end;
end;


{ SelectedString/SelectedObject Tests }

procedure TTesTLightDownSearch.TestSelectedString_NoSelection;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    // With no selection, should return empty string
    Assert.AreEqual('', SearchBox.SelectedString);
  finally
    FreeAndNil(SearchBox);
  end;
end;


procedure TTesTLightDownSearch.TestSelectedObject_NoSelection;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    // With no selection, should return nil
    Assert.IsNull(SearchBox.SelectedObject);
  finally
    FreeAndNil(SearchBox);
  end;
end;


{ AddDemoStrings Tests }

procedure TTesTLightDownSearch.TestAddDemoStrings;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    SearchBox.AddDemoStrings;
    Assert.IsTrue(SearchBox.WordCount > 0, 'AddDemoStrings should populate the word list');
  finally
    FreeAndNil(SearchBox);
  end;
end;


{ MaxDropHeight Property Tests }

procedure TTesTLightDownSearch.TestMaxDropHeight_Default;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    // Default should be 50 (percent)
    Assert.AreEqual(50, SearchBox.MaxDropHeight);
  finally
    FreeAndNil(SearchBox);
  end;
end;


procedure TTesTLightDownSearch.TestMaxDropHeight_SetValue;
var
  SearchBox: TLightDownSearch;
begin
  FForm:= TForm.Create(nil);
  SearchBox:= TLightDownSearch.Create(FForm);
  try
    SearchBox.Parent:= FForm;

    SearchBox.MaxDropHeight:= 75;
    Assert.AreEqual(75, SearchBox.MaxDropHeight);
  finally
    FreeAndNil(SearchBox);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTesTLightDownSearch);

end.
