unit Test.LightVcl.Common.Registry;

{=============================================================================================================
   Unit tests for LightVcl.Common.Registry.pas
   Tests Windows Registry helper functions

   Note: These tests write to HKEY_CURRENT_USER\Software\LightSaber_Tests which is cleaned up after tests.
   Running these tests requires registry access permissions (normal user rights are sufficient for HKCU).
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  LightVcl.Common.Registry;

type
  [TestFixture]
  TTestRegistry = class
  private
    const
      TestRootKey = HKEY_CURRENT_USER;
      TestKeyPath = 'Software\LightSaber_Tests\RegistryUnitTest';
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Key existence tests }
    [Test]
    procedure TestRegKeyExist_NonExistent;

    [Test]
    procedure TestRegKeyExist_AfterWrite;

    { Value existence tests }
    [Test]
    procedure TestRegValueExist_NonExistent;

    [Test]
    procedure TestRegValueExist_AfterWrite;

    { String read/write tests }
    [Test]
    procedure TestWriteReadString;

    [Test]
    procedure TestWriteReadString_Empty;

    [Test]
    procedure TestWriteReadString_Unicode;

    [Test]
    procedure TestReadString_Default;

    { Integer read/write tests }
    [Test]
    procedure TestWriteReadInteger;

    [Test]
    procedure TestWriteReadInteger_Negative;

    [Test]
    procedure TestReadInteger_Default;

    { Boolean read/write tests }
    [Test]
    procedure TestWriteReadBool_True;

    [Test]
    procedure TestWriteReadBool_False;

    [Test]
    procedure TestReadBool_Default;

    { Date read/write tests }
    [Test]
    procedure TestWriteReadDate;

    [Test]
    procedure TestReadDate_NotFound;

    { Delete tests }
    [Test]
    procedure TestRegDeleteValue;

    [Test]
    procedure TestRegDeleteKey;

    [Test]
    procedure TestRegClearKey;

    { SubKeys tests }
    [Test]
    procedure TestRegHasSubKeys_False;

    [Test]
    procedure TestRegHasSubKeys_True;

    [Test]
    procedure TestRegEnumSubKeys;

    { Value names/data tests }
    [Test]
    procedure TestRegReadValueNames;

    [Test]
    procedure TestRegReadValueDatas;

    [Test]
    procedure TestRegReadValuePairs;

    [Test]
    procedure TestRegWriteValuePairs;

    { Converter tests }
    [Test]
    procedure TestConvert_HKey2Str;

    [Test]
    procedure TestConvert_Str2HKey;

    { Nil parameter tests }
    [Test]
    procedure TestRegReadValueNames_NilParam;

    [Test]
    procedure TestRegReadValueDatas_NilParam;

    [Test]
    procedure TestRegReadValuePairs_NilParam;

    [Test]
    procedure TestRegWriteValuePairs_NilParam;
  end;

implementation


procedure TTestRegistry.Setup;
begin
  { Clean up any leftover test keys from previous failed runs }
  RegDeleteKey(TestRootKey, TestKeyPath);
  RegDeleteKey(TestRootKey, 'Software\LightSaber_Tests');
end;


procedure TTestRegistry.TearDown;
begin
  { Clean up test keys }
  RegDeleteKey(TestRootKey, TestKeyPath);
  RegDeleteKey(TestRootKey, 'Software\LightSaber_Tests');
end;


{ Key existence tests }

procedure TTestRegistry.TestRegKeyExist_NonExistent;
begin
  Assert.IsFalse(RegKeyExist(TestRootKey, TestKeyPath + '\NonExistent'));
end;


procedure TTestRegistry.TestRegKeyExist_AfterWrite;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'TestValue', 'TestData');
  Assert.IsTrue(RegKeyExist(TestRootKey, TestKeyPath));
end;


{ Value existence tests }

procedure TTestRegistry.TestRegValueExist_NonExistent;
begin
  Assert.IsFalse(RegValueExist(TestRootKey, TestKeyPath, 'NonExistent'));
end;


procedure TTestRegistry.TestRegValueExist_AfterWrite;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'MyValue', 'MyData');
  Assert.IsTrue(RegValueExist(TestRootKey, TestKeyPath, 'MyValue'));
end;


{ String read/write tests }

procedure TTestRegistry.TestWriteReadString;
begin
  Assert.IsTrue(RegWriteString(TestRootKey, TestKeyPath, 'StringTest', 'Hello World'));
  Assert.AreEqual('Hello World', RegReadString(TestRootKey, TestKeyPath, 'StringTest', ''));
end;


procedure TTestRegistry.TestWriteReadString_Empty;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'EmptyString', '');
  Assert.AreEqual('', RegReadString(TestRootKey, TestKeyPath, 'EmptyString', 'Default'));
end;


procedure TTestRegistry.TestWriteReadString_Unicode;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'UnicodeTest', 'Test Unicode');
  Assert.AreEqual('Test Unicode', RegReadString(TestRootKey, TestKeyPath, 'UnicodeTest', ''));
end;


procedure TTestRegistry.TestReadString_Default;
begin
  Assert.AreEqual('DefaultValue', RegReadString(TestRootKey, TestKeyPath, 'NonExistent', 'DefaultValue'));
end;


{ Integer read/write tests }

procedure TTestRegistry.TestWriteReadInteger;
begin
  Assert.IsTrue(RegWriteInteger(TestRootKey, TestKeyPath, 'IntTest', 12345));
  Assert.AreEqual(12345, RegReadInteger(TestRootKey, TestKeyPath, 'IntTest', 0));
end;


procedure TTestRegistry.TestWriteReadInteger_Negative;
begin
  RegWriteInteger(TestRootKey, TestKeyPath, 'NegInt', -9999);
  Assert.AreEqual(-9999, RegReadInteger(TestRootKey, TestKeyPath, 'NegInt', 0));
end;


procedure TTestRegistry.TestReadInteger_Default;
begin
  Assert.AreEqual(42, RegReadInteger(TestRootKey, TestKeyPath, 'NonExistent', 42));
end;


{ Boolean read/write tests }

procedure TTestRegistry.TestWriteReadBool_True;
begin
  Assert.IsTrue(RegWriteBool(TestRootKey, TestKeyPath, 'BoolTrue', True));
  Assert.IsTrue(RegReadBool(TestRootKey, TestKeyPath, 'BoolTrue', False));
end;


procedure TTestRegistry.TestWriteReadBool_False;
begin
  RegWriteBool(TestRootKey, TestKeyPath, 'BoolFalse', False);
  Assert.IsFalse(RegReadBool(TestRootKey, TestKeyPath, 'BoolFalse', True));
end;


procedure TTestRegistry.TestReadBool_Default;
begin
  Assert.IsTrue(RegReadBool(TestRootKey, TestKeyPath, 'NonExistent', True));
  Assert.IsFalse(RegReadBool(TestRootKey, TestKeyPath, 'NonExistent2', False));
end;


{ Date read/write tests }

procedure TTestRegistry.TestWriteReadDate;
var
  TestDate: TDate;
begin
  TestDate:= EncodeDate(2026, 1, 30);
  Assert.IsTrue(RegWriteDate(TestRootKey, TestKeyPath, 'DateTest', TestDate));
  Assert.AreEqual(Double(TestDate), Double(RegReadDate(TestRootKey, TestKeyPath, 'DateTest')), 0.0001);
end;


procedure TTestRegistry.TestReadDate_NotFound;
begin
  { RegReadDate returns -1 when value not found }
  Assert.AreEqual(Double(-1), Double(RegReadDate(TestRootKey, TestKeyPath, 'NonExistent')), 0.0001);
end;


{ Delete tests }

procedure TTestRegistry.TestRegDeleteValue;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'ToDelete', 'DeleteMe');
  Assert.IsTrue(RegValueExist(TestRootKey, TestKeyPath, 'ToDelete'));

  Assert.IsTrue(RegDeleteValue(TestRootKey, TestKeyPath, 'ToDelete'));
  Assert.IsFalse(RegValueExist(TestRootKey, TestKeyPath, 'ToDelete'));
end;


procedure TTestRegistry.TestRegDeleteKey;
begin
  RegWriteString(TestRootKey, TestKeyPath + '\SubKey', 'Value', 'Data');
  Assert.IsTrue(RegKeyExist(TestRootKey, TestKeyPath + '\SubKey'));

  Assert.IsTrue(RegDeleteKey(TestRootKey, TestKeyPath + '\SubKey'));
  Assert.IsFalse(RegKeyExist(TestRootKey, TestKeyPath + '\SubKey'));
end;


procedure TTestRegistry.TestRegClearKey;
begin
  { Create key with multiple values }
  RegWriteString(TestRootKey, TestKeyPath, 'Val1', 'Data1');
  RegWriteString(TestRootKey, TestKeyPath, 'Val2', 'Data2');
  RegWriteInteger(TestRootKey, TestKeyPath, 'Val3', 123);

  { Clear all values but keep the key }
  Assert.IsTrue(RegClearKey(TestRootKey, TestKeyPath));

  { Key should still exist but values should be gone }
  Assert.IsTrue(RegKeyExist(TestRootKey, TestKeyPath));
  Assert.IsFalse(RegValueExist(TestRootKey, TestKeyPath, 'Val1'));
  Assert.IsFalse(RegValueExist(TestRootKey, TestKeyPath, 'Val2'));
  Assert.IsFalse(RegValueExist(TestRootKey, TestKeyPath, 'Val3'));
end;


{ SubKeys tests }

procedure TTestRegistry.TestRegHasSubKeys_False;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'Value', 'Data');
  Assert.IsFalse(RegHasSubKeys(TestRootKey, TestKeyPath));
end;


procedure TTestRegistry.TestRegHasSubKeys_True;
begin
  RegWriteString(TestRootKey, TestKeyPath + '\SubKey1', 'Value', 'Data');
  Assert.IsTrue(RegHasSubKeys(TestRootKey, TestKeyPath));
end;


procedure TTestRegistry.TestRegEnumSubKeys;
var
  SubKeys: TStringList;
begin
  { Create subkeys }
  RegWriteString(TestRootKey, TestKeyPath + '\Alpha', 'V', 'D');
  RegWriteString(TestRootKey, TestKeyPath + '\Beta', 'V', 'D');
  RegWriteString(TestRootKey, TestKeyPath + '\Gamma', 'V', 'D');

  SubKeys:= RegEnumSubKeys(TestRootKey, TestKeyPath);
  TRY
    Assert.AreEqual(3, SubKeys.Count);
    Assert.IsTrue(SubKeys.IndexOf('Alpha') >= 0);
    Assert.IsTrue(SubKeys.IndexOf('Beta') >= 0);
    Assert.IsTrue(SubKeys.IndexOf('Gamma') >= 0);
  FINALLY
    FreeAndNil(SubKeys);
  END;
end;


{ Value names/data tests }

procedure TTestRegistry.TestRegReadValueNames;
var
  Names: TStringList;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'Name1', 'Data1');
  RegWriteString(TestRootKey, TestKeyPath, 'Name2', 'Data2');

  Names:= TStringList.Create;
  TRY
    Assert.IsTrue(RegReadValueNames(TestRootKey, TestKeyPath, Names));
    Assert.AreEqual(2, Names.Count);
    Assert.IsTrue(Names.IndexOf('Name1') >= 0);
    Assert.IsTrue(Names.IndexOf('Name2') >= 0);
  FINALLY
    FreeAndNil(Names);
  END;
end;


procedure TTestRegistry.TestRegReadValueDatas;
var
  Datas: TStringList;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'Name1', 'DataA');
  RegWriteString(TestRootKey, TestKeyPath, 'Name2', 'DataB');

  Datas:= TStringList.Create;
  TRY
    Assert.IsTrue(RegReadValueDatas(TestRootKey, TestKeyPath, Datas));
    Assert.AreEqual(2, Datas.Count);
    Assert.IsTrue(Datas.IndexOf('DataA') >= 0);
    Assert.IsTrue(Datas.IndexOf('DataB') >= 0);
  FINALLY
    FreeAndNil(Datas);
  END;
end;


procedure TTestRegistry.TestRegReadValuePairs;
var
  Pairs: TStringList;
begin
  RegWriteString(TestRootKey, TestKeyPath, 'Key1', 'Value1');
  RegWriteString(TestRootKey, TestKeyPath, 'Key2', 'Value2');

  Pairs:= TStringList.Create;
  TRY
    Assert.IsTrue(RegReadValuePairs(TestRootKey, TestKeyPath, Pairs));
    Assert.AreEqual(2, Pairs.Count);
    Assert.AreEqual('Value1', Pairs.Values['Key1']);
    Assert.AreEqual('Value2', Pairs.Values['Key2']);
  FINALLY
    FreeAndNil(Pairs);
  END;
end;


procedure TTestRegistry.TestRegWriteValuePairs;
var
  Pairs: TStringList;
begin
  Pairs:= TStringList.Create;
  TRY
    Pairs.Add('PairName1=PairData1');
    Pairs.Add('PairName2=PairData2');

    Assert.IsTrue(RegWriteValuePairs(TestRootKey, TestKeyPath, Pairs, '='));
    Assert.AreEqual('PairData1', RegReadString(TestRootKey, TestKeyPath, 'PairName1', ''));
    Assert.AreEqual('PairData2', RegReadString(TestRootKey, TestKeyPath, 'PairName2', ''));
  FINALLY
    FreeAndNil(Pairs);
  END;
end;


{ Converter tests }

procedure TTestRegistry.TestConvert_HKey2Str;
begin
  Assert.AreEqual('HKEY_CLASSES_ROOT', Convert_HKey2Str(HKEY_CLASSES_ROOT));
  Assert.AreEqual('HKEY_CURRENT_USER', Convert_HKey2Str(HKEY_CURRENT_USER));
  Assert.AreEqual('HKEY_LOCAL_MACHINE', Convert_HKey2Str(HKEY_LOCAL_MACHINE));
  Assert.AreEqual('', Convert_HKey2Str(0));  { Invalid key }
end;


procedure TTestRegistry.TestConvert_Str2HKey;
begin
  Assert.AreEqual(NativeUInt(HKEY_CLASSES_ROOT), NativeUInt(Convert_Str2HKey('HKEY_CLASSES_ROOT')));
  Assert.AreEqual(NativeUInt(HKEY_CURRENT_USER), NativeUInt(Convert_Str2HKey('HKEY_CURRENT_USER')));
  Assert.AreEqual(NativeUInt(HKEY_LOCAL_MACHINE), NativeUInt(Convert_Str2HKey('HKEY_LOCAL_MACHINE')));
  Assert.AreEqual(NativeUInt(0), NativeUInt(Convert_Str2HKey('INVALID_KEY')));  { Unknown key }
end;


{ Nil parameter tests - verify exceptions are raised }

procedure TTestRegistry.TestRegReadValueNames_NilParam;
begin
  Assert.WillRaise(
    procedure
    begin
      RegReadValueNames(TestRootKey, TestKeyPath, nil);
    end,
    Exception);
end;


procedure TTestRegistry.TestRegReadValueDatas_NilParam;
begin
  Assert.WillRaise(
    procedure
    begin
      RegReadValueDatas(TestRootKey, TestKeyPath, nil);
    end,
    Exception);
end;


procedure TTestRegistry.TestRegReadValuePairs_NilParam;
begin
  Assert.WillRaise(
    procedure
    begin
      RegReadValuePairs(TestRootKey, TestKeyPath, nil);
    end,
    Exception);
end;


procedure TTestRegistry.TestRegWriteValuePairs_NilParam;
begin
  Assert.WillRaise(
    procedure
    begin
      RegWriteValuePairs(TestRootKey, TestKeyPath, nil, '=');
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestRegistry);

end.
