unit Test.LightCore.EnvironmentVar;

{=============================================================================================================
   Unit tests for LightCore.EnvironmentVar.pas
   Tests ListEnvironmentVars, which lists the variables of the process environment block.

   The tests of ExpandEnvironmentStrings, SetEnvironmentVars and GetEnvironmentVars are in Test.LightCore.Win.EnvironmentVar.pas.
=============================================================================================================}

interface
{$IFDEF MSWINDOWS}

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  LightCore.EnvironmentVar;

type
  [TestFixture]
  TTestEnvironmentVar = class
  public
    { ListEnvironmentVars tests }
    [Test]
    procedure Test_ListEnvironmentVars_NotEmpty;

    [Test]
    procedure Test_ListEnvironmentVars_ContainsPath;

    [Test]
    procedure Test_ListEnvironmentVars_Format;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}


{ ListEnvironmentVars tests }

procedure TTestEnvironmentVar.Test_ListEnvironmentVars_NotEmpty;
VAR
  EnvList: TStringList;
  Success: Boolean;
begin
  EnvList:= TStringList.Create;
  TRY
    Success:= ListEnvironmentVars(EnvList);
    Assert.IsTrue(Success, 'ListEnvironmentVars should succeed');
    Assert.IsTrue(EnvList.Count > 0, 'Environment should have at least one variable');
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


procedure TTestEnvironmentVar.Test_ListEnvironmentVars_ContainsPath;
VAR
  EnvList: TStringList;
  i: Integer;
  FoundPath: Boolean;
begin
  EnvList:= TStringList.Create;
  TRY
    ListEnvironmentVars(EnvList);

    FoundPath:= FALSE;
    for i:= 0 to EnvList.Count - 1 do
      if EnvList[i].ToUpper.StartsWith('PATH=') then
        begin
          FoundPath:= TRUE;
          Break;
        end;

    Assert.IsTrue(FoundPath, 'Environment should contain PATH variable');
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


procedure TTestEnvironmentVar.Test_ListEnvironmentVars_Format;
VAR
  EnvList: TStringList;
  i: Integer;
begin
  EnvList:= TStringList.Create;
  TRY
    ListEnvironmentVars(EnvList);

    { Each entry should be in NAME=VALUE format }
    for i:= 0 to EnvList.Count - 1 do
      Assert.IsTrue(EnvList[i].Contains('='),
        'Entry should contain = separator: ' + EnvList[i]);
  FINALLY
    FreeAndNil(EnvList);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestEnvironmentVar);
{$ENDIF}

end.
