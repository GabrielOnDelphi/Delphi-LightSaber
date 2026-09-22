unit Test.LightVcl.Common.WinVersion;

{=============================================================================================================
   Unit tests for LightVcl.Common.WinVersion.pas
   That unit holds only IsNTKernel. The tests of the IsWindowsXX routines, GetOSName, GetOSDetails and GenerateReport are in Test.LightCore.WinVersion.pas (project Tests_LightCore).
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  LightVcl.Common.WinVersion;

type
  [TestFixture]
  TTestWinVersion = class
  public
    { Utility Function Tests }
    [Test]
    procedure Test_IsNTKernel_ReturnsTrue;
  end;


implementation


{ Utility Function Tests }

procedure TTestWinVersion.Test_IsNTKernel_ReturnsTrue;
begin
  { All modern Windows versions (2000+) use NT kernel }
  Assert.IsTrue(IsNTKernel, 'Modern Windows should use NT kernel');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWinVersion);

end.
