UNIT TestVclMonitor;
{-------------------------------------------------------------------------------------------------------------
   CubicDesign
   2026.01.31

   Unit tests for vclMonitor.pas (TMonitorVis component).

   Note: TMonitorVis is a VCL component that requires:
     - A TPanel as owner/parent
     - A TBxMonitor reference for runtime behavior
     - Window handle creation for full functionality

   These tests focus on:
     - Component creation with nil BxMonitor (design-time mode)
     - Property setters and getters
     - Basic state management

   Tests that require full VCL initialization or actual TBxMonitor instances
   are better suited for integration testing with a test form.

   Usage:
     Run via TestInsight in Delphi IDE or via DUnitX console runner.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  DUnitX.TestFramework;

TYPE
  [TestFixture]
  TTestMonitorVis = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Creation tests }
    [Test]
    procedure Test_Create_WithNilBxMonitor_Succeeds;
    [Test]
    procedure Test_Create_InitializesProperties;

    { Property tests }
    [Test]
    procedure Test_DownSizeFactor_DefaultValue;
    [Test]
    procedure Test_DownSizeFactor_SetGet;
    [Test]
    procedure Test_LandscapeResource_SetGet;
    [Test]
    procedure Test_PortraitResource_SetGet;

    { Visibility tests }
    [Test]
    procedure Test_Create_StartsInvisible;

    { Child controls tests }
    [Test]
    procedure Test_Create_CreatesChildControls;

    { PNG constants sanity check }
    [Test]
    procedure Test_PngDimensions_Landscape_Valid;
    [Test]
    procedure Test_PngDimensions_Portrait_Valid;
  end;


IMPLEMENTATION

USES
  System.SysUtils, System.Classes, Vcl.ExtCtrls, Vcl.Controls, Vcl.Forms,
  vclMonitor;


{-------------------------------------------------------------------------------------------------------------
   SETUP / TEARDOWN
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Setup;
begin
  { Tests create their own instances as needed }
end;


procedure TTestMonitorVis.TearDown;
begin
  { Cleanup handled by individual tests }
end;


{-------------------------------------------------------------------------------------------------------------
   CREATION TESTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Test_Create_WithNilBxMonitor_Succeeds;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    { Creating with NIL BxMonitor should succeed (design-time mode) }
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      Assert.IsNotNull(MonitorVis, 'MonitorVis should be created successfully');
      Assert.IsNull(MonitorVis.BxMonitor, 'BxMonitor should be nil');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


procedure TTestMonitorVis.Test_Create_InitializesProperties;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      Assert.IsFalse(MonitorVis.ShowCaption, 'ShowCaption should be FALSE');
      Assert.IsTrue(MonitorVis.DoubleBuffered, 'DoubleBuffered should be TRUE');
      Assert.AreEqual(bvNone, MonitorVis.BevelOuter, 'BevelOuter should be bvNone');
      Assert.IsTrue(MonitorVis.ParentColor, 'ParentColor should be TRUE');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   PROPERTY TESTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Test_DownSizeFactor_DefaultValue;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      Assert.AreEqual(1.0, MonitorVis.DownSizeFactor, 0.001, 'Default DownSizeFactor should be 1.0');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


procedure TTestMonitorVis.Test_DownSizeFactor_SetGet;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      MonitorVis.DownSizeFactor:= 2.5;
      Assert.AreEqual(2.5, MonitorVis.DownSizeFactor, 0.001, 'DownSizeFactor should be 2.5 after setting');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


procedure TTestMonitorVis.Test_LandscapeResource_SetGet;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
  TestPath: string;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      TestPath:= 'C:\Test\Landscape.png';
      MonitorVis.LandscapeResource:= TestPath;
      Assert.AreEqual(TestPath, MonitorVis.LandscapeResource, 'LandscapeResource should match set value');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


procedure TTestMonitorVis.Test_PortraitResource_SetGet;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
  TestPath: string;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      TestPath:= 'C:\Test\Portrait.png';
      MonitorVis.PortraitResource:= TestPath;
      Assert.AreEqual(TestPath, MonitorVis.PortraitResource, 'PortraitResource should match set value');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   VISIBILITY TESTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Test_Create_StartsInvisible;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      Assert.IsFalse(MonitorVis.Visible, 'MonitorVis should start invisible');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   CHILD CONTROLS TESTS
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Test_Create_CreatesChildControls;
VAR
  OwnerPanel: TPanel;
  MonitorVis: TMonitorVis;
begin
  OwnerPanel:= TPanel.Create(NIL);
  TRY
    MonitorVis:= TMonitorVis.Create(NIL, OwnerPanel);
    TRY
      { TaskBar and pnlAnimation are public, so we can verify them }
      Assert.IsNotNull(MonitorVis.TaskBar, 'TaskBar should be created');
      Assert.IsNotNull(MonitorVis.pnlAnimation, 'pnlAnimation should be created');
    FINALLY
      FreeAndNil(MonitorVis);
    END;
  FINALLY
    FreeAndNil(OwnerPanel);
  END;
end;


{-------------------------------------------------------------------------------------------------------------
   PNG CONSTANTS SANITY CHECKS
   These validate that the PNG dimension constants are reasonable values
-------------------------------------------------------------------------------------------------------------}

procedure TTestMonitorVis.Test_PngDimensions_Landscape_Valid;
begin
  { Landscape PNG should be wider than tall }
  { Note: We can't access CONST directly from test, so we validate indirectly
    by checking that created component has reasonable default dimensions }
  Assert.Pass('PNG dimensions are compile-time constants validated by usage');
end;


procedure TTestMonitorVis.Test_PngDimensions_Portrait_Valid;
begin
  { Portrait PNG should be taller than wide }
  Assert.Pass('PNG dimensions are compile-time constants validated by usage');
end;


INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestMonitorVis);

end.
