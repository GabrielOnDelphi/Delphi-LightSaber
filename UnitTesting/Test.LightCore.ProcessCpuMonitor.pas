unit Test.LightCore.ProcessCpuMonitor;

{=============================================================================================================
   Unit tests for LightCore.ProcessCpuMonitor.pas
   Tests TProcessListMonitor against the CPU time this test process really uses.

   Note:
     - Test_Sample_AllCoresBusy_ThisProcessTopsTheList keeps every logical processor busy for 600 ms, so the machine stutters briefly.
     - The whole fixture is compiled only when MSWINDOWS is defined, because off Windows LightCore.ProcessCpuMonitor declares nothing.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  LightCore.ProcessCpuMonitor;

type
  [TestFixture]
  TTestProcessCpuMonitor = class
  public
    [Test]
    procedure Test_Sample_FirstCall_ListsRamButNoCpu;

    [Test]
    procedure Test_Sample_AllCoresBusy_ThisProcessTopsTheList;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}


{ Keeps the calling thread busy for at least DurationMs. }
procedure BurnCpu(DurationMs: Cardinal);
VAR
  Start: UInt64;
begin
  Start:= GetTickCount64;
  while GetTickCount64 - Start < DurationMs do ;
end;


procedure TTestProcessCpuMonitor.Test_Sample_FirstCall_ListsRamButNoCpu;
VAR
  Monitor: TProcessListMonitor;
begin
  Monitor:= TProcessListMonitor.Create;
  TRY
    Monitor.Sample;
    Assert.AreEqual(0, Monitor.TopCpuCount, 'The first Sample has no previous cycle count, so it must list no CPU entry');
    Assert.IsTrue(Monitor.TopRamCount > 0, 'The first Sample must already list the processes by RAM');
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


{ One thread per logical processor stays busy between the two samples, so this process uses nearly the whole CPU.
  CpuPercent is normalised to all processors, so it must be high but never above 100. }
procedure TTestProcessCpuMonitor.Test_Sample_AllCoresBusy_ThisProcessTopsTheList;
VAR
  Monitor: TProcessListMonitor;
  Workers: array of TThread;
  Top: RProcessInfo;
  OwnName: string;
  i: Integer;
begin
  OwnName:= ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Monitor:= TProcessListMonitor.Create;
  TRY
    Monitor.Sample;

    SetLength(Workers, CPUCount);
    for i:= 0 to High(Workers) do
      begin
        Workers[i]:= TThread.CreateAnonymousThread(procedure begin BurnCpu(600); end);
        Workers[i].FreeOnTerminate:= FALSE;
        Workers[i].Start;
      end;
    for i:= 0 to High(Workers) do
      begin
        Workers[i].WaitFor;
        FreeAndNil(Workers[i]);
      end;

    Monitor.Sample;

    Assert.IsTrue(Monitor.TopCpuCount > 0, 'The second Sample must list processes by CPU');
    Top:= Monitor.GetTopCpu(0);
    Assert.IsTrue(SameText(OwnName, Top.Name), 'This busy process should top the CPU list, but the top is ' + Top.Name);
    Assert.IsTrue(Top.CpuPercent > 50,   'All processors were busy for most of the interval, got ' + FloatToStr(Top.CpuPercent));
    Assert.IsTrue(Top.CpuPercent <= 100, 'CpuPercent is normalised to all processors and must not pass 100, got ' + FloatToStr(Top.CpuPercent));
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestProcessCpuMonitor);
{$ENDIF}

end.
