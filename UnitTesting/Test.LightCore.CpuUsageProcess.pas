unit Test.LightCore.CpuUsageProcess;

{=============================================================================================================
   Unit tests for LightCore.CpuUsageProcess.pas
   Tests TCpuUsageMonitor against the CPU time this test process really uses.

   Note:
     - The tests measure the current process, so they keep the calling thread busy (or idle) for a known time.
     - Test_CoreUsage_ShortBursts_MatchesSpinTime is the case GetProcessTimes gets wrong on Windows 11: work in short bursts with sleeps between them (the header of LightCore.CpuUsageProcess.pas has the measurement).
     - The whole fixture is compiled only when MSWINDOWS is defined, because off Windows LightCore.CpuUsageProcess declares nothing.
=============================================================================================================}

interface

{$IFDEF MSWINDOWS}
uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  Winapi.Windows,
  LightCore.CpuUsageProcess;

type
  [TestFixture]
  TTestCpuUsageProcess = class
  public
    [Test]
    procedure Test_CoreUsage_BusyThread_IsNearOneCore;

    [Test]
    procedure Test_CoreUsage_IdleThread_IsNearZero;

    [Test]
    procedure Test_CoreUsage_CalledWithinMinInterval_ReturnsLastValue;

    [Test]
    procedure Test_CoreUsage_ShortBursts_MatchesSpinTime;

    [Test]
    procedure Test_CoreUsage_ThreadThatExited_IsCounted;
  end;
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}


{ Keeps the calling thread busy for at least DurationMs. Returns the number of loop passes. }
function BurnCpu(DurationMs: Cardinal): Int64;
VAR
  Start: UInt64;
begin
  Result:= 0;
  Start:= GetTickCount64;
  while GetTickCount64 - Start < DurationMs do
    Inc(Result);
end;


{ Keeps the calling thread busy for at least DurationMs, timed with the high-resolution counter (GetTickCount64 moves in 15.6 ms steps). Returns the milliseconds it really spun. }
function SpinFor(DurationMs: Double): Double;
VAR
  Watch: TStopwatch;
begin
  Watch:= TStopwatch.StartNew;
  REPEAT
  UNTIL Watch.Elapsed.TotalMilliseconds >= DurationMs;
  Result:= Watch.Elapsed.TotalMilliseconds;
end;


procedure TTestCpuUsageProcess.Test_CoreUsage_BusyThread_IsNearOneCore;
VAR
  Monitor: TCpuUsageMonitor;
  Usage: Single;
begin
  Monitor:= TCpuUsageMonitor.Create;
  TRY
    BurnCpu(600);
    Usage:= Monitor.CoreUsage;

    { One thread kept busy for the whole interval uses close to 100% of one core. 50 leaves room for a loaded machine.
      The upper bound catches a cycle count that runs faster than the time-stamp counter it is divided by. }
    Assert.IsTrue(Usage > 50,  'CoreUsage after 600 ms of busy loop should be near 100, got ' + FloatToStr(Usage));
    Assert.IsTrue(Usage < 110, 'CoreUsage of one busy thread should be near 100, got ' + FloatToStr(Usage));
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


procedure TTestCpuUsageProcess.Test_CoreUsage_IdleThread_IsNearZero;
VAR
  Monitor: TCpuUsageMonitor;
  Usage: Single;
begin
  Monitor:= TCpuUsageMonitor.Create;
  TRY
    Sleep(600);
    Usage:= Monitor.CoreUsage;

    Assert.IsTrue(Usage >= 0, 'CoreUsage must never be negative, got ' + FloatToStr(Usage));
    Assert.IsTrue(Usage < 25, 'CoreUsage of a sleeping process should be near 0, got ' + FloatToStr(Usage));
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


procedure TTestCpuUsageProcess.Test_CoreUsage_CalledWithinMinInterval_ReturnsLastValue;
VAR
  Monitor: TCpuUsageMonitor;
  First, Second, Last: Double;  { Double, not Single: picks one Assert.AreEqual overload. Single to Double is exact. }
begin
  Monitor:= TCpuUsageMonitor.Create;
  TRY
    BurnCpu(600);
    First:= Monitor.CoreUsage;
    Second:= Monitor.CoreUsage;  { Well within MIN_MEASUREMENT_INTERVAL of the first call }
    Last:= Monitor.LastUsage;

    Assert.IsTrue(First > 50, 'Precondition: the first call should measure the busy loop, got ' + FloatToStr(First));
    Assert.AreEqual(First, Second, 'A call within MIN_MEASUREMENT_INTERVAL must return the last measured value');
    Assert.AreEqual(First, Last, 'LastUsage must hold the last measured value');
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


{ Works in 2 ms bursts with a Sleep(1) between them, like a frame drawn on every TTimer tick, and checks three one-second readings in a row.
  The expected value is the time the bursts really spun, measured with TStopwatch, not with the monitor. }
procedure TTestCpuUsageProcess.Test_CoreUsage_ShortBursts_MatchesSpinTime;
CONST
  BURST_MS     = 2;
  WINDOW_MS    = 1000;
  WINDOW_COUNT = 3;
VAR
  Monitor: TCpuUsageMonitor;
  Watch: TStopwatch;
  SpinMs, Expected: Double;
  Usage: Single;
  i: Integer;
begin
  Monitor:= TCpuUsageMonitor.Create;
  TRY
    for i:= 1 to WINDOW_COUNT do
      begin
        SpinMs:= 0;
        Watch:= TStopwatch.StartNew;
        while Watch.ElapsedMilliseconds < WINDOW_MS do
          begin
            SpinMs:= SpinMs + SpinFor(BURST_MS);
            Sleep(1);
          end;
        Usage:= Monitor.CoreUsage;
        Expected:= 100 * SpinMs / Watch.Elapsed.TotalMilliseconds;

        { Spin time measured on the wall clock includes any time the thread was pre-empted, so the real CPU use can only be lower: 0.7 leaves room for that. }
        Assert.IsTrue(Expected > 5, 'Precondition: the bursts should keep the thread busy for more than 5% of the time, got ' + FloatToStr(Expected));
        Assert.IsTrue(Usage > Expected * 0.7, 'Window ' + IntToStr(i) + ': CoreUsage ' + FloatToStr(Usage) + ' is far below the measured spin time ' + FloatToStr(Expected));
        Assert.IsTrue(Usage < Expected * 1.3 + 5, 'Window ' + IntToStr(i) + ': CoreUsage ' + FloatToStr(Usage) + ' is far above the measured spin time ' + FloatToStr(Expected));
      end;
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


{ The CPU time of a thread that has already ended must still count. }
procedure TTestCpuUsageProcess.Test_CoreUsage_ThreadThatExited_IsCounted;
VAR
  Monitor: TCpuUsageMonitor;
  Worker: TThread;
  Usage: Single;
begin
  Monitor:= TCpuUsageMonitor.Create;
  TRY
    Worker:= TThread.CreateAnonymousThread(procedure begin BurnCpu(600); end);
    TRY
      Worker.FreeOnTerminate:= FALSE;
      Worker.Start;
      Worker.WaitFor;
    FINALLY
      FreeAndNil(Worker);
    END;
    Usage:= Monitor.CoreUsage;

    Assert.IsTrue(Usage > 50,  'CoreUsage should count the 600 ms the ended thread was busy, got ' + FloatToStr(Usage));
    Assert.IsTrue(Usage < 110, 'CoreUsage of one busy thread should be near 100, got ' + FloatToStr(Usage));
  FINALLY
    FreeAndNil(Monitor);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCpuUsageProcess);
{$ENDIF}

end.
