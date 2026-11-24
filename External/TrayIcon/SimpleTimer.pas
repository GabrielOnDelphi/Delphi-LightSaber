{**************************************************************************************************}
{ SimpleTimer is a timer class. It has the same timer resolution  as TTimer, but it is more lightweight because it's derived from
{ TObject in stead of TComponent. Furthermore, the same handle is shared between multiple instances of SimpleTimer.
{ This makes it ideal for developers who need a timer in their own components or applications, but want to keep the resource usage minimal. The unit is freeware.   }
{                                                                 }
{ Troels Jakobsen - troels.jakobsen@gmail.com . Copyright (c) 2006}
{ Home: http://subsimple.com/delphi.asp
{
{ v4.4.4
{
{*************************************************************************************************}

unit SimpleTimer;

{$D-}


INTERFACE
USES Winapi.Windows, System.Classes;

TYPE
  TSimpleTimer = class(TObject)
  private
    FId: NativeUInt;
    FEnabled: Boolean;
    FInterval: Cardinal;
    FAutoDisable: Boolean;
    FTag: Integer;
    FOnTimer: TNotifyEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure Initialize(AInterval: Cardinal; AOnTimer: TNotifyEvent);
  protected
    function Start: Boolean;
    function Stop(Disable: Boolean): Boolean;
  public
    constructor Create;
    constructor CreateEx(AInterval: Cardinal; AOnTimer: TNotifyEvent);
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property AutoDisable: Boolean read FAutoDisable write FAutoDisable;
    property Tag: Integer read Ftag write Ftag default 0;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

function GetSimpleTimerCount: Cardinal;
function GetSimpleTimerActiveCount: Cardinal;

IMPLEMENTATION

uses
  System.SysUtils, Winapi.Messages;

type
  TSimpleTimerHandler = class(TObject)
  private
    RefCount: Cardinal;
    ActiveCount: Cardinal;
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTimer;
    procedure RemoveTimer;
  end;

VAR SimpleTimerHandler: TSimpleTimerHandler = nil;


function GetSimpleTimerCount: Cardinal;
begin
  if Assigned(SimpleTimerHandler)
  then Result := SimpleTimerHandler.RefCount
  else Result := 0;
end;

function GetSimpleTimerActiveCount: Cardinal;
begin
  if Assigned(SimpleTimerHandler) then
    Result := SimpleTimerHandler.ActiveCount
  else
    Result := 0;
end;







{--------------- TSimpleTimerHandler ------------------}
constructor TSimpleTimerHandler.Create;
begin
  inherited Create;
  FWindowHandle := System.Classes.AllocateHWnd(WndProc);
end;


destructor TSimpleTimerHandler.Destroy;
begin
  System.Classes.DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;


procedure TSimpleTimerHandler.AddTimer;
begin
  Inc(RefCount);
end;


procedure TSimpleTimerHandler.RemoveTimer;
begin
  if RefCount > 0 then
    Dec(RefCount);
end;


procedure TSimpleTimerHandler.WndProc(var Msg: TMessage);
var
  Timer: TSimpleTimer;
begin
  if Msg.Msg = WM_TIMER then
  begin
    Timer := TSimpleTimer(Msg.wParam);

    if Timer.FAutoDisable
    then Timer.Stop(True);
    if Assigned(Timer.FOnTimer)
    then Timer.FOnTimer(Timer);     // Call OnTimer event method if assigned
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;






{---------------- Container management ----------------}
procedure AddTimer;
begin
  if not Assigned(SimpleTimerHandler) then
    // Create new handler
    SimpleTimerHandler := TSimpleTimerHandler.Create;
  SimpleTimerHandler.AddTimer;
end;


procedure RemoveTimer;
begin
  if Assigned(SimpleTimerHandler) then
  begin
    SimpleTimerHandler.RemoveTimer;
    if SimpleTimerHandler.RefCount = 0
    then FreeAndNil(SimpleTimerHandler);
  end;
end;

{------------------- TSimpleTimer ---------------------}

constructor TSimpleTimer.Create;
begin
  inherited Create;
  Initialize(1000, NIL);
end;


constructor TSimpleTimer.CreateEx(AInterval: Cardinal; AOnTimer: TNotifyEvent);
begin
  inherited Create;
  Initialize(AInterval, AOnTimer);
end;


destructor TSimpleTimer.Destroy;
begin
  if FEnabled
  then Stop(True);
  RemoveTimer;               // Container management
  inherited Destroy;
end;


procedure TSimpleTimer.Initialize(AInterval: Cardinal; AOnTimer: TNotifyEvent);
begin
  FId := NativeUInt(Self);       // Use Self as id in call to SetTimer and callback method
  FAutoDisable := False;
  FEnabled := False;
  FInterval := AInterval;
  SetOnTimer(AOnTimer);
  AddTimer;                  // Container management
end;


procedure TSimpleTimer.SetEnabled(Value: Boolean);
begin
  if Value
  then Start
  else Stop(True);
end;



procedure TSimpleTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FEnabled then
      if FInterval <> 0
      then Start
      else Stop(False);
  end;
end;


procedure TSimpleTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer:= Value;
  if (not Assigned(Value)) and (FEnabled)
  then Stop(False);
end;


function TSimpleTimer.Start: Boolean;
begin
  if FInterval = 0 then
   begin
    Result := False;
    Exit;
  end;
  if FEnabled then Stop(True);

  Result := (SetTimer(SimpleTimerHandler.FWindowHandle, FId, FInterval, nil) <> 0);
  if Result then
  begin
    FEnabled := True;
    Inc(SimpleTimerHandler.ActiveCount);
  end;
end;


function TSimpleTimer.Stop(Disable: Boolean): Boolean;
begin
  if Disable
  then FEnabled := FALSE;
  if not Assigned(SimpleTimerHandler) then
   begin
    Result := False;
    Exit;
   end;
  Result := KillTimer(SimpleTimerHandler.FWindowHandle, FId);
    if Result and (SimpleTimerHandler.ActiveCount > 0)
    then Dec(SimpleTimerHandler.ActiveCount);
end;

end.


