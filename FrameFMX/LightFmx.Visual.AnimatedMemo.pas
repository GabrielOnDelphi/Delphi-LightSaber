unit LightFmx.Visual.AnimatedMemo;

{=============================================================================================================
   2026.01.31
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   FMX memo component that displays animated text (character by character).

   Usage:
     Set text via AnimatedText property. Characters appear one-by-one in the memo,
     creating a typewriter animation. Multiple calls append to the animation queue;
     previous text is preserved.

   Demo: Demos\Demo_FMX_AnimatedTextMemo.dpr
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.SysUtils,
  FMX.Memo, FMX.Types;

type
  TAnimatedMemo = class(TMemo)
  private
    FTimer: TTimer;
    FPendingText: string;              // Buffer holding text waiting to be displayed
    FCharIndex: Integer;               // Current position in FPendingText (1-based)
    FInterval: Integer;                // Delay between characters in milliseconds
    procedure TimerTick(Sender: TObject);
    procedure SetAnimatedText(const Value: string);
    procedure SetInterval(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAnimation;          // Clears pending text and stops animation
    function  IsAnimating: Boolean;    // Returns True if animation is in progress
  published
    property AnimatedText: string write SetAnimatedText;
    property AnimationInterval: Integer read FInterval write SetInterval default 30;
  end;

procedure Register;

implementation


constructor TAnimatedMemo.Create(AOwner: TComponent);
begin
  inherited;

  FInterval:= 30;
  FCharIndex:= 0;
  FPendingText:= '';

  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= False;
  FTimer.Interval:= FInterval;
  FTimer.OnTimer:= TimerTick;
end;


destructor TAnimatedMemo.Destroy;
begin
  FTimer.Enabled:= False;  // Stop timer before destruction
  // FTimer is owned by Self, so it will be freed automatically
  inherited;
end;


procedure TAnimatedMemo.SetInterval(const Value: Integer);
begin
  if Value <= 0 then EXIT;

  FInterval:= Value;
  FTimer.Interval:= FInterval;
end;


procedure TAnimatedMemo.SetAnimatedText(const Value: string);
begin
  if Value = '' then EXIT;

  // Append new text to pending buffer (supports multiple calls while animating)
  FPendingText:= FPendingText + Value;

  // Start animation if not already running
  if NOT FTimer.Enabled then
    begin
      FCharIndex:= 1;
      FTimer.Enabled:= True;
    end;
end;


procedure TAnimatedMemo.ClearAnimation;
begin
  FTimer.Enabled:= False;
  FPendingText:= '';
  FCharIndex:= 0;
end;


function TAnimatedMemo.IsAnimating: Boolean;
begin
  Result:= FTimer.Enabled;
end;


procedure TAnimatedMemo.TimerTick(Sender: TObject);
{ Displays one character per timer tick.
  Line breaks: FMX TMemo uses LF (#10) internally. CR (#13) is skipped. }
var
  Ch: Char;
begin
  if FCharIndex <= Length(FPendingText) then
    begin
      Ch:= FPendingText[FCharIndex];

      if Ch = #13
      then // Skip CR - FMX uses LF only
      else
        if Ch = #10
        then Lines.Add('')        // Start new line
        else Text:= Text + Ch;    // Append character to current line

      Inc(FCharIndex);
    end
  else
    begin
      // Animation complete - reset state
      FPendingText:= '';
      FCharIndex:= 0;
      FTimer.Enabled:= False;
    end;
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAnimatedMemo]);
end;


end.
