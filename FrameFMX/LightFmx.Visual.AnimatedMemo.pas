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

//Todo 6: show also word by word

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
    FCurrentLine: string;              // Accumulation buffer for the current line (avoids O(n²) Text+= Ch)
    procedure TimerTick(Sender: TObject);
    procedure SetAnimatedText(const Value: string);
    procedure SetInterval(const Value: Integer);
    procedure FlushCurrentLine;        // Appends FCurrentLine to the last visible memo line
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
  FCurrentLine:= '';

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
  FCurrentLine:= '';
end;


function TAnimatedMemo.IsAnimating: Boolean;
begin
  Result:= FTimer.Enabled;
end;


{ Writes FCurrentLine content into the last line of the memo.
  FMX TMemo has no direct "append to last line" API, so we replace the last Lines entry.
  If there are no lines yet we use Lines.Add. }
procedure TAnimatedMemo.FlushCurrentLine;
begin
  if FCurrentLine = '' then EXIT;
  if Lines.Count = 0
  then Lines.Add(FCurrentLine)
  else Lines[Lines.Count - 1]:= Lines[Lines.Count - 1] + FCurrentLine;
  FCurrentLine:= '';
end;


procedure TAnimatedMemo.TimerTick(Sender: TObject);
{ Displays one character per timer tick.
  Characters accumulate in FCurrentLine buffer and are flushed to the memo only on
  line-break or animation end — avoids O(n²) Text+= Ch cost for long messages.
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
        if Ch = #10 then
          begin
            FlushCurrentLine;  // Push buffered text to memo before starting a new line
            Lines.Add('');     // Start a new (empty) line
          end
        else
          FCurrentLine:= FCurrentLine + Ch;  // Accumulate in buffer (O(1) amortised)

      Inc(FCharIndex);
    end
  else
    begin
      // Animation complete — flush any remaining buffered text, then reset state
      FlushCurrentLine;
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
