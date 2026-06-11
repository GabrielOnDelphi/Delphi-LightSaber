unit LightFmx.Visual.AnimatedMemo;

{=============================================================================================================
   2026.06.10
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
    FCurrentLine: string;              // Chars of the currently animated line typed so far (avoids O(n²) Text+= Ch)
    FLineStart: string;                // Content the last memo line already had when the current animated line began (animation can append to pre-existing text)
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
  FLineStart:= '';
end;


function TAnimatedMemo.IsAnimating: Boolean;
begin
  Result:= FTimer.Enabled;
end;


procedure TAnimatedMemo.TimerTick(Sender: TObject);
{ Displays one character per timer tick (the typewriter effect this component exists for).
  FMX TMemo has no "append char to last line" API, so each tick replaces the LAST line with
  FLineStart + FCurrentLine. Cost per tick is O(current line length) — not the O(total²) of
  the old Text:= Text + Ch approach.
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
            Lines.Add('');     // Start a new (empty) line; the previous line is already fully displayed
            FLineStart:= '';
            FCurrentLine:= '';
          end
        else
          begin
            // First char of a new animated line: capture what the last memo line already contains,
            // so the animation appends to pre-existing text instead of overwriting it.
            if FCurrentLine = '' then
              if Lines.Count = 0
              then begin Lines.Add(''); FLineStart:= ''; end
              else FLineStart:= Lines[Lines.Count - 1];

            FCurrentLine:= FCurrentLine + Ch;
            Lines[Lines.Count - 1]:= FLineStart + FCurrentLine;  // Show the character NOW
          end;

      Inc(FCharIndex);
    end
  else
    begin
      // Animation complete — everything is already displayed; just reset state
      FPendingText:= '';
      FCharIndex:= 0;
      FCurrentLine:= '';
      FLineStart:= '';
      FTimer.Enabled:= False;
    end;
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAnimatedMemo]);
end;


end.
