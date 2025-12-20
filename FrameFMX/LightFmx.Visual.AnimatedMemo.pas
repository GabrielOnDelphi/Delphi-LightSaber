unit LightFmx.Visual.AnimatedMemo;

{=============================================================================================================
   2023.04
   www.GabrielMoraru.com
==============================================================================================================
   A FMX component from TMemo that can show animated text (char by char).
   Set text to the AnimatedText property. The memo will show the characters one my one in the memo, creating a nice animation.
   We can set the property multiple times. The previous text is preserved.

   Demo: Demos\Demo_FMX_AnimatedTextMemo.dpr
=============================================================================================================}

INTERFACE

USES
  System.Classes, System.SysUtils, System.Types,
  FMX.Memo, FMX.Types;

type
  TAnimatedMemo = class(TMemo)
  private
    FTimer: TTimer;
    FPendingText: string;
    FCharIndex: Integer;
    FInterval: Integer;
    procedure OnTimer(Sender: TObject);
    procedure SetAnimatedText(const Value: string);
    procedure SetInterval(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AnimatedText: string write SetAnimatedText;
    property AnimationInterval: Integer read FInterval write SetInterval default 30; // ms per char
  end;

procedure Register;

implementation


constructor TAnimatedMemo.Create(AOwner: TComponent);
begin
  inherited;

  FInterval := 30;
  FCharIndex := 0;
  FPendingText := '';

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := FInterval;
  FTimer.OnTimer := OnTimer;
end;


procedure TAnimatedMemo.SetInterval(const Value: Integer);
begin
  if Value <= 0 then Exit;

  FInterval := Value;
  FTimer.Interval := FInterval;
end;

procedure TAnimatedMemo.SetAnimatedText(const Value: string);
begin
  if Value = '' then Exit;

  // Append new text to pending buffer
  FPendingText := FPendingText + Value;

  // Start animation if not already running
  if not FTimer.Enabled then
    begin
      FCharIndex := 1;
      FTimer.Enabled := True;
    end;
end;

procedure TAnimatedMemo.OnTimer(Sender: TObject);
var
  Ch: Char;
begin
  if FCharIndex <= Length(FPendingText) then
    begin
      Ch := FPendingText[FCharIndex];

      // FMX TMemo uses LF (#10) as line break, not CRLF
      if Ch = #13
      then // ignore CR
      else
        if Ch = #10
        then Lines.Add('')
        else Text := Text + Ch;

      Inc(FCharIndex);
    end
  else
    begin
      // Finished current pending text
      FPendingText := '';
      FCharIndex := 0;
      FTimer.Enabled := False;
    end;
end;



procedure Register;
begin
  RegisterComponents('LightSaber FMX', [TAnimatedMemo]);
end;


end.
