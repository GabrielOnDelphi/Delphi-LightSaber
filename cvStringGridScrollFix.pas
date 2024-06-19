unit cvStringGridScrollFix;
{-------------------------------------------------------------------------------------------------------------
  2020-10-18

  Problem:
      Stop scroll on mousedown on bottom row of grid when bottom row is a partial cell:
  Fix:
      Have to block both initial scroll and timer-based scroll.
      This code is pretty dependent on the implementation in Vcl.Grids.pas, so it should be checked if we upgrade to new version of Delphi.

  Sources:
    https://marc.durdin.net/2017/07/working-around-delphis-default-grid-scrolling-behaviour/
    http://www.devsuperpage.com/search/Articles.asp?ArtID=742127
    https://quality.embarcadero.com/browse/RSP-18542
-------------------------------------------------------------------------------------------------------------}


{$IFNDEF VER320}
//{$MESSAGE ERROR 'Check that this fix is still applicable for a new version of Delphi. Checked against Delphi 10.2' }
{$ENDIF}


INTERFACE

USES
  System.Classes, Vcl.Controls, Vcl.Grids, Winapi.Windows;

TYPE
  TStringGridSF = class(TStringGrid)
  private
    TimerStarted: Boolean;
    HackedMousedown: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    function  SelectCell(ACol, ARow: Longint): Boolean; override;
  end;

IMPLEMENTATION


procedure TStringGridSF.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // When we first mouse-down, we know the grid has no active scroll timer
  TimerStarted := False;

  // Call the inherited event, blocking the default MoveCurrent behaviour that scrolls the cell into view
  HackedMouseDown := True;
  TRY
    inherited;
  FINALLY
    HackedMouseDown := False;
  END;

  // Cancel scrolling timer started by the mousedown event for selecting
  if FGridState = gsSelecting
  then KillTimer(Handle, 1);
end;


procedure TStringGridSF.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Start the scroll timer if we are selecting and mouse
  // button is down, on our first movement with mouse down
  if NOT TimerStarted AND (FGridState = gsSelecting) then
   begin
    SetTimer(Handle, 1, 60, nil);
    TimerStarted := True;
   end;
  inherited;
end;


function TStringGridSF.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited;
  if Result AND HackedMousedown then
   begin
    // MoveColRow calls MoveCurrent, which calls SelectCell. If SelectCell returns False,
    // then movement is blocked. But we fake it by re-calling with Show=False to get the behaviour we want
    HackedMouseDown := False;
    TRY
      MoveColRow(ACol, ARow, True, False);
    FINALLY
      HackedMouseDown := True;
    END;
    Result := False;
   end;
end;

end.
