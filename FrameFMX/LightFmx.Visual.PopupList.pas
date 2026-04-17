UNIT LightFmx.Visual.PopupList;

{=============================================================================================================
   2026.04
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TPopupList — a lightweight FMX popup menu built on TPopup + TListBox.

   Why not TPopupMenu?
     TPopupMenu has an unfixed FMX bug (present since early XE versions, still broken in Delphi 13.1):
     when menu items have Visible=False, FMX temporarily reparents only the VISIBLE items to the
     popup window on open, then fails to restore hidden items to their original index positions on
     close. Hidden items get pushed to the top of the list, so items silently reorder on every
     open/close cycle. There is also no OnPopup event, and TPopupMenu doesn't work on Android/iOS.

   Usage:
     FMenu := TPopupList.Create(Self, btnTrigger);
     mniFoo := FMenu.AddItem('Foo');
     mniBar := FMenu.AddItem('Bar');
     FMenu.OnItemSelected := procedure(Item: TListBoxItem)
       begin
         if Item = mniFoo then DoFoo
         else if Item = mniBar then DoBar;
       end;

     // In trigger button click handler:
     FMenu.Open;

     // Show/hide items at any time — order is always stable:
     mniFoo.Visible := SomeCondition;

     // In containing class destructor:
     FreeAndNil(FMenu);

   Ownership:
     TPopupList creates TPopup with nil owner and sets Parent to AParent.
     TPopupList owns the TPopup exclusively — free TPopupList in the containing class destructor.
     Freeing TPopupList also frees TPopup, TListBox, and all TListBoxItems returned by AddItem.

   Hover:
     Each item gets a TRectangle background (HitTest=False, stored in TagObject).
     OnMouseMove colors the hovered item's rect; OnMouseLeave + PopupClosed clear all highlights.
==============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Controls.Presentation, FMX.ListBox, FMX.Objects, FMX.Graphics;


TYPE
  TPopupList = class
  private
    FPopup          : TPopup;
    FListBox        : TListBox;
    FOnItemSelected : TProc<TListBoxItem>;
    procedure ListBoxChange    (Sender: TObject);
    procedure PopupClosed      (Sender: TObject);
    procedure ListBoxMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ListBoxMouseLeave(Sender: TObject);
    procedure ClearHover;
  public
    constructor Create(AParent: TFmxObject; PlacementTarget: TControl; Width: Single = 180);
    destructor  Destroy; override;
    function    AddItem(const Text: string): TListBoxItem;
    procedure   Open;
    property    OnItemSelected: TProc<TListBoxItem> read FOnItemSelected write FOnItemSelected;
  end;


IMPLEMENTATION

// TThread.ForceQueue is in System.Classes (already in interface USES — no re-declaration needed)

CONST
  HOVER_COLOR: TAlphaColor = $20A0C0E0;   // ~12% alpha blue-gray — visible on both light and dark themes


{---------------------------------------------------------------------------------------------
   TPopupList
---------------------------------------------------------------------------------------------}
constructor TPopupList.Create(AParent: TFmxObject; PlacementTarget: TControl; Width: Single);
begin
  inherited Create;

  FPopup                  := TPopup.Create(NIL);   // TPopupList owns it — freed in Destroy
  FPopup.Parent           := AParent;
  FPopup.PlacementTarget  := PlacementTarget;
  FPopup.Placement        := TPlacement.Bottom;
  FPopup.Width            := Width;
  FPopup.Height           := 0;                    // Hide inline — only visible when IsOpen=TRUE
  FPopup.OnClosePopup     := PopupClosed;

  FListBox                := TListBox.Create(FPopup);
  FListBox.Parent         := FPopup;
  FListBox.Align          := TAlignLayout.Client;
  FListBox.ShowScrollBars := FALSE;
  FListBox.ItemHeight     := 30;
  FListBox.ItemIndex      := -1;
  FListBox.OnMouseMove    := ListBoxMouseMove;
  FListBox.OnMouseLeave   := ListBoxMouseLeave;
  FListBox.OnChange       := ListBoxChange;        // Assigned AFTER items (added via AddItem) to prevent construction-time firing
end;


destructor TPopupList.Destroy;
begin
  // Clear event handlers before freeing — prevents FMX from calling back into freed Self
  // during any deferred destruction steps
  if Assigned(FListBox) then
    begin
      FListBox.OnChange     := NIL;
      FListBox.OnMouseMove  := NIL;
      FListBox.OnMouseLeave := NIL;
    end;
  if Assigned(FPopup) then FPopup.OnClosePopup := NIL;
  FreeAndNil(FPopup);   // Also frees FListBox (owned by FPopup) and all TListBoxItems
  inherited;
end;


{ Creates a TListBoxItem and adds a transparent TRectangle behind its text.
  The rect is stored in TagObject for fast access during hover. }
function TPopupList.AddItem(const Text: string): TListBoxItem;
VAR Bg: TRectangle;
begin
  Result      := TListBoxItem.Create(FListBox);
  Result.Text := Text;
  FListBox.AddObject(Result);    // Triggers ApplyStyle — styled text children added here

  // Add hover background AFTER style application so SendToBack places it behind styled text
  Bg              := TRectangle.Create(Result);
  Bg.Parent       := Result;
  Bg.Align        := TAlignLayout.Client;
  Bg.Fill.Color   := TAlphaColors.Null;   // Transparent — colored on hover
  Bg.Stroke.Kind  := TBrushKind.None;
  Bg.HitTest      := FALSE;               // Don't block clicks on the item
  Bg.SendToBack;
  Result.TagObject:= Bg;
end;


procedure TPopupList.Open;
VAR VisibleCount, i: Integer;
begin
  FListBox.ItemIndex      := -1;   // Reset so re-selecting same item still fires OnChange
  VisibleCount            := 0;
  for i:= 0 to FListBox.Count - 1 do
    if FListBox.ListItems[i].Visible then Inc(VisibleCount);
  FPopup.Height              := VisibleCount * FListBox.ItemHeight + 2;  // +2: 1px top+bottom border from style
  FPopup.PreferedDisplayIndex:= -1;   // Force FMX to detect correct monitor from PlacementTarget
  FPopup.IsOpen              := TRUE;
end;


procedure TPopupList.PopupClosed(Sender: TObject);
begin
  FPopup.Height      := 0;    // Collapse inline so it doesn't intercept clicks on the parent control
  FListBox.ItemIndex := -1;
  ClearHover;
end;


procedure TPopupList.ClearHover;
VAR i: Integer;
begin
  for i:= 0 to FListBox.Count - 1 do
    if FListBox.ListItems[i].TagObject is TRectangle then
      TRectangle(FListBox.ListItems[i].TagObject).Fill.Color:= TAlphaColors.Null;
end;


procedure TPopupList.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
VAR i: Integer;
    Item, Hovered: TListBoxItem;
begin
  Hovered:= FListBox.ItemByPoint(X, Y);
  for i:= 0 to FListBox.Count - 1 do
    begin
      Item:= FListBox.ListItems[i];
      if Item.TagObject is TRectangle then
        if Item = Hovered
        then TRectangle(Item.TagObject).Fill.Color:= HOVER_COLOR
        else TRectangle(Item.TagObject).Fill.Color:= TAlphaColors.Null;
    end;
end;


procedure TPopupList.ListBoxMouseLeave(Sender: TObject);
begin
  ClearHover;
end;


procedure TPopupList.ListBoxChange(Sender: TObject);
VAR Item    : TListBoxItem;
    Callback: TProc<TListBoxItem>;
begin
  if NOT FPopup.IsOpen then EXIT;       // Ignore spurious OnChange when popup isn't shown
  if FListBox.ItemIndex < 0 then EXIT;
  Item     := FListBox.ListItems[FListBox.ItemIndex];
  Callback := FOnItemSelected;          // Capture value (not Self) — keeps the anon-method ref-counted independently
  FPopup.IsOpen := FALSE;

  // Defer to next message loop — FMX popup teardown interferes with modal forms if called inline.
  // Safety contract: Item is a raw TListBoxItem* owned by this TPopupList. The caller must ensure
  // TPopupList outlives the queued proc — i.e. do not call FreeAndNil(FMenu) synchronously in
  // a path that can run before the next message loop tick. In practice this holds because any
  // Free of the containing object is itself queued via ForceQueue, which is FIFO after this one.
  if Assigned(Callback) then
    TThread.ForceQueue(NIL, procedure
      begin
        Callback(Item);
      end);
end;


end.
