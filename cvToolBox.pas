UNIT cvToolBox;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   A panel that acts as a toolbox. You can drag it arround the screen with the mouse and close it from the X button.
   Useful for non-window tools such as Search boxes.

   Also see:
      c:\Myprojects\Packages\Third party packages\FloatingWindow.pas
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.UITypes, System.Classes, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TToolBox = class(TCustomPanel)
   private
    Initialized : Boolean;
    FCloseButton: TButton;
    FTopBar: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure TopBarMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TopBarMouseMove (Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TopBarMouseUp   (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function  getCaption: string;
    procedure setCaption(CONST Value: string);
   protected
    procedure CreateWnd;  override;
   public
    constructor Create(AOwner: TComponent); override;
    procedure ShowIt(Parent: TControl);
   published
    property TopCaption: string read getCaption write setCaption;    { I cannot name it 'Caption' because it will conflict with TPanel.Caption }
    property CloseButton: TButton read FCloseButton;
    property TopBar: TLabel read FTopBar;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

IMPLEMENTATION

USES cbCenterControl;




constructor TToolBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                       { Dont set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }

 FCloseButton:= TButton.Create(Self);            { Freed by: self }
 FCloseButton.Parent := Self;                    // Here I can set the parent
 FCloseButton.SetSubComponent(True);

 FTopBar:= TLabel.Create(Self);                   { Freed by: self }
 ftopBar.Name:= 'Caption';
 FTopBar.Parent := Self;                          // Here I can set the parent
 FTopBar.SetSubComponent(True);
 FTopBar.Caption:= ' Tool box';                   { This line cannot be moved to CreateWnd otherwise this BUG appears: It looses the text set via TopCaption. I think I have to set if before "Loaded()" }
end;



procedure TToolBox.CreateWnd; //CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
begin
 inherited CreateWnd;

 if NOT Initialized then  { Make sure we don't call this code twice }
  begin
   Initialized := TRUE;

   Width       := 300;  { DO NOT move this in CreateWnd because controls based on this won't size properly during Create }
   Height      := 120;
   BevelKind   := bkTile;
   ParentColor := TRUE;
   ShowCaption := FALSE;
   Caption     := '';
   {}
   FTopBar.Align       := alTop;
   FTopBar.Height      := 18;
   FTopBar.ParentColor := FALSE;
   FTopBar.Transparent := FALSE;
   FTopBar.Cursor      := crHandPoint;
   FTopBar.AutoSize    := FALSE;
   FTopBar.Color       := TColors.Navy;
   FTopBar.Font.Color  := TColors.White;
   FTopBar.Font.Name   := 'Tahoma';
   FTopBar.Font.Style  := [System.UITypes.TFontStyle.fsBold];
   FTopBar.ParentFont  := FALSE;
   FTopBar.Layout      := tlCenter;
   FTopBar.OnMouseDown := TopBarMouseDown;
   FTopBar.OnMouseMove := TopBarMouseMove;
   FTopBar.OnMouseUp   := TopBarMouseUp;
   FTopBar.Show;
   {}
   FCloseButton.Width  := 22;
   FCloseButton.Height := 20;
   FCloseButton.Top    := 0;
   FCloseButton.Left   := Self.ClientWidth- CloseButton.Width- 1;
   FCloseButton.Anchors:= [akRight, akTop];
   FCloseButton.Hint   := 'Close';
   FCloseButton.Caption:= 'X';
   FCloseButton.OnClick:= CloseButtonClick;
   FCloseButton.Show;
  end;
end;









{--------------------------------------------------------------------------------------------------
   STUFF
--------------------------------------------------------------------------------------------------}
procedure TToolBox.CloseButtonClick(Sender: TObject);  { CLOSE SEARCH BOX }
begin
 Self.Visible:= FALSE;
end;


procedure TToolBox.ShowIt(Parent: TControl);
begin
 Visible:= TRUE;
 BringToFront;
 CorrectCtrlPosition(Self, Parent);
end;




function TToolBox.getCaption: string;
begin
 Result:= TopBar.Caption;
end;

procedure TToolBox.setCaption(const Value: string);
begin
 TopBar.Caption:= Value;
end;







{--------------------------------------------------------------------------------------------------
   DRAG
--------------------------------------------------------------------------------------------------}
VAR
  Dragged: Boolean;
  OldPos: TPoint;


procedure TToolBox.TopBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 Dragged:= True;
 GetCursorPos(OldPos);
 SetCapture(Self.Handle);
end;


procedure TToolBox.TopBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
VAR NewPos: TPoint;
begin
  if Dragged then
   with Self DO
    begin
      GetCursorPos(NewPos);
      Left:= Left-OldPos.X+NewPos.X;
      Top := Top -OldPos.Y+NewPos.Y;
      OldPos:= NewPos;
    end;
end;


procedure TToolBox.TopBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Dragged then
  begin
   ReleaseCapture;
   Dragged:=False;
  end;
end;













procedure Register;
begin
  RegisterComponents('LightSaber', [TToolBox]);
end;



end.
