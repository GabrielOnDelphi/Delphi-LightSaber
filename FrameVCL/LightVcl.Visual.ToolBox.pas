UNIT LightVcl.Visual.ToolBox;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   A panel that acts as a toolbox. You can drag it arround the screen with the mouse and close it from the X button.
   Useful for non-window tools such as Search boxes.

   Also see:
      c:\Myprojects\Packages\Third party packages\FloatingWindow.pas
=============================================================================================================}
//ToDo: Later: Rename to TToolBoxPanel
INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.UITypes, System.Classes, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls;

TYPE
  TToolBox = class(TCustomPanel)
   private
    Initialized : Boolean;
    FCloseButton: TButton;
    FTopBar: TLabel;
    FDragging: Boolean;                                                                              { Drag state - TRUE while user is dragging the toolbox }
    FDragStartPos: TPoint;                                                                           { Cursor position when drag started }
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

USES LightVcl.Common.CenterControl;




constructor TToolBox.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                       { Dont set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }

 FCloseButton:= TButton.Create(Self);            { Freed by: Self }
 FCloseButton.Parent := Self;                    // Here I can set the parent
 FCloseButton.SetSubComponent(True);

 FTopBar:= TLabel.Create(Self);                   { Freed by: Self }
 FTopBar.Name:= 'TopBar';
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
   FCloseButton.Left   := ClientWidth - FCloseButton.Width - 1;
   FCloseButton.Anchors:= [akRight, akTop];
   FCloseButton.Hint   := 'Close';
   FCloseButton.Caption:= 'X';
   FCloseButton.OnClick:= CloseButtonClick;
   FCloseButton.Show;
  end;
end;









{--------------------------------------------------------------------------------------------------
   VISIBILITY
--------------------------------------------------------------------------------------------------}

procedure TToolBox.CloseButtonClick(Sender: TObject);
begin
 Visible:= FALSE;
end;


{ Shows the toolbox and ensures it is positioned within the bounds of the specified Parent control. }
procedure TToolBox.ShowIt(Parent: TControl);
begin
 Visible:= TRUE;
 BringToFront;
 CorrectCtrlPosition(Self, Parent);
end;




function TToolBox.getCaption: string;
begin
 Result:= FTopBar.Caption;
end;

procedure TToolBox.setCaption(CONST Value: string);
begin
 FTopBar.Caption:= Value;
end;







{--------------------------------------------------------------------------------------------------
   DRAG
   Drag functionality allows the user to move the toolbox by clicking and dragging the top bar.
   SetCapture ensures we receive all mouse messages even when cursor leaves the control.
--------------------------------------------------------------------------------------------------}

procedure TToolBox.TopBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FDragging:= TRUE;
 GetCursorPos(FDragStartPos);
 SetCapture(Handle);
end;


procedure TToolBox.TopBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
VAR NewPos: TPoint;
begin
 if FDragging then
  begin
   GetCursorPos(NewPos);
   Left:= Left - FDragStartPos.X + NewPos.X;
   Top := Top  - FDragStartPos.Y + NewPos.Y;
   FDragStartPos:= NewPos;
  end;
end;


procedure TToolBox.TopBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FDragging then
  begin
   ReleaseCapture;
   FDragging:= FALSE;
  end;
end;













procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TToolBox]);
end;



end.
