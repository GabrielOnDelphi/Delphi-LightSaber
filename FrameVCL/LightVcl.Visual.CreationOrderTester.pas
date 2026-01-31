UNIT LightVcl.Visual.CreationOrderTester;

{-------------------------------------------------------------------------------------------------------------
 2026.01.31

 Multiple constructors:
   https://stackoverflow.com/questions/1758917/delphi-pascal-overloading-a-constructor-with-a-different-prototype

 A custom control used for experimenting with the initialization (constructor) sequence.
   https://stackoverflow.com/questions/56859524

 Rule:
   In a custom control NEVER set Self.Parent:= aOwner. The parent of this control (or the DFM) will set its parent.
   However, you can set SubComponent.Parent:= Self;
   Source: https://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create/23491286?noredirect=1#comment114893484_23491286

 Best place
   Assuming your control is a TCustomControl you should probably override CreateWindowHandle().
   This has the benefit that all initialization is correctly repeated every time a new window handle is created for the control.
   This allows to change some window style flags that can not be set or reset without recreating the window.
   It does also allow to conserve resources by freeing the handle when it is not needed, and recreating it later.
   https://stackoverflow.com/questions/582721/why-shouldnt-you-use-a-handle-during-component-creation-or-streaming

   BUT I need to make sure I don't create objects twice! So I need a boolean var.

 Discussions:
       Why shouldn't you use a handle during component creation or streaming?
        https://stackoverflow.com/questions/582721/why-shouldnt-you-use-a-handle-during-component-creation-or-streaming
        Allen Bauer: At it's core, it's a performance thing. There are potentially other "bad" side-effects that can happen as well since during the streaming process. Things are in "mid-construction" and all that is normally expected to be there are probably not. When you reference the "Handle" property, this will initiate the handle creation process. This is because reading Handle actually calls GetHandle. Do this too soon in the streaming process, and you may end up with, at best, slower streaming performance, at worse, a partially configured "handle." If you need to refer to the Handle properly from within a property setter, you should check if the handle has been created by checking HandleAllocated, and only then do you reference it. If you needed to make some flag changes to the handle like calling SetWindowLong() or something, then you should "cache" that state in the component instance and then override CreateWnd and apply those settings at that point. Another option is to defer all handle access while streaming (if csLoading in ComponentState then) until the Loaded virtual method is called. Finally, you need to be aware of cases where your handle may need to get recreated. This can happen if the surrounding form or the parent component's handle goes through a recreate process. Up until more recent releases of Windows, the only way to change some window flags was to destroy the handle and recreate with new flags in the CreateWindowEx() call. There are many components that still do this. You know if you're in a recreate situation by checking (csRecreating in ControlState).
        !So, THE BEST PLACE is to override CreateWnd and do your work in there. CreateWnd will only be called when the handle gets created. A properly designed component should get only one call to CreateWnd right before it is going to be shown!
        -
        mghie: Assuming your control is a TCustomControl you should probably override CreateWindowHandle(). This has the benefit that all initialization is correctly repeated every time a new window handle is created for the control. This allows to change some window style flags that can not be set or reset without recreating the window. It does also allow to conserve resources by freeing the handle when it is not needed, and recreating it later.

        What's the difference between CreateWnd and CreateWindowHandle?
        CreateWnd can be called more than once:
            http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
            https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle

 Tester:
   c:\Myprojects\Project Testers\Custom component creation order\ProjectTester.dpr


  About constructors
    https://stackoverflow.com/questions/19750813/delphi-6-create-new-form-with-constructor
--------------------------------------------------------------------------------------------------------------

Results of the experiment (this is shown on screen if you run the following experiments):

   Dropping control on a form (design time):
      Create
      AfterConstruction
      SetParent
       CreateWnd
        CreateWindowHandle
       CreateWnd post
      SetParent post

   Executing the program (DFM streaming):
      Create
      AfterConstruction
      SetParent
      SetParent post
      SetParent
      SetParent post
      Loaded
       CreateWnd
        CreateWindowHandle
       CreateWnd post

   Dynamic creation of the control:
      Create
      AfterConstruction
      SetParent
       CreateWnd
        CreateWindowHandle
       CreateWnd post
      SetParent post

   Destroying the control (FreeAndNil):
      BeforeDestruction
      SetParent (AParent=nil)
       DestroyWnd
       DestroyWnd post
      SetParent post
      Destroy

    ----

    Deleting control from form (design time):
      SetParent (AParent=nil)
      SetParent post

    Cutting ctrl from form and pasting it back:
      SetParent
      SetParent post
      Create
      AfterConstruction
      SetParent
       CreateWnd
        CreateWindowHandle
       CreateWnd post
      SetParent post
      SetParent
      SetParent post
      Loaded

   Control on hidden form (Form.Visible=False at design time):
      Create
      AfterConstruction
      SetParent
      SetParent post
      Loaded
      (CreateWnd NOT called yet - deferred until form is shown!)

   When hidden Form.Show is called:
       CreateWnd
        CreateWindowHandle
       CreateWnd post

   RecreateWnd (triggered by changing properties like DoubleBuffered, Ctl3D, etc.):
       DestroyWnd
       DestroyWnd post
       CreateWnd
        CreateWindowHandle
       CreateWnd post

   Reconstructing the form (FormStyle change, etc.):
      Similar to RecreateWnd - the form and all child controls get their handles destroyed and recreated.
-------------------------------------------------------------------------------------------------------------}

INTERFACE
USES
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls;

TYPE
  TCreationOrderTest = class(TPanel)
    private
      FButton: TButton;
      FInitialized: Boolean;
      CONST LogActive= FALSE;
      CONST LogFile= 'c:\Projects\LightSaber\CreationOrder.txt';
    protected
      procedure Loaded; override;
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      procedure CreateWindowHandle(const Params: TCreateParams); override;
      procedure SetParent(AParent: TWinControl); override;      { SetParent is called during construction AND also during destruction with aParent=nil }
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteLog(CONST s: string);
      procedure WriteControlState;
      procedure WriteComponentState;
    published
      property Button: TButton read FButton;
  end;




procedure Register;

IMPLEMENTATION

USES
  LightCore.TextFile;



constructor TCreationOrderTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Caption }
  Caption:= '';         // Doesn't work if I set caption to '' (in the constructor) but works if I set it to anything else than empty string
  ShowCaption := FALSE; // https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040
  ControlStyle:= ControlStyle - [csSetCaption];

  { Subcomponent }
  FButton:= TButton.Create(Self);
  FButton.SetSubComponent(TRUE);    // As in VclExtCtrls.TBoundLabel
  FButton.Parent   := Self;         // Typically, this works fine, provided that the sub-control does not require a valid HWND right way. Remy Lebeau
  FButton.Visible  := TRUE;
  FButton.Align    := alLeft;
  FButton.Font.Size:= 20;
  FButton.Caption  := 'SOMETHING';

  { LOG }
  WriteLog('');
  WriteLog('_____________________');
  WriteLog(TimeToStr(Now));
  WriteComponentState;
  WriteControlState;
  WriteLog('Create');
end;


destructor TCreationOrderTest.Destroy;
begin
  WriteLog('Destroy');
  inherited Destroy;
  WriteLog('Destroy post');
end;


procedure TCreationOrderTest.AfterConstruction;  { This is not different than calling the code at the end of Create constructor. }
begin
  inherited;
  WriteLog('AfterConstruction');
end;


procedure TCreationOrderTest.BeforeDestruction;
begin
  WriteLog('BeforeDestruction');
  inherited;
end;


procedure TCreationOrderTest.Loaded;
begin
  inherited;
  WriteLog('Loaded');
end;



{Important notes:
    1. CreateWnd could be created much much later, after Create.
       For example if this control is on a form that is created hidden, CreateWnd will not be called until the form is shown!

    2. CreateWnd can be called more than once:
       http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html

       It is called when we set:
           MyControl.Parent:= Form1;
}
procedure TCreationOrderTest.CreateWnd;
begin
  WriteLog(' CreateWnd');
  inherited;
  WriteLog(' CreateWnd post');
end;


procedure TCreationOrderTest.DestroyWnd;
begin
  WriteLog(' DestroyWnd');
  inherited;
  WriteLog(' DestroyWnd post');
end;


procedure TCreationOrderTest.CreateWindowHandle(const Params: TCreateParams);
begin
  WriteLog('  CreateWindowHandle');
  inherited CreateWindowHandle(Params);
  WriteLog('  CreateWindowHandle post');
end;


{ SetParent is called during construction AND also during destruction (with aParent=Nil) }
procedure TCreationOrderTest.SetParent(AParent: TWinControl);
begin
  if AParent = NIL
  then WriteLog('SetParent (AParent=nil)')
  else WriteLog('SetParent');
  inherited SetParent(AParent);
  WriteLog('SetParent post');

  if NOT FInitialized then { Make sure we don't call this code twice }
   begin
     FInitialized:= TRUE;
     Caption:= 'SOMETHING';    // Doesn't work if I set caption to '' (in the constructor) but works if I set it to anything else than empty string
   end;
end;






{-------------------------------------------------------------------------------------------------------------
   LOGGING
-------------------------------------------------------------------------------------------------------------}
procedure TCreationOrderTest.WriteLog(CONST s: string);
begin
  if LogActive
  then StringToFile(LogFile, s + sLineBreak, woAppend, wpOff);
  { Without a full path, the output will be in Delphi\bin folder when the control is used inside the IDE (dropped on a form) and in app's folder when running inside the EXE file. }
end;


procedure TCreationOrderTest.WriteComponentState;
begin
  WriteLog('ComponentState:');
  if csLoading          in ComponentState then WriteLog('  csLoading');
  if csReading          in ComponentState then WriteLog('  csReading');
  if csDesigning        in ComponentState then WriteLog('  csDesigning');
  if csAncestor         in ComponentState then WriteLog('  csAncestor');
  if csUpdating         in ComponentState then WriteLog('  csUpdating');
  if csFixups           in ComponentState then WriteLog('  csFixups');
  if csInline           in ComponentState then WriteLog('  csInline');
  if csDesignInstance   in ComponentState then WriteLog('  csDesignInstance');
  if csWriting          in ComponentState then WriteLog('  csWriting');
  if csDestroying       in ComponentState then WriteLog('  csDestroying');
  if csFreeNotification in ComponentState then WriteLog('  csFreeNotification');
end;


procedure TCreationOrderTest.WriteControlState;
begin
  WriteLog('ControlState:');
  if csLButtonDown      in ControlState then WriteLog('  csLButtonDown');
  if csClicked          in ControlState then WriteLog('  csClicked');
  if csPalette          in ControlState then WriteLog('  csPalette');
  if csReadingState     in ControlState then WriteLog('  csReadingState');
  if csFocusing         in ControlState then WriteLog('  csFocusing');
  if csCreating         in ControlState then WriteLog('  csCreating');          { Set during CreateWnd }
  if csPaintCopy        in ControlState then WriteLog('  csPaintCopy');
  if csCustomPaint      in ControlState then WriteLog('  csCustomPaint');
  if csDestroyingHandle in ControlState then WriteLog('  csDestroyingHandle');  { Set during DestroyWnd }
  if csDocking          in ControlState then WriteLog('  csDocking');
  {$IF CompilerVersion >= 21} { Delphi 2010+ }
  if csRecreating       in ControlState then WriteLog('  csRecreating');        { Set during RecreateWnd - mentioned in Allen Bauer's comment }
  {$IFEND}
end;


procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCreationOrderTest]);
end;


end.
