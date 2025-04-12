UNIT cvCreationOrderTester;

{-------------------------------------------------------------------------------------------------------------
 2019-07-02
 
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

   Dropping control on a form:
      Create
      AfterConstruction
      SetParent
       CreateWnd
        CreateWindowHandle
       CreateWnd post
      SetParent post

   Executing the program (DFM streaming)
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

   Dynamic creation of the control
     Create
     AfterConstruction
     SetParent
      CreateWnd
       CreateWindowHandle
      CreateWnd post
     SetParent post     // SetParent is called during construction AND also during destruction with aParent=nil

    ----

    Deleting control from form:
      SetParent
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

   Reconstructing the form
      Not tested yet
-------------------------------------------------------------------------------------------------------------}

INTERFACE
USES
  System.SysUtils, System.IOUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls;

TYPE
  TCreationOrderTest = class(TPanel)
    private
      FButton: TButton;
      Initialized: Boolean;
      CONST LogActive= FALSE;
    protected
      procedure Loaded; override;
      procedure CreateWnd; override;
      procedure CreateWindowHandle(const Params: TCreateParams); override;
      procedure SetParent(AParent: TWinControl); override;      { SetParent is called during construction AND also during deconstruction with aParent=nil }
    public
      procedure AfterConstruction; override;
      constructor Create(AOwner: TComponent); override;
      procedure WriteToString(s: string);
    published
      property Button: TButton read FButton;
  end;




procedure Register;

IMPLEMENTATION





constructor TCreationOrderTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Caption }
  Caption:= '';         // Doesn't work if I set caption to '' (in the constructor) but works if I set it to anything else than empty string
  ShowCaption := FALSE; // https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040
  ControlStyle:= ControlStyle - [cssetcaption];

  { Subcomponent }
  FButton:= TButton.Create(Self);
  FButton.SetSubComponent(TRUE);    // As in VclExtCtrls.TBoundLabel
  FButton.Parent   := Self;         // Typically, this works fine, provided that the sub-control does not require a valid HWND right way. Remy Lebeau
  FButton.Visible  := TRUE;
  FButton.Align    := alLeft;
  FButton.Font.Size:= 20;
  FButton.Caption  := 'SOMETHING';

  { LOG }
  WriteToString(#13#10);
  WriteToString('_____________________'+ #13#10);
  WriteToString(TimeToStr(Now)+ #13#10);
  WriteToString('ComponentState: '+ #13#10);
  if csLoading          in ComponentState then WriteToString(' csLoading'+ #13#10);
  if csReading          in ComponentState then WriteToString(' csReading'+ #13#10);
  if csDesigning        in ComponentState then WriteToString(' csDesigning'+ #13#10);
  if csAncestor         in ComponentState then WriteToString(' csAncestor'+ #13#10);
  if csUpdating         in ComponentState then WriteToString(' csUpdating'+ #13#10);
  if csFixups           in ComponentState then WriteToString(' csFixups'+ #13#10);
  if csInline           in ComponentState then WriteToString(' csInline'+ #13#10);
  if csDesignInstance   in ComponentState then WriteToString(' csDesignInstance'+ #13#10);
  if csWriting          in ComponentState then WriteToString(' csWriting'+ #13#10);               // cannot happen in constructor
  if csDestroying       in ComponentState then WriteToString(' csDestroying'+ #13#10);            // cannot happen in constructor
  if csFreeNotification in ComponentState then WriteToString(' csFreeNotification'+ #13#10);      // cannot happen in constructor
  WriteToString(#13#10);
  WriteToString('Create'+ #13#10);
end;


procedure TCreationOrderTest.AfterConstruction;  { This is not different than calling the code at the end of Create constructor. }
begin
  inherited;
  WriteToString('AfterConstruction'+ #13#10);
end;


procedure TCreationOrderTest.Loaded;
begin
  inherited;
  WriteToString('Loaded'+ #13#10);
end;



{Important notes:
    1. CreateWnd could be create much much later, after Create.
       For example if this control is on a form that is created hidden, CreateWnd will not be called until the form is shown!

    2. CreateWnd can be called more than once:
       http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html

       It is called when we set:
           MyControl.Parent:= Form1;
}
procedure TCreationOrderTest.CreateWnd;
begin
  WriteToString(' CreateWnd'+ #13#10);
  inherited;
  WriteToString(' CreateWnd post'+ #13#10);
end;


procedure TCreationOrderTest.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  WriteToString('  CreateWindowHandle'+ #13#10);
end;


{ SetParent is called during construction AND also during deconstruction (with aParent=Nil) }
procedure TCreationOrderTest.SetParent(AParent: TWinControl);
begin
  WriteToString('SetParent'+ #13#10);
  inherited SetParent(AParent);
  WriteToString('SetParent post'+ #13#10);

  if NOT Initialized then { Make sure we don't call this code twice }
   begin
     Initialized:= TRUE;
     Caption:= 'SOMETHING';    // Doesn't work if I set caption to '' (in the constructor) but works if I set it to anything else than empty string
   end;
end;







{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}
procedure TCreationOrderTest.WriteToString(s: string);
begin
  if LogActive
  then System.IOUtils.TFile.AppendAllText('c:\MyProjects\LightSaber\CreationOrder.txt', s);
  { Without a full path, the output will be in Delphi\bin folder when the control is used inside the IDE (dropped on a form) and in app's folder when running inside the EXE file. }
end;


procedure Register;
begin
  RegisterComponents('LightSaber', [TCreationOrderTest]);
end;


end.


