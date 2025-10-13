UNIT LightVcl.Visual.MsgDispatcher;

{=============================================================================================================
   2024.05
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

  See: What's the difference between CreateWnd and CreateWindowHandle? https://stackoverflow.com/questions/582903/whats-the-difference-between-createwnd-and-createwindowhandle
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Graphics,
   LightVcl.Visual.GradientPanel, AniImg;

TYPE
 TOnClose = procedure(Sender: TObject) of object;

 TMsgDispatcher = class(TPanel)
   private
     Initialized: Boolean;
     FAnimate   : TAnimateImage;
     FCloseBtn  : TButton;
     FGradient  : TGradPanel;
     FMainText  : TLabel;
     FMessages  : TStringList;
     FMuteBtn   : TButton;
     FRightPanel: TPanel;
     FSound     : string;
     FTime      : Integer;
     FMute      : Boolean;
     FTimer     : TTimer;
     procedure setTime(Value: Integer);
   protected
     FOnClose: TOnClose;
     procedure TimerTimer  (Sender: TObject);
     procedure ClickExecute(Sender: TObject);
     procedure CloseExecute(Sender: TObject);
     procedure MuteExecute (Sender: TObject);
     procedure PlayTopMsg;                                                                         { When there are no Winapi.Messages, I stop the timer }
     procedure StartAnimation;
     procedure CreateWnd; override;
   public
     constructor Create(AOwner: TComponent); override;
     destructor  Destroy;                    override;
     procedure AddMessage(sMsg: string);
     procedure ClearMessages;                                                                      { Clear all existing messages }
     procedure AddBlank;
     procedure SetTimerNextRow;                                                                    { Set time out based on the number of words in the next string that I have to display }
     procedure SetImageList(Value: TImageList);
   published
     property dSoundFile  : string   read FSound   write FSound;
     property dTimePerWord: Integer  read FTime    write setTime default 320;                      { How fast can the text disapear from screen (miliseconds per char) }

     property Mute        : Boolean  read FMute    write FMute default FALSE;                      { How fast can the text disapear from screen (miliseconds per char) }
     property OnClose     : TOnClose read FOnClose Write FOnClose;                                 { The user pushed the little Close button and the control is not visible on screen anymore }
 end;


procedure Register;

IMPLEMENTATION
USES LightVcl.Common.Sound, LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.Dialogs;
{$WARN GARBAGE OFF}                                                                                {Silence the: 'W1011 Text after final END' warning }
{.$D-}



{--------------------------------------------------------------------------------------------------
   CREATE/DESTROY
--------------------------------------------------------------------------------------------------}
constructor TMsgDispatcher.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);    // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 { Animated image }
 FAnimate:= TAnimateImage.Create(Self);                                                            { It will be freed by the Parent control }
 FAnimate.Parent:= Self;   //But I can set Parent for child controls
 //FAnimate.SetSubComponent(True);

 { Gradient }
 FGradient:= TGradPanel.Create(Self);                                                               { It will be freed by the Parent control }
 FGradient.Parent:= Self;    //But I can set Parent for child controls

 { lblMainText }
 FMainText:= TLabel.Create(Self);                                                                  { It will be freed by the Parent control }
 FMainText.Parent:= Self;    //But I can set Parent for child controls

 { Panel - Close/Mute holder }
 FRightPanel:= TPanel.Create(Self);                                                                { It will be freed by the Parent control }
 FRightPanel.Parent:= Self;    //But I can set Parent for child controls

 { Close button }
 FCloseBtn:= TButton.Create(Self);                                                                 { It will be freed by the Parent control }
 FCloseBtn.Parent:= FRightPanel;

 { Mute button }
 FMuteBtn:= TButton.Create(Self);                                                                  { It will be freed by the Parent control }
 FMuteBtn.Parent:= FRightPanel;

 FMute:= FALSE;
 FMessages:= TStringList.Create;
 FTimer:= TTimer.Create(NIL);
 FTimer.OnTimer:= TimerTimer;
 FTime:= 320;  {ms}

 { Show the first message which explains what is this control }
 //FMainText.Font.Size:= 11;
 AddMessage('TUTORIAL PANEL - Hints and tutorial messages will be displayed here.');
end;


//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TMsgDispatcher.CreateWnd;
begin
 inherited CreateWnd;

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;
   Width  := 544;
   Height := 46;
   Anchors:= [akLeft, akTop, akRight, akBottom];
   BevelInner := bvLowered;
   //Caption:= '';
   ShowCaption := FALSE; // https://stackoverflow.com/questions/56859524/how-to-initialize-a-custom-control/64974040?noredirect=1#comment114931111_64974040

   with FAnimate DO
    begin
     AutoSize:= FALSE;
     Width  := 40;
     Align  := alLeft;
     Center := TRUE;
     Active := FALSE;
     Transparent:= TRUE;
     NumLoops:= 1;                                                                                   { Set to loop only once }
     Interval:= 50;
    end;

   with FGradient DO
    begin
     Align   := alClient;
     Color1  := 16762056;
     Color2  := 15657160;
     //Style     := gsDiagonalRF;
    end;

   with FMainText DO
    begin
     Align      := alClient;
     Caption    := 'Welcome';   //  ANYTHING that requires a component to have its HWND cannot be done inside a constructor. For example, you cannot set the Caption of a custom control in its own constructor. Do it later in SetParent() or Loaded().
     Font.Color := clBlack;
     Font.Style := [fsBold];
     Font.Size  := 11;
     ParentFont := FALSE;
     Transparent:= TRUE;
     WordWrap   := TRUE;
     Layout     := tlCenter;
     OnClick    := ClickExecute;
    end;

    with FRightPanel DO
    begin
     Align  := alRight;
     Caption:= '';
     Width  := 22;
    end;

   with FCloseBtn DO
    begin
     Top        := 2;
     Left       := 2;
     Width      := 18;
     Height     := 18;
     Caption    := 'X';
     Hint       := 'Close';
     ShowHint   := TRUE;
     ParentFont := FALSE;
     OnClick    := CloseExecute;
    end;

   with FMuteBtn DO
    begin
     Top        := FCloseBtn.Top+ FCloseBtn.Height+ 3;
     Left       := 2;
     Width      := 18;
     Height     := 18;
     Caption    := 'M';
     Hint       := 'Mute'+CRLFw+ 'Toggle sound on/off';
     ShowHint   := TRUE;
     ParentFont := FALSE;
     OnClick    := MuteExecute;
    end;
  end;
end;


destructor TMsgDispatcher.Destroy;
begin
 Assert(FTimer<> NIL, 'Timer is NIL.');  //this hsould not happen
 FreeAndNil(FTimer);
 FreeAndNil(FMessages);
 // asta da un AV cand inchid Delphi, asa ca l-am scos de aici:
 // StopAnimation;

 inherited Destroy;
end;



procedure TMsgDispatcher.CloseExecute(Sender: TObject);
begin
 Visible:= FALSE;
 if Assigned(FOnClose)
 then FOnClose(Self);
end;





{--------------------------------------------------------------------------------------------------
   ADD MESSAGE
--------------------------------------------------------------------------------------------------}
procedure TMsgDispatcher.AddMessage(sMsg: string);
begin
 FMessages.Add(sMsg);
 if NOT FTimer.Enabled                                                                             { If the list of messages is empty... }
 then PlayTopMsg;                                                                                  { Display the message right now }
end;


procedure TMsgDispatcher.ClearMessages;                                                            { Clear all existing messages }
begin
 FMessages.Clear;
 FTimer.Enabled:= FALSE;
end;



procedure TMsgDispatcher.AddBlank;
begin
 if Visible then
  begin
    FMessages.Add(' ');
    if NOT FTimer.Enabled                                                                          { If the list of messages is empty... }
    then PlayTopMsg;                                                                               { Display the message right now }
  end;
end;






 

{--------------------------------------------------------------------------------------------------
   TIMER
--------------------------------------------------------------------------------------------------}
procedure TMsgDispatcher.SetTimerNextRow;                                                          { Set time out based on the number of words in the next string that I have to display }
CONST ctSlackTime= 1500;
begin
 FTimer.Enabled:= FMessages.Count> 0;

 if FTimer.Enabled
 then
   if FMessages[0]= ' '                                                                            { Blank the Tutorial }
   then FTimer.Interval:= 3500
   else FTimer.Interval:= dTimePerWord* WordCount(FMessages[0])+ ctSlackTime;
end;



procedure TMsgDispatcher.PlayTopMsg;                                                               { Play message 0 right now. If there are no Winapi.Messages, I stop the timer, else I prepere the timer to trigger after x seconds }
begin
 if FMessages.Count> 0 then
  begin
    { Show next message }
    FMainText.Caption:= FMessages[0];

    if Visible AND (FMessages[0]<> ' '  ) then
     begin
      { Play GIF & sound }
      if (FAnimate.Images <> NIL) then StartAnimation;
      if (FileExists(dSoundFile)) AND NOT Mute
      then PlaySoundFile(dSoundFile);
     end;

    { Set timeout for the next row }
    SetTimerNextRow;
  end;
end;



procedure TMsgDispatcher.TimerTimer(Sender: TObject);                                              { Everytimne the time lapse I pickup and display the next message. When there are no Winapi.Messages, I stop the timer }
begin
 { Delete top message }
 if FMessages.Count> 0
 then FMessages.Delete(0);

 if FMessages.Count> 0
 then PlayTopMsg
 else FTimer.Enabled:= FALSE;

 FMainText.Font.Size:= 8;                                                                          { On Create, the font was set to 11. Now put the text back to normal }
end;



procedure TMsgDispatcher.ClickExecute(Sender: TObject);
begin
 FTimer.Enabled:= FALSE;
 TimerTimer(Sender);
end;



procedure TMsgDispatcher.setTime(Value: Integer);
begin
 if Value< 5
 then MessageWarning('Interval is too small.');

 FTime:= Value;
end;







{--------------------------------------------------------------------------------------------------
   ANIMATION
--------------------------------------------------------------------------------------------------}
procedure TMsgDispatcher.StartAnimation;
begin
 FAnimate.Active:= FALSE;
 FAnimate.Active:= TRUE;
end;




procedure TMsgDispatcher.SetImageList(Value: TImageList);
begin
 FAnimate.Active:= FALSE;
 FAnimate.Images:= Value;

 if (Value<> NIL) then
  begin
   FAnimate.Width:= Value.Width;
   FMainText.Left:= FAnimate.Left+ FAnimate.Width+ 1;
  end
end;











procedure TMsgDispatcher.MuteExecute(Sender: TObject);
begin
 Mute:= NOT Mute;
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TMsgDispatcher]);
end;



end.===========================================================================                    {

 TGIFImage(Animate.Picture.Graphic).PaintStart;
 (Animate.Picture.Graphic as TGIFImage).DrawOptions:= (Animate.Picture.Graphic as TGIFImage).DrawOptions+ [goAnimate];
 (Animate.Picture.Graphic as TGIFImage).DrawOptions:= (Animate.Picture.Graphic as TGIFImage).DrawOptions+ [goLoop];
 (Animate.Picture.Graphic as TGIFImage).DrawOptions:= (Animate.Picture.Graphic as TGIFImage).DrawOptions- [goLoop];
 Animate.Invalidate;
