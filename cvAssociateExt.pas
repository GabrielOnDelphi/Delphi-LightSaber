UNIT cvAssociateExt;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  http://stackoverflow.com/questions/23316113/control-has-no-parrent-window-error
  http://edn.embarcadero.com/article/20569

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  cvCheckBox, cbDialogs;

TYPE
  TAssociateFileExt = class(TGroupBox)
  private
    Initialized: Boolean;
    FFileType: string;
    FAssocName: string;
    FAssociateBtn: TButton;
  protected
    procedure CreateWnd; override;
  public
    btnAssociateDel: TButton;
    chkAllUsers    : TCubicCheckBox;
    constructor Create(aOwner: TComponent); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;

    procedure chkAllUsersClick  (Sender: TObject);
    procedure btnAssociateClick (Sender: TObject);
    procedure btnAssociateDelClick(Sender: TObject);
  published
    property FileType : string read FFileType  write FFileType;                                    { Example:   FileType:= '.txt'  }
    property AssocName: string read FAssocName write FAssocName;                                   { Example:   Winamp Audio File  }
  end;


procedure Register;
{$WARN GARBAGE OFF}   {Silent the: 'W1011 Text after final END' warning }


IMPLEMENTATION
USES
    cmPermissions, csShell;


constructor TAssociateFileExt.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);    // Note: Don't set 'Parent:= Owner' in constructor. See this for details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 FFileType:= '.txt';
 FAssocName:= 'Demo Application';

 FAssociateBtn:= TButton.Create(Self);
 FAssociateBtn.SetSubComponent(True);
 FAssociateBtn.Parent:= Self;            // Here I can set the parent

 btnAssociateDel:= TButton.Create(Self);
 btnAssociateDel.Parent:= Self;      // Here I can set the parent

 chkAllUsers:= TCubicCheckBox.Create(Self);
 chkAllUsers.Parent:= Self;       // Here I can set the parent
end;




//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TAssociateFileExt.CreateWnd;
begin
 inherited CreateWnd;

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;
   Height:= 129;
   Width := 174;
   Caption:= 'Associate with...';   // https://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create  // Creatend   CreateParams   CreateindowHandle
   DoubleBuffered:= True;

   FAssociateBtn.Visible:= TRUE;
   FAssociateBtn.SetBounds(17, 26, 142, 25);
   FAssociateBtn.Hint:= 'Associate this application with its files. When you double click a file this program will automatically start and load that file.';
   FAssociateBtn.Caption:= 'Associate';
   FAssociateBtn.DoubleBuffered:= TRUE;
   FAssociateBtn.ParentDoubleBuffered:= FALSE;
   FAssociateBtn.ParentFont:= FALSE;
   FAssociateBtn.TabOrder:= 0;
   FAssociateBtn.OnClick:= btnAssociateClick;

   btnAssociateDel.Visible:= TRUE;
   btnAssociateDel.Left:= 17;
   btnAssociateDel.Top:= 53;
   btnAssociateDel.Width:= 142;
   btnAssociateDel.Height:= 25;
   btnAssociateDel.Hint:= 'Remove association';
   btnAssociateDel.Caption:= 'Remove association';
   btnAssociateDel.DoubleBuffered:= TRUE;
   btnAssociateDel.ParentDoubleBuffered:= FALSE;
   btnAssociateDel.ParentFont:= FALSE;
   btnAssociateDel.TabOrder:= 1;
   btnAssociateDel.OnClick:= btnAssociateDelClick;

   chkAllUsers.Visible:= TRUE;
   chkAllUsers.Left:= 31;
   chkAllUsers.Top:= 97;
   chkAllUsers.Width:= 115;
   chkAllUsers.Height:= 17;
   chkAllUsers.Hint:= 'Please note that if you want to do this for all users then you need administrator/elevated rights.';
   chkAllUsers.Caption:= 'Do this for all users';
   chkAllUsers.DoubleBuffered:= TRUE;
   chkAllUsers.ParentDoubleBuffered:= FALSE;
   chkAllUsers.ParentFont:= FALSE;
   chkAllUsers.TabOrder:= 2;
   chkAllUsers.OnClick:= chkAllUsersClick;
  end;
end;

procedure TAssociateFileExt.CreatewindowHandle(const Params: TCreateParams);
begin
 inherited CreatewindowHandle(Params);
end;


procedure TAssociateFileExt.chkAllUsersClick(Sender: TObject);
begin
 FAssociateBtn  .ElevationRequired:= chkAllUsers.Checked;
 btnAssociateDel.ElevationRequired:= chkAllUsers.Checked;
end;


procedure TAssociateFileExt.btnAssociateClick(Sender: TObject);
begin
 if AssocName= ''
 then AssocName:= Application.ExeName;

 if AssociateWith(FileType, AssocName, chkAllUsers.Checked, TRUE, TRUE)
 then MesajInfo('The program will now automatically start when you double click a '+ FileType+ ' file.')
 else
    if chkAllUsers.Checked AND NOT AppHasAdminRights
    then MesajInfo('On Windows Vista/7 you need to run the program with administrator rights in order to set file association for ALL users in the system!');
end;


procedure TAssociateFileExt.btnAssociateDelClick(Sender: TObject);
begin
 if AssociationReset(FileType, chkAllUsers.Checked)
 then MesajInfo('Association removed.')
 else MesajInfo('Cannot delete file association.');
end;




procedure Register;
begin
  RegisterComponents('LightSaber', [TAssociateFileExt]);
end;


end.
