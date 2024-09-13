UNIT FormUniversalEula;
{-------------------------------------------------------------------------------------------------------------
   2022.03
   DON'T ADD IT TO ANY DPK!
-------------------------------------------------------------------------------------------------------------}

INTERFACE
{$DENYPACKAGEUNIT ON} {Prevents unit from being placed in a package. https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Packages_(Delphi)#Naming_packages }

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

TYPE
  TfrmEULA = class(TForm)
    btnOK: TButton;
    mmoLicense: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
  end;

procedure ShowEulaModal;

implementation {$R *.dfm}
Uses
   ccIO, ccTextFile, cbAppData;



procedure ShowEulaModal;
begin
 var frmEULA:= TfrmEULA.Create(Nil);

 Assert(frmEULA.FormStyle= fsStayOnTop, 'EULA form is not fsStayOnTop!');
 Assert(frmEULA.Visible = False);  // The form visibility must be False in the editor, otherwise ShowModal won't work!

 if FileExists(AppData.SysDir+ 'Eula.txt')
 then frmEULA.mmoLicense.Text:= StringFromFile(AppData.SysDir+ 'Eula.Text');

 frmEULA.ShowModal;
end;


procedure TfrmEULA.btnOKClick(Sender: TObject);
begin
 Close;
end;


procedure TfrmEULA.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:= TCloseAction.caFree;
end;


procedure TfrmEULA.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 27) OR (Key = Winapi.Windows.VK_RETURN) then Close;
end;


end.
