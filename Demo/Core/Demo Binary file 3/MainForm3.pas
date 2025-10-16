UNIT MainForm3;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, using versioning.
=============================================================================================================}


INTERFACE

USES
  System.SysUtils, System.Classes, System.Contnrs,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin,
  LightVcl.Visual.AppDataForm, LightCore.StreamBuff, uSoldier_v3;

type
  TfrmMain = class(TLightForm)
    btnAddGun    : TButton;
    btnClear     : TButton;
    btnLoad      : TButton;
    btnSave      : TButton;
    btnSetActive : TButton;
    edtGunName   : TLabeledEdit;
    edtName      : TLabeledEdit;
    GroupBox1    : TGroupBox;
    GroupBox2    : TGroupBox;
    GroupBox3    : TGroupBox;
    Label2       : TLabel;
    Label3       : TLabel;
    lblInfo      : TLabel;
    ListBox1     : TListBox;
    Panel1       : TPanel;
    spnAmmo      : TSpinEdit;
    spnLife      : TSpinEdit;
    btnNewGun: TButton;
    procedure btnAddGunClick    (Sender: TObject);
    procedure btnClearClick     (Sender: TObject);
    procedure btnLoadClick      (Sender: TObject);
    procedure btnNewGunClick    (Sender: TObject);
    procedure btnSaveClick      (Sender: TObject);
    procedure btnSetActiveClick (Sender: TObject);
    procedure FormCreate        (Sender: TObject);
    procedure FormDestroy       (Sender: TObject);
    procedure ListBox1Click     (Sender: TObject);
  private
    Soldier3: TSoldier3;
    procedure ShowGuns;
  public
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightCore.AppData, LightVcl.Visual.AppData;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if AppData.RunningFirstTime then
   begin
    spnLife.Value := 100;
    edtName.Text  := 'Ryan';
   end;

  Soldier3:= TSoldier3.Create;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Soldier3.Free;
end;






function GetBinaryFileName: string;
CONST
  MyFile= 'Soldier.bin';
begin
  Result:= AppData.ExeFolder+ MyFile;  // The files is saved where the EXE file is.
end;


procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  edtName.Text:= '';
  spnLife.Value:= 0;
  edtGunName.Text:= '';
  spnAmmo.Value:= 0;
  Soldier3.Guns.Clear;
  ShowGuns;
end;


procedure TfrmMain.btnAddGunClick(Sender: TObject);
VAR
   Gun: TGun; // Freed by 'Guns'
begin
  Gun:= TGun.Create;
  Gun.Name:= edtGunName.Text;
  Gun.Ammo:= spnAmmo.Value;
  Soldier3.Guns.Add(Gun);
  Soldier3.Guns.ActiveGun:= Soldier3.Guns.Count-1;
  btnSave.Enabled:= TRUE;
  GroupBox2.Visible:= FALSE;

  ShowGuns;
end;


procedure TfrmMain.ShowGuns;
begin
  ListBox1.Clear;
  for VAR i:= 0 to Soldier3.Guns.Count-1 DO
    ListBox1.AddItem((Soldier3.Guns[i] as TGun).Name, Soldier3.Guns[i]);

  if Soldier3.Guns.ActiveGun > -1
  then ListBox1.ItemIndex:= Soldier3.Guns.ActiveGun;
end;


procedure TfrmMain.btnSetActiveClick(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
    begin
      Soldier3.Guns.ActiveGun:= ListBox1.ItemIndex;
      btnSave.Enabled:= TRUE;
    end;
end;


procedure TfrmMain.ListBox1Click(Sender: TObject);
begin
  VAR Gun:= Soldier3.Guns[ListBox1.ItemIndex] as TGun;
  edtGunName.Text:= Gun.Name;
  spnAmmo.Value:= Gun.Ammo;
end;



{-------------------------------------------------------------------------------------------------------------
   LOAD / SAVE
-------------------------------------------------------------------------------------------------------------}
procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  // Read data from GUI
  Soldier3.Life:= spnLife.Value;
  Soldier3.Name:= edtName.Text;

  // Prepare binary file
  VAR Stream:= TLightStream.CreateWrite(GetBinaryFileName);
  TRY
    Soldier3.Save(Stream);  // Write Soldier to binary file
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  btnClearClick(Sender);

  // Prepare binary file
  VAR Stream:= TLightStream.CreateRead(GetBinaryFileName);
  TRY
    Soldier3.Load(Stream); // Read Soldier from binary file
    Caption:= Soldier3.ShowVersion;
    ShowGuns;
  FINALLY
    FreeAndNil(Stream);
  END;

  // Put data back into the GUI
  spnLife.Value := Soldier3.Life;
  edtName.Text  := Soldier3.Name;
end;


procedure TfrmMain.btnNewGunClick(Sender: TObject);
begin
  GroupBox2.Visible:= TRUE;
end;

end.
