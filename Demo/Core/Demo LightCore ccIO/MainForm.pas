UNIT MainForm;

INTERFACE

USES
  //Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  LightVcl.Common.AppDataForm;

TYPE
  TfrmTestIO = class(TLightForm)
    Memo: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

VAR
  frmTestIO: TfrmTestIO;

IMPLEMENTATION {$R *.dfm}
USES LightCore.IO, LightCore.TextFile, LightVcl.Common.IO;


procedure TfrmTestIO.Button1Click(Sender: TObject);
begin
 Memo.Clear;
 Memo.Lines.Add('Some functions will fail. This  is normal!');


 // Test "normal"
 Memo.Lines.Add('ForceDirectoriesB(''C:\1'') = '+ BoolToStr(ForceDirectoriesB('C:\1'), TRUE));


 // Create an existing folder
 Memo.Lines.Add('Recreate same folder = '+ BoolToStr(ForceDirectoriesB('C:\1'), TRUE));
 Memo.Lines.Add('');


 // Test create a readonly folder (please prepare the folder first)
 Memo.Lines.Add('Readonly folder: ForceDirectoriesB(''C:\ReadOnly\1'') = '+ BoolToStr(ForceDirectoriesB('C:\ReadOnly\1'), TRUE));  // Returns true
 Memo.Lines.Add('');


 // Test create on readonly drive (please enter here a CD drive or locked USB stick)
 Memo.Lines.Add('Readonly drive: ForceDirectoriesB(''i:\1'') = '+ BoolToStr(ForceDirectoriesB('i:\1'), TRUE));  // Returns false
 Memo.Lines.Add('');


 // Test invalid path
 // This should crash with "Exception: Invalid characters in path"
 Memo.Lines.Add('ForceDirectoriesB(''C:\?'')');
 TRY
   Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('C:\?'), TRUE));
 EXCEPT
   on E: Exception DO
    Memo.Lines.Add('Exception: '+ E.Message);
 END;
 Memo.Lines.Add('');


 // Test invalid parameters
 // This should crash with "Exception: Path is empty"
 Memo.Lines.Add('ForceDirectoriesB('''')');
 TRY
   Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB(''), TRUE));
 EXCEPT
   on E: Exception DO
    Memo.Lines.Add('Exception: '+ E.Message);
 END;
 Memo.Lines.Add('');


 // Test invalid drive
 // This will not raise an exception
 Memo.Lines.Add('ForceDirectoriesB(''W:\1'')');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('W:\1'), TRUE));
 Memo.Lines.Add('');
end;


end.
