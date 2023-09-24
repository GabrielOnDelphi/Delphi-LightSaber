UNIT MainForm;

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

TYPE
  TForm6 = class(TForm)
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

VAR
  Form6: TForm6;

IMPLEMENTATION {$R *.dfm}
USES ccIO;


procedure TForm6.FormCreate(Sender: TObject);
begin
 Memo.Clear;

 // Test "normal"
 Memo.Lines.Add('ForceDirectoriesB(''C:\1'')');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('C:\1'), TRUE));

 // Create an existing folder
 Memo.Lines.Add('Repeat');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('C:\1'), TRUE));
 Memo.Lines.Add('');

 // Test create on readonly folder (please prepare the folder first)
 Memo.Lines.Add('Readonly folder: ForceDirectoriesB(''C:\ReadOnly\1'')');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('C:\ReadOnly\1'), TRUE));  // Returns true
 Memo.Lines.Add('');

 // Test create on readonly drive (please enter here a CD drive or locked USB stick)
 Memo.Lines.Add('Readonly drive: ForceDirectoriesB(''D:\1'')');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('D:\1'), TRUE));  // Returns false
 Memo.Lines.Add('');

 // Test invalid path
 Memo.Lines.Add('ForceDirectoriesB(''C:\?'')');
 Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('C:\?'), TRUE));  // Returns false
 Memo.Lines.Add('');

 // Test invalid parameters
 // This will raise an exception
 Memo.Lines.Add('ForceDirectoriesB('''')');
 TRY
   Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB(''), TRUE));
 EXCEPT
   on E: Exception DO
    Memo.Lines.Add('Exception: '+ E.Message);
 END;
 Memo.Lines.Add('');

 // Test invalid drive
 // This will raise an exception
 Memo.Lines.Add('ForceDirectoriesB(''W:\1'')');
 TRY
    Memo.Lines.Add(' '+ BoolToStr(ForceDirectoriesB('W:\1'), TRUE));
 EXCEPT
   on E: Exception DO
    Memo.Lines.Add('Exception: '+ E.Message);
 END;
 Memo.Lines.Add('');
end;

end.
