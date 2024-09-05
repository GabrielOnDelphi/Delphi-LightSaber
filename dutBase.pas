UNIT dutBase;

{ Base class.
  Provides support for checking PAS files for invalid code.
  The actual code is implemented in: dutWin64, dutUpgradeCode, dutCodeUtils }

INTERFACE

USES
  System.SysUtils, cmSearchResult;

TYPE
  TDUTBase= class(TObject)
   private
   public
    BackupFile: Boolean;                            { If true, create a backup file IF the file is changed (in case of 'replace') }
    SearchResults: TSearchResults;                  { A list of results where the issue (invalid code) was found }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure NewFile(const aFileName: String);
  end;


IMPLEMENTATION


constructor TDUTBase.Create;
begin
  inherited Create;
  BackupFile:= true;
  SearchResults:= TSearchResults.Create(True);
end;


destructor TDUTBase.Destroy;
begin
  FreeAndNil(SearchResults);
  inherited Destroy;
end;


{ Add this file to the Search Results so we can add info about it }
procedure TDUTBase.NewFile(const aFileName: String);
begin
  SearchResults.Add(TSearchResult.Create(aFileName));
end;


end.
