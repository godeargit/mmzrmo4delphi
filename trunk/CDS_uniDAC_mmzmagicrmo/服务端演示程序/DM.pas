unit DM;

interface

uses
  SysUtils, Classes, DB, MemDS, DBAccess, Uni, Provider,
  SQLServerUniProvider, SQLiteUniProvider, OracleUniProvider,
  MySQLUniProvider, UniProvider, ODBCUniProvider, AccessUniProvider;

type
  TDataModel = class(TDataModule)
    coner: TUniConnection;
    Gqry: TUniQuery;
    DP: TDataSetProvider;
    AccessUniProvider1: TAccessUniProvider;
    MySQLUniProvider1: TMySQLUniProvider;
    OracleUniProvider1: TOracleUniProvider;
    SQLiteUniProvider1: TSQLiteUniProvider;
    SQLServerUniProvider1: TSQLServerUniProvider;
    UniSQL1: TUniSQL;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModel: TDataModel;

implementation

uses
  untFunctions;

{$R *.dfm}

procedure TDataModel.DataModuleCreate(Sender: TObject);
begin
  coner.Database := GetCurrPath() + 'demo.mdb';
  coner.Connect;
end;

end.

