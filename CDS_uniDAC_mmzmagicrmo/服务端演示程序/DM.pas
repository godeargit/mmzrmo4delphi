unit DM;

interface

uses
  SysUtils, Classes, DB, MemDS, DBAccess, Uni, Provider,
  SQLServerUniProvider, SQLiteUniProvider, OracleUniProvider,
  MySQLUniProvider, UniProvider, ODBCUniProvider, AccessUniProvider;

type
  TDataModel = class(TDataModule)
    DP: TDataSetProvider;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    Coner: TUniConnection;
    Gqry: TUniQuery;
    UniSQL1: TUniSQL;
  end;

var
  DataModel: TDataModel;

implementation

uses
  untFunctions;

{$R *.dfm}

procedure TDataModel.DataModuleCreate(Sender: TObject);
begin
  coner := TUniConnection.Create(self);
  coner.ProviderName := 'Access';
  coner.Database := GetCurrPath() + 'demo.mdb';
  coner.Connect;
  Gqry := TUniQuery.Create(Self);
  Gqry.Connection := coner;
  UniSQL1 := TUniSQL.Create(Self);
  UniSQL1.Connection := coner;
  DP.DataSet:=Gqry;
end;

end.

