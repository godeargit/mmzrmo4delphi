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

//在此开启数据库连接选项
{$DEFINE  Access}
{.$DEFINE  InterBase}

procedure TDataModel.DataModuleCreate(Sender: TObject);
begin
  coner := TUniConnection.Create(self);
//------------------------------------------------------------------------------
// 在此处可以根据需要连接不同的数据库以及填入不同连接参数 2010-04-23 马敏钊
//------------------------------------------------------------------------------
  with Coner do begin
//连接interbase
{$IFDEF InterBase}
    ProviderName := 'InterBase'; //为InterBase,支持InterBase和FireBird
    UserName := 'SYSDBA'; //数据库密码
    Password := 'masterkey'; //数据库密码
    SpecificOptions.Clear;
{$IFDEF EMBED} //连接文件形式的
    Server := ''; //嵌入式为空
    DataBase := GetCurrPath() + 'demo.fdb';
    SpecificOptions.Add('InterBase.ClientLibrary=fbembed.dll'); //设置embeddll的dll文件位置
{$ELSE} // 连接服务形式的
    Server := '192.168.1.88';
    Port := 3050; //确保服务器开放Firebird的3050端口
    Database := 'UniDemoDB'; //CS服务器使用了数据库别名
    SpecificOptions.Add('InterBase.ClientLibrary=gds32.dll');
{$ENDIF}
    SpecificOptions.Add('InterBase.CharLength=0'); //设置为0，自动读取FireBird设置
    SpecificOptions.Add('SQLDialet=3'); //设置为3
    SpecificOptions.Add('CharSet=GBK'); //设置为GBK
{$ENDIF}
//连接Access
{$IFDEF Access}
    coner.ProviderName := 'Access';
    coner.Database := GetCurrPath() + 'demo.mdb';
{$ENDIF}
  end;



  coner.Connect;
  Gqry := TUniQuery.Create(Self);
  Gqry.Connection := coner;
  UniSQL1 := TUniSQL.Create(Self);
  UniSQL1.Connection := coner;
  DP.DataSet := Gqry;
end;

end.

