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

//�ڴ˿������ݿ�����ѡ��
{$DEFINE  Access}
{.$DEFINE  InterBase}

procedure TDataModel.DataModuleCreate(Sender: TObject);
begin
  coner := TUniConnection.Create(self);
//------------------------------------------------------------------------------
// �ڴ˴����Ը�����Ҫ���Ӳ�ͬ�����ݿ��Լ����벻ͬ���Ӳ��� 2010-04-23 ������
//------------------------------------------------------------------------------
  with Coner do begin
//����interbase
{$IFDEF InterBase}
    ProviderName := 'InterBase'; //ΪInterBase,֧��InterBase��FireBird
    UserName := 'SYSDBA'; //���ݿ�����
    Password := 'masterkey'; //���ݿ�����
    SpecificOptions.Clear;
{$IFDEF EMBED} //�����ļ���ʽ��
    Server := ''; //Ƕ��ʽΪ��
    DataBase := GetCurrPath() + 'demo.fdb';
    SpecificOptions.Add('InterBase.ClientLibrary=fbembed.dll'); //����embeddll��dll�ļ�λ��
{$ELSE} // ���ӷ�����ʽ��
    Server := '192.168.1.88';
    Port := 3050; //ȷ������������Firebird��3050�˿�
    Database := 'UniDemoDB'; //CS������ʹ�������ݿ����
    SpecificOptions.Add('InterBase.ClientLibrary=gds32.dll');
{$ENDIF}
    SpecificOptions.Add('InterBase.CharLength=0'); //����Ϊ0���Զ���ȡFireBird����
    SpecificOptions.Add('SQLDialet=3'); //����Ϊ3
    SpecificOptions.Add('CharSet=GBK'); //����ΪGBK
{$ENDIF}
//����Access
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

