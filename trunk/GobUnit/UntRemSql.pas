unit UntRemSql;

interface

uses
  Classes, SysUtils, ADODB, Forms, Windows, DB,
  untRmoDbClient, pmybasedebug;


type
  TRmoHelper = class

  public
    FPublic: tadoquery;
    FLastSql: string; //���һ�β�ѯ�����
    FRmoClient: TRmoClient;

      {��ѯISqlStr��IADOQry�ؼ�}
    procedure MyQuery(IADOQry: TADOQuery; ISqlStr: string);
    {ִ��ISqlStr���}
    procedure MyExec(ISqlStr: string);
    function GetCount(ItabName, IFieldName: string; Ivalue: variant):
      Cardinal;
    function OpenDataset(ISql: string; const Args: array of const): TADOQuery; overload;
    function OpenDataset(ISql: string): TADOQuery; overload;
    function OpenDataset(Iado: TADOQuery; ISql: string): TADOQuery; overload;
    function OpenDataset(Iado: TADOQuery; ISql: string; const Args: array of const):
      TADOQuery; overload;

    function ExecAnSql(Iado: TADoquery; Isql: string; const Args: array of const): Integer; overload;
    function ExecAnSql(Isql: string; const Args: array of const): Integer; overload;

    function OpenTable(ItabName: string; Iado: TADOQuery): TADOQuery;


   // function OpenTable(ItabName: string; Iado: TADOQuery): TADOQuery; overload;
    //function OpenTable(ItabName: string; IQueryRight: integer = 1): TADOQuery; overload;
    function ConnetToSvr(ISvrIP: string; ISvrPort: Word): Boolean;
    //�������ӷ�����
    function ReConnSvr(ISvrIP: string; ISvrPort: Integer = -1): boolean;
    constructor Create(Iport: integer = 9989);
    destructor Destroy; override;
  end;




var
  Gob_Rmo: TRmoHelper;

implementation



procedure TRmoHelper.MyQuery(IADOQry: TADOQuery;
  ISqlStr: string);
begin
  FRmoClient.OpenAndataSet(ISqlStr, IADOQry);
end;

procedure TRmoHelper.MyExec(ISqlStr: string);
begin
//  try
  FRmoClient.ExeSQl(ISqlStr);
//  except
//    on e: Exception do
//      Application.MessageBox(PChar('ROִ��SQL�쳣��' + e.message), '��ʾ', MB_OK +
//        MB_ICONINFORMATION);
//  end;
end;

constructor TRmoHelper.Create(Iport: integer = 9989);
begin
//  FChannel := tROIndyTCPChannel.Create(nil); // ����Host �� Port
//  FChannel.Port := 8090;
//  FChannel.Host := '127.0.0.1';
//  FMessage := tROBinMessage.Create(nil);
//  FServer := tRORemoteService.Create(nil);
//  FServer.Channel := FChannel;
//  FServer.Message := FMessage;
//  FServer.ServiceName := 'OracleAccessService';

  FPublic := TADOQuery.Create(nil);
  FRmoClient := TRmoClient.Create;
  FRmoClient.FHost := '127.0.0.1';
  FRmoClient.FPort := Iport;

end;

destructor TRmoHelper.Destroy;
begin
  FRmoClient.Free;
//  FPublic.Free;
//  FChannel.Free;
//  FMessage.Free;
//  FServer.Free;
  inherited;
end;



function TRmoHelper.OpenDataset(ISql: string;
  const Args: array of const): TADOQuery;
begin
  ISql := Format(Isql, Args);
  MyQuery(FPublic, ISql);
  Result := FPublic;
end;


function TRmoHelper.OpenDataset(ISql: string): TADOQuery;
begin
  MyQuery(FPublic, ISql);
  Result := FPublic;
end;

function TRmoHelper.OpenDataset(Iado: TADOQuery; ISql: string): TADOQuery;
begin
  MyQuery(Iado, ISql);
  Result := Iado
end;

function TRmoHelper.OpenDataset(Iado: TADOQuery; ISql: string; const Args: array of const):
  TADOQuery;
begin
  ISql := Format(Isql, Args);
  MyQuery(Iado, ISql);
  Result := Iado
end;

function TRmoHelper.GetCount(ItabName, IFieldName: string;
  Ivalue: variant): Cardinal;
var
  sql: string;
begin
  sql := 'select Count(' + IFieldName + ') as MyCount from ' + ItabName + ' where ' + IFieldName + ' = ' + Ivalue + '';
  MyQuery(FPublic, sql);
  Result := FPublic.fieldbyname('MyCount').AsInteger;
end;








function TRmoHelper.ExecAnSql(Iado: TADoquery; Isql: string;
  const Args: array of const): Integer;
begin
  MyExec(Format(Isql, Args));
end;

function TRmoHelper.ExecAnSql(Isql: string;
  const Args: array of const): Integer;
begin
  MyExec(Format(Isql, Args));
end;

function TRmoHelper.ConnetToSvr(ISvrIP: string; ISvrPort: Word): Boolean;
var
  LSql: string;
begin
  try
    Result := FRmoClient.ConnToSvr(ISvrIP, ISvrPort);
  except
    Result := False;
  end;
end;

function TRmoHelper.OpenTable(ItabName: string;
  Iado: TADOQuery): TADOQuery;
begin
  MyQuery(Iado, Format('Select * from %s ', [ItabName]));
  Result := Iado;
end;

function TRmoHelper.ReConnSvr(ISvrIP: string; ISvrPort: Integer): boolean;
begin
  Result := FRmoClient.ReConn(ISvrIP, ISvrPort);
end;

initialization


finalization
  if Assigned(Gob_Rmo) then
    Gob_Rmo.Free;

end.
