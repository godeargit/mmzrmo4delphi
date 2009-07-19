{*******************************************************
        ��Ԫ���ƣ�untRmoDbClient.pas
        �������ڣ�2008-09-16 17:25:52
        ������	  ������
        ����:     Զ�����ݿ�ͻ���
        ��ǰ�汾�� v1.1

��ʷ�ͼƻ�
v1.0  ��Ԫʵ��
v1.1  �����֧���������ֶε�����
v1.2  ���id�ű����ǵ�1���ֶε�����
v1.3  Ϊ�����ٶȣ������岻��ÿ���������

*******************************************************}


unit untRmoDbClient;

interface

uses
  Classes, Controls, ExtCtrls, db, adodb,
  Un_socket_control;

type
  TConnthread = class;
  TRmoClient = class(TSocketClient)
  private
    FSqlPart1, FSqlPart2: string;
    FtableName: string;
    Fsn: Cardinal;
    FQryForID: TADOQuery;
    FIsDisConn: boolean; //�Ƿ����Լ��ֶ��Ͽ����ӵ�
    Ftimer: TTimer; //���ӱ�����
    FisConning: Boolean; //�Ƿ����ӳɹ�
    //��ʱ����Ƿ���Ҫ���� �������ӶϿ�
    procedure OnCheck(Sender: TObject);
     //����Ƿ����Ӵ��
    procedure checkLive;

    procedure OnAfterPost(DataSet: TDataSet);
    procedure OnBeginPost(DataSet: TDataSet);
    procedure OnBeforeDelete(DataSet: TDataSet);
  public

    IsInserIDfield: boolean; //�Ƿ������� ֧��ID�ֶ� �����������������ֶ�Ĭ����false
    IsOPenAutoPost: boolean; //�Ƿ�֧���Զ�Post���� Ĭ����֧��

    //���ӷ����
    function ConnToSvr(ISvrIP: string; ISvrPort: Integer = 9988): boolean;
    //�Ͽ�����
    procedure DisConn;
    //���������µ�IP
    function ReConn(ISvrIP: string; IPort: Integer = -1): boolean;

    //��postģʽ���Ϊ ������䵽Զ��ִ��
    procedure ReadySqls(IAdoquery: TADOQuery);

    //ִ��һ�����
    function ExeSQl(ISql: string): Integer;
    //��һ�������ݼ�
    function OpenAndataSet(ISql: string; IADoquery: TADOQuery): Boolean;

    procedure OnCreate; override;
    procedure OnDestory; override;
  end;


  TConnthread = class(TThread)
  public
    Client: TRmoClient;
    procedure execute; override;
  end;

var
  //Զ�����ӿ��ƶ���
  Gob_RmoCtler: TRmoClient;

implementation

uses untfunctions, sysUtils, pmybasedebug, UntBaseProctol, DXSock;


procedure TRmoClient.checkLive;
begin
  try
    WriteInteger(4);
  except
    if FIsDisConn = False then
      FisConning := False;
  end;
end;

function TRmoClient.ConnToSvr(ISvrIP: string;
  ISvrPort: Integer): boolean;
begin
  Result := True;
  if (IsConnected = false) or (FHost <> ISvrIP) or (FPort <> ISvrPort) then begin
    DisConn;
    if not IsConnected then begin
      try
        Result := Connto(ISvrIP, ISvrPort);
        FIsDisConn := Result;
      except

      end;
      if Result = True then begin
        SendHead(CTSLogin);
        WriteInteger(CClientID);
        if ReadInteger <> STCLogined then
          Result := False;
        FisConning := True;
        FIsDisConn := False;
        Ftimer.Enabled := True;
      end;
    end;
  end;
end;

procedure TRmoClient.DisConn;
begin
  try
    if IsConnected then
      Disconnect;
  except
  end;
  FIsDisConn := True;
end;

{ TConnthread }

procedure TConnthread.execute;
begin
  if Client.ConnToSvr(Client.FHost, Client.FPort) then begin
    Client.FisConning := True;
    Client.Ftimer.Tag := 0;
  end;
end;

function TRmoClient.ExeSQl(ISql: string): Integer;
var
  llen: Integer;
begin
  WriteInteger(1);
  WriteInteger(Length(ISql));
  Write(ISql);
  llen := ReadInteger();
  if llen = -1 then begin
    llen := ReadInteger();
    ISql := ReadStr(llen);
    raise Exception.Create(ISql);
  end
  else begin
    Result := llen;
  end;
end;


//------------------------------------------------------------------------------
// ����postʱ�Զ����·���� 2009-05-22 ������
// Ҫ��������id�Ŷ��ұ����ǵ�һ���ֶ�
//------------------------------------------------------------------------------


var
  lglst: Tstrings;

procedure TRmoClient.OnAfterPost(DataSet: TDataSet);
var
  llen: integer;
  LS: string;
begin
  if TADOQuery(DataSet).Tag = 999 then
    Exit;

  //��Ҫʹ�������ֶ�ȥ�ύ
  LS := GetCurrPath + GetDocTime;
  //�����һ���ֶ�Ϊֻ����˵����������ID�ֶ� �ĵ���
  if DataSet.Fields[0].ReadOnly = true then begin
    IsInserIDfield := True;
    DataSet.Fields[0].ReadOnly := False;
  end;
  if IsInserIDfield then begin
    //�����ҪID�ֶ� �Զ���ȡ
    if FQryForID = nil then
      FQryForID := TADOQuery.Create(nil);
    OpenAndataSet(Format('select max(%s) as myid from %s', [DataSet.Fields[0].FieldName, FtableName]), FQryForID);
    DataSet.Edit;
    DataSet.Fields[0].AsInteger := FQryForID.FieldByName('myid').AsInteger + 1;
  end;

  TADOQuery(DataSet).Tag := 999;
  TADOQuery(DataSet).SaveToFile(LS, pfXML);
  TADOQuery(DataSet).Tag := 0;
  WriteInteger(6);
  WriteInteger(TADOQuery(DataSet).Fields[0].AsInteger);
  SendZipFile(LS);
  DeleteFile(LS);
  llen := ReadInteger();
  if llen = -1 then begin
    llen := ReadInteger();
    LS := ReadStr(llen);
    raise Exception.Create(LS);
  end
end;

procedure TRmoClient.OnBeforeDelete(DataSet: TDataSet);
var
  I: Integer;
  lsql: string;
  Result, ltablename: string;
  Lkey, lvalue: string;
  Lindex: integer;
begin

  //��ȡ����
  lsql := LowerCase(TADOQuery(DataSet).Filter);
  if Pos('select', lsql) > 0 then begin
    if lglst = nil then
      lglst := TStringList.Create;
    GetEveryWord(lsql, lglst, ' ');
    for i := 0 to lglst.Count - 1 do
      if lglst.Strings[i] = 'from' then begin
        Lindex := i;
        Break;
      end;
    if Lindex < 2 then
      ExceptTip('SQL������');
    ltablename := '';
    for i := Lindex + 1 to lglst.Count - 1 do
      if lglst.Strings[i] <> '' then begin
        ltablename := lglst.Strings[i];
        Break;
      end;
    if ltablename = '' then
      ExceptTip('SQL������');
  end
  else
    ExceptTip('�޷��Զ��ύ������ִ��select');
  //��ȡ����
  with DataSet.Fields do begin
    Result := 'delete from ' + ltablename + Format(' where %s=%d', [Fields[0].FieldName, Fields[0].AsInteger]);
    ExeSQl(Result);
  end;
end;


procedure TRmoClient.OnBeginPost(DataSet: TDataSet);
var
  I, n: Integer;
  lsql, lBobName: string;
  Result: string;
  Lkey, lvalue: string;
  Lindex: integer;
  LblobStream: TStream;
begin
  //��ȡ����
  //��ȡ����
  case TADOQuery(DataSet).State of //
    dsinsert: begin
        with DataSet.Fields do begin
        //�����һ���ֶ�Ϊֻ����˵����������ID�ֶ� �ĵ���
          if Fields[0].ReadOnly = true then begin
            IsInserIDfield := True;
            Fields[0].ReadOnly := False;
          end;
          if IsInserIDfield then begin
            n := 0;
             //�����ҪID�ֶ� �Զ���ȡ
            if FQryForID = nil then
              FQryForID := TADOQuery.Create(nil);
            OpenAndataSet(Format('select max(%s) as myid from %s', [Fields[0].FieldName, FtableName]), FQryForID);
            Fields[0].AsInteger := FQryForID.FieldByName('myid').AsInteger + 1;
          end
          else
            n := 1;

          FSqlPart1 := 'insert into ' + FtableName + '(';
          FSqlPart2 := '';
          for i := n to count - 1 do begin
            //�����blob�ֶ�������
            if Fields[i].DataType in [ftBlob] then begin
              LblobStream := TMemoryStream.Create;
              TBlobField(Fields[i]).SaveToStream(LblobStream);
              EnCompressStream(TMemoryStream(LblobStream));
              lBobName := Fields[i].FieldName;
              Continue;
            end;
            if i <> Count - 1 then begin
              FSqlPart1 := FSqlPart1 + Fields[i].FieldName + ',';
              case Fields[i].DataType of
                ftCurrency, ftBCD, ftWord, ftFloat, ftBytes: Result := FSqlPart2 + Fields[i].AsString + ',';
                ftSmallint, ftInteger: FSqlPart2 := FSqlPart2 + IntToStr(Fields[i].AsInteger) + ',';
              else
                FSqlPart2 := FSqlPart2 + '''' + Fields[i].AsString + '''' + ',';
              end;
            end
            else begin
              FSqlPart1 := FSqlPart1 + Fields[i].FieldName + ') values (';
              case Fields[i].DataType of
                ftCurrency, ftBCD, ftWord, ftFloat, ftBytes: Result := FSqlPart2 + Fields[i].AsString + ',';
                ftSmallint, ftInteger: FSqlPart2 := FSqlPart2 + IntToStr(Fields[i].AsInteger) + ',';
              else
                FSqlPart2 := FSqlPart2 + '''' + Fields[i].AsString + '''' + ')';
              end;
            end;
          end;
          Result := FSqlPart1 + FSqlPart2;
        end;
      end;
    dsEdit: begin
        with DataSet.Fields do begin
          Result := 'Update ' + FtableName + ' Set ';
          for I := 0 to count - 1 do begin // Iterate
            if I = 0 then begin
              Lkey := Fields[i].FieldName;
              lvalue := Fields[i].AsString;
              Continue;
            end;
            Result := Result + Fields[i].FieldName + '=';
            case Fields[i].DataType of //
              ftCurrency, ftBCD, ftWord: Result := Result + Fields[i].AsString;
              ftFloat: Result := Result + Fields[i].AsString;
              ftBytes, ftSmallint, ftInteger: Result := Result + IntToStr(Fields[i].AsInteger);
            else
              Result := Result + '''' + Fields[i].AsString + '''';
            end; // case
            if i <> Count - 1 then
              Result := Result + ',';
          end; // for
          Result := Result + Format(' where %s=%s', [Lkey, lvalue]);
        end; // with
      end;
  end; // case
  ExeSQl(Result);
  //�����blob�ֶ��� ׷��д��
  if LblobStream <> nil then begin
    lsql := format('update %s set %s=:%s where %s=%d', [FtableName, lBobName, 'Pbob'
      , DataSet.Fields[0].FieldName, DataSet.Fields[0].AsInteger]);
    WriteInteger(6);
    WriteInteger(length(lsql));
    Write(lsql);
//    WriteStream(LblobStream, True, True);
  end;
end;


procedure TRmoClient.OnCheck(Sender: TObject);
begin
  if TTimer(sender).tag = 0 then begin
    if ((IsConnected = false) or (FisConning = false)) and (FIsDisConn = false) then begin
      TTimer(sender).tag := 1;
      with TConnthread.Create(True) do begin
        FreeOnTerminate := True;
        Client := Self;
        Resume;
      end;
    end
    else begin
      checkLive;
    end;
  end;
end;

procedure TRmoClient.OnCreate;
begin
  inherited;
  IsOPenAutoPost := true;
  Ftimer := TTimer.Create(nil);
  Ftimer.OnTimer := OnCheck;
  Ftimer.Interval := 3000;
  Ftimer.Enabled := False;
  Ftimer.Tag := 0;
  FisConning := false;
  FIsDisConn := False;
end;

procedure TRmoClient.OnDestory;
begin
  inherited;
  if FQryForID <> nil then
    FQryForID.Free;
  Ftimer.Free;
end;

function TRmoClient.OpenAndataSet(ISql: string;
  IADoquery: TADOQuery): Boolean;
var
  llen: Integer;
  ls: string;
  Lend: integer;
begin
  inc(Fsn);
  Lend := 0;
  ls := ISql;
  WriteInteger(2);
  WriteInteger(Length(ISql));
  Write(ISql);
  llen := ReadInteger();
  if llen = -1 then begin
    llen := ReadInteger();
    ISql := ReadStr(llen);
    raise Exception.Create(ISql);
  end
  else begin
    ISql := GetCurrPath + GetDocDate + GetDocTime + IntToStr(Fsn);
    GetZipFile(ISql);
    IADoquery.LoadFromFile(ISql);
    if IsOPenAutoPost then begin
      IADoquery.Filter := (ls);
      ReadySqls(IADoquery);
    end;
    DeleteFile(ISql);
    Result := True;
  end;
end;

procedure TRmoClient.ReadySqls(IAdoquery: TADOQuery);
var
  i, Lindex: integer;
  lsql: string;
begin
  lsql := LowerCase(IAdoquery.Filter);
  if Pos('select', lsql) > 0 then begin
    if lglst = nil then
      lglst := TStringList.Create;
    GetEveryWord(lsql, lglst, ' ');
    for i := 0 to lglst.Count - 1 do
      if lglst.Strings[i] = 'from' then begin
        Lindex := i;
        Break;
      end;
    if Lindex < 2 then
      ExceptTip('SQL������');
    FtableName := '';
    for i := Lindex + 1 to lglst.Count - 1 do
      if lglst.Strings[i] <> '' then begin
        FtableName := lglst.Strings[i];
        Break;
      end;
    if FtableName = '' then
      ExceptTip('SQL������');
  end
  else
    ExceptTip('�޷��Զ��ύ������ִ��select');

  //������κ�һ���ֶ���tblob�ֶ�
  for i := 0 to IAdoquery.Fields.Count - 1 do begin // Iterate
    if IAdoquery.Fields[i].DataType in [ftBlob, ftOraClob] then begin
      IAdoquery.AfterPost := OnAfterPost;
      IAdoquery.BeforeDelete := OnBeforeDelete;
      exit;
    end;
  end; // for

  IAdoquery.BeforePost := OnBeginPost;
  IAdoquery.BeforeDelete := OnBeforeDelete;
end;

function TRmoClient.ReConn(ISvrIP: string; IPort: Integer): boolean;
begin
  Result := False;
  if IsLegalIP(ISvrIP) then begin
    Result := ConnToSvr(ISvrIP, IfThen(IPort = -1, FPort, IPort));
  end;
end;

end.

