{*******************************************************
        单元名称：untRmoDbClient.pas
        创建日期：2008-09-16 17:25:52
        创建者	  马敏钊
        功能:     远程数据库客户端
        当前版本： v1.1

历史和计划
v1.0  单元实现
v1.1  解决不支持自增长字段的问题
v1.2  解决id号必须是第1个字段的问题
v1.3  为增加速度，做缓冲不用每次生成语句

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
    FIsDisConn: boolean; //是否是自己手动断开连接的
    Ftimer: TTimer; //连接保活器
    FisConning: Boolean; //是否连接成功
    //定时检查是否需要重连 或者连接断开
    procedure OnCheck(Sender: TObject);
     //检查是否连接存活
    procedure checkLive;

    procedure OnAfterPost(DataSet: TDataSet);
    procedure OnBeginPost(DataSet: TDataSet);
    procedure OnBeforeDelete(DataSet: TDataSet);
  public

    IsInserIDfield: boolean; //是否插入语句 支持ID字段 自增长不允许插入该字段默认是false
    IsOPenAutoPost: boolean; //是否支持自动Post数据 默认是支持

    //连接服务端
    function ConnToSvr(ISvrIP: string; ISvrPort: Integer = 9988): boolean;
    //断开连接
    procedure DisConn;
    //重新连接新的IP
    function ReConn(ISvrIP: string; IPort: Integer = -1): boolean;

    //将post模式变更为 更新语句到远端执行
    procedure ReadySqls(IAdoquery: TADOQuery);

    //执行一条语句
    function ExeSQl(ISql: string): Integer;
    //打开一个过数据集
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
  //远程连接控制对象
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
// 数据post时自动更新服务端 2009-05-22 马敏钊
// 要求表必须有id号而且必须是第一个字段
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

  //需要使用特殊手段去提交
  LS := GetCurrPath + GetDocTime;
  //如果第一个字段为只读，说明是自增长ID字段 改掉它
  if DataSet.Fields[0].ReadOnly = true then begin
    IsInserIDfield := True;
    DataSet.Fields[0].ReadOnly := False;
  end;
  if IsInserIDfield then begin
    //如果需要ID字段 自动获取
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

  //获取表名
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
      ExceptTip('SQL语句错误！');
    ltablename := '';
    for i := Lindex + 1 to lglst.Count - 1 do
      if lglst.Strings[i] <> '' then begin
        ltablename := lglst.Strings[i];
        Break;
      end;
    if ltablename = '' then
      ExceptTip('SQL语句错误！');
  end
  else
    ExceptTip('无法自动提交，请先执行select');
  //获取方法
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
  //获取表名
  //获取方法
  case TADOQuery(DataSet).State of //
    dsinsert: begin
        with DataSet.Fields do begin
        //如果第一个字段为只读，说明是自增长ID字段 改掉它
          if Fields[0].ReadOnly = true then begin
            IsInserIDfield := True;
            Fields[0].ReadOnly := False;
          end;
          if IsInserIDfield then begin
            n := 0;
             //如果需要ID字段 自动获取
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
            //如果有blob字段则跳过
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
  //如果有blob字段则 追加写入
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
      ExceptTip('SQL语句错误！');
    FtableName := '';
    for i := Lindex + 1 to lglst.Count - 1 do
      if lglst.Strings[i] <> '' then begin
        FtableName := lglst.Strings[i];
        Break;
      end;
    if FtableName = '' then
      ExceptTip('SQL语句错误！');
  end
  else
    ExceptTip('无法自动提交，请先执行select');

  //如果有任何一个字段是tblob字段
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

