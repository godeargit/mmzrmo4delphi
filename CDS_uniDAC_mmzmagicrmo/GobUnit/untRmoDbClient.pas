{*******************************************************
        单元名称：untRmoDbClient.pas
        创建日期：2008-09-16 17:25:52
        创建者	  马敏钊
        功能:     远程数据库客户端
        当前版本： v1.7

更新历史
v1.0  单元实现
v1.1  解决不支持自增长字段的问题
v1.2  解决id号必须是第1个字段的问题
v1.3  为增加速度，做缓冲不用每次生成语句 ，改变自动更新时导致filter属性暂用的方式
v1.4  在sabason 兄的热心帮助下，解决了流试传输存在的问题，大大提高了传输效率 20100413
v1.5  全面修改为支持高效率的UniDAC数据库驱动套件 和ClientDataset (原来是ADO方式)支持所有主流数据库，大幅提高传输效率，且使用方法没有改变
v1.6  解决流传输存在的BUG  ，修正最后一个字段为blob字段导致语句生成错误的BUG
v1.7  增加服务端sys.ini文件配置客户端登陆权限，增加批量执行SQL语句接口

*******************************************************}


unit untRmoDbClient;

interface

uses
  Classes, UntsocketDxBaseClient, IdComponent, Controls, ExtCtrls, db, dbclient;

type
  TConnthread = class;
  TSelectitems = class
  public
    Sql: string;
  end;
  TRmoClient = class(TSocketClient)
  private
    gLmemStream: TMemoryStream;
    FCachSQllst, FsqlLst: TStrings; //用来记录已经打开了的数据集 以及对于的语句
    FSqlPart1, FSqlPart2: string;

    Fsn: Cardinal;
    FQryForID: TClientDataSet;
    FIsDisConn: boolean; //是否是自己手动断开连接的
    Ftimer: TTimer; //连接保活器
    FisConning: Boolean; //是否连接成功
    //定时检查是否需要重连 或者连接断开
    procedure OnCheck(Sender: TObject);
     //检查是否连接存活
    procedure checkLive;

    procedure OnBeginPost(DataSet: TDataSet);
    procedure OnBeforeDelete(DataSet: TDataSet);
  public

    IsInserIDfield: boolean; //是否插入语句 支持ID字段 自增长不允许插入该字段默认是false

    //连接服务端
    function ConnToSvr(ISvrIP: string; ISvrPort: Integer = 9988; Iacc: string = '';
      iPsd: string = ''): boolean;
    //断开连接
    procedure DisConn;
    //重新连接新的IP
    function ReConn(ISvrIP: string; IPort: Integer = -1; Iacc: string = '';
      iPsd: string = ''): boolean;

    //将post模式变更为 更新语句到远端执行
    procedure ReadySqls(IAdoquery: TClientDataSet);

    //执行一条语句
    function ExeSQl(ISql: string): Integer;
    //打开一个过数据集
    function OpenAndataSet(ISql: string; IADoquery: TClientDataSet): Boolean;
    //批量提交语句  立即执行所传入的语句列表
    function BathExecSqls(IsqlList: TStrings): Integer;

    //检查升级
    procedure CheckUpdate;

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
  GCurrVer: integer = 1; //当前程序升级版本号

implementation

uses untfunctions, sysUtils, UntBaseProctol, DXSock, IniFiles, ADOInt, Variants,
  Windows;


function TRmoClient.BathExecSqls(IsqlList: TStrings): Integer;
var
  IErr: string;
  llen, i: Integer;
  ls: TMemoryStream;
begin
  //批量执行SQL语句
  WriteInteger(110);
  ls := TMemoryStream.Create;
  IsqlList.SaveToStream(ls);
  SendZIpStream(ls, Self);
  llen := ReadInteger();
  if llen = -1 then begin
    llen := ReadInteger();
    IErr := ReadStr(llen);
//    IsqlList.SaveToFile('D:\2.txt');
    raise Exception.Create(IErr);
  end
  else begin
    Result := llen;
  end;
end;

procedure TRmoClient.checkLive;
begin
  try
    if IsConnected then begin
      if WriteInteger(4) <> 4 then begin
        if FIsDisConn = False then
          FisConning := False;
      end;
    end
    else begin
      if FIsDisConn = False then
        FisConning := False;
    end;

  except
    if FIsDisConn = False then
      FisConning := False;
  end;
end;

procedure TRmoClient.CheckUpdate;
var
  i: Integer;
  li, lr, lm: integer;
  ls, lspath, lflst: string;
  lspit: TStringList;
begin
  WriteInteger(9998);
  lr := ReadInteger;
  if lr > 0 then begin
    lspit := TStringList.Create;
    lr := ReadInteger; //ver
    lm := ReadInteger;
    lspath := ReadLn(10000);
    ls := ReadLn(10000);
    li := ReadInteger;
    lflst := ReadStr(li);
    if lr > GCurrVer then begin
      lspit.Add(IntToStr(lm));
      lspit.Add(ls);
      //后台下载下来
      GetEveryWord(lflst, '|');
      lspit.Add(IntToStr(GlGetEveryWord.Count));

      for i := 0 to GlGetEveryWord.Count - 1 do begin // Iterate
        SendHead(9997);
        WriteLn(GlGetEveryWord[i]);
        li := ReadInteger;
        if li = 1 then begin
          ls := StringReplace(GlGetEveryWord[i], lspath, '', []);
          ls := GetCurrPath + ls;
          ForceDirectories(ExtractFilePath(ls));
          GetZipFile(ls);
          lspit.Add(ls);
        end;
      end; // for
      lspit.SaveToFile('up.cfg');
      lspit.Free;
      WinExec(pchar('up.exe  ' + ExtractFileName(ParamStr(0))), SW_SHOW);
    end;
  end;
end;

function TRmoClient.ConnToSvr(ISvrIP: string; ISvrPort: Integer = 9988;
  Iacc: string = ''; iPsd: string = ''): boolean;
begin
  Result := True;
  if (IsConnected = false) or (FHost <> ISvrIP) or (FPort <> ISvrPort) then begin
    DisConn;
    FHost := ISvrIP;
    FPort := ISvrPort;
    Facc := Iacc;
    iPsd := iPsd;
    FIsDisConn := False;
    if not IsConnected then begin
      try
        Result := Connto(FHost, FPort);
      except
        Result := False;
        FIsDisConn := False;
      end;
      if Result = True then begin
        SendHead(CTSLogin);
        WriteInteger(CClientID);
        if ReadInteger <> STCLogined then
          Result := False;
        WriteLn(format('%s|%s', [Iacc, Str_Encry(iPsd, 'rmo')]));
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
  try
    if Client.ConnToSvr(Client.FHost, Client.FPort, Client.Facc, Client.Fpsd) then begin
      Client.FisConning := True;
    end;
  finally
    Client.Ftimer.Tag := 0;
  end;
end;

function TRmoClient.ExeSQl(ISql: string): Integer;
var
  llen, i: Integer;
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

procedure TRmoClient.OnBeforeDelete(DataSet: TDataSet);
var
  I: Integer;
  lsql: string;
  Result, ltablename: string;
  Lkey, lvalue: string;
  Lindex: integer;
begin

  //获取表名
  i := FsqlLst.IndexOf(IntToStr(integer(DataSet)));
  if i > -1 then
    lsql := LowerCase(TSelectitems(FsqlLst.Objects[i]).Sql); //  LowerCase(DataSet.Filter);
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
  Result, FtableName: string;
  Lkey, lvalue: string;
  Lindex: integer;
  LblobStream: TStream;
begin
  //获取表名
  i := FsqlLst.IndexOf(IntToStr(integer(DataSet)));
  if i > -1 then
    lsql := LowerCase(TSelectitems(FsqlLst.Objects[i]).Sql); //  LowerCase(DataSet.Filter);
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

  //获取方法
  case TClientDataSet(DataSet).State of //
    dsinsert: begin
        with DataSet.Fields do begin
        //如果第一个字段为只读，说明是自增长ID字段 改掉它
          if Fields[0].ReadOnly = true then begin
            IsInserIDfield := True;
            Fields[0].ReadOnly := False;
          end;
          if IsInserIDfield then begin
            n := 1;
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
//------------------------------------------------------------------------------
// 如果是最后一个字段则跳过之前去掉上次生成的，号  2010-04-21 马敏钊
//------------------------------------------------------------------------------
              if i = count - 1 then begin
                if FSqlPart1[length(FSqlPart1) - 1] = ',' then begin
                  FSqlPart1 := copy(FSqlPart1, 1, length(FSqlPart1) - 1);
                  FSqlPart2 := copy(FSqlPart2, 1, length(FSqlPart2) - 1);
                end;
              end;
              Continue;
            end;

            FSqlPart1 := FSqlPart1 + ifthen(i = n, '', ',') + Fields[i].FieldName;
            case Fields[i].DataType of
              ftCurrency, ftBCD, ftWord, ftFloat, ftBytes: FSqlPart2 := FSqlPart2 + ifthen(i = n, '', ',') + ifthen(Fields[i].AsString = '', '0', Fields[i].AsString);
              ftBoolean, ftSmallint, ftInteger: FSqlPart2 := FSqlPart2 + ifthen(i = n, '', ',') + IntToStr(Fields[i].AsInteger);
            else
              FSqlPart2 := FSqlPart2 + ifthen(i = n, '', ',') + '''' + Fields[i].AsString + '''';
            end;
          end;
          Result := FSqlPart1 + ') values (' + FSqlPart2 + ')';
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
             //如果有blob字段则跳过
            if Fields[i].DataType in [ftBlob] then begin
              LblobStream := TMemoryStream.Create;
              TBlobField(Fields[i]).SaveToStream(LblobStream);
              EnCompressStream(TMemoryStream(LblobStream));
              lBobName := Fields[i].FieldName;
//------------------------------------------------------------------------------
// 如果是最后一个字段则跳过之前去掉上次生成的，号  2010-04-21 马敏钊
//------------------------------------------------------------------------------
              if i = count - 1 then begin
                Result := copy(Result, 1, length(Result) - 1);
              end;
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
  if DataSet.State = dsInsert then begin
  //如果需要ID字段 自动获取
    if FQryForID = nil then
      FQryForID := TClientDataSet.Create(nil);
    OpenAndataSet(Format('select max(%s) as myid from %s', [DataSet.Fields[0].FieldName, FtableName]), FQryForID);
    DataSet.Fields[0].AsInteger := FQryForID.FieldByName('myid').AsInteger;
  end;
  //如果有blob字段则 追加写入
  if LblobStream <> nil then begin
    lsql := format('update %s set %s=:%s where %s=%d', [FtableName, lBobName, 'Pbob'
      , DataSet.Fields[0].FieldName, DataSet.Fields[0].AsInteger]);
    WriteInteger(6);
    WriteInteger(length(lsql));
    Write(lsql);
    WriteStream(LblobStream);
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
  FCachSQllst := THashedStringList.Create;
  Ftimer := TTimer.Create(nil);
  Ftimer.OnTimer := OnCheck;
  Ftimer.Interval := 3000;
  Ftimer.Enabled := False;
  Ftimer.Tag := 0;
  FisConning := false;
  FIsDisConn := False;
  FsqlLst := THashedStringList.Create;
  gLmemStream := TMemoryStream.Create;
end;

procedure TRmoClient.OnDestory;
begin
  inherited;
  FCachSQllst.Free;
  if FQryForID <> nil then
    FQryForID.Free;
  Ftimer.Free;
  FsqlLst.Free;
  gLmemStream.Free;
end;


function StreamToVarArray(const S: TStream): Variant;
var P: Pointer;
  C: Integer;
  L: Integer;
begin
  S.Position := 0;
  C := S.Size;
  Result := VarArrayCreate([1, C], varByte);
  L := Length(Result);
  if L <> 0 then
    ;
  P := VarArrayLock(Result);
  try
    S.Read(P^, C);
  finally
    VarArrayUnlock(Result);
  end;
end;

procedure VarArrayToStream(const V: Variant; S: TStream);
var P: Pointer;
  C: Integer;
begin
  if not VarIsArray(V) then
    raise Exception.Create('Var is not array');
  if VarType(V[1]) <> varByte then
    raise Exception.Create('Var array is not blob array');
  C := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
  if not (C > 0) then
    Exit;

  P := VarArrayLock(V);
  try
    S.Write(P^, C * SizeOf(Byte));
    S.Position := 0;
  finally
    VarArrayUnLock(V);
  end;
end;




function DatasetFromStream(Idataset: TClientDataSet; Stream:
  TMemoryStream): boolean;
var
  RS: Variant;
begin
  Result := false;
  if Stream.Size < 1 then
    Exit;
  try
    Idataset.Data := StreamToVarArray(Stream);
    Result := true;
  finally;
  end;
end;

function TRmoClient.OpenAndataSet(ISql: string;
  IADoquery: TClientDataSet): Boolean;
var
  llen, i: Integer;
  ls: string;
  Lend: integer;
  Litem: TSelectitems;
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
    //记录着 是否可以自动保存
    i := FsqlLst.IndexOf(IntToStr(integer(IADoquery)));
    if i = -1 then begin
      Litem := TSelectitems.Create;
      FsqlLst.AddObject(IntToStr(integer(IADoquery)), Litem);
    end
    else
      Litem := TSelectitems(FsqlLst.Objects[i]);
    Litem.Sql := ISql;
     //记录一下
    ReadySqls(IADoquery);
    if llen = 1 then begin
      if gLmemStream = nil then
        gLmemStream := TMemoryStream.Create;
      GetZipStream(gLmemStream, self);
      DatasetFromStream(IADoquery, gLmemStream);
    end
    else begin
      ISql := GetCurrPath + GetDocDate + GetDocTime + IntToStr(Fsn);
      GetZipFile(ISql);
      IADoquery.LoadFromFile(ISql);
      DeleteFile(pchar(ISql));
    end;
    Result := True;
  end;
end;

procedure TRmoClient.ReadySqls(IAdoquery: TClientDataSet);
begin
  IAdoquery.BeforePost := OnBeginPost;
  IAdoquery.BeforeDelete := OnBeforeDelete;
end;

function TRmoClient.ReConn(ISvrIP: string; IPort: Integer = -1; Iacc: string = '';
  iPsd: string = ''): boolean;
begin
  Result := False;
  if IsLegalIP(ISvrIP) then begin
    Result := ConnToSvr(ISvrIP, IfThen(IPort = -1, FPort, IPort), iacc, ipsd);
  end;
end;

end.

