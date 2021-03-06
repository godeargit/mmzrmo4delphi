{*******************************************************
        单元名称：UntRmodbSvr.pas
        创建日期：2008-09-16 17:26:15
        创建者	  马敏钊
        功能:     远程数据库服务端

*******************************************************}

unit UntRmodbSvr;


interface

uses Classes, UntSocketServer, DXServerCore, UntTBaseSocketServer, untFunctions, syncobjs, Windows, Forms,
  DBClient;


type
  TRmodbSvr = class(TCenterServer)
  private
    Flock: TCriticalSection;
    function ReadStream(Istream: TStream; ClientThread: TDXClientThread):
      TMemoryStream;
  public
    GGDBPath: string;
    gLastCpTime: Cardinal;
    gLmemStream: TMemoryStream;
    glBatchLst: TStrings;
    //连接到数据库
    function ConnToDb(IConnStr: string): boolean;
    function OnCheckLogin(ClientThread: TDXClientThread): boolean; override;
    function OnDataCase(ClientThread: TDXClientThread; Ihead: integer): Boolean; override;
    procedure OnCreate(ISocket: TBaseSocketServer); override;
    procedure OnDestroy; override;
    function GetCurrDBPath(InPath: string): string; //zbt 得到当前路径
    function DatasetFromStream(Idataset: TClientDataSet; Stream: TMemoryStream): boolean;
    function DatasetToStream(iRecordset: TClientDataSet; Stream: TMemoryStream): boolean;
  end;

var
  Gob_RmoDBsvr: TRmodbSvr;

implementation

uses sysUtils, pmybasedebug, DXSock, db, DM, Variants, UntCFGer, IniFiles;

{ TRmoSvr }

function TRmodbSvr.DatasetFromStream(Idataset: TClientDataSet; Stream:
  TMemoryStream): boolean;
var
  RS: Variant;
begin
  Result := false;
  if Stream.Size < 1 then
    Exit;
  try
    Stream.Position := 0;
//    RS := Idataset.Recordset;
    Rs.Open(TStreamAdapter.Create(Stream) as IUnknown);
    Result := true;
  finally;
  end;
end;

function TRmodbSvr.DatasetToStream(iRecordset: TClientDataSet; Stream:
  TMemoryStream): boolean;
const
  adPersistADTG = $00000000;
var
  RS: Variant;
begin
  Result := false;
  if iRecordset = nil then
    Exit;
  try
//    RS := iRecordset.Recordset;
    RS.Save(TStreamAdapter.Create(stream) as IUnknown, adPersistADTG);
    Stream.Position := 0;
    Result := true;
  finally;
  end;
end;

function TRmodbSvr.ConnToDb(IConnStr: string): boolean;
begin

  Result := True;
  if Shower <> nil then begin
    Shower.AddShow('连接数据库功<%s>', [IConnStr]);
  end;
end;

procedure TRmodbSvr.OnCreate(ISocket: TBaseSocketServer);
begin
  inherited;
  Flock := TCriticalSection.Create;
  gLmemStream := TMemoryStream.Create;
  gLastCpTime := 0;
  glBatchLst := TStringList.Create;
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

var
  lini: TIniFile;

procedure MGetFileListToStr(var Resp: string; ISpit: string; iFilter, iPath:
  string; ContainSubDir: Boolean = True; INeedPath: boolean = True);
var
  FSearchRec, DSearchRec: TSearchRec;
  FindResult: Cardinal;
begin
  FindResult := FindFirst(iPath + iFilter, sysutils.faAnyFile, FSearchRec);
  try
    while FindResult = 0 do begin
      if ((FSearchRec.Attr and faDirectory) = faDirectory) or (FSearchRec.Name = '.') or (FSearchRec.Name = '..') or (ExtractFileExt(FSearchRec.Name) = '.lnk') then begin
        FindResult := FindNext(FSearchRec);
        Continue;
      end;
      if INeedPath then
        Resp := Resp + (iPath + FSearchRec.Name)
      else
        Resp := Resp + FSearchRec.Name;
      Resp := Resp + ISpit;
      FindResult := FindNext(FSearchRec);
    end;
    if ContainSubDir then begin
      FindResult := FindFirst(iPath + iFilter, faDirectory, DSearchRec);
      while FindResult = 0 do begin
        if ((DSearchRec.Attr and faDirectory) = faDirectory)
          and (DSearchRec.Name <> '.') and (DSearchRec.Name <> '..') then begin
          MGetFileListToStr(Resp, ISpit, iFilter, iPath + DSearchRec.Name + '\', ContainSubDir);
        end;
        FindResult := FindNext(DSearchRec);
      end;
    end;
  finally
    sysUtils.FindClose(FSearchRec);
    sysUtils.FindClose(DSearchRec);
  end;
end;

function TRmodbSvr.OnDataCase(ClientThread: TDXClientThread;
  Ihead: integer): Boolean;
var
  i: Integer;
  Llen: integer;
  LSQl, ls: string;
begin
  Result := True;
  try
    case Ihead of //
      9998: begin
        //客户端查询升级信息
          if FileExists(GetCurrPath() + 'update.ini') then begin
            if lini = nil then
              lini := TIniFile.Create(GetCurrPath + 'update.ini');
            ClientThread.Socket.WriteInteger(1);
            ClientThread.Socket.WriteInteger(lini.ReadInteger('info', 'ver', 0));
            i:= lini.ReadInteger('info', 'isfrce', 1);
            ClientThread.Socket.WriteInteger(i);
            ClientThread.Socket.WriteLn(GetCurrPath);
            ClientThread.Socket.WriteLn(lini.ReadString('info', 'hint', '无'));
            MGetFileListToStr(ls, '|', '*.*', GetCurrPath + lini.ReadString('info', 'filepath', 'update'), True);
            ClientThread.Socket.WriteInteger(length(ls));
            ClientThread.Socket.Write(ls);
          end
          else
            ClientThread.Socket.WriteInteger(0);
        end;
      9997: begin
          ls := ClientThread.Socket.ReadLn(10000);
          if FileExists(ls) then begin
            ClientThread.Socket.WriteInteger(1);
            Socket.SendZIpFile(ls, ClientThread);
          end
          else
            ClientThread.Socket.WriteInteger(0);
        end;
      0: begin //断开连接
          ClientThread.Socket.Disconnect;
        end;
      1: begin //执行一条SQL语句 更新或者执行
          Flock.Enter;
          try
            Llen := ClientThread.Socket.ReadInteger;
            LSQl := ClientThread.Socket.ReadStr(Llen);
            if Shower <> nil then
              Shower.AddShow('客户端执行语句<%s>', [LSQl]);
            try
              DataModel.UniSQL1.SQL.Clear;
              DataModel.UniSQL1.SQL.Add(LSQl);
              DataModel.UniSQL1.Execute;
              ClientThread.Socket.WriteInteger(1);
            except
              on e: Exception do begin
                ClientThread.Socket.WriteInteger(-1);
                ClientThread.Socket.WriteInteger(Length(e.Message));
                ClientThread.Socket.Write(e.Message);
                if Shower <> nil then
                  Shower.AddShow('客户端执行语句异常<%s>', [e.Message]);
              end;
            end;
          finally
            Flock.Leave;
          end;
        end;
      110: begin //批量执行语句
          Flock.Enter;
          try
            gLmemStream.Size := 0;
            Socket.GetZipStream(gLmemStream, ClientThread);
            glBatchLst.LoadFromStream(gLmemStream);
            gLmemStream.Size := 0;
            if Shower <> nil then
              Shower.AddShow('客户端批量执行语句', [LSQl]);
            try
              if glBatchLst.Count > 0 then begin
                for Llen := 0 to glBatchLst.Count - 1 do begin // Iterate
                  DataModel.UniSQL1.SQL.Clear;
                  DataModel.UniSQL1.SQL.Add(glBatchLst[Llen]);
                  DataModel.UniSQL1.Execute;
                end; // for
              end;
              ClientThread.Socket.WriteInteger(1);
            except
              on e: Exception do begin
                glBatchLst.SaveToFile('D:\1.txt');
                ClientThread.Socket.WriteInteger(-1);
                ClientThread.Socket.WriteInteger(Length(e.Message));
                ClientThread.Socket.Write(e.Message);
                if Shower <> nil then
                  Shower.AddShow('客户端执行语句异常<%s>', [e.Message]);
              end;
            end;
          finally
            Flock.Leave;
          end;
        end;
      2: begin //执行一个查询语句
          Flock.Enter;
          try
            gLmemStream.Size := 0;
            Llen := ClientThread.Socket.ReadInteger;
            LSQl := ClientThread.Socket.ReadStr(Llen);
            try
//              ls := GetCurrPath + GetDocDate + GetDocTime;
              if Shower <> nil then
                Shower.AddShow('客户端执行查询语句<%s>', [LSQl]);
              DataModel.Gqry.Close;
              DataModel.Gqry.SQL.Clear;
              DataModel.Gqry.SQL.Add(LSQl);
              DataModel.Gqry.Open;
              VarArrayToStream(DataModel.Dp.Data, gLmemStream);
              ClientThread.Socket.WriteInteger(1);
              Socket.SendZIpStream(gLmemStream, ClientThread);
            except
              on e: Exception do begin
                ClientThread.Socket.WriteInteger(-1);
                ClientThread.Socket.WriteInteger(Length(e.Message));
                ClientThread.Socket.Write(e.Message);
                if Shower <> nil then
                  Shower.AddShow('客户端执行语句异常<%s>', [e.Message]);
              end;
            end;
          finally
            Flock.Leave;
          end;
        end;
      3: begin //查询服务端数据库连接是否正常
        end;
      4: begin //激活包
        end;
      5: begin
          Flock.Enter;
          try
            ls := GetCurrDBPath(GGDBPath) + 'cfg1.mdb';
            if (gLastCpTime = 0) or (GetTickCount - gLastCpTime > 3600 * 1000 * 5) then begin
              CopyFile(PChar(GetCurrDBPath(GGDBPath) + 'cfg.mdb'), PChar(GetCurrDBPath(GGDBPath) + 'cfg1.mdb'), False);
              gLastCpTime := GetTickCount;
            end;
            Socket.SendZIpFile(ls, ClientThread);
          finally
            Flock.Leave;
          end;
        end;
      6: begin
          Flock.Enter;
          try
            gLmemStream.Size := 0;
            Llen := ClientThread.Socket.ReadInteger;
            LSQl := ClientThread.Socket.ReadStr(Llen);
            gLmemStream := ReadStream(gLmemStream, ClientThread);
            if Shower <> nil then
              Shower.AddShow('客户端执行Blob字段<%s>', [LSQl]);
            try
              DataModel.Gqry.Close;
              DataModel.Gqry.SQL.Clear;
              DataModel.Gqry.SQL.Add(LSQl);
              DataModel.Gqry.Params.ParamByName('Pbob').LoadFromStream(gLmemStream, ftBlob);
              DataModel.Gqry.Execute;
            except
              on e: Exception do begin
                ClientThread.Socket.WriteInteger(-1);
                ClientThread.Socket.WriteInteger(Length(e.Message));
                ClientThread.Socket.Write(e.Message);
                if Shower <> nil then
                  Shower.AddShow('客户端执行Blob字段<%s>', [e.Message]);
              end;
            end;
          finally
            Flock.Leave;
          end;
        end;
    end; //case
  except
    on e: Exception do
      if Shower <> nil then
        Shower.AddShow('线程执行异常<%s>', [e.Message]);
  end;
end;

procedure TRmodbSvr.OnDestroy;
begin
  inherited;
  Flock.Free;
  glBatchLst.Free;
  gLmemStream.Free;
end;


function TRmodbSvr.GetCurrDBPath(InPath: string): string;
var
  ISql: string;
  IGetPath: string;
  TStr: TStrings;
  i: Integer;
  iCount: Integer;
begin
  try
    Result := '';
    ISql := InPath;
    TStr := TStringList.Create;
    GetEveryWord(ISql, TStr, '\');
    iCount := TStr.Count;
    for i := 0 to Tstr.Count - 2 do begin
      IGetPath := IGetPath + TStr[i] + '\';
    end;
    TStr.Free;
  finally
    Result := IGetPath;
  end;
end;

function TRmodbSvr.ReadStream(Istream: TStream; ClientThread: TDXClientThread):
  TMemoryStream;
var
  LBuff: Pointer;
  i, ltot, x: Integer;
begin
  if Istream = nil then
    Istream := TMemoryStream.Create;
  x := ClientThread.Socket.ReadInteger;
  TMemoryStream(Istream).Size := x;
  LBuff := TMemoryStream(Istream).Memory;
  ltot := Istream.Size;
  x := 0;
  while ltot > 0 do begin
    i := ClientThread.Socket.Read(PChar(LBuff) + x, ltot);
    Dec(ltot, i);
    inc(x, i);
  end; // while
  Result := TMemoryStream(Istream);
end;


function TRmodbSvr.OnCheckLogin(ClientThread: TDXClientThread): boolean;
var
  lspit: TStrings;
  lname, lpsd, ls: string;
begin
  inherited OnCheckLogin(ClientThread);
  ls := ClientThread.Socket.ReadLn(10000);
  Result := false;
  try
    Gob_CFGer.SetSecton('auth');
    lspit := TStringList.Create;
    ExtractStrings(['|'], [' '], pchar(ls), lspit);
    if lspit.Count = 2 then begin
      if trim(lspit[0]) <> '' then begin
        //做客户端登陆权限认证
        lpsd := Gob_CFGer.ReadString(lspit[0]);
        if length(lspit[1]) > 0 then begin
          if lpsd = Str_Decry(lspit[1], 'rmo') then begin
            Result := true;
            if Shower <> nil then
              Shower.AddShow('用户:%s，认证通过，成功连接...', [lspit[0]]);
          end;
        end;
      end;
    end
  finally
    lspit.Free;
  end;
  if Result then
    ClientThread.Socket.WriteInteger(1001)
  else
    ClientThread.Socket.WriteInteger(1002)
end;

end.

