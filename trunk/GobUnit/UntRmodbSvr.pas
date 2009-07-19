{*******************************************************
        单元名称：UntRmodbSvr.pas
        创建日期：2008-09-16 17:26:15
        创建者	  马敏钊
        功能:     远程数据库服务端

*******************************************************}

unit UntRmodbSvr;


interface

uses Classes, UntSocketServer, DXServerCore, UntTBaseSocketServer, untFunctions, syncobjs, Windows, Forms,
  adodb;


type
  TRmodbSvr = class(TCenterServer)
  private
    Flock: TCriticalSection;
    function ReadStream(Istream: TStream; ClientThread: TDXClientThread):
      TMemoryStream;
  public
    Gqry: TADOQuery;
    Db: TDBMrg;
    GGDBPath: string;
    //连接到数据库
    function ConnToDb(IConnStr: string): boolean;
    function OnDataCase(ClientThread: TDXClientThread; Ihead: integer): Boolean; override;
    procedure OnCreate(ISocket: TBaseSocketServer); override;
    procedure OnDestroy; override;
    function GetCurrDBPath(InPath: string): string; //zbt 得到当前路径
  end;

var
  Gob_RmoDBsvr: TRmodbSvr;

implementation

uses sysUtils, pmybasedebug, DXSock, db;

{ TRmoSvr }

function TRmodbSvr.ConnToDb(IConnStr: string): boolean;
begin
  Db := TDBMrg.Create(IConnStr);
  Result := True;
  if Shower <> nil then begin
    Shower.AddShow('连接数据库功<%s>', [IConnStr]);
    Gqry := Db.GetAnQuery('GobQryer');
  end;
end;

procedure TRmodbSvr.OnCreate(ISocket: TBaseSocketServer);
begin
  inherited;
  Flock := TCriticalSection.Create;

end;

var
  gLastCpTime: Cardinal = 0;
  gLmemStream: TMemoryStream;

function TRmodbSvr.OnDataCase(ClientThread: TDXClientThread;
  Ihead: integer): Boolean;
var
  Llen: integer;
  LSQl, ls: string;
begin
  Result := True;
  try
    case Ihead of //
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
              ClientThread.Socket.WriteInteger(Db.ExecAnSql(Gqry, LSQl, []));
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
      2: begin //执行一个查询语句
          Flock.Enter;
          try
            Llen := ClientThread.Socket.ReadInteger;
            LSQl := ClientThread.Socket.ReadStr(Llen);
            if Shower <> nil then
              Shower.AddShow('客户端执行语句<%s>', [LSQl]);
            try
              ls := GetCurrPath + GetDocDate + GetDocTime;
              Db.OpenDataset(Gqry, LSQl, []).SaveToFile(ls);
              ClientThread.Socket.WriteInteger(1);
              Socket.SendZIpFile(ls, ClientThread);
              DeleteFile(ls);
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
            Llen := ClientThread.Socket.ReadInteger;
            LSQl := ClientThread.Socket.ReadStr(Llen);
            gLmemStream := ReadStream(gLmemStream, ClientThread);
//            if gLmemStream.Size > 0 then begin
//              DeCompressStream(gLmemStream);
//              gLmemStream.SaveToFile('C:\mm.bmp');
//            end;
            if Shower <> nil then
              Shower.AddShow('客户端执行Blob字段<%s>', [LSQl]);
            try
              Gqry.Close;
              Gqry.SQL.Clear;
              Gqry.SQL.Add(LSQl);
              Gqry.Parameters.ParamByName('Pbob').LoadFromStream(gLmemStream, ftBlob);
              Gqry.ExecSQL;



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
  try
    Db.Free;
  except
  end;
  Flock.Free;
end;


function TRmodbSvr.GetCurrDBPath(InPath: string): string;
var
  ISql: string;
  IGetPath: string;
  TStr: TStrings;
  i: Integer;
//  iCount: Integer;
begin
  try
    Result := '';
    ISql := InPath;
    TStr := TStringList.Create;
    GetEveryWord(ISql, TStr, '\');
//    iCount := TStr.Count;
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


end.

