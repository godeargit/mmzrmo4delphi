unit UntTBaseSocketServer;
{封装了Dxsock服务端的基类}
interface
uses DXString, DXServerCore, DXSock, CLasses;
type
  Pint = ^integer;
  TBaseSocketServer = class
  private
    procedure DefaultConn(ClientThread: TDXClientThread);
  public
    Server: TDXServerCore;
    function GetHead(IConn: TDXSock): Integer; //读取报头
    procedure SendHead(ICmd: Integer; IConn: TDXSock); overload; //发送报头
    procedure SendHead(ICmd: Integer; IConn: TDXClientThread); overload;
    procedure SendObject(IObj: TObject; IConn: TDXSock); //发送对象
    procedure GetObject(IObj: TObject; IClass: TClass; IConn: TDXSock); overload; //接收对象
    procedure GetObject(IObj: TObject; IConn: TDXSock); overload;
    procedure GetObject(IObj: TObject; IConn: TDXClientThread); overload;
    function GetZipFile(IFileName: string; IConn: TDXClientThread): Integer;
    function SendZIpFile(IFileName: string; IConn: TDXClientThread): Integer;
    function SendZIpStream(IStream: tStream; IConn: TDXClientThread): Integer;
       function GetZipStream(IStream: TStream;IConn: TDXClientThread):integer;
    procedure StartServer;
    constructor Create(Iport: Integer);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, untfunctions, pmybasedebug, windows;
{ TBaseSocketServer }

constructor TBaseSocketServer.Create(Iport: Integer);
begin
  Server := TDXServerCore.Create(nil);
  Server.ServerPort := Iport;
  Server.Timeout := 4294967295;              //设置超时为最大值
  Server.OnNewConnect := DefaultConn;
end;

procedure TBaseSocketServer.DefaultConn(ClientThread: TDXClientThread);
begin
  //本过程什么都不做 只为用户提供一个接口
end;

destructor TBaseSocketServer.Destroy;
begin
  if Assigned(Server) then
  begin
    Server.Stop;
    Server.Close;
    FreeAndNil(Server);
  end;
  inherited;
end;

function TBaseSocketServer.GetHead(IConn: TDXSock): Integer;
begin
  Result := IConn.ReadInteger;
end;

function TBaseSocketServer.GetZipFile(IFileName: string; IConn:
  TDXClientThread): Integer;
var
  LZipMM                 : TMemoryStream;
  LBuff                  : Pointer;
  i, ltot, x             : Integer;
begin
  LZipMM := TMemoryStream.Create;
  try
    x := IConn.Socket.ReadInteger;
    LZipMM.Size := x;
    LBuff := LZipMM.Memory;
    ltot := LZipMM.Size;
    x := 0;
    while ltot > 0 do begin
      i := IConn.Socket.Read(PChar(LBuff) + x, ltot);
      Dec(ltot, i);
      inc(x, i);
    end;                                     // while
    DeCompressStream(LZipMM);
    LZipMM.SaveToFile(IFileName);
    Result := LZipMM.Size;
  finally                                    // wrap up
    LZipMM.Free;
  end;                                       // try/finally
end;

procedure TBaseSocketServer.GetObject(IObj: TObject; IClass: TClass;
  IConn: TDXSock);
var
  Ltep                   : pint;
begin
  IObj := TClass.Create;
  Ltep := Pointer(Iobj);
  inc(Ltep);
  IConn.ReceiveBuf(Ltep^, Iobj.InstanceSize - 4);
end;

procedure TBaseSocketServer.SendHead(ICmd: Integer; IConn: TDXSock);
begin
  IConn.WriteInteger(ICmd);
end;

procedure TBaseSocketServer.GetObject(IObj: TObject;
  IConn: TDXClientThread);
var
  Ltep                   : pint;
begin
  Ltep := Pointer(Iobj);
  inc(Ltep);
  IConn.Socket.ReceiveBuf(Ltep^, Iobj.InstanceSize - 4);
end;

procedure TBaseSocketServer.SendHead(ICmd: Integer;
  IConn: TDXClientThread);
begin
  IConn.Socket.WriteInteger(ICmd);
end;

procedure TBaseSocketServer.SendObject(IObj: TObject; IConn: TDXSock);
var
  Ltep                   : Pint;
begin
  Ltep := Pointer(IObj);
  inc(Ltep);
  IConn.SendBuf(ltep^, IObj.InstanceSize - 4);
end;

procedure TBaseSocketServer.StartServer;
begin
  Server.Start;
end;

procedure TBaseSocketServer.GetObject(IObj: TObject; IConn: TDXSock);
var
  Ltep                   : pint;
begin
  Ltep := Pointer(Iobj);
  inc(Ltep);
  IConn.ReceiveBuf(Ltep^, Iobj.InstanceSize - 4);
end;

function TBaseSocketServer.SendZIpFile(IFileName: string; IConn:
  TDXClientThread): Integer;
var
  LZipMM                 : TMemoryStream;
begin
  LZipMM := TMemoryStream.Create;
  try
    LZipMM.LoadFromFile(IFileName);
    EnCompressStream(LZipMM);
    IConn.Socket.WriteInteger(LZipMM.Size);
    IConn.Socket.Write(LZipMM.Memory, LZipMM.Size);
    Result := LZipMM.Size;
  finally
    LZipMM.Free;
  end;
end;

function TBaseSocketServer.SendZIpStream(IStream: tStream;
  IConn: TDXClientThread): Integer;
begin
  EnCompressStream(TMemoryStream(IStream));
  IConn.Socket.WriteInteger(IStream.Size);
  IConn.Socket.Write(TMemoryStream(IStream).Memory, IStream.Size);
  Result := IStream.Size;
end;

function TBaseSocketServer.GetZipStream(IStream: TStream;IConn: TDXClientThread): integer;
var
  LZipMM                 : TMemoryStream;
begin
  LZipMM := TMemoryStream(IStream);
  LZipMM.Size := IConn.socket.ReadInteger;
  IConn.socket.ReceiveBuf(LZipMM.Memory^, LZipMM.Size);
  DeCompressStream(LZipMM);
end;


end.

