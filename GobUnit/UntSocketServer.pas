{*******************************************************}
{      ��Ԫ����  UntSocketServer.pas                    }
{      �������ڣ�2006-2-28 20:36:19                     }
{      ������    ������                                 }
{      ���ܣ�    Tcp����������                        }
{                                                       }
{*******************************************************}



unit UntSocketServer;

interface
uses
  DXString, DXServerCore, UntTBaseSocketServer, UntTIO, sysutils;
type
  TCenterServer = class
  private
  protected
    procedure OnCreate(ISocket: TBaseSocketServer); virtual;
    procedure OnDestroy; virtual;
    procedure OnConning(ClientThread: TDXClientThread); virtual;
    function OnCheckLogin(ClientThread: TDXClientThread): boolean; virtual;
    {�û��Ͽ��¼�}
    procedure OnDisConn(ClientThread: TDXClientThread); virtual;
    function OnDataCase(ClientThread: TDXClientThread; Ihead: integer): Boolean; virtual;
    procedure OnException(ClientThread: TDXClientThread; Ie: Exception); virtual;
//------------------------------------------------------------------------------
// �����Լ�ʹ�õķ��� 2006-8-23 ������
//------------------------------------------------------------------------------
    {�û������¼� Ҳ������¼�}
    procedure UserConn(ClientThread: TDXClientThread);
    {���������¼�}
    procedure DataCase(ClientThread: TDXClientThread); virtual;
  public
    Shower: TIOer;
    Socket: TBaseSocketServer;
    {*�����̻߳�ȡIP�Ͷ˿ں�}
    function GetUserIpAndPort(ClientThread: TDXClientThread): string;
    constructor Create(IServerPort: Integer; Iio: TIOer = nil);
    destructor Destroy; override;
  end;


implementation

uses UntBaseProctol, pmybasedebug;

{ TSocketServer }

constructor TCenterServer.Create(IServerPort: Integer; Iio: TIOer = nil);
begin
  Socket := TBaseSocketServer.Create(IServerPort);
  Socket.Server.OnNewConnect := UserConn;
  Socket.Server.Start;
  Shower := Iio;
  OnCreate(Socket);
end;

destructor TCenterServer.Destroy;
begin
  OnDestroy;
  Socket.Free;
  inherited;
end;

procedure TCenterServer.DataCase(ClientThread: TDXClientThread);
var
  Lhead: Integer;
begin
  while (ClientThread.Socket.Connected) do begin
    Lhead := ClientThread.Socket.ReadInteger;
    case Lhead of //
      -1: ; //Shower.AddShow('�յ�Client %s:%d ��������Ϣ',[ClientThread.Socket.PeerIPAddress, ClientThread.Socket.PeerPort]);
    else
      if not OnDataCase(ClientThread, Lhead) then
        if Shower <> nil then
          Shower.AddShow(Format('�յ�����������%d', [Lhead]));
    end; // case
  end; // while
end;


procedure TCenterServer.OnCreate(ISocket: TBaseSocketServer);
begin
  if Shower <> nil then
    Shower.AddShow('����ɹ�����...�˿�:%d', [ISocket.Server.ServerPort]);
end;

procedure TCenterServer.UserConn(ClientThread: TDXClientThread);
begin
  try
    OnConning(ClientThread);
    if OnCheckLogin(ClientThread) then
      DataCase(ClientThread);
    OnDisConn(ClientThread);
  except

  end;
end;

procedure TCenterServer.OnConning(ClientThread: TDXClientThread);
begin
  if Shower <> nil then
    Shower.AddShow(Format('����%s:%d�û���������', [ClientThread.Socket.PeerIPAddress, ClientThread.Socket.PeerPort]));
end;

function TCenterServer.OnCheckLogin(
  ClientThread: TDXClientThread): boolean;
begin
  Result := True;
  if ClientThread.Socket.ReadInteger <> CTSLogin then begin
    Result := False;
    Socket.SendHead(STCLoginFault_Vison, ClientThread.Socket);
  end
  else if ClientThread.Socket.ReadInteger <> CClientID then begin
    Result := False;
    Socket.SendHead(STCLoginFault_Vison, ClientThread.Socket);
  end
  else
    Socket.SendHead(STCLogined, ClientThread.Socket);
end;

procedure TCenterServer.OnDisConn(ClientThread: TDXClientThread);
begin
  if (ClientThread <> nil) and (ClientThread.Socket <> nil) then begin
    if Shower <> nil then
      Shower.AddShow('�û��Ͽ�������');
    ClientThread.Socket.Disconnect;
  end
end;

function TCenterServer.OnDataCase(ClientThread: TDXClientThread; Ihead: integer):
  Boolean;
begin
  Result := True;
end;

procedure TCenterServer.OnException(ClientThread: TDXClientThread;
  IE: Exception);
begin
  if Shower <> nil then
    Shower.AddShow(Format('�û������߳��쳣 ԭ��:%s', [Ie.ClassName + '>> ' + Ie.Message]));
end;

procedure TCenterServer.OnDestroy;
begin
  if Shower <> nil then
    Shower.AddShow('�����ͷųɹ�...');
end;

function TCenterServer.GetUserIpAndPort(ClientThread: TDXClientThread): string;
begin
  Result := ClientThread.Socket.PeerIPAddress + ':' + IntToStr(ClientThread.Socket.PeerPort);
end;

end.

