{*******************************************************}
{      单元名：  Un_socket_control.pas                  }
{      创建日期：2008-7-7 23:18:37                      }
{      创建者    马敏钊                                 }
{      功能：    通讯对象单元                           }
{                                                       }
{*******************************************************}

unit UntsocketDxBaseClient;

interface

uses DXSock, classes;

type
  //客户端对象
  TSocketClient = class(TDXSock)
  private
  public
    FHost: string;
    FPort: Word;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuff(var obj; Ilen: integer);
    procedure WriteStream(Istream: TStream);
    function Getipandport(IConn: TDXSock): string;
    function GetHead: Integer; //读取报头
    procedure SendHead(ICmd: Integer); //发送报头
    procedure SendObject(IObj: TObject); //发送对象
    procedure GetObject(IObj: TObject; IClass: TClass); overload;
    //接收对象 自己根据类之类来创建对象
    procedure GetObject(IObj: TObject); overload;
    //由外部代入已经创建好的对象
    procedure SendZipFile(IFileName: string); //发送压缩文件
    function GetZipFile(IFileName: string): Integer; //接收压缩文件   //MMWIN:MEMBERSCOPY
    function GetZipStream(IStream: TStream; IConn: TDXsock): integer;
    function SendZIpStream(IStream: tStream; IConn: TDXsock): Integer;
    //连接
    function Connto(IIP: string; Iport: Word): boolean;

    procedure OnCreate; virtual; abstract;
    procedure OnDestory; virtual; abstract;

  end;

var
  GSocketClient: TSocketClient;

implementation

uses
  Windows, SysUtils, untfunctions, DXSocket;

{ TSocketClient }

function TSocketClient.Connto(IIP: string; Iport: Word): boolean;
var
  LConn: TNewConnect;
begin
  Result := false;
  FHost := IIP;
  FPort := Iport;
  LConn.Port := Iport;
  LConn.UseBlocking := True;
  LConn.UseNAGLE := True;
  LConn.UseUDP := False;
  LConn.ipAddress := IIP;
  Result := Connect(@LConn);
end;

constructor TSocketClient.Create;
begin
  inherited Create(nil);
  OnCreate;
end;

destructor TSocketClient.Destroy;
begin
  OnDestory;
  Disconnect;
  inherited;
end;

function TSocketClient.GetHead: Integer;
begin
  Result := ReadInteger;
end;

function TSocketClient.Getipandport(IConn: TDXSock): string;
begin
  Result := format('%S:%d', [PeerIPAddress, peerport]);
end;

procedure TSocketClient.GetObject(IObj: TObject; IClass: TClass);
var
  Ltep: pint;
begin
  IObj := TClass.Create;
  Ltep := Pointer(Iobj);
  inc(Ltep);
  ReceiveBuf(Ltep^, Iobj.InstanceSize - 4);
end;

procedure TSocketClient.GetObject(IObj: TObject);
var
  Ltep: pint;
begin
  Ltep := Pointer(Iobj);
  inc(Ltep);
  ReceiveBuf(Ltep^, Iobj.InstanceSize - 4);
end;

function TSocketClient.GetZipFile(IFileName: string): integer;
var
  LZipMM: TMemoryStream;
  LBuff: Pointer;
  i, ltot, x: Integer;
begin
  LZipMM := TMemoryStream.Create;
  try
    ltot := ReadInteger;
    LZipMM.Size := ltot;
    LBuff := LZipMM.Memory;
    x := 0;
    while ltot > 0 do begin
      i := Read(PChar(LBuff) + x, ltot);
      Dec(ltot, i);
      inc(x, i);
    end; // while
    DeCompressStream(LZipMM);
    LZipMM.SaveToFile(IFileName);
    Result := LZipMM.Size;
  finally // wrap up
    LZipMM.Free;
  end; // try/finally
end;

function TSocketClient.GetZipStream(IStream: TStream; IConn: TDXsock): integer;
var
  LZipMM: TMemoryStream;
begin
  LZipMM := TMemoryStream(IStream);
  LZipMM.Size := IConn.ReadInteger;
  IConn.ReceiveBuf(LZipMM.Memory^, LZipMM.Size);
  DeCompressStream(LZipMM);
end;

procedure TSocketClient.SendHead(ICmd: Integer);
begin
  WriteInteger(ICmd);
end;

procedure TSocketClient.SendObject(IObj: TObject);
var
  Ltep: Pint;
begin
  Ltep := Pointer(IObj);
  inc(Ltep);
  SendBuf(ltep^, IObj.InstanceSize - 4);
end;

procedure TSocketClient.SendZipFile(IFileName: string);
var
  LZipMM: TMemoryStream;
begin
  LZipMM := TMemoryStream.Create;
  try
    LZipMM.LoadFromFile(IFileName);
    EnCompressStream(LZipMM);
    WriteInteger(LZipMM.Size);
    SendBuf(LZipMM.Memory^, LZipMM.Size);
  finally
    LZipMM.Free;
  end;
end;

function TSocketClient.SendZIpStream(IStream: tStream; IConn: TDXsock): Integer;
begin
  EnCompressStream(TMemoryStream(IStream));
  IConn.WriteInteger(IStream.Size);
  IConn.Write(TMemoryStream(IStream).Memory, IStream.Size);
  Result := IStream.Size;
end;

procedure TSocketClient.WriteBuff(var obj; Ilen: integer);
begin
  Write(Pointer(obj), Ilen);
end;

procedure TSocketClient.WriteStream(Istream: TStream);
begin
  WriteInteger(Istream.Size);
  Write(TMemoryStream(Istream).Memory, Istream.Size);
end;

end.

