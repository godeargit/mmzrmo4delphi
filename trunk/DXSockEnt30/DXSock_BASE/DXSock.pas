unit DXSock;

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSock
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2002
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2002
//               DX, Inc. Source may not be distributed without
//               written permission from both Brain Patchwork DX,
//               and DX, Inc.
//      License: (Reminder), None of this code can be added to other
//               developer products without permission. This includes
//               but not limited to DCU's, DCP's, DLL's, OCX's, or
//               any other form of merging our technologies. All of
//               your products released to a public consumer be it
//               shareware, freeware, commercial, etc. must contain a
//               license notification somewhere visible in the
//               application.
//               Example is Internet Explorer - Help->About screen
//               shows the licensed code contained in the application.
// Code Version: (4th Generation Code)
// ========================================================================
//  Description: Low-Level Socket Wrapper as an Object.
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

interface

{$I DXSock.def}

uses
  DXString,
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  DXSocket,
{$IFDEF CODE_TRACER}
  DXCodeTracer,
{$ENDIF}
{$IFDEF TLS_EDITION}
  MjbLIFO,
  BrkApart,
{$ENDIF}
  Classes;

{$WARNINGS OFF}
{$J+} // 4.0
const
  TDXHugeSize = 8192 * 2; // 16kb CHUNKS
  TDXXferTimeout: Word = 50000; // if data loss then set to 50000
  TDXMaxSocketBuffer: Word = TDXHugeSize; // Winsock Buffer Size
  PeekBufferSize: Byte = 250; // do not go over 250!
{$J-}

type
  Str1 = string[1];

//     *   <B>ddAboutToWrite</B> denotes that your Socket Layer
//         filter should encode, or compress the data.
//     *   <B>ddAfterRead</B> denotes that your Socket Layer
//         filter should decode, or decompres the data
//     *   <B>dFreePointer</B> denotes that your Socket Layer
//         filter should free the work pointer
  TDXDataDirection = (ddAboutToWrite, ddAfterRead, ddCleanRead, ddFreePointer);

  TDXFilterCallBack = procedure(DataDirection: TDXDataDirection; const InData: Pointer; var OutData: Pointer; const
    InSize: Integer; var OutSize: Integer; var Handled: Boolean; xClientThread: TThread) of object;

  // This controls outbound data only. It was implemented to
  // assist people who wanted to be able to just say send this big
  // chunk of data, without undertsanding the socket layer itself.
  //
  //
  //     *   <B>bsfRealSmall</B> uses 128 byte block of data.
  //         Obviously smaller than a TCP packet. And with Nagle
  //         enabled it would actually slow down your output.
  //         However, if you are designing a UDP based
  //         application, and know the average package size will
  //         be under 128 bytes, then you should set your output
  //         buffer to this setting. Used: <B>Very Rare</B>
  //     *   <B>bsfSmall</B> uses 256 byte block of data. Which is
  //         a standard TCP packet. Used: <B>Occasionally</B>
  //     *   <B>bsfNormal</B> uses 512 byte block of data. Which
  //         is bigger than a standard TCP packet, but is optimial
  //         when you know your data is usually bigger than 256
  //         bytes, and smaller than 512. Our testing has shown
  //         this to be very optimal when using that rule. Used: <B>Very
  //         Frequently</B>
  //     *   <B>bsfBigger</B> uses a 2048 byte block of data.
  //         Again, bigger than a standard TCP packet. Used <B>Very
  //         Rare</B>
  //     *   <B>bsgBiggest</B> uses a 4096 byte block of data.
  //         Bigger than a standard TCP packet, but is used very
  //         often by people who are porting from DOS based
  //         applications to windows. They are used to the old
  //         Borland examples which used 4kb buffers for file copy
  //         etc. Used: <B>Often</B>
  //     *   <B>bsfHUGE</B> uses a 8192 byte block of data. Bigger
  //         than a standard TCP packet, but produces the best
  //         performance. Especially is you plan to just say "Send
  //         this data, who cares how big it is". Used: <B>Most
  //         Often</B>
  // <B>Note</B> when we point out that the block of data is
  // bigger then a standard TCP packet, that is just a mental note
  // for you. The socket layer will always build the packet to the
  // appropriate network packet size. These buffers are strictly
  // used internally for how to break the data apart and shove it
  // down to the socket layer.
  //
  //
  //
  // Summary
  // Define the packet size between DXSock and the Socket Layer.
  TDXBlockSizeFlags = (
    bsfZero, // special meaning for TLS!
    bsfRealSmall,
    bsfSmall, bsfNormal,
    bsfBigger,
    bsfBiggest,
    bsfHUGE);

type
  TDXBSArray = array[0..65500] of Char;
  TDXBSArray2 = array[0..250] of Char;
{$IFDEF VER100}
  Longword = Cardinal;
{$ENDIF}

  TDXSock = class(TDXComponent) // RC2
  private
{$IFDEF CODE_TRACER}
    CodeTracer: TDXCodeTracer;
{$ENDIF}
{$IFDEF TLS_EDITION}
    tBuf: TBrkApart;
    tStack: TMJBLIFO;
    Straggler: string;
{$ENDIF}
    FClientThread: TThread;
    FTLS: Boolean;
    fChunkBuf: Pointer;
    fbClientMode: Boolean;
    fbIsUDP: Boolean;
    fbIsKeepAlive: Boolean;
    FsBindTo: string;
    FPeekBuffer: ^TDXBSArray2;
    FReadTimeout: Boolean;
    FUseBlocking: Boolean;
    FBlockSizeFlags: TDXBlockSizeFlags;
    FActualBlockSize: Integer;
    FErrStatus: Integer;
    fTooManyCharacters: Integer;
    feOnFilter: TDXFilterCallBack;
{$IFDEF TLS_EDITION}
    feOnReadFilter: TDXFilterCallBack;
{$ENDIF}
    GlobalPeerPort: Integer;
    GlobalPeerIPAddress: string;
//      GlobalTimeout:TTimeVal;
    VarConstSizeofTSockAddrIn: Integer;
// new 4.0 features
    fTotalWBytes: Cardinal;
    fTotalRBytes: Cardinal;
    fCPSStart: TDateTime;
  protected
    function GetReleaseDate: string;
    procedure SetReleaseDate(value: string);
    function GetLocalPort: Integer;
    function GetLocalIPAddr: string;
    function IsConnected: Boolean;
    function IsValidSocket: Boolean;
    function IsReadable: Boolean;
    function IsWritable: Boolean;
    function DidReadTimeout: Boolean;
    procedure SetfBlockSizeFlags(Value: TDXBlockSizeFlags);
    function CountWaiting: Integer;
  public
    SockAddr: TSockAddrIn;
{$IFDEF LINUX}
    Sock: TFileDescriptor;
{$ELSE}
    Sock: Integer;
{$ENDIF}
    constructor Create(AOwner: TComponent); // RC2
{$IFNDEF OBJECTS_ONLY} override;
{$ENDIF}
    destructor Destroy; override;
    function Connect(Parameters: PNewConnect): Boolean;
    function Listen(Parameters: PNewListen): Boolean;
    function Accept(var NewSock: TDXSock): Boolean;
    procedure CloseGracefully;
    procedure Disconnect; // Borland Friendly
    procedure CloseNow;
    function SendBuf(const Buf; Count: Integer): Integer; // Borland friendly
    function ReceiveBuf(var Buf; Count: Integer): Integer; // Borland friendly
{$IFDEF VER100}
    function BlockWrite(buf: Pointer; len: Integer): Integer;
    function WriteCh(c: Char): Integer;
    function Write(const s: string): Integer;
{$ELSE}
    function Write(c: Char): Integer; overload;
    function Write(const s: string): Integer; overload;
    function Write(buf: Pointer; len: Integer): Integer; overload;
{$ENDIF}
    function WriteLn(const s: string): Integer;
    function WriteResultCode(const Code: Integer; const Rslt: string): Integer;
    function WriteWithSize(S: string): Boolean;
    function WriteInteger(const n: integer): integer;

{$IFDEF VER100}
    function SendFromStreamRange(Stream: TStream; Range: Integer): Boolean;
    function SendFromStream(Stream: TStream): Boolean;
    function SendFromWindowsFile(var Handle: Integer): boolean;
    function SendFromBorlandFile(var Handle: file): boolean;
{$ELSE}
    function SendFrom(Stream: TStream; Range: Integer): Boolean; overload;
    function SendFrom(Stream: TStream): Boolean; overload;
    function SendFrom(var Handle: Integer): boolean; overload;
    function SendFrom(var Handle: file): boolean; overload;
{$ENDIF}
    function SendFromStreamWithSize(Stream: TStream): Boolean;
{$IFDEF VER100}
    function BlockRead(buf: Pointer; len: Integer): Integer;
    function Read: Char;
{$ELSE}
    function Read(buf: Pointer; len: Integer): Integer; overload;
    function Read: Char; overload;
{$ENDIF}
    function ReadInteger: integer;
    function ReadStr(MaxLength: Integer): string;
    function ReadString(MaxLength: Integer; Timeout: Longword): string;
    function ReadLn(Timeout: Longword): string;
    function ReadCRLF(Timeout: Longword): string;
    function ReadToAnyDelimiter(Timeout: Longword; Delimiter: string): string;
    function ReadNull(Timeout: Longword): string;
    function ReadSpace(Timeout: Longword): string;
    function ReadWithSize: string;
{$IFDEF VER100}
    function SaveToStream(Stream: TStream; Timeout: Longword): Boolean;
    function SaveToWindowsFile(var Handle: Integer; Timeout: Longword): boolean;
    function SaveToBorlandFile(var Handle: file; Timeout: Longword): boolean;
{$ELSE}
    function SaveTo(Stream: TStream; Timeout: Longword): Boolean; overload;
    function SaveTo(var Handle: Integer; Timeout: Longword): boolean; overload;
    function SaveTo(var Handle: file; Timeout: Longword): boolean; overload;
{$ENDIF}
    function SaveToStreamWithSize(Stream: TStream; Timeout: Longword): Boolean;
    function GetChar: Str1;
    function GetByte: Byte;
    function FilterRead(const InBuf: Pointer; var OutBuf: Pointer; InSize: Integer; xClientThread: TThread): Integer;
    function PeekString: string;
    function PeekChar: Char;
    function GetErrorStr: string;
    function GetErrorDesc(errorCode: Integer): string;
    procedure SetNagle(TurnOn: Boolean);
    procedure SetBlocking(TurnOn: Boolean);
    procedure WinsockVersion(var WinsockInfo: PWinsockInfo);
    // made public for new TDXSockClient:
    procedure SockClientSetGlobal(I: string; P: Integer);
    procedure SetTimeoutAndBuffer(SocketHandle: Integer);
// new 3.0 features:
    function DroppedConnection: Boolean;
    function WaitForData(timeout: Longint): Boolean;
// new 4.0 features:
    procedure RestartCharactersPerSecondTimer;
    function CharactersPerSecondWritten: Integer;
    function CharactersPerSecondReceived: Integer;
  published
    property TLSActive: Boolean read FTLS write FTLS;
    property TLSClientThread: TThread read FClientThread write FClientThread;
    property BindTo: string read fsBindTo
      write fsBindTo;
    property Connected: Boolean read IsConnected;
    property CharactersToRead: Integer read CountWaiting;
    property ReceiveLength: Integer read CountWaiting; // Borland Friendly
    property ValidSocket: Boolean read IsValidSocket;
    property LastReadTimeout: Boolean read DidReadTimeout;
    property LastCommandStatus: Integer read FErrStatus write FErrStatus;
    property OutputBufferSize: TDXBlockSizeFlags read fBlockSizeFlags
      write SetfBlockSizeFlags;
    property TooManyCharacters: Integer read fTooManyCharacters
      write fTooManyCharacters;
    property IsUDPMode: Boolean read fbIsUDP
      write fbIsUDP;
    property IsKeepAliveMode: Boolean read fbIsKeepAlive write fbIsKeepAlive;
    property PeerIPAddress: string read GlobalPeerIPAddress
      write GlobalPeerIPAddress;
    property PeerPort: Integer read GlobalPeerPort
      write GlobalPeerPort;
    property LocalIPAddress: string read GetLocalIPAddr;
    property LocalPort: Integer read GetLocalPort;
    property Readable: Boolean read IsReadable;
    property Writable: Boolean read IsWritable;
    property ReleaseDate: string read GetReleaseDate
      write SetReleaseDate;
    property OnFilter: TDXFilterCallBack read feOnFilter
      write feOnFilter;
{$IFDEF CODE_TRACER}
    property DXCodeTracer: TDXCodeTracer read CodeTracer
      write CodeTracer;
{$ENDIF}
{$IFDEF TLS_EDITION}
    property OnReadFilter: TDXFilterCallBack read feOnReadFilter
      write feOnReadFilter;
{$ENDIF}
  end;

implementation

uses
{$IFNDEF LINUX}
  Windows,
{$ENDIF}
  SysUtils;

function TDXSock.GetReleaseDate: string;
begin
  Result := BPDX_RELEASE_DATE;
end;

procedure TDXSock.SetReleaseDate(value: string);
begin
       // Absorb!
end;

constructor TDXSock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // RC2
  FReadTimeout := False;
//   GetMem (FPeekBuffer,PeekBufferSize) ;
  FPeekBuffer := System.GetMemory(PeekBufferSize);
  fChunkBuf := nil;
  SetFBlockSizeFlags(bsfNormal);
  if not SocketLayerLoaded then
    ShowMessageWindow('Fatal Socket Error', '(WSAStartup) ' + GetErrorStr);
  fTooManyCharacters := 2048;
  Sock := INVALID_SOCKET;
  fbIsUDP := False;
  fbIsKeepAlive := False;
  fbClientMode := False;
  FUseBlocking := True;
  GlobalPeerPort := 0;
  GlobalPeerIPAddress := '';
//   GlobalTimeout.tv_Sec:=0;
//   GlobalTimeout.tv_uSec:=1000; // was 10000 4RC2
  VarConstSizeofTSockAddrIn := ConstSizeofTSockAddrIn;
end;

destructor TDXSock.Destroy;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.EndTransaction;
  end;
{$ENDIF}
{$IFDEF TLS_EDITION}
  if assigned(tstack) then begin
    tStack.Free;
    tStack := nil;
  end;
{$ENDIF}
  if Assigned(fChunkBuf) then
//      FreeMem (fChunkBuf,fActualBlockSize);
    System.FreeMemory(fChunkBuf);
  fChunkBuf := nil;
//   FreeMem (FPeekBuffer,PeekBufferSize);
  System.FreeMemory(FPeekBuffer);
  if Sock <> INVALID_SOCKET then
    CloseNow;
  inherited Destroy;
end;

procedure TDXSock.SetTimeoutAndBuffer(SocketHandle: Integer);
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SetTimeoutAndBuffer(' + IntToStr(SocketHandle) + ')');
  end;
{$ENDIF}
  ResetBufferAndTimeout(SocketHandle, TDXXferTimeout, TDXMaxSocketBuffer);
  FErrStatus := 0;
end;

function TDXSock.Connect(Parameters: PNewConnect): Boolean;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.StartTransaction;
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Connect');
  end;
{$ENDIF}
  fTotalWBytes := 0;
  fTotalRBytes := 0;
  Result := False;
  with Parameters^ do begin
    FUseBlocking := UseBlocking;
    fbIsUDP := UseUDP;
    Sock := ClientConnectToServer(ipAddress, Port, UseUDP, UseNAGLE, @SockAddr, FErrStatus);
    if (FErrStatus <> 0) then
      Exit;
    GlobalPeerPort := ntohs(SockAddr.sin_port);
    GlobalPeerIPAddress := inet_ntoa(SockAddr.sin_addr);
    DXSocket.SetBlocking(Sock, UseBlocking, FErrStatus);
    fbIsKeepAlive := False;
    if not FbIsUDP then begin
      SetSockStatusBool(Sock, SO_KeepAlive, True, FErrStatus);
      fbIsKeepAlive := FErrStatus = 0;
    end;
    SetTimeoutAndBuffer(Sock);
//      if FbIsUDP then begin
    SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 4, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 3, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 2, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer, FErrStatus);
//      end;
  end;
  fbClientMode := True;
  Result := True;
  fCPSStart := Now;
end;

function TDXSock.Listen(Parameters: PNewListen): Boolean;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.StartTransaction;
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Listen');
  end;
{$ENDIF}
  Result := False;
  with Parameters^ do begin
    FUseBlocking := UseBlocking;
    Sock := BindAndListen(fsBindTo, Port, WinsockQueue, UseUDP, UseNAGLE,
      Connectionless, @SockAddr, FErrStatus);
    fbIsUDP := UseUDP;
    if Sock = Invalid_Socket then
      Exit; // linux does not set FErrStatus!
    if FErrStatus = 0 then
      DXSocket.SetBlocking(Sock, UseBlocking, FErrStatus)
    else
      Exit;
    if not fbIsUDP then begin
      SetSockStatusBool(Sock, SO_KeepAlive, True, FErrStatus);
      fbIsKeepAlive := fErrStatus = 0;
    end;
    SetTimeoutAndBuffer(Sock);
    SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 4, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 3, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer * 2, FErrStatus);
    if FErrStatus <> 0 then
      SetReceiveBuffer(Sock, TDXMaxSocketBuffer, FErrStatus);
  end;
  fErrStatus := 0;
  fbClientMode := False;
  Result := True;
  GlobalPeerPort := 0;
  GlobalPeerIPAddress := '';
end;

function TDXSock.Accept(var NewSock: TDXSock): Boolean;
var
  ICreatedIt: Boolean;

begin
  Result := False;
  if Sock = INVALID_SOCKET then
    exit;
  Result := IsAcceptWaiting(Sock);
  if (not Result) or fbIsUDP then
    Exit;
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Accept');
  end;
{$ENDIF}
  ICreatedIt := not Assigned(NewSock);
  if ICreatedIt then
    NewSock := TDXSock.Create(nil); // RC2
  NewSock.Sock := AcceptNewConnect(Sock, @NewSock.SockAddr, @VarConstSizeofTSockAddrIn, FErrStatus);
  if FErrStatus <> 0 then begin
    NewSock.Sock := Invalid_Socket;
    if ICreatedIt then begin
      NewSock.Free;
      NewSock := nil;
    end;
    Result := False;
    Exit;
  end;
  NewSock.GlobalPeerPort := ntohs(NewSock.SockAddr.sin_port);
  NewSock.GlobalPeerIPAddress := inet_ntoa(NewSock.SockAddr.sin_addr);
  NewSock.fbClientMode := False;
  NewSock.fCPSStart := Now;
//   SetTimeoutAndBuffer(NewSock.Sock);
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    NewSock.DXCodeTracer := CodeTracer; // link new sessions automatically
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Accepted/Configured');
  end;
{$ENDIF}
end;

{$IFDEF SUPPORT_DESIGNTIME_CLIENTS}

procedure ProcessMessages;
var
  MsgRec: TMsg;

begin
  if not IsConsole then
    if PeekMessage(MsgRec, 0, 0, 0, PM_REMOVE) then begin
      TranslateMessage(MsgRec);
      DispatchMessage(MsgRec)
    end;
end;
{$ENDIF}

{$IFDEF VER100}

function TDXSock.BlockWrite(buf: Pointer; len: Integer): Integer;
{$ELSE}

function TDXSock.Write(buf: Pointer; len: Integer): Integer;
{$ENDIF}
var
  BytesLeft: Integer;
  BytesSent: Integer;
  XferSize: Integer;
  TmpP: Pointer;
  Filtered: Pointer;
  NewLen: Integer;
  Handled: Boolean;

begin
{$IFDEF TLS_EDITION}
  DoSleepEx(0);
{$ENDIF}
  Result := 0;
  if Sock = INVALID_SOCKET then
    Exit;
  if (Len < 1) then begin
    if fbIsUDP then begin
      UDPSend(Sock, Buf^, 0, 0, SockAddr, ConstSizeofTSockAddrIn, FErrStatus); // 2.3 - empty udp packet
      GlobalPeerPort := ntohs(SockAddr.sin_port);
      GlobalPeerIPAddress := inet_ntoa(SockAddr.sin_addr);
    end;
    Exit;
  end;
  NewLen := 0;
  if Assigned(feOnFilter) then begin
    Handled := False;
    Filtered := nil;
    feOnFilter(ddAboutToWrite, Buf, Filtered, Len, NewLen, Handled, FClientThread);
    if not Handled then begin
      fErrStatus := 9999; {onFilter failed!}
      Exit;
    end;
  end;
  if fbIsUDP then begin
    if NewLen = 0 then
      Result := UDPSend(Sock, Buf^, Len, 0, SockAddr, ConstSizeofTSockAddrIn, FErrStatus)
    else begin
      Result := UDPSend(Sock, Filtered^, NewLen, 0, SockAddr, ConstSizeofTSockAddrIn, FErrStatus);
      if Assigned(feOnFilter) then
        feOnFilter(ddFreePointer, Filtered, Filtered, NewLen, NewLen, Handled, FClientThread);
    end;
    GlobalPeerPort := ntohs(SockAddr.sin_port);
    GlobalPeerIPAddress := inet_ntoa(SockAddr.sin_addr);
    Exit;
  end;
  if NewLen = 0 then begin
    BytesLeft := Len;
    TmpP := Buf;
  end
  else begin
    BytesLeft := NewLen;
    Len := NewLen;
    TmpP := Filtered;
  end;
  fErrStatus := 0;
  repeat
{$IFDEF SUPPORT_DESIGNTIME_CLIENTS}
    ProcessMessages;
{$ENDIF}
    XferSize := BytesLeft;
    if IsWritAble then begin
// DXS4 do not remove this line: it is manditory!
      if XFerSize > FActualBlockSize then
        XFerSize := FActualBlockSize;
      BytesSent := BasicSend(Sock, TmpP^, XferSize, 0, FErrStatus);
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
        CodeTracer.SendMessage(dxctDebug, 'TDXSock.Write SENT: [' + IntToStr(BytesSent) + ' bytes] FErrStatus=' + IntToStr(FErrStatus));
      end;
{$ENDIF}
      case BytesSent of
        -1: begin
            case fErrStatus of
              WSAETIMEDOUT,
                WSAENOBUFS,
                WSAEWOULDBLOCK: fErrStatus := 0;
              WSAECONNABORTED, WSAECONNRESET: begin
                  CloseNow;
                end;
//                  else ShowMessageWindow('','unknown fErrStatus='+IntToStr(fErrStatus));
            end;
          end;
        0: begin
//               ShowMessageWindow('','ReadError(0) '+IntToStr(fErrStatus));
            CloseNow;
          end;
      else begin
          if BytesSent > 0 then
            Dec(BytesLeft, BytesSent);
          if (BytesLeft > 0) and (fErrStatus = 0) then begin // 3.0 [major bug fix!!]
            Inc(LongInt(TmpP), BytesSent);
          end;
        end;
      end;
    end; // Is Write able.
  until (BytesLeft = 0) or (FErrStatus <> 0) or (sock = Invalid_Socket);
  Result := Len - BytesLeft;
  if Result > 0 then
    fTotalWBytes := fTotalWBytes + Result;
  if Assigned(feOnFilter) then
    feOnFilter(ddFreePointer, nil, Filtered, NewLen, NewLen, Handled, FClientThread);
end;

function TDXSock.WriteInteger(const n: integer): integer;
var
  x: integer;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WriteInteger(' + IntToStr(N) + ')');
  end;
{$ENDIF}
  x := htonl(n);
{$IFDEF VER100}
  result := BlockWrite(@x, sizeof(x));
{$ELSE}
  result := Write(@x, sizeof(x));
{$ENDIF}
end;

{$IFDEF VER100}

function TDXSock.WriteCh(c: Char): Integer;
{$ELSE}

function TDXSock.Write(c: Char): Integer;
{$ENDIF}
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WriteCh(' + C + ')');
  end;
{$ENDIF}
{$IFDEF VER100}
  Result := BlockWrite(@C, 1);
{$ELSE}
  Result := Write(@C, 1);
{$ENDIF}
end;

function TDXSock.Write(const s: string): Integer;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Write(' + S + ')');
  end;
{$ENDIF}
{$IFDEF VER100}
  Result := BlockWrite(@S[1], Length(S));
{$ELSE}
  Result := Write(@S[1], Length(S));
{$ENDIF}
end;

function TDXSock.WriteLn(const s: string): Integer;
var
  Len: Integer;
  Ws: string;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WriteLn(' + S + ')');
  end;
{$ENDIF}
  if Assigned(feOnFilter) then begin
    Len := 2;
    Result := Write(S) + Len; // send via filter
    Ws := #13#10;
    if fbIsUDP then begin // append CRLF unfiltered!
      UDPSend(Sock, Ws[1], Len, 0, SockAddr, ConstSizeofTSockAddrIn, FErrStatus);
    end
    else begin
      BasicSend(Sock, Ws[1], Len, 0, FErrStatus);
    end;
  end
  else
    Result := Write(S + #13#10);
end;

function TDXSock.WriteResultCode(const Code: Integer; const Rslt: string): Integer;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WriteResult()');
  end;
{$ENDIF}
  Result := Writeln(IntToStr(Code) + #32 + Rslt);
end;

function TDXSock.ReadInteger: integer;
var
  n: integer;
  cnt: integer;

begin
{$IFDEF VER100}
  cnt := BlockRead(@n, sizeof(n));
{$ELSE}
  cnt := Read(@n, sizeof(n));
{$ENDIF}
  if cnt = sizeof(n) then begin
    n := ntohl(n);
    result := n;
  end
  else
    result := -1;
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.ReadInteger=' + IntToStr(Result));
  end;
{$ENDIF}
end;

{$IFDEF VER100}

function TDXSock.BlockRead(buf: Pointer; len: Integer): Integer;
{$ELSE}

function TDXSock.Read(buf: Pointer; len: Integer): Integer;
{$ENDIF}
var
  UDPAddrSize: Integer;
//   Tries:Integer;
{$IFDEF TLS_EDITION}
  Filtered, InData: Pointer;
  Handled: Boolean;
  NewLen: Integer;
  StartTime: Longword;
  SizeToRead: Integer;
{$ENDIF}

begin
{$IFDEF TLS_EDITION}
  DoSleepEx(0);
{$ENDIF}
  fReadTimeout := False;
  Result := 0;
  if (Sock = INVALID_SOCKET) or (Len < 1) then
    exit;
//   Tries:=0;
  if fbIsUDP then begin
    UDPAddrSize := ConstSizeofTSockAddrIn;
    Result := UDPRecv(Sock, Buf^, Len, 0, SockAddr, UDPAddrSize, FErrStatus);
    GlobalPeerPort := ntohs(SockAddr.sin_port);
    GlobalPeerIPAddress := inet_ntoa(SockAddr.sin_addr);
  end
  else begin
{$IFNDEF TLS_EDITION}
//      if (CountWaiting>0) or (Tries>=3) then begin
    Result := BasicRecv(Sock, Buf^, Len, 0, FErrStatus);
{$IFDEF CODE_TRACER}
    if Assigned(CodeTracer) then begin
      if (Result = -1) and ((fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) then {absorb}
      else if Result > 0 then
        CodeTracer.SendMessage(dxctDebug, 'TDXSock.Read RECV: ' + PChar(Buf) + ' [' + IntToStr(Result) + '] fes=' + IntToStr(FErrStatus))
      else
        CodeTracer.SendMessage(dxctDebug, 'TDXSock.Read RECV: [' + IntToStr(Result) + '] fes=' + IntToStr(FErrStatus));
    end;
{$ENDIF}
 //     end;
{$ELSE}
//      if (CountWaiting>0) or (Tries>=3) then begin
    if Assigned(feOnFilter) then begin
      SetBlocking(True);
      SizeToRead := 0;
      StartTime := DxString.TimeCounter + 120000;
      while (SizeToRead = 0) and Connected and (not DXString.Timeout(StartTime)) do begin
        ioctlsocket(Sock, FIONREAD, Longint(SizeToRead));
        DoSleepEx(1);
      end;
      if SizeToRead <> 0 then begin
        InData := nil;
        Filtered := nil;
//               GetMem (InData,SizeToRead) ;
        InData := System.GetMemory(SizeToRead);
        Result := Recv(Sock, InData^, SizeToRead, 0);
      end;
    end
    else
      Result := BasicRecv(Sock, Buf^, Len, 0, FErrStatus);
//      end;
  end;
  if Result = 0 then
    CloseGracefully;
  fReadTimeout := Result < 1;
  if (Result > 0) and Assigned(feOnFilter) then begin
    Handled := False;
    Len := 0;
    feOnFilter(ddAfterRead, InData, Filtered, SizeToRead, Len, Handled, FClientThread);
    if not Handled then begin
      fErrStatus := 9999; {onFilter failed!}
      if InData <> nil then begin
//            FreeMem (InData,SizeToRead) ;
        System.FreeMemory(InData);
        InData := nil;
      end;
      CloseGracefully;
    end
    else
      Result := Len;
    if Filtered = nil then
      Result := 0;
    if Filtered <> nil then
      Move(Filtered^, Buf^, Len);
    if InData <> nil then begin
//         FreeMem (InData,SizeToRead) ;
      System.FreeMemory(InData);
      InData := nil;
    end;
    feOnFilter(ddFreePointer, nil, Filtered, Len, Len, Handled, FClientThread);
  end;
{$ENDIF}
end;
fReadTimeout := Result < 1;
if Result = 0 then
  CloseGracefully
else if Result > 0 then
  fTotalRBytes := fTotalRBytes + Result;
end;

function TDXSock.Read: Char;
var
  Size: Integer;

begin
{$IFDEF VER100}
  Size := BlockRead(@Result, 1);
{$ELSE}
  Size := Read(@Result, 1);
{$ENDIF}
  if Size < 1 then
    Result := #0;
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Read=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.ReadStr(MaxLength: Integer): string;
var
  Size: Integer;
  Ctr: Integer;
  Done: Boolean;
  ReadSize: Integer;

begin
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    Exit;
  Result := '';
  if MaxLength = 0 then
    Exit;
  Size := MaxLength;
  if MaxLength < 0 then
    Size := TDXHugeSize;
  Setlength(Result, Size);
  fErrStatus := 0;
  Ctr := 0;
  Done := False;
  while (not Done) and (IsConnected) do begin
{$IFDEF VER100}
    ReadSize := BlockRead(@Result[Ctr + 1], Size - Ctr);
{$ELSE}
    ReadSize := Read(@Result[Ctr + 1], Size - Ctr);
{$ENDIF}
    Done := (Ctr + ReadSize = Size) or
      ((ReadSize = -1) and (MaxLength = -1));
    if not Done then begin
      if ReadSize > 0 then
        Inc(Ctr, ReadSize);
      if (ReadSize > 0) and
        (MaxLength = -1) and
        (CountWaiting = 0) then begin
        Done := True;
      end
      else begin
        DoSleepEx(1); // allow sockets to digest tcpip.sys packets...
        ProcessWindowsMessageQueue;
      end;
    end
    else
      fErrStatus := 0;
  end;
  if (((fErrStatus <> 0) and (fErrStatus <> WSAETIMEDOUT) and (fErrStatus <> WSAEWOULDBLOCK))) or (Size = 0) then
    Result := ''
  else if (Size = Socket_Error) then
    Result := ''
  else begin
    Setlength(Result, MaxLength);
    fReadTimeout := False;
  end;
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    if Result <> '' then
      CodeTracer.SendMessage(dxctDebug, 'TDXSock.ReadStr=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.ReadString(MaxLength: Integer; Timeout: Longword): string;
var
  Size: Integer;
  StartTime: Comp;

begin
  if (MaxLength < 1) or (MaxLength > 250) then begin // 4RC2
    Result := ReadStr(MaxLength);
    Exit;
  end;
  Result := '';
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    Exit;
  fReadTimeout := False;
  StartTime := TimeCounter + Timeout;
  fErrStatus := 0;
  while (CountWaiting < MaxLength) and
    (not DXString.Timeout(StartTime)) and
    (IsConnected) do begin
    DoSleepEx(1);
  end;
  if (CountWaiting < MaxLength) then begin
    fReadTimeout := True;
    Exit;
  end;
  Setlength(Result, MaxLength);
  FillChar(Result[1], MaxLength, 0);
{$IFDEF VER100}
  Size := BlockRead(@Result[1], MaxLength);
{$ELSE}
  Size := Read(@Result[1], MaxLength);
{$ENDIF}
  if (((fErrStatus <> 0) and (fErrStatus <> WSAETIMEDOUT) and (fErrStatus <> WSAEWOULDBLOCK))) or (Size = 0) then
    Result := ''
  // 3.0
  else if (Size = Socket_Error) then
    Result := ''
  else
    Setlength(Result, Size);
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.ReadString=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.GetChar: Str1;
var
  Size: Integer;

begin
// 7-27   SetLength(Result, 1);
  Result := #32;
{$IFDEF VER100}
  Size := BlockRead(@Result[1], 1);
{$ELSE}
  Size := Read(@Result[1], 1);
{$ENDIF}
  case Size of
    0: begin
        CloseNow;
        Result := '';
      end;
    1: begin
      end;
  else begin
      if (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK) then
        fReadTimeout := False;
      Result := '';
    end;
  end;
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.GetChar=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.GetByte: Byte;
var
  L: Str1;

begin
  L := GetChar;
  if L = '' then
    Result := 0
  else
    Result := Ord(L[1]);
end;

function TDXSock.ReadLn(Timeout: Longword): string;
var
  markerCR, markerLF: Integer;
  s: string;
  startTime: Comp;
  LastChar: Str1;
  pstring: string;

{$IFDEF TLS_EDITION}
  function TestStack(ts: TMJBLIFO): Boolean;
  begin
    Result := False;
    if assigned(tStack) then
      Result := ts.ItemCount > 0
    else
      tStack := TMJBLIFO.Create;
  end;
{$ENDIF}

begin
  Result := '';
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    exit;
{$IFDEF TLS_EDITION}
  if FTLS = True then begin
    if TestStack(tStack) then
      Result := tStack.Pop
    else begin
      pString := ReadStr(-1);
      if pString = '' then
        pString := ReadStr(-1);
  //   If pString[1] = #0 Then pString := ReadStr(-1);
      if Straggler <> '' then
        pString := Straggler + pString;
{$IFDEF OBJECTS_ONLY}
      tBuf := TBrkApart.Create;
{$ELSE}
      tBuf := TBrkApart.Create(nil);
{$ENDIF}
      tBuf.AllowEmptyString := True;
      tBuf.BaseString := pString;
      tBuf.BreakString := #13#10;
      tBuf.BreakApart;
      MarkerLF := tbuf.StringList.Count - 2; // Allow for last String as CRLF
      for markerCR := MarkerLF downto 0 do begin
        tStack.Push(tbuf.StringList.Strings[markerCR]);
      end;
      Straggler := tBuf.Straggler;
      FreeAndNil(tBuf);
      if tStack.ItemCount > 0 then
        Result := tStack.Pop
      else
        Result := pString;
    end;
    Exit;
  end;
{$ENDIF}
  S := GetChar;
  LastChar := S;
  if (Sock = INVALID_SOCKET) {or (fReadTimeout) removed 7-27} then
    exit;
  MarkerLF := 0;
  MarkerCR := 0;
  fErrStatus := 0;
  StartTime := TimeCounter + Timeout;
  while (Sock <> Invalid_Socket) and
    (MarkerLF + MarkerCR = 0) and
    (not DXString.Timeout(StartTime)) and
    (Length(S) < fTooManyCharacters) and
    ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT)
    // 7-27:
    or (fErrStatus = WSAEWOULDBLOCK)) do begin
    if fErrStatus = WSAEWOULDBLOCK then
      ProcessWindowsMessageQueue;
    if (LastChar = '') or (not (LastChar[1] in [#10, #13])) then begin {handles getchar from above!}
      pString := PeekString;
      if DXString.Timeout(StartTime) then
        Break;
      if (pString = '') then begin
        LastChar := GetChar;
      end
      else begin
        MarkerLF := CharPos(#10, pString);
        MarkerCR := CharPos(#13, pString);
        if MarkerLF + MarkerCR > 0 then begin
          if MarkerLF = 0 then
            MarkerLF := MarkerCR
          else if MarkerCR = 0 then
            MarkerCR := MarkerLF;
          if Min(MarkerLF, MarkerCR) > 1 then // 2.4
            S := S + Copy(pString, 1, Min(MarkerLF, MarkerCR) - 1);
          ReadStr(Min(MarkerLF, MarkerCR));
          LastChar := #13;
        end
        else begin
          S := S + pString;
          ReadStr(Length(pString));
          LastChar := '';
        end;
      end;
      if DXString.Timeout(StartTime) then
        Break;
      if LastChar > '' then begin
        S := S + LastChar;
      end;
    end;
    if (Length(LastChar) > 0) and (LastChar[1] in [#10, #13]) then begin
      MarkerLF := CharPos(#10, S);
      MarkerCR := CharPos(#13, S);
      if MarkerLF + MarkerCR > 0 then begin
        if MarkerLF = Length(S) then begin {unix or DOS}
          if MarkerCR = 0 then begin {unix or Mac}
            if CountWaiting > 0 then
              if PeekChar = #13 then begin {Mac}
                LastChar := GetChar;
                S := S + LastChar;
              end;
          end
          else if MarkerCR < MarkerLF then
            MarkerLF := MarkerCR;
          MarkerCR := MarkerLF;
        end;
        if MarkerCR = Length(S) then begin {Mac or DOS}
          if MarkerLF = 0 then begin {Mac or DOS}
            if CountWaiting > 0 then
              if PeekChar = #10 then begin {DOS}
                LastChar := GetChar;
                S := S + LastChar;
              end;
          end
          else if MarkerLF < MarkerCR then
            MarkerCR := MarkerLF;
          MarkerLF := MarkerCR;
        end;
      end;
    end;
  end;
  if Sock = INVALID_SOCKET then
    exit;
  FReadTimeout := (MarkerCR < 1) and (DXString.Timeout(StartTime));
  Result := Copy(S, 1, MarkerCR - 1);
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.ReadLn=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.ReadCRLF(Timeout: Longword): string;
begin
  Result := ReadToAnyDelimiter(Timeout, #13#10);
end;
{var
  marker: Integer;
  s: string;
  startTime: Longword;

begin
  Result := '';
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then exit;
  Marker := 0;
  StartTime := TimeCounter + Timeout;
  fErrStatus := 0;
  while (sock <> Invalid_Socket) and
    (Marker = 0) and
    (not DXString.Timeout(StartTime)) and
    (Length(S) < fTooManyCharacters) and
    ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT)) do begin
    S := S + GetChar;
    Marker := QuickPos(#13#10, S);
  end;
  if Sock = INVALID_SOCKET then exit;
  Result := Copy(S, 1, Marker - 1);
end;}

function TDXSock.ReadToAnyDelimiter(Timeout: Longword; Delimiter: string): string;
var
  slen: Integer;
  marker: Integer;
  s: string;
  startTime: Comp;
  pString: string;
  iDel: Integer;

begin
  Result := '';
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    exit;
  S := '';
  sLen := 0;
  StartTime := TimeCounter + Timeout;
  Marker := 0;
  while (sock <> Invalid_Socket) and
    (Marker = 0) and
    (not DXString.Timeout(StartTime)) and
    (sLen < fTooManyCharacters) and
    ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) do begin
    pString := PeekString;
    if pString <> '' then begin
      sLen := Length(S);
      S := S + pString;
      Marker := QuickPos(Delimiter, S);
      if Marker = 0 then begin
        ReadStr(Length(pString)); // clear socket
      end
      else begin
        S := Copy(S, 1, Marker - 1);
        if Marker < sLen then
          iDel := Length(Delimiter) - (sLen - Marker)
        else
          iDel := (Marker - sLen) + Length(Delimiter);
//          If Marker<sLen then iDel:=Length(Delimiter)-(sLen-Marker+1)
//          Else iDel:=Marker-sLen+(Length(Delimiter)-1);
        ReadStr(iDel);
      end;
    end
    else begin
      pString := GetChar;
      if pString = '' then
        DoSleepEx(1)
      else begin
        Inc(sLen);
        S := S + pString;
      end;
    end;
  end;
  if Sock = INVALID_SOCKET then
    exit;
  fReadTimeout := DXString.Timeout(StartTime);
  Result := S; // return what ever is collected, even if not done!
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.ReadToAnyDelimeter=' + Result);
  end;
{$ENDIF}
end;

function TDXSock.ReadNull(Timeout: Longword): string;
begin
  Result := ReadToAnyDelimiter(Timeout, #0);
end;

function TDXSock.ReadSpace(Timeout: Longword): string;
begin
  Result := ReadToAnyDelimiter(Timeout, #32);
end;

function TDXSock.SendBuf(const Buf; Count: Integer): Integer; // Borland friendly
begin
{$IFDEF VER100}
  Result := BlockWrite(@Buf, Count);
{$ELSE}
  Result := Write(@Buf, Count);
{$ENDIF}
end;

function TDXSock.ReceiveBuf(var Buf; Count: Integer): Integer; // Borland friendly
begin
{$IFDEF VER100}
  Result := BlockRead(@Buf, Count);
{$ELSE}
  Result := Read(@Buf, Count);
{$ENDIF}
end;

{$IFDEF VER100}

function TDXSock.SendFromStream(Stream: TStream): Boolean;
{$ELSE}

function TDXSock.SendFrom(Stream: TStream): Boolean;
{$ENDIF}
var
  Len: Integer;
  SSize, SPosition: Integer;
  Tries: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SendFrom');
  end;
{$ENDIF}
  fErrStatus := 0;
  SSize := Stream.Size;
  SPosition := Stream.Position;
  Tries := 0;
  while (sock <> Invalid_Socket) and
    (Stream.Position < Stream.Size) and
    (fErrStatus = 0) and
    (Tries < 3) do begin
    if (SSize - SPosition) < FActualBlockSize then
      Len := SSize - SPosition
    else
      Len := FActualBlockSize;
    if Len > 0 then begin
      Stream.Seek(SPosition, 0);
      Stream.Read(fChunkBuf^, Len);
{$IFDEF VER100}
      Len := BlockWrite(fChunkBuf, Len);
{$ELSE}
      Len := Write(fChunkBuf, Len);
{$ENDIF}
      SPosition := SPosition + Len;
      if fErrStatus > 0 then begin
        Tries := 3;
      end
      else if Len < 1 then
        Inc(Tries)
      else
        Tries := 0;
    end;
  end;
  Result := (Sock <> INVALID_SOCKET) and (fErrStatus = 0);
end;

{$IFDEF VER100}

function TDXSock.SendFromStreamRange(Stream: TStream; Range: Integer): Boolean;
{$ELSE}

function TDXSock.SendFrom(Stream: TStream; Range: Integer): Boolean;
{$ENDIF}
var
  Len: Integer;
  SSize, SPosition: Integer;
  Tries: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SendFromRange');
  end;
{$ENDIF}
  fErrStatus := 0;
  SSize := Range;
  SPosition := Stream.Position;
  Tries := 0;
  while (sock <> Invalid_Socket) and
    (Stream.Position < Stream.Size) and
    (fErrStatus = 0) and
    (Tries < 3) do begin
    if (SSize - SPosition) < FActualBlockSize then
      Len := SSize - SPosition
    else
      Len := FActualBlockSize;
    if Len > 0 then begin
      Stream.Seek(SPosition, 0);
      Stream.Read(fChunkBuf^, Len);
{$IFDEF VER100}
      Len := BlockWrite(fChunkBuf, Len);
{$ELSE}
      Len := Write(fChunkBuf, Len);
{$ENDIF}
      SPosition := SPosition + Len;
      if fErrStatus > 0 then begin
        Tries := 3;
      end
      else if Len < 1 then
        Inc(Tries)
      else
        Tries := 0;
    end;
  end;
  Result := (Sock <> INVALID_SOCKET) and (fErrStatus = 0);
end;

{$IFDEF VER100}

function TDXSock.SendFromWindowsFile(var Handle: Integer): boolean;
{$ELSE}

function TDXSock.SendFrom(var Handle: Integer): boolean;
{$ENDIF}
var
  Len: Integer;
  SLen: Integer;
  Offset: Integer;
  FSize: Integer;
  Tries: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SendFrom');
  end;
{$ENDIF}
  Result := False;
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    Exit;
  if Handle <> 0 then begin
    Offset := FileSeek(Handle, 0, 1);
    FSize := FileSeek(Handle, 0, 2);
    FileSeek(Handle, Offset, 0);
    fErrStatus := 0;
    Tries := 0;
    while (sock <> Invalid_Socket) and
      (Offset < FSize) and
      (fErrStatus = 0) and
      (Tries < 3) do begin
      if Sock <> INVALID_SOCKET then begin
        Len := FileRead(Handle, fChunkBuf^, FActualBlockSize - 1);
        if Len > 0 then begin
{$IFDEF VER100}
          SLen := BlockWrite(fChunkBuf, Len);
{$ELSE}
          SLen := Write(fChunkBuf, Len);
{$ENDIF}
          if SLen <> Len then begin
            Offset := SLen + Offset;
            FileSeek(Handle, Offset, 0);
            Inc(Tries);
          end
          else
            Tries := 0;
          if fErrStatus > 0 then
            Tries := 3;
        end;
      end;
      Offset := FileSeek(Handle, 0, 1);
    end;
  end;
  Result := (Sock <> INVALID_SOCKET) and (fErrStatus = 0);
end;

{$IFDEF VER100}

function TDXSock.SendFromBorlandFile(var Handle: file): boolean;
{$ELSE}

function TDXSock.SendFrom(var Handle: file): boolean;
{$ENDIF}
var
  Len: Integer;
  SLen: Integer;
  OffSet: Integer;
  Tries: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SendFrom');
  end;
{$ENDIF}
  Result := False;
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    Exit;
  fErrStatus := 0;
  Tries := 0;
  while not Eof(Handle) and (fErrStatus = 0) and (Tries < 3) and (sock <> Invalid_Socket) do begin
    Offset := System.FilePos(Handle);
    if (Sock <> INVALID_SOCKET) then begin
      System.BlockRead(Handle, fChunkBuf^, FActualBlockSize - 1, Len);
{$IFDEF VER100}
      SLen := BlockWrite(fChunkBuf, Len);
{$ELSE}
      SLen := Write(fChunkBuf, Len);
{$ENDIF}
      if SLen = Len then begin
        Tries := 0;
      end
      else begin
        Offset := SLen + Offset;
        System.Seek(Handle, Offset);
        Inc(Tries);
      end;
      if fErrStatus > 0 then
        Tries := 3;
    end;
  end;
  Result := (Sock <> INVALID_SOCKET) and (fErrStatus = 0);
end;

{$IFDEF VER100}

function TDXSock.SaveToStream(Stream: TStream; Timeout: Longword): Boolean;
{$ELSE}

function TDXSock.SaveTo(Stream: TStream; Timeout: Longword): Boolean;
{$ENDIF}
var
  SLen: Integer;
  StartTime: Comp;
  OldSize: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SaveTo');
  end;
{$ENDIF}
  OldSize := Stream.Size;
  fErrStatus := 0;
  fReadTimeout := False;
  StartTime := TimeCounter + Timeout;
  while ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) and
    (not DXString.Timeout(StartTime)) do begin
{$IFDEF VER100}
    SLen := BlockRead(fChunkBuf, FActualBlockSize);
{$ELSE}
    SLen := Read(fChunkBuf, FActualBlockSize);
{$ENDIF}
    if SLen < 1 then begin
      if SLen = 0 then
        Break;
    end
    else
      Stream.Write(fChunkBuf^, SLen);
    if SLen < FActualBlockSize then
      Break; //GT for TLS Stops looping until timeout
  end;
  Result := (Sock <> INVALID_SOCKET) and ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK));
  if Result then
    Result := Stream.Size <> OldSize;
end;

{$IFDEF VER100}

function TDXSock.SaveToWindowsFile(var Handle: Integer; Timeout: Longword): boolean;
{$ELSE}

function TDXSock.SaveTo(var Handle: Integer; Timeout: Longword): boolean;
{$ENDIF}
var
  SLen: Integer;
{$IFDEF VER100}
  STmp: Integer;
{$ELSE}
  STmp: Cardinal;
{$ENDIF}
  StartTime: Comp;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SaveTo');
  end;
{$ENDIF}
  fErrStatus := 0;
  fReadTimeout := False;
  StartTime := TimeCounter + Timeout;
  while ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) and
    (not DXString.Timeout(StartTime)) do begin
{$IFDEF VER100}
    SLen := BlockRead(fChunkBuf, FActualBlockSize);
{$ELSE}
    SLen := Read(fChunkBuf, FActualBlockSize);
{$ENDIF}
    STmp := 0;
    if SLen < 1 then begin
      if SLen = 0 then
        Break;
    end
    else
      WindowsWriteFile(Handle, fChunkBuf^, SLen, STmp);
  end;
  Result := (Sock <> INVALID_SOCKET) and ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK));
end;

{$IFDEF VER100}

function TDXSock.SaveToBorlandFile(var Handle: file; Timeout: Longword): boolean;
{$ELSE}

function TDXSock.SaveTo(var Handle: file; Timeout: Longword): boolean;
{$ENDIF}
var
  SLen: Integer;
  StartTime: Comp;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SaveTo');
  end;
{$ENDIF}
  fErrStatus := 0;
  fReadTimeout := False;
  StartTime := TimeCounter + Timeout;
  while ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) and
    (not DXString.Timeout(StartTime)) do begin
{$IFDEF VER100}
    SLen := BlockRead(fChunkBuf, FActualBlockSize);
{$ELSE}
    SLen := Read(fChunkBuf, FActualBlockSize);
{$ENDIF}
    if SLen < 1 then begin
      if SLen = 0 then
        Break;
    end
    else
      System.BlockWrite(Handle, fChunkBuf^, SLen);
  end;
  Result := (Sock <> INVALID_SOCKET) and ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK));
end;

function TDXSock.WriteWithSize(S: string): Boolean;
var
  Size, OriginalSize: Integer;
  Ws: string;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WriteWithSize(' + S + ')');
  end;
{$ENDIF}
  Result := False;
  if S = '' then
    Exit;
  OriginalSize := Length(S);
  SetLength(Ws, OriginalSize + 4);
  Move(S[1], Ws[5], OriginalSize);
  size := htonl(OriginalSize);
  Move(Size, Ws[1], 4);
{$IFDEF VER100}
  Result := BlockWrite(@Ws[1], OriginalSize + 4) = OriginalSize + 4;
{$ELSE}
  Result := Write(@Ws[1], OriginalSize + 4) = OriginalSize + 4;
{$ENDIF}
end;

function TDXSock.ReadWithSize: string;
var
  Done: Boolean;
  Size: Integer;

begin
  Result := '';
  FErrStatus := 0;
   // redesigned for non-blocking mode and blocking mode and nagle on/off
  Done := False;
  while ((fErrStatus = 0) or (fErrStatus = WSAEWOULDBLOCK)) and not Done do begin
    Result := Result + GetChar; // ReadStr(4-Length(Result));
    Done := Length(Result) = 4;
  end;
  if not Done then
    Exit;
  Move(Result[1], Size, 4);
  size := ntohl(size);
  if (Size > fTooManyCharacters) or (Size < 1) then begin
//      ShowMessageWindow ('',HexDump (Result) +#13+
//         CleanStr (ReadStr (100) ) ) ;
    exit;
  end;
  Result := ReadStr(Size);
end;

function TDXSock.SendFromStreamWithSize(Stream: TStream): Boolean;
var
  Size: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SendFromStreamWithSize');
  end;
{$ENDIF}
  Result := False;
  Size := Stream.Size;
  if size < 1 then
    Exit;
  size := htonl(size);
  Stream.Seek(0, 0);
{$IFDEF VER100}
  if BlockWrite(@Size, 4) = 4 then
    Result := SendFromStream(Stream);
{$ELSE}
  if Write(@Size, 4) = 4 then
    Result := SendFrom(Stream);
{$ENDIF}
end;

function TDXSock.SaveToStreamWithSize(Stream: TStream; Timeout: Longword): Boolean;
var
  Size: Integer;
  StartTime: Comp;
  SLen: Integer;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SaveToStreamWithSize');
  end;
{$ENDIF}
  Stream.Size := 0;
  fReadTimeout := False;
{$IFDEF VER100}
  if BlockRead(@Size, 4) = 4 then begin
{$ELSE}
  if Read(@Size, 4) = 4 then begin
{$ENDIF}
    size := ntohl(size);
    StartTime := TimeCounter + Timeout;
    fErrStatus := 0;
    while ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) and
      (not DXString.Timeout(StartTime)) and
      (Size > 0) do begin
{$IFDEF VER100}
      SLen := BlockRead(fChunkBuf, Min(Size, FActualBlockSize));
{$ELSE}
      SLen := Read(fChunkBuf, Min(Size, FActualBlockSize));
{$ENDIF}
      case SLen of
        -1: begin // non-fatal
          end;
        0: Break; // fatal
      else begin
          Stream.Write(fChunkBuf^, SLen);
          Dec(Size, SLen);
        end;
      end;
    end;
  end;
  Result := (Sock <> INVALID_SOCKET) and ((fErrStatus = 0) or (fErrStatus = WSAETIMEDOUT) or (fErrStatus = WSAEWOULDBLOCK)) and
    ((Size = 0) and (Stream.Size > 0)); // 2.3c
end;

function TDXSock.PeekString: string;
var
  Size: Integer;
{$IFDEF TLS_EDITION}
  Filtered, InData: Pointer;
  Handled: Boolean;
  NewLen: Integer;
  SizeToRead: Integer;
  S: string;
  StartTime: Longword;
{$ENDIF}

begin
  Result := '';
{$IFDEF TLS_EDITION}
  indata := nil;
{$ENDIF}
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    exit;
{$IFDEF TLS_EDITION}
  if Assigned(feOnFilter) then begin
    SizeToRead := 0;
    StartTime := DxString.TimeCounter + 1000;
    while (SizeToRead = 0) and Connected and (not DXString.Timeout(StartTime)) do begin
      ioctlsocket(Sock, FIONREAD, Longint(SizeToRead));
      DoSleepEx(1);
    end;
    if SizeToRead = 0 then begin
      Result := '';
      Exit;
    end;

//      GetMem (InData,SizeToRead) ;
    InData := System.GetMemory(SizeToRead);

    if Sock <> Invalid_Socket then
      FErrStatus := Recv(Sock, Indata^, SizeToRead, 0)
    else
      FErrStatus := Socket_Error;
  end
  else
{$ENDIF}
    FErrStatus := BasicPeek(Sock, FPeekBuffer^, PeekBufferSize);
  if FErrStatus = Socket_Error then begin
    FErrStatus := 0;
    Exit;
  end
  else
    Size := FErrStatus;
{$IFDEF TLS_EDITION}
  if Assigned(feOnFilter) then begin
    Handled := False;
    Filtered := nil;
    feOnFilter(ddAfterRead, InData, Filtered, SizeToRead, NewLen, Handled, FClientThread);
    if not Handled then begin
      fErrStatus := 9999; {onFilter failed!}
      if Assigned(feOnFilter) then begin
        feOnFilter(ddFreePointer, nil, Filtered, NewLen, NewLen, Handled, FClientThread);
        if InData <> nil then begin
//               FreeMem (InData,SizeToRead) ;
          System.FreeMemory(InData);
          InData := nil;
        end;
      end;
      Exit;
    end;
    if Filtered <> nil then begin
      SetLength(S, NewLen);
      Move(TDXBSArray(Filtered^), S[1], NewLen);
      Result := S;
      fReadTimeout := False;
      FErrStatus := 0;
    end
    else
      Result := '';
    if Assigned(feOnFilter) then begin
      feOnFilter(ddFreePointer, nil, Filtered, NewLen, NewLen, Handled, FClientThread);
      if InData <> nil then begin
//            FreeMem (InData,SizeToRead) ;
        System.FreeMemory(InData);
        InData := nil;
      end;
    end;
  end
  else begin
{$ENDIF}
    Setlength(Result, Size);
    if Size > 0 then
      Move(FPeekBuffer^, Result[1], Size); // 3.0
{$IFDEF TLS_EDITION}
    fReadTimeout := False;
    FErrStatus := 0;
  end;
{$ENDIF}
end;

function TDXSock.PeekChar: Char;
begin
  Result := #0;
  fReadTimeout := False;
  if Sock = INVALID_SOCKET then
    exit;
  FErrStatus := BasicPeek(Sock, FPeekBuffer^, 1);
  case fErrStatus of
    0: begin
//         ShowMessageWindow('','PeekChar '+IntToStr(fErrStatus));
        CloseNow;
      end;
    Socket_Error: FErrStatus := 0;
  else
    Result := FPeekBuffer^[0];
  end;
end;

procedure TDXSock.CloseGracefully;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.CloseGraceFully');
  end;
{$ENDIF}
  CloseConnection(Sock, True);
end;

procedure TDXSock.Disconnect;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.Disconnect');
  end;
{$ENDIF}
  CloseConnection(Sock, True);
end;

procedure TDXSock.CloseNow;
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.CloseNow');
  end;
{$ENDIF}
  CloseConnection(Sock, False);
end;

function TDXSock.IsValidSocket: Boolean;
begin
  Result := Sock <> INVALID_SOCKET;
end;

function TDXSock.IsConnected: Boolean;
begin
  Result := (Sock <> INVALID_SOCKET)
    and ((FErrStatus = 0) or (FErrStatus = WSAETIMEDOUT) or
    (FErrStatus = WSAEWOULDBLOCK) or (fErrStatus = 10038));
  if not Result and (CountWaiting > 0) then
    Result := True;
{   If (fErrStatus<>0) and
      (fErrStatus<>WSAEWOULDBLOCK) and
      (fErrStatus<>WSAETIMEDOUT) and
      (fErrStatus<>10038) then ShowMessageWindow('IsConnected',IntToStr(fErrStatus));
   If not Result then Begin
      If Sock=INVALID_SOCKET then ShowMessageWindow('IsConnected','Invalid_Socket');
   End;}
end;

function TDXSock.IsReadable: Boolean;
begin
  fReadTimeout := False;
  Result := False;
  if Sock = INVALID_SOCKET then
    exit;
  Result := BasicSelect(Sock, True, GlobalTimeout) > 0;
//   SetTimeoutAndBuffer(Sock);
  fErrStatus := 0;
end;

function TDXSock.IsWritable: Boolean;
begin
  fReadTimeout := False;
  Result := False;
  if Sock = INVALID_SOCKET then
    exit;
  Result := BasicSelect(Sock, False, GlobalTimeout) > 0;
//   SetTimeoutAndBuffer(Sock);
  fErrStatus := 0;
end;

function TDXSock.DidReadTimeout: Boolean;
begin
  Result := fReadTimeout;
end;

function TDXSock.GetLocalPort: Integer;
begin
  Result := 0;
  if Sock = INVALID_SOCKET then
    exit;
  Result := DXSocket.GetLocalPort(Sock);
end;

function TDXSock.GetLocalIPAddr: string;
begin
  Result := '';
  if Sock = INVALID_SOCKET then
    exit;
  Result := DXSocket.GetLocalIPAddr(Sock);
end;

function TDXSock.GetErrorStr: string;
begin
  result := GetErrorDesc(GetLastError);
end;

procedure TDXSock.WinsockVersion(var WinsockInfo: PWinsockInfo);
begin
  if not Assigned(WinsockInfo) then
    Exit;
  if not SocketLayerLoaded then
    Exit;
  GetSocketVersion(WinsockInfo);
end;

procedure TDXSock.SetNagle(TurnOn: Boolean);
begin
  DXSocket.SetNagle(Sock, TurnOn, FErrStatus);
end;

procedure TDXSock.SetBlocking(TurnOn: Boolean);
begin
  fUseBlocking := TurnOn;
  DXSocket.SetBlocking(Sock, TurnOn, FErrStatus);
end;

function TDXSock.GetErrorDesc(errorCode: Integer): string;
begin
  Result := DXSocket.GetErrorDesc(ErrorCode);
end;

procedure TDXSock.SetfBlockSizeFlags(Value: TDXBlockSizeFlags);
begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.SetfBlockSizeFlags');
  end;
{$ENDIF}
  if Assigned(fChunkBuf) then
//      FreeMem (fChunkBuf,FActualBlockSize);
    System.FreeMemory(fChunkBuf);
  fChunkBuf := nil;
  fBlockSizeFlags := Value;
  case FBlockSizeFlags of
    bsfZero: fActualBlockSize := 0;
    bsfRealSmall: fActualBlockSize := 128;
    bsfSmall: fActualBlockSize := 256;
    bsfNormal: fActualBlockSize := 512;
    bsfBigger: fActualBlockSize := 2048;
    bsfBiggest: fActualBlockSize := 4096;
    bsfHUGE: fActualBlockSize := 32768;
  else
    fActualBlockSize := TDXHugeSize;
  end;
  if FBlockSizeFlags <> bsfZero then
//      GetMem (fChunkBuf,FActualBlockSize) ;
    fChunkBuf := System.GetMemory(FActualBlockSize);
end;

function TDXSOCK.CountWaiting: Integer;
begin
  Result := DXSocket.CountWaiting(Sock, FErrStatus);
  if FErrStatus <> 0 then begin
//------------------------------------------------------------------------------
// �׳��쳣 2008-2-14 ������
//------------------------------------------------------------------------------
//    raise Exception.Create('��� �ȴ�����ʱ����socket �ѶϿ����׳��쳣');

    Result := 0;
    Exit;
  end;
end;

function TDXSOCK.FilterRead(const InBuf: Pointer; var OutBuf: Pointer; InSize: Integer; xClientThread: TThread): Integer;
var
  Handled: Boolean;

begin
  if InSize > 0 then
    if Assigned(feOnFilter) then begin
      Handled := False;
      Result := 0;
      feOnFilter(ddAfterRead, InBuf, OutBuf, InSize, Result, Handled, xClientThread);
      if not Handled then begin
        fErrStatus := 9999; {onFilter failed!}
        Exit;
      end;
    end;
end;

// used by TDXSockClient only!

procedure TDXSock.SockClientSetGlobal(I: string; P: Integer);
begin
  GlobalPeerPort := P;
  GlobalPeerIPAddress := I;
end;

// new 3.0 features:

function TDXSock.DroppedConnection: Boolean;
begin
  Result := False;
  if IsReadable then
    if CharactersToRead = 0 then begin
      CloseNow; // invalidates the handle
      Result := True;
    end;
end;

function TDXSock.WaitForData(timeout: Longint): Boolean;
var
  StartTime: Comp;

begin
{$IFDEF CODE_TRACER}
  if Assigned(CodeTracer) then begin
    CodeTracer.SendMessage(dxctDebug, 'TDXSock.WaitForData');
  end;
{$ENDIF}
  Result := False;
  StartTime := TimeCounter + Cardinal(timeout);
  while not DXString.TimeOut(StartTime) do begin
    if DroppedConnection then begin
      CloseNow;
      Exit;
    end
    else begin
      if CharactersToRead > 0 then begin
        Result := True;
        Exit;
      end
      else begin
        ProcessWindowsMessageQueue;
        DoSleepEx(0);
      end;
    end;
  end;
end;

procedure TDXSock.RestartCharactersPerSecondTimer;
begin
  fCPSStart := Now;
  fTotalWBytes := 0;
  fTotalRBytes := 0;
end;

function TDXSock.CharactersPerSecondWritten: Integer;
var
  H1, M1, S1, MS1: Word;

begin
  try
    DecodeTime(Now - fCPSStart, H1, M1, S1, MS1);
    Result := fTotalWBytes div Max(((MS1 + (S1 * 1000) + (M1 * 3600000) + (H1 * 216000000)) div 1000), 1);
  except
    Result := 0;
  end;
end;

function TDXSock.CharactersPerSecondReceived: Integer;
var
  H1, M1, S1, MS1: Word;

begin
  try
    DecodeTime(Now - fCPSStart, H1, M1, S1, MS1);
    Result := fTotalRBytes div Max(((MS1 + (S1 * 1000) + (M1 * 3600000) + (H1 * 216000000)) div 1000), 1);
  except
    Result := 0;
  end;
end;

end.
