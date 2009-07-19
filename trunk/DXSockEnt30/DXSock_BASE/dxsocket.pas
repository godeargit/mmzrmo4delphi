{$I DXSock.def}
unit DXSocket;

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXSocket
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
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description: Low-Level Socket Wrapper, producing a common framework.
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

interface

{$IFDEF LINUX}
uses
   Libc;
{$ELSE}
uses
   Windows,
   Winsock;
{$ENDIF}

{$WARNINGS OFF}
{$IFDEF LINUX}
Const
   INVALID_SOCKET=Libc.INVALID_SOCKET;
   SO_KeepAlive=Libc.SO_KEEPALIVE;
   WSAEINTR=Libc.EINTR;
   WSAEBADF=Libc.EBADF;
   WSAEACCES=Libc.EACCES;
   WSAEFAULT=Libc.EFAULT;
   WSAEINVAL=Libc.EINVAL;
   WSAEMFILE=Libc.EMFILE;
   WSAEWOULDBLOCK=Libc.EWOULDBLOCK;
   WSAEINPROGRESS=Libc.EINPROGRESS;
   WSAEALREADY=Libc.EALREADY;
   WSAENOTSOCK=Libc.ENOTSOCK;
   WSAEDESTADDRREQ=Libc.EDESTADDRREQ;
   WSAEMSGSIZE=Libc.EMSGSIZE;
   WSAEPROTOTYPE=Libc.EPROTOTYPE;
   WSAENOPROTOOPT=Libc.ENOPROTOOPT;
   WSAEPROTONOSUPPORT=Libc.EPROTONOSUPPORT;
   WSAESOCKTNOSUPPORT=Libc.ESOCKTNOSUPPORT;
   WSAEOPNOTSUPP=Libc.EOPNOTSUPP;
   WSAEPFNOSUPPORT=Libc.EPFNOSUPPORT;
   WSAEAFNOSUPPORT=Libc.EAFNOSUPPORT;
   WSAEADDRINUSE=Libc.EADDRINUSE;
   WSAEADDRNOTAVAIL=Libc.EADDRNOTAVAIL;
   WSAENETDOWN=Libc.ENETDOWN;
   WSAENETUNREACH=Libc.ENETUNREACH;
   WSAENETRESET=Libc.ENETRESET;
   WSAECONNABORTED=Libc.ECONNABORTED;
   WSAECONNRESET=Libc.ECONNRESET;
   WSAENOBUFS=Libc.ENOBUFS;
   WSAEISCONN=Libc.EISCONN;
   WSAENOTCONN=Libc.ENOTCONN;
   WSAESHUTDOWN=Libc.ESHUTDOWN;
   WSAETOOMANYREFS=Libc.ETOOMANYREFS;
   WSAETIMEDOUT=Libc.ETIMEDOUT;
   WSAECONNREFUSED=Libc.ECONNREFUSED;
   WSAELOOP=Libc.ELOOP;
   WSAENAMETOOLONG=Libc.ENAMETOOLONG;
   WSAEHOSTDOWN=Libc.EHOSTDOWN;
   WSAEHOSTUNREACH=Libc.EHOSTUNREACH;
   WSAENOTEMPTY=Libc.ENOTEMPTY;
   WSAEPROCLIM=1000; // not applicable
   WSAEUSERS=Libc.EUSERS;
   WSAEDQUOT=Libc.EDQUOT;
   WSAESTALE=Libc.ESTALE;
   WSAEREMOTE=Libc.EREMOTE;
   WSASYSNOTREADY=1001; // not applicable
   WSAVERNOTSUPPORTED=1002; // not applicable
   WSANOTINITIALISED=1003; // not applicable
   WSAHOST_NOT_FOUND=Libc.HOST_NOT_FOUND;
   WSATRY_AGAIN=Libc.TRY_AGAIN;
   WSANO_RECOVERY=Libc.NO_RECOVERY;
   WSANO_DATA=Libc.ENODATA;
   WSAUNKNOWN=Libc.UNKNOWN_REQUEST;
   Socket_Error=Libc.SOCKET_ERROR;
{$ELSE}
Const
   INVALID_SOCKET=Winsock.INVALID_SOCKET;
   SO_KeepAlive=Winsock.SO_KEEPALIVE;
   WSAENOBUFS=Winsock.WSAENOBUFS;
   WSAETIMEDOUT=Winsock.WSAETIMEDOUT;
   WSAECONNABORTED=Winsock.WSAECONNABORTED;
   Socket_Error=Winsock.SOCKET_ERROR;
   // 7-27:
   WSAEWOULDBLOCK=Winsock.WSAEWOULDBLOCK;
   // 6-27:
   WSAECONNRESET=Winsock.WSAECONNRESET;
{$ENDIF}

{$IFDEF VER100}
type
  in_addr = TInAddr;
{$ENDIF}

Type
{$IFDEF LINUX}
   TSockAddrIn=Libc.TSockAddrin;
   TTimeVal=Libc.TTimeVal;
   TFDSet=Libc.TFDSet;
{$ELSE}
   TSockAddrIn=Winsock.TSockAddrIn;
   TTimeVal=Winsock.TTimeVal;
   TFDSet=Winsock.TFDSet;
{$ENDIF}
   PNewConnect=^TNewConnect;
   TNewConnect=Record
      Port:Integer;
      UseNAGLE:Boolean;
      UseUDP:Boolean;
      UseBlocking:Boolean;
      ipAddress:String;
   End;

   PNewListen=^TNewListen;
   TNewListen=Record
      Port:Integer;
      WinsockQueue:Integer;
      UseNAGLE:Boolean;
      UseUDP:Boolean;
      UseBlocking:Boolean;
      ConnectionLess:Boolean;
   End;

   PWinsockInfo=^TWinsockInfo; // 2.3 changed array from 0.. to 1..
   TWinsockInfo=Record
     Major_Version:Byte;                 {current version}
     Minor_Version:Byte;                 {current version}
     Highest_Major_Version:Byte;         {available on disk}
     Highest_Minor_Version:Byte;         {available on disk}
     Description:array[1..256] of Char;  // C++ Char Description[256];
     SystemStatus:array[1..128] of Char; // C++ Char Description[128];
     MaxSockets:Word;                    // C++ Unsigned short MaxSockets;
     MaxUDPDatagramSize:Word;            // C++ Unsigned short MaxUDPDatagramSize;
     VendorInfo:PChar;                   // C++ Char FAR * VendorInfo;
   End;

Const
   ConstSizeofTSockAddrIn=16;

Function CreateSocket(sin_family,socket_type,protocol:integer;
                      Var ErrorCode:Integer):TSocket;
Function ClientConnectToServer(ServerIPAddress:String;
                               ServerPort:Integer;
                               UseUDP,UseNAGLE:Boolean;
                               ResultSockAddr:PSockAddr;
                               Var ErrorCode:Integer):TSocket;
Function BindAndListen(BindToIPAddress:String;
                       BindToPort,WinsockQueue:Integer;
                       UseUDP,UseNAGLE,ConnectionLess:Boolean;
                       ResultSockAddr:PSockAddr;
                       Var ErrorCode:Integer):TSocket;
Function IsAcceptWaiting(ListenerSock:TSocket):Boolean;
Function AcceptNewConnect(ListenerSock:TSocket;
                          ResultAddr:PSockAddr;
                          ResultAddrlen:PInteger;
                          Var ErrorCode:Integer):TSocket;
Procedure CloseConnection(Var Sock:TSocket;
                          Gracefully:Boolean);
Function BasicSend(Sock:TSocket;
                   Var Buf;
                   Len:Integer;
                   Flags:Integer;
                   Var ErrorCode:Integer):Integer;
Function BasicRecv(Sock:TSocket;
                   Var Buf;
                   Len:Integer;
                   Flags:Integer;
                   Var ErrorCode:Integer):Integer;
Function UDPSend(Sock:TSocket;
                 Var Buf;
                 Len:Integer;
                 Flags:Integer;
                 SendTo:TSockAddr;
                 SendToSize:Integer;
                 Var ErrorCode:Integer):Integer;
Function UDPRecv(Sock:TSocket;
                 Var Buf;
                 Len:Integer;
                 Flags:Integer;
                 Var RcvFrom:TSockAddr;
                 Var RcvFromSize:Integer;
                 Var ErrorCode:Integer):Integer;
Function BasicPeek(Sock:TSocket;
                   Var Buf;
                   Len:Integer):Integer;
Function BasicSelect(Sock:TSocket;
                     CheckRead:Boolean;
                     Timeout:TTimeVal):Integer;
Procedure SetNagle(Sock:TSocket;
                   TurnOn:Boolean;
                   Var ErrorCode:Integer);
Procedure SetBlocking(Sock:TSocket;
                      UseBlocking:Boolean;
                      Var ErrorCode:Integer);
Procedure SetReceiveTimeout(Sock:TSocket;
                            TimeoutMS:Integer;
                            Var ErrorCode:Integer);
Procedure SetSendTimeout(Sock:TSocket;
                         TimeoutMS:Integer;
                         Var ErrorCode:Integer);
Procedure SetReceiveBuffer(Sock:TSocket;
                           WantedSize:Integer;
                           Var ErrorCode:Integer);
Procedure SetSendBuffer(Sock:TSocket;
                        WantedSize:Integer;
                        Var ErrorCode:Integer);
Procedure ResetBufferAndTimeout(Sock:TSocket;
                                TimeoutMS:Integer;
                                WantedSize:Integer);
Function GetReceiveBuffer(Sock:TSocket;
                          Var ErrorCode:Integer):Integer;
Function GetSendBuffer(Sock:TSocket;
                       Var ErrorCode:Integer):Integer;
Function GetSockStatusBool(Sock:TSocket;
                           SO_Flag:Integer;
                           Var ErrorCode:Integer):Boolean;
Function GetSockStatusInt(Sock:TSocket;
                          SO_Flag:Integer;
                          Var ErrorCode:Integer):Integer;
Procedure SetSockStatusBool(Sock:TSocket;
                           SO_Flag:Integer;
                           Setting:Boolean;
                           Var ErrorCode:Integer);
Procedure SetSockStatusInt(Sock:TSocket;
                          SO_Flag:Integer;
                          Setting:Integer;
                          Var ErrorCode:Integer);
Function CountWaiting(Sock:TSocket;
                      Var ErrorCode:Integer):Integer;
Function GetAddressCountByIP(IPAddress:String):Integer;
Function GetAddressCountByHost(Host:String):Integer;
Function GetIPAddressByHost(Host:String;Which:Integer):String;
Function GetHostByIPAddress(IPAddress:String):String;
Function GetLocalHostName:String;
function GetLocalPort(Sock:TSocket):Integer;
function GetLocalIPAddr(Sock:TSocket):string;
function GetLastError:Integer;
Function GetErrorDesc(ErrorCode:Integer):String;
function ByteSwap4(long:Cardinal):Cardinal;
function ByteSwap2(short:smallint):smallint;
Function IPIntToIPStr(IPAddr:Integer):String;
Function IPStrToIPInt(IPAddr:String):Integer;
Function SocketLayerLoaded:Boolean;
Procedure GetSocketVersion(WinsockInfo:PWinsockInfo);
Function ntohs(netshort:Word):Word;
Function inet_ntoa(inaddr:in_addr):PChar;
Function htonl(Hostlong:Integer):Integer;
Function ntohl(Netlong:Integer):Integer;
Function SetErrorCode(ResultCode:Integer):Integer;

Var
   GlobalTimeout:TTimeVal; //6-9

implementation

Uses
   DXString,
   SysUtils;

Var
{$IFNDEF LINUX}
   DLLData:TWSAData;
{$ENDIF}
   StartupResult:Integer;

Var
{$IFDEF LINUX}
   SizeOfInt:Cardinal=4; // optimize compiling
{$ELSE}
   SizeOfInt:Integer=4; // optimize compiling
{$ENDIF}

{$IFDEF ENGLISH1}
{$I Resource_English.inc}
{$ENDIF}
{$IFDEF FRENCH1}
{$I Resource_French.inc}
{$ENDIF}
{$IFDEF GERMAN1}
{$I Resource_German.inc}
{$ENDIF}
{$IFDEF ITALIAN1}
{$I Resource_Italian.inc}
{$ENDIF}
{$IFDEF LOWMEM1}
{$I Resource_LowMem.inc}
{$ENDIF}
{$IFDEF PORTUGUESE1}
{$I Resource_Portuguese.inc}
{$ENDIF}
{$IFDEF RUSSIAN1}
{$I Resource_Russian.inc}
{$ENDIF}
{$IFDEF SPANISH1}
{$I Resource_Spanish.inc}
{$ENDIF}
{$IFDEF TURKISH1}
{$I Resource_Turkish.inc}
{$ENDIF}

{$IFDEF LINUX}
Function WSAGetLastError:Integer;
Begin
   Result:=System.GetLastError
End;
{$ENDIF}

Function CreateSocket(sin_family,socket_type,protocol:integer;
                      Var ErrorCode:Integer):TSocket;
Begin
   ErrorCode:=0;
         Result:={$IFDEF LINUX}Libc.
                 {$ELSE}Winsock.
                 {$ENDIF}Socket(sin_family,socket_type,protocol);
         If Result=Invalid_Socket then ErrorCode:=WSAGetLastError;
End;

Function SetErrorCode(ResultCode:Integer):Integer;
Begin
   If ResultCode=Socket_Error then Result:=WSAGetLastError
   Else Result:=0;
End;

Procedure SetNagle(Sock:TSocket;
                   TurnOn:Boolean;
                   Var ErrorCode:Integer);
Var
   TA:Array[0..3] of Char;

Begin
   If Not TurnOn then TA:='1111'
   Else TA:='0000';
   ErrorCode:=SetErrorCode(SetSockOpt(Sock,IPPROTO_TCP,TCP_NODELAY,@TA,SizeofInt));
End;

Procedure SetBlocking(Sock:TSocket;
                      UseBlocking:Boolean;
                      Var ErrorCode:Integer);
{$IFDEF LINUX}
Const
   FIONBIO=$5421;
{$ENDIF}

Var
{$IFDEF VER90}
   iBlocking:u_long;
{$ELSE}
   iBlocking:Integer;
{$ENDIF}

Begin
   If UseBlocking then iBlocking:=0
   Else iBlocking:=1;
   ErrorCode:=SetErrorCode(
   {$IFDEF VER90}
      Winsock.ioctlsocket(Sock,FIONBIO,iBlocking)
   {$ELSE}
      {$IFDEF LINUX}Libc.ioctl(Sock,FIONBIO,iBlocking)
      {$ELSE}Winsock.ioctlsocket(Sock,FIONBIO,iBlocking)
      {$ENDIF}
   {$ENDIF}
   );
End;

Procedure SetReceiveTimeout(Sock:TSocket;
                            TimeoutMS:Integer;
                            Var ErrorCode:Integer);
Begin
   ErrorCode:=SetErrorCode(setsockopt(Sock,SOL_SOCKET,SO_RCVTIMEO,@TimeoutMS,SizeOfInt));
End;

Procedure SetSendTimeout(Sock:TSocket;
                         TimeoutMS:Integer;
                         Var ErrorCode:Integer);
Begin
   ErrorCode:=SetErrorCode(setsockopt(Sock,SOL_SOCKET,SO_SNDTIMEO,@TimeoutMS,SizeofInt));
End;

Procedure SetReceiveBuffer(Sock:TSocket;
                           WantedSize:Integer;
                           Var ErrorCode:Integer);
Begin
   ErrorCode:=SetErrorCode(setsockopt(Sock,SOL_SOCKET,SO_RCVBUF,@WantedSize,SizeofInt));
End;

Procedure ResetBufferAndTimeout(Sock:TSocket;
                                TimeoutMS:Integer;
                                WantedSize:Integer);
Begin
   setsockopt(Sock,SOL_SOCKET,SO_SNDTIMEO,@TimeoutMS,SizeofInt);
   setsockopt(Sock,SOL_SOCKET,SO_RCVTIMEO,@TimeoutMS,SizeOfInt);
   setsockopt(Sock,SOL_SOCKET,SO_RCVBUF,@WantedSize,SizeofInt);
   setsockopt(Sock,SOL_SOCKET,SO_SNDBUF,@WantedSize,SizeofInt);
End;

Function GetSockStatusBool(Sock:TSocket;
                           SO_Flag:Integer;
                           Var ErrorCode:Integer):Boolean;
Var
   Rslt:Boolean;

Begin
// 7-27
   ErrorCode:=SetErrorCode(GetSockOpt(Sock,SOL_SOCKET,SO_Flag,PChar(@Rslt),SizeofInt));
   If ErrorCode=0 then Result:=Rslt
   Else Result:=False;
End;

Function GetSockStatusInt(Sock:TSocket;
                          SO_Flag:Integer;
                          Var ErrorCode:Integer):Integer;
Var
   Rslt:Integer;

Begin
// 7-27
   ErrorCode:=SetErrorCode(GetSockOpt(Sock,SOL_SOCKET,SO_Flag,PChar(@Rslt),SizeofInt));
   If ErrorCode=0 then Result:=Rslt
   Else Result:=0;
End;

Procedure SetSockStatusBool(Sock:TSocket;
                           SO_Flag:Integer;
                           Setting:Boolean;
                           Var ErrorCode:Integer);
var
   intval:integer;

Begin
   if (Setting) then intval:=1
   else intval:=0;
   ErrorCode:=SetErrorCode(SetSockOpt(Sock,SOL_Socket,SO_Flag,@intval,SizeofInt));
End;

Procedure SetSockStatusInt(Sock:TSocket;
                          SO_Flag:Integer;
                          Setting:Integer;
                          Var ErrorCode:Integer);
Begin
   ErrorCode:=SetErrorCode(SetSockOpt(Sock,SOL_Socket,SO_Flag,@Setting,SizeofInt));
End;


Procedure SetSendBuffer(Sock:TSocket;
                        WantedSize:Integer;
                        Var ErrorCode:Integer);
Begin
   ErrorCode:=SetErrorCode(setsockopt(Sock,SOL_SOCKET,SO_SNDBUF,@WantedSize,SizeofInt));
End;

Function GetReceiveBuffer(Sock:TSocket;
                          Var ErrorCode:Integer):Integer;
Begin
   Result:=GetSockStatusInt(Sock,SO_RCVBUF,ErrorCode);
End;

Function GetSendBuffer(Sock:TSocket;
                       Var ErrorCode:Integer):Integer;
Begin
   Result:=GetSockStatusInt(Sock,SO_SNDBUF,ErrorCode);
End;

Procedure KillSocket(Var Sock:TSocket);
Begin
   If Sock<>Invalid_Socket then Begin
      ShutDown(Sock,2);
{$IFDEF LINUX}
      Libc.__close(Sock);
{$ELSE}
      CloseSocket(Sock);
{$ENDIF}
      Sock:=Invalid_Socket;
   End;
End;

Procedure CloseConnection(Var Sock:TSocket;
                          Gracefully:Boolean);
Var
{$IFDEF VER100} // Delphi3 code
   Lin:TLinger;
{$ELSE}
   Lin:Linger;
{$ENDIF}

Begin
   If Sock=Invalid_Socket then Exit;
   Lin.l_linger:=0;
   If Gracefully then Begin
      Lin.l_onoff:=1; // Not(0);
      {$IFDEF LINUX}Libc.
      {$ELSE}Winsock.
      {$ENDIF}setsockopt(Sock,SOL_SOCKET,SO_LINGER,@lin,Sizeof(Lin));
   End
   Else Begin
      Lin.l_onoff:=0;
      {$IFDEF LINUX}Libc.
      {$ELSE}Winsock.
      {$ENDIF}setsockopt(Sock,SOL_SOCKET,SO_LINGER,@lin,sizeof(lin)); {DONT 2.0.f}
   End;
   KillSocket(Sock);
End;

Function ClientConnectToServer(ServerIPAddress:String;
                               ServerPort:Integer;
                               UseUDP,UseNAGLE:Boolean;
                               ResultSockAddr:PSockAddr;
                               Var ErrorCode:Integer):TSocket;
{$IFDEF LINUX}
Const
   SOCK_dgram=2;
   SOCK_stream=1;
{$ENDIF}

begin
   Result:=Invalid_Socket;
   If ServerIPAddress='' then Exit;
   ServerIPAddress:=FixDottedIp(ServerIPAddress);
   FillChar(ResultSockAddr^,Sizeof(ResultSockAddr^),#0);
   ResultSockAddr.sin_family:=AF_INET;
   ResultSockAddr.sin_port:=htons(ServerPort);
   If IsNumericString(ServerIPAddress) then Begin
      ResultSockAddr.sin_addr.S_addr:=Inet_Addr(Pchar(ServerIPAddress));
   End
   Else begin
      ServerIPAddress:=GetIPAddressByHost(ServerIPAddress,1);
      If ServerIPAddress='' then Begin
         ErrorCode:=WSAEFAULT; // invalid address
         Exit;
      End;
      ResultSockAddr.sin_addr.S_addr:=Inet_Addr(Pchar(ServerIPAddress));
   End;
   Case UseUDP of
      True:Begin
         Result:=CreateSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,ErrorCode);
         Exit;
      End;
      Else Begin
         Result:=CreateSocket(AF_INET,SOCK_STREAM,IPPROTO_TCP,ErrorCode);
         If (Result<>Invalid_Socket) and (Not UseNAGLE) then
            SetNAGLE(Result,UseNAGLE,ErrorCode);
      End;
   End;
   If Result=Invalid_Socket then Exit;
   SetSendTimeout(Result,500,ErrorCode);
   If Connect(Result,ResultSockAddr^,ConstSizeofTSockAddrIn)=SOCKET_ERROR then begin
      ErrorCode:=WSAGetLastError;
      KillSocket(Result);
   End;
end;

Function BindAndListen(BindToIPAddress:String;
                       BindToPort,WinsockQueue:Integer;
                       UseUDP,UseNAGLE,ConnectionLess:Boolean;
                       ResultSockAddr:PSockAddr;
                       Var ErrorCode:Integer):TSocket;
{$IFDEF LINUX}
Const
   SOCK_dgram=2;
   SOCK_stream=1;
{$ENDIF}

begin
   FillChar(ResultSockAddr^,Sizeof(ResultSockAddr^),#0); // DO ! USE ZEROMEMORY
// SPX: Result:=CreateSocket(AF_IPX,SOCK_STREAM,NSPROTO_SPX,ErrorCode);
// IPX: Result:=CreateSocket(AF_IPX,SOCK_DGRAM,NSPROTO_IPX,ErrorCode);
   Case UseUDP of
      True:Result:=CreateSocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP,ErrorCode);
      Else Begin
         Result:=CreateSocket(AF_INET,SOCK_STREAM,IPPROTO_TCP,ErrorCode);
         If (Result<>Invalid_Socket) and (Not UseNAGLE) then SetNAGLE(Result,UseNAGLE,ErrorCode);
      End;
   End;
   If Result=Invalid_Socket then Exit;
   ResultSockAddr.sin_family:=AF_INET;
   ResultSockAddr.sin_port:=htons(BindToPORT);
// 7-27
   if (length(BindToIPAddress)<7) then ResultSockAddr.sin_addr.S_addr:=INADDR_ANY
   else ResultSockAddr.sin_addr.S_addr:=Inet_Addr(PChar(BindToIPAddress));
   If Bind(Result,ResultSockAddr^,ConstSizeofTSockAddrIn)=Socket_Error then Begin
      Result:=Invalid_Socket;
      ErrorCode:=WSAGetLastError;
      Exit;
   End;
// 7-27
   If Not ConnectionLess then
      If Listen(Result,WinsockQueue)=Socket_Error then Begin
         Result:=Invalid_Socket;
         ErrorCode:=WSAGetLastError;
      End;
End;

Function IsAcceptWaiting(ListenerSock:TSocket):Boolean;
{$IFNDEF LINUX}
Var
   SockList:TFDSet;
{$ENDIF}

Begin
{$IFDEF LINUX}
   Result:=BasicSelect(ListenerSock,True,GlobalTimeout)>0;
{$ELSE}
   SockList.fd_count:=1;
   SockList.fd_array[0]:=ListenerSock;
   Result:=Select(0,@sockList,nil,nil,@GlobalTimeout)>0;
{$ENDIF}
End;

Function AcceptNewConnect(ListenerSock:TSocket;
                          ResultAddr:PSockAddr;
                          ResultAddrlen:PInteger;
                          Var ErrorCode:Integer):TSocket;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}Accept(ListenerSock,
{$IFDEF VER90}
      ResultAddr^,ResultAddrLen^);
{$ELSE}
{$IFDEF LINUX}
      ResultAddr,PSocketLength(ResultAddrLen));
{$ELSE}
      ResultAddr,ResultAddrLen);
{$ENDIF}
{$ENDIF}
   If Result=Invalid_Socket then ErrorCode:=WSAGetLastError
   Else If ResultAddrLen^=0 then ErrorCode:=WSAEFault
   Else ErrorCode:=0;
End;

Function BasicSend(Sock:TSocket;
                   Var Buf;
                   Len:Integer;
                   Flags:Integer;
                   Var ErrorCode:Integer):Integer;
Begin
//   Result:=Socket_Error;
//   ErrorCode:=WSAEINTR;
//   While (Result<0) and ((ErrorCode=WSAEINTR) or (ErrorCode=WSAETIMEDOUT)) do Begin
       Result:=Send(Sock,Buf,Len,Flags);
       ErrorCode:=SetErrorCode(Result);
//   End;
End;

Function UDPSend(Sock:TSocket;
                 Var Buf;
                 Len:Integer;
                 Flags:Integer;
                 SendTo:TSockAddr;
                 SendToSize:Integer;
                 Var ErrorCode:Integer):Integer;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}SendTo(Sock,Buf,Len,Flags,SendTo,SendToSize);
   ErrorCode:=SetErrorCode(Result);
End;

Function BasicRecv(Sock:TSocket;
                   Var Buf;
                   Len:Integer;
                   Flags:Integer;
                   Var ErrorCode:Integer):Integer;
Begin
      Result:=Recv(Sock,Buf,Len,Flags);
      ErrorCode:=SetErrorCode(Result);
End;

Function UDPRecv(Sock:TSocket;
                 Var Buf;
                 Len:Integer;
                 Flags:Integer;
                 Var RcvFrom:TSockAddr;
                 Var RcvFromSize:Integer;
                 Var ErrorCode:Integer):Integer;
Begin
      Result:={$IFDEF LINUX}Libc.recvfrom(Sock,Buf,Len,Flags,@RcvFrom,@RcvFromSize);
              {$ELSE}Winsock.recvfrom(Sock,Buf,Len,Flags,RcvFrom,RcvFromSize);
              {$ENDIF}
      ErrorCode:=SetErrorCode(Result);
End;

Function BasicPeek(Sock:TSocket;
                   Var Buf;
                   Len:Integer):Integer;
Begin
      Result:=Recv(Sock, Buf, Len, MSG_PEEK);
End;

Function BasicSelect(Sock:TSocket;
                     CheckRead:Boolean;
                     Timeout:TTimeVal):Integer;
var
  SockList: TFDSet;

Begin
{$IFDEF LINUX}
   FD_ZERO(SockList);
   SockList.fds_bits[0]:=Sock;
   If CheckRead then
      Result:=Select(1,@SockList,nil,nil,@Timeout)
   Else
      Result:=Select(1,nil,@SockList,nil,@Timeout);
{$ELSE}
   SockList.fd_count:=1;
   SockList.fd_array[0]:=Sock;
   If CheckRead then
     Result:=Select(0,@sockList,nil,nil,@Timeout)
   Else
     Result:=Select(0,nil,@sockList,nil,@Timeout)
{$ENDIF}
End;

Function CountWaiting(Sock:TSocket;Var ErrorCode:Integer):Integer;
{$IFDEF LINUX}
Const
   FIONREAD=$541B;
{$ENDIF}
var
   numWaiting:longint;

begin
   Result:=0;
// in linux IOCtl is used to "set" not "get" values.
   ErrorCode:=SetErrorCode({$IFDEF LINUX}Libc.IOCtl(Sock,FIONREAD,numWaiting));
                           {$ELSE}Winsock.IOCtlSocket(Sock,FIONREAD,numWaiting));
                           {$ENDIF}
   If ErrorCode=0 then Result:=numWaiting;
end;

Function GetAddressCountByIP(IPAddress:String):Integer;
Var
   HostEnt:PHostEnt;
   InAddr:u_long;

Begin
   IPAddress:=FixDottedIp(IPAddress);
   InAddr:=inet_addr(PChar(IPAddress));
   HostEnt:=gethostbyaddr(@InAddr,Length(IPAddress),AF_INET);
   If Assigned(HostEnt) then Result:=HostEnt^.h_length div 4
   Else Result:=0;
End;

Function GetAddressCountByHost(Host:String):Integer;
Var
   HostEnt:PHostEnt;

Begin
   HostEnt:=gethostbyname(PChar(Host));
   If Assigned(HostEnt) then Result:=HostEnt^.h_length div 4
   Else Result:=0;
End;

Function GetIPAddressByHost(Host:String;Which:Integer):String;
Var
   HostEnt:PHostEnt;
   iAddr:Integer;

Begin
   HostEnt:=gethostbyname(PChar(Host));
   If Assigned(HostEnt) then Begin
      If Which<=(HostEnt^.h_length div 4) then Begin
         Move(PByteArray(HostEnt^.h_addr_list^)[(Which-1)*4],iAddr,4);
         Result:=inet_ntoa(in_Addr(iAddr));
      End
      Else Result:='';
   End
   Else Result:='';
End;

Function GetHostByIPAddress(IPAddress:String):String;
Var
   HostEnt:PHostEnt;
   InAddr:u_long;

Begin
   IPAddress:=FixDottedIp(IPAddress);
   InAddr:=inet_addr(PChar(IPAddress));
   HostEnt:=gethostbyaddr(@InAddr,Length(IPAddress),AF_INET);
   If Assigned(HostEnt) then Result:=StrPas(HostEnt^.h_name)
   Else Result:='';
End;

Function GetLocalHostName:String;
Begin
   Result:=GetHostByIPAddress(
      GetIPAddressByHost('localhost',1));
   If Result='' then Result:='Localhost';
End;

function GetLocalPort(Sock:TSocket):Integer;
var
  addr: TSockAddrIn;
{$IFDEF LINUX}
  addrlen: cardinal;
{$ELSE}
  addrlen: integer;
{$ENDIF}

begin
  addrlen:=ConstSizeofTSockAddrIn;
  if getsockname(Sock,addr,addrlen)=0 then Result:=ntohs(addr.sin_port)
  else Result := 0;
end;

function GetLocalIPAddr(Sock:TSocket):string;
var
  addr: TSockAddrIn;
{$IFDEF LINUX}
  addrlen: cardinal;
{$ELSE}
  addrlen: integer;
{$ENDIF}

begin
  addrlen:=ConstSizeofTSockAddrIn;
  FillChar(Addr,Sizeof(TSockAddrIn),#0);
  getsockname(Sock,addr,addrlen);
  Result:=inet_ntoa(addr.sin_addr);
end;

Procedure GetRemoteSockAddr(Sock:TSocket;
                            ResultAddr:PSockAddr;
                            ResultAddrlen:PInteger;
                            Var ErrorCode:Integer);
{$IFDEF LINUX}
Var
   TmpAddrLen:Cardinal;
{$ENDIF}

Begin
{$IFDEF LINUX}
   ErrorCode:=SetErrorCode(getpeername(Sock,ResultAddr^,TmpAddrlen));
   ResultAddrLen^:=TmpAddrLen;
{$ELSE}
   ErrorCode:=SetErrorCode(getpeername(Sock,ResultAddr^,ResultAddrlen^));
{$ENDIF}
End;

function GetLastError:Integer;
Begin
   Result:=WSAGetLastError;
End;

Function GetErrorDesc(errorCode:Integer):String;
begin
// If you compile and get "Undeclared Identified -
// Edit DXSock.DEF - and select a language!
   case errorCode of
      WSAEINTR:Result:=_WSAEINTR;
      WSAEBADF:Result:=_WSAEBADF;
      WSAEACCES:Result:=_WSAEACCES;
      WSAEFAULT:Result:=_WSAEFAULT;
      WSAEINVAL:Result:=_WSAEINVAL;
      WSAEMFILE:Result:=_WSAEMFILE;
      WSAEWOULDBLOCK:Result:=_WSAEWOULDBLOCK;
      WSAEINPROGRESS:Result:=_WSAEINPROGRESS;
      WSAEALREADY:Result:=_WSAEALREADY;
      WSAENOTSOCK:Result:=_WSAENOTSOCK;
      WSAEDESTADDRREQ:Result:=_WSAEDESTADDRREQ;
      WSAEMSGSIZE:Result:=_WSAEMSGSIZE;
      WSAEPROTOTYPE:Result:=_WSAEPROTOTYPE;
      WSAENOPROTOOPT:Result:=_WSAENOPROTOOPT;
      WSAEPROTONOSUPPORT:Result:=_WSAEPROTONOSUPPORT;
      WSAESOCKTNOSUPPORT:Result:=_WSAESOCKTNOSUPPORT;
      WSAEOPNOTSUPP:Result:=_WSAEOPNOTSUPP;
      WSAEPFNOSUPPORT:Result:=_WSAEPFNOSUPPORT;
      WSAEAFNOSUPPORT:Result:=_WSAEAFNOSUPPORT;
      WSAEADDRINUSE:Result:=_WSAEADDRINUSE;
      WSAEADDRNOTAVAIL:Result:=_WSAEADDRNOTAVAIL;
      WSAENETDOWN:Result:=_WSAENETDOWN;
      WSAENETUNREACH:Result:=_WSAENETUNREACH;
      WSAENETRESET:Result:=_WSAENETRESET;
      WSAECONNABORTED:Result:=_WSAECONNABORTED;
      WSAECONNRESET:Result:=_WSAECONNRESET;
      WSAENOBUFS:Result:=_WSAENOBUFS;
      WSAEISCONN:Result:=_WSAEISCONN;
      WSAENOTCONN:Result:=_WSAENOTCONN;
      WSAESHUTDOWN:Result:=_WSAESHUTDOWN;
      WSAETOOMANYREFS:Result:=_WSAETOOMANYREFS;
      WSAETIMEDOUT:Result:=_WSAETIMEDOUT;
      WSAECONNREFUSED:Result:=_WSAECONNREFUSED;
      WSAELOOP:Result:=_WSAELOOP;
      WSAENAMETOOLONG:Result:=_WSAENAMETOOLONG;
      WSAEHOSTDOWN:Result:=_WSAEHOSTDOWN;
      WSAEHOSTUNREACH:Result:=_WSAEHOSTUNREACH;
      WSAENOTEMPTY:Result:=_WSAENOTEMPTY;
      WSAEPROCLIM:Result:=_WSAEPROCLIM;
      WSAEUSERS:Result:=_WSAEUSERS;
      WSAEDQUOT:Result:=_WSAEDQUOT;
      WSAESTALE:Result:=_WSAESTALE;
      WSAEREMOTE:Result:=_WSAEREMOTE;
      WSASYSNOTREADY:Result:=_WSASYSNOTREADY;
      WSAVERNOTSUPPORTED:Result:=_WSAVERNOTSUPPORTED;
      WSANOTINITIALISED:Result:=_WSANOTINITIALISED;
      WSAHOST_NOT_FOUND:Result:=_WSAHOST_NOT_FOUND;
      WSATRY_AGAIN:Result:=_WSATRY_AGAIN;
      WSANO_RECOVERY:Result:=_WSANO_RECOVERY;
      WSANO_DATA:Result:=_WSANO_DATA;
      Else Result:=_WSAUNKNOWN+' ('+IntToCommaStr(ErrorCode)+')';
   end;
end;

function ByteSwap4(long:Cardinal):Cardinal;
begin
   result:=ntohl(long);
end;

function ByteSwap2(short:smallint):smallint;
begin
   result:=ntohs(short);
end;

Function IPIntToIPStr(IPAddr:Integer):String;
Var
   Ws:String;

Begin
   Setlength(Ws,4);
   Move(IPAddr,Ws[1],4);
   Result:=IntToStr(Ord(Ws[1]))+'.'+
      IntToStr(Ord(Ws[2]))+'.'+
      IntToStr(Ord(Ws[3]))+'.'+
      IntToStr(Ord(Ws[4]));
End;

Function IPStrToIPInt(IPAddr:String):Integer;
Var
   Ws:String;

Begin
   Setlength(Ws,4);
   Ws[1]:=Char(StrToInt(FetchByChar(IPAddr,'.',False)));
   Ws[2]:=Char(StrToInt(FetchByChar(IPAddr,'.',False)));
   Ws[3]:=Char(StrToInt(FetchByChar(IPAddr,'.',False)));
   Ws[4]:=Char(StrToInt(FetchByChar(IPAddr,'.',False)));
   Move(Ws[1],Result,4);
End;

Function SocketLayerLoaded:Boolean;
Begin
   Result:=(StartupResult=999);
End;

Procedure GetSocketVersion(WinsockInfo:PWinsockInfo);
Begin
{$IFDEF LINUX}
   With WinsockInfo^ do Begin
      Major_Version:=2;
      Minor_Version:=0;
      Highest_Major_Version:=2;
      Highest_Minor_Version:=0;
      Move('Linux Socket Layer 2.0',Description,256);
      Move('Ready',SystemStatus,128);
      MaxSockets:=65000;
      MaxUDPDatagramSize:=1500;
      VendorInfo:='Brain Patchwork DX, LLC.';
   End;
{$ELSE}
   With WinsockInfo^ do Begin
      Major_Version:=BYTE(DllData.wVersion);
      Minor_Version:=HIBYTEOfWORD(DllData.wVersion);
      Highest_Major_Version:=BYTE(DllData.wHighVersion);
      Highest_Minor_Version:=HIBYTEOfWORD(DllData.wHighVersion);
      Move(DllData.szDescription,Description,256);
      Move(DllData.szSystemStatus,SystemStatus,128);
      MaxSockets:=DllData.iMaxSockets;
      MaxUDPDatagramSize:=DllData.iMaxUdpDg;
      VendorInfo:=DllData.lpVendorInfo;
   End;
{$ENDIF}
End;

Function ntohs(netshort:Word):Word;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}ntohs(Netshort);
End;

Function inet_ntoa(inaddr:in_addr):PChar;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}inet_ntoa(inaddr);
End;

Function htonl(Hostlong:Integer):Integer;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}htonl(Hostlong);
End;

Function ntohl(Netlong:Integer):Integer;
Begin
   Result:={$IFDEF LINUX}Libc.
           {$ELSE}Winsock.
           {$ENDIF}ntohl(netlong)
End;

initialization
{$IFDEF LINUX}
   StartupResult:=0;
{$ELSE}
   StartupResult:=WSAStartup(MAKEBytesToWORD(2,2),DLLData);
{$ENDIF}
   if StartupResult=0 then Begin
      StartupResult:=999;
      // 6-9: added to load 1 time.
      GlobalTimeout.tv_Sec:=0;
      GlobalTimeout.tv_uSec:=500; //2500;
   End
   else StartupResult:=123;

finalization
{$IFNDEF LINUX}
   If StartupResult=999 then WSACleanup;
{$ENDIF}

End.

