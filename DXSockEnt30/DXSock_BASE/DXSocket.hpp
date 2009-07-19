// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSocket.pas' rev: 5.00

#ifndef DXSocketHPP
#define DXSocketHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <WinSock.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsocket
{
//-- type declarations -------------------------------------------------------
typedef sockaddr_in  TSockAddrIn;

typedef timeval  TTimeVal;

typedef Winsock::TFDSet  TFDSet;

struct TNewConnect;
typedef TNewConnect *PNewConnect;

struct TNewConnect
{
	int Port;
	bool UseNAGLE;
	bool UseUDP;
	bool UseBlocking;
	AnsiString ipAddress;
} ;

struct TNewListen;
typedef TNewListen *PNewListen;

struct TNewListen
{
	int Port;
	int WinsockQueue;
	bool UseNAGLE;
	bool UseUDP;
	bool UseBlocking;
	bool ConnectionLess;
} ;

struct TWinsockInfo;
typedef TWinsockInfo *PWinsockInfo;

struct TWinsockInfo
{
	Byte Major_Version;
	Byte Minor_Version;
	Byte Highest_Major_Version;
	Byte Highest_Minor_Version;
	char Description[256];
	char SystemStatus[128];
	Word MaxSockets;
	Word MaxUDPDatagramSize;
	char *VendorInfo;
} ;

//-- var, const, procedure ---------------------------------------------------
static const int INVALID_SOCKET = 0xffffffff;
static const Shortint SO_KeepAlive = 0x8;
static const Word WSAENOBUFS = 0x2747;
static const Word WSAETIMEDOUT = 0x274c;
static const Word WSAECONNABORTED = 0x2745;
static const Shortint Socket_Error = 0xffffffff;
static const Word WSAEWOULDBLOCK = 0x2733;
static const Shortint ConstSizeofTSockAddrIn = 0x10;
extern PACKAGE int __fastcall CreateSocket(int sin_family, int socket_type, int protocol, int &ErrorCode
	);
extern PACKAGE int __fastcall SetErrorCode(int ResultCode);
extern PACKAGE void __fastcall SetNagle(int Sock, bool TurnOn, int &ErrorCode);
extern PACKAGE void __fastcall SetBlocking(int Sock, bool UseBlocking, int &ErrorCode);
extern PACKAGE void __fastcall SetReceiveTimeout(int Sock, int TimeoutMS, int &ErrorCode);
extern PACKAGE void __fastcall SetSendTimeout(int Sock, int TimeoutMS, int &ErrorCode);
extern PACKAGE void __fastcall SetReceiveBuffer(int Sock, int WantedSize, int &ErrorCode);
extern PACKAGE bool __fastcall GetSockStatusBool(int Sock, int SO_Flag, int &ErrorCode);
extern PACKAGE int __fastcall GetSockStatusInt(int Sock, int SO_Flag, int &ErrorCode);
extern PACKAGE void __fastcall SetSockStatusBool(int Sock, int SO_Flag, bool Setting, int &ErrorCode
	);
extern PACKAGE void __fastcall SetSockStatusInt(int Sock, int SO_Flag, int Setting, int &ErrorCode);
	
extern PACKAGE void __fastcall SetSendBuffer(int Sock, int WantedSize, int &ErrorCode);
extern PACKAGE int __fastcall GetReceiveBuffer(int Sock, int &ErrorCode);
extern PACKAGE int __fastcall GetSendBuffer(int Sock, int &ErrorCode);
extern PACKAGE void __fastcall CloseConnection(int &Sock, bool Gracefully);
extern PACKAGE int __fastcall ClientConnectToServer(AnsiString ServerIPAddress, int ServerPort, bool 
	UseUDP, bool UseNAGLE, PSOCKADDR ResultSockAddr, int &ErrorCode);
extern PACKAGE int __fastcall BindAndListen(AnsiString BindToIPAddress, int BindToPort, int WinsockQueue
	, bool UseUDP, bool UseNAGLE, bool ConnectionLess, PSOCKADDR ResultSockAddr, int &ErrorCode);
extern PACKAGE bool __fastcall IsAcceptWaiting(int ListenerSock);
extern PACKAGE int __fastcall AcceptNewConnect(int ListenerSock, PSOCKADDR ResultAddr, Windows::PInteger 
	ResultAddrlen, int &ErrorCode);
extern PACKAGE int __fastcall BasicSend(int Sock, void *Buf, int Len, int Flags, int &ErrorCode);
extern PACKAGE int __fastcall UDPSend(int Sock, void *Buf, int Len, int Flags, const sockaddr_in &SendTo
	, int SendToSize, int &ErrorCode);
extern PACKAGE int __fastcall BasicRecv(int Sock, void *Buf, int Len, int Flags, int &ErrorCode);
extern PACKAGE int __fastcall UDPRecv(int Sock, void *Buf, int Len, int Flags, sockaddr_in &RcvFrom, 
	int &RcvFromSize, int &ErrorCode);
extern PACKAGE int __fastcall BasicPeek(int Sock, void *Buf, int Len);
extern PACKAGE int __fastcall BasicSelect(int Sock, bool CheckRead, const timeval &Timeout);
extern PACKAGE int __fastcall CountWaiting(int Sock, int &ErrorCode);
extern PACKAGE int __fastcall GetAddressCountByIP(AnsiString IPAddress);
extern PACKAGE int __fastcall GetAddressCountByHost(AnsiString Host);
extern PACKAGE AnsiString __fastcall GetIPAddressByHost(AnsiString Host, int Which);
extern PACKAGE AnsiString __fastcall GetHostByIPAddress(AnsiString IPAddress);
extern PACKAGE AnsiString __fastcall GetLocalHostName();
extern PACKAGE int __fastcall GetLocalPort(int Sock);
extern PACKAGE AnsiString __fastcall GetLocalIPAddr(int Sock);
extern PACKAGE int __fastcall GetLastError(void);
extern PACKAGE AnsiString __fastcall GetErrorDesc(int ErrorCode);
extern PACKAGE unsigned __fastcall ByteSwap4(unsigned long);
extern PACKAGE short __fastcall ByteSwap2(short short);
extern PACKAGE AnsiString __fastcall IPIntToIPStr(int IPAddr);
extern PACKAGE int __fastcall IPStrToIPInt(AnsiString IPAddr);
extern PACKAGE bool __fastcall SocketLayerLoaded(void);
extern PACKAGE void __fastcall GetSocketVersion(PWinsockInfo WinsockInfo);
extern PACKAGE Word __fastcall ntohs(Word netshort);
extern PACKAGE char * __fastcall inet_ntoa(in_addr inaddr);
extern PACKAGE int __fastcall htonl(int Hostlong);
extern PACKAGE int __fastcall ntohl(int Netlong);

}	/* namespace Dxsocket */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsocket;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSocket
