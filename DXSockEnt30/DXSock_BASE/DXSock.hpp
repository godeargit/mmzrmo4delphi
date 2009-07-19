// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSock.pas' rev: 5.00

#ifndef DXSockHPP
#define DXSockHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <DXSocket.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsock
{
//-- type declarations -------------------------------------------------------
typedef SmallString<1>  Str1;

#pragma option push -b-
enum TDXDataDirection { ddAboutToWrite, ddAfterRead, ddCleanRead, ddFreePointer };
#pragma option pop

typedef void __fastcall (__closure *TDXFilterCallBack)(TDXDataDirection DataDirection, const void * 
	InData, void * &OutData, const int InSize, int &OutSize, bool &Handled, Classes::TThread* xClientThread
	);

#pragma option push -b-
enum TDXBlockSizeFlags { bsfZero, bsfRealSmall, bsfSmall, bsfNormal, bsfBigger, bsfBiggest, bsfHUGE 
	};
#pragma option pop

typedef char TDXBSArray[65501];

typedef char TDXBSArray2[251];

class DELPHICLASS TDXSock;
class PASCALIMPLEMENTATION TDXSock : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TThread* FClientThread;
	bool FTLS;
	void *fChunkBuf;
	bool fbClientMode;
	bool fbIsUDP;
	bool fbIsKeepAlive;
	AnsiString FsBindTo;
	char *FPeekBuffer;
	bool FReadTimeout;
	bool FUseBlocking;
	TDXBlockSizeFlags FBlockSizeFlags;
	int FActualBlockSize;
	int FErrStatus;
	int fTooManyCharacters;
	TDXFilterCallBack feOnFilter;
	int GlobalPeerPort;
	AnsiString GlobalPeerIPAddress;
	timeval GlobalTimeout;
	int VarConstSizeofTSockAddrIn;
	unsigned fTotalWBytes;
	unsigned fTotalRBytes;
	System::TDateTime fCPSStart;
	
protected:
	AnsiString __fastcall GetReleaseDate();
	void __fastcall SetReleaseDate(AnsiString value);
	int __fastcall GetLocalPort(void);
	AnsiString __fastcall GetLocalIPAddr();
	bool __fastcall IsConnected(void);
	bool __fastcall IsValidSocket(void);
	bool __fastcall IsReadable(void);
	bool __fastcall IsWritable(void);
	bool __fastcall DidReadTimeout(void);
	void __fastcall SetfBlockSizeFlags(TDXBlockSizeFlags Value);
	int __fastcall CountWaiting(void);
	
public:
	sockaddr_in SockAddr;
	int Sock;
	__fastcall TDXSock(void);
	__fastcall virtual ~TDXSock(void);
	bool __fastcall Connect(Dxsocket::PNewConnect Parameters);
	bool __fastcall Listen(Dxsocket::PNewListen Parameters);
	bool __fastcall Accept(TDXSock* &NewSock);
	void __fastcall CloseGracefully(void);
	void __fastcall Disconnect(void);
	void __fastcall CloseNow(void);
	int __fastcall SendBuf(const void *Buf, int Count);
	int __fastcall ReceiveBuf(void *Buf, int Count);
	int __fastcall Write(char c)/* overload */;
	int __fastcall Write(const AnsiString s)/* overload */;
	int __fastcall Write(void * buf, int len)/* overload */;
	int __fastcall WriteLn(const AnsiString s);
	int __fastcall WriteResultCode(const int Code, const AnsiString Rslt);
	bool __fastcall WriteWithSize(AnsiString S);
	int __fastcall WriteInteger(const int n);
	bool __fastcall SendFrom(Classes::TStream* Stream)/* overload */;
	bool __fastcall SendFrom(int &Handle)/* overload */;
	bool __fastcall SendFrom(file &Handle)/* overload */;
	bool __fastcall SendFromStreamWithSize(Classes::TStream* Stream);
	int __fastcall Read(void * buf, int len)/* overload */;
	char __fastcall Read(void)/* overload */;
	int __fastcall ReadInteger(void);
	AnsiString __fastcall ReadStr(int MaxLength);
	AnsiString __fastcall ReadString(int MaxLength, unsigned Timeout);
	AnsiString __fastcall ReadLn(unsigned Timeout);
	AnsiString __fastcall ReadCRLF(unsigned Timeout);
	AnsiString __fastcall ReadToAnyDelimiter(unsigned Timeout, AnsiString Delimiter);
	AnsiString __fastcall ReadNull(unsigned Timeout);
	AnsiString __fastcall ReadSpace(unsigned Timeout);
	AnsiString __fastcall ReadWithSize();
	bool __fastcall SaveTo(Classes::TStream* Stream, unsigned Timeout)/* overload */;
	bool __fastcall SaveTo(int &Handle, unsigned Timeout)/* overload */;
	bool __fastcall SaveTo(file &Handle, unsigned Timeout)/* overload */;
	bool __fastcall SaveToStreamWithSize(Classes::TStream* Stream, unsigned Timeout);
	Str1 __fastcall GetChar();
	Byte __fastcall GetByte(void);
	int __fastcall FilterRead(const void * InBuf, void * &OutBuf, int InSize, Classes::TThread* xClientThread
		);
	AnsiString __fastcall PeekString();
	char __fastcall PeekChar(void);
	AnsiString __fastcall GetErrorStr();
	AnsiString __fastcall GetErrorDesc(int errorCode);
	void __fastcall SetNagle(bool TurnOn);
	void __fastcall SetBlocking(bool TurnOn);
	void __fastcall WinsockVersion(Dxsocket::PWinsockInfo &WinsockInfo);
	void __fastcall SockClientSetGlobal(AnsiString I, int P);
	void __fastcall SetTimeoutAndBuffer(int SocketHandle);
	bool __fastcall DroppedConnection(void);
	bool __fastcall WaitForData(int timeout);
	void __fastcall RestartCharactersPerSecondTimer(void);
	int __fastcall CharactersPerSecondWritten(void);
	int __fastcall CharactersPerSecondReceived(void);
	
__published:
	__property bool TLSActive = {read=FTLS, write=FTLS, nodefault};
	__property Classes::TThread* TLSClientThread = {read=FClientThread, write=FClientThread};
	__property AnsiString BindTo = {read=FsBindTo, write=FsBindTo};
	__property bool Connected = {read=IsConnected, nodefault};
	__property int CharactersToRead = {read=CountWaiting, nodefault};
	__property int ReceiveLength = {read=CountWaiting, nodefault};
	__property bool ValidSocket = {read=IsValidSocket, nodefault};
	__property bool LastReadTimeout = {read=DidReadTimeout, nodefault};
	__property int LastCommandStatus = {read=FErrStatus, write=FErrStatus, nodefault};
	__property TDXBlockSizeFlags OutputBufferSize = {read=FBlockSizeFlags, write=SetfBlockSizeFlags, nodefault
		};
	__property int TooManyCharacters = {read=fTooManyCharacters, write=fTooManyCharacters, nodefault};
	__property bool IsUDPMode = {read=fbIsUDP, write=fbIsUDP, nodefault};
	__property bool IsKeepAliveMode = {read=fbIsKeepAlive, write=fbIsKeepAlive, nodefault};
	__property AnsiString PeerIPAddress = {read=GlobalPeerIPAddress, write=GlobalPeerIPAddress};
	__property int PeerPort = {read=GlobalPeerPort, write=GlobalPeerPort, nodefault};
	__property AnsiString LocalIPAddress = {read=GetLocalIPAddr};
	__property int LocalPort = {read=GetLocalPort, nodefault};
	__property bool Readable = {read=IsReadable, nodefault};
	__property bool Writable = {read=IsWritable, nodefault};
	__property AnsiString ReleaseDate = {read=GetReleaseDate, write=SetReleaseDate};
	__property TDXFilterCallBack OnFilter = {read=feOnFilter, write=feOnFilter};
};


//-- var, const, procedure ---------------------------------------------------
static const Word TDXHugeSize = 0x4000;
extern PACKAGE Word TDXXferTimeout;
extern PACKAGE Word TDXMaxSocketBuffer;
extern PACKAGE Byte PeekBufferSize;

}	/* namespace Dxsock */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsock;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSock
