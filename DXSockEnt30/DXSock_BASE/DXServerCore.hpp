// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXServerCore.pas' rev: 5.00

#ifndef DXServerCoreHPP
#define DXServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <DXSock.hpp>	// Pascal unit
#include <DXSessionTracker.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxservercore
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TWhichProtocol { wpUDPOnly, wpTCPOnly };
#pragma option pop

#pragma option push -b-
enum TServerType { stNonBlocking, stThreadBlocking };
#pragma option pop

class DELPHICLASS TDXClientThread;
typedef void __fastcall (__closure *TDX_NewConnect)(TDXClientThread* ClientThread);

typedef void __fastcall (__closure *TDX_DestroySessionData)(TDXClientThread* ClientThread, void * SessionData
	);

class PASCALIMPLEMENTATION TDXClientThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	Dxsock::TDXSock* Client;
	TDX_NewConnect feNewConnect;
	TDX_DestroySessionData feDestroySessionData;
	Dxsock::TDXBlockSizeFlags fBlockSizeFlags;
	Classes::TThread* ListenerThreadObject;
	
protected:
	virtual void __fastcall Execute(void);
	int __fastcall GetSessionID(void);
	
public:
	void *fpSessionData;
	__fastcall TDXClientThread(bool CreateSuspended);
	__fastcall virtual ~TDXClientThread(void);
	void __fastcall SetSocketLater(Dxsock::TDXSock* Socket);
	
__published:
	__property int SessionID = {read=GetSessionID, nodefault};
	__property Dxsock::TDXSock* Socket = {read=Client, write=Client};
	__property TDX_NewConnect OnNewConnect = {read=feNewConnect, write=feNewConnect};
	__property TDX_DestroySessionData OnDestroySessionData = {read=feDestroySessionData, write=feDestroySessionData
		};
	__property Terminated ;
};


typedef void __fastcall (__closure *TDX_ListenerFailed)(int ErrorCode);

class DELPHICLASS TDXServerCoreThread;
typedef void __fastcall (__closure *TDX_MaxConnects)(TDXServerCoreThread* ServerCoreThread);

typedef void __fastcall (__closure *TDX_Idle)(TDXServerCoreThread* ServerCoreThread);

typedef void __fastcall (__closure *TDX_Sleep)(TDXServerCoreThread* ServerCoreThread);

typedef void __fastcall (__closure *TDX_WakeUp)(TDXServerCoreThread* ServerCoreThread);

typedef void __fastcall (__closure *TDX_UDPData)(void * Data, AnsiString PeerIP, int PeerPort, int DataLen
	);

class PASCALIMPLEMENTATION TDXServerCoreThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	bool fbSuspend;
	bool fbBufferCreates;
	Dxsock::TDXSock* ListenerSocket;
	Dxsessiontracker::TDXSessionTracker* fSessionTracker;
	AnsiString fsBindTo;
	int fiServerPort;
	int fiMaxConn;
	Byte fbAnnouncedIdle;
	TDX_NewConnect feNewConnect;
	TDX_MaxConnects feMaxConnects;
	TDX_ListenerFailed feListenerFailed;
	TDX_Idle feIdle;
	TDX_Sleep feSleep;
	TDX_WakeUp feWakeUp;
	TDX_UDPData feUDPData;
	Classes::TThreadPriority fstPriority;
	Classes::TList* fThreadPool;
	TWhichProtocol fWhichprotocol;
	int FActiveConnections;
	Dxsock::TDXBlockSizeFlags fBlockSizeFlags;
	bool fUseBlocking;
	bool fUseNagle;
	Classes::TNotifyEvent feListenerStarted;
	Classes::TNotifyEvent feListenerStopped;
	
protected:
	Dxstring::TDXCritical* MyCriticalSection;
	virtual void __fastcall Execute(void);
	void __fastcall SetBufferCreates(bool value);
	void __fastcall SetSuspend(bool value);
	void __fastcall SetBlocking(bool value);
	void __fastcall SetNagle(bool value);
	Dxsock::TDXSock* __fastcall GetSocket(void);
	
public:
	__fastcall TDXServerCoreThread(bool CreateSuspended);
	__fastcall virtual ~TDXServerCoreThread(void);
	int __fastcall ActiveNumberOfConnections(void);
	
__published:
	__property Dxsock::TDXSock* MainSocket = {read=GetSocket};
	__property Classes::TThreadPriority SpawnedThreadPriority = {read=fstPriority, write=fstPriority, nodefault
		};
	__property bool BlockingListener = {read=fUseBlocking, write=SetBlocking, nodefault};
	__property bool NagleListener = {read=fUseNagle, write=SetNagle, nodefault};
	__property bool BufferCreates = {read=fbBufferCreates, write=SetBufferCreates, nodefault};
	__property bool SuspendListener = {read=fbSuspend, write=SetSuspend, nodefault};
	__property AnsiString BindTo = {read=fsBindTo, write=fsBindTo};
	__property int ServerPort = {read=fiServerPort, write=fiServerPort, nodefault};
	__property int ThreadCacheSize = {read=fiMaxConn, write=fiMaxConn, nodefault};
	__property TDX_NewConnect OnNewConnect = {read=feNewConnect, write=feNewConnect};
	__property TDX_MaxConnects OnMaxConnects = {read=feMaxConnects, write=feMaxConnects};
	__property TDX_Idle OnGoingIdle = {read=feIdle, write=feIdle};
	__property TDX_Sleep OnAsleep = {read=feSleep, write=feSleep};
	__property TDX_WakeUp OnWakeUp = {read=feWakeUp, write=feWakeUp};
	__property TDX_ListenerFailed OnListenerFailed = {read=feListenerFailed, write=feListenerFailed};
	__property Classes::TNotifyEvent OnListenerStarted = {read=feListenerStarted, write=feListenerStarted
		};
	__property Classes::TNotifyEvent OnListenerStopped = {read=feListenerStopped, write=feListenerStopped
		};
	__property TDX_UDPData OnUDPDataNoPool = {read=feUDPData, write=feUDPData};
	__property TWhichProtocol ProtocolToBind = {read=fWhichprotocol, write=fWhichprotocol, nodefault};
};


class DELPHICLASS TDXServerCore;
class PASCALIMPLEMENTATION TDXServerCore : public Dxstring::TDXComponent 
{
	typedef Dxstring::TDXComponent inherited;
	
private:
	bool fbSSL;
	bool fbActive;
	bool fbSuspend;
	bool fbBufferCreates;
	AnsiString fsBindTo;
	int fiServerPort;
	int fiMaxConn;
	TDX_NewConnect feNewConnect;
	TDX_MaxConnects feMaxConnects;
	TDX_ListenerFailed feListenerFailed;
	TDX_Idle feIdle;
	TDX_Sleep feSleep;
	TDX_WakeUp feWakeUp;
	TDX_UDPData feUDPData;
	Classes::TThreadPriority fltPriority;
	Classes::TThreadPriority fstPriority;
	TWhichProtocol fWhichprotocol;
	Dxsock::TDXBlockSizeFlags fBlockSizeFlags;
	TServerType fServerType;
	AnsiString fDummy;
	bool fbNagle;
	unsigned fiTimeout;
	Classes::TNotifyEvent fListenerStarted;
	Classes::TNotifyEvent fListenerStopped;
	
protected:
	bool fbForceAbort;
	TDXServerCoreThread* ListenerThread;
	Classes::TList* fEventArray;
	void __fastcall SetActive(bool value);
	void __fastcall SetSuspend(bool value);
	int __fastcall GetThreadCacheSize(void);
	void __fastcall SetThreadCacheSize(int value);
	Dxsock::TDXSock* __fastcall GetSocket(void);
	void __fastcall SetfiMaxConn(int Value);
	
public:
	__fastcall virtual TDXServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXServerCore(void);
	int __fastcall ActiveNumberOfConnections(void);
	virtual void __fastcall Start(void);
	virtual void __fastcall Stop(void);
	virtual void __fastcall Open(void);
	virtual void __fastcall Close(void);
	virtual void __fastcall Pause(void);
	virtual void __fastcall Resume(void);
	void __fastcall ForceAbort(void);
	__property Dxsock::TDXSock* Socket = {read=GetSocket};
	virtual Dxsessiontracker::TDXSessionTracker* __fastcall InternalSessionTracker(void);
	
__published:
	__property Classes::TThreadPriority ListenerThreadPriority = {read=fltPriority, write=fltPriority, 
		nodefault};
	__property Classes::TThreadPriority SpawnedThreadPriority = {read=fstPriority, write=fstPriority, nodefault
		};
	__property TWhichProtocol ProtocolToBind = {read=fWhichprotocol, write=fWhichprotocol, nodefault};
	__property AnsiString BindTo = {read=fsBindTo, write=fsBindTo};
	__property int ServerPort = {read=fiServerPort, write=fiServerPort, nodefault};
	__property bool IsActive = {read=fbActive, nodefault};
	__property bool Suspend = {read=fbSuspend, write=SetSuspend, nodefault};
	__property bool UseSSL = {read=fbSSL, write=fbSSL, nodefault};
	__property bool UseNagle = {read=fbNagle, write=fbNagle, nodefault};
	__property bool UseThreadPool = {read=fbBufferCreates, write=fbBufferCreates, nodefault};
	__property Dxsock::TDXBlockSizeFlags SocketOutputBufferSize = {read=fBlockSizeFlags, write=fBlockSizeFlags
		, nodefault};
	__property TServerType ServerType = {read=fServerType, write=fServerType, nodefault};
	__property AnsiString Service = {read=fDummy, write=fDummy};
	__property int ThreadCacheSize = {read=GetThreadCacheSize, write=SetThreadCacheSize, nodefault};
	__property unsigned Timeout = {read=fiTimeout, write=fiTimeout, nodefault};
	__property TDX_NewConnect OnNewConnect = {read=feNewConnect, write=feNewConnect};
	__property TDX_UDPData OnUDPDataNoPool = {read=feUDPData, write=feUDPData};
	__property TDX_MaxConnects OnMaxConnects = {read=feMaxConnects, write=feMaxConnects};
	__property TDX_Idle OnGoingIdle = {read=feIdle, write=feIdle};
	__property TDX_Sleep OnAsleep = {read=feSleep, write=feSleep};
	__property TDX_WakeUp OnWakeUp = {read=feWakeUp, write=feWakeUp};
	__property TDX_ListenerFailed OnListenerFailed = {read=feListenerFailed, write=feListenerFailed};
	__property Classes::TNotifyEvent OnListenerStarted = {read=fListenerStarted, write=fListenerStarted
		};
	__property Classes::TNotifyEvent OnListenerStopped = {read=fListenerStopped, write=fListenerStopped
		};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXServerCore
