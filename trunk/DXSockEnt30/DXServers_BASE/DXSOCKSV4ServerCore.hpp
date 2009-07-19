// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSOCKSV4ServerCore.pas' rev: 5.00

#ifndef DXSOCKSV4ServerCoreHPP
#define DXSOCKSV4ServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsocksv4servercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *SOCKSV4TConnectEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString DestAddr, int DestIP, Word DestPort, AnsiString UserID);

typedef void __fastcall (__closure *SOCKSV4TBindEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString DestAddr, int DestIP, Word DestPort, AnsiString UserID);

class DELPHICLASS TDXSOCKSV4ServerCore;
class PASCALIMPLEMENTATION TDXSOCKSV4ServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	SOCKSV4TConnectEvent fOnCommandConnect;
	SOCKSV4TBindEvent fOnCommandBind;
	char fVersionID;
	
public:
	__fastcall virtual TDXSOCKSV4ServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSOCKSV4ServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall RequestGranted(Dxservercore::TDXClientThread* ClientThread, Word DestPort, int DestIP
		);
	void __fastcall RequestFailed(Dxservercore::TDXClientThread* ClientThread, Word DestPort, int DestIP
		);
	
__published:
	__property SOCKSV4TConnectEvent OnCommandConnect = {read=fOnCommandConnect, write=fOnCommandConnect
		};
	__property SOCKSV4TBindEvent OnCommandBind = {read=fOnCommandBind, write=fOnCommandBind};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsocksv4servercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsocksv4servercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSOCKSV4ServerCore
