// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSysLogServerCore.pas' rev: 5.00

#ifndef DXSysLogServerCoreHPP
#define DXSysLogServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsyslogservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *SysLogTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Priority, AnsiString Facility, AnsiString LogString);

typedef void __fastcall (__closure *SysLogTDetailEvent)(Byte AppAndPriorityByte, AnsiString Priority
	, AnsiString Facility, AnsiString RFCHeader, AnsiString LogString, AnsiString SenderIP, int SenderPort
	);

class DELPHICLASS TDXSysLogServerCore;
class PASCALIMPLEMENTATION TDXSysLogServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	SysLogTOtherEvent fOnCommandOther;
	SysLogTDetailEvent fOnCommandQuick;
	
public:
	__fastcall virtual TDXSysLogServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXSysLogServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall SpecialHandler(Dxservercore::TDXClientThread* ClientThread, AnsiString PeerIP, int 
		PeerPort, void * Data, int DataLen);
	
__published:
	__property SysLogTOtherEvent OnCommandSyslog = {read=fOnCommandOther, write=fOnCommandOther};
	__property SysLogTDetailEvent OnCommandDetailSyslog = {read=fOnCommandQuick, write=fOnCommandQuick}
		;
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxsyslogservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsyslogservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSysLogServerCore
