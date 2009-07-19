// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXMessageBaseServerCore.pas' rev: 5.00

#ifndef DXMessageBaseServerCoreHPP
#define DXMessageBaseServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxmessagebaseservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *MessageBaseTBasicEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1);

typedef void __fastcall (__closure *MessageBaseTRangeEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString First, AnsiString Last);

typedef void __fastcall (__closure *MessageBaseTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	);

typedef void __fastcall (__closure *MessageBaseTOtherEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Command, AnsiString Parms, bool &Handled);

class DELPHICLASS TDXMessageBaseServerCore;
class PASCALIMPLEMENTATION TDXMessageBaseServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	MessageBaseTBasicEvent fOnCommandLOGIN;
	MessageBaseTBasicEvent fOnCommandFORMAT;
	MessageBaseTSimpleEvent fOnCommandMESSAGES;
	MessageBaseTBasicEvent fOnCommandGETMSG;
	MessageBaseTBasicEvent fOnCommandDELMSG;
	MessageBaseTSimpleEvent fOnCommandADDMSG;
	MessageBaseTRangeEvent fOnCommandMSGHDRS;
	MessageBaseTBasicEvent fOnCommandMSGBODY;
	MessageBaseTBasicEvent fOnCommandEDITMSG;
	MessageBaseTSimpleEvent fOnCommandRESCAN;
	MessageBaseTSimpleEvent fOnCommandAREAS;
	MessageBaseTBasicEvent fOnCommandNEWAREA;
	MessageBaseTBasicEvent fOnCommandDELAREA;
	MessageBaseTBasicEvent fOnCommandGETAREA;
	MessageBaseTSimpleEvent fOnCommandHELP;
	MessageBaseTOtherEvent fOnCommandOther;
	Classes::TList* fEventArray;
	unsigned fiTimeout;
	bool fbForceAbort;
	
protected:
	void __fastcall SetOnCommandLOGIN(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandFORMAT(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandMESSAGES(MessageBaseTSimpleEvent value);
	void __fastcall SetOnCommandGETMSG(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandDELMSG(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandADDMSG(MessageBaseTSimpleEvent value);
	void __fastcall SetOnCommandMSGHDRS(MessageBaseTRangeEvent value);
	void __fastcall SetOnCommandMSGBODY(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandEDITMSG(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandRESCAN(MessageBaseTSimpleEvent value);
	void __fastcall SetOnCommandAREAS(MessageBaseTSimpleEvent value);
	void __fastcall SetOnCommandNEWAREA(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandDELAREA(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandGETAREA(MessageBaseTBasicEvent value);
	void __fastcall SetOnCommandHELP(MessageBaseTSimpleEvent value);
	
public:
	__fastcall virtual TDXMessageBaseServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXMessageBaseServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, MessageBaseTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, MessageBaseTSimpleEvent EventProc);
	void __fastcall AddRangeEvent(AnsiString Command, MessageBaseTRangeEvent EventProc);
	HIDESBASE void __fastcall ForceAbort(void);
	
__published:
	__property MessageBaseTBasicEvent OnCommandLOGIN = {read=fOnCommandLOGIN, write=SetOnCommandLOGIN};
		
	__property MessageBaseTBasicEvent OnCommandFORMAT = {read=fOnCommandFORMAT, write=SetOnCommandFORMAT
		};
	__property MessageBaseTSimpleEvent OnCommandMESSAGES = {read=fOnCommandMESSAGES, write=SetOnCommandMESSAGES
		};
	__property MessageBaseTBasicEvent OnCommandGETMSG = {read=fOnCommandGETMSG, write=SetOnCommandGETMSG
		};
	__property MessageBaseTBasicEvent OnCommandDELMSG = {read=fOnCommandDELMSG, write=SetOnCommandDELMSG
		};
	__property MessageBaseTSimpleEvent OnCommandADDMSG = {read=fOnCommandADDMSG, write=SetOnCommandADDMSG
		};
	__property MessageBaseTRangeEvent OnCommandMSGHDRS = {read=fOnCommandMSGHDRS, write=SetOnCommandMSGHDRS
		};
	__property MessageBaseTBasicEvent OnCommandMSGBODY = {read=fOnCommandMSGBODY, write=SetOnCommandMSGBODY
		};
	__property MessageBaseTBasicEvent OnCommandEDITMSG = {read=fOnCommandEDITMSG, write=SetOnCommandEDITMSG
		};
	__property MessageBaseTSimpleEvent OnCommandRESCAN = {read=fOnCommandRESCAN, write=SetOnCommandRESCAN
		};
	__property MessageBaseTSimpleEvent OnCommandAREAS = {read=fOnCommandAREAS, write=SetOnCommandAREAS}
		;
	__property MessageBaseTBasicEvent OnCommandNEWAREA = {read=fOnCommandNEWAREA, write=SetOnCommandNEWAREA
		};
	__property MessageBaseTBasicEvent OnCommandDELAREA = {read=fOnCommandDELAREA, write=SetOnCommandDELAREA
		};
	__property MessageBaseTBasicEvent OnCommandGETAREA = {read=fOnCommandGETAREA, write=SetOnCommandGETAREA
		};
	__property MessageBaseTSimpleEvent OnCommandHELP = {read=fOnCommandHELP, write=SetOnCommandHELP};
	__property MessageBaseTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxmessagebaseservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxmessagebaseservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXMessageBaseServerCore
