// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDatabaseServerCore.pas' rev: 5.00

#ifndef DXDatabaseServerCoreHPP
#define DXDatabaseServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdatabaseservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *DatabaseTBasicEvent)(Dxservercore::TDXClientThread* ClientThread
	);

typedef void __fastcall (__closure *DatabaseTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1);

typedef void __fastcall (__closure *DatabaseTComplexEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *DatabaseTOtherEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXDatabaseServerCore;
class PASCALIMPLEMENTATION TDXDatabaseServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	DatabaseTSimpleEvent fOnCommandUSER;
	DatabaseTSimpleEvent fOnCommandPASS;
	DatabaseTComplexEvent fOnCommandAUTH;
	DatabaseTSimpleEvent fOnCommandSETACTIVE;
	DatabaseTBasicEvent fOnCommandISBOF;
	DatabaseTBasicEvent fOnCommandISEOF;
	DatabaseTBasicEvent fOnCommandISEMPTY;
	DatabaseTBasicEvent fOnCommandCANMODIFY;
	DatabaseTBasicEvent fOnCommandFIELDCOUNT;
	DatabaseTBasicEvent fOnCommandRECORDCOUNT;
	DatabaseTBasicEvent fOnCommandRECORDSIZE;
	DatabaseTBasicEvent fOnCommandFOUND;
	DatabaseTBasicEvent fOnCommandMODIFIED;
	DatabaseTSimpleEvent fOnCommandCREATEDATASET;
	DatabaseTSimpleEvent fOnCommandDISPOSEDATASET;
	DatabaseTSimpleEvent fOnCommandISLINKEDTODATASET;
	DatabaseTBasicEvent fOnCommandOPEN;
	DatabaseTBasicEvent fOnCommandCLOSE;
	DatabaseTBasicEvent fOnCommandFIRST;
	DatabaseTBasicEvent fOnCommandNEXT;
	DatabaseTBasicEvent fOnCommandPRIOR;
	DatabaseTBasicEvent fOnCommandLAST;
	DatabaseTSimpleEvent fOnCommandMOVEBY;
	DatabaseTBasicEvent fOnCommandFINDFIRST;
	DatabaseTBasicEvent fOnCommandFINDLAST;
	DatabaseTBasicEvent fOnCommandFINDNEXT;
	DatabaseTBasicEvent fOnCommandFINDPRIOR;
	DatabaseTSimpleEvent fOnCommandFINDFIELD;
	DatabaseTBasicEvent fOnCommandGETFIELDLIST;
	DatabaseTSimpleEvent fOnCommandLOCATECaseInsensitive;
	DatabaseTSimpleEvent fOnCommandLOCATEPartialKey;
	DatabaseTBasicEvent fOnCommandLOCATENoOptions;
	DatabaseTBasicEvent fOnCommandLOCATEBothOptions;
	DatabaseTComplexEvent fOnCommandLOCATE;
	DatabaseTBasicEvent fOnCommandAPPEND;
	DatabaseTBasicEvent fOnCommandINSERT;
	DatabaseTBasicEvent fOnCommandEDIT;
	DatabaseTBasicEvent fOnCommandDELETE;
	DatabaseTBasicEvent fOnCommandPOST;
	DatabaseTBasicEvent fOnCommandCANCEL;
	DatabaseTBasicEvent fOnCommandREFRESH;
	DatabaseTComplexEvent fOnCommandFIELDBYNAME;
	DatabaseTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandUSER(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandPASS(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandAUTH(DatabaseTComplexEvent value);
	void __fastcall SetOnCommandSETACTIVE(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandISBOF(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandISEOF(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandISEMPTY(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandCANMODIFY(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFIELDCOUNT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandRECORDCOUNT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandRECORDSIZE(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFOUND(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandMODIFIED(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandCREATEDATASET(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandDISPOSEDATASET(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandOPEN(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandCLOSE(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFIRST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandNEXT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandPRIOR(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandLAST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandMOVEBY(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandFINDFIRST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFINDLAST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFINDNEXT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFINDPRIOR(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFINDFIELD(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandGETFIELDLIST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandLOCATECASEINSENSITIVE(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandLOCATEPARTIALKEY(DatabaseTSimpleEvent value);
	void __fastcall SetOnCommandLOCATENOOPTIONS(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandLOCATEBOTHOPTIONS(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandLOCATE(DatabaseTComplexEvent value);
	void __fastcall SetOnCommandAPPEND(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandINSERT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandEDIT(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandDELETE(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandPOST(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandCANCEL(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandREFRESH(DatabaseTBasicEvent value);
	void __fastcall SetOnCommandFIELDBYNAME(DatabaseTComplexEvent value);
	
public:
	__fastcall virtual TDXDatabaseServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDatabaseServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddSimpleEvent(AnsiString Command, DatabaseTSimpleEvent EventProc);
	void __fastcall AddBasicEvent(AnsiString Command, DatabaseTBasicEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, DatabaseTComplexEvent EventProc);
	
__published:
	__property DatabaseTSimpleEvent OnCommandUSER = {read=fOnCommandUSER, write=SetOnCommandUSER};
	__property DatabaseTSimpleEvent OnCommandPASS = {read=fOnCommandPASS, write=SetOnCommandPASS};
	__property DatabaseTComplexEvent OnCommandAUTH = {read=fOnCommandAUTH, write=SetOnCommandAUTH};
	__property DatabaseTSimpleEvent OnCommandSETACTIVE = {read=fOnCommandSETACTIVE, write=SetOnCommandSETACTIVE
		};
	__property DatabaseTBasicEvent OnCommandISBOF = {read=fOnCommandISBOF, write=SetOnCommandISBOF};
	__property DatabaseTBasicEvent OnCommandISEOF = {read=fOnCommandISEOF, write=SetOnCommandISEOF};
	__property DatabaseTBasicEvent OnCommandISEMPTY = {read=fOnCommandISEMPTY, write=SetOnCommandISEMPTY
		};
	__property DatabaseTBasicEvent OnCommandCANMODIFY = {read=fOnCommandCANMODIFY, write=SetOnCommandCANMODIFY
		};
	__property DatabaseTBasicEvent OnCommandFIELDCOUNT = {read=fOnCommandFIELDCOUNT, write=SetOnCommandFIELDCOUNT
		};
	__property DatabaseTBasicEvent OnCommandRECORDCOUNT = {read=fOnCommandRECORDCOUNT, write=SetOnCommandRECORDCOUNT
		};
	__property DatabaseTBasicEvent OnCommandRECORDSIZE = {read=fOnCommandRECORDSIZE, write=SetOnCommandRECORDSIZE
		};
	__property DatabaseTBasicEvent OnCommandFOUND = {read=fOnCommandFOUND, write=SetOnCommandFOUND};
	__property DatabaseTBasicEvent OnCommandMODIFIED = {read=fOnCommandMODIFIED, write=SetOnCommandMODIFIED
		};
	__property DatabaseTSimpleEvent OnCommandCREATEDATASET = {read=fOnCommandCREATEDATASET, write=SetOnCommandCREATEDATASET
		};
	__property DatabaseTSimpleEvent OnCommandDISPOSEDATASET = {read=fOnCommandDISPOSEDATASET, write=SetOnCommandDISPOSEDATASET
		};
	__property DatabaseTBasicEvent OnCommandOPEN = {read=fOnCommandOPEN, write=SetOnCommandOPEN};
	__property DatabaseTBasicEvent OnCommandCLOSE = {read=fOnCommandCLOSE, write=SetOnCommandCLOSE};
	__property DatabaseTBasicEvent OnCommandFIRST = {read=fOnCommandFIRST, write=SetOnCommandFIRST};
	__property DatabaseTBasicEvent OnCommandNEXT = {read=fOnCommandNEXT, write=SetOnCommandNEXT};
	__property DatabaseTBasicEvent OnCommandPRIOR = {read=fOnCommandPRIOR, write=SetOnCommandPRIOR};
	__property DatabaseTBasicEvent OnCommandLAST = {read=fOnCommandLAST, write=SetOnCommandLAST};
	__property DatabaseTSimpleEvent OnCommandMOVEBY = {read=fOnCommandMOVEBY, write=SetOnCommandMOVEBY}
		;
	__property DatabaseTBasicEvent OnCommandFINDFIRST = {read=fOnCommandFINDFIRST, write=SetOnCommandFINDFIRST
		};
	__property DatabaseTBasicEvent OnCommandFINDLAST = {read=fOnCommandFINDLAST, write=SetOnCommandFINDLAST
		};
	__property DatabaseTBasicEvent OnCommandFINDNEXT = {read=fOnCommandFINDNEXT, write=SetOnCommandFINDNEXT
		};
	__property DatabaseTBasicEvent OnCommandFINDPRIOR = {read=fOnCommandFINDPRIOR, write=SetOnCommandFINDPRIOR
		};
	__property DatabaseTSimpleEvent OnCommandFINDFIELD = {read=fOnCommandFINDFIELD, write=SetOnCommandFINDFIELD
		};
	__property DatabaseTBasicEvent OnCommandGETFIELDLIST = {read=fOnCommandGETFIELDLIST, write=SetOnCommandGETFIELDLIST
		};
	__property DatabaseTSimpleEvent OnCommandLOCATECaseInsensitive = {read=fOnCommandLOCATECaseInsensitive
		, write=SetOnCommandLOCATECASEINSENSITIVE};
	__property DatabaseTSimpleEvent OnCommandLOCATEPartialKey = {read=fOnCommandLOCATEPartialKey, write=
		SetOnCommandLOCATEPARTIALKEY};
	__property DatabaseTBasicEvent OnCommandLOCATENoOptions = {read=fOnCommandLOCATENoOptions, write=SetOnCommandLOCATENOOPTIONS
		};
	__property DatabaseTBasicEvent OnCommandLOCATEBothOptions = {read=fOnCommandLOCATEBothOptions, write=
		SetOnCommandLOCATEBOTHOPTIONS};
	__property DatabaseTComplexEvent OnCommandLOCATE = {read=fOnCommandLOCATE, write=SetOnCommandLOCATE
		};
	__property DatabaseTBasicEvent OnCommandAPPEND = {read=fOnCommandAPPEND, write=SetOnCommandAPPEND};
		
	__property DatabaseTBasicEvent OnCommandINSERT = {read=fOnCommandINSERT, write=SetOnCommandINSERT};
		
	__property DatabaseTBasicEvent OnCommandEDIT = {read=fOnCommandEDIT, write=SetOnCommandEDIT};
	__property DatabaseTBasicEvent OnCommandDELETE = {read=fOnCommandDELETE, write=SetOnCommandDELETE};
		
	__property DatabaseTBasicEvent OnCommandPOST = {read=fOnCommandPOST, write=SetOnCommandPOST};
	__property DatabaseTBasicEvent OnCommandCANCEL = {read=fOnCommandCANCEL, write=SetOnCommandCANCEL};
		
	__property DatabaseTBasicEvent OnCommandREFRESH = {read=fOnCommandREFRESH, write=SetOnCommandREFRESH
		};
	__property DatabaseTComplexEvent OnCommandFIELDBYNAME = {read=fOnCommandFIELDBYNAME, write=SetOnCommandFIELDBYNAME
		};
	__property DatabaseTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdatabaseservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdatabaseservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDatabaseServerCore
