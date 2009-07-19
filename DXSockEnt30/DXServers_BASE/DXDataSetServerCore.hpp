// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXDatasetServerCore.pas' rev: 5.00

#ifndef DXDatasetServerCoreHPP
#define DXDatasetServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxdatasetservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *DataSetTBasicEvent)(Dxservercore::TDXClientThread* ClientThread)
	;

typedef void __fastcall (__closure *DataSetTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1);

typedef void __fastcall (__closure *DataSetTComplexEvent)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *DataSetTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXDataSetServerCore;
class PASCALIMPLEMENTATION TDXDataSetServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	DataSetTOtherEvent fOnCommandOther;
	
public:
	__fastcall virtual TDXDataSetServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXDataSetServerCore(void);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, DataSetTSimpleEvent EventProc);
	
__published:
	__property DataSetTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxdatasetservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxdatasetservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXDatasetServerCore
