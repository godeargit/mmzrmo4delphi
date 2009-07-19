// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXMUDServerCore.pas' rev: 5.00

#ifndef DXMUDServerCoreHPP
#define DXMUDServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxmudservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *MUDTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *MUDTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *MUDTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm1, AnsiString Parm2);

typedef void __fastcall (__closure *MUDTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

class DELPHICLASS TDXMUDServerCore;
class PASCALIMPLEMENTATION TDXMUDServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	MUDTSimpleEvent fOnCommandNORTH;
	MUDTSimpleEvent fOnCommandSOUTH;
	MUDTSimpleEvent fOnCommandEAST;
	MUDTSimpleEvent fOnCommandWEST;
	MUDTSimpleEvent fOnCommandQUIT;
	MUDTSimpleEvent fOnCommandINV;
	MUDTBasicEvent fOnCommandSAY;
	MUDTBasicEvent fOnCommandLOOK;
	MUDTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandNORTH(MUDTSimpleEvent value);
	void __fastcall SetOnCommandSOUTH(MUDTSimpleEvent value);
	void __fastcall SetOnCommandEAST(MUDTSimpleEvent value);
	void __fastcall SetOnCommandWEST(MUDTSimpleEvent value);
	void __fastcall SetOnCommandQUIT(MUDTSimpleEvent value);
	void __fastcall SetOnCommandINV(MUDTSimpleEvent value);
	void __fastcall SetOnCommandSAY(MUDTBasicEvent value);
	void __fastcall SetOnCommandLOOK(MUDTBasicEvent value);
	
public:
	__fastcall virtual TDXMUDServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXMUDServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, Classes::TStrings* Header, Classes::TStrings* 
		MOTD);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, MUDTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, MUDTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, MUDTComplexEvent EventProc);
	
__published:
	__property MUDTSimpleEvent OnCommandNORTH = {read=fOnCommandNORTH, write=SetOnCommandNORTH};
	__property MUDTSimpleEvent OnCommandSOUTH = {read=fOnCommandSOUTH, write=SetOnCommandSOUTH};
	__property MUDTSimpleEvent OnCommandEAST = {read=fOnCommandEAST, write=SetOnCommandEAST};
	__property MUDTSimpleEvent OnCommandWEST = {read=fOnCommandWEST, write=SetOnCommandWEST};
	__property MUDTSimpleEvent OnCommandQUIT = {read=fOnCommandQUIT, write=SetOnCommandQUIT};
	__property MUDTSimpleEvent OnCommandINV = {read=fOnCommandINV, write=SetOnCommandINV};
	__property MUDTBasicEvent OnCommandSAY = {read=fOnCommandSAY, write=SetOnCommandSAY};
	__property MUDTBasicEvent OnCommandLOOK = {read=fOnCommandLOOK, write=SetOnCommandLOOK};
	__property MUDTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxmudservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxmudservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXMUDServerCore
