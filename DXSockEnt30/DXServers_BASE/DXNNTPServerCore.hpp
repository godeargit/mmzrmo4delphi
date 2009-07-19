// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXNNTPServerCore.pas' rev: 5.00

#ifndef DXNNTPServerCoreHPP
#define DXNNTPServerCoreHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXServerCore.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxnntpservercore
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *NNTPTSimpleEvent)(Dxservercore::TDXClientThread* ClientThread);

typedef void __fastcall (__closure *NNTPTOtherEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Command, AnsiString Parm, bool &Handled);

typedef void __fastcall (__closure *NNTPTDoByIDEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	ActualID);

typedef void __fastcall (__closure *NNTPTDoByNoEvent)(Dxservercore::TDXClientThread* ClientThread, int 
	ActualNumber);

typedef void __fastcall (__closure *NNTPTBasicEvent)(Dxservercore::TDXClientThread* ClientThread, AnsiString 
	Parm);

typedef void __fastcall (__closure *NNTPTComplexEvent)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Parm1, AnsiString Parm2);

class DELPHICLASS TDXNNTPServerCore;
class PASCALIMPLEMENTATION TDXNNTPServerCore : public Dxservercore::TDXServerCore 
{
	typedef Dxservercore::TDXServerCore inherited;
	
private:
	NNTPTComplexEvent fOnCommandAuthInfo;
	NNTPTDoByIDEvent fOnCommandArticleID;
	NNTPTDoByNoEvent fOnCommandArticleNO;
	NNTPTDoByIDEvent fOnCommandBodyID;
	NNTPTDoByNoEvent fOnCommandBodyNO;
	NNTPTDoByIDEvent fOnCommandHeadID;
	NNTPTDoByNoEvent fOnCommandHeadNO;
	NNTPTDoByIDEvent fOnCommandStatID;
	NNTPTDoByNoEvent fOnCommandStatNO;
	NNTPTBasicEvent fOnCommandGroup;
	NNTPTBasicEvent fOnCommandList;
	NNTPTSimpleEvent fOnCommandHelp;
	NNTPTDoByIDEvent fOnCommandIHave;
	NNTPTSimpleEvent fOnCommandLast;
	NNTPTBasicEvent fOnCommandMode;
	NNTPTBasicEvent fOnCommandNewGroups;
	NNTPTBasicEvent fOnCommandNewNews;
	NNTPTSimpleEvent fOnCommandNext;
	NNTPTSimpleEvent fOnCommandPost;
	NNTPTSimpleEvent fOnCommandQuit;
	NNTPTSimpleEvent fOnCommandSlave;
	NNTPTBasicEvent fOnCommandXOver;
	NNTPTBasicEvent fOnCommandXHDR;
	NNTPTSimpleEvent fOnCommandDate;
	NNTPTDoByIDEvent fOnCommandCheck;
	NNTPTDoByIDEvent fOnCommandTakethis;
	NNTPTBasicEvent fOnCommandXReplic;
	NNTPTBasicEvent fOnCommandListgroup;
	NNTPTBasicEvent fOnCommandXGTitle;
	NNTPTBasicEvent fOnCommandXIndex;
	NNTPTComplexEvent fOnCommandXPAT;
	NNTPTDoByIDEvent fOnCommandXPath;
	NNTPTBasicEvent fOnCommandXThread;
	NNTPTDoByIDEvent fOnCommandCancel;
	NNTPTDoByIDEvent fOnCommandSendMe;
	NNTPTBasicEvent fOnCommandRMGroup;
	NNTPTSimpleEvent fOnCommandSendSys;
	NNTPTSimpleEvent fOnCommandVersion;
	NNTPTOtherEvent fOnCommandOther;
	
protected:
	void __fastcall SetOnCommandAuthInfo(NNTPTComplexEvent value);
	void __fastcall SetOnCommandArticleID(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandArticleNO(NNTPTDoByNoEvent value);
	void __fastcall SetOnCommandBodyID(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandBodyNO(NNTPTDoByNoEvent value);
	void __fastcall SetOnCommandHeadID(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandHeadNO(NNTPTDoByNoEvent value);
	void __fastcall SetOnCommandStatID(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandStatNO(NNTPTDoByNoEvent value);
	void __fastcall SetOnCommandGroup(NNTPTBasicEvent value);
	void __fastcall SetOnCommandList(NNTPTBasicEvent value);
	void __fastcall SetOnCommandHelp(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandIHave(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandLast(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandMode(NNTPTBasicEvent value);
	void __fastcall SetOnCommandNewGroups(NNTPTBasicEvent value);
	void __fastcall SetOnCommandNewNews(NNTPTBasicEvent value);
	void __fastcall SetOnCommandNext(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandPost(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandQuit(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandSlave(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandXOver(NNTPTBasicEvent value);
	void __fastcall SetOnCommandXHDR(NNTPTBasicEvent value);
	void __fastcall SetOnCommandDate(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandListGroup(NNTPTBasicEvent value);
	void __fastcall SetOnCommandCheck(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandTakeThis(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandXReplic(NNTPTBasicEvent value);
	void __fastcall SetOnCommandXGTitle(NNTPTBasicEvent value);
	void __fastcall SetOnCommandXIndex(NNTPTBasicEvent value);
	void __fastcall SetOnCommandXPAT(NNTPTComplexEvent value);
	void __fastcall SetOnCommandXPATH(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandXThread(NNTPTBasicEvent value);
	void __fastcall SetOnCommandCancel(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandSendMe(NNTPTDoByIDEvent value);
	void __fastcall SetOnCommandRMGroup(NNTPTBasicEvent value);
	void __fastcall SetOnCommandSendSys(NNTPTSimpleEvent value);
	void __fastcall SetOnCommandVersion(NNTPTSimpleEvent value);
	
public:
	__fastcall virtual TDXNNTPServerCore(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXNNTPServerCore(void);
	void __fastcall SayHello(Dxservercore::TDXClientThread* ClientThread, AnsiString Header);
	void __fastcall SayGoodbye(Dxservercore::TDXClientThread* ClientThread, AnsiString Footer);
	void __fastcall ProcessSession(Dxservercore::TDXClientThread* ClientThread);
	void __fastcall AddBasicEvent(AnsiString Command, NNTPTBasicEvent EventProc);
	void __fastcall AddSimpleEvent(AnsiString Command, NNTPTSimpleEvent EventProc);
	void __fastcall AddComplexEvent(AnsiString Command, NNTPTComplexEvent EventProc);
	void __fastcall AddDoByIDEvent(AnsiString Command, NNTPTDoByIDEvent EventProc);
	void __fastcall AddDoByNOEvent(AnsiString Command, NNTPTDoByNoEvent EventProc);
	
__published:
	__property NNTPTComplexEvent OnCommandAuthInfo = {read=fOnCommandAuthInfo, write=SetOnCommandAuthInfo
		};
	__property NNTPTDoByIDEvent OnCommandArticleID = {read=fOnCommandArticleID, write=SetOnCommandArticleID
		};
	__property NNTPTDoByNoEvent OnCommandArticleNo = {read=fOnCommandArticleNO, write=SetOnCommandArticleNO
		};
	__property NNTPTDoByIDEvent OnCommandBodyID = {read=fOnCommandBodyID, write=SetOnCommandBodyID};
	__property NNTPTDoByNoEvent OnCommandBodyNo = {read=fOnCommandBodyNO, write=SetOnCommandBodyNO};
	__property NNTPTDoByIDEvent OnCommandHeadID = {read=fOnCommandHeadID, write=SetOnCommandHeadID};
	__property NNTPTDoByNoEvent OnCommandHeadNo = {read=fOnCommandHeadNO, write=SetOnCommandHeadNO};
	__property NNTPTDoByIDEvent OnCommandStatID = {read=fOnCommandStatID, write=SetOnCommandStatID};
	__property NNTPTDoByNoEvent OnCommandStatNo = {read=fOnCommandStatNO, write=SetOnCommandStatNO};
	__property NNTPTBasicEvent OnCommandGroup = {read=fOnCommandGroup, write=SetOnCommandGroup};
	__property NNTPTBasicEvent OnCommandList = {read=fOnCommandList, write=SetOnCommandList};
	__property NNTPTSimpleEvent OnCommandHelp = {read=fOnCommandHelp, write=SetOnCommandHelp};
	__property NNTPTDoByIDEvent OnCommandIHave = {read=fOnCommandIHave, write=SetOnCommandIHave};
	__property NNTPTSimpleEvent OnCommandLast = {read=fOnCommandLast, write=SetOnCommandLast};
	__property NNTPTBasicEvent OnCommandMode = {read=fOnCommandMode, write=SetOnCommandMode};
	__property NNTPTBasicEvent OnCommandNewGroups = {read=fOnCommandNewGroups, write=SetOnCommandNewGroups
		};
	__property NNTPTBasicEvent OnCommandNewNews = {read=fOnCommandNewNews, write=SetOnCommandNewNews};
	__property NNTPTSimpleEvent OnCommandNext = {read=fOnCommandNext, write=SetOnCommandNext};
	__property NNTPTSimpleEvent OnCommandPost = {read=fOnCommandPost, write=SetOnCommandPost};
	__property NNTPTSimpleEvent OnCommandQuit = {read=fOnCommandQuit, write=SetOnCommandQuit};
	__property NNTPTSimpleEvent OnCommandSlave = {read=fOnCommandSlave, write=SetOnCommandSlave};
	__property NNTPTBasicEvent OnCommandXOver = {read=fOnCommandXOver, write=SetOnCommandXOver};
	__property NNTPTBasicEvent OnCommandXHDR = {read=fOnCommandXHDR, write=SetOnCommandXHDR};
	__property NNTPTSimpleEvent OnCommandDate = {read=fOnCommandDate, write=SetOnCommandDate};
	__property NNTPTBasicEvent OnCommandListgroup = {read=fOnCommandListgroup, write=SetOnCommandListGroup
		};
	__property NNTPTDoByIDEvent OnCommandCheck = {read=fOnCommandCheck, write=SetOnCommandCheck};
	__property NNTPTDoByIDEvent OnCommandTakethis = {read=fOnCommandTakethis, write=SetOnCommandTakeThis
		};
	__property NNTPTBasicEvent OnCommandXReplic = {read=fOnCommandXReplic, write=SetOnCommandXReplic};
	__property NNTPTBasicEvent OnCommandXGTitle = {read=fOnCommandXGTitle, write=SetOnCommandXGTitle};
	__property NNTPTBasicEvent OnCommandXIndex = {read=fOnCommandXIndex, write=SetOnCommandXIndex};
	__property NNTPTComplexEvent OnCommandXPAT = {read=fOnCommandXPAT, write=SetOnCommandXPAT};
	__property NNTPTDoByIDEvent OnCommandXPath = {read=fOnCommandXPath, write=SetOnCommandXPATH};
	__property NNTPTBasicEvent OnCommandXThread = {read=fOnCommandXThread, write=SetOnCommandXThread};
	__property NNTPTDoByIDEvent OnCommandCancel = {read=fOnCommandCancel, write=SetOnCommandCancel};
	__property NNTPTDoByIDEvent OnCommandSendMe = {read=fOnCommandSendMe, write=SetOnCommandSendMe};
	__property NNTPTBasicEvent OnCommandRMGroup = {read=fOnCommandRMGroup, write=SetOnCommandRMGroup};
	__property NNTPTSimpleEvent OnCommandSendSys = {read=fOnCommandSendSys, write=SetOnCommandSendSys};
		
	__property NNTPTSimpleEvent OnCommandVersion = {read=fOnCommandVersion, write=SetOnCommandVersion};
		
	__property NNTPTOtherEvent OnCommandOther = {read=fOnCommandOther, write=fOnCommandOther};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxnntpservercore */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxnntpservercore;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXNNTPServerCore
