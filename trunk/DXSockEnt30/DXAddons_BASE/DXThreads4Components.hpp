// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXThreads4Components.pas' rev: 6.00

#ifndef DXThreads4ComponentsHPP
#define DXThreads4ComponentsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxthreads4components
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXComponentThread;
class PASCALIMPLEMENTATION TDXComponentThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	bool fSleepFirst;
	bool fSynchronized;
	Classes::TComponent* TT;
	unsigned fInterval;
	Classes::TNotifyEvent fOnTimer;
	
protected:
	void __fastcall DoExecute(void);
	
public:
	__fastcall TDXComponentThread(Classes::TComponent* TT);
	virtual void __fastcall Execute(void);
	
__published:
	__property unsigned Interval = {read=fInterval, write=fInterval, default=60000};
	__property Classes::TNotifyEvent OnTimer = {read=fOnTimer, write=fOnTimer};
	__property bool RunSychronized = {read=fSynchronized, write=fSynchronized, nodefault};
	__property bool SleepFirst = {read=fSleepFirst, write=fSleepFirst, nodefault};
public:
	#pragma option push -w-inl
	/* TThread.Create */ inline __fastcall TDXComponentThread(bool CreateSuspended) : Classes::TThread(CreateSuspended) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TThread.Destroy */ inline __fastcall virtual ~TDXComponentThread(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TDX_OnTimerSpawn)(void * fpSessionData);

class DELPHICLASS TDXComponentSpawn;
class PASCALIMPLEMENTATION TDXComponentSpawn : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	void *fpPointer;
	TDX_OnTimerSpawn fOnTimer;
	int fID;
	
public:
	__fastcall TDXComponentSpawn(void * fpSessionData);
	virtual void __fastcall Execute(void);
	void __fastcall SetSessionData(void * fpSessionData);
	void * __fastcall GetSessionData(void);
	
__published:
	__property TDX_OnTimerSpawn OnTimer = {read=fOnTimer, write=fOnTimer};
	__property int ID = {read=fID, write=fID, nodefault};
public:
	#pragma option push -w-inl
	/* TThread.Create */ inline __fastcall TDXComponentSpawn(bool CreateSuspended) : Classes::TThread(CreateSuspended) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TThread.Destroy */ inline __fastcall virtual ~TDXComponentSpawn(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDXFork;
class PASCALIMPLEMENTATION TDXFork : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
protected:
	Classes::TThreadMethod fProc;
	
public:
	__fastcall TDXFork(Classes::TThreadMethod Proc);
	virtual void __fastcall Execute(void);
public:
	#pragma option push -w-inl
	/* TThread.Create */ inline __fastcall TDXFork(bool CreateSuspended) : Classes::TThread(CreateSuspended) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TThread.Destroy */ inline __fastcall virtual ~TDXFork(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxthreads4components */
using namespace Dxthreads4components;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXThreads4Components
