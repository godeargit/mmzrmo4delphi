// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXISAPI.pas' rev: 5.00

#ifndef DXISAPIHPP
#define DXISAPIHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DXHTTPHeaderTools.hpp>	// Pascal unit
#include <DXISAPIFilter.hpp>	// Pascal unit
#include <DXSecurity.hpp>	// Pascal unit
#include <DXServerCore.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxisapi
{
//-- type declarations -------------------------------------------------------
typedef int HCONN;

typedef bool __stdcall (*TGetServerVariableProc)(int hConn, char * VariableName, void * Buffer, PDWORD 
	Size);

typedef bool __stdcall (*TWriteClientProc)(int ConnID, void * Buffer, unsigned &Bytes, unsigned dwReserved
	);

typedef bool __stdcall (*TReadClientProc)(int ConnID, void * Buffer, unsigned &Size);

typedef bool __stdcall (*TServerSupportFunctionProc)(int hConn, unsigned HSERRequest, void * Buffer, 
	void * Size, void * DataType);

struct TEXTENSION_CONTROL_BLOCK;
typedef TEXTENSION_CONTROL_BLOCK *PEXTENSION_CONTROL_BLOCK;

#pragma pack(push, 1)
struct TEXTENSION_CONTROL_BLOCK
{
	unsigned cbSize;
	unsigned dwVersion;
	int ConnID;
	unsigned dwHttpStatusCode;
	char lpszLogData[80];
	char *lpszMethod;
	char *lpszQueryString;
	char *lpszPathInfo;
	char *lpszPathTranslated;
	unsigned cbTotalBytes;
	unsigned cbAvailable;
	void *lpbData;
	char *lpszContentType;
	TGetServerVariableProc GetServerVariable;
	TWriteClientProc WriteClient;
	TReadClientProc ReadClient;
	TServerSupportFunctionProc ServerSupportFunction;
} ;
#pragma pack(pop)

struct HSE_VERSION_INFO;
typedef HSE_VERSION_INFO *PHSE_VERSION_INFO;

#pragma pack(push, 1)
struct HSE_VERSION_INFO
{
	unsigned dwExtensionVersion;
	char lpszExtensionDesc[256];
} ;
#pragma pack(pop)

typedef HSE_VERSION_INFO  THSE_VERSION_INFO;

typedef HSE_VERSION_INFO *LPHSE_VERSION_INFO;

typedef void __fastcall (__closure *TDX_GetServerVariable)(Dxservercore::TDXClientThread* ClientThread
	, AnsiString Variable, AnsiString &Results);

typedef void __fastcall (__closure *TDX_RedirectHeader)(Dxservercore::TDXClientThread* ClientThread, 
	AnsiString Location, AnsiString &Header);

typedef void __fastcall (__closure *TDX_BuildHeader)(int dwHttpStatusCode, AnsiString &Results);

typedef bool __stdcall (*TGetExtensionVersion)(HSE_VERSION_INFO &Ver);

typedef unsigned __stdcall (*THttpExtensionProc)(TEXTENSION_CONTROL_BLOCK &ECB);

typedef bool __stdcall (*TTerminateExtension)(unsigned dwFlags);

typedef void __fastcall (*TNotificationFiltersProc)(unsigned Notify, void * Buf1, unsigned sizeBuf1, 
	void * Buf2, unsigned sizeBuf2);

typedef bool __fastcall (__closure *TDX_FilterGetServerVariable)(Dxisapifilter::THTTP_FILTER_CONTEXT 
	&pfc, char * VariableName, void * Buffer, unsigned &Size);

class DELPHICLASS TDXISAPI;
class PASCALIMPLEMENTATION TDXISAPI : public Dxstring::TDXComponent 
{
	typedef Dxstring::TDXComponent inherited;
	
private:
	TDX_GetServerVariable fGetServerVariable;
	TDX_RedirectHeader fRedirectHeader;
	TDX_BuildHeader fBuildHeader;
	int fTimeout;
	
protected:
	void __fastcall RegisterFilters(AnsiString &ResultLog);
	void __fastcall UnRegisterFilters(AnsiString &ResultLog);
	void __fastcall SetServerVariableProc(Dxisapifilter::TFilterGetServerVariableProc value);
	Dxisapifilter::TFilterGetServerVariableProc __fastcall GetServerVariableProc();
	
public:
	void __fastcall ServerStartEvent(void);
	void __fastcall ServerStopEvent(void);
	void __fastcall ServerRawRead(AnsiString ReadString, unsigned Len);
	void __fastcall ServerPreprocHeaderEvent(Dxhttpheadertools::PHeaderInfo DXHeaderInfo);
	void __fastcall ConvertedURL2Physical(AnsiString URL, AnsiString Physical);
	void __fastcall ServerEndOfRequest(void);
	void __fastcall ServerEndSession(void);
	__fastcall virtual TDXISAPI(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXISAPI(void);
	bool __fastcall Execute(Dxservercore::TDXClientThread* Session, AnsiString ISAPI, AnsiString Method
		, AnsiString QueryString, AnsiString PathInfo, AnsiString PathTranslated, AnsiString POSTContent_Type
		, AnsiString POSTData, int POSTDataSize, AnsiString &ResultLog);
	bool __fastcall RegisterDLL(AnsiString ISAPI);
	bool __fastcall UnRegisterDLL(AnsiString ISAPI);
	bool __fastcall AddFilter(AnsiString ISAPI);
	bool __fastcall RemoveFilter(AnsiString ISAPI);
	int __fastcall FilterCount(void);
	
__published:
	__property TDX_GetServerVariable GetVariable = {read=fGetServerVariable, write=fGetServerVariable};
		
	__property Dxisapifilter::TFilterGetServerVariableProc GetVariableFilter = {read=GetServerVariableProc
		, write=SetServerVariableProc};
	__property TDX_RedirectHeader NeedHeaderForRedirect = {read=fRedirectHeader, write=fRedirectHeader}
		;
	__property TDX_BuildHeader NeedHeader = {read=fBuildHeader, write=fBuildHeader};
	__property int ReadSocketTimeout = {read=fTimeout, write=fTimeout, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const Shortint HSE_LOG_BUFFER_LEN = 0x50;
static const Word HSE_MAX_EXT_DLL_NAME_LEN = 0x100;

}	/* namespace Dxisapi */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxisapi;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXISAPI
