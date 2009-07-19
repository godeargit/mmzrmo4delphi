// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXHTTPHeaderTools.pas' rev: 5.00

#ifndef DXHTTPHeaderToolsHPP
#define DXHTTPHeaderToolsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <DXString.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxhttpheadertools
{
//-- type declarations -------------------------------------------------------
struct THeaderInfo;
typedef THeaderInfo *PHeaderInfo;

struct THeaderInfo
{
	int ContentLength;
	AnsiString Raw;
	AnsiString Protocol;
	AnsiString Method;
	AnsiString URI;
	AnsiString PhysPath;
	AnsiString Allow;
	AnsiString AuthType;
	AnsiString AuthName;
	AnsiString AuthPass;
	AnsiString Date;
	AnsiString Pragma;
	AnsiString CacheControl;
	AnsiString Connection;
	AnsiString TransferEncoding;
	AnsiString Upgrade;
	AnsiString Via;
	AnsiString Host;
	AnsiString From;
	AnsiString IfModSince;
	AnsiString IfMatch;
	AnsiString IfNoneMatch;
	AnsiString IfRange;
	AnsiString IfUnModSince;
	AnsiString MaxForwards;
	AnsiString ProxyAuthorization;
	AnsiString KeepAlive;
	AnsiString PublicCache;
	AnsiString Range;
	AnsiString Referer;
	AnsiString UserAgent;
	AnsiString ContentType;
	AnsiString Accept;
	AnsiString AcceptCharset;
	AnsiString AcceptEncoding;
	AnsiString AcceptLanguage;
	AnsiString ClientName;
	AnsiString ClientAddr;
	AnsiString ClientHost;
	AnsiString Cookie;
	AnsiString QueryString;
	AnsiString Weferer;
	AnsiString WserAgent;
	AnsiString Forwarded;
	AnsiString ForwardedFor;
	AnsiString ProxyConnection;
	AnsiString CacheInfo;
	AnsiString PostData;
	AnsiString HostRootPath;
	AnsiString All_RAW;
	AnsiString All_HTTP;
	AnsiString Unknown;
} ;

class DELPHICLASS TDXHTTPHeaderTools;
class PASCALIMPLEMENTATION TDXHTTPHeaderTools : public Dxstring::TDXComponent 
{
	typedef Dxstring::TDXComponent inherited;
	
private:
	THeaderInfo *fSessionHeader;
	
protected:
	AnsiString __fastcall FindUnknownHeader(AnsiString ServerVariable);
	void __fastcall ReplaceUnknownHeader(AnsiString ServerVariable, AnsiString VariableValue);
	bool __fastcall UnknownHeaderExists(AnsiString ServerVariable);
	
public:
	__fastcall virtual TDXHTTPHeaderTools(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXHTTPHeaderTools(void);
	void __fastcall Init(PHeaderInfo HTTPHeader);
	AnsiString __fastcall HeaderGetString(AnsiString ServerVariable);
	int __fastcall HeaderGetInteger(AnsiString ServerVariable);
	bool __fastcall HeaderDataExists(AnsiString ServerVariable);
	AnsiString __fastcall QueryGetString(AnsiString ServerVariable);
	int __fastcall QueryGetInteger(AnsiString ServerVariable);
	bool __fastcall QueryDataExists(AnsiString ServerVariable);
	bool __fastcall AddHeader(AnsiString ServerVariable, AnsiString VariableValue);
	bool __fastcall SetHeader(AnsiString ServerVariable, AnsiString VariableValue);
	Classes::TStrings* __fastcall ToStrings(void);
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dxhttpheadertools */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxhttpheadertools;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXHTTPHeaderTools
