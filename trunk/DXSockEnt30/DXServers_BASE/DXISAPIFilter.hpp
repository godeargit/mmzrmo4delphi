// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXISAPIFilter.pas' rev: 5.00

#ifndef DXISAPIFilterHPP
#define DXISAPIFilterHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxisapifilter
{
//-- type declarations -------------------------------------------------------
typedef unsigned SF_REQ_TYPE;

typedef unsigned SF_STATUS_TYPE;

struct THTTP_FILTER_CONTEXT;
typedef THTTP_FILTER_CONTEXT *PHTTP_FILTER_CONTEXT;

typedef bool __fastcall (__closure *TFilterGetServerVariableProc)(PHTTP_FILTER_CONTEXT pfc, char * VariableName
	, void * Buffer, unsigned &Size);

typedef bool __stdcall (*TFilterAddResponseHeadersProc)(void *pfc, char * lpszHeaders, unsigned dwReserved
	);

typedef bool __stdcall (*TFilterWriteClientProc)(void *pfc, void * Buffer, unsigned &Bytes, unsigned 
	dwReserved);

typedef void * __stdcall (*TFilterAllocMemProc)(void *pfc, unsigned cbSize, unsigned dwReserved);

typedef bool __stdcall (*TFilterServerSupportFunctionProc)(void *pfc, unsigned sfReq, void * pData, 
	unsigned ul1, unsigned ul2);

struct THTTP_FILTER_CONTEXT
{
	unsigned cbSize;
	unsigned Revision;
	void *ServerContext;
	unsigned ulReserved;
	bool fIsSecurePort;
	void *pFilterContext;
	TFilterGetServerVariableProc GetServerVariable;
	TFilterAddResponseHeadersProc AddResponseHeaders;
	TFilterWriteClientProc WriteClient;
	TFilterAllocMemProc AllocMem;
	TFilterServerSupportFunctionProc ServerSupportFunction;
} ;

struct THTTP_FILTER_RAW_DATA;
typedef THTTP_FILTER_RAW_DATA *PHTTP_FILTER_RAW_DATA;

struct THTTP_FILTER_RAW_DATA
{
	void *pvInData;
	unsigned cbInData;
	unsigned cbInBuffer;
	unsigned dwReserved;
} ;

typedef bool __stdcall (*TGetHeaderProc)(THTTP_FILTER_CONTEXT &pfc, char * lpszName, void *lpvBuffer
	, unsigned &lpdwSize);

typedef bool __stdcall (*TSetHeaderProc)(THTTP_FILTER_CONTEXT &pfc, char * lpszName, char * lpszValue
	);

typedef bool __stdcall (*TAddHeaderProc)(THTTP_FILTER_CONTEXT &pfc, char * lpszName, char * lpszValue
	);

struct THTTP_FILTER_SEND_RESPONSE
{
	TAddHeaderProc AddHeader;
	TSetHeaderProc SetHeader;
	TGetHeaderProc GetHeader;
	unsigned HttpStatus;
	unsigned dwReserved;
} ;

typedef THTTP_FILTER_SEND_RESPONSE *PHTTP_FILTER_SEND_RESPONSE;

struct THTTP_FILTER_PREPROC_HEADERS
{
	TGetHeaderProc GetHeader;
	TSetHeaderProc SetHeader;
	TAddHeaderProc AddHeader;
	unsigned HttpStatus;
	unsigned dwReserved;
} ;

typedef THTTP_FILTER_PREPROC_HEADERS *PHTTP_FILTER_PREPROC_HEADERS;

struct THTTP_FILTER_AUTHENT;
typedef THTTP_FILTER_AUTHENT *PHTTP_FILTER_AUTHENT;

struct THTTP_FILTER_AUTHENT
{
	char *pszUser;
	unsigned cbUserBuff;
	char *pszPassword;
	unsigned cbPasswordBuff;
} ;

struct THTTP_FILTER_URL_MAP;
typedef THTTP_FILTER_URL_MAP *PHTTP_FILTER_URL_MAP;

struct THTTP_FILTER_URL_MAP
{
	char *pszURL;
	char *pszPhysicalPath;
	unsigned cbPathBuff;
} ;

struct THTTP_FILTER_ACCESS_DENIED;
typedef THTTP_FILTER_ACCESS_DENIED *PHTTP_FILTER_ACCESS_DENIED;

struct THTTP_FILTER_ACCESS_DENIED
{
	char *pszURL;
	char *pszPhysicalPath;
	unsigned dwReason;
} ;

struct THTTP_FILTER_LOG;
typedef THTTP_FILTER_LOG *PHTTP_FILTER_LOG;

struct THTTP_FILTER_LOG
{
	char *pszClientHostName;
	char *pszClientUserName;
	char *pszServerName;
	char *pszOperation;
	char *pszTarget;
	char *pszParameters;
	unsigned dwHttpStatus;
	unsigned dwWin32Status;
	unsigned dwBytesSent;
	unsigned dwBytesRecvd;
	unsigned msTimeForProcessing;
} ;

struct THTTP_FILTER_VERSION;
typedef THTTP_FILTER_VERSION *PHTTP_FILTER_VERSION;

struct THTTP_FILTER_VERSION
{
	unsigned dwServerFilterVersion;
	unsigned dwFilterVersion;
	char lpszFilterDesc[257];
	unsigned dwFlags;
} ;

typedef unsigned __stdcall (*THttpFilterProc)(THTTP_FILTER_CONTEXT &pfc, unsigned Notificationtype, 
	void * pvNotification);

typedef bool __stdcall (*TGetFilterVersion)(THTTP_FILTER_VERSION &pVer);

typedef bool __stdcall (*TTerminateFilter)(unsigned flags);

//-- var, const, procedure ---------------------------------------------------
static const int HTTP_FILTER_REVISION = 0x40000;
static const Word SF_MAX_USERNAME = 0x101;
static const Word SF_MAX_PASSWORD = 0x101;
static const Word SF_MAX_FILTER_DESC_LEN = 0x101;
static const Shortint SF_REQ_SEND_RESPONSE_HEADER = 0x0;
static const Shortint SF_REQ_ADD_HEADERS_ON_DENIAL = 0x1;
static const Shortint SF_REQ_SET_NEXT_READ_SIZE = 0x2;
static const Shortint SF_REQ_SET_PROXY_INFO = 0x3;
static const Shortint SF_REQ_GET_CONNID = 0x4;
static const Shortint SF_REQ_DISABLE_NOTIFICATIONS = 0x5;
static const Shortint SF_REQ_GET_PROPERTY = 0x6;
static const Shortint SF_REQ_NORMALIZE_URL = 0x7;
static const int SF_STATUS_REQ_FINISHED = 0x8000000;
static const int SF_STATUS_REQ_FINISHED_KEEP_CONN = 0x8000001;
static const int SF_STATUS_REQ_NEXT_NOTIFICATION = 0x8000002;
static const int SF_STATUS_REQ_HANDLED_NOTIFICATION = 0x8000003;
static const int SF_STATUS_REQ_ERROR = 0x8000004;
static const int SF_STATUS_REQ_READ_NEXT = 0x8000005;
static const Shortint SF_DENIED_LOGON = 0x1;
static const Shortint SF_DENIED_RESOURCE = 0x2;
static const Shortint SF_DENIED_FILTER = 0x4;
static const Shortint SF_DENIED_APPLICATION = 0x8;
static const int SF_DENIED_BY_CONFIG = 0x10000;
static const Shortint SF_NOTIFY_SECURE_PORT = 0x1;
static const Shortint SF_NOTIFY_NONSECURE_PORT = 0x2;
static const Word SF_NOTIFY_READ_RAW_DATA = 0x8000;
static const Word SF_NOTIFY_PREPROC_HEADERS = 0x4000;
static const Word SF_NOTIFY_AUTHENTICATION = 0x2000;
static const Word SF_NOTIFY_URL_MAP = 0x1000;
static const Word SF_NOTIFY_ACCESS_DENIED = 0x800;
static const Word SF_NOTIFY_SEND_RAW_DATA = 0x400;
static const Word SF_NOTIFY_LOG = 0x200;
static const Word SF_NOTIFY_END_OF_NET_SESSION = 0x100;
static const Byte SF_NOTIFY_END_OF_REQUEST = 0x80;
static const int SF_NOTIFY_SEND_RESPONSE = 0x10000;
static const int SF_NOTIFY_ORDER_HIGH = 0x80000;
static const int SF_NOTIFY_ORDER_MEDIUM = 0x40000;
static const int SF_NOTIFY_ORDER_LOW = 0x20000;
static const int SF_NOTIFY_ORDER_DEFAULT = 0x20000;
static const int SF_NOTIFY_ORDER_MASK = 0xe0000;
static const Shortint SF_PROPERTY_INSTANCE_NUM_ID = 0x1;

}	/* namespace Dxisapifilter */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxisapifilter;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXISAPIFilter
