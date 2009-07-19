unit DXISAPIFilter;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXISAPIFilter
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2002
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2002
//               DX, Inc. Source may not be distributed without
//               written permission from both Brain Patchwork DX,
//               and DX, Inc.
//      License: (Reminder), None of this code can be added to other
//               developer products without permission. This includes
//               but not limited to DCU's, DCP's, DLL's, OCX's, or
//               any other form of merging our technologies. All of
//               your products released to a public consumer be it
//               shareware, freeware, commercial, etc. must contain a
//               license notification somewhere visible in the
//               application.
//               Example is Internet Explorer - Help->About screen
//               shows the licensed code contained in the application.
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description:
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

{$IFDEF VER100}
Type
   Longword=Cardinal;
{$ENDIF}

Const
  HTTP_FILTER_REVISION    = $00040000;

  SF_MAX_USERNAME         = (256+1);
  SF_MAX_PASSWORD         = (256+1);

  SF_MAX_FILTER_DESC_LEN  = (256+1);

  //
  //  These values can be used with the pfnSFCallback function supplied in
  //  the filter context structure
  //

  //
  //  Sends a complete HTTP server response header including
  //  the status, server version, message time and MIME version.
  //
  //  Server extensions should append other information at the end,
  //  such as Content-type, Content-length etc followed by an extra
  //  '\r\n'.
  //
  //  pData - Zero terminated string pointing to optional
  //      status string (i.e., "401 Access Denied") or NULL for
  //      the default response of "200 OK".
  //
  //  ul1 - Zero terminated string pointing to optional data to be
  //      appended and set with the header.  If NULL, the header will
  //      be terminated with an empty line.
  //

  SF_REQ_SEND_RESPONSE_HEADER = 0;
  {.$EXTERNALSYM SF_REQ_SEND_RESPONSE_HEADER}

  //
  //  If the server denies the HTTP request, add the specified headers
  //  to the server error response.
  //
  //  This allows an authentication filter to advertise its services
  //  w/o filtering every request.  Generally the headers will be
  //  WWW-Authenticate headers with custom authentication schemes but
  //  no restriction is placed on what headers may be specified.
  //
  //  pData - Zero terminated string pointing to one or more header lines
  //      with terminating '\r\n'.
  //

  SF_REQ_ADD_HEADERS_ON_DENIAL = 1;
  {.$EXTERNALSYM SF_REQ_ADD_HEADERS_ON_DENIAL}

  //
  //  Only used by raw data filters that return SF_STATUS_READ_NEXT
  //
  //  ul1 - size in bytes for the next read
  //

  SF_REQ_SET_NEXT_READ_SIZE = 2;
  {.$EXTERNALSYM SF_REQ_SET_NEXT_READ_SIZE}

  //
  //  Used to indicate this request is a proxy request
  //
  //  ul1 - The proxy flags to set
  //      0x00000001 - This is a HTTP proxy request
  //
  //

  SF_REQ_SET_PROXY_INFO = 3;
  {.$EXTERNALSYM SF_REQ_SET_PROXY_INFO}

  SF_REQ_GET_CONNID = 4;
  {.$EXTERNALSYM SF_REQ_GET_CONNID}

  SF_REQ_DISABLE_NOTIFICATIONS = 5;
  {.$EXTERNALSYM SF_REQ_DISABLE_NOTIFICATIONS}

  SF_REQ_GET_PROPERTY = 6;
  {.$EXTERNALSYM SF_REQ_GET_PROPERTY}

  SF_REQ_NORMALIZE_URL = 7;
  {.$EXTERNALSYM SF_REQ_NORMALIZE_URL}

  //
  //  These values are returned by the filter entry point when a new request is
  //  received indicating their interest in this particular request
  //

  //
  //  The filter has handled the HTTP request.  The server should disconnect
  //  the session.
  //

  SF_STATUS_REQ_FINISHED = $8000000;
  {.$EXTERNALSYM SF_STATUS_REQ_FINISHED}

  //
  //  Same as SF_STATUS_FINISHED except the server should keep the TCP
  //  session open if the option was negotiated
  //

  SF_STATUS_REQ_FINISHED_KEEP_CONN = $8000001;
  {.$EXTERNALSYM SF_STATUS_REQ_FINISHED_KEEP_CONN}

  //
  //  The next filter in the notification chain should be called
  //

  SF_STATUS_REQ_NEXT_NOTIFICATION = $8000002;
  {.$EXTERNALSYM SF_STATUS_REQ_NEXT_NOTIFICATION}

  //
  //  This filter handled the notification.  No other handles should be
  //  called for this particular notification type
  //

  SF_STATUS_REQ_HANDLED_NOTIFICATION = $8000003;
  {.$EXTERNALSYM SF_STATUS_REQ_HANDLED_NOTIFICATION}

  //
  //  An error occurred.  The server should use GetLastError() and indicate
  //  the error to the client
  //

  SF_STATUS_REQ_ERROR = $8000004;
  {.$EXTERNALSYM SF_STATUS_REQ_ERROR}

  //
  //  The filter is an opaque stream filter and we're negotiating the
  //  session parameters.  Only valid for raw read notification.
  //

  SF_STATUS_REQ_READ_NEXT = $8000005;
  {.$EXTERNALSYM SF_STATUS_REQ_READ_NEXT}

  //
  //  Bitfield indicating the requested resource has been denied by the server due
  //  to a logon failure, an ACL on a resource, an ISAPI Filter or an
  //  ISAPI Application/CGI Application.
  //
  //  SF_DENIED_BY_CONFIG can appear with SF_DENIED_LOGON if the server
  //  configuration did not allow the user to logon.
  //

  SF_DENIED_LOGON             = $00000001;
  {.$EXTERNALSYM SF_DENIED_LOGON}
  SF_DENIED_RESOURCE          = $00000002;
  {.$EXTERNALSYM SF_DENIED_RESOURCE}
  SF_DENIED_FILTER            = $00000004;
  {.$EXTERNALSYM SF_DENIED_FILTER}
  SF_DENIED_APPLICATION       = $00000008;
  {.$EXTERNALSYM SF_DENIED_APPLICATION}
  SF_DENIED_BY_CONFIG         = $00010000;
  {.$EXTERNALSYM SF_DENIED_BY_CONFIG}


  //
  //  Notification Flags
  //
  //  SF_NOTIFY_SECURE_PORT
  //  SF_NOTIFY_NONSECURE_PORT
  //
  //      Indicates whether the application wants to be notified for transactions
  //      that are happenning on the server port(s) that support data encryption
  //      (such as PCT and SSL), on only the non-secure port(s) or both.
  //
  //  SF_NOTIFY_READ_RAW_DATA
  //
  //      Applications are notified after the server reads a block of memory
  //      from the client but before the server does any processing on the
  //      block.  The data block may contain HTTP headers and entity data.
  //
  //
  //

  SF_NOTIFY_SECURE_PORT               = $00000001;
  {.$EXTERNALSYM SF_NOTIFY_SECURE_PORT}
  SF_NOTIFY_NONSECURE_PORT            = $00000002;
  {.$EXTERNALSYM SF_NOTIFY_NONSECURE_PORT}
  SF_NOTIFY_READ_RAW_DATA             = $00008000;
  {.$EXTERNALSYM SF_NOTIFY_READ_RAW_DATA}
  SF_NOTIFY_PREPROC_HEADERS           = $00004000;
  {.$EXTERNALSYM SF_NOTIFY_PREPROC_HEADERS}
  SF_NOTIFY_AUTHENTICATION            = $00002000;
  {.$EXTERNALSYM SF_NOTIFY_AUTHENTICATION}
  SF_NOTIFY_URL_MAP                   = $00001000;
  {.$EXTERNALSYM SF_NOTIFY_URL_MAP}
  SF_NOTIFY_ACCESS_DENIED             = $00000800;
  {.$EXTERNALSYM SF_NOTIFY_ACCESS_DENIED}
  SF_NOTIFY_SEND_RAW_DATA             = $00000400;
  {.$EXTERNALSYM SF_NOTIFY_SEND_RAW_DATA}
  SF_NOTIFY_LOG                       = $00000200;
  {.$EXTERNALSYM SF_NOTIFY_LOG}
  SF_NOTIFY_END_OF_NET_SESSION        = $00000100;
  {.$EXTERNALSYM SF_NOTIFY_END_OF_NET_SESSION}
  SF_NOTIFY_END_OF_REQUEST =            $00000080;
  {.$EXTERNALSYM SF_NOTIFY_END_OF_REQUEST}
  SF_NOTIFY_SEND_RESPONSE =             $00010000;
  {.$EXTERNALSYM SF_NOTIFY_SEND_RESPONSE}

  //
  //  Filter ordering flags
  //
  //  Filters will tend to be notified by their specified
  //  ordering.  For ties, notification order is determined by load order.
  //
  //  SF_NOTIFY_ORDER_HIGH - Authentication or data transformation filters
  //  SF_NOTIFY_ORDER_MEDIUM
  //  SF_NOTIFY_ORDER_LOW  - Logging filters that want the results of any other
  //                      filters might specify this order.
  //

  SF_NOTIFY_ORDER_HIGH               = $00080000;
  {.$EXTERNALSYM SF_NOTIFY_ORDER_HIGH}
  SF_NOTIFY_ORDER_MEDIUM             = $00040000;
  {.$EXTERNALSYM SF_NOTIFY_ORDER_MEDIUM}
  SF_NOTIFY_ORDER_LOW                = $00020000;
  {.$EXTERNALSYM SF_NOTIFY_ORDER_LOW}
  SF_NOTIFY_ORDER_DEFAULT            = SF_NOTIFY_ORDER_LOW;
  {.$EXTERNALSYM SF_NOTIFY_ORDER_DEFAULT}
  SF_NOTIFY_ORDER_MASK               = SF_NOTIFY_ORDER_HIGH or
                                       SF_NOTIFY_ORDER_MEDIUM or
                                       SF_NOTIFY_ORDER_LOW;
  {.$EXTERNALSYM SF_NOTIFY_ORDER_MASK}

  SF_PROPERTY_INSTANCE_NUM_ID = 1;
  {.$EXTERNALSYM SF_PROPERTY_INSTANCE_NUM_ID}
//
//  pvNotification points to this structure for all request notification types
//

Type
   SF_REQ_TYPE = LONGWORD;
   SF_STATUS_TYPE = LONGWORD;

  PHTTP_FILTER_CONTEXT = ^THTTP_FILTER_CONTEXT;
  TFilterGetServerVariableProc = function (pfc: PHTTP_FILTER_CONTEXT;
    VariableName: PChar; Buffer: Pointer; var Size: LONGWORD ): BOOLEAN of Object;
//  TFilterGetServerVariableProc = function (var pfc{: THTTP_FILTER_CONTEXT};
//    VariableName: PChar; Buffer: Pointer; var Size: DWORD ): BOOL of Object;

  TFilterAddResponseHeadersProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    lpszHeaders: PChar; dwReserved: Longword): BOOLEAN stdcall;

  TFilterWriteClientProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    Buffer: Pointer; var Bytes: Longword; dwReserved: Longword ): BOOLEAN stdcall;

  TFilterAllocMemProc = function (var pfc{: THTTP_FILTER_CONTEXT}; cbSize: Longword;
    dwReserved: Longword): Pointer stdcall;

  TFilterServerSupportFunctionProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    sfReq: Longword; pData: Pointer; ul1, ul2: Longword): BOOLEAN stdcall;

  THTTP_FILTER_CONTEXT = record
    cbSize: LONGWORD;

    //
    //  This is the structure revision level.
    //

    Revision: LONGWORD;

    //
    //  Private context information for the server.
    //

    ServerContext: Pointer;
    ulReserved: LONGWORD;

    //
    //  TRUE if this request is coming over a secure port
    //

    fIsSecurePort: BOOLEAN;

    //
    //  A context that can be used by the filter
    //

    pFilterContext: Pointer;

    //
    //  Server callbacks
    //

    GetServerVariable: TFilterGetServerVariableProc;
    AddResponseHeaders: TFilterAddResponseHeadersProc;
    WriteClient: TFilterWriteClientProc;
    AllocMem: TFilterAllocMemProc;
    ServerSupportFunction: TFilterServerSupportFunctionProc;
  end;

  PHTTP_FILTER_RAW_DATA = ^THTTP_FILTER_RAW_DATA;
  THTTP_FILTER_RAW_DATA = record
    //
    //  This is a pointer to the data for the filter to process.
    //
    pvInData: Pointer;
    cbInData: LONGWORD;       // Number of valid data bytes
    cbInBuffer: LONGWORD;     // Total size of buffer
    dwReserved: LONGWORD;
  end;

  //
  //  This structure is the notification info for when the server is about to
  //  process the client headers
  //

  TGetHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName: PChar;
    var lpvBuffer; var lpdwSize: Longword): BOOLEAN stdcall;

  TSetHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName,
    lpszValue: PChar): BOOLEAN stdcall;

  TAddHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName,
    lpszValue: PChar): BOOLEAN stdcall;


   PHTTP_FILTER_SEND_RESPONSE = ^THTTP_FILTER_SEND_RESPONSE;
   THTTP_FILTER_SEND_RESPONSE = record
      AddHeader:TAddHeaderProc;
      SetHeader:TSetHeaderProc;
      GetHeader:TGetHeaderProc;
      HttpStatus:LONGWORD;
      dwReserved:LONGWORD;
   End;


  PHTTP_FILTER_PREPROC_HEADERS = ^THTTP_FILTER_PREPROC_HEADERS;
  THTTP_FILTER_PREPROC_HEADERS = record
    //
    //  Retrieves the specified header value.  Header names should include
    //  the trailing ':'.  The special values 'method', 'url' and 'version'
    //  can be used to retrieve the individual portions of the request line
    //

    GetHeader: TGetHeaderProc;

    //
    //  Replaces this header value to the specified value.  To delete a header,
    //  specified a value of '\0'.
    //

    SetHeader: TSetHeaderProc;

    //
    //  Adds the specified header and value
    //

    AddHeader: TAddHeaderProc;

    HttpStatus: LONGWORD;

    dwReserved: LONGWORD;
  end;


  //
  //  Authentication information for this request.
  //

  PHTTP_FILTER_AUTHENT = ^THTTP_FILTER_AUTHENT;
  THTTP_FILTER_AUTHENT = record
    //
    //  Pointer to username and password, empty strings for the anonymous user
    //
    //  Client's can overwrite these buffers which are guaranteed to be at
    //  least SF_MAX_USERNAME and SF_MAX_PASSWORD bytes large.
    //

    pszUser: PChar;
    cbUserBuff: LONGWORD;

    pszPassword: PChar;
    cbPasswordBuff: LONGWORD;
  end;

  //
  //  Indicates the server is going to use the specific physical mapping for
  //  the specified URL.  Filters can modify the physical path in place.
  //

  PHTTP_FILTER_URL_MAP = ^THTTP_FILTER_URL_MAP;
  THTTP_FILTER_URL_MAP = record
    pszURL: PChar;
    pszPhysicalPath: PChar;
    cbPathBuff: LONGWORD;
  end;

  PHTTP_FILTER_ACCESS_DENIED = ^THTTP_FILTER_ACCESS_DENIED;
  THTTP_FILTER_ACCESS_DENIED = record
    pszURL: PChar;            // Requesting URL
    pszPhysicalPath: PChar;   // Physical path of resource
    dwReason: LONGWORD;          // Bitfield of SF_DENIED flags
  end;

  //
  //  The log information about to be written to the server log file.  The
  //  string pointers can be replaced but the memory must remain valid until
  //  the next notification
  //

  PHTTP_FILTER_LOG = ^THTTP_FILTER_LOG;
  THTTP_FILTER_LOG = record
    pszClientHostName: PChar;
    pszClientUserName: PChar;
    pszServerName: PChar;
    pszOperation: PChar;
    pszTarget: PChar;
    pszParameters: PChar;
    dwHttpStatus: LONGWORD;
    dwWin32Status: LONGWORD;
    dwBytesSent:LONGWORD;
    dwBytesRecvd:LONGWORD;
    msTimeForProcessing:LONGWORD;
  end;

  //
  //  Filter version information, passed to GetFilterVersion
  //
  PHTTP_FILTER_VERSION = ^THTTP_FILTER_VERSION;
  THTTP_FILTER_VERSION = record
    //
    //  Version of the spec the server is using
    //

    dwServerFilterVersion: LONGWORD;

    //
    //  Fields specified by the client
    //
    dwFilterVersion: LONGWORD;
    lpszFilterDesc: array[0..SF_MAX_FILTER_DESC_LEN - 1] of Char;
    dwFlags: LONGWORD;
  end;


  THttpFilterProc = function (var pfc: THTTP_FILTER_CONTEXT;
    Notificationtype: LONGWORD; pvNotification: Pointer): LONGWORD stdcall;

  TGetFilterVersion = function (var pVer: THTTP_FILTER_VERSION): BOOLEAN stdcall;
  
  TTerminateFilter = function(flags:LONGWORD): BOOLEAN stdcall;

implementation

end.
