unit DXISAPI;

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXISAPI
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
// IMPLEMENTATION OF ISAPI 4.0 SPECIFICATIONS (FEBRUARY 2002)
// Create one instance of the TDXIAPI component in your application. To use:
// TDXISAPI.RegisterDLL(Path+Filename) - Registers the DLL with our DLLManager!
// TDXISAPI.Execute(TDXClientThread;Path+Filename) - Executes the DLL
// TDXISAPI.UnregisterDLL(Path+Filename) - removes from our DLLManager.
//
// You must specify the following events!
// TDXISAPI.GetServerVariable(Session:TDXClientThread;Variable:String;Var Results:String);
//
// All other ISAPI callbacks are actually handled internal to this component.
//
// To achieve HIGH-SPEED, register all known DLL's at the startup of your
// application, and then on termination unregister all of these DLL's. Then
// all you have to do is execute the DLL as needed.
//
// MSDN:
// Important - It is vital that the work performed by ISAPI filters is minimized,
// because the effects on scalability and performance is potentially quite severe.
// For example, if your ISAPI filter implemented your own custom encryption scheme,
// the encryption and decryption would need to take place within your ISAPI filter
// code, but the reading and writing of the data should be handled by IIS.
///////////////////////////////////////////////////////////////////////////////

interface

{.$DEFINE USE_DLL_MGR}
{$DEFINE SUPPORT_COM}

Uses
{$IFNDEF LINUX}
  Windows,
{$ENDIF}
{$IFDEF SUPPORT_COM}
  ActiveX,
{$ENDIF}
  Classes,
  DXString,
  DXServerCore,
{$IFDEF USE_DLL_MGR}
  DXDLLManager,
{$ENDIF}
  DXSecurity,
  DXISAPIFilter,
  DXHTTPHeaderTools;

{$I DXSock.def}

Type
   HCONN=Integer;
{$IFDEF VER100}
   Longword=Cardinal;
{$ENDIF}
{$IFDEF LINUX}
   LPDWord=^Longword;
   LPCSTR=PChar;
{$ENDIF}

Const
   HSE_LOG_BUFFER_LEN=80;
   HSE_MAX_EXT_DLL_NAME_LEN=256;

type
  TGetServerVariableProc = function ( hConn: HCONN;
                                      VariableName: PChar;
                                      Buffer: Pointer;
                                      Size:LPDWord): BOOLEAN stdcall;

  TWriteClientProc = function ( ConnID: HCONN;
                                Buffer: Pointer;
                                var Bytes: Longword;
                                dwReserved: Longword ): BOOLEAN stdcall;

  TReadClientProc = function ( ConnID: HCONN;
                               Buffer: Pointer;
                               var Size: Longword ): BOOLEAN stdcall;

  TServerSupportFunctionProc = function ( hConn: HCONN;
                                          HSERRequest: Longword;
                                          Buffer: Pointer;
                                          Size:Pointer{LPDWord};
                                          DataType: Pointer{LPDWord} ): BOOLEAN stdcall;

//
// passed to extension procedure on a new request
//
type

  PEXTENSION_CONTROL_BLOCK = ^TEXTENSION_CONTROL_BLOCK;
  TEXTENSION_CONTROL_BLOCK = packed record
    cbSize: Longword;                    // size of this struct.
    dwVersion: Longword;                 // version info of this spec
    ConnID: HCONN;                    // Context number not to be modified!
    dwHttpStatusCode: Longword;          // HTTP Status code
                     // null terminated log info specific to this Extension DLL
    lpszLogData: array [0..HSE_LOG_BUFFER_LEN-1] of Char;
    lpszMethod: PChar;                // REQUEST_METHOD
    lpszQueryString: PChar;           // QUERY_STRING
    lpszPathInfo: PChar;              // PATH_INFO
    lpszPathTranslated: PChar;        // PATH_TRANSLATED
    cbTotalBytes: Longword;              // Total bytes indicated from client
    cbAvailable: Longword;               // Available number of bytes
    lpbData: Pointer;                 // pointer to cbAvailable bytes
    lpszContentType: PChar;           // Content type of client data

    GetServerVariable: TGetServerVariableProc;
    WriteClient: TWriteClientProc;
    ReadClient: TReadClientProc;
    ServerSupportFunction: TServerSupportFunctionProc;
  end;
  PHSE_VERSION_INFO = ^HSE_VERSION_INFO;
  HSE_VERSION_INFO = packed record
    dwExtensionVersion: Longword;
    lpszExtensionDesc: array [0..HSE_MAX_EXT_DLL_NAME_LEN-1] of Char;
  end;
  THSE_VERSION_INFO = HSE_VERSION_INFO;
  LPHSE_VERSION_INFO = PHSE_VERSION_INFO;

Type
   TDX_GetServerVariable=procedure(ClientThread:TDXClientThread;Variable:String;Var Results:String) of object;
   TDX_RedirectHeader=Procedure(ClientThread:TDXClientThread;Location:String;Var Header:String) of object;
   TDX_BuildHeader=Procedure(dwHttpStatusCode:Integer;var Results:String) of object;

   //Extension Function
   TGetExtensionVersion = function (var Ver: THSE_VERSION_INFO): BOOLEAN stdcall;
   THttpExtensionProc = function (var ECB: TEXTENSION_CONTROL_BLOCK): Longword stdcall;
   TTerminateExtension = function (dwFlags: Longword): BOOLEAN stdcall;

   TNotificationFiltersProc = Procedure (Notify:Longword; Buf1:Pointer;
                        sizeBuf1:Longword; Buf2:Pointer; sizeBuf2:Longword);

   TDX_FilterGetServerVariable = function (var pfc: THTTP_FILTER_CONTEXT;
    VariableName: PChar; Buffer: Pointer; var Size: Longword ): BOOLEAN of Object;

///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This structure is used to send certificate information to the
// ISAPI extension when the HSE_REQ_GET_CERT_INFO_EX value is set for the
// dwHSERRequest parameter of the ServerSupportFunction.
///////////////////////////////////////////////////////////////////////////////


type
  TDXISAPI = class(TDXComponent)
  private
    fGetServerVariable:TDX_GetServerVariable;
    fRedirectHeader:TDX_RedirectHeader;
    fBuildHeader:TDX_BuildHeader;
    fTimeout:Integer;
//    fCertificate:TCERT_CONTEXT_EX;
  protected
    Procedure RegisterFilters(var ResultLog:String);
    Procedure UnRegisterFilters(var ResultLog:String);
    Procedure SetServerVariableProc(value:TFilterGetServerVariableProc);
    Function GetServerVariableProc:TFilterGetServerVariableProc;
  public
    //////////////////////////////
    // Server Event Procedure
    // Not of use to end-user.
    //////////////////////////////
    Procedure ServerStartEvent;
    Procedure ServerStopEvent;
    Procedure ServerRawRead(ReadString:String;Len:Longword);
    Procedure ServerPreprocHeaderEvent(DXHeaderInfo:PHeaderInfo);
    Procedure ConvertedURL2Physical(URL:String;Physical:String);
    //////////////////////////////
    // there are more to be added between converted and end of request in 2.4
    //////////////////////////////
    Procedure ServerEndOfRequest;
    Procedure ServerEndSession;

  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    Destructor Destroy; override;
    Function Execute(Session:TDXClientThread;ISAPI,Method,QueryString,
                     PathInfo,PathTranslated,POSTContent_Type,POSTData:String;
                     POSTDataSize:Integer;Var ResultLog:String):Boolean;

    Function RegisterDLL(ISAPI:String):Boolean;
    Function UnRegisterDLL(ISAPI:String):Boolean;

    Function AddFilter(ISAPI:String):Boolean;
    Function RemoveFilter(ISAPI:String):Boolean;
    Function FilterCount:Integer;

  published
    Property GetVariable:TDX_GetServerVariable read fGetServerVariable
                                               write fGetServerVariable;
    Property GetVariableFilter:TFilterGetServerVariableProc read GetServerVariableProc
                                               write SetServerVariableProc;
    Property NeedHeaderForRedirect:TDX_RedirectHeader read fRedirectHeader
                                               write fRedirectHeader;
    Property NeedHeader:TDX_BuildHeader read fBuildHeader
                                        write fBuildHeader;
    Property ReadSocketTimeout:Integer read fTimeout
                                       write fTimeout;
  end;

Implementation

Uses
{$IFDEF LINUX}
   Libc,
{$ENDIF}
   SysUtils, {StrPCopy}
   DXHTTPServerCore;

Const
{from Microsoft's .h files for ISAPI!}
   HSE_VERSION_MAJOR=5;
   HSE_VERSION_MINOR=1;
   HSE_VERSION=$0501;
   HSE_REQ_END_RESERVED=1000;
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// The extension has finished processing and the server should disconnect the
// client and free up allocated resources.
///////////////////////////////////////////////////////////////////////////////
   HSE_STATUS_SUCCESS=1;
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// The extension has finished processing and the server should wait for the next
// HTTP request if the client supports Keep-Alive connections. The extension
// can return this only if it was able to send the correct Content-Length header
// to the client.
///////////////////////////////////////////////////////////////////////////////
   HSE_STATUS_SUCCESS_AND_KEEP_CONN=2;
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// The extension has queued the request for processing and will notify the
// server when it has finished. See HSE_REQ_DONE_WITH_SESSION under
// ServerSupportFunction.
///////////////////////////////////////////////////////////////////////////////
   HSE_STATUS_PENDING=3;
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// The extension has encountered an error while processing the request, so the
// server can disconnect the client and free up allocated resources. An HTTP
// status code of 500 is written to the IIS log for the request.
///////////////////////////////////////////////////////////////////////////////
   HSE_STATUS_ERROR=4;
// The following are the values to request services with the
//   ServerSupportFunction().
//  Values from 0 to 1000 are reserved for future versions of the interface
   HSE_REQ_BASE=0;
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function allows your ISAPI extension to redirect a client
// browser to a different URL from the one they initially requested. When this
// support function is called, you must provide the new URL. IIS then sends the
// HTTP status code 302 (URL Redirect) to the client browser.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_SEND_URL_REDIRECT_RESP=( HSE_REQ_BASE + 1 ); // ISAPI 2.0/3.0
   HSE_REQ_SEND_URL=( HSE_REQ_BASE + 2 ); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function allows you to request that IIS send a complete HTTP
// response header to the client browser, including the HTTP status, server
// version, message time, and MIME version. Your extension can also, optionally,
// append other header information to the end of IIS-generated header, such as
// Content-Type or Content-Length.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_SEND_RESPONSE_HEADER=( HSE_REQ_BASE + 3 ); // ISAPI 2.0/3.0 // depreciated
   HSE_REQ_SEND_RESPONSE_HEADER_EX=(HSE_REQ_END_RESERVED+16); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// If your extension is performing some form of extended processing, and its
// HttpExtensionProc entry-point function has returned the status code
// HSE_STATUS_PENDING, then your extension should notify IIS when all
// processing is complete by using this support function. Calling this function
// will terminate the session connection.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_DONE_WITH_SESSION=( HSE_REQ_BASE + 4 ); // ISAPI 2.0/3.0

//  These are Microsoft specific extensions

///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function allows your ISAPI extension to map a logical URL path
// to a physical path.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_MAP_URL_TO_PATH=(HSE_REQ_END_RESERVED+1); // ISAPI 2.0/3.0
   HSE_REQ_MAP_URL_TO_PATH_EX=(HSE_REQ_END_RESERVED+12); // ISAPI 4.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function allows your ISAPI extension to retrieve context and
// credential handles to the CtxtHandle and CredHandle structures, as defined
// in the header file sspi.h. Once these handles are retrieved, they can be
// used to query or manipulate the server certificate information by using the
// standard certificate APIs that are also defined in sspi.h.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_GET_SSPI_INFO=(HSE_REQ_END_RESERVED+2); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// You can use this support function to write your own custom log strings to
// the log record. When this function is called, the string contained in the
// buffer you specify is appended to the log record for the current HTTP
// request.
///////////////////////////////////////////////////////////////////////////////
   HSE_APPEND_LOG_PARAMETER=(HSE_REQ_END_RESERVED+3); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// Using this support function, you can set a special callback function that
// will be used for handling asynchronous I/O operations.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_IO_COMPLETION=(HSE_REQ_END_RESERVED+5); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function enables your ISAPI extension to call the
// high-performance Win32® API TransmitFile function to send a file to the
// client browser. This function accepts as a parameter a pointer to a
// structure, HSE_TF_INFO, so that you can specify the file handle of the file
// to be sent, HTTP headers for the response, and other important information.
//
// TransmitFile is carried out asynchronously, so your extension must specify a
// special callback function so IIS can notify your extension when the
// asynchronous write operation has completed. You can do this either by using
// the pfnHseIO member of the HSE_TF_INFO used in the function call, or by using
// the support function HSE_REQ_IO_COMPLETION.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_TRANSMIT_FILE=(HSE_REQ_END_RESERVED+6); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function forces IIS to reprocess the discretionary access
// control list (DACL) for an ISAPI extension's DLL. Given sufficient security
// permissions, this support function can be used to reprocess another
// extension's DACL, as well.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_REFRESH_ISAPI_ACL=(HSE_REQ_END_RESERVED+7); // ISAPI 2.0/3.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function can be used to determine the Keep-Alive status of the
// current connection.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_IS_KEEP_CONN=(HSE_REQ_END_RESERVED+8);
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// Using this support function, your ISAPI extension can request that IIS
// attempt to read from the client asynchronously. In order for this function
// to work properly, however, you must have specified a special callback
// function, which IIS will call when the asynchronous read has completed. This
// callback function can be set using the ServerSupportFunction request
// HSE_REQ_IO_COMPLETION.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_ASYNC_READ_CLIENT=(HSE_REQ_END_RESERVED+10);
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function allows you to retrieve a handle to the impersonation
// token that the request is using. An impersonation token represents a user
// context.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_GET_IMPERSONATION_TOKEN=(HSE_REQ_END_RESERVED+11); // ISAPI 4.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function can be used to request that, when disconnecting the
// current connection, IIS will use an abortive shutdown sequence to close the
// TCP/IP connection socket.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_ABORTIVE_CLOSE=(HSE_REQ_END_RESERVED+14);
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This support function specifies the certificate context for the first
// certificate in the client's certificate chain.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_GET_CERT_INFO_EX=(HSE_REQ_END_RESERVED+15); // ISAPI 4.0
///////////////////////////////////////////////////////////////////////////////
// MSDN:
// This server support function sends a request to IIS to close the current
// client socket connection, even if there is an asynchronous read pending.
///////////////////////////////////////////////////////////////////////////////
   HSE_REQ_CLOSE_CONNECTION=(HSE_REQ_END_RESERVED+17); // ISAPI 5.0/5.1

   HSE_TERM_MUST_UNLOAD=2;

Type
   PHSE_SEND_HEADER_EX_INFO=^HSE_SEND_HEADER_EX_INFO;
   HSE_SEND_HEADER_EX_INFO=Record
      pszStatus:LPCSTR;   // HTTP status code; for example "200 OK".
      pszHeader:LPCSTR;  // HTTP header.
      cchStatus:Longword;   // Number of characters in status code.
      cchHeader:Longword;   // Number of characters in header.
      fKeepConn:BOOLEAN;    // Keep client connection alive?
   End;
Const
   MAX_PATH = (256);

Type
   PCERT_CONTEXT_EX=^TCERT_CONTEXT_EX;
   TCERT_CONTEXT_EX = packed record
      CertContext:TCERT_CONTEXT;
      cbAllocated:Longword;
      dwCertificateFlags:Longword;
   End;
   PHSE_URL_MAPEX_INFO = ^THSE_URL_MAPEX_INFO;
   THSE_URL_MAPEX_INFO = record
      lpszPath: array [0..MAX_PATH-1] of Char; // Physical path mapped to by the URL.
      dwFlags:Longword;              // Flags associated with this URL path.
      cchMatchingPath:Longword;    // Number of matching characters in
      cchMatchingURL:Longword;     // Number of matching characters in URL.
      dwReserved1:Longword;
      dwReserved2:Longword;
   End;
   PDXThreadRecord=^TDXThreadRecord;
   TDXThreadRecord=record
      fAllocMem:TList;
      fToken:Cardinal;
      fDopHeaders:String;
      fDopDeniedHeaders:String;
      fHeaderInfo:PHeaderInfo;
      ThreadID:HCONN;
      Thread:TDXClientThread;
      fGetServerVariable:TDX_GetServerVariable;
      fRedirectHeader:TDX_RedirectHeader;
      fBuildHeader:TDX_BuildHeader;
      fiTimeout:Integer;
      ECB:TEXTENSION_CONTROL_BLOCK;
      mapex:THSE_URL_MAPEX_INFO;
      fRealPath:String; // 3.0
      fISAPIThreadPoolDone:Boolean;
   end;
   PDXFilterRecord = ^TDXFilterRecord;
   TDXFilterRecord = record
      fName:String;
      fVersion:PHTTP_FILTER_VERSION;
      fNotification:Boolean;
      fDisableNotification:Longword;
      fNextReadSize:Longword;
   End;
   PDXMemoryBlock= ^TDXMemoryBlock;
   TDXMemoryBlock = record
      fPointer:Pointer;
      fSize:Longword;
   End;

Var
   fDXThreadArray:TList;
   fActiveFilter:PDXFilterRecord;
   fFilter:TList;
{$IFDEF USE_DLL_MGR}
   fDLLManager:TDXDLLManager;
{$ENDIF}
   fPreprocHeader:PHeaderInfo;
   fGetServerVariableFilterProc:TFilterGetServerVariableProc;

constructor TDXISAPI.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
{$IFDEF USE_DLL_MGR}
   fDLLManager:=TDXDLLManager.Create(Nil);
{$ENDIF}
   fDXThreadArray:=TList.Create;
   fFilter:=TList.Create;
   fTimeout:=30000;
end;

destructor TDXISAPI.Destroy;
Var
   DXThreadRecord:PDXThreadRecord;
   DXMemoryBlock:PDXMemoryBlock;

begin
   MyCriticalSection.StartingWrite;
   If Assigned(fDXThreadArray) then Begin
      While fDXThreadArray.Count>0 do Begin
         DXThreadRecord:=fDXThreadArray[0];
         //Free memory for FilterAllocMem
         while DXThreadRecord.fAllocMem.Count>0 do Begin
            DXMemoryBlock:=DXThreadRecord.fAllocMem[0];
            FreeMem(DXMemoryBlock.fPointer, DXMemoryBlock.fSize);
            DXThreadRecord.fAllocMem.Delete(0);
         End;
         Dispose(DXThreadRecord);
         fDXThreadArray.Delete(0);
      End;
      fDXThreadArray.Free;
      fDXThreadArray := Nil;
   End;
   MyCriticalSection.FinishedWrite;

{$IFDEF USE_DLL_MGR}
   If Assigned(fDLLManager) then Begin
      fDLLManager.Free;
      fDLLManager := Nil;
   End;
{$ENDIF}

   If Assigned(fFilter) then Begin
      fFilter.Free;
      fFilter := Nil;
   End;

   inherited destroy;
end;

Function FindThread(FindHCONN:HCONN):PDXThreadRecord;
Var
   Loop:Integer;
   DXThreadRecord:PDXThreadRecord;

Begin
   Result:=Nil;
   For Loop:=fDXThreadArray.Count-1 downto 0 do Begin
      DXThreadRecord:=PDXThreadRecord(fDXThreadArray[Loop]);
      If DXThreadRecord^.ThreadID=FindHCONN then Begin
         Result:=DXThreadRecord;
         Break; // o.z
      End;
   End;
End;

Function FindThreadToFilter(var pfc: THTTP_FILTER_CONTEXT):PDXThreadRecord;
var
   conn:Pointer;
Begin
   GetMem(conn, sizeof(Longword));
   THTTP_FILTER_CONTEXT(pfc).ServerSupportFunction(pfc, SF_REQ_GET_CONNID, conn, 0, 0);
   Result:=FindThread(HCONN(conn^));
   FreeMem(conn, sizeof(Longword));
End;

///////////////////////////////////////////////////////////////////////////////
// Filter function implementation
///////////////////////////////////////////////////////////////////////////////

Function FilterAddResponseHeaders(var pfc{: THTTP_FILTER_CONTEXT};
    lpszHeaders: PChar; dwReserved: Longword):BOOLEAN; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
Begin
   DXThreadRecord:=FindThreadToFilter(THTTP_FILTER_CONTEXT(pfc));
   DXThreadRecord.fDopHeaders:=DXThreadRecord.fDopHeaders+lpszHeaders;
   Result:=True;
End;

Function FilterWriteClient(var pfc{: THTTP_FILTER_CONTEXT};
    Buffer: Pointer; var Bytes: Longword; dwReserved: Longword): BOOLEAN; stdcall;
var   DXThreadRecord:PDXThreadRecord;
      conn:Pointer;
Begin
   GetMem(conn, sizeof(Longword));
   THTTP_FILTER_CONTEXT(pfc).ServerSupportFunction(pfc, SF_REQ_GET_CONNID, conn, 0, 0);
   DXThreadRecord:=FindThread(HCONN(conn^));
   FreeMem(conn, sizeof(Longword));
{$IFDEF VER100} // D3
   DXThreadRecord^.Thread.Socket.BlockWrite(Buffer,Bytes);
{$ELSE}
   DXThreadRecord^.Thread.Socket.Write(Buffer,Bytes);
{$ENDIF}
   Result:=True;
End;

Function FilterAllocMem(var pfc{: THTTP_FILTER_CONTEXT}; cbSize: Longword;
    dwReserved: Longword):Pointer;  stdcall;
var p:Pointer;
   MemoryBlock:TDXMemoryBlock;
   DXThreadRecord:PDXThreadRecord;
Begin
   p:=Nil;
Try
   GetMem(p, cbSize);
Except
   FreeMem(p);
   Result:=Nil;
   Exit;
End;
   MemoryBlock.fPointer:=p;
   MemoryBlock.fSize:=cbSize;
   DXThreadRecord:=FindThreadToFilter(THTTP_FILTER_CONTEXT(pfc));
   DXThreadRecord.fAllocMem.Add(@MemoryBlock);
   Result:=p;
End;

Function FilterServerSupportFunction(var pfc{: THTTP_FILTER_CONTEXT};
    sfReq: Longword; pData: Pointer; ul1, ul2: Longword):BOOLEAN; stdcall;
var
   Results, hex:String;
   normal:PString;
   size:Longword;
   DXThreadRecord:PDXThreadRecord;
   i, dec, code : Integer;
Begin
   Result:=True;

   case sfReq of
      SF_REQ_SEND_RESPONSE_HEADER:Begin
         If pData = Nil then Results:='HTTP/1.1 200 OK'+#13#10
         Else Results:=String(pData^);
         Results:=Results+String(Pointer(ul1)^);
         size:=Length(Results)+1;
         THTTP_FILTER_CONTEXT(pfc).WriteClient(pfc, PChar(Results), size, 0);
      End; //SF_REQ_SEND_RESPONSE_HEADER

      SF_REQ_ADD_HEADERS_ON_DENIAL:Begin
         DXThreadRecord := FindThreadToFilter(THTTP_FILTER_CONTEXT(pfc));
         DXThreadRecord.fDopDeniedHeaders :=
            DXThreadRecord.fDopDeniedHeaders+ String(pData);
      End;//SF_REQ_ADD_HEADERS_ON_DENIAL

      SF_REQ_SET_NEXT_READ_SIZE:Begin
         fActiveFilter.fNextReadSize := ul1;
      End; //SF_REQ_SET_NEXT_READ_SIZE

      SF_REQ_SET_PROXY_INFO:Begin
         //You can use this support function to indicate whether
         //the current request is an HTTP proxy request.
         if ul1 = 1 then Begin
           //request is a proxy request.
         End
         Else
         if ul1 = 0 then Begin
           //request is not a proxy request
         End;
      End; //SF_REQ_SET_PROXY_INFO

      SF_REQ_GET_CONNID:Begin
         HCONN(pData^):=GetCurrentThreadId;
      End;

      SF_REQ_DISABLE_NOTIFICATIONS:Begin
         fActiveFilter.fDisableNotification:=ul1;
      End;

      SF_REQ_GET_PROPERTY:Begin
         //You can use this support function to retrieve special IIS
         //properties defined in SF_PROPERTY_IIS.
         case ul1 of
            SF_PROPERTY_INSTANCE_NUM_ID:Begin
               pData := Nil;
            End;
         End; //case
      End;

      SF_REQ_NORMALIZE_URL:Begin
         normal := PString(pData);
         i:=1;
         while i<> Length(normal^) do
         Begin
            if normal^[i] = '%' then Begin
               hex := Copy(normal^, i, 3);
               hex[1] := '$';
               Val(hex, dec, Code);
               if code <> 0 then Delete(normal^, i, 3)
               Else Begin
                  Delete(normal^, i, 2);
                  normal^[i]:=Char(dec);
               End;
            End;
            i := i + 1;
         End;//while
      End; //SF_REQ_NORMALIZE_URL
   else
      Result:=False;
   End;
End;

///////////////////////////////////////////////////////////////////////////////
// MSDN:
// The AddHeader callback function adds an HTTP header to the incoming request
// or outgoing response. This callback function is available only during
// notifications for the SF_NOTIFY_PREPROC_HEADERS and
// SF_NOTIFY_SEND_RESPONSE events.
///////////////////////////////////////////////////////////////////////////////
Function FilterAddHeader(var pfc: THTTP_FILTER_CONTEXT; lpszName,
          lpszValue: PChar): BOOLEAN; stdcall;
var DXThreadRecord:PDXThreadRecord;
    HeaderTools:TDXHTTPHeaderTools;
Begin
   DXThreadRecord:=FindThreadToFilter(pfc);
   HeaderTools := TDXHTTPHeaderTools.Create(Nil);
   if Not Assigned(DXThreadRecord) then HeaderTools.Init(fPreprocHeader)
   Else HeaderTools.Init(DXThreadRecord.fHeaderInfo);
   Result := HeaderTools.AddHeader(String(lpszName), String(lpszValue));
   HeaderTools.Free;
End;

Function FilterGetHeader(var pfc: THTTP_FILTER_CONTEXT; lpszName: PChar;
    var lpvBuffer; var lpdwSize: Longword):BOOLEAN; stdcall;
var DXThreadRecord:PDXThreadRecord;
    HeaderTools:TDXHTTPHeaderTools;
    res:String;
    resLength:Longword;
Begin
   Result := False;
   DXThreadRecord:=FindThreadToFilter(pfc);
   HeaderTools := TDXHTTPHeaderTools.Create(Nil);
   if Not Assigned(DXThreadRecord) then HeaderTools.Init(fPreprocHeader)
   Else HeaderTools.Init(DXThreadRecord.fHeaderInfo);
   res := HeaderTools.HeaderGetString(String(lpszName));
   if res <> '' then Begin
      resLength:=Length(res);
      Result := True;
      PChar(lpvBuffer) := PChar(res);
      lpdwSize := resLength;
   End;
   HeaderTools.Free;
End;

Function FilterSetHeader(var pfc: THTTP_FILTER_CONTEXT; lpszName,
    lpszValue: PChar):BOOLEAN; stdcall;
var DXThreadRecord:PDXThreadRecord;
    HeaderTools:TDXHTTPHeaderTools;
Begin
   DXThreadRecord:=FindThreadToFilter(pfc);
   HeaderTools := TDXHTTPHeaderTools.Create(Nil);
   if Not Assigned(DXThreadRecord) then HeaderTools.Init(fPreprocHeader)
   Else HeaderTools.Init(DXThreadRecord.fHeaderInfo);
   Result := HeaderTools.SetHeader(String(lpszName), String(lpszValue));
   HeaderTools.Free;
End;

Procedure NotificationFilters(Notify:Longword; Buf1:Pointer; sizeBuf1:Longword;
                                                     Buf2:Pointer; sizeBuf2:Longword);
var p:Pointer;
    filterContext:THTTP_FILTER_CONTEXT;
    i:Integer;
    filterRecord:PDXFilterRecord;
    DLLHandle:THandle;
    filterProc:THttpFilterProc;
    outRes:PChar;
    str:String;
    outResSize:Longword;
    DXThreadRecord:PDXThreadRecord;

Begin
   If fFilter.Count<1 then Exit;
   with filterContext do Begin
      cbSize:=sizeof(THTTP_FILTER_CONTEXT);
      Revision:=4;
      ServerContext:=Nil;
      ulReserved:=0;
      fIsSecurePort:=False;
      pFilterContext:=Nil;
      GetServerVariable:=fGetServerVariableFilterProc;
      AddResponseHeaders:=FilterAddresponseHeaders;
      WriteClient:=FilterWriteClient;
      AllocMem:=FilterAllocMem;
      ServerSupportFunction:=FilterServerSupportFunction;
   End;
   DXThreadRecord:=FindThreadToFilter(filterContext);

   Case Notify of
      //Port Security Settings
      SF_NOTIFY_SECURE_PORT : Begin
         filterContext.fIsSecurePort:=True;
         p:=Nil;
      End;

      SF_NOTIFY_NONSECURE_PORT : Begin
         filterContext.fIsSecurePort:=False;
         p:=Nil;
      End;

////////////////////////////////////////////////////////////////////////////////
// This event was used when the ISAPI sends data to the client.
// Or when the server is reading data from the client.
////////////////////////////////////////////////////////////////////////////////
      SF_NOTIFY_SEND_RAW_DATA: Begin
         //this probably needs to be redesigned
         GetMem(p, sizeof(THTTP_FILTER_RAW_DATA));
         with PHTTP_FILTER_RAW_DATA(p)^ do Begin
            pvInData:=Buf1;
            cbInData:=sizeBuf1;
            cbInBuffer:=sizeof(Buf1);
            dwReserved:=0;
         End;
      End;
      SF_NOTIFY_READ_RAW_DATA:Begin
         //HTTP_FILTER_RAW_DATA
         GetMem(p, sizeof(THTTP_FILTER_RAW_DATA));
         with PHTTP_FILTER_RAW_DATA(p)^ do Begin
            pvInData:=Buf1;
            cbInData:=sizeBuf1;
            cbInBuffer:=sizeof(Buf1);
            dwReserved:=0;
         End;
      End;

////////////////////////////////////////////////////////////////////////////////
// Create hooks for Get/Set/Add Header Routines
////////////////////////////////////////////////////////////////////////////////
      SF_NOTIFY_PREPROC_HEADERS:Begin
         //HTTP_FILTER_PREPROC_HEADERS
         GetMem(p, sizeof(THTTP_FILTER_PREPROC_HEADERS));
         with PHTTP_FILTER_PREPROC_HEADERS(p)^ do Begin
            GetHeader:=FilterGetHeader;
            SetHeader:=FilterSetHeader;
            AddHeader:=FilterAddHeader;
            HttpStatus:=200; //HTTP_STATUS_CONTINUE
            dwReserved:=0;
         End;
         fPreprocHeader := PHeaderInfo(Buf1);
      End;

      SF_NOTIFY_AUTHENTICATION:Begin
         //HTTP_FILTER_AUTHENT
         GetMem(p, sizeof(THTTP_FILTER_AUTHENT));
         with PHTTP_FILTER_AUTHENT(p)^ do Begin
            pszUser:=PChar(Buf1);
            cbUserBuff:=sizeBuf1;
            pszPassword:=PChar(Buf2);
            cbPasswordBuff:=sizeBuf2;
         End;
      End;
///////////////////////////////////////////////////////////////////////////////
// Notification is given to the filters after you have looked up the physical
// path. If the filters do not like it, then they can only close the connection
// and use SF_NOTIFY_PREPROC_HEADERS to write a 302 url.
///////////////////////////////////////////////////////////////////////////////
      SF_NOTIFY_URL_MAP:Begin
         // HTTP_FILTER_URL_MAP
         GetMem(p, sizeof(THTTP_FILTER_URL_MAP));
         with PHTTP_FILTER_URL_MAP(p)^ do Begin
            pszURL:=PChar(Buf1);
            pszPhysicalPath:=PChar(Buf2);
            cbPathBuff:=sizeBuf2;
         End;
      End;

      SF_NOTIFY_ACCESS_DENIED:Begin
         // HTTP_FILTER_ACCESS_DENIED
         GetMem(p, sizeof(THTTP_FILTER_ACCESS_DENIED));
         with PHTTP_FILTER_ACCESS_DENIED(p)^ do Begin
            pszURL:=PChar(Buf1);            // Requesting URL
            dwReason:=sizeBuf1;          // Bitfield of SF_DENIED flags
            pszPhysicalPath:=PChar(Buf2);   // Physical path of resource
         End;
      End;

      SF_NOTIFY_LOG:Begin
         //HTTP_FILTER_LOG
         GetMem(p, sizeof(THTTP_FILTER_LOG));
         with PHTTP_FILTER_LOG(p)^ do Begin
            pszClientHostName:='not support';
            pszClientUserName:='not support';
            pszServerName:='not support';
            pszOperation:='not support';
            pszTarget:='not support';
            pszParameters:='not support';
            dwHttpStatus:=200;
            dwWin32Status:=0;
            dwBytesSent:=0;
            dwBytesRecvd:=0;
            msTimeForProcessing:=0;
         End;
      End;

      SF_NOTIFY_END_OF_REQUEST:Begin
         p:=Nil;
      End;

      SF_NOTIFY_END_OF_NET_SESSION:Begin
         p:=Nil;
      End;

////////////////////////////////////////////////////////////////////////////////
// Create hooks for Get/Set/Add Header Routines - guess means to actually
// transmit the header to the client.
////////////////////////////////////////////////////////////////////////////////
      SF_NOTIFY_SEND_RESPONSE:Begin
         //HTTP_FILTER_SEND_RESPONSE
         GetMem(p, sizeof(THTTP_FILTER_SEND_RESPONSE));
         with PHTTP_FILTER_SEND_RESPONSE(p)^ do Begin
            GetHeader:=FilterGetHeader;
            SetHeader:=FilterSetHeader;
            AddHeader:=FilterAddHeader;
            HttpStatus:=200;
            dwReserved:=0;
         End;
//         DXThreadRecord.fHeaderInfo := PHeaderInfo(Buf1);
      End;
      Else
       Exit;
   End;

   for i:=fFilter.Count-1 downto 0 do Begin
      filterRecord:=fFilter.Items[i];
      if Not filterRecord.fNotification then continue;
      if (filterRecord.fVersion.dwFlags AND
         (Not filterRecord.fDisableNotification) AND Notify)=0 then continue;
      fActiveFilter:=filterRecord;
{$IFDEF USE_DLL_MGR}
      DLLHandle:=fDLLManager.GetDLLHandle(filterRecord.fName);
{$ELSE}
{$IFDEF LINUX}
      DLLHandle:=LoadLibrary(PChar(filterRecord.fName));
{$ELSE}
      DLLHandle:=LoadLibraryEx(PChar(filterRecord.fName),0,0);
{$ENDIF}
{$ENDIF}
      If DLLHandle=0 then continue;
      @filterProc:=GetProcAddress(DLLHandle,PChar('HttpFilterProc'));
      If Not Assigned(filterProc) then continue;
      case filterProc(filterContext, Notify, p) of
          SF_STATUS_REQ_FINISHED_KEEP_CONN,
          SF_STATUS_REQ_FINISHED:Begin
            if Assigned(DXThreadRecord) then
               DXThreadRecord.Thread.Socket.CloseNow;
          End;
          SF_STATUS_REQ_NEXT_NOTIFICATION:Begin
             continue; // pass to next filter
          End;
          SF_STATUS_REQ_HANDLED_NOTIFICATION:Begin
             Break; // handled and done
          End;
          SF_STATUS_REQ_ERROR:Begin
            NotificationFilters(SF_NOTIFY_ACCESS_DENIED, PChar(filterRecord.fName),
                                SF_DENIED_FILTER, PChar(''), 0);

{$IFNDEF LINUX}
            FormatMessage(
                FORMAT_MESSAGE_ALLOCATE_BUFFER OR
                FORMAT_MESSAGE_FROM_SYSTEM OR
                FORMAT_MESSAGE_IGNORE_INSERTS,
                Nil,
                GetLastError(),
                0, // Default language
                @outRes,
                0,
                Nil
             );
{$ENDIF}
            if outRes <> '' then Begin
               str := 'HTTP/1.1 500 '+#13#10;
               str := str + DXThreadRecord.fDopDeniedHeaders;
               str := str + String(outRes) +#13#10;
               outResSize:=Length(str);
               filterContext.WriteClient(filterContext, PChar(str), outResSize, 0);
            End
            Else Begin
               str:='HTTP/1.1 500'+#13#10 + DXThreadRecord.fDopDeniedHeaders;
               outResSize:=Length(str);
               filterContext.WriteClient(filterContext, PChar(str), outResSize, 0);
            End;
            if Assigned(DXThreadRecord) then
               DXThreadRecord.Thread.Socket.CloseNow;
          End; //SF_STATUS_REQ_ERROR

// to-do: this has to be able to let the web server object do the read,
// that way no data disappear from the stream. Normally this status type
// is only returned when the filter is opaque (like SSL).
          SF_STATUS_REQ_READ_NEXT:Begin
            DXThreadRecord.ECB.ReadClient(DXThreadRecord.ThreadID, Pointer(outRes), filterRecord.fNextReadSize);
          End; //SF_STATUS_REQ_READ_NEXT
      End; //case

   End; //for
// once done - we have to return the changes to the P content!
   if Assigned(p) then FreeMem(p);
   fActiveFilter:=Nil;
End;

///////////////////////////////////////////////////////////////////////////////
// LOG INFORMATION
// Use to write log information of transaction request and response.
///////////////////////////////////////////////////////////////////////////////
function WriteTransactionToLog(LogStr:String; Size:Longword):BOOLEAN;
Begin
  NotificationFilters(SF_NOTIFY_LOG, PChar(LogStr), Size, Nil, 0);
  Result:=True;
End;

function SortFilterCompare(Item1, Item2: Pointer): Integer;
var fv1, fv2 : PHTTP_FILTER_VERSION;
Begin
   fv1:=PDXFilterRecord(Item1).fVersion;
   fv2:=PDXFilterRecord(Item2).fVersion;
   Result:= (fv1.dwFlags AND SF_NOTIFY_ORDER_MASK) -
            (fv2.dwFlags AND SF_NOTIFY_ORDER_MASK);
End;

Procedure TDXISAPI.RegisterFilters(var ResultLog:String);
var DXFilterRecord:PDXFilterRecord;
    filterVersion:THTTP_FILTER_VERSION;
    i:Integer;
    DLLHandle:THandle;
    ISAPI:String;
    Version:TGetFilterVersion;
Begin
   for i:=0 to fFilter.Count-1 do Begin
      DXFilterRecord:=fFilter.Items[i];
      ISAPI:=DXFilterRecord.fName;
      RegisterDLL(ISAPI);
{$IFDEF USE_DLL_MGR}
      DLLHandle:=fDLLManager.GetDLLHandle(ISAPI);
{$ELSE}
{$IFDEF LINUX}
      DLLHandle:=LoadLibrary(PChar(ISAPI));
{$ELSE}
      DLLHandle:=LoadLibraryEx(PChar(ISAPI),0,0);
{$ENDIF}
{$ENDIF}
      If DLLHandle=0 then Begin
         ResultLog:=ISAPI+' was not registered!';
         Continue;
      End;

      @Version:=GetProcAddress(DLLHandle,PChar('GetFilterVersion'));
      If Not Assigned(Version) then Begin
         ResultLog:=ISAPI+' does not export "GetFilterVersion"';
         RemoveFilter(ISAPI);
         Exit;
      End;

      DXFilterRecord.fNotification:=Version(filterVersion);
      DXFilterRecord.fVersion^:=filterVersion;
   End;

   fFilter.Sort(SortFilterCompare);
End;

Procedure TDXISAPI.UnRegisterFilters(var ResultLog:String);
var DXFilterRecord:PDXFilterRecord;
    i:Integer;
    DLLHandle:THandle;
    Term:TTerminateFilter;
Begin
   for i:=0 to fFilter.Count-1 do Begin
      DXFilterRecord:=fFilter.Items[i];
{$IFDEF USE_DLL_MGR}
      DLLHandle:=fDLLManager.GetDLLHandle(DXFilterRecord.fName);
{$ELSE}
{$IFDEF LINUX}
      DLLHandle:=LoadLibrary(PChar(DXFilterRecord.fName));
{$ELSE}
      DLLHandle:=LoadLibraryEx(PChar(DXFilterRecord.fName),0,0);
{$ENDIF}
{$ENDIF}
      If DLLHandle=0 then Begin
         ResultLog:=DXFilterRecord.fName+' was not registered!';
         Continue;
      End;

      @Term:=GetProcAddress(DLLHandle,PChar('TerminateFilter'));
      If Not Assigned(Term) then
         Continue;

      Term(0);
{$IFDEF USE_DLL_MGR}
      fDLLManager.UnLoadThisDLL(DXFilterRecord.fName);
{$ELSE}
      FreeLibrary(DLLHandle);
{$ENDIF}
   End;
End;

Function TDXISAPI.AddFilter(ISAPI:String):Boolean;
var DXFilterRecord:PDXFilterRecord;
    filterVersion:PHTTP_FILTER_VERSION;
Begin
   Result:=False;
   New(DXFilterRecord);
   New(filterVersion);
   DXFilterRecord.fName:=ISAPI;
   with filterVersion^ do
   Begin
      dwServerFilterVersion:=HTTP_FILTER_REVISION;
      dwFilterVersion:=0;
      lpszFilterDesc:='';
      dwFlags:=0;
   End;
   DXFilterRecord.fVersion := filterVersion;
   DXFilterRecord.fNotification := True;
   DXFilterRecord.fDisableNotification := 0;
   DXFilterRecord.fNextReadSize := 16*1024; //16Kb
   if (fFilter.Add(DXFilterRecord) >= 0) then Result:=True
   else Begin
      Dispose(filterVersion);
      Dispose(DXFilterRecord);
   End;
End;

Function TDXISAPI.RemoveFilter(ISAPI:String):Boolean;
var DXFilterRecord:PDXFilterRecord;
    HTTP_FILTER_VERSION:PHTTP_FILTER_VERSION;
    i : Integer;
Begin
   Result:=False;
   MyCriticalSection.StartingWrite;
   for i:=0 to fFilter.Count-1 do
   Begin
      DXFilterRecord:=fFilter.Items[i];
      if DXFilterRecord.fName = ISAPI then
      Begin
       HTTP_FILTER_VERSION:=DXFilterRecord.fVersion;
       Dispose(HTTP_FILTER_VERSION);
       Dispose(DXFilterRecord);
       fFilter.Delete(i);
       Result:=True;
       MyCriticalSection.FinishedWrite;
       Exit;
      End;
   End;
   MyCriticalSection.FinishedRead;
End;

Procedure TDXISAPI.ServerRawRead(ReadString:String;Len:Longword);
Begin
   NotificationFilters(SF_NOTIFY_READ_RAW_DATA, @ReadString, Len, Nil, 0);
End;

Procedure TDXISAPI.ServerPreprocHeaderEvent(DXHeaderInfo:PHeaderInfo);
Begin
   NotificationFilters(SF_NOTIFY_PREPROC_HEADERS, DXHeaderInfo, 0, Nil, 0);
End;

Procedure TDXISAPI.ConvertedURL2Physical(URL:String;Physical:String);
Begin
   NotificationFilters(SF_NOTIFY_URL_MAP,@URL,Length(URL),@Physical,Length(Physical));
End;

Procedure TDXISAPI.ServerEndOfRequest;
Begin
   NotificationFilters(SF_NOTIFY_END_OF_REQUEST, Nil, 0, Nil, 0);
End;

Procedure TDXISAPI.ServerEndSession;
Begin
   NotificationFilters(SF_NOTIFY_END_OF_NET_SESSION, Nil, 0, Nil, 0);
End;

Procedure TDXISAPI.SetServerVariableProc(value:TFilterGetServerVariableProc);
Begin
   fGetServerVariableFilterProc:=value;
End;

Function TDXISAPI.GetServerVariableProc:TFilterGetServerVariableProc;
Begin
   Result := fGetServerVariableFilterProc;
End;

Function TDXISAPI.FilterCount:Integer;
Begin
try
   Result:=fFilter.Count;
except
   Result:=0;
End;
End;

///////////////////////////////////////////////////////////////////////////////
// Extension ISAPI implementation
// ==============================
// ISAPI CALLBACK FUNCTIONS
///////////////////////////////////////////////////////////////////////////////

function ServerVariable(ThreadID:HCONN;Variable:PChar;Buffer:Pointer;Size:LPDWord):BOOLEAN; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
   Results:String;
   Str:String;

begin
   Result:=False;
   LongWord(Size^):=0;
   DXThreadRecord:=FindThread(ThreadID);
   If Not Assigned(DXThreadRecord) then Exit;
   Results:='';
   str := Variable;
   If Str='INSTANCE_ID' then Results:=IntToStr(ThreadID)
   Else if Assigned(DXThreadRecord^.fGetServerVariable) then
      DXThreadRecord^.fGetServerVariable(DXThreadRecord^.Thread,Str,Results);
   StrPCopy(PChar(Buffer),Results);
   If Results<>'' then LongWord(Size^):=Length(Results)+1;
   Result:=True;
end;

Function ExtensionWriteClient(ThreadID:HCONN;Buf:Pointer;Var Bytes:Longword;dwReserved:Longword):BOOLEAN; stdcall;
var
   DXThreadRecord:PDXThreadRecord;

begin
   If Bytes=0 then Begin // PHP occasionally calls this with zero bytes?!
      Result:=True;
      Exit;
   End;
   Result:=False;
   DXThreadRecord:=FindThread(ThreadID);
   If Not Assigned(DXThreadRecord) then Exit;
   NotificationFilters(SF_NOTIFY_SEND_RAW_DATA, Buf, Bytes, Nil, 0);
{$IFDEF VER100} // D3
   DXThreadRecord^.Thread.Socket.BlockWrite(Buf,Bytes);
{$ELSE}
   DXThreadRecord^.Thread.Socket.Write(Buf,Bytes);
{$ENDIF}
   Result:=DXThreadRecord^.Thread.Socket.LastCommandStatus=0;
end;

function ExtensionReadClient(ThreadID:HCONN;Buf:Pointer;var Size:Longword):BOOLEAN; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
begin
   Result:=False;
   DXThreadRecord:=FindThread(ThreadID);
   If Not Assigned(DXThreadRecord) then Exit;
   NotificationFilters(SF_NOTIFY_READ_RAW_DATA, Buf, Size, Nil, 0);
{$IFDEF VER100}
   Size:=DXThreadRecord^.Thread.Socket.BlockRead(Buf,Size);
{$ELSE}
   Size:=DXThreadRecord^.Thread.Socket.Read(Buf,Size);
{$ENDIF}
   Result:=True;
end;

{$HINTS OFF}
function ServerSupportFunctionS(ThreadID:HCONN;
                                HSE_REQ:Longword;
                                Buf:Pointer;
                                Size:Pointer{PDWORD};
                                DataType:Pointer{PDWORD}):BOOLEAN; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
   Results:String;

begin // HTTPODBC.DLL calls with HSE_REQ=61442 (wtf?)
   Result:=False;
   Results:='';
   DXThreadRecord:=FindThread(ThreadID);
   If Not Assigned(DXThreadRecord) then Exit;
   Case HSE_REQ of
      HSE_APPEND_LOG_PARAMETER:Begin
         Move(PChar(Buf),DXThreadRecord^.ECB.lpszLogData,StrLen(PChar(Buf)));
         Result:=False;
      End;
      HSE_REQ_ABORTIVE_CLOSE,
      HSE_REQ_CLOSE_CONNECTION:Begin
         DXThreadRecord^.Thread.Socket.CloseNow;
         Result:=True;
      End;
      HSE_REQ_IS_KEEP_CONN:Begin
         Boolean(Buf^):=False; // when I get keep alive for ISAPI working set this to true
         Result:=True;
      End;
      HSE_REQ_MAP_URL_TO_PATH:
      Begin
         Results := DXThreadRecord^.fRealPath+StringReplace(String(PChar(Buf)), '/', '\', [rfReplaceAll]);
         Results := StringReplace(Results,'\\','\',[rfReplaceAll]);
         LongWord(Size^):=Length(Results); // 9/11/2002
         Buf:=PChar(Results);
         with DXThreadRecord^.mapex do Begin
            StrPCopy(lpszPath, Results);
            dwFlags:=1+2+4; //READ+WRITE+EXECUTE
            cchMatchingPath:=StrLen(lpszPath);
            cchMatchingURL:=StrLen(lpszPath);
            dwReserved1:=0;
            dwReserved2:=0;
         End;
         Move(DXThreadRecord^.Mapex,DataType^,Sizeof(DXThreadRecord^.Mapex));
         NotificationFilters(SF_NOTIFY_URL_MAP, Buf, Longword(Size), @Results, Length(Results));
         Result:=True;
      End;
      HSE_REQ_MAP_URL_TO_PATH_EX:
      Begin
         Results := DXThreadRecord^.fRealPath+StringReplace(String(PChar(Buf)), '/', '\', [rfReplaceAll]);
         Results := StringReplace(Results,'\\','\',[rfReplaceAll]);
         with DXThreadRecord^.mapex do Begin
            StrPCopy(lpszPath,Results);
            dwFlags:=1+2+4; //READ+WRITE+EXECUTE
            cchMatchingPath:=StrLen(lpszPath);
            cchMatchingURL:=StrLen(lpszPath);
            dwReserved1:=0;
            dwReserved2:=0;
         End;
         Move(DXThreadRecord^.Mapex,DataType^,Sizeof(DXThreadRecord^.Mapex));
         NotificationFilters(SF_NOTIFY_URL_MAP, Buf, Longword(Size), @Results, Length(Results));
         Result:=True;
      End;
      HSE_REQ_SEND_URL_REDIRECT_RESP,
      HSE_REQ_SEND_URL:Begin
         If Assigned(DXThreadRecord^.fRedirectHeader) then
            DXThreadRecord^.fRedirectHeader(
            DXThreadRecord^.Thread,
            StrPas(PChar(Buf)),Results)
         Else Results:='HTTP/1.1 302 Moved Temporarily'+#13#10+'Location: '+StrPas(PChar(Buf))+#13#10;
         DXThreadRecord^.Thread.Socket.Writeln(Results);
         Result:=True;
      End;
      HSE_REQ_SEND_RESPONSE_HEADER:Begin
         Result := True;
         Results:='';
         If Assigned(Buf) then begin
            If PChar(Buf)<>'' then Begin
               Results:='HTTP/1.1 '+PChar(Buf)+#13#10;
            End;
         End;
         If Results='' then Begin
            If Assigned(DXThreadRecord^.fBuildHeader) then
               DXThreadRecord^.fBuildHeader(DXThreadRecord^.ECB.dwHttpStatusCode,Results)
            Else Begin
               Results:='HTTP/1.1 200 OK'+#13#10;
            End;
         End;
         If Assigned(DataType) then begin
            Results:=Results+PChar(DataType);
         End;
         If Copy(Results,Length(Results)-1,2)<>#13#10 then
            Results:=Results+#13#10;
// 5-11-2002
         If (Copy(Results,Length(Results)-3,4)<>#13#10#13#10) and
            (Copy(Results,Length(Results)-1,2)<>#10#10) and //10-3-2002
            (Copy(Results,Length(Results)-1,2)<>#13#13) then Begin //10-3-2002
            // 10-3-2002
            If PChar(Buf)<>'' then DXThreadRecord^.Thread.Socket.Writeln(Results)
            Else DXThreadRecord^.Thread.Socket.Write(Results);
         End
         Else
            DXThreadRecord^.Thread.Socket.Write(Results)
      End;
      HSE_REQ_SEND_RESPONSE_HEADER_EX:Begin
         Results:='';
         If Assigned(DXThreadRecord^.fBuildHeader) then
            DXThreadRecord^.fBuildHeader(DXThreadRecord^.ECB.dwHttpStatusCode,Results)
         Else Begin
            If PHSE_SEND_HEADER_EX_INFO(Buf)^.cchStatus>0 then
               Results:='HTTP/1.1 '+PChar(PHSE_SEND_HEADER_EX_INFO(Buf)^.pszStatus)+#13#10
            Else
               Results:='HTTP/1.1 200 OK'+#13#10;
         End;
         If PHSE_SEND_HEADER_EX_INFO(Buf)^.cchHeader=0 then Results:=Results+#13#10
         Else Results:=Results+PHSE_SEND_HEADER_EX_INFO(Buf)^.pszHeader;
         // redesign
         NotificationFilters(SF_NOTIFY_SEND_RESPONSE, @Results, 0, Nil, 0);
         DXThreadRecord^.Thread.Socket.Write(Results);
         Result:=True;
      End;
      HSE_REQ_TRANSMIT_FILE:Begin
         Result:=False;
      // ?
      //if access denied to call
      //NotificationFilters(SF_NOTIFY_ACCESS_DENIED, Pointer1, SF_DENIED_RESOURCE, Pointer2, 0);
      //This: Pointer1 - Requesting URL,
      //      Pointer2 - Physical path of resource
      End;
      HSE_REQ_DONE_WITH_SESSION:Begin
         DXThreadRecord^.fISAPIThreadPoolDone:=True;
         Result:=True;
      End;
      HSE_REQ_GET_SSPI_INFO:Begin
         Result:=False;
      End;
      HSE_REQ_IO_COMPLETION:Begin
         Result:=False;
      End;
      HSE_REQ_REFRESH_ISAPI_ACL:Begin
         Result:=False;
      End;
      HSE_REQ_ASYNC_READ_CLIENT:Begin
         Result:=True;
      End;
{ do not know there values!
      HSE_REQ_EXECUTE_CHILD:Begin // ISAPI 5.1
         Result:=False;
      End;
      HSE_REQ_GET_EXECUTE_FLAGS:Begin // ISAPI 5.1
         Reslut:=True;
         Move(HSE_EXEC_CUSTOM_ERROR,DataType,Sizeof(HSE_EXEC_CUSTOM_ERROR));
      End;
}
      HSE_REQ_GET_IMPERSONATION_TOKEN:Begin
        Result:=False;
{$IFNDEF LINUX}
        If Assigned(Buf) then Begin
          Result:=OpenThreadToken(HCONN(DXThreadRecord^.Thread), TOKEN_ALL_ACCESS, False, THandle(DXThreadRecord^.fToken));
          if Result then Buf:=@DXThreadRecord^.fToken;
        End;
{$ENDIF}
      End;
      HSE_REQ_GET_CERT_INFO_EX:Begin
         Result:=False;
//         fCertificate:=TCERT_CONTEXT_EX(Buf);
      End;

   End;
end;
{$HINTS ON}

///////////////////////////////////////////////////////////////////////////////
// Find and Load DLL instance
///////////////////////////////////////////////////////////////////////////////
Function TDXISAPI.RegisterDLL(ISAPI:String):Boolean;
Begin
{$IFDEF USE_DLL_MGR}
   Result:=fDLLManager.LoadThisDLL(ISAPI);
{$ELSE}
   Result:=True;
{$ENDIF}
End;

///////////////////////////////////////////////////////////////////////////////
// Find and Un-Load DLL instance
///////////////////////////////////////////////////////////////////////////////
Function TDXISAPI.UnRegisterDLL(ISAPI:String):Boolean;
Begin
{$IFDEF USE_DLL_MGR}
   If fDLLManager.GetDLLNameInstances(ISAPI)>1 then // 10-24-2002
      Result:=fDLLManager.UnLoadThisDLL(ISAPI);
{$ELSE}
   Result:=True;
{$ENDIF}
End;

///////////////////////////////////////////////////////////////////////////////
// LAUNCH THE DLL!
///////////////////////////////////////////////////////////////////////////////

Function TDXISAPI.Execute(Session:TDXClientThread;
                          ISAPI,
                          Method,
                          QueryString,
                          PathInfo,
                          PathTranslated,
                          POSTContent_Type,
                          POSTData:String;
                          POSTDataSize:Integer;
                          Var ResultLog:String):Boolean;
Var
   DXThreadRecord:PDXThreadRecord;
   Version:TGetExtensionVersion;
   Proc:THttpExtensionProc;
   AllDone:TTerminateExtension;
   Loop:Integer;
   DLLHandle:THandle;
   ProcResults:Longword;
   Err:PChar;
   EVI:THSE_VERSION_INFO;
   StartTime:Comp;

Begin
   Result:=False;
{$IFDEF USE_DLL_MGR}
   DLLHandle:=fDLLManager.GetDLLHandle(ISAPI);
{$ELSE}
{$IFDEF LINUX}
   DLLHandle:=LoadLibrary(PChar(ISAPI));
{$ELSE}
   DLLHandle:=LoadLibraryEx(PChar(ISAPI),0,0);
{$ENDIF}
{$ENDIF}
   If DLLHandle=0 then Begin
      GetMem(Err,256);
{$IFNDEF LINUX}
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,GetLastError,0,Err,256,Nil);
{$ENDIF}
      ResultLog:=ISAPI+' was not registered! ('+Err+')';
      FreeMem(Err,256);
{$IFNDEF USE_DLL_MGR}
      FreeLibrary(DLLHandle);
{$ENDIF}
      Exit;
   End;
   @Version:=GetProcAddress(DLLHandle,PChar('GetExtensionVersion'));
   If Not Assigned(Version) then Begin
      ResultLog:=ISAPI+' does not export "GetExtensionVersion"';
{$IFNDEF USE_DLL_MGR}
      FreeLibrary(DLLHandle);
{$ENDIF}
      Exit;
   End;
   @Proc:=GetProcAddress(DLLHandle,PChar('HttpExtensionProc'));
   If Not Assigned(Proc) then Begin
      ResultLog:=ISAPI+' does not export "HTTPExtensionProc"';
{$IFNDEF USE_DLL_MGR}
      FreeLibrary(DLLHandle);
{$ENDIF}
      Exit;
   End;

   New(DXThreadRecord);
   With DXThreadRecord^ do Begin
      ThreadID:=GetCurrentThreadID;
      Thread:=Session;
      fToken:=0;
      fDopHeaders:='';
      fDopDeniedHeaders:='';
      fGetServerVariable:=self.fGetServerVariable;
      fRedirectHeader:=self.fRedirectHeader;
      fBuildHeader:=self.fBuildHeader;
      fRealPath:=ExtractFilePath(PathTranslated);
      fiTimeout:=fTimeout;
      fISAPIThreadPoolDone:=False;
   End;
   If Assigned(Version) then Begin
      FillChar(EVI,Sizeof(EVI),#0);
      If Not Version(EVI) then Begin
         Dispose(DXThreadRecord);
         ResultLog:=ISAPI+' Version Hook Refused';
{$IFNDEF USE_DLL_MGR}
         FreeLibrary(DLLHandle);
{$ENDIF}
         Exit;
      End
      Else ResultLog:=EVI.lpszExtensionDesc;
   End;

   With DXThreadRecord^.ECB do Begin
      cbSize:=sizeof(TEXTENSION_CONTROL_BLOCK);
      dwVersion:=HSE_VERSION_MAJOR shl 16 + HSE_VERSION_MINOR;
      ConnID:=DXThreadRecord^.ThreadID;
      dwHttpStatusCode:=200;
      FillChar(lpszLogData,HSE_LOG_BUFFER_LEN,#0);
      lpszMethod:=PChar(Method);
      lpszQueryString:=PChar(QueryString);
      lpszPathInfo:=PChar(PathInfo);
      lpszPathTranslated:=PChar(PathTranslated);
      cbAvailable:=Length(PostData);
      lpbData:=PChar(PostData);
      cbTotalBytes:=POSTDataSize;
      lpszContentType:=PChar(PostContent_Type);
      GetServerVariable:=ServerVariable;
      ServerSupportFunction:=ServerSupportFunctionS;
      WriteClient:=ExtensionWriteClient;
      ReadClient:=ExtensionReadClient;
   End;

   MyCriticalSection.StartingWrite;
   fDXThreadArray.Add(DXThreadRecord);
   MyCriticalSection.FinishedWrite;
try
   ProcResults:=Proc(DXThreadRecord^.ECB);
   Result:=True;
   Case ProcResults of
      HSE_STATUS_SUCCESS,
      HSE_STATUS_SUCCESS_AND_KEEP_CONN:Begin
         ResultLog:=ResultLog+' success: '+StrPas(DXThreadRecord^.ECB.lpszLogData);
      End;
      HSE_STATUS_PENDING:Begin
         // we must loop here and wait for ISAPIThreadPool to finish.
         // 3.0.b
         StartTime:=TimeCounter; // 9/11/2002 moved down here.
         While (Not DXThreadRecord^.fISAPIThreadPoolDone) and (TimeCounter<StartTime+30000) do Begin
            DoSleepEx(1);
         End;
      End;
      HSE_STATUS_ERROR:Begin
         ResultLog:=ResultLog+' failed: '+StrPas(DXThreadRecord^.ECB.lpszLogData);
         Result:=False;
         NotificationFilters(SF_NOTIFY_ACCESS_DENIED, PChar(ISAPI), SF_DENIED_APPLICATION, PChar(PathTranslated), 0);
      End;
      else Begin
         ResultLog:=ResultLog+' returned unknown status '+
            StrPas(DXThreadRecord^.ECB.lpszLogData);
         Result:=False;
      End; //else
   End; //case
except
   on E: Exception do Begin
      ResultLog:=ISAPI+' failed: '+E.Message;
      Result:=False;
   End;
end;
   NotificationFilters(SF_NOTIFY_END_OF_REQUEST, Nil, 0, Nil, 0); //END Request
   @AllDone:=GetProcAddress(DLLHandle,PCHar('TerminateExtension'));
   If Assigned(AllDone) then AllDone(HSE_TERM_MUST_UNLOAD);
{$IFNDEF USE_DLL_MGR}
   FreeLibrary(DLLHandle);
{$ENDIF}
   WriteTransactionToLog('',0);
   Loop:=0;
   MyCriticalSection.StartingRead;
   While (Loop<fDXThreadArray.Count) do Begin
      DXThreadRecord:=fDXThreadArray[Loop];
      If DXThreadRecord^.ThreadID=Integer(getCurrentThreadID) then  Begin
         // closing token if he is exist
         MyCriticalSection.FinishedRead;
         MyCriticalSection.StartingWrite;
{$IFNDEF LINUX}
         If DXThreadRecord^.fToken<>0 then
            CloseHandle(DXThreadRecord^.fToken);
{$ENDIF}
         Dispose(DXThreadRecord);
         fDXThreadArray.Delete(Loop);
         MyCriticalSection.FinishedWrite;
         Exit;
      End
      Else Inc(Loop);
   End;
   MyCriticalSection.FinishedRead;
End;

Procedure TDXISAPI.ServerStartEvent;
var
   res:String;
Begin
   RegisterFilters(res);
End;

Procedure TDXISAPI.ServerStopEvent;
var
   res:String;
Begin
   self.UnRegisterFilters(res);
End;

{$IFDEF SUPPORT_COM}
initialization
  CoInitializeEx(nil,COINIT_MULTITHREADED);
{$ENDIF}
finalization
{$IFDEF SUPPORT_COM}
  CoUninitialize;
{$ENDIF}

end.

