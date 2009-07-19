unit DXWINCGI;

///////////////////////////////////////////////////////////////////////////////
// Component: TDXWINCGI
//    Author: G.E. Ozz Nixon Jr. (usasupport@brainpatchworkdx.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999 Brain Patchwork DX.
//   Version: 2.0 (2nd Generation Code)
// ==========================================================================
// Rough Draft!
///////////////////////////////////////////////////////////////////////////////

interface

uses
   {$IFNDEF OBJECTS_ONLY}
   Classes,
   {$ENDIF}
   Windows,
   DXDLLManager,
   DXServerCore;

{$I DXSOCK.DEF}

type
   TDX_GetServerVariable=procedure(ClientThread:TDXServerThread;
      Variable:string; var Results:string) of object;
   TDX_RedirectHeader=procedure(ClientThread:TDXServerThread; Location:string;
      var Header:string) of object;
   TDX_200_OKHeader=procedure(ClientThread:TDXServerThread;
      PossibleHeader:string; var Header:string) of object;

   HCONN=THandle; {ISAPI.PAS}

   PDXThreadRecord=^TDXThreadRecord;
   TDXThreadRecord=record
      ThreadID:HCONN;
      Thread:TDXServerThread;
      fGetServerVariable:TDX_GetServerVariable;
      fRedirectHeader:TDX_RedirectHeader;
      f200OKHeader:TDX_200_OKHeader;
      fTimeout:Integer;
   end;

   {$IFDEF OBJECTS_ONLY}
   TDXWINCGI=class
      {$ELSE}
   TDXWINCGI=class(TComponent)
      {$ENDIF}
   private
      fDLLManager:TDXDLLManager;
      fGetServerVariable:TDX_GetServerVariable;
      fRedirectHeader:TDX_RedirectHeader;
      f200OKHeader:TDX_200_OKHeader;
      fTimeout:Integer;
   protected
   public
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      function Execute(Session:TDXServerThread; WINCGI, Method, QueryString,
         PathInfo, PathTranslated, POSTContent_Type:string;
         POSTDataSize:Integer; var ResultLog:string):Boolean;
   published
      {     Property CGI_RequestProtocol:String
           Property CGI_RequestMethod:String
           Property CGI_ExecutablePath:String
           Property CGI_DocumentRoot:String
           Property CGI_LogicalPath:String
           Property CGI_PhysicalPath:String
           Property CGI_QueryString:String
           Property CGI_RequestRange:String
           Property CGI_Referer:String
           Property CGI_From:String
           Property CGI_UserAgent:String
           Property CGI_ContentLength:String
           Property CGI_ServerSoftware:String
           Property CGI_ServerName:String
           Property CGI_ServerPort:String
           Property CGI_ServerAdmin:String
           Property CGI_CGIVersion:String
           Property CGI_RemoteHost:String
           Property CGI_RemoteAddress:String
           Property CGI_AuthenticationMethod:String
           Property CGI_AuthenticationRealm:String
           Property CGI_AuthenticationUserName:String
           Property CGI_AuthenticationPassword:String
           Property CGI_RequestKeepAlive:String
           Property CGI_ContentType:String
           Property ACCEPT_Accept:String
           Property SYSTEM_GMTOffset:String
           Property SYSTEM_DebugMode:String
           Property SYSTEM_OutputFile:String
           Property SYSTEM_ContentFile:String
           Property EXTRA_HEADERS:String
           Property FORM_LITERAL:String
           Property FORM_EXTERNAL_Key:String
           Property FORM_HUGE_Key:String
           Property FORM_FILE_Key:String}
   end;

implementation

uses
   SysUtils; {StrPCopy}

var
   fDXThreadArray:TList;

   {$IFDEF OBJECTS_ONLY}

constructor TDXWINCGI.Create;
{$ELSE}

constructor TDXWINCGI.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   fDLLManager:=TDXDLLManager.Create;
   {$ELSE}
   inherited Create(AOwner);
   if not(csDesigning in ComponentState) then
      fDLLManager:=TDXDLLManager.Create(AOwner);
   {$ENDIF}
   fDXThreadArray:=TList.Create;
   fTimeout:=120000;
end;

destructor TDXWINCGI.Destroy;
var
   DXThreadRecord:PDXThreadRecord;

begin
   if Assigned(fDXThreadArray) then begin
      while fDXThreadArray.Count>0 do begin
         DXThreadRecord:=fDXThreadArray[0];
         Dispose(DXThreadRecord);
         fDXThreadArray.Delete(0);
      end;
      fDXThreadArray.Free;
      fDXThreadArray:=nil;
   end;
   if Assigned(fDLLManager) then fDLLManager.Free;
   inherited destroy;
end;

function{TDXWINCGI.}  FindThread(FindHCONN:HCONN):PDXThreadRecord;
var
   Loop:Integer;
   DXThreadRecord:PDXThreadRecord;

begin
   Result:=nil;
   Loop:=0;
   while (Loop<fDXThreadArray.Count)and(not Assigned(Result)) do begin
      DXThreadRecord:=PDXThreadRecord(fDXThreadArray[Loop]);
      if DXThreadRecord^.ThreadID=FindHCONN then
         Result:=DXThreadRecord
      else
         Inc(Loop);
   end;
end;

///////////////////////////////////////////////////////////////////////////////
// WINCGI CALLBACK FUNCTIONS
///////////////////////////////////////////////////////////////////////////////

function{TDXWINCGI.}  ServerVariable(ThreadID:HCONN; Variable:PChar;
   Buffer:Pointer; var Size:DWORD):LongBool; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
   Results:string;

begin
   Result:=False;
   DXThreadRecord:=FindThread(ThreadID);
   if not Assigned(DXThreadRecord) then Exit;
   Results:='';
   if Assigned(DXThreadRecord^.fGetServerVariable) then
      DXThreadRecord^.fGetServerVariable(DXThreadRecord^.Thread,
         string(Variable), Results);
   StrPCopy(PChar(Buffer), PChar(Results));
   Size:=Length(Results);
   Result:=True;
end;

function{TDXWINCGI.}  WriteClient(ThreadID:HCONN; Buf:Pointer; var Bytes:DWORD;
   dwReserved:DWORD):Bool; stdcall;
var
   DXThreadRecord:PDXThreadRecord;

begin
   Result:=False;
   DXThreadRecord:=FindThread(ThreadID);
   if not Assigned(DXThreadRecord) then Exit;
   {$IFDEF VER100}// D3
   DXThreadRecord^.Thread.Socket.BlockWrite(Buf, Bytes);
   {$ELSE}
   DXThreadRecord^.Thread.Socket.Write(Buf, Bytes);
   {$ENDIF}
   Result:=True;
end;

function{TDXWINCGI.}  ReadClient(ThreadID:HCONN; Buf:Pointer; var
   Size:DWORD):Bool; stdcall;
var
   DXThreadRecord:PDXThreadRecord;

begin
   Result:=False;
   DXThreadRecord:=FindThread(ThreadID);
   if not Assigned(DXThreadRecord) then Exit;
   {$IFDEF VER100}
   Size:=DXThreadRecord^.Thread.Socket.BlockRead(Buf, Size,
      DXThreadRecord^.fTimeout);
   {$ELSE}
   Size:=DXThreadRecord^.Thread.Socket.Read(Buf, Size,
      DXThreadRecord^.fTimeout);
   {$ENDIF}
   Result:=True;
end;

function{TDXWINCGI.}  ServerSupport(ThreadID:HCONN;
   HSE_REQ:DWORD;
   Buf:Pointer;
   var Size:DWORD;
   var DataType:DWORD):Bool; stdcall;
var
   DXThreadRecord:PDXThreadRecord;
   Results:string;

begin
end;

///////////////////////////////////////////////////////////////////////////////
// LAUNCH THE DLL!
///////////////////////////////////////////////////////////////////////////////

function TDXWINCGI.Execute(Session:TDXServerThread;
   WINCGI,
   Method,
   QueryString,
   PathInfo,
   PathTranslated,
   POSTContent_Type:string;
   POSTDataSize:Integer;
   var ResultLog:string):Boolean;
var
   DXThreadRecord:PDXThreadRecord;
   Loop:Integer;
   DLLHandle:THandle;
   ProcResult:LongInt;

begin
   Result:=False;
   New(DXThreadRecord);
   DXThreadRecord^.ThreadID:=GetCurrentThreadId;
   DXThreadRecord^.Thread:=Session;
   DXThreadRecord^.fGetServerVariable:=fGetServerVariable;
   DXThreadRecord^.fRedirectHeader:=fRedirectHeader;
   DXThreadRecord^.f200OKHeader:=f200OKHeader;
   DXThreadRecord^.fTimeout:=fTimeout;
   DLLHandle:=fDLLManager.GetDLLHandle(WINCGI);
   if DLLHandle=0 then begin
      ResultLog:=WINCGI+' was not registered!';
      Dispose(DXThreadRecord);
      Exit;
   end;
   // Actual Execution:
   fDXThreadArray.Add(DXThreadRecord);
   // RUNWINCGI HERE!!
   // Notify Result:
   // Clean-up
   Loop:=0;
   while (Loop<fDXThreadArray.Count) do begin
      DXThreadRecord:=fDXThreadArray[Loop];
      if DXThreadRecord^.ThreadID=GetCurrentThreadId then begin
         Dispose(DXThreadRecord);
         fDXThreadArray.Delete(Loop);
         Exit;
      end
      else
         Inc(Loop);
   end;
end;

end.

