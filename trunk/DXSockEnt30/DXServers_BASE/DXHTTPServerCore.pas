unit DXHTTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXHTTPServerCore
//       Author: G.E. Ozz Nixon Jr. (staff@bpdx.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2003
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2003
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
// Code Version: (4th Generation Code)
// ========================================================================
//  Description: implements HTTP (HyperText Transport Protocol)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

{$I DXSock.def}

uses
{$IFDEF CODE_TRACER}
   DXCodeTracer,
{$ENDIF}
   DXSocket,
   Classes,
   DXServerCore,
   DXISAPI,
   DXHTTPHeaderTools;

// RFC2068, Response Codes
const
   MaxStatusCodes=40;
   StatusCodes:array[0..MaxStatusCodes] of record
      Code:Integer;
      Msg:string
   end=
   ((Code:100;Msg:'Continue') ,
      (Code:101;Msg:'Switching Protocols') ,
      (Code:200;Msg:'OK') ,
      (Code:201;Msg:'Created') ,
      (Code:202;Msg:'Accepted') ,
      (Code:203;Msg:'Non-Authoritative Information') ,
      (Code:204;Msg:'No Content') ,
      (Code:205;Msg:'Reset Content') ,
      (Code:206;Msg:'Partial Content') ,
      (Code:300;Msg:'Multiple Choices') ,
      (Code:301;Msg:'Moved Permanently') ,
      (Code:302;Msg:'Moved Temporarily') ,
      (Code:303;Msg:'See Other') ,
      (Code:304;Msg:'Not Modified') ,
      (Code:305;Msg:'Use Proxy') ,
      (Code:306;Msg:'Unused') ,
      (Code:307;Msg:'Temporary Redirect') ,
      (Code:400;Msg:'Bad Request') ,
      (Code:401;Msg:'Unauthorized') ,
      (Code:402;Msg:'Payment Required') ,
      (Code:403;Msg:'Forbidden') ,
      (Code:404;Msg:'Not Found') ,
      (Code:405;Msg:'Method Not Allowed') ,
      (Code:406;Msg:'Not Acceptable') ,
      (Code:407;Msg:'Proxy Authentication Required') ,
      (Code:408;Msg:'Request Time-out') ,
      (Code:409;Msg:'Conflict') ,
      (Code:410;Msg:'Gone') ,
      (Code:411;Msg:'Length Required') ,
      (Code:412;Msg:'Precondition Failed') ,
      (Code:413;Msg:'Request Entity Too Large') ,
      (Code:414;Msg:'Request-URI Too Large') ,
      (Code:415;Msg:'Unsupported Media Type') ,
      (Code:416;Msg:'Requested Range Not Satisfiable') ,
      (Code:417;Msg:'Expectation Failed') ,
      (Code:500;Msg:'Internal Server Error') ,
      (Code:501;Msg:'Not Implemented') ,
      (Code:502;Msg:'Bad Gateway') ,
      (Code:503;Msg:'Service Unavailable') ,
      (Code:504;Msg:'Gateway Time-out') ,
      (Code:505;Msg:'HTTP Version Not Supported') ) ;

type
   HTTPTBasicEvent=procedure (ClientThread:TDXClientThread;HeaderInfo:PHeaderInfo;var EnableKeepAlive:Boolean) of object;
   HTTPTOtherEvent=procedure (ClientThread:TDXClientThread;HeaderInfo:PHeaderInfo;var Handled:Boolean) of object;

   TDXHTTPServerCore=class (TDXServerCore)
   private
      fOnCommandGET:HTTPTBasicEvent;// GET <file> HTTP/1.0
      fOnCommandPOST:HTTPTBasicEvent;// POST <file> HTTP/1.0
      fOnCommandHEAD:HTTPTBasicEvent;// HEAD <file> HTTP/1.0
      fOnCommandCHECKOUT:HTTPTBasicEvent;
      fOnCommandSHOWMETHOD:HTTPTBasicEvent;
      fOnCommandPUT:HTTPTBasicEvent;
      fOnCommandDELETE:HTTPTBasicEvent;
      fOnCommandLINK:HTTPTBasicEvent;
      fOnCommandUNLINK:HTTPTBasicEvent;
      fOnCommandCHECKIN:HTTPTBasicEvent;
      fOnCommandTEXTSEARCH:HTTPTBasicEvent;
      fOnCommandSPACEJUMP:HTTPTBasicEvent;
      fOnCommandSEARCH:HTTPTBasicEvent;
      fOnCommandOPTIONS:HTTPTBasicEvent;
      fOnCommandTRACE:HTTPTBasicEvent;
      fOnCommandCONNECT:HTTPTBasicEvent;
      fOnCommandPATCH:HTTPTBasicEvent;
      fOnCommandOther:HTTPTOtherEvent;
      fSupportKeepAlive:Boolean;
      fDXISAPI:TDXISAPI;
   protected
      procedure SetOnCommandGET (value:HTTPTBasicEvent) ;
      procedure SetOnCommandPOST (value:HTTPTBasicEvent) ;
      procedure SetOnCommandHEAD (value:HTTPTBasicEvent) ;
      procedure SetOnCommandCHECKOUT (value:HTTPTBasicEvent) ;
      procedure SetOnCommandSHOWMETHOD (value:HTTPTBasicEvent) ;
      procedure SetOnCommandPUT (value:HTTPTBasicEvent) ;
      procedure SetOnCommandDELETE (value:HTTPTBasicEvent) ;
      procedure SetOnCommandLINK (value:HTTPTBasicEvent) ;
      procedure SetOnCommandUNLINK (value:HTTPTBasicEvent) ;
      procedure SetOnCommandCHECKIN (value:HTTPTBasicEvent) ;
      procedure SetOnCommandTEXTSEARCH (value:HTTPTBasicEvent) ;
      procedure SetOnCommandSPACEJUMP (value:HTTPTBasicEvent) ;
      procedure SetOnCommandSEARCH (value:HTTPTBasicEvent) ;
      procedure SetOnCommandOPTIONS (value:HTTPTBasicEvent) ;
      procedure SetOnCommandTRACE (value:HTTPTBasicEvent) ;
      procedure SetOnCommandCONNECT (value:HTTPTBasicEvent) ;
      procedure SetOnCommandPATCH (value:HTTPTBasicEvent) ;
   public
      constructor Create (AOwner:TComponent) ;
{$IFNDEF OBJECTS_ONLY}override;
{$ENDIF}
      destructor Destroy;override;
      procedure ProcessSession (ClientThread:TDXClientThread) ;
      procedure AddBasicEvent (Command:string;EventProc:HTTPTBasicEvent) ;
      function HeaderText (StatusCode:Integer) :string;//2.0.12
      procedure Start;override;
      procedure Stop;override;
   published
      property SupportKeepAlive:Boolean read fSupportKeepAlive
         write fSupportKeepAlive;
      property OnCommandGET:HTTPTBasicEvent read fOnCommandGET
         write SetOnCommandGET;
      property OnCommandPOST:HTTPTBasicEvent read fOnCommandPOST
         write SetOnCommandPOST;
      property OnCommandHEAD:HTTPTBasicEvent read fOnCommandHEAD
         write SetOnCommandHEAD;
      property OnCommandCHECKOUT:HTTPTBasicEvent read fOnCommandCHECKOUT
         write SetOnCommandCHECKOUT;
      property OnCommandSHOWMETHOD:HTTPTBasicEvent read fOnCommandSHOWMETHOD
         write SetOnCommandSHOWMETHOD;
      property OnCommandPUT:HTTPTBasicEvent read fOnCommandPUT
         write SetOnCommandPUT;
      property OnCommandDELETE:HTTPTBasicEvent read fOnCommandDELETE
         write SetOnCommandDELETE;
      property OnCommandLINK:HTTPTBasicEvent read fOnCommandLINK
         write SetOnCommandLINK;
      property OnCommandUNLINK:HTTPTBasicEvent read fOnCommandUNLINK
         write SetOnCommandUNLINK;
      property OnCommandCHECKIN:HTTPTBasicEvent read fOnCommandCHECKIN
         write SetOnCommandCHECKIN;
      property OnCommandTEXTSEARCH:HTTPTBasicEvent read fOnCommandTEXTSEARCH
         write SetOnCommandTEXTSEARCH;
      property OnCommandSPACEJUMP:HTTPTBasicEvent read fOnCommandSPACEJUMP
         write SetOnCommandSPACEJUMP;
      property OnCommandSEARCH:HTTPTBasicEvent read fOnCommandSEARCH
         write SetOnCommandSEARCH;
      property OnCommandOPTIONS:HTTPTBasicEvent read fOnCommandOPTIONS
         write SetOnCommandOPTIONS;
      property OnCommandTRACE:HTTPTBasicEvent read fOnCommandTRACE
         write SetOnCommandTRACE;
      property OnCommandCONNECT:HTTPTBasicEvent read fOnCommandCONNECT
         write SetOnCommandCONNECT;
      property OnCommandPATCH:HTTPTBasicEvent read fOnCommandPATCH
         write SetOnCommandPATCH;
      property OnCommandOther:HTTPTOtherEvent read fOnCommandOther
         write fOnCommandOther;
      property ISAPIServer:TDXISAPI read fDXISAPI
         write fDXISAPI;
   end;

implementation

uses
   DXSock,
   SysUtils,
   DXString;

type
   PHTTPBasicEvent=^THTTPBasicEvent;
   THTTPBasicEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:HTTPTBasicEvent;
   end;

///////////////////////////////////////////////////////////////////////////////
//CREATE:
//       Define the Default Port number to Listen On.
///////////////////////////////////////////////////////////////////////////////

constructor TDXHTTPServerCore.Create (AOwner:TComponent) ;
begin
   inherited Create (AOwner) ;
   ServerPort:=80;
   Timeout:=30000;
   TDXXferTimeout:=500; // change timeout for socket leyer
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////

destructor TDXHTTPServerCore.Destroy;
var
   PBasicEvent:PHTTPBasicEvent;

begin
   if Assigned (fEventArray) then begin
      while fEventArray.Count>0 do begin
         case PHTTPBasicEvent (fEventArray[0]) .Tag of
            1:begin
                  PBasicEvent:=fEventArray[0];
                  Dispose (PBasicEvent) ;
               end;
         end;
         fEventArray.Delete (0) ;
      end;
   end;
   inherited Destroy;
end;

procedure TDXHTTPServerCore.AddBasicEvent (Command:string;EventProc:HTTPTBasicEvent) ;
var
   PBasicEvent:PHTTPBasicEvent;
   Loop:Integer;

begin
   Command:=Uppercase (Command) ;
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PHTTPBasicEvent (fEventArray[Loop]) .Command=Command then begin
         PHTTPBasicEvent (fEventArray[Loop]) .EventProcedure:=EventProc;
         Exit;
      end
      else Inc (Loop) ;
   end;
   New (PBasicEvent) ;
   PBasicEvent.Tag:=1;// Denotes Event in fEventArray is a TBasicEvent!
   PBasicEvent.Command:=Command;
   PBasicEvent.EventProcedure:=EventProc;
   fEventArray.Add (PBasicEvent) ;
end;

procedure TDXHTTPServerCore.SetOnCommandGET (value:HTTPTBasicEvent) ;
begin
   fOnCommandGET:=Value;
   AddBasicEvent ('GET',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandPOST (value:HTTPTBasicEvent) ;
begin
   fOnCommandPOST:=Value;
   AddBasicEvent ('POST',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandHEAD (value:HTTPTBasicEvent) ;
begin
   fOnCommandHEAD:=Value;
   AddBasicEvent ('HEAD',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandCHECKOUT (value:HTTPTBasicEvent) ;
begin
   fOnCommandCHECKOUT:=Value;
   AddBasicEvent ('CHECKOUT',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandSHOWMETHOD (value:HTTPTBasicEvent) ;
begin
   fOnCommandSHOWMETHOD:=Value;
   AddBasicEvent ('SHOWMETHOD',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandPUT (value:HTTPTBasicEvent) ;
begin
   fOnCommandPUT:=Value;
   AddBasicEvent ('PUT',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandDELETE (value:HTTPTBasicEvent) ;
begin
   fOnCommandDELETE:=Value;
   AddBasicEvent ('DELETE',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandLINK (value:HTTPTBasicEvent) ;
begin
   fOnCommandLINK:=Value;
   AddBasicEvent ('LINK',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandUNLINK (value:HTTPTBasicEvent) ;
begin
   fOnCommandUNLINK:=Value;
   AddBasicEvent ('UNLINK',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandCHECKIN (value:HTTPTBasicEvent) ;
begin
   fOnCommandCHECKIN:=Value;
   AddBasicEvent ('CHECKIN',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandTEXTSEARCH (value:HTTPTBasicEvent) ;
begin
   fOnCommandTEXTSEARCH:=Value;
   AddBasicEvent ('TEXTSEARCH',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandSPACEJUMP (value:HTTPTBasicEvent) ;
begin
   fOnCommandSPACEJUMP:=Value;
   AddBasicEvent ('SPACEJUMP',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandSEARCH (value:HTTPTBasicEvent) ;
begin
   fOnCommandSEARCH:=Value;
   AddBasicEvent ('SEARCH',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandOPTIONS (value:HTTPTBasicEvent) ;
begin
   fOnCommandOPTIONS:=Value;
   AddBasicEvent ('OPTIONS',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandTRACE (value:HTTPTBasicEvent) ;
begin
   fOnCommandTRACE:=Value;
   AddBasicEvent ('TRACE',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandCONNECT (value:HTTPTBasicEvent) ;
begin
   fOnCommandCONNECT:=Value;
   AddBasicEvent ('CONNECT',Value) ;
end;

procedure TDXHTTPServerCore.SetOnCommandPATCH (value:HTTPTBasicEvent) ;
begin
   fOnCommandPATCH:=Value;
   AddBasicEvent ('PATCH',Value) ;
end;

procedure TDXHTTPServerCore.ProcessSession (ClientThread:TDXClientThread) ;
var
   S,WS:string;
   HeaderInfo:PHeaderInfo;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;
   WantKeepAlive:Boolean;
   ManualTimeout:Comp;

   procedure NotHandled;
   begin
      ClientThread.Socket.Write (
         'HTTP/1.0 '+HeaderText (400) +#13#10+#13#10+
         '<html><head><title>Error</title>'+
         '<body><h1>Error</h1><hr><h3>Unsupported Method <B>"'+
         HeaderInfo.Method+'"</B></h3></body></head></html>') ;
   end;

   procedure BuildHeader (ClientHeader:string) ;
   var
      line,token1,token2:string;
      Error:Integer;
      Ch:Char;
      I:Integer;

      procedure AddToUnknown;
      begin
         if HeaderInfo^.Unknown='' then HeaderInfo^.Unknown:=token1+': '+line
         else HeaderInfo^.Unknown:=HeaderInfo^.Unknown+#13#10+token1+': '+line;
      end;

   begin
      line:=ClientHeader;
      with HeaderInfo^ do begin
         if RAW='' then begin
            RAW:=Line;
            Method:=Uppercase (FetchByChar (line,#32,False) ) ;
            URI:=EscapeDecode (FetchByChar (line,#32,False) ) ;
            if CharPos ('?',URI) >0 then begin
               QueryString:=Copy (URI,CharPos ('?',URI) +1,Length (URI) ) ;
               Delete (URI,CharPos ('?',URI) ,Length (URI) ) ;
            end;
            Protocol:=FetchByChar (line,#32,False) ;
         end
         else begin
            ALL_RAW:=ALL_RAW+Line+#13#10;
            token1:=Uppercase (FetchByChar (line,#32,False) ) ;
            // 4RC2
            I:=Length(Token1);
            Setlength(Token2,I);
            While I>0 do Begin
               Case Token1[i] of
                  '-':Token2[i]:='_';
                  Else Token2[i]:=Token1[i];
               End;
               Dec(I);
            End;
//            token2:=StringReplace (token1,'-','_', [rfReplaceAll]) ;
            ALL_HTTP:=ALL_HTTP+'HTTP_'+Token2+Line+#13#10;
            Ch:=Token1[1];
            case Ch of
               'A':if token1='ACCEPT:' then Accept:=Accept+Line
                  else if token1='ACCEPT-CHARSET:' then AcceptCharset:=Line
                  else if token1='ACCEPT-ENCODING:' then AcceptEncoding:=Line
                  else if token1='ACCEPT-LANGUAGE:' then AcceptLanguage:=Line
                  else if token1='ALLOW:' then Allow:=Line
                  else if token1='AUTHORIZATION:' then begin
                     AuthType:=FetchByChar (line,#32,False) ;// Usually is "Basic"
                     token2:=FetchByChar (line,#32,False) ;
                     token2:=Base64ToString (token2) ;// Decode the "Basic" encoded string
                     AuthName:=FetchByChar (token2,':',False) ;
                     AuthPass:=FetchByChar (token2,';',False) ;// should actually be leftovers
                  end
                  else AddToUnknown;
               'C':if token1='CACHE-CONTROL:' then CacheControl:=Line
                  else if token1='CONNECTION:' then Connection:=Line
                  else if token1='CACHE-INFO:' then CacheInfo:=Line
                  else if token1='CONTENT-LENGTH:' then begin
                     Val (FetchByChar (line,#32,False) ,ContentLength,Error) ;
                  end
                  else if token1='CONTENT-TYPE:' then ContentType:=Line
                  else if token1='COOKIE:' then Cookie:=Line
                  else AddToUnknown;
               'D':if token1='DATE:' then Date:=Line
                  else AddToUnknown;
               'F':if token1='FROM:' then From:=Line
                  else if token1='FORWARDED:' then Forwarded:=Line
                  else if token1='FORWARDED-FOR:' then ForwardedFor:=Line
                  else AddToUnknown;
               'H':if token1='HOST:' then Host:=Line
                  else AddToUnknown;
               'I':if token1='IF-MODIFIED-SINCE:' then IfModSince:=Line
                  else if token1='IF-MATCH:' then IfMatch:=Line
                  else if token1='IF-NONE-MATCH:' then IfNoneMatch:=Line
                  else if token1='IF-RANGE:' then IfRange:=Line
                  else if token1='IF-UNMODIFIED-SINCE:' then IfUnModSince:=Line
                  else AddToUnknown;
               'K':if token1='KEEP-ALIVE:' then KeepAlive:=Line
                  else AddToUnknown;
               'M':if token1='MAX-FORWARDS:' then MaxForwards:=Line
                  else AddToUnknown;
               'P':if token1='PUBLIC:' then PublicCache:=Line
                  else if token1='PRAGMA:' then Pragma:=Line
                  else if token1='PROXY-CONNECTION:' then ProxyConnection:=Line
                  else if token1='PROXY-AUTHORIZATION:' then ProxyAuthorization:=Line
                  else AddToUnknown;
               'R':if token1='REFERER:' then Referer:=Line
                  else if token1='RANGE:' then Range:=Line
                  else AddToUnknown;
               'T':if token1='TRANSFER-ENCODING:' then TransferEncoding:=Line
                  else AddToUnknown;
               'U':if token1='UPGRADE:' then Upgrade:=Line
                  else if token1='USER-AGENT:' then
                     UserAgent:=Trim (Line)
                  else AddToUnknown;
               'V':if token1='VIA:' then Via:=Line
                  else AddToUnknown;
               'W':if token1='WEFERER:' then Weferer:=Line
                  else if token1='WSER-AGENT:' then WserAgent:=Line
                  else AddToUnknown;
            else AddToUnknown;
            end;
         end;
      end;
   end;

   procedure ManuallyGetHost;
   begin
      with HeaderInfo^ do begin
         if Quickpos ('//',URI) >0 then begin
            Host:=Copy (URI,1,QuickPos ('//',URI) +1) ;
            Delete (URI,1,Length (Host) ) ;
         end;
         if URI<>'/' then begin
            Host:=Host+Copy (URI,1,CharPos ('/',URI) ) ;
            Delete (URI,1,CharPos ('/',URI) ) ;
         end;
      end;
   end;

begin
   New (HeaderInfo) ;
   repeat
      with HeaderInfo^ do begin
         ClientAddr:=ClientThread.Socket.PeerIPAddress;
         ClientHost:=HeaderInfo^.ClientAddr;
         ContentLength:=0;
         Raw:='';
         Protocol:='';
         Method:='';
         URI:='';
         PhysPath:='';
         Allow:='';
         AuthType:='';
         AuthName:='';
         AuthPass:='';
         Date:='';
         Pragma:='';
         CacheControl:='';
         Connection:='';
         TransferEncoding:='';
         Upgrade:='';
         Via:='';
         Host:='';
         From:='';
         IfMatch:='';
         IfNoneMatch:='';
         IfRange:='';
         IfRange:='';
         IfUnModSince:='';
         MaxForwards:='';
         ProxyAuthorization:='';
         KeepAlive:='';
         PublicCache:='';
         Range:='';
         Referer:='';
         UserAgent:='';
         ContentType:='';
         Accept:='';
         AcceptCharset:='';
         AcceptEncoding:='';
         AcceptLanguage:='';
         Cookie:='';
         ClientName:='';
         QueryString:='';
         HostRootPath:='';
         Weferer:='';
         WserAgent:='';
         CacheInfo:='';
         ProxyConnection:='';
         Forwarded:='';
         ForwardedFor:='';
         All_HTTP:='';
         All_RAW:='';
         PostData:='';
         Unknown:='';
      end;
      fbForceAbort:=False;
      with ClientThread.Socket do begin
         if not fSupportKeepAlive then begin
            S:='';
            while (QuickPos (#13#10#13#10,S) =0) and
               (Length (S) <8192) do begin
               WS:=ReadStr (-1) ;
               If Ws='' then Begin
                  if (not Connected) then begin
{$IFDEF CODE_TRACER}
                     if Assigned (ClientThread.DXCodeTracer) then begin
                        ClientThread.DXCodeTracer.SendMessage (dxctDebug,'TDXServerCore.ProcessSession - Header timeout - aborting.') ;
                     end;
{$ENDIF}
                     Dispose (Headerinfo) ;
                     Exit;
                  end;
                  DoSleepEx(10);
               End
               Else S:=S+Ws;
            end;
         end
         else begin
            ManualTimeout:=TimeCounter+Timeout;
            S:='';
            while (QuickPos (#13#10#13#10,S) =0) and
               (Length (S) <8192) and
               (ManualTimeout>TimeCounter) do begin
               WS:=ReadStr (-1) ;
               If Ws='' then Begin
                  if (not Connected) then begin
{$IFDEF CODE_TRACER}
                     if Assigned (ClientThread.DXCodeTracer) then begin
                        ClientThread.DXCodeTracer.SendMessage (dxctDebug,'TDXServerCore.ProcessSession - Header timeout - aborting.') ;
                     end;
{$ENDIF}
                     Dispose (Headerinfo) ;
                     Exit;
                  end;
                  DoSleepEx(10);
               End
               Else S:=S+Ws;
            end;
         end;
         if (LastReadTimeout) or (S=''{new}) then begin
{$IFDEF CODE_TRACER}
            if Assigned (ClientThread.DXCodeTracer) then begin
               ClientThread.DXCodeTracer.SendMessage (dxctDebug,'TDXServerCore.ProcessSession - Header timeout - aborting.') ;
            end;
{$ENDIF}
            Dispose (Headerinfo) ;
            Exit;
         end;
         while Length (S) >0 do begin
            Ws:=Copy (S,1,QuickPos (#13#10,S) -1) ;
            if Assigned (OnFilter) then begin
               Loop:=FilterRead (@WS[1],OutData,Length (WS) ,ClientThread) ;
               SetLength (WS,Loop) ;
               if Assigned (Outdata) then begin
                  Move (TDXBSArray (OutData^) ,WS[1],Loop) ;
                  OnFilter (ddFreePointer,nil,OutData,Loop,Loop,WasHandled,ClientThread) ;
               end;
            end;
         //SF_NOTIFY_READ_RAW_DATA
            if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then
               fDXISAPI.ServerRawRead (WS,Length (WS) ) ;
            BuildHeader (Ws) ;
            Delete (S,1,Length (Ws) +2) ;
            if Copy (S,1,2) =#13#10 then begin//end of header
               HeaderInfo^.PostData:=Copy (S,3,Length (S) ) ;
               S:='';
            end;
         end;
         if HeaderInfo^.Host='' then ManuallyGetHost;
         Delete (HeaderInfo^.ALL_HTTP,Length (HeaderInfo^.ALL_HTTP) -1,2) ;
         HeaderInfo^.ALL_HTTP:=HeaderInfo^.ALL_HTTP+#0;
      //SF_NOTIFY_PREPROC_HEADERS
         if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then
            fDXISAPI.ServerPreprocHeaderEvent (HeaderInfo) ;
         Loop:=0;
         WasHandled:=False;
         WantKeepAlive:=fSupportKeepAlive;
         while (Loop<fEventArray.Count) and (not WasHandled) do begin
            if PHTTPBasicEvent (fEventArray[Loop]) .Command=HeaderInfo^.Method then begin
               case PHTTPBasicEvent (fEventArray[Loop]) .Tag of
                  1:if Assigned (PHTTPBasicEvent (fEventArray[Loop]) .EventProcedure) then
                        HTTPTBasicEvent (PHTTPBasicEvent (fEventArray[Loop]) .EventProcedure) (ClientThread,HeaderInfo,WantKeepAlive) ;
               end;
               WasHandled:=True;
            end
            else Inc (Loop) ;
         end;
         if not WasHandled then begin
            if assigned (OnCommandOther) then
               OnCommandOther (ClientThread,HeaderInfo,WasHandled) ;
            if not WasHandled then NotHandled;
            WantKeepAlive:=False;
         end;
      end;
   //SF_NOTIFY_END_OF_REQUEST
      if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then
         fDXISAPI.ServerEndOfRequest;
      if not fSupportKeepAlive then WantKeepAlive:=False;
      if WantKeepAlive then ProcessWindowsMessageQueue;// 9/11/2002
{$IFDEF CODE_TRACER}
      if Assigned (ClientThread.DXCodeTracer) then begin
         if WantKeepAlive then
            ClientThread.DXCodeTracer.SendMessage (dxctDebug,'TDXServerCore.ProcessSession - looping for keepalive.') ;
      end;
{$ENDIF}
   until (not WantKeepAlive) or (not ClientThread.Socket.Connected) ;
   Dispose (HeaderInfo) ;//2.4
   //SF_NOTIFY_END_OF_NET_SESSION
   if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then
      fDXISAPI.ServerEndSession;
end;

function TDXHTTPServerCore.HeaderText (StatusCode:Integer) :string;
var
   Loop:Integer;

begin
   if StatusCode>404 then begin// divide and conquor
      Loop:=MaxStatusCodes;
      while StatusCodes[Loop].Code>StatusCode do Dec (Loop) ;
   end
   else begin
      Loop:=0;
      while StatusCodes[Loop].Code<StatusCode do Inc (Loop) ;
   end;
   if StatusCodes[Loop].Code=StatusCode then
      Result:=IntToStr (StatusCode) +#32+StatusCodes[Loop].Msg
   else
      Result:='500 Internal Server Error';
end;

procedure TDXHTTPServerCore.Start;
begin
   if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then
      fDXISAPI.ServerStartEvent;
   inherited Start;
end;

procedure TDXHTTPServerCore.Stop;
begin
   if assigned (fDXISAPI) and (fDXISAPI.FilterCount>0) then begin
      fDXISAPI.ServerStopEvent;
   end;
   inherited Stop;
end;

end.

