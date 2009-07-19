unit DXHTTPHeaderTools;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXHTTPHeaderTools
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

uses
  DXString,
  Classes;

{$I DXSock.def}

Type
  PHeaderInfo=^THeaderInfo;
  THeaderInfo=Record
    ContentLength:Integer;
    Raw:String;       // Original Header Request
    Protocol:String;
    Method:String;    //GET, HEAD, etc.  All caps!!!
    URI:String;
    PhysPath:String;
    Allow:String;
    AuthType:String;  // BASIC
    AuthName:String;
    AuthPass:String;
    Date:String;
    Pragma:String;
    CacheControl:String;
    Connection:String;
    TransferEncoding:String;
    Upgrade:String;
    Via:String;
    Host:String;      // multiple domains on single IP!
    From:String;
    IfModSince:String;
    IfMatch:String;
    IfNoneMatch:String;
    IfRange:String;
    IfUnModSince:String;
    MaxForwards:String;
    ProxyAuthorization:String;
    KeepAlive:String;
    PublicCache:String; // 2.3 renamed field!
    Range:String;
    Referer:String;
    UserAgent:String;
    ContentType:String;
    Accept:String;
    AcceptCharset:String;
    AcceptEncoding:String;
    AcceptLanguage:String;
    ClientName:String;
    ClientAddr:String;
    ClientHost:String;
    Cookie:String;
    QueryString:String;
    Weferer:String;
    WserAgent:String;
    Forwarded:String;
    ForwardedFor:String;
    ProxyConnection:String;
    CacheInfo:String;
    PostData:String;  // 3.0
    HostRootPath:String;  // needed for SSI+ Banner routines and ISAPI Variables
    All_RAW:String; // needed for trace, and ISAPIs!
    All_HTTP:String;
    Unknown:String;
  end;

  TDXHTTPHeaderTools = class(TDXComponent)
  private
    fSessionHeader:PHeaderInfo;
  protected
    Function FindUnknownHeader(ServerVariable:String):String;
    Procedure ReplaceUnknownHeader(ServerVariable, VariableValue:String);
    Function UnknownHeaderExists(ServerVariable:String):Boolean;
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    Destructor Destroy; override;
    Procedure Init(HTTPHeader:PHeaderInfo);
    // 2.0.12
    Function HeaderGetString(ServerVariable:String):String;
    Function HeaderGetInteger(ServerVariable:String):Integer;
    Function HeaderDataExists(ServerVariable:String):Boolean;
    Function QueryGetString(ServerVariable:String):String;
    Function QueryGetInteger(ServerVariable:String):Integer;
    Function QueryDataExists(ServerVariable:String):Boolean;

    Function AddHeader(ServerVariable, VariableValue:String):Boolean;
    Function SetHeader(ServerVariable, VariableValue:String):Boolean;
    Function ToStrings:TStrings;

  published
  end;

implementation

Uses
   SysUtils;

constructor TDXHTTPHeaderTools.Create(AOwner:TComponent);
Begin
   Inherited Create(AOwner);
end;

Destructor TDXHTTPHeaderTools.Destroy;
Begin
   Inherited Destroy;
end;

Procedure TDXHTTPHeaderTools.Init(HTTPHeader:PHeaderInfo);
Begin
   fSessionHeader:=HTTPHeader;
   fSessionHeader^.ContentLength:=0;
End;

Function TDXHTTPHeaderTools.HeaderGetString(ServerVariable:String):String;
Var
   Ch:Char;
   Ws:String;
   URI2:String;

Begin
   Result:='';
   Ws:=Uppercase(ServerVariable);
   If Ws='' then Exit; //2.0.11
   If (Copy(Ws,1,5)='HTTP_') then Begin
      Delete(Ws,1,5); // 3.0
   End;
   Ch:=Ws[1];
   With fSessionHeader^ do
   Case Ch of
      'A':If (Ws='ALLOW') then Result:=Allow
          Else If (Ws='ALL_RAW') then Result:=All_Raw // .9k & 2.3b
          Else If (Ws='ALL_HTTP') then Result:=All_HTTP // 3.0
          Else If (Ws='AUTHTYPE') or (Ws='AUTH_TYPE') then Result:=AuthType
          Else If (Ws='AUTHNAME') or (Ws='AUTH_NAME') or (Ws='AUTH_USER') then Result:=AuthName
          Else If (Ws='AUTHPASS') or (Ws='AUTH_PASS') or (Ws='AUTH_PASSWORD') then Result:=AuthPass
          Else If (Ws='ACCEPT') then Result:=Accept
          Else If (Ws='ACCEPT_CHARSET') or (Ws='ACCEPTCHARSET') then Result:=AcceptCharset
          Else If (Ws='ACCEPT_ENCODING') or (Ws='ACCEPTENCODING') then Result:=AcceptEncoding
          Else If (Ws='ACCEPT_LANGUAGE') or (Ws='ACCEPTLANGUAGE') then Result:=AcceptLanguage
          Else Result := FindUnknownHeader(ServerVariable);
      'C':If (Ws='CONTENT_LENGTH') or (Ws='CONTENTLENGTH') then Result:=IntToStr(ContentLength)
          Else If (Ws='CACHECONTROL') or (Ws='CACHE_CONTROL') then Result:=CacheControl
          Else If (Ws='CACHE_INFO') then Result:=CacheInfo
          Else If (Ws='CONNECTION') then Result:=Connection
          Else If (Ws='CONTENT_TYPE') or (Ws='CONTENTTYPE') then Result:=ContentType
          Else If (Ws='CLIENTNAME') or (Ws='CLIENT_NAME') then Result:=ClientName
          Else If (Ws='CLIENTADDR') or (Ws='CLIENT_ADDR') then Result:=ClientAddr
          Else If (Ws='COOKIE') then Result:=Cookie
          Else Result := FindUnknownHeader(ServerVariable);
      'D':If (Ws='DATE') then Result:=Date
          Else Result := FindUnknownHeader(ServerVariable);
      'F':If (Ws='FROM') then Result:=From
          Else If (Ws='FORWARDED') then Result:=Forwarded
          Else If (Ws='FORWARDED_FOR') then Result:=ForwardedFor
          Else Result := FindUnknownHeader(ServerVariable);
      'H':If (Ws='HOST') then Begin
             Result:=Host;
             If CharPos('@',Result)>0 then Delete(Result,1,CharPos(':',Result));
             If CharPos(':',Result)>0 then Delete(Result,CharPos(':',Result),Length(Result));
          End
          Else If (Ws='HTTPS') then Result:='off'
          Else Result := FindUnknownHeader(ServerVariable);
      'I':If (Ws='IFMODSINCE') or (Ws='IF_MODSINCE') or (Ws='IF_MOD_SINCE') then Result:=IfModSince
          Else If (Ws='IFMATCH') or (Ws='IF_MATCH') then Result:=IfMatch
          Else If (Ws='IFNONEMATCH') or (Ws='IF_NONEMATCH') or (Ws='IF_NONE_MATCH') then Result:=IfNoneMatch
          Else If (Ws='IFRANGE') or (Ws='IF_RANGE') then Result:=IfRange
          Else If (Ws='IFUNMODSINCE') or (Ws='IF_UNMODSINCE') or (Ws='IF_UNMOD_SINCE') then Result:=IfUnModSince
          Else Result := FindUnknownHeader(ServerVariable);
      'K':If (Ws='KEEPALIVE') or (Ws='KEEP_ALIVE') then Result:=KeepAlive
          Else Result := FindUnknownHeader(ServerVariable);
      'M':If (Ws='METHOD') then Result:=Method
          Else If (Ws='MAXFORWARDS') or (Ws='MAX_FORWARDS') then Result:=MaxForwards
          Else Result := FindUnknownHeader(ServerVariable);
      'P':If (Ws='PROTOCOL') then Result:=Protocol
          Else If (Ws='PRAGMA') then Result:=Pragma
          Else if (Ws='PROXYAUTHORIZATION') or (Ws='PROXY_AUTHORIZATION') then Result:=ProxyAuthorization
          Else if (Ws='PUBLIC') then Result:=PublicCache
          Else if (Ws='PROXY_CONNECTION') then Result:=ProxyConnection
          Else Result := FindUnknownHeader(ServerVariable);
      'Q':If (Ws='QUERYSTRING') or (Ws='QUERY_STRING') then Result:=QueryString
          Else Result := FindUnknownHeader(ServerVariable);
      'R':If (Ws='RANGE') then Result:=Range
          Else If (Ws='REFERER') or (Ws='REFERRER') or (Ws='REFFERER') then Result:=Referer
          Else If (Ws='REQUEST_METHOD') then Result:=Method
          Else If (Ws='REMOTE_NAME') or (Ws='REMOTE_HOST') then Result:=ClientName
          Else If (Ws='REMOTE_ADDR') then Result:=ClientAddr
          Else If (Ws='REQUEST_URI') then Begin
             URI2:=ToUnixSlashes(URI);
             If CharPos('/',URI2)=0 then Result:='/'+URI2
             Else Begin
                If Copy(URI2,1,1)<>'/' then URI2:='/'+URI2;
                Result:=URI2; // 4/2/2002 Copy(URI2,1,PosLastChar('/',URI2));
             End;
          End
          Else Result := FindUnknownHeader(ServerVariable);
      'S':If (Ws='SERVER_PROTOCOL') then Result:=Protocol
          Else If (Ws='SERVER_NAME') then Begin
             Result:=Host;
             If CharPos('@',Result)>0 then Delete(Result,1,CharPos(':',Result));
             If CharPos(':',Result)>0 then Delete(Result,CharPos(':',Result),Length(Result));
          End
          Else If (Ws='SERVER_PORT_SECURE') then Result:='0'
          Else Result := FindUnknownHeader(ServerVariable);
      'T':If (Ws='TRANSFERENCODING') or (Ws='TRANSFER_ENCODING') then Result:=TransferEncoding
          Else Result := FindUnknownHeader(ServerVariable);
      'U':If (Ws='URI') then Begin
             URI2:=ToUnixSlashes(URI);
             If CharPos('/',URI2)=0 then Result:='/'+URI2
             Else Begin
                If Copy(URI2,1,1)<>'/' then URI2:='/'+URI2;
                Result:=URI2; // 4/2/2002 Copy(URI2,1,PosLastChar('/',URI2));
             End;
          End
          Else If (Ws='URL') then Begin
             URI2:=ToUnixSlashes(URI);
             If Copy(URI2,1,1)='/' then Result:=URI2
             Else Result:='/'+URI2;
          End
          Else If (Ws='UPGRADE') then Result:=Upgrade
          Else if (Ws='USERAGENT') or (Ws='USER_AGENT') then Result:=UserAgent
          Else Result := FindUnknownHeader(ServerVariable);
      'V':If (Ws='VIA') then Result:=Via
          Else Result := FindUnknownHeader(ServerVariable);
      'W':If (Ws='WEFERER') then Result:=Weferer
          Else If (Ws='WSER_AGENT') then Result:=WserAgent
          Else Result := FindUnknownHeader(ServerVariable);
       Else
          Result := FindUnknownHeader(ServerVariable);
   End;
End;

Function TDXHTTPHeaderTools.HeaderGetInteger(ServerVariable:String):Integer;
Var
   Ch:Char;
   Ws:String;

Begin
   Result:=0;
   Ws:=Uppercase(ServerVariable);
   If Ws='' then Exit; // 2.0.11
   Ch:=Ws[1];
   With fSessionHeader^ do
   Case Ch of
      'C':If (Ws='CONTENT_LENGTH') or (Ws='CONTENTLENGTH') then Result:=ContentLength;
   End;
End;

Function TDXHTTPHeaderTools.HeaderDataExists(ServerVariable:String):Boolean;
Begin
   Result:=HeaderGetString(ServerVariable)<>'';
End;

// 2.0.12
Function TDXHTTPHeaderTools.QueryGetString(ServerVariable:String):String;
Var
   Ws:String;
   Tmp:String;
   Value:String;

Begin
   Result:='';
   Tmp:=fSessionHeader.QueryString+'&';
   While (CharPos('&',Tmp)>0) do Begin
      Ws:=Copy(Tmp,1,CharPos('&',Tmp)-1);
      Delete(Tmp,1,CharPos('&',Tmp));
      Value:=Copy(Ws,1,CharPos('=',Ws)-1);
      Delete(Ws,1,CharPos('=',Ws));
      If Uppercase(Value)=Uppercase(ServerVariable) then Begin
         If Result<>'' then Result:=Result+#13#10+Ws
         Else Result:=Ws;
      End;
   End;
End;

Function TDXHTTPHeaderTools.QueryGetInteger(ServerVariable:String):Integer;
Var
   Ws:String;

Begin
   Ws:=QueryGetString(ServerVariable);
   If (Ws<>'') and (IsNumericString(Ws)) then Result:=StrToInt(Ws)
   Else Result:=-1;
End;

Function TDXHTTPHeaderTools.QueryDataExists(ServerVariable:String):Boolean;
Begin
   Result:=QueryGetString(ServerVariable)<>'';
End;

Function TDXHTTPHeaderTools.AddHeader(ServerVariable, VariableValue:String):Boolean;
Var
   Ch:Char;
   Ws:String;

Procedure AddToUnknown; // 7-27
Begin
   If fSessionHeader^.Unknown='' then fSessionHeader^.Unknown:=ServerVariable+': '+VariableValue
   Else fSessionHeader^.Unknown:=fSessionHeader^.Unknown+#13#10+ServerVariable+': '+VariableValue;
End;

Begin
   Result:=False;
   Ws:=Uppercase(ServerVariable);
   If Ws='' then Exit;
// 7-27
   Ws:=StringReplace(Ws,'-','_',[rfReplaceAll]);
   Ch:=Ws[1];
   With fSessionHeader^ do
   Case Ch of
      'A':If (Ws='ALLOW') then Allow:=VariableValue
          Else If (Ws='AUTHTYPE') or (Ws='AUTH_TYPE') then AuthType:=VariableValue
          Else If (Ws='AUTHNAME') or (Ws='AUTH_NAME') then AuthName:=VariableValue
          Else If (Ws='AUTHPASS') or (Ws='AUTH_PASS') then AuthPass:=VariableValue
          Else If (Ws='ACCEPT') then Accept:=VariableValue
          Else If (Ws='ACCEPT_CHARSET') or (Ws='ACCEPTCHARSET') then AcceptCharset:=VariableValue
          Else If (Ws='ACCEPT_ENCODING') or (Ws='ACCEPTENCODING') then AcceptEncoding:=VariableValue
          Else If (Ws='ACCEPT_LANGUAGE') or (Ws='ACCEPTLANGUAGE') then AcceptLanguage:=VariableValue
          Else AddToUnknown;
      'C':If (Ws='CONTENT_LENGTH') or (Ws='CONTENTLENGTH') then ContentLength:=StrToInt(VariableValue)
          Else If (Ws='CACHECONTROL') or (Ws='CACHE_CONTROL') then CacheControl:=VariableValue
          Else If (Ws='CACHE_INFO') then CacheInfo:=VariableValue
          Else If (Ws='CONNECTION') then Connection:=VariableValue
          Else If (Ws='CONTENT_TYPE') or (Ws='CONTENTTYPE') then ContentType:=VariableValue
          Else If (Ws='CLIENTNAME') or (Ws='CLIENT_NAME') then ClientName:=VariableValue
          Else If (Ws='CLIENTADDR') or (Ws='CLIENT_ADDR') then ClientAddr:=VariableValue
          Else If (Ws='COOKIE') then Cookie:=VariableValue
          Else AddToUnknown;
      'D':If (Ws='DATE') then Date:=VariableValue
          Else AddToUnknown;
      'F':If (Ws='FROM') then From:=VariableValue
          Else If (Ws='FORWARDED') then Forwarded:=VariableValue
          Else If (Ws='FORWARDED_FOR') then ForwardedFor:=VariableValue
          Else AddToUnknown;
      'H':If (Ws='HOST') then Host:=VariableValue
          Else AddToUnknown;
      'I':If (Ws='IFMODSINCE') or (Ws='IF_MODSINCE') or (Ws='IF_MOD_SINCE') then IfModSince:=VariableValue
          Else If (Ws='IFMATCH') or (Ws='IF_MATCH') then IfMatch:=VariableValue
          Else If (Ws='IFNONEMATCH') or (Ws='IF_NONEMATCH') or (Ws='IF_NONE_MATCH') then IfNoneMatch:=VariableValue
          Else If (Ws='IFRANGE') or (Ws='IF_RANGE') then IfRange:=VariableValue
          Else If (Ws='IFUNMODSINCE') or (Ws='IF_UNMODSINCE') or (Ws='IF_UNMOD_SINCE') then IfUnModSince:=VariableValue
          Else AddToUnknown;
      'K':If (Ws='KEEPALIVE') or (Ws='KEEP_ALIVE') then KeepAlive:=VariableValue
          Else AddToUnknown;
      'M':If (Ws='METHOD') then Method:=VariableValue
          Else If (Ws='MAXFORWARDS') or (Ws='MAX_FORWARDS') then MaxForwards:=VariableValue
          Else AddToUnknown;
      'P':If (Ws='PROTOCOL') then Protocol:=VariableValue
          Else If (Ws='PRAGMA') then Pragma:=VariableValue
          Else if (Ws='PROXYAUTHORIZATION') or (Ws='PROXY_AUTHORIZATION') then ProxyAuthorization:=VariableValue
          Else if (Ws='PUBLIC') then PublicCache:=VariableValue
          Else if (Ws='PROXY_CONNECTION') then ProxyConnection:=VariableValue
          Else AddToUnknown;
      'Q':If (Ws='QUERYSTRING') or (Ws='QUERY_STRING') then QueryString:=VariableValue
          Else AddToUnknown;
      'R':If (Ws='RANGE') then Range:=VariableValue
          Else If (Ws='REFERER') or (Ws='REFERRER') or (Ws='REFFERER') then Referer:=VariableValue
          Else AddToUnknown;
      'U':If (Ws='URI') then URI:=VariableValue
          Else If (Ws='UPGRADE') then Upgrade:=VariableValue
          Else if (Ws='USERAGENT') or (Ws='USER_AGENT') then UserAgent:=VariableValue
          Else AddToUnknown;
      'T':If (Ws='TRANSFERENCODING') or (Ws='TRANSFER_ENCODING') then TransferEncoding:=VariableValue
          Else AddToUnknown;
      'V':If (Ws='VIA') then Via:=VariableValue // 7-27
          Else AddToUnknown;
      'W':If (Ws='WEFERER') then Weferer:=VariableValue // 7-27
          Else If (Ws='WSER_AGENT') then WserAgent:=VariableValue // 7-27
          Else AddToUnknown;
     Else
       AddToUnknown;
   End;//case
   Result:=True;
End;

Function TDXHTTPHeaderTools.SetHeader(ServerVariable, VariableValue:String):Boolean;
Begin
   Result:=AddHeader(ServerVariable, VariableValue);
End;

Function TDXHTTPHeaderTools.FindUnknownHeader(ServerVariable:String):String;
var
   line, buf:String;
   i : Integer;

Begin
   line:=fSessionHeader.Unknown;
   i := QuickPos(ServerVariable, line);
   if i <> 0 then
   Begin
      buf := Copy(line, i, Length(line));
      i := QuickPos(#13#10, buf);
      buf := Copy(buf, 0, i-1);
      i := CharPos(#32, buf);
      Result := Copy(buf, i, Length(buf));
   End; //IF
End;

Procedure TDXHTTPHeaderTools.ReplaceUnknownHeader(ServerVariable, VariableValue:String);
var line:PString;
   str, buf:String;
   i, k, m:Integer;
Begin
   if Not UnknownHeaderExists(ServerVariable) then Exit;
   line:=@fSessionHeader.Unknown;
   k := QuickPos(ServerVariable, line^);
   if k <> 0 then
   Begin
      str := Copy(line^, k, Length(line^));
      m := QuickPos(#13#10, str);
      buf := Copy(str, 0, k-1);
      i := CharPos(#32, buf);
      k := k + i;
      Delete(line^, k, m);
      Insert(VariableValue, line^, k);
   End;
End;

Function TDXHTTPHeaderTools.UnknownHeaderExists(ServerVariable:String):Boolean;
Begin
   if ServerVariable = '' then Result:=False
   Else if fSessionHeader.Unknown = '' then Result:=False
   Else if QuickPos(ServerVariable, fSessionHeader.Unknown) = 0 then Result:=False
   Else Result:=True;
End;

Function TDXHTTPHeaderTools.ToStrings:TStrings;
var res:TStrings;
    endch:String;
    split :String;
//    CleanLoop:Integer;

Begin
   split:=': ';
   endch :='';// 7-27 #13#10;
   res := TStringList.Create;
   With fSessionHeader^ do
   Begin
    if Allow <> '' then
       res.Add('ALLOW'+split+Allow+endch);
    if AuthType <> '' then
       res.Add('AUTHTYPE'+split+AuthType+endch);
    if AuthName <> '' then
       res.Add('AUTHNAME'+split+AuthName+endch);
    if AuthPass <> '' then
       res.Add('AUTHPASS'+split+AuthPass+endch);
    if Accept <> '' then
       res.Add('ACCEPT'+split+Accept+endch);
    if AcceptCharset <> '' then
       res.Add('ACCEPT-CHARSET'+split+AcceptCharset+endch);
    if AcceptEncoding <> '' then
       res.Add('ACCEPT-ENCODING'+split+AcceptEncoding+endch);
    if AcceptLanguage <> '' then
       res.Add('ACCEPT-LANGUAGE'+split+AcceptLanguage+endch);
    if ContentLength <> 0 then
       res.Add('CONTENT-LENGTH'+split+IntToStr(ContentLength)+endch);
    if CacheControl <> '' then // 7-27
       res.Add('CACHE-CONTROL'+split+CacheControl+endch);
    if CacheInfo <> '' then // 7-27
       res.Add('CACHE-INFO'+split+CacheInfo+endch);
    if Connection <> '' then
       res.Add('CONNECTION'+split+Connection+endch);
    if ContentType <> '' then
       res.Add('CONTENT-TYPE'+split+ContentType+endch);
    if ClientName <> '' then
       res.Add('CLIENTNAME'+split+ClientName+endch);
    if ClientAddr <> '' then
       res.Add('CLIENTADDR'+split+ClientAddr+endch);
    if Cookie <> '' then
       res.Add('COOKIE'+split+Cookie+endch);
    if Date <> '' then
       res.Add('DATE'+split+Date+endch);
    if From <> '' then
       res.Add('FROM'+split+From+endch);
    if Forwarded <> '' then
       res.Add('FORWARDED'+split+Forwarded+endch);
    if ForwardedFor <> '' then
       res.Add('FORWARDED-FOR'+split+ForwardedFor+endch);
    if Host <> '' then
       res.Add('HOST'+split+Host+endch);
    if IfModSince <> '' then
       res.Add('IFMODSINCE'+split+IfModSince+endch);
    if IfMatch <> '' then
       res.Add('IFMATCH'+split+IfMatch+endch);
    if IfNoneMatch <> '' then
       res.Add('IFNONEMATCH'+split+IfNoneMatch+endch);
    if IfRange <> '' then
       res.Add('IFRANGE'+split+IfRange+endch);
    if IfUnModSince <> '' then
       res.Add('IFUNMODSINCE'+split+IfUnModSince+endch);
    if KeepAlive <> '' then
       res.Add('KEEPALIVE'+split+KeepAlive+endch);
    if Method <> '' then
       res.Add('METHOD'+split+Method+endch);
    if MaxForwards <> '' then
       res.Add('MAXFORWARDS'+split+MaxForwards+endch);
    if Protocol <> '' then
       res.Add('PROTOCOL'+split+Protocol+endch);
    if ProxyConnection <> '' then
       res.Add('PROXY-CONNECTION'+split+ProxyConnection+endch);
    if Pragma <> '' then
       res.Add('PRAGMA'+split+Pragma+endch);
    if ProxyAuthorization <> '' then
       res.Add('PROXYAUTHORIZATION'+split+ProxyAuthorization+endch);
    if PublicCache <> '' then
       res.Add('PUBLIC'+split+PublicCache+endch);
    if QueryString <> '' then
       res.Add('QUERYSTRING'+split+QueryString+endch);
    if Range <> '' then
       res.Add('RANGE'+split+Range+endch);
    if Referer <> '' then
       res.Add('REFERER'+split+Referer+endch);
    if URI <> '' then
       res.Add('URI'+split+URI+endch);
    if Upgrade <> '' then
       res.Add('UPGRADE'+split+Upgrade+endch);
    if UserAgent <> '' then
       res.Add('USERAGENT'+split+UserAgent+endch);
    if TransferEncoding <> '' then
       res.Add('TRANSFERENCODING'+split+TransferEncoding+endch);
    if Via <> '' then
       res.Add('VIA'+split+Via+endch);
    if Weferer <> '' then
       res.Add('WEFERER'+split+Weferer+endch);
    if WserAgent <> '' then
       res.Add('WSER-AGENT'+split+WserAgent+endch);
    if Unknown <> '' then
       res.Add(Unknown+endch);
   End; //With
{   CleanLoop:=res.Count;
   While CleanLoop>0 do Begin
      if res[CleanLoop-1]='' then res.delete(CleanLoop-1);
      Dec(CleanLoop);
   End;}
   Result:=res;
End;

end.
