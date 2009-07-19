unit DXDNSQuery;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXDNSQuery
//    Author: G.E. Ozz Nixon Jr. (usasupport@brainpatchworkdx.com)
//            Federico "BackDream" Simonetti
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999 Brain Patchwork DX.
//   Version: 2.0 (2nd Generation Code)
// ==========================================================================
// Since this code has been designed to function on a server, and we want your
// products to excel because of cache techniques. All requestes are archived in
// a TList, redundant calls to a previously queried DNS server respond almost
// instantly. In your server you should call "FlushCache" once every couple of
// hours to make sure you are not reading old results. We allow you to control
// how often your ending application calls FlushCache.
//
// This component normally reads DNS entries from the registry, but if you want
// your code can use "AlternativeDNS" to override the DNS entries with your own
// list. For multiple DNS servers seperate with a space or comma, and you IP not
// domain! (grin).
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

const
   DX_Query_A=1; // Host Address
   DX_Query_NS=2; // Authoritative name server
   DX_Query_MD=3; // Mail Destination - USE MX!
   DX_Query_MF=4; // Mail Forwarder - USE MX!
   DX_Query_CNAME=5; // Cononical name for an alias
   DX_Query_SOA=6; // Start of Zone Authority
   DX_Query_MB=7; // Mailbox Domain Name
   DX_Query_MG=8; // Mailbox Member
   DX_Query_MR=9; // Mail rename domain
   DX_Query_NULL=10; // Null Resource Record
   DX_Query_WKS=11; // Well-known service
   DX_Query_PTR=12; // Domain name pointer
   DX_Query_HINFO=13; // Host Information (experimental)
   DX_Query_MINFO=14; // Mailbox/Mail-List Information
   DX_Query_MX=15; // Mail Exchange
   DX_Query_TXT=16; // Text Strings
   DX_Query_RP=17; // Responsible person (experimental)
   DX_Query_AFSDB=18; // Authority Format identifier-type server (experimental)
   DX_Query_X25=19; // X.25 Address, X.121 (experimental)
   DX_Query_ISDN=20; // ISDN Address, E.163/E.164 (experimental)
   DX_Query_RT=21; // Route through (experimental)
   DX_Query_OSINSAP=22;
      // OSI Network service access point address (experimental)
   DX_Query_NSAPPTR=23;
   DX_Query_SIG=24; //RFC-2065
   DX_Query_KEY=25; //RFC-2065
   DX_Query_PX=26;
   DX_Query_GPOS=27;
   DX_Query_AAAA=28; //IP6 Address                     [Susan Thomson]
   DX_Query_LOC=29; //RFC-1876
   DX_Query_NXT=30; //RFC-2065
   DX_Query_SRV=33; //RFC-2052
   DX_Query_NAPTR=35; //RFC-2168
   DX_Query_KX=36;
   DX_Query_AXFR=252; // Request an entire ZONE
   DX_Query_MAILB=253; // Request all mail relatest records (MB,MG,MR)
   DX_Query_MAILA=254; // Request all mail agents Resource Records - Use MX!
   DX_Query_ALL=255; // Request all records

type
   PDNSResultSet=^TDNSResultSet;
   TDNSResultSet=record
      Domain:string;
      DNSServer:string;
      QueryType:SmallInt;
      QueryClass:SmallInt;
      {$IFDEF VER90}
      Results:TMemoryStream;
      {$ELSE}
      Results:TStream;
      {$ENDIF}
   end;

   TDXDNSQuery=class(TDXComponent)
   private
      // Private declarations
      FDNSServers:TStrings;
      FQueryCache:TList;
      FUseUDP:Boolean;
      FAlternativeDNS:string;
      fQDCount:Integer;
      fANCount:Integer;
      fNSCount:Integer;
      fRRCount:Integer;
   protected
      // Protected declarations
      procedure SetFAlternativeDNS(value:string);
      procedure SetDNSServers(value:TStrings);
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function Resolve(Domain:string; QueryType:Integer):string;
      procedure FlushCache;
      procedure FindDNSEntries(var DNSResultSet:PDNSResultSet);
      procedure LoadNameServers;
   published
      // Published declarations
      property UseUDP:Boolean read FUseUDP
         write FUseUDP;
      property AlternativeDNS:string read FAlternativeDNS
         write SetFAlternativeDNS;
      property QuestionCount:Integer read fQDCount;
      property AnswerCount:Integer read fANCount;
      property NameServerCount:Integer read fNSCount;
      property ResourceRecordsCount:Integer read fRRCount;
      property FoundDNSServers:TStrings read FDNSServers
         write SetDNSServers;
   end;

implementation

uses
   Windows,
   DXSocket,
   DXSock,
   {$IFDEF IP_HELPER}
   Jedi_IPHLPAPI,
   IPTypes,
   {$ENDIF}
   SysUtils;

///////////////////////////////////////////////////////////////////////////////
// DNS Message Header
//    +-----------------------------------------------+
//    |                    ID                         |
//    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
//    |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
//    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
//    |                  QDCOUNT                      |
//    +-----------------------------------------------+
//    |                  ANCOUNT                      |
//    +-----------------------------------------------+
//    |                  NSCOUNT                      |
//    +-----------------------------------------------+
//    |                  ARCOUNT                      |
//    +-----------------------------------------------+
//    ID=16bits
//    QR=1bit
//    OPCODE=4bits(0=query, 1=inverse query, 2=server status, 3-15 reserved.}
//    AA=1bit     (Authoritative flag}
//    TC=1bit     (Truncation flag}
//    RD=1bit     (Recursion desired)
//    RA=1bit     (Recursion available)
//    Z=3bits     (reserved)
//    RCODE=4bites(0=no error, 1=format error, 2=server problem,
//                 3=server cant find it, 4=query not supported,
//                 6=not authorized to do this,6-15 reserved.)
//    QDCount=16bits (number of entries in question section)
//    ANCount=16bits (number of Resource Results in answer section)
//    NSCOUNT=16bits (number of records in the authority section)
//    ARCount=16bits (number of extra records to read)
///////////////////////////////////////////////////////////////////////////////

type
   TDNSMessageHeader=record
      ID:SmallInt;
      QR:Boolean;
      OpCode:Byte;
      AA:Boolean;
      TC:Boolean;
      RD:Boolean;
      RA:Boolean;
      Z:Byte;
      RCode:Byte;
      QDCount:SmallInt;
      ANCount:SmallInt;
      NSCount:SmallInt;
      ARCount:SmallInt;
   end;

   ///////////////////////////////////////////////////////////////////////////////
   // CREATE:
   //       Define the mode of optimization.
   ///////////////////////////////////////////////////////////////////////////////

constructor TDXDNSQuery.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   FQueryCache:=TList.Create;
   FDNSServers:=TStringList.Create;
   FUseUDP:=TRUE;
   FAlternativeDNS:='';
   fQDCount:=0;
   fANCount:=0;
   fNSCount:=0;
   fRRCount:=0;
end;

///////////////////////////////////////////////////////////////////////////////
// DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////

destructor TDXDNSQuery.Destroy;
begin
   if Assigned(FQueryCache) then begin
      FlushCache;
      FQueryCache.Free;
      FQueryCache:=nil;
   end;
   if Assigned(FDNSServers) then begin
      FDNSServers.Free;
      FDNSServers:=nil;
   end;
   inherited Destroy;
end;

procedure TDXDNSQuery.FlushCache;
var
   StoredDNSResultSet:PDNSResultSet;

begin
   if Assigned(FQueryCache) then begin
      while FQueryCache.Count>0 do begin
         StoredDNSResultSet:=FQueryCache[0];
         TMemoryStream(StoredDNSResultSet.Results).Free;
         StoredDNSResultSet.Results:=nil;
         Dispose(StoredDNSResultSet);
         FQueryCache.Delete(0);
      end;
   end;
end;

procedure TDXDNSQuery.SetFAlternativeDNS(value:string);
begin
   if Value<>FAlternativeDNS then begin
      FAlternativeDNS:=Value;
      {$IFNDEF OBJECTS_ONLY}
      if (csDesigning in ComponentState) then Exit;
      {$ENDIF}
      LoadNameServers;
   end;
end;

procedure TDXDNSQuery.LoadNameServers;
var
   RegDNS:string;
   I:Integer;
   {$IFDEF IP_HELPER}
   Err, FixedInfoSize:DWORD;
   pFixedInfo:PFIXED_INFO;
   pAddrStr:PIP_ADDR_STRING;
   {$ENDIF}

begin
   {$IFNDEF OBJECTS_ONLY}
   if (csDesigning in ComponentState) then Exit;
   {$ENDIF}
   if FAlternativeDNS='' then begin
      {$IFDEF VER100}
      ShowMessageWindow('Fatal DNS Error',
         'Older Delphi Requires "AlternativeDNS" to be set manually!');
      Exit;
      {$ELSE}
      REGDNS:=RegistryStringGet(HKEY_LOCAL_MACHINE,
         'SYSTEM\CurrentControlSet\Services\TCPIP\Parameters\NameServer');
      if RegDNS='' then
         REGDNS:=RegistryStringGet(HKEY_LOCAL_MACHINE,
            'SYSTEM\CurrentControlSet\Services\TCPIP\Parameters\DhcpNameServer');
      if RegDNS='' then
         REGDNS:=RegistryStringGet(HKEY_LOCAL_MACHINE,
            'SYSTEM\CurrentControlSet\Services\VxD\MSTCP\NameServer');
      {$ENDIF}
   end
   else
      RegDNS:=FAlternativeDNS;
   {$IFDEF IP_HELPER}
   if REGDNS='' then begin
      FixedInfoSize:=0;
      try
         Err:=GetNetworkParams(nil, FixedInfoSize);
         if (Err=ERROR_BUFFER_OVERFLOW) then begin
            GetMem(pFixedInfo, FixedInfoSize);
            try
               Err:=GetNetworkParams(pFixedInfo, FixedInfoSize);
               if (Err=0) then begin
                  REGDNS:=pFixedInfo.DnsServerList.IpAddress.S;
                  pAddrStr:=pFixedInfo.DnsServerList.Next;
                  while (pAddrStr<>nil) do begin
                     REGDNS:=REGDNS+' '+pAddrStr.IpAddress.S;
                     pAddrStr:=pAddrStr.Next;
                  end;
               end;
            finally
               FreeMem(pFixedInfo);
            end;
         end;
      except
         REGDNS:='';
      end;
   end;
   {$ENDIF}
   if REGDNS='' then Exit;
   while Length(REGDNS)>0 do begin
      I:=CharPos(#32, REGDNS);
      if I=0 then I:=Pos(',', REGDNS);
      if I>0 then begin
         FDNSServers.Add(Copy(REGDNS, 1, I-1));
         Delete(REGDNS, 1, I);
      end
      else begin
         FDNSServers.Add(REGDNS);
         REGDNS:='';
      end;
   end;
end;

///////////////////////////////////////////////////////////////////////////////
// RESOLVE:
///////////////////////////////////////////////////////////////////////////////

function TDXDNSQuery.Resolve(Domain:string; QueryType:Integer):string;
var
   TmpDNS:PDNSResultSet;
   MemoryStream:TMemoryStream;

begin
   Result:='';
   if Domain='' then Exit;
   MemoryStream:=TMemoryStream.Create;
   New(TmpDNS);
   TmpDNS^.Domain:=Domain;
   TmpDNS^.QueryType:=QueryType;
   with TmpDNS^ do begin
      Results:=MemoryStream;
      QueryClass:=255;
      FindDNSEntries(TmpDNS);
      if DNSServer<>'' then begin
         SetLength(Result, Results.Size);
         Results.Seek(0, 0);
         Results.Read(Result[1], Results.Size);
      end;
      Results.Free;
      Results:=nil;
   end;
   Dispose(TmpDNS);
end;

procedure TDXDNSQuery.FindDNSEntries(var DNSResultSet:PDNSResultSet);
const
   HeaderSize=16;

var
   QueryQuestion:string;
   QueryOffset:Integer;
   QueryBuf:string;
   QueryWord:Word;
   QueryString:string;
   Loop:Integer;
   NewConnect:PNewConnect;
   Found:Boolean;
   Socket:TDXSock;
   FDNSHeader:TDNSMessageHeader;
   ErrCode:Integer;

   function SetFlags:Word;
   begin
      Result:=0;
      with FDNSHeader do begin
         if QR then
            Result:=Result or $8000
         else
            Result:=Result and $EFFF;
         Result:=((OpCode shl 11)and $7800)or(Result and $87FF);
         if AA then
            Result:=Result or $0400
         else
            Result:=Result and $FBFF;
         if TC then
            Result:=Result or $0200
         else
            Result:=Result and $FDFF;
         if RD then
            Result:=Result or $0100
         else
            Result:=Result and $FEFF;
         if RA then
            Result:=Result or $0080
         else
            Result:=Result and $FF7F;
         Result:=(RCode and $000F)or(Result and $FFF0);
      end;
   end;

   procedure DecodeFlags(const W:Word);
   begin
      FillChar(FDNSHeader, Sizeof(FDNSHeader), #0);
      with FDNSHeader do begin
         QR:=(W and $8000)=$8000;
         OpCode:=((W and $7800)shr 11)and $000F;
         AA:=(W and $0400)=$0400;
         TC:=(W and $0200)=$0200;
         RD:=(W and $0100)=$0100;
         RA:=(W and $0080)=$0080;
         RCode:=(W and $000F);
      end;
   end;

   {$HINTS OFF}

   procedure GetCounts(const QS:string);
   var
      QWord:Word;

   begin
      Move(QS[1], QWord, 2);
      SwapMove(QWord, fQDCount);
      Move(QS[3], QWord, 2);
      SwapMove(QWord, fANCount);
      Move(QS[5], QWord, 2);
      SwapMove(QWord, fNSCount);
      Move(QS[7], QWord, 2);
      SwapMove(QWord, fRRCount);
   end;

begin
   if FDNSServers.Count<1 then
      ShowMessageWindow('Fatal DNS Error',
         'Could not find DNS Server entries in registry!');
   if not Assigned(DNSResultSet) then Exit;
   if (DNSResultSet^.Domain='')or
      (not Assigned(DNSResultSet^.Results))or
      (FDNSServers.Count<1) then Exit;
   with DNSResultSet^ do begin
      Domain:=lowercase(Domain);
      DNSServer:='';
      {$IFDEF VER90}
      Results.WriteBuffer('', 0);
      {$ELSE}
      Results.Size:=0;
      {$ENDIF}
   end;
   Found:=False;
   Loop:=0;
   Socket:=TDXSock.Create(Nil);
   while (not Found)and(Loop<FDNSServers.Count) do begin
      {$IFDEF VER90}
      DNSResultSet^.Results.WriteBuffer('', 0);
      {$ELSE}
      DNSResultSet^.Results.Size:=0;
      {$ENDIF}
      with FDNSHeader do begin
         ID:=Random(Trunc(Now))+1;
         QR:=False;
         OpCode:=0;
         AA:=False;
         TC:=False;
         RD:=True;
         RA:=False;
         Z:=0;
         RCode:=0;
         QDCount:=1;
         ANCount:=0;
         NSCount:=0;
         ARCount:=0;
      end;
      case DNSResultSet^.QueryType of
         DX_QUERY_PTR:QueryQuestion:=EncodeAddress(DNSResultSet^.Domain);
      else
         QueryQuestion:=EncodeDomain(DNSResultSet^.Domain);
      end;
      QueryOffset:=Length(QueryQuestion);
      Setlength(QueryBuf, HeaderSize+QueryOffset);
      FillChar(QueryBuf[1], HeaderSize+QueryOffset, #0);
      with FDNSHeader do begin
         SwapMove(ID, QueryBuf[1]);
         QueryWord:=SetFlags;
         SwapMove(QueryWord, QueryBuf[3]);
         SwapMove(QDCount, QueryBuf[5]);
      end;
      Move(QueryQuestion[1], QueryBuf[13], QueryOffset);
      SwapMove(DNSResultSet^.QueryType, QueryBuf[13+QueryOffset]);
      SwapMove(DNSResultSet^.QueryClass, QueryBuf[15+QueryOffset]);
      New(NewConnect);
      with NewConnect^ do begin
         Port:=53;
         UseNAGLE:=False;
         UseUDP:=FUseUDP;
         UseBlocking:=True;
         ipAddress:=FDNSServers[Loop];
      end;
      if Socket.Connect(NewConnect) then begin
         Socket.Write(QueryBuf);
         SetReceiveTimeout(Socket.Sock, 3000, ErrCode);
         QueryQuestion:=Socket.ReadStr(2048);
         Socket.CloseNow;
         {$IFDEF VER90}
         DNSResultSet^.Results.SetSize(0);
         {$ELSE}
         DNSResultSet^.Results.Size:=0;
         {$ENDIF}
         if QueryQuestion<>'' then
            DNSResultSet^.Results.WriteBuffer(QueryQuestion[1],
               Length(QueryQuestion));
         if Copy(QueryBuf, 1, 2)=Copy(QueryQuestion, 1, 2) then begin
            Move(QueryQuestion[3], QueryWord, 2);
            SwapMove(QueryWord, QueryWord);
            DecodeFlags(QueryWord);
            if FDNSHeader.RCode=0 then begin
               if DNSResultSet^.Results.Size>3 then
                  DNSResultSet^.DNSServer:=NewConnect.ipAddress;
            end;
            DNSResultSet^.Results.Seek(0, 0);
         end;
      end;
      Found:=DNSResultSet^.DNSServer<>'';
      if Assigned(NewConnect) then Dispose(NewConnect);
      Inc(Loop);
   end;
   Socket.Free;
   Socket:=nil;
   if Found then begin
      DNSResultSet^.Results.Seek(4, 0);
      Setlength(QueryString, 8);
      DNSResultSet^.Results.Read(QueryString[1], 8);
      GetCounts(QueryString);
      DNSResultSet^.Results.Seek(0, 0);
   end
   else begin
      fQDCount:=0;
      fANCount:=0;
      fNSCount:=0;
      fRRCount:=0;
   end;
end;
{$HINTS ON}

procedure TDXDNSQuery.SetDNSServers(value:TStrings);
begin
   fDNSServers.Assign(Value);
end;

end.

