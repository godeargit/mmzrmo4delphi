unit DXMXResolver;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXMXResolver
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
   Classes,
   DXDNSQuery;

{$I DXAddons.def}

type
   TDXMXResolver=class(TDXDNSQuery)
   private
      // Private declarations
   protected
      // Protected declarations
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function Resolve(ServerAddress:string; var
         ResultsList:TStringList):Boolean;
      function completeTheDomain(T:string; pOwner:Integer):string;
   published
      // Published declarations
   end;

implementation

uses
   SysUtils,
   DXSock,
   DXString;

///////////////////////////////////////////////////////////////////////////////
//CREATE:
//       Define the mode of optimization.
///////////////////////////////////////////////////////////////////////////////

constructor TDXMXResolver.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////

destructor TDXMXResolver.Destroy;
begin
   inherited Destroy;
end;

function TDXMXResolver.Resolve(ServerAddress:string; var
   ResultsList:TStringList):Boolean;
var
   TmpDNS:PDNSResultSet;
   MemoryStream:TMemoryStream;
   Question:string;
   S:string;
   QueryWord:Word;
   LengthWord:Word;
   GetLoop:Integer;
   Answer:string;
   pos:Integer;

begin
   Result:=False;
   if (not Assigned(ResultsList)) then Exit;
   ResultsList.Clear;
   if ServerAddress='' then Exit;
   MemoryStream:=TMemoryStream.Create;
   New(TmpDNS);
   with TmpDNS^ do begin
      Domain:=ServerAddress;
      Results:=MemoryStream;
      QueryType:=DX_Query_MX;
      QueryClass:=1;
      FindDNSEntries(TmpDNS);
      if (DNSServer<>'')and(Results.Size>12) then begin
         Result:=True;
         SetLength(S, Results.Size);
         Results.Seek(0, 0);
         Results.Read(S[1], Results.Size);
         Question:=completeTheDomain(S, 12);
         pos:=18+Length(Question);
         for GetLoop:=1 to AnswerCount do begin
            pos:=pos+11;
            Move(S[pos], LengthWord, 2);
            SwapMove(LengthWord, LengthWord);
            pos:=pos+2;
            Move(S[pos], QueryWord, 2);
            SwapMove(QueryWord, QueryWord);
            pos:=pos+1;
            Answer:=completeTheDomain(S, pos);
            pos:=pos+LengthWord-2;
            ResultsList.Add(copy('00000'+IntToStr(QueryWord),
               Length(IntToStr(QueryWord))+1, 5)+#32+Answer);
         end;
      end;
      Results.Free;
   end;
   Dispose(TmpDNS);
   ResultsList.Sort;
end;

function TDXMXResolver.completeTheDomain(T:string; pOwner:Integer):string;
var
   done:Boolean;
   pos:Integer;
   L:Integer;
   temp:string;
begin
   done:=False;
   temp:='';
   pos:=pOwner;
   while not done do begin
      inc(pos);
      if T[pos]=#0 then begin
         if temp[length(temp)]='.' then
            temp:=copy(temp, 1, length(temp)-1);
         result:=temp;
         done:=True;
      end;
      if Ord(T[pos])>=192 then begin
         pos:=((Ord(T[pos])-192)*256)+(Ord(T[pos+1]));
      end
      else begin
         L:=Ord(T[pos]);
         temp:=temp+copy(T, pos+1, L)+'.';
         pos:=pos+L;
      end;
   end;
end;

{ - This is deleted - I need to return it, once I parse it correctly!
                                    1  1  1  1  1  1
      0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      ID                       |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    QDCOUNT                    | // Questions
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ANCOUNT                    | // Answers
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    NSCOUNT                    | // Name Servers
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ARCOUNT                    | // Result Records
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

Result Records:
                                    1  1  1  1  1  1
      0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                     Owner                     |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      TYPE                     |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                     CLASS                     |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      TTL                      |
    |                                               |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                   RDLENGTH                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
    /                     RDATA                     /
    /                base on RDLength               /
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

}

end.

