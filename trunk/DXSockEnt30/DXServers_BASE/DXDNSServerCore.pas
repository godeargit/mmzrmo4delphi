unit DXDNSServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDNSServerCore
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
//  Description: implements DNS (Domain Name Server) protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXString,
  DXServerCore;

{$I DXSock.def}

Type // structure changed in 3.0 code!
   PDXDNSQueryPacket=^TDXDNSQueryPacket;
   TDXDNSQueryPacket=Record
      ID:Word;     //2
      QR:Boolean;
      OpCode:Byte;
      AA:Boolean;
      TC:Boolean;
      RD:Boolean;
      RA:Boolean;
      Z:Byte;
      RCode:Byte;      //2
      QDCount:Word;//2
      ANCount:Word;//2
      NSCount:Word;//2
      ARCount:Word;//2
      Query:String;
      QueryType:Word;
      QueryClass:Word;
   End;

  DNSTSimpleEvent = procedure(ClientThread: TDXClientThread;QueryPacket:PDXDNSQueryPacket) of object;
  DNSTOtherEvent = procedure(ClientThread: TDXClientThread;QueryPacket:PDXDNSQueryPacket;Var Handled:Boolean) of object;

  TDXDNSServerCore = class(TDXServerCore)
  private
    fOnCommandA: DNSTSimpleEvent;        // 1
    fOnCommandNS: DNSTSimpleEvent;       // 2
    fOnCommandMD: DNSTSimpleEvent;       // 3
    fOnCommandMF: DNSTSimpleEvent;       // 4
    fOnCommandCNAME: DNSTSimpleEvent;    // 5
    fOnCommandSOA: DNSTSimpleEvent;      // 6
    fOnCommandMB: DNSTSimpleEvent;       // 7
    fOnCommandMG: DNSTSimpleEvent;       // 8
    fOnCommandMR: DNSTSimpleEvent;       // 9
    fOnCommandNULL: DNSTSimpleEvent;     //10
    fOnCommandWKS: DNSTSimpleEvent;      //11
    fOnCommandPTR: DNSTSimpleEvent;      //12
    fOnCommandHINFO: DNSTSimpleEvent;    //13
    fOnCommandMINFO: DNSTSimpleEvent;    //14
    fOnCommandMX: DNSTSimpleEvent;       //15
    fOnCommandTXT: DNSTSimpleEvent;      //16
    fOnCommandRP: DNSTSimpleEvent;       //17
    fOnCommandAFSDB: DNSTSimpleEvent;    //18
    fOnCommandX25: DNSTSimpleEvent;      //19
    fOnCommandISDN: DNSTSimpleEvent;     //20
    fOnCommandRT: DNSTSimpleEvent;       //21
    fOnCommandOSINSAP: DNSTSimpleEvent;  //22
    fOnCommandAXFR: DNSTSimpleEvent;     //252
    fOnCommandMAILB: DNSTSimpleEvent;    //253
    fOnCommandMAILA: DNSTSimpleEvent;    //254
    fOnCommandALL: DNSTSimpleEvent;      //255
    fOnCommandOther: DNSTOtherEvent;
  protected
    Procedure SetOnCommandA(Value:DNSTSimpleEvent);
    Procedure SetOnCommandNS(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMD(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMF(Value:DNSTSimpleEvent);
    Procedure SetOnCommandCNAME(Value:DNSTSimpleEvent);
    Procedure SetOnCommandSOA(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMB(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMG(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMR(Value:DNSTSimpleEvent);
    Procedure SetOnCommandNULL(Value:DNSTSimpleEvent);
    Procedure SetOnCommandWKS(Value:DNSTSimpleEvent);
    Procedure SetOnCommandPTR(Value:DNSTSimpleEvent);
    Procedure SetOnCommandHINFO(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMINFO(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMX(Value:DNSTSimpleEvent);
    Procedure SetOnCommandTXT(Value:DNSTSimpleEvent);
    Procedure SetOnCommandRP(Value:DNSTSimpleEvent);
    Procedure SetOnCommandAFSDB(Value:DNSTSimpleEvent);
    Procedure SetOnCommandX25(Value:DNSTSimpleEvent);
    Procedure SetOnCommandISDN(Value:DNSTSimpleEvent);
    Procedure SetOnCommandRT(Value:DNSTSimpleEvent);
    Procedure SetOnCommandOSINSAP(Value:DNSTSimpleEvent);
    Procedure SetOnCommandAXFR(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMAILB(Value:DNSTSimpleEvent);
    Procedure SetOnCommandMAILA(Value:DNSTSimpleEvent);
    Procedure SetOnCommandALL(Value:DNSTSimpleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddSimpleEvent(Command:SmallInt;EventProc:DNSTSimpleEvent);
  published
    property OnCommandA: DNSTSimpleEvent read fOnCommandA
                                         write SetOnCommandA;
    property OnCommandNS: DNSTSimpleEvent read fOnCommandNS
                                          write SetOnCommandNS;
    property OnCommandMD: DNSTSimpleEvent read fOnCommandMD
                                          write SetOnCommandMD;
    property OnCommandMF: DNSTSimpleEvent read fOnCommandMF
                                          write SetOnCommandMF;
    property OnCommandCNAME: DNSTSimpleEvent read fOnCommandCNAME
                                             write SetOnCommandCNAME;
    property OnCommandSOA: DNSTSimpleEvent read fOnCommandSOA
                                           write SetOnCommandSOA;
    property OnCommandMB: DNSTSimpleEvent read fOnCommandMB
                                          write SetOnCommandMB;
    property OnCommandMG: DNSTSimpleEvent read fOnCommandMG
                                          write SetOnCommandMG;
    property OnCommandMR: DNSTSimpleEvent read fOnCommandMR
                                          write SetOnCommandMR;
    property OnCommandNULL: DNSTSimpleEvent read fOnCommandNULL
                                            write SetOnCommandNULL;
    property OnCommandWKS: DNSTSimpleEvent read fOnCommandWKS
                                           write SetOnCommandWKS;
    property OnCommandPTR: DNSTSimpleEvent read fOnCommandPTR
                                           write SetOnCommandPTR;
    property OnCommandHINFO: DNSTSimpleEvent read fOnCommandHINFO
                                             write SetOnCommandHINFO;
    property OnCommandMINFO: DNSTSimpleEvent read fOnCommandMINFO
                                             write SetOnCommandMINFO;
    property OnCommandMX: DNSTSimpleEvent read fOnCommandMX
                                          write SetOnCommandMX;
    property OnCommandTXT: DNSTSimpleEvent read fOnCommandTXT
                                           write SetOnCommandTXT;
    property OnCommandRP: DNSTSimpleEvent read fOnCommandRP
                                          write SetOnCommandRP;
    property OnCommandAFSDB: DNSTSimpleEvent read fOnCommandAFSDB
                                             write SetOnCommandAFSDB;
    property OnCommandX25: DNSTSimpleEvent read fOnCommandX25
                                           write SetOnCommandX25;
    property OnCommandISDN: DNSTSimpleEvent read fOnCommandISDN
                                            write SetOnCommandISDN;
    property OnCommandRT: DNSTSimpleEvent read fOnCommandRT
                                          write SetOnCommandRT;
    property OnCommandOSINSAP: DNSTSimpleEvent read fOnCommandOSINSAP
                                               write SetOnCommandOSINSAP;
    property OnCommandAXFR: DNSTSimpleEvent read fOnCommandAXFR
                                            write SetOnCommandAXFR;
    property OnCommandMAILB: DNSTSimpleEvent read fOnCommandMAILB
                                             write SetOnCommandMAILB;
    property OnCommandMAILA: DNSTSimpleEvent read fOnCommandMAILA
                                             write SetOnCommandMAILA;
    property OnCommandALL: DNSTSimpleEvent read fOnCommandALL
                                           write SetOnCommandALL;
    property OnCommandOther: DNSTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
  end;

implementation

Uses
   DXSock;
   
Type
  PDNSSimpleEvent=^TDNSSimpleEvent;
  TDNSSimpleEvent=record
     Tag:Integer;
     Command:SmallInt;
     EventProcedure:DNSTSimpleEvent;
  End;


(******************************************************************************
CREATE:
       Define the Default Port number to Listen On.
******************************************************************************)
constructor TDXDNSServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=53;
   ProtocolToBind:=wpUDPOnly;
end;

(******************************************************************************
DESTROY:
        Destory this object.
******************************************************************************)
destructor TDXDNSServerCore.Destroy;
Var
   PSimpleEvent:PDNSSimpleEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PDNSSimpleEvent(fEventArray[0]).Tag of
            1:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXDNSServerCore.AddSimpleEvent(Command:SmallInt;EventProc:DNSTSimpleEvent);
Var
   PSimpleEvent:PDNSSimpleEvent;
   Loop:Integer;

Begin
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDNSSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PDNSSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=1;      // Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXDNSServerCore.SetOnCommandA(value:DNSTSimpleEvent);
Begin
   fOnCommandA:=Value;
   AddSimpleEvent(1,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandNS(Value:DNSTSimpleEvent);
Begin
   fOnCommandNS:=Value;
   AddSimpleEvent(2,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMD(Value:DNSTSimpleEvent);
Begin
   fOnCommandMD:=Value;
   AddSimpleEvent(3,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMF(Value:DNSTSimpleEvent);
Begin
   fOnCommandMF:=Value;
   AddSimpleEvent(4,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandCNAME(Value:DNSTSimpleEvent);
Begin
   fOnCommandCNAME:=Value;
   AddSimpleEvent(5,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandSOA(Value:DNSTSimpleEvent);
Begin
   fOnCommandSOA:=Value;
   AddSimpleEvent(6,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMB(Value:DNSTSimpleEvent);
Begin
   fOnCommandMB:=Value;
   AddSimpleEvent(7,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMG(Value:DNSTSimpleEvent);
Begin
   fOnCommandMG:=Value;
   AddSimpleEvent(8,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMR(Value:DNSTSimpleEvent);
Begin
   fOnCommandMR:=Value;
   AddSimpleEvent(9,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandNULL(Value:DNSTSimpleEvent);
Begin
   fOnCommandNULL:=Value;
   AddSimpleEvent(10,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandWKS(Value:DNSTSimpleEvent);
Begin
   fOnCommandWKS:=Value;
   AddSimpleEvent(11,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandPTR(Value:DNSTSimpleEvent);
Begin
   fOnCommandPTR:=Value;
   AddSimpleEvent(12,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandHINFO(Value:DNSTSimpleEvent);
Begin
   fOnCommandHINFO:=Value;
   AddSimpleEvent(13,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMINFO(Value:DNSTSimpleEvent);
Begin
   fOnCommandMINFO:=Value;
   AddSimpleEvent(14,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMX(Value:DNSTSimpleEvent);
Begin
   fOnCommandMX:=Value;
   AddSimpleEvent(15,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandTXT(Value:DNSTSimpleEvent);
Begin
   fOnCommandTXT:=Value;
   AddSimpleEvent(16,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandRP(Value:DNSTSimpleEvent);
Begin
   fOnCommandRP:=Value;
   AddSimpleEvent(17,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandAFSDB(Value:DNSTSimpleEvent);
Begin
   fOnCommandAFSDB:=Value;
   AddSimpleEvent(18,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandX25(Value:DNSTSimpleEvent);
Begin
   fOnCommandX25:=Value;
   AddSimpleEvent(19,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandISDN(Value:DNSTSimpleEvent);
Begin
   fOnCommandISDN:=Value;
   AddSimpleEvent(20,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandRT(Value:DNSTSimpleEvent);
Begin
   fOnCommandRT:=Value;
   AddSimpleEvent(21,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandOSINSAP(Value:DNSTSimpleEvent);
Begin
   fOnCommandOSINSAP:=Value;
   AddSimpleEvent(22,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandAXFR(Value:DNSTSimpleEvent);
Begin
   fOnCommandAXFR:=Value;
   AddSimpleEvent(252,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMAILB(Value:DNSTSimpleEvent);
Begin
   fOnCommandMAILB:=Value;
   AddSimpleEvent(253,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandMAILA(Value:DNSTSimpleEvent);
Begin
   fOnCommandMAILA:=Value;
   AddSimpleEvent(254,Value);
End;

Procedure TDXDNSServerCore.SetOnCommandALL(Value:DNSTSimpleEvent);
Begin
   fOnCommandALL:=Value;
   AddSimpleEvent(255,Value);
End;


(******************************************************************************
PROCESSSESSION:
               If you want this CORE to process the parsing, you should call
               this from your "OnNewConnect" implementation. This should be
               right after your call to SayHello (optional).
******************************************************************************)
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
procedure TDXDNSServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  S:String;
  Loop:Integer;
  WasHandled:Boolean;
  Packet:PDXDNSQueryPacket;
  QueryWord:Word;
  OutData:Pointer;

Procedure DecodeFlags(W:Word);
Begin
   With Packet^ do Begin
      QR:=(W And $8000)=$8000;                  // 1bit
      OpCode:=((W And $7800) Shr 11) And $000F; // 4bit
      AA:=(W And $0400)=$0400;                  // 1bit
      TC:=(W And $0200)=$0200;                  // 1bit
      RD:=(W And $0100)=$0100;                  // 1bit
      RA:=(W And $0080)=$0080;                  // 1bit
      // z (reserved)                           // 3bit
      RCode:=(W And $000F);                     // 4bit
   End;
End;

Procedure CustomSwap(S:String;Position:Integer;Var Dest:Word);
Var
   OWord:Word;

Begin
   Move(S[Position],OWord,2);
   SwapMove(OWord,Dest);
End;

begin
   with ClientThread.Socket do begin
      S:=ReadStr(CharactersToRead);
      If S<>'' then Begin
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
      End
      Else Exit;
      ProcessWindowsMessageQueue;
      New(Packet);
//      FillChar(Packet^,Sizeof(TDXDNSQueryPacket),#0);
      // ID=16bits
      CustomSwap(S,1,Packet^.ID);
      // QR,OPCODE,AA,TC,RD,RA,Z,RCODE=16bits
      CustomSwap(S,3,QueryWord);
      DecodeFlags(QueryWord);
      // QDCOUNT
      CustomSwap(S,5,Packet^.QDCount);
      // ANCOUNT
      CustomSwap(S,7,Packet^.ANCount);
      // NSCOUNT
      CustomSwap(S,9,Packet^.NSCount);
      // ARCOUNT
      CustomSwap(S,11,Packet^.ARCount);
      Delete(S,1,12); // only thing left is the query.
      Packet^.Query:=DecodeDomain(Copy(S,1,Length(S)-4));
      Delete(S,1,Length(S)-4);
      CustomSwap(S,1,Packet^.QueryType);
      CustomSwap(S,3,Packet^.QueryClass);
      Loop:=0;
      WasHandled:=False;
      While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
         If PDNSSimpleEvent(fEventArray[Loop]).Command=Packet^.QueryType then Begin
            Case PDNSSimpleEvent(fEventArray[Loop]).Tag of
               1:With Packet^ Do
                 if Assigned(PDNSSimpleEvent(fEventArray[Loop]).EventProcedure) then
                    DNSTSimpleEvent(PDNSSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Packet);
            End;
            WasHandled:=True;
         End
         Else Inc(Loop);
      End;
      Dispose(Packet);
      If Not WasHandled then Begin
         if assigned(OnCommandOther) then
            OnCommandOther(ClientThread,Packet,WasHandled);
      end;
   end;
end;

end.

