unit DXNetLinkServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXNetLinkServerCore
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
//  Description: implements AT&T's Fiber network monitoring protocl NETLINK
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DXSock,
  Classes,
  DXServerCore;

{$I DXSock.def}

Const
   NetlinkStrDelim=#255;

type
  NetLinkTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  NetLinkTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  NetLinkTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;
  NetLinkTRealComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2,Parm3,Parm4:string) of object;
  NetLinkTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; var Handled: Boolean) of object;

  // DXNetLinkServerCore is a proprietary protocol established by
  // Brain Patchwork DX, LLC. during a co-development project with
  // a customer. It can be massaged into a new protocol if you are
  // looking for a starting point to make your own
  // implementations.
  //
  //
  //
  // Summary
  // A proprietary protocol by Brain Patchwork DX, LLC.
TDXNetLinkServerCore = class(TDXServerCore)
  private
    fOnCommandTimeout:NetLinkTSimpleEvent;
    fOnCommandALERT: NetLinkTRealComplexEvent;
    fOnCommandLOGIN: NetLinkTComplexEvent;
    fOnCommandPING: NetLinkTSimpleEvent;
    fOnCommandPONG: NetLinkTSimpleEvent;
    fOnCommandSENDIM: NetLinkTComplexEvent;
    fOnCommandMYSTATUS: NetLinkTBasicEvent;
    fOnCommandADDCONTACT: NetLinkTBasicEvent;
    fOnCommandDELCONTACT: NetLinkTBasicEvent;
    fOnCommandADDSERVICE: NetLinkTBasicEvent;
    fOnCommandDELSERVICE: NetLinkTBasicEvent;
    fOnCommandSERVERSTATUS: NetLinkTSimpleEvent;
    fOnCommandWATCHLIST: NetLinkTSimpleEvent;
    fOnCommandALLSTATUS: NetLinkTSimpleEvent;
    fOnCommandSERVICES: NetLinkTSimpleEvent;
    fOnCommandOther: NetLinkTOtherEvent;
  protected
    Procedure SetOnCommandSENDIM(value: NetLinkTComplexEvent);
    Procedure SetOnCommandMYSTATUS(value: NetLinkTBasicEvent);
    Procedure SetOnCommandADDCONTACT(value: NetLinkTBasicEvent);
    Procedure SetOnCommandDELCONTACT(value: NetLinkTBasicEvent);
    Procedure SetOnCommandADDSERVICE(value: NetLinkTBasicEvent);
    Procedure SetOnCommandDELSERVICE(value: NetLinkTBasicEvent);
    Procedure SetOnCommandSERVERSTATUS(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandWATCHLIST(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandALLSTATUS(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandSERVICES(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandPING(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandPONG(value: NetLinkTSimpleEvent);
    Procedure SetOnCommandLOGIN(value: NetLinkTComplexEvent);
    Procedure SetOnCommandALERT(value: NetLinkTRealComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header,MOTD:TStream);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:TStream);
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:NetLinkTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:NetLinkTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:NetLinkTComplexEvent);
    Procedure AddRealComplexEvent(Command:String;EventProc:NetLinkTRealComplexEvent);
  published
    // When the NetLink Server Core receives an LOGIN command, the string
    // from the client is parsed and this event is fired.
    //
    //
    // 
    // The first parameter is always the clientthread.
    // 
    // 
    // 
    // The next parameter is usally a login name, it is up to your
    // implementation if this is case sensative of not.
    // 
    // 
    // 
    // The next parameter in our implementation is a MD5 hash code
    // for the password. Allowing us to transmit it an readable text
    // and know that it will be rare for someone to hack the hash
    // code.
    // 
    //
    // 
    // Summary
    // LOGIN command received.                                       
property OnCommandLOGIN: NetLinkTComplexEvent read fOnCommandLOGIN
                                             write SetOnCommandLOGIN;
property OnCommandALERT: NetLinkTRealComplexEvent read fOnCommandALERT
                                                  write SetOnCommandALERT;
Property OnCommandSENDIM: NetLinkTComplexEvent read fOnCommandSENDIM
                                               write SetOnCommandSENDIM;
property OnCommandMYSTATUS: NetLinkTBasicEvent read fOnCommandMYSTATUS
                                               write SetOnCommandMYSTATUS;
property OnCommandADDCONTACT: NetLinkTBasicEvent read fOnCommandADDCONTACT
                                                 write SetOnCommandADDCONTACT;
property OnCommandDELCONTACT: NetLinkTBasicEvent read fOnCommandDELCONTACT
                                                 write SetOnCommandDELCONTACT;
property OnCommandADDSERVICE: NetLinkTBasicEvent read fOnCommandADDSERVICE
                                                 write SetOnCommandADDSERVICE;
property OnCommandDELSERVICE: NetLinkTBasicEvent read fOnCommandDELSERVICE
                                                 write SetOnCommandDELSERVICE;
property OnCommandSERVERSTATUS: NetLinkTSimpleEvent read fOnCommandSERVERSTATUS
                                                    write SetOnCommandSERVERSTATUS;
property OnCommandWATCHLIST: NetLinkTSimpleEvent read fOnCommandWATCHLIST
                                                 write SetOnCommandWATCHLIST;
property OnCommandALLSTATUS: NetLinkTSimpleEvent read fOnCommandALLSTATUS
                                                 write SetOnCommandALLSTATUS;
property OnCommandSERVICES: NetLinkTSimpleEvent read fOnCommandSERVICES
                                                write SetOnCommandSERVICES;
    // When the NetLink Server Core receives an PING command, the string
    // from the client is parsed and this event is fired.
    //
    //
    //
    // The first parameter is always the clientthread.
    //
    //
    //
    // In our implementation the client sends a PING statment to the
    // server, and the server tries to respond as fast as possible
    // with a PONG reply. Like IRC protocol.
    //
    //
    //
    // Summary
    // PING command received.
property OnCommandPING: NetLinkTSimpleEvent read fOnCommandPING
                                            write SetOnCommandPING;
    // When the NetLink Server Core receives an PONG command, the string
    // from the client is parsed and this event is fired.
    // 
    // 
    //
    // The first parameter is always the clientthread.
    // 
    // 
    // 
    // In our implementation the server periodically sends a PING
    // message to the client, which tries to respond to the server
    // as fast as possible with a PONG reply.
    //
    // 
    // 
    // Summary
    // PONG command received.                                       
property OnCommandPONG: NetLinkTSimpleEvent read fOnCommandPONG
                                            write SetOnCommandPONG;
    // Almost every protocol in our suite has this "Unknown Command
    // was received" event handler. If you do not implement this
    // event handler, the internal "default" error message will be
    // sent to the client sending the unknown command.
    // 
    // 
    // 
    // Summary
    // Unknown command was received.                               
property OnCommandOther: NetLinkTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
    // When the NetLink Server Core does not receives a command within
    // the Timeout property.
    // 
    // 
    // 
    // The first parameter is always the clientthread.
    // 
    //
    // 
    // Unlike most of our server implementations, we needed to be
    // able to set the TIMEOUT low, and when a command was not
    // received from the client within this TIMEOUT. This event
    // would fire and we would write to the client a PING command.
    // Internally (via the fpSessionData pointer) we would track how
    // many PING commands were sent, and after an excess amount we
    // would terminate the session. And in our OnCommandPONG event
    // we would set this counter back to zero.
    // 
    //
    // 
    // Summary
    // No command received within timeout limit.                    
property OnCommandTimeout: NetLinkTSimpleEvent read fOnCommandTimeout
                                            write fOnCommandTimeout;
  end;

implementation

uses
   DxString;

Type
  PNetLinkBasicEvent=^TNetLinkBasicEvent;
  TNetLinkBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NetLinkTBasicEvent;
  End;
  PNetLinkSimpleEvent=^TNetLinkSimpleEvent;
  TNetLinkSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NetLinkTSimpleEvent;
  End;
  PNetLinkComplexEvent=^TNetLinkComplexEvent;
  TNetLinkComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NetLinkTComplexEvent;
  End;
  PNetLinkRealComplexEvent=^TNetLinkRealComplexEvent;
  TNetLinkRealComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NetLinkTRealComplexEvent;
  End;

constructor TDXNetLinkServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=8101;
   Timeout:=6000; // client should send a command, or noop every 5 seconds!
end;

destructor TDXNetLinkServerCore.Destroy;
Var
   PBasicEvent:PNetLinkBasicEvent;
   PSimpleEvent:PNetLinkSimpleEvent;
   PComplexEvent:PNetLinkComplexEvent;
   PRealComplexEvent:PNetLinkRealComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PNetLinkBasicEvent(fEventArray[0]).Tag of
            1:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            2:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
            3:Begin
              PComplexEvent:=fEventArray[0];
              Dispose(PComplexEvent);
            End;
            4:Begin
              PRealComplexEvent:=fEventArray[0];
              Dispose(PRealComplexEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

// Allows you to dynamically assign a new command to the
// \internal parser. This allows the servercore to support the
// 'pre-defined' OnCommand* events, plus you can add other
// commands dynamically at run-time in your application without
// requiring a source code modification to our components!
// 
//
// 
// To make support easier for us, we ask that you use the
// Add*Event procedures to expand our code, reducing code
// changes when an upgrade is released!<B>
// 
// 
// 
// \Example Usage</B> AddBasicEvent('CDROM',MySpecialEvent);
// 
// 
// 
// Summary
// Implement a new command to the internal dynamic parser.     
Procedure TDXNetLinkServerCore.AddBasicEvent(Command:String;EventProc:NetLinkTBasicEvent);
Var
   PBasicEvent:PNetLinkBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNetLinkBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PNetLinkBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PBasicEvent);
   PBasicEvent.Tag:=1;      // Denotes Event in fEventArray is a TBasicEvent!
   PBasicEvent.Command:=Command;
   PBasicEvent.EventProcedure:=EventProc;
   fEventArray.Add(PBasicEvent);
End;

// Allows you to dynamically assign a new command to the
// \internal parser. This allows the servercore to support the
// 'pre-defined' OnCommand* events, plus you can add other
// commands dynamically at run-time in your application without
// requiring a source code modification to our components!
// 
// 
// 
// To make support easier for us, we ask that you use the
// Add*Event procedures to expand our code, reducing code
// changes when an upgrade is released!<B>
// 
// 
// 
// \Example Usage</B> AddSimpleEvent('DATE',MySpecialEvent3);
// 
// 
// 
// Summary
// Implement a new command to the internal dynamic parser.     
Procedure TDXNetLinkServerCore.AddSimpleEvent(Command:String;EventProc:NetLinkTSimpleEvent);
Var
   PSimpleEvent:PNetLinkSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNetLinkSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PNetLinkSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=2;      // Denotes Event in fEventArray is a TBasicEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

// Allows you to dynamically assign a new command to the
// \internal parser. This allows the servercore to support the
// 'pre-defined' OnCommand* events, plus you can add other
// commands dynamically at run-time in your application without
// requiring a source code modification to our components!
// 
// 
// 
// To make support easier for us, we ask that you use the
// Add*Event procedures to expand our code, reducing code
// changes when an upgrade is released!<B>
// 
// 
// 
// \Example Usage</B>
// AddComplexEvent('CDROMEX',MySpecialEvent2);
//
//
//
// Summary
// Implement a new command to the internal dynamic parser.
Procedure TDXNetLinkServerCore.AddComplexEvent(Command:String;EventProc:NetLinkTComplexEvent);
Var
   PComplexEvent:PNetLinkComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNetLinkComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PNetLinkComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PComplexEvent);
   PComplexEvent.Tag:=3;      // Denotes Event in fEventArray is a TBasicEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PComplexEvent);
End;

Procedure TDXNetLinkServerCore.AddRealComplexEvent(Command:String;EventProc:NetLinkTRealComplexEvent);
Var
   PRealComplexEvent:PNetLinkRealComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNetLinkRealComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PNetLinkRealComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PRealComplexEvent);
   PRealComplexEvent.Tag:=4;      // Denotes Event in fEventArray is a TBasicEvent!
   PRealComplexEvent.Command:=Command;
   PRealComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PRealComplexEvent);
End;

Procedure TDXNetLinkServerCore.SetOnCommandSENDIM(value: NetLinkTComplexEvent);
Begin
   fOnCommandSENDIM:=Value;
   AddComplexEvent('SENDIM',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandMYSTATUS(value: NetLinkTBasicEvent);
Begin
   fOnCommandMYSTATUS:=Value;
   AddBasicEvent('MYSTATUS',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandADDCONTACT(value: NetLinkTBasicEvent);
Begin
   fOnCommandADDCONTACT:=Value;
   AddBasicEvent('ADDCONTACT',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandDELCONTACT(value: NetLinkTBasicEvent);
Begin
   fOnCommandDELCONTACT:=Value;
   AddBasicEvent('DELCONTACT',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandADDSERVICE(value: NetLinkTBasicEvent);
Begin
   fOnCommandADDSERVICE:=Value;
   AddBasicEvent('ADDSERVICE',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandDELSERVICE(value: NetLinkTBasicEvent);
Begin
   fOnCommandDELSERVICE:=Value;
   AddBasicEvent('DELSERVICE',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandSERVERSTATUS(value: NetLinkTSimpleEvent);
Begin
   fOnCommandSERVERSTATUS:=Value;
   AddSimpleEvent('SERVERSTATUS',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandWATCHLIST(value: NetLinkTSimpleEvent);
Begin
   fOnCommandWATCHLIST:=Value;
   AddSimpleEvent('WATCHLIST',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandALLSTATUS(value: NetLinkTSimpleEvent);
Begin
   fOnCommandALLSTATUS:=Value;
   AddSimpleEvent('ALLSTATUS',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandSERVICES(value: NetLinkTSimpleEvent);
Begin
   fOnCommandSERVICES:=Value;
   AddSimpleEvent('SERVICES',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandLOGIN(value:NetLinkTComplexEvent);
Begin
   fOnCommandLOGIN:=Value;
   AddComplexEvent('LOGIN',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandALERT(value:NetLinkTRealComplexEvent);
Begin
   fOnCommandALERT:=Value;
   AddRealComplexEvent('ALERT',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandPING(value:NetLinkTSimpleEvent);
Begin
   fOnCommandPING:=Value;
   AddSimpleEvent('PING',Value);
End;

Procedure TDXNetLinkServerCore.SetOnCommandPONG(value:NetLinkTSimpleEvent);
Begin
   fOnCommandPONG:=Value;
   AddSimpleEvent('PONG',Value);
End;

// A built-in Routine to Call in your "onNewConnect", reduces
// problems if you do not know how the protocols work. Simply
// call this routine with a String containing the "Hello
// String", if blank this routine will just respond to the
// client "NetLink Server (Ready)", and the client will begin login.
// 
// 
// 
// Summary
// Simple wrapper to say hello.                                 
procedure TDXNetLinkServerCore.SayHello(ClientThread:TDXClientThread;Header,MOTD:TStream);
Begin
   If Assigned(Header) then Begin
{$IFDEF VER100}
      ClientThread.Socket.SendFromStream(Header);
{$ELSE}
      ClientThread.Socket.SendFrom(Header);
{$ENDIF}
   End
   Else Begin
      ClientThread.Socket.Writeln('NetLink Server Ready.');
   End;
   If Assigned(MOTD) then Begin
{$IFDEF VER100}
      ClientThread.Socket.SendFromStream(MOTD);
{$ELSE}
      ClientThread.Socket.SendFrom(MOTD);
{$ENDIF}
   End;
   ClientThread.Socket.Writeln('Proceed with login');
   ClientThread.Socket.Writeln('.');
End;

// A built-in Routine to Call at the end of your "OnNewConnect",
// this will send a String "Footer", or the defacto "Goodbye."
// to the client program. This should be the last piece of code
// in your onNewConnect. Remember the onDisconnect is not a good
// place to send any output - as the client could have
// disconnected already!
// 
// 
// 
// Summary
// Simple wrapper to say goodbye.                               
procedure TDXNetLinkServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:TStream);
Begin
   If Assigned(Footer) then
{$IFDEF VER100}
      ClientThread.Socket.SendFromStream(Footer)
{$ELSE}
      ClientThread.Socket.SendFrom(Footer)
{$ENDIF}
   Else
      ClientThread.Socket.Writeln('Goodbye, please come back soon!');
   ClientThread.Socket.Writeln('.');
End;

// If you want this CORE to process the parsing, you should call
// this from your "OnNewConnect" implementation. This should be
// right after your call to SayHello (optional).
// 
// 
// 
// Summary
// Call the "Defacto" dynamic command parser for this protocol. 
procedure TDXNetLinkServerCore.ProcessSession(ClientThread:TDXClientThread);
var
   s, sCmd: string;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;
   Okay:Boolean;
   ServiceID,
   Msg,
   WebSite:String;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while connected do begin
         Okay:=False;
         While not Okay do Begin
            if fbForceAbort then exit;
            s:=ReadLn(Timeout);
            If LastReadTimeout then Begin
               If Assigned(fOnCommandTimeout) then
                  fOnCommandTimeout(ClientThread)
               Else
                  Exit;
            End
            Else
               If Not Connected then Exit
               Else Okay:=True;
         End;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(s,NetlinkStrDelim,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PNetLinkBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PNetLinkBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PNetLinkBasicEvent(fEventArray[Loop]).EventProcedure) then
                       NetLinkTBasicEvent(PNetLinkBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PNetLinkSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       NetLinkTSimpleEvent(PNetLinkSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PNetLinkComplexEvent(fEventArray[Loop]).EventProcedure) then
                       NetLinkTComplexEvent(PNetLinkComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,NetlinkStrDelim,False)),S);
                  4:if Assigned(PNetLinkRealComplexEvent(fEventArray[Loop]).EventProcedure) then Begin
                       ServiceID:=Fetch(S,NetlinkStrDelim,False);
                       Msg:=Fetch(S,NetlinkStrDelim,False);
                       WebSite:=Fetch(S,NetlinkStrDelim,False);
                       NetLinkTRealComplexEvent(PNetLinkRealComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,ServiceID,Msg,WebSite,S);
                  End;
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         if sCMD='QUIT' then Exit;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then
            Writeln('-CMD(' + sCMD + ')');
      end;
   end;
end;


end.

