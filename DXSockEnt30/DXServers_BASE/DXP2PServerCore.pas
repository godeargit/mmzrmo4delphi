unit DXP2PServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXP2PServerCore
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
//  Description: *Example*
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Dialogs,
  SysUtils,
  DXSock,
  Classes,
  DXServerCore;

{$I dxsock.def}

type
  P2PTSimpleEvent = procedure(ClientThread: TDXServerThread) of object;
  P2PTBasicEvent = procedure(ClientThread: TDXServerThread; Parm: string) of object;
  P2PTComplexEvent = procedure(ClientThread:TDXServerThread;Parm1,Parm2:string) of object;
  P2PTOtherEvent = procedure(ClientThread: TDXServerThread; Command: string; Parm: string; var Handled: Boolean) of object;

  // DXP2PServerCore is a proprietary protocol established by
  // Brain Patchwork DX, LLC.
  //
  // 
  // 
  // Summary
  // A proprietary protocol by Brain Patchwork DX, LLC.           
TDXP2PServerCore = class(TDXServerCore)
  private
    fOnCommandTimeout:P2PTSimpleEvent;
    fOnCommandAUTH: P2PTComplexEvent;
    fOnCommandQUERY: P2PTBasicEvent;
    fOnCommandPING: P2PTSimpleEvent;
    fOnCommandPONG: P2PTSimpleEvent;
    fOnCommandOther: P2PTOtherEvent;
    fEventArray:TList;
    fiTimeout:Cardinal;
  protected
    Procedure SetOnCommandPING(value: P2PTSimpleEvent);
    Procedure SetOnCommandPONG(value: P2PTSimpleEvent);
    Procedure SetOnCommandQUERY(value: P2PTBasicEvent);
    Procedure SetOnCommandAUTH(value: P2PTComplexEvent);
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXServerThread;Header,MOTD:TStream);
    procedure SayGoodbye(ClientThread:TDXServerThread;Footer:TStream);
    procedure ProcessSession(ClientThread:TDXServerThread);
    Procedure AddBasicEvent(Command:String;EventProc:P2PTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:P2PTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:P2PTComplexEvent);
  published
    // All of our protocols the expect commands to be received from
    // the client, have a timeout value. This timeout is used
    // internally by the socket Readln command. In this protocol, we
    // implemented support for an OnCommandTimeout event. FOr this
    // protocol we expect to have client systems too busy or with
    // bad connections, so we support multiple timeouts before
    // calling it quits.
    // 
    // 
    // 
    // \See Also
    // 
    // <LINK TDXP2PServerCore.OnCommandTimeout, OnCommandTimeout> 
    //
    //
    // 
    // Summary
    // Maximum idle time before considering the client has timed
    // out.                                                         
property Timeout:Cardinal read fiTimeout
                           write fiTimeout;
    // When the P2P Server Core receives an AUTH command, the string
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
    // AUTH command received.                                       
property OnCommandAUTH: P2PTComplexEvent read fOnCommandAUTH
                                             write SetOnCommandAUTH;
    // When the P2P Server Core receives an QUERY command, the string
    // from the client is parsed and this event is fired.
    // 
    // 
    //
    // The first parameter is always the clientthread.
    // 
    // 
    // 
    // In our implementation the second parameter was the query.
    //
    // 
    // 
    // Summary
    // QUERY command received.
property OnCommandQUERY: P2PTBasicEvent read fOnCommandQUERY
                                           write SetOnCommandQUERY;
    // When the P2P Server Core receives an PING command, the string
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
property OnCommandPING: P2PTSimpleEvent read fOnCommandPING
                                            write SetOnCommandPING;
    // When the P2P Server Core receives an PONG command, the string
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
property OnCommandPONG: P2PTSimpleEvent read fOnCommandPONG
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
property OnCommandOther: P2PTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
    // When the P2P Server Core does not receives a command within
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
property OnCommandTimeout: P2PTSimpleEvent read fOnCommandTimeout
                                            write fOnCommandTimeout;
  end;

implementation

uses
   DxString;

Type
  PP2PBasicEvent=^TP2PBasicEvent;
  TP2PBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:P2PTBasicEvent;
  End;
  PP2PSimpleEvent=^TP2PSimpleEvent;
  TP2PSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:P2PTSimpleEvent;
  End;
  PP2PComplexEvent=^TP2PComplexEvent;
  TP2PComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:P2PTComplexEvent;
  End;

{$IFDEF OBJECTS_ONLY}
constructor TDXP2PServerCore.Create;
{$ELSE}
constructor TDXP2PServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=1881;
   fiTimeout:=12000; // client should send a command, or noop every 10 seconds!
   fEventArray:=TList.Create;
end;


destructor TDXP2PServerCore.Destroy;
Var
   PBasicEvent:PP2PBasicEvent;
   PSimpleEvent:PP2PSimpleEvent;
   PComplexEvent:PP2PComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PP2PBasicEvent(fEventArray[0]).Tag of
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
         End;
         fEventArray.Delete(0);
      End;
      fEventArray.Free;
      fEventArray:=Nil;
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
Procedure TDXP2PServerCore.AddBasicEvent(Command:String;EventProc:P2PTBasicEvent);
Var
   PBasicEvent:PP2PBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PP2PBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PP2PBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
Procedure TDXP2PServerCore.AddSimpleEvent(Command:String;EventProc:P2PTSimpleEvent);
Var
   PSimpleEvent:PP2PSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PP2PSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PP2PSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
Procedure TDXP2PServerCore.AddComplexEvent(Command:String;EventProc:P2PTComplexEvent);
Var
   PComplexEvent:PP2PComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PP2PComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PP2PComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXP2PServerCore.SetOnCommandAUTH(value:P2PTComplexEvent);
Begin
   fOnCommandAUTH:=Value;
   AddComplexEvent('AUTH',Value);
End;

Procedure TDXP2PServerCore.SetOnCommandQUERY(value: P2PTBasicEvent);
Begin
   fOnCommandQUERY:=Value;
   AddBasicEvent('QUERY',Value);
End;

Procedure TDXP2PServerCore.SetOnCommandPING(value:P2PTSimpleEvent);
Begin
   fOnCommandPING:=Value;
   AddSimpleEvent('PING',Value);
End;

Procedure TDXP2PServerCore.SetOnCommandPONG(value:P2PTSimpleEvent);
Begin
   fOnCommandPONG:=Value;
   AddSimpleEvent('PONG',Value);
End;

// A built-in Routine to Call in your "onNewConnect", reduces
// problems if you do not know how the protocols work. Simply
// call this routine with a String containing the "Hello
// String", if blank this routine will just respond to the
// client "P2P Server (Ready)", and the client will begin login.
// 
// 
// 
// Summary
// Simple wrapper to say hello.                                 
procedure TDXP2PServerCore.SayHello(ClientThread:TDXServerThread;Header,MOTD:TStream);
Begin
   If Assigned(Header) then Begin
{$IFDEF VER100}
      ClientThread.Socket.SendFromStream(Header);
{$ELSE}
      ClientThread.Socket.SendFrom(Header);
{$ENDIF}
   End
   Else Begin
      ClientThread.Socket.Writeln('P2P Server Ready.');
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
procedure TDXP2PServerCore.SayGoodbye(ClientThread:TDXServerThread;Footer:TStream);
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
procedure TDXP2PServerCore.ProcessSession(ClientThread:TDXServerThread);
var
   s, sCmd: string;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;
   Okay:Boolean;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while Connected do begin
         Okay:=False;
         While not Okay do Begin
            if fbForceAbort then exit;
            s:=ReadLn(fiTimeout);
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
         if S='' then continue;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled);
            End;
         End;
         sCmd:=UpperCase(Fetch(s,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PP2PBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PP2PBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PP2PBasicEvent(fEventArray[Loop]).EventProcedure) then
                       P2PTBasicEvent(PP2PBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PP2PSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       P2PTSimpleEvent(PP2PSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PP2PComplexEvent(fEventArray[Loop]).EventProcedure) then
                       P2PTComplexEvent(PP2PComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,#32,False)),S);
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
            Writeln('-CMD(' + CMD + ')');
      end;
   end;
end;

end.

