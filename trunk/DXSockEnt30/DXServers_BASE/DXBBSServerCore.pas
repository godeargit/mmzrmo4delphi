unit DXBBSServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXBBSServerCore
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
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  BBSTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  BBSTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  BBSTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;
  BBSTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; var Handled: Boolean) of object;

  // DXBBSServerCore is a proprietary protocol established by
  // Brain Patchwork DX, LLC. during a co-development project with
  // a customer. It can be massaged into a new protocol if you are
  // looking for a starting point to make your own
  // implementations.
  // 
  // 
  // 
  // Summary
  // A proprietary protocol by Brain Patchwork DX, LLC.
TDXBBSServerCore = class(TDXServerCore)
  private
    fOnCommandTimeout:BBSTSimpleEvent;
    fOnCommandAUTH: BBSTComplexEvent;
    fOnCommandMENU: BBSTBasicEvent;
    fOnCommandPING: BBSTSimpleEvent;
    fOnCommandPONG: BBSTSimpleEvent;
    fOnCommandOther: BBSTOtherEvent;
  protected
    Procedure SetOnCommandPING(value: BBSTSimpleEvent);
    Procedure SetOnCommandPONG(value: BBSTSimpleEvent);
    Procedure SetOnCommandMENU(value: BBSTBasicEvent);
    Procedure SetOnCommandAUTH(value: BBSTComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header,MOTD:TStream);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:TStream);
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:BBSTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:BBSTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:BBSTComplexEvent);
  published
    // When the BBS Server Core receives an AUTH command, the string
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
property OnCommandAUTH: BBSTComplexEvent read fOnCommandAUTH
                                             write SetOnCommandAUTH;
    // When the BBS Server Core receives an MENU command, the string
    // from the client is parsed and this event is fired.
    // 
    // 
    // 
    // The first parameter is always the clientthread.
    // 
    // 
    // 
    // In our implementation the second parameter was a numeric code
    // which referenced internal functions on the server. You could
    // actually implement this to work like the old BBS packages,
    // and this be the menu letter corresponding to the fuinction,
    // but then you would have to track session information per user
    // connected.
    // 
    // 
    //
    // Summary
    // MENU command received.                                       
property OnCommandMENU: BBSTBasicEvent read fOnCommandMENU
                                           write SetOnCommandMENU;
    // When the BBS Server Core receives an PING command, the string
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
property OnCommandPING: BBSTSimpleEvent read fOnCommandPING
                                            write SetOnCommandPING;
    // When the BBS Server Core receives an PONG command, the string
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
property OnCommandPONG: BBSTSimpleEvent read fOnCommandPONG
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
property OnCommandOther: BBSTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
    // When the BBS Server Core does not receives a command within
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
property OnCommandTimeout: BBSTSimpleEvent read fOnCommandTimeout
                                            write fOnCommandTimeout;
    // Used internally to show the date of this release in the
    // object inspector. Only useful when reporting bugs to
    // development team.
  end;

implementation

uses
   DXSock,
   DxString;

Type
  PBBSBasicEvent=^TBBSBasicEvent;
  TBBSBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:BBSTBasicEvent;
  End;
  PBBSSimpleEvent=^TBBSSimpleEvent;
  TBBSSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:BBSTSimpleEvent;
  End;
  PBBSComplexEvent=^TBBSComplexEvent;
  TBBSComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:BBSTComplexEvent;
  End;


constructor TDXBBSServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=7625;
   Timeout:=12000; // client should send a command, or noop every 10 seconds!
end;

destructor TDXBBSServerCore.Destroy;
Var
   PBasicEvent:PBBSBasicEvent;
   PSimpleEvent:PBBSSimpleEvent;
   PComplexEvent:PBBSComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PBBSBasicEvent(fEventArray[0]).Tag of
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
Procedure TDXBBSServerCore.AddBasicEvent(Command:String;EventProc:BBSTBasicEvent);
Var
   PBasicEvent:PBBSBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PBBSBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PBBSBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
Procedure TDXBBSServerCore.AddSimpleEvent(Command:String;EventProc:BBSTSimpleEvent);
Var
   PSimpleEvent:PBBSSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PBBSSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PBBSSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
Procedure TDXBBSServerCore.AddComplexEvent(Command:String;EventProc:BBSTComplexEvent);
Var
   PComplexEvent:PBBSComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PBBSComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PBBSComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXBBSServerCore.SetOnCommandAUTH(value:BBSTComplexEvent);
Begin
   fOnCommandAUTH:=Value;
   AddComplexEvent('AUTH',Value);
End;

Procedure TDXBBSServerCore.SetOnCommandMENU(value: BBSTBasicEvent);
Begin
   fOnCommandMENU:=Value;
   AddBasicEvent('MENU',Value);
End;

Procedure TDXBBSServerCore.SetOnCommandPING(value:BBSTSimpleEvent);
Begin
   fOnCommandPING:=Value;
   AddSimpleEvent('PING',Value);
End;

Procedure TDXBBSServerCore.SetOnCommandPONG(value:BBSTSimpleEvent);
Begin
   fOnCommandPONG:=Value;
   AddSimpleEvent('PONG',Value);
End;

// A built-in Routine to Call in your "onNewConnect", reduces
// problems if you do not know how the protocols work. Simply
// call this routine with a String containing the "Hello
// String", if blank this routine will just respond to the
// client "BBS Server (Ready)", and the client will begin login.
// 
// 
// 
// Summary
// Simple wrapper to say hello.
procedure TDXBBSServerCore.SayHello(ClientThread:TDXClientThread;Header,MOTD:TStream);
Begin
   If Assigned(Header) then Begin
{$IFDEF VER100}
      ClientThread.Socket.SendFromStream(Header);
{$ELSE}
      ClientThread.Socket.SendFrom(Header);
{$ENDIF}
   End
   Else Begin
      ClientThread.Socket.Writeln('BBS Server Ready.');
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
procedure TDXBBSServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:TStream);
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
procedure TDXBBSServerCore.ProcessSession(ClientThread:TDXClientThread);
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
         if s='' then continue;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(s,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PBBSBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PBBSBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PBBSBasicEvent(fEventArray[Loop]).EventProcedure) then
                       BBSTBasicEvent(PBBSBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PBBSSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       BBSTSimpleEvent(PBBSSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PBBSComplexEvent(fEventArray[Loop]).EventProcedure) then
                       BBSTComplexEvent(PBBSComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,#32,False)),S);
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

