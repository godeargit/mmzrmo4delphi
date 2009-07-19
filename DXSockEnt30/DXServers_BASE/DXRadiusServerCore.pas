unit DXRadiusServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXRadiusServerCore
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
//  Description: *NOT FINISHED V3.0 YET*
// ========================================================================
// RFC 2139
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I dxsock.def}

type
  RadiusTSimpleEvent = procedure(ClientThread: TDXServerThread) of object;
  RadiusTBasicEvent = procedure(ClientThread: TDXServerThread; Parm: string) of object;
  RadiusTComplexEvent = procedure(ClientThread:TDXServerThread;Parm1,Parm2:string) of object;
  RadiusTOtherEvent = procedure(ClientThread: TDXServerThread; Command: string; Parm: string; var Handled: Boolean) of object;

  TDXRadiusServerCore = class(TDXServerCore)
  private
    fOnCommandNORTH: RadiusTSimpleEvent;
    fOnCommandSOUTH: RadiusTSimpleEvent;
    fOnCommandEAST: RadiusTSimpleEvent;
    fOnCommandWEST: RadiusTSimpleEvent;
    fOnCommandQUIT: RadiusTSimpleEvent;
    fOnCommandINV: RadiusTSimpleEvent;    // INV = INVentory
    fOnCommandSAY: RadiusTBasicEvent;
    fOnCommandLOOK: RadiusTBasicEvent;
    fOnCommandOther: RadiusTOtherEvent;
    fEventArray:TList;
    fiTimeout:Cardinal;
    fbForceAbort:Boolean;
  protected
    Procedure SetOnCommandNORTH(value: RadiusTSimpleEvent);
    Procedure SetOnCommandSOUTH(value: RadiusTSimpleEvent);
    Procedure SetOnCommandEAST(value: RadiusTSimpleEvent);
    Procedure SetOnCommandWEST(value: RadiusTSimpleEvent);
    Procedure SetOnCommandQUIT(value: RadiusTSimpleEvent);
    Procedure SetOnCommandINV(value: RadiusTSimpleEvent);    // INV = INVentory
    Procedure SetOnCommandSAY(value: RadiusTBasicEvent);
    Procedure SetOnCommandLOOK(value: RadiusTBasicEvent);
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXServerThread;Header,MOTD:TStrings);
    procedure SayGoodbye(ClientThread:TDXServerThread;Footer:String);
    procedure ProcessSession(ClientThread:TDXServerThread);
    Procedure AddBasicEvent(Command:String;EventProc:RadiusTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:RadiusTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:RadiusTComplexEvent);
    Procedure ForceAbort;
  published
    property Timeout:Cardinal read fiTimeout
                           write fiTimeout;
    property OnCommandNORTH: RadiusTSimpleEvent read fOnCommandNORTH
                                            write SetOnCommandNORTH;
    property OnCommandSOUTH: RadiusTSimpleEvent read fOnCommandSOUTH
                                            write SetOnCommandSOUTH;
    property OnCommandEAST: RadiusTSimpleEvent read fOnCommandEAST
                                            write SetOnCommandEAST;
    property OnCommandWEST: RadiusTSimpleEvent read fOnCommandWEST
                                            write SetOnCommandWEST;
    property OnCommandQUIT: RadiusTSimpleEvent read fOnCommandQUIT
                                            write SetOnCommandQUIT;
    property OnCommandINV: RadiusTSimpleEvent read fOnCommandINV
                                            write SetOnCommandINV;
    property OnCommandSAY: RadiusTBasicEvent read fOnCommandSAY
                                            write SetOnCommandSAY;
    property OnCommandLOOK: RadiusTBasicEvent read fOnCommandLOOK
                                            write SetOnCommandLOOK;
    property OnCommandOther: RadiusTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
  end;

implementation

uses
   DXSock,
   DXString;

Type
  PRadiusBasicEvent=^TRadiusBasicEvent;
  TRadiusBasicEvent=record
     Tag:Integer;
     Command:MySmallString;
     EventProcedure:RadiusTBasicEvent;
  End;
  PRadiusSimpleEvent=^TRadiusSimpleEvent;
  TRadiusSimpleEvent=record
     Tag:Integer;
     Command:MySmallString;
     EventProcedure:RadiusTSimpleEvent;
  End;
  PRadiusComplexEvent=^TRadiusComplexEvent;
  TRadiusComplexEvent=record
     Tag:Integer;
     Command:MySmallString;
     EventProcedure:RadiusTComplexEvent;
  End;

///////////////////////////////////////////////////////////////////////////////
//CREATE:
//       Define the Default Port number to Listen On.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OBJECTS_ONLY}
constructor TDXRadiusServerCore.Create;
{$ELSE}
constructor TDXRadiusServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=1813;
   ProtocolToBind:=wpUDPOnly;
   fiTimeout:=120000;
   fEventArray:=TList.Create;
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////
destructor TDXRadiusServerCore.Destroy;
Var
   PBasicEvent:PRadiusBasicEvent;
   PSimpleEvent:PRadiusSimpleEvent;
   PComplexEvent:PRadiusComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PRadiusBasicEvent(fEventArray[0]).Tag of
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

///////////////////////////////////////////////////////////////////////////////
//ADDBASICEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddBasicEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXRadiusServerCore.AddBasicEvent(Command:String;EventProc:RadiusTBasicEvent);
Var
   PBasicEvent:PRadiusBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PRadiusBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PRadiusBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

///////////////////////////////////////////////////////////////////////////////
//ADDSIMPLEEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddBasicEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXRadiusServerCore.AddSimpleEvent(Command:String;EventProc:RadiusTSimpleEvent);
Var
   PSimpleEvent:PRadiusSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PRadiusSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PRadiusSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

///////////////////////////////////////////////////////////////////////////////
//ADDCOMPLEXEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddBasicEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXRadiusServerCore.AddComplexEvent(Command:String;EventProc:RadiusTComplexEvent);
Var
   PComplexEvent:PRadiusComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PRadiusComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PRadiusComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXRadiusServerCore.SetOnCommandNORTH(value:RadiusTSimpleEvent);
Begin
   fOnCommandNORTH:=Value;
   AddSimpleEvent('NORTH',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandSOUTH(value:RadiusTSimpleEvent);
Begin
   fOnCommandSOUTH:=Value;
   AddSimpleEvent('SOUTH',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandEAST(value:RadiusTSimpleEvent);
Begin
   fOnCommandEAST:=Value;
   AddSimpleEvent('EAST',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandWEST(value:RadiusTSimpleEvent);
Begin
   fOnCommandWEST:=Value;
   AddSimpleEvent('WEST',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandQUIT(value:RadiusTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandINV(value:RadiusTSimpleEvent);
Begin
   fOnCommandINV:=Value;
   AddSimpleEvent('INV',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandSAY(value:RadiusTBasicEvent);
Begin
   fOnCommandSAY:=Value;
   AddBasicEvent('SAY',Value);
End;

Procedure TDXRadiusServerCore.SetOnCommandLOOK(value:RadiusTBasicEvent);
Begin
   fOnCommandLOOK:=Value;
   AddBasicEvent('LOOK',Value);
End;

///////////////////////////////////////////////////////////////////////////////
//SAYHELLO:
//         A built-in Routine to Call in your "onNewConnect", reduces problems
//         if you do not know how the protocols work. Simply call this routine
//         with a String containing the "Hello String", if blank this routine
//         will just respond to the client "Radius Server (Ready)", and the client
//         will begin login.
///////////////////////////////////////////////////////////////////////////////
procedure TDXRadiusServerCore.SayHello(ClientThread:TDXServerThread;Header,MOTD:TStrings);
Var
   Loop:Integer;
   Ws:String;

Begin
   If Assigned(Header) then Begin
      For Loop:=1 to Header.Count do Begin
         Ws:=Header[Loop-1];
         ClientThread.Socket.Writeln(Ws);
      End;
   End
   Else Begin
      ClientThread.Socket.Writeln('Radius Server Ready.');
   End;
   If Assigned(MOTD) then Begin
      For Loop:=1 to MOTD.Count do Begin
      Ws:=MOTD[Loop-1];
         ClientThread.Socket.Writeln(Ws);
      End;
   End;
   ClientThread.Socket.Writeln('Proceed with login...');
End;

///////////////////////////////////////////////////////////////////////////////
//SAYGOODBYE:
//           A built-in Routine to Call at the end of your "onNewConnect", this
//           will send a String "Footer", or the defacto "Goodbye." to the
//           client program. This should be the last piece of code in your
//           onNewConnect. Remember the onDisconnect is not a good place to
//           send any output - as the client could have disconnected already!
///////////////////////////////////////////////////////////////////////////////
procedure TDXRadiusServerCore.SayGoodbye(ClientThread:TDXServerThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln(Footer)
   Else
      ClientThread.Socket.Writeln('Goodbye, please come back soon!');
End;

///////////////////////////////////////////////////////////////////////////////
//PROCESSSESSION:
//               If you want this CORE to process the parsing, you should call
//               this from your "OnNewConnect" implementation. This should be
//               right after your call to SayHello (optional).
///////////////////////////////////////////////////////////////////////////////
procedure TDXRadiusServerCore.ProcessSession(ClientThread:TDXServerThread);
var
   s, sCmd: string;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;

  procedure NotHandled(CMD: string);
  begin
    ClientThread.Socket.Writeln('500 command not recognized (' + CMD + ')');
  end;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         if fbForceAbort then exit;
         s:=ReadLn(fiTimeout);
         If LastReadTimeout then Exit;
         If Not ValidSocket then Exit;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            Move(TDXBSArray(OutData^),S[1],Loop);
            OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled); // clear pointers
         End;
         sCmd:=UpperCase(Fetch(s,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PRadiusBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PRadiusBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PRadiusBasicEvent(fEventArray[Loop]).EventProcedure) then
                       RadiusTBasicEvent(PRadiusBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PRadiusSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       RadiusTSimpleEvent(PRadiusSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PRadiusComplexEvent(fEventArray[Loop]).EventProcedure) then
                       RadiusTComplexEvent(PRadiusComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,#32,False)),S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then NotHandled(sCmd);
         if sCMD='QUIT' then Exit;
      end;
   end;
end;

Procedure TDXRadiusServerCore.ForceAbort;
Begin
   fbForceAbort:=True;
End;

///////////////////////////////////////////////////////////////////////////////
// First Byte = 4 Accounting-Request, or 5 Accounting-Reply
// Second Byte = Identifier, to match multiple requests from same machine
// Third/Fourth Byte = Length (include all 4 bytes)+ Authenticator + Attrib fields
//                     (Min is 20, Max is 4096) Ignore anything smaller than 20!
// 16 Bytes = Authenticator (requester mode this is an MD5 Checksum!)
//
// Attributes: Type Byte + Length Byte + Value(s)
//  Type Byte: 1-39 Refer RADIUS documentation
//             40   Acct-Status-Type
//             41   Acct-Delay-Time
//             42   Acct-Input-Octets
//             43   Acct-Output-Octets
//             44   Acct-Session-ID
//             45   Acct-Authentic
//             46   Acct-Session-Time
//             47   Acct-Input-Packets
//             48   Acct-Output-Packets
//             49   Acct-Terminate-Cause
//             50   Acct-Multi-Session-ID
//             51   Acct-Link-Count
//             60+  Refer to RADIUS documentation
// Value(s)
//  String = 0-253 characters
//  address = 32bit address, MSB
//  integer = 32bit, MSB
//  time = 32bit, MSB (not normally used, but supported)
///////////////////////////////////////////////////////////////////////////////

end.

