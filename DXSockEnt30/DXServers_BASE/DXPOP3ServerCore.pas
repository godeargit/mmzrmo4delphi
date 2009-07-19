unit DXPOP3ServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXPOP3ServerCore
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
//  Description: implements POP3 (Post Office Protocol v3.0)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  POP3TSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  POP3TBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  POP3TComplexEvent = procedure(ClientThread: TDXClientThread; Parm1, Parm2: string) of object;
  POP3TOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXPOP3ServerCore = class(TDXServerCore)
  private
    fOnCommandUSER: POP3TBasicEvent;  {USER john}
    fOnCommandPASS: POP3TBasicEvent;  {PASS doe}
    fOnCommandSTAT: POP3TSimpleEvent; {STAT}
    fOnCommandLIST: POP3TBasicEvent;  {LIST || LIST [msg]}
    fOnCommandUIDL: POP3TBasicEvent;  {UIDL || UIDL [msg]}
    fOnCommandRETR: POP3TBasicEvent;  {RETR [msg]}
    fOnCommandNOOP: POP3TSimpleEvent; {NOOP}
    fOnCommandDELE: POP3TBasicEvent;  {DELE [msg]}
    fOnCommandRSET: POP3TSimpleEvent; {RSET}
    fOnCommandQUIT: POP3TSimpleEvent; {QUIT}
    fOnCommandAPOP: POP3TComplexEvent;{APOP [username] [encrypted_password]}
    fOnCommandTOP:  POP3TComplexEvent;{TOP [msg] [how_many_lines]}
    fOnCommandOther: POP3TOtherEvent; {COMMAND parameters...}
  protected
    Procedure SetOnCommandUSER(value:POP3TBasicEvent);
    Procedure SetOnCommandPASS(value:POP3TBasicEvent);
    Procedure SetOnCommandSTAT(value:POP3TSimpleEvent);
    Procedure SetOnCommandLIST(value:POP3TBasicEvent);
    Procedure SetOnCommandUIDL(value:POP3TBasicEvent);
    Procedure SetOnCommandRETR(value:POP3TBasicEvent);
    Procedure SetOnCommandNOOP(value:POP3TSimpleEvent);
    Procedure SetOnCommandDELE(value:POP3TBasicEvent);
    Procedure SetOnCommandRSET(value:POP3TSimpleEvent);
    Procedure SetOnCommandQUIT(value:POP3TSimpleEvent);
    Procedure SetOnCommandAPOP(value:POP3TComplexEvent);
    Procedure SetOnCommandTOP(value:POP3TComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure SayOK(ClientThread:TDXClientThread;Message:String);
    procedure SayERR(ClientThread:TDXClientThread;Message:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:POP3TBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:POP3TSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:POP3TComplexEvent);
  published
    property OnCommandUSER: POP3TBasicEvent read fOnCommandUSER
                                        write SetOnCommandUSER;
    property OnCommandPASS: POP3TBasicEvent read fOnCommandPASS
                                        write SetOnCommandPASS;
    property OnCommandSTAT: POP3TSimpleEvent read fOnCommandSTAT
                                         write SetOnCommandSTAT;
    property OnCommandLIST: POP3TBasicEvent read fOnCommandLIST
                                        write SetOnCommandLIST;
    property OnCommandUIDL: POP3TBasicEvent read fOnCommandUIDL
                                        write SetOnCommandUIDL;
    property OnCommandRETR: POP3TBasicEvent read fOnCommandRETR
                                         write SetOnCommandRETR;
    property OnCommandNOOP: POP3TSimpleEvent read fOnCommandNOOP
                                         write SetOnCommandNOOP;
    property OnCommandDELE: POP3TBasicEvent read fOnCommandDELE
                                        write SetOnCommandDELE;
    property OnCommandRSET: POP3TSimpleEvent read fOnCommandRSET
                                        write SetOnCommandRSET;
    property OnCommandQUIT: POP3TSimpleEvent read fOnCommandQUIT
                                        write SetOnCommandQUIT;
    property OnCommandAPOP: POP3TComplexEvent read fOnCommandAPOP
                                        write SetOnCommandAPOP;
    property OnCommandTOP: POP3TComplexEvent read fOnCommandTOP
                                         write SetOnCommandTOP;
    property OnCommandOther: POP3TOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PPOP3BasicEvent=^TPOP3BasicEvent;
  TPOP3BasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP3TBasicEvent;
  End;
  PPOP3SimpleEvent=^TPOP3SimpleEvent;
  TPOP3SimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP3TSimpleEvent;
  End;
  PPOP3ComplexEvent=^TPOP3ComplexEvent;
  TPOP3ComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP3TComplexEvent;
  End;

(* from RFC1725...
    'USER', {1}
    'PASS', {2}
    'STAT', {3}
    'LIST', {4}
    'UIDL', {5}
    'RETR', {6}
    'NOOP', {7}
    'DELE', {8}
    'RSET', {9}
    'QUIT', {10}
    'APOP', {11}
    'TOP'  {12}
*)

constructor TDXPOP3ServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=110;
end;

destructor TDXPOP3ServerCore.Destroy;
Var
   PBasicEvent:PPOP3BasicEvent;
   PSimpleEvent:PPOP3SimpleEvent;
   PComplexEvent:PPOP3ComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PPOP3BasicEvent(fEventArray[0]).Tag of
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

Procedure TDXPOP3ServerCore.AddBasicEvent(Command:String;EventProc:POP3TBasicEvent);
Var
   PBasicEvent:PPOP3BasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP3BasicEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP3BasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOP3ServerCore.AddSimpleEvent(Command:String;EventProc:POP3TSimpleEvent);
Var
   PSimpleEvent:PPOP3SimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP3SimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP3SimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=2;      // Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXPOP3ServerCore.AddComplexEvent(Command:String;EventProc:POP3TComplexEvent);
Var
   PComplexEvent:PPOP3ComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP3ComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP3ComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PComplexEvent);
   PComplexEvent.Tag:=3;      // Denotes Event in fEventArray is a TComplexEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PComplexEvent);
End;

Procedure TDXPOP3ServerCore.SetOnCommandUSER(value:POP3TBasicEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandPASS(value:POP3TBasicEvent);
Begin
   fOnCommandPASS:=Value;
   AddBasicEvent('PASS',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandSTAT(value:POP3TSimpleEvent);
Begin
   fOnCommandSTAT:=Value;
   AddSimpleEvent('STAT',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandLIST(value:POP3TBasicEvent);
Begin
   fOnCommandLIST:=Value;
   AddBasicEvent('LIST',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandUIDL(value:POP3TBasicEvent);
Begin
   fOnCommandUIDL:=Value;
   AddBasicEvent('UIDL',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandRETR(value:POP3TBasicEvent);
Begin
   fOnCommandRETR:=Value;
   AddBasicEvent('RETR',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandNOOP(value:POP3TSimpleEvent);
Begin
   fOnCommandNOOP:=Value;
   AddSimpleEvent('NOOP',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandDELE(value:POP3TBasicEvent);
Begin
   fOnCommandDELE:=Value;
   AddBasicEvent('DELE',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandRSET(value:POP3TSimpleEvent);
Begin
   fOnCommandRSET:=Value;
   AddSimpleEvent('RSET',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandQUIT(value:POP3TSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandAPOP(value:POP3TComplexEvent);
Begin
   fOnCommandAPOP:=Value;
   AddComplexEvent('APOP',Value);
End;

Procedure TDXPOP3ServerCore.SetOnCommandTOP(value:POP3TComplexEvent);
Begin
   fOnCommandTOP:=Value;
   AddComplexEvent('TOP',Value);
End;

procedure TDXPOP3ServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;
Begin
   If Header<>'' then Ws:='+OK '+Header
   Else Ws:='+OK POP3 Server (ready).';
   ClientThread.Socket.Writeln(Ws);
End;

procedure TDXPOP3ServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('+OK '+Footer)
   Else
      ClientThread.Socket.Writeln('+OK Goodbye');
End;

procedure TDXPOP3ServerCore.SayOK(ClientThread:TDXClientThread;Message:String);
Begin
   ClientThread.Socket.Writeln('+OK '+Message);
End;

procedure TDXPOP3ServerCore.SayERR(ClientThread:TDXClientThread;Message:String);
Begin
   ClientThread.Socket.Writeln('+OK '+Message);
End;

procedure TDXPOP3ServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, sCmd: string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while connected do begin
         if fbForceAbort then Exit;
         s:=ReadLn(Timeout);
         If LastReadTimeout or Not ValidSocket then Exit;
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
            If PPOP3BasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PPOP3BasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PPOP3BasicEvent(fEventArray[Loop]).EventProcedure) then
                       POP3TBasicEvent(PPOP3BasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PPOP3SimpleEvent(fEventArray[Loop]).EventProcedure) then
                       POP3TSimpleEvent(PPOP3SimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PPOP3ComplexEvent(fEventArray[Loop]).EventProcedure) then
                       POP3TComplexEvent(PPOP3ComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End; {while}
         If sCMD='QUIT' then Exit; {terminate}
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then
            Writeln('-ERR command not recognized (' + sCMD + ')');
      end; {while}
   end; {with}
end; {doExecute}

end.

