unit DXPOPPassDServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXPOPPassDServerCore
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
//  Description: implements POP Password Daemon Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  POPPassDTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  POPPassDTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  POPPassDTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXPOPPassDServerCore = class(TDXServerCore)
  private
    fOnCommandUSER: POPPassDTBasicEvent;  {USER john}
    fOnCommandPASS: POPPassDTBasicEvent;  {PASS doe}
    fOnCommandNEWPASS: POPPassDTBasicEvent;  {NEWPASS doe2}
    fOnCommandQUIT: POPPassDTSimpleEvent; {QUIT}
    fOnCommandOther: POPPassDTOtherEvent; {COMMAND parameters...}
  protected
    Procedure SetOnCommandUSER(value:POPPassDTBasicEvent);
    Procedure SetOnCommandPASS(value:POPPassDTBasicEvent);
    Procedure SetOnCommandNEWPASS(value:POPPassDTBasicEvent);
    Procedure SetOnCommandQUIT(value:POPPassDTSimpleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure NewPassOK(ClientThread:TDXClientThread;Message:String);
    procedure NewPassERR(ClientThread:TDXClientThread;Message:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:POPPassDTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:POPPassDTSimpleEvent);
  published
    property OnCommandUSER: POPPassDTBasicEvent read fOnCommandUSER
                                                write SetOnCommandUSER;
    property OnCommandPASS: POPPassDTBasicEvent read fOnCommandPASS
                                                write SetOnCommandPASS;
    property OnCommandNEWPASS: POPPassDTBasicEvent read fOnCommandNEWPASS
                                                   write SetOnCommandNEWPASS;
    property OnCommandQUIT: POPPassDTSimpleEvent read fOnCommandQUIT
                                        write SetOnCommandQUIT;
    property OnCommandOther: POPPassDTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PPOPPassDBasicEvent=^TPOPPassDBasicEvent;
  TPOPPassDBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POPPassDTBasicEvent;
  End;
  PPOPPassDSimpleEvent=^TPOPPassDSimpleEvent;
  TPOPPassDSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POPPassDTSimpleEvent;
  End;
(* from http://www.netwinsite.com/poppassd/
    'USER', {1}
    'PASS', {2}
    'NEWPASS', {3}
    'QUIT', {4}
*)

constructor TDXPOPPassDServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=106;
end;

destructor TDXPOPPassDServerCore.Destroy;
Var
   PBasicEvent:PPOPPassDBasicEvent;
   PSimpleEvent:PPOPPassDSimpleEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PPOPPassDBasicEvent(fEventArray[0]).Tag of
            1:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            2:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXPOPPassDServerCore.AddBasicEvent(Command:String;EventProc:POPPassDTBasicEvent);
Var
   PBasicEvent:PPOPPassDBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOPPassDBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PPOPPassDBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOPPassDServerCore.AddSimpleEvent(Command:String;EventProc:POPPassDTSimpleEvent);
Var
   PSimpleEvent:PPOPPassDSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOPPassDSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PPOPPassDSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOPPassDServerCore.SetOnCommandUSER(value:POPPassDTBasicEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

Procedure TDXPOPPassDServerCore.SetOnCommandPASS(value:POPPassDTBasicEvent);
Begin
   fOnCommandPASS:=Value;
   AddBasicEvent('PASS',Value);
End;

Procedure TDXPOPPassDServerCore.SetOnCommandNEWPASS(value:POPPassDTBasicEvent);
Begin
   fOnCommandNEWPASS:=Value;
   AddBasicEvent('NEWPASS',Value);
End;

Procedure TDXPOPPassDServerCore.SetOnCommandQUIT(value:POPPassDTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

procedure TDXPOPPassDServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;
Begin
   If Header<>'' then Ws:='200 '+Header
   Else Ws:='200 POPPassD Server (ready).';
   ClientThread.Socket.Writeln(Ws);
End;

procedure TDXPOPPassDServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('200 '+Footer)
   Else
      ClientThread.Socket.Writeln('200 Goodbye');
End;

procedure TDXPOPPassDServerCore.NewPassOK(ClientThread:TDXClientThread;Message:String);
Begin
   ClientThread.Socket.Writeln('200 '+Message);
End;

procedure TDXPOPPassDServerCore.NewPassERR(ClientThread:TDXClientThread;Message:String);
Begin
   ClientThread.Socket.Writeln('500 '+Message);
End;

procedure TDXPOPPassDServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, sCmd: string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;

  procedure NotHandled(CMD: string);
  Var
     Ws:String;

  begin
     Ws:='500 command not recognized (' + CMD + ')';
     ClientThread.Socket.Writeln(Ws);
  end;

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
            If PPOPPassDBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PPOPPassDBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PPOPPassDBasicEvent(fEventArray[Loop]).EventProcedure) then
                       POPPassDTBasicEvent(PPOPPassDBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PPOPPassDSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       POPPassDTSimpleEvent(PPOPPassDSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
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
         if not WasHandled then NotHandled(sCmd);
      end; {while}
   end; {with}
end; {doExecute}

end.

