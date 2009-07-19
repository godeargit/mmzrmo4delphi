unit DXPOP2ServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXPOP2ServerCore
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
//  Description: implements POP2 (Post Office Protocol v2.0)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  POP2TSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  POP2TBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  POP2TComplexEvent = procedure(ClientThread: TDXClientThread; Parm1, Parm2: string) of object;
  POP2TOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXPOP2ServerCore = class(TDXServerCore)
  private
    fOnCommandHELO: POP2TComplexEvent;
    fOnCommandFOLD: POP2TBasicEvent;
    fOnCommandREAD: POP2TBasicEvent;
    fOnCommandRETR: POP2TSimpleEvent;
    fOnCommandACKS: POP2TSimpleEvent;
    fOnCommandACKD: POP2TSimpleEvent;
    fOnCommandNACK: POP2TSimpleEvent;
    fOnCommandQUIT: POP2TSimpleEvent;
    fOnCommandOther: POP2TOtherEvent; {COMMAND parameters...}
  protected
    Procedure SetOnCommandHELO(value:POP2TComplexEvent);
    Procedure SetOnCommandFOLD(value:POP2TBasicEvent);
    Procedure SetOnCommandREAD(value:POP2TBasicEvent);
    Procedure SetOnCommandRETR(value:POP2TSimpleEvent);
    Procedure SetOnCommandACKS(value:POP2TSimpleEvent);
    Procedure SetOnCommandACKD(value:POP2TSimpleEvent);
    Procedure SetOnCommandNACK(value:POP2TSimpleEvent);
    Procedure SetOnCommandQUIT(value:POP2TSimpleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:POP2TBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:POP2TSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:POP2TComplexEvent);
  published
    property OnCommandHELO: POP2TComplexEvent read fOnCommandHELO
                                              write SetOnCommandHELO;
    property OnCommandFOLD: POP2TBasicEvent read fOnCommandFOLD
                                            write SetOnCommandFOLD;
    property OnCommandREAD: POP2TBasicEvent read fOnCommandREAD
                                            write SetOnCommandREAD;
    property OnCommandRETR: POP2TSimpleEvent read fOnCommandRETR
                                             write SetOnCommandRETR;
    property OnCommandACKS: POP2TSimpleEvent read fOnCommandACKS
                                             write SetOnCommandACKS;
    property OnCommandACKD: POP2TSimpleEvent read fOnCommandACKD
                                             write SetOnCommandACKD;
    property OnCommandNACK: POP2TSimpleEvent read fOnCommandNACK
                                             write SetOnCommandNACK;
    property OnCommandQUIT: POP2TSimpleEvent read fOnCommandQUIT
                                             write SetOnCommandQUIT;
    property OnCommandOther: POP2TOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PPOP2BasicEvent=^TPOP2BasicEvent;
  TPOP2BasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP2TBasicEvent;
  End;
  PPOP2SimpleEvent=^TPOP2SimpleEvent;
  TPOP2SimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP2TSimpleEvent;
  End;
  PPOP2ComplexEvent=^TPOP2ComplexEvent;
  TPOP2ComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:POP2TComplexEvent;
  End;

(* from RFC937
*)

constructor TDXPOP2ServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=109;
end;

destructor TDXPOP2ServerCore.Destroy;
Var
   PBasicEvent:PPOP2BasicEvent;
   PSimpleEvent:PPOP2SimpleEvent;
   PComplexEvent:PPOP2ComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PPOP2BasicEvent(fEventArray[0]).Tag of
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

Procedure TDXPOP2ServerCore.AddBasicEvent(Command:String;EventProc:POP2TBasicEvent);
Var
   PBasicEvent:PPOP2BasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP2BasicEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP2BasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOP2ServerCore.AddSimpleEvent(Command:String;EventProc:POP2TSimpleEvent);
Var
   PSimpleEvent:PPOP2SimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP2SimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP2SimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOP2ServerCore.AddComplexEvent(Command:String;EventProc:POP2TComplexEvent);
Var
   PComplexEvent:PPOP2ComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PPOP2ComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PPOP2ComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXPOP2ServerCore.SetOnCommandHELO(value:POP2TComplexEvent);
Begin
   fOnCommandHELO:=Value;
   AddComplexEvent('HELO',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandFOLD(value:POP2TBasicEvent);
Begin
   fOnCommandFOLD:=Value;
   AddBasicEvent('FOLD',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandREAD(value:POP2TBasicEvent);
Begin
   fOnCommandREAD:=Value;
   AddBasicEvent('READ',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandRETR(value:POP2TSimpleEvent);
Begin
   fOnCommandRETR:=Value;
   AddSimpleEvent('RETR',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandACKS(value:POP2TSimpleEvent);
Begin
   fOnCommandACKS:=Value;
   AddSimpleEvent('ACKS',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandACKD(value:POP2TSimpleEvent);
Begin
   fOnCommandACKD:=Value;
   AddSimpleEvent('ACKD',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandNACK(value:POP2TSimpleEvent);
Begin
   fOnCommandNACK:=Value;
   AddSimpleEvent('NACK',Value);
End;

Procedure TDXPOP2ServerCore.SetOnCommandQUIT(value:POP2TSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

procedure TDXPOP2ServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;
Begin
   If Header<>'' then Ws:='+OK '+Header
   Else Ws:='+OK POP2 Server (ready).';
   ClientThread.Socket.Writeln(Ws);
End;

procedure TDXPOP2ServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('+ OK '+Footer)
   Else
      ClientThread.Socket.Writeln('+ OK Goodbye');
End;

procedure TDXPOP2ServerCore.ProcessSession(ClientThread: TDXClientThread);
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
            If PPOP2BasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PPOP2BasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PPOP2BasicEvent(fEventArray[Loop]).EventProcedure) then
                       POP2TBasicEvent(PPOP2BasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PPOP2SimpleEvent(fEventArray[Loop]).EventProcedure) then
                       POP2TSimpleEvent(PPOP2SimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PPOP2ComplexEvent(fEventArray[Loop]).EventProcedure) then
                       POP2TComplexEvent(PPOP2ComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
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
            Writeln('- command not recognized (' + sCMD + ')');
      end; {while}
   end; {with}
end; {doExecute}

end.

