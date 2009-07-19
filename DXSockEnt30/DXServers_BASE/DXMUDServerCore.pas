unit DXMUDServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXMUDServerCore
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
//  Description: implements MUD (Multi-User Dungeon) Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  MUDTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  MUDTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  MUDTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;
  MUDTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; var Handled: Boolean) of object;

  TDXMUDServerCore = class(TDXServerCore)
  private
    fOnCommandNORTH: MUDTSimpleEvent;
    fOnCommandSOUTH: MUDTSimpleEvent;
    fOnCommandEAST: MUDTSimpleEvent;
    fOnCommandWEST: MUDTSimpleEvent;
    fOnCommandQUIT: MUDTSimpleEvent;
    fOnCommandINV: MUDTSimpleEvent;    // INV = INVentory
    fOnCommandSAY: MUDTBasicEvent;
    fOnCommandLOOK: MUDTBasicEvent;
    fOnCommandOther: MUDTOtherEvent;
  protected
    Procedure SetOnCommandNORTH(value: MUDTSimpleEvent);
    Procedure SetOnCommandSOUTH(value: MUDTSimpleEvent);
    Procedure SetOnCommandEAST(value: MUDTSimpleEvent);
    Procedure SetOnCommandWEST(value: MUDTSimpleEvent);
    Procedure SetOnCommandQUIT(value: MUDTSimpleEvent);
    Procedure SetOnCommandINV(value: MUDTSimpleEvent);    // INV = INVentory
    Procedure SetOnCommandSAY(value: MUDTBasicEvent);
    Procedure SetOnCommandLOOK(value: MUDTBasicEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header,MOTD:TStrings);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:MUDTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:MUDTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:MUDTComplexEvent);
  published
    property OnCommandNORTH: MUDTSimpleEvent read fOnCommandNORTH
                                            write SetOnCommandNORTH;
    property OnCommandSOUTH: MUDTSimpleEvent read fOnCommandSOUTH
                                            write SetOnCommandSOUTH;
    property OnCommandEAST: MUDTSimpleEvent read fOnCommandEAST
                                            write SetOnCommandEAST;
    property OnCommandWEST: MUDTSimpleEvent read fOnCommandWEST
                                            write SetOnCommandWEST;
    property OnCommandQUIT: MUDTSimpleEvent read fOnCommandQUIT
                                            write SetOnCommandQUIT;
    property OnCommandINV: MUDTSimpleEvent read fOnCommandINV
                                            write SetOnCommandINV;
    property OnCommandSAY: MUDTBasicEvent read fOnCommandSAY
                                            write SetOnCommandSAY;
    property OnCommandLOOK: MUDTBasicEvent read fOnCommandLOOK
                                            write SetOnCommandLOOK;
    property OnCommandOther: MUDTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
  end;

implementation

uses
   DXSock,
   DXString;

Type
  PMUDBasicEvent=^TMUDBasicEvent;
  TMUDBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MUDTBasicEvent;
  End;
  PMUDSimpleEvent=^TMUDSimpleEvent;
  TMUDSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MUDTSimpleEvent;
  End;
  PMUDComplexEvent=^TMUDComplexEvent;
  TMUDComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MUDTComplexEvent;
  End;

constructor TDXMUDServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=4040;
end;

destructor TDXMUDServerCore.Destroy;
Var
   PBasicEvent:PMUDBasicEvent;
   PSimpleEvent:PMUDSimpleEvent;
   PComplexEvent:PMUDComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PMUDBasicEvent(fEventArray[0]).Tag of
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

Procedure TDXMUDServerCore.AddBasicEvent(Command:String;EventProc:MUDTBasicEvent);
Var
   PBasicEvent:PMUDBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMUDBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PMUDBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXMUDServerCore.AddSimpleEvent(Command:String;EventProc:MUDTSimpleEvent);
Var
   PSimpleEvent:PMUDSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMUDSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PMUDSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXMUDServerCore.AddComplexEvent(Command:String;EventProc:MUDTComplexEvent);
Var
   PComplexEvent:PMUDComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMUDComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PMUDComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXMUDServerCore.SetOnCommandNORTH(value:MUDTSimpleEvent);
Begin
   fOnCommandNORTH:=Value;
   AddSimpleEvent('NORTH',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandSOUTH(value:MUDTSimpleEvent);
Begin
   fOnCommandSOUTH:=Value;
   AddSimpleEvent('SOUTH',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandEAST(value:MUDTSimpleEvent);
Begin
   fOnCommandEAST:=Value;
   AddSimpleEvent('EAST',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandWEST(value:MUDTSimpleEvent);
Begin
   fOnCommandWEST:=Value;
   AddSimpleEvent('WEST',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandQUIT(value:MUDTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandINV(value:MUDTSimpleEvent);
Begin
   fOnCommandINV:=Value;
   AddSimpleEvent('INV',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandSAY(value:MUDTBasicEvent);
Begin
   fOnCommandSAY:=Value;
   AddBasicEvent('SAY',Value);
End;

Procedure TDXMUDServerCore.SetOnCommandLOOK(value:MUDTBasicEvent);
Begin
   fOnCommandLOOK:=Value;
   AddBasicEvent('LOOK',Value);
End;

procedure TDXMUDServerCore.SayHello(ClientThread:TDXClientThread;Header,MOTD:TStrings);
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
      ClientThread.Socket.Writeln('MUD Server Ready.');
   End;
   If Assigned(MOTD) then Begin
      For Loop:=1 to MOTD.Count do Begin
      Ws:=MOTD[Loop-1];
         ClientThread.Socket.Writeln(Ws);
      End;
   End;
   ClientThread.Socket.Writeln('Proceed with login...');
End;

procedure TDXMUDServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln(Footer)
   Else
      ClientThread.Socket.Writeln('Goodbye, please come back soon!');
End;

procedure TDXMUDServerCore.ProcessSession(ClientThread:TDXClientThread);
var
   s, sCmd: string;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         if fbForceAbort then exit;
         s:=ReadLn(Timeout);
         If LastReadTimeout then Exit;
         If Not ValidSocket then Exit;
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
            If PMUDBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PMUDBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PMUDBasicEvent(fEventArray[Loop]).EventProcedure) then
                       MUDTBasicEvent(PMUDBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PMUDSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       MUDTSimpleEvent(PMUDSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PMUDComplexEvent(fEventArray[Loop]).EventProcedure) then
                       MUDTComplexEvent(PMUDComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,#32,False)),S);
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
            Writeln('500 command not recognized (' + sCMD + ')');
      end;
   end;
end;

end.

