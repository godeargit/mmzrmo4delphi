unit DXTFTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXTFTPServerCore
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
//  Description: implements TFTP (Trivial File Transfer Protocol)
// ========================================================================
// RFC - Trivial File Transfer Protocol
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TFTPTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  TFTPTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  TFTPTComplexEvent = procedure(ClientThread: TDXClientThread; Parm1, Parm2: string) of object;
  TFTPTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXTFTPServerCore = class(TDXServerCore)
  private
    fOnCommandUSER: TFTPTBasicEvent;
    fOnCommandACCT: TFTPTBasicEvent;
    fOnCommandPASS: TFTPTBasicEvent;
    fOnCommandTYPE: TFTPTBasicEvent;
    fOnCommandLIST: TFTPTBasicEvent;
    fOnCommandCDIR: TFTPTBasicEvent;
    fOnCommandKILL: TFTPTBasicEvent;
    fOnCommandNAME: TFTPTBasicEvent;
    fOnCommandTOBE: TFTPTBasicEvent;
    fOnCommandDONE: TFTPTSimpleEvent;
    fOnCommandRETR: TFTPTBasicEvent;
    fOnCommandSTOR: TFTPTComplexEvent;
    fOnCommandOther: TFTPTOtherEvent; {COMMAND parameters...}
  protected
    Procedure SetOnCommandUSER(value:TFTPTBasicEvent);
    Procedure SetOnCommandACCT(value:TFTPTBasicEvent);
    Procedure SetOnCommandPASS(value:TFTPTBasicEvent);
    Procedure SetOnCommandTYPE(value:TFTPTBasicEvent);
    Procedure SetOnCommandLIST(value:TFTPTBasicEvent);
    Procedure SetOnCommandCDIR(value:TFTPTBasicEvent);
    Procedure SetOnCommandKILL(value:TFTPTBasicEvent);
    Procedure SetOnCommandNAME(value:TFTPTBasicEvent);
    Procedure SetOnCommandTOBE(value:TFTPTBasicEvent);
    Procedure SetOnCommandDONE(value:TFTPTSimpleEvent);
    Procedure SetOnCommandRETR(value:TFTPTBasicEvent);
    Procedure SetOnCommandSTOR(value:TFTPTComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:TFTPTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:TFTPTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:TFTPTComplexEvent);
  published
    property OnCommandUSER: TFTPTBasicEvent read fOnCommandUSER
                                            write SetOnCommandUSER;
    property OnCommandACCT: TFTPTBasicEvent read fOnCommandACCT
                                            write SetOnCommandACCT;
    property OnCommandPASS: TFTPTBasicEvent read fOnCommandPASS
                                            write SetOnCommandPASS;
    property OnCommandTYPE: TFTPTBasicEvent read fOnCommandTYPE
                                            write SetOnCommandTYPE;
    property OnCommandLIST: TFTPTBasicEvent read fOnCommandLIST
                                            write SetOnCommandLIST;
    property OnCommandCDIR: TFTPTBasicEvent read fOnCommandCDIR
                                            write SetOnCommandCDIR;
    property OnCommandKILL: TFTPTBasicEvent read fOnCommandKILL
                                            write SetOnCommandKILL;
    property OnCommandNAME: TFTPTBasicEvent read fOnCommandNAME
                                            write SetOnCommandNAME;
    property OnCommandTOBE: TFTPTBasicEvent read fOnCommandTOBE
                                            write SetOnCommandTOBE;
    property OnCommandDONE: TFTPTSimpleEvent read fOnCommandDONE
                                             write SetOnCommandDONE;
    property OnCommandRETR: TFTPTBasicEvent read fOnCommandRETR
                                            write SetOnCommandRETR;
    property OnCommandSTOR: TFTPTComplexEvent read fOnCommandSTOR
                                               write SetOnCommandSTOR;
    property OnCommandOther: TFTPTOtherEvent read fOnCommandOther
                                             write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PTFTPBasicEvent=^TTFTPBasicEvent;
  TTFTPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:TFTPTBasicEvent;
  End;
  PTFTPSimpleEvent=^TTFTPSimpleEvent;
  TTFTPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:TFTPTSimpleEvent;
  End;
  PTFTPComplexEvent=^TTFTPComplexEvent;
  TTFTPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:TFTPTComplexEvent;
  End;

(* from RFC906, 1350, 2090, 2347, 2348, 2349
*)

constructor TDXTFTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=69;
end;

destructor TDXTFTPServerCore.Destroy;
Var
   PBasicEvent:PTFTPBasicEvent;
   PSimpleEvent:PTFTPSimpleEvent;
   PComplexEvent:PTFTPComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PTFTPBasicEvent(fEventArray[0]).Tag of
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

Procedure TDXTFTPServerCore.AddBasicEvent(Command:String;EventProc:TFTPTBasicEvent);
Var
   PBasicEvent:PTFTPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PTFTPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PTFTPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXTFTPServerCore.AddSimpleEvent(Command:String;EventProc:TFTPTSimpleEvent);
Var
   PSimpleEvent:PTFTPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PTFTPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PTFTPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXTFTPServerCore.AddComplexEvent(Command:String;EventProc:TFTPTComplexEvent);
Var
   PComplexEvent:PTFTPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PTFTPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PTFTPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXTFTPServerCore.SetOnCommandUSER(value:TFTPTBasicEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandACCT(value:TFTPTBasicEvent);
Begin
   fOnCommandACCT:=Value;
   AddBasicEvent('ACCT',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandPASS(value:TFTPTBasicEvent);
Begin
   fOnCommandPASS:=Value;
   AddBasicEvent('PASS',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandTYPE(value:TFTPTBasicEvent);
Begin
   fOnCommandTYPE:=Value;
   AddBasicEvent('TYPE',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandLIST(value:TFTPTBasicEvent);
Begin
   fOnCommandLIST:=Value;
   AddBasicEvent('LIST',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandCDIR(value:TFTPTBasicEvent);
Begin
   fOnCommandCDIR:=Value;
   AddBasicEvent('CDIR',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandKILL(value:TFTPTBasicEvent);
Begin
   fOnCommandKILL:=Value;
   AddBasicEvent('KILL',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandNAME(value:TFTPTBasicEvent);
Begin
   fOnCommandNAME:=Value;
   AddBasicEvent('NAME',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandTOBE(value:TFTPTBasicEvent);
Begin
   fOnCommandTOBE:=Value;
   AddBasicEvent('TOBE',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandDONE(value:TFTPTSimpleEvent);
Begin
   fOnCommandDONE:=Value;
   AddSimpleEvent('DONE',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandRETR(value:TFTPTBasicEvent);
Begin
   fOnCommandRETR:=Value;
   AddBasicEvent('RETR',Value);
End;

Procedure TDXTFTPServerCore.SetOnCommandSTOR(value:TFTPTComplexEvent);
Begin
   fOnCommandSTOR:=Value;
   AddComplexEvent('STOR',Value);
End;

procedure TDXTFTPServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;
Begin
   If Header<>'' then Ws:='+ '+Header
   Else Ws:='+ TFTP Server (ready).';
   ClientThread.Socket.Writeln(Ws);
End;

procedure TDXTFTPServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('+ OK '+Footer)
   Else
      ClientThread.Socket.Writeln('+ OK Goodbye');
End;

procedure TDXTFTPServerCore.ProcessSession(ClientThread: TDXClientThread);
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
            If PTFTPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PTFTPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PTFTPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       TFTPTBasicEvent(PTFTPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PTFTPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       TFTPTSimpleEvent(PTFTPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PTFTPComplexEvent(fEventArray[Loop]).EventProcedure) then
                       TFTPTComplexEvent(PTFTPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
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
            Writeln('! command not recognized (' + sCMD + ')');
      end; {while}
   end; {with}
end; {doExecute}

end.

