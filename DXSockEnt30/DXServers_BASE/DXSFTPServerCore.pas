unit DXSFTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSFTPServerCore
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
//  Description: implements SFTP (Simple File Transfer Protocol)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  SFTPTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  SFTPTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  SFTPTComplexEvent = procedure(ClientThread: TDXClientThread; Parm1, Parm2: string) of object;
  SFTPTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXSFTPServerCore = class(TDXServerCore)
  private
    fOnCommandUSER: SFTPTBasicEvent;
    fOnCommandACCT: SFTPTBasicEvent;
    fOnCommandPASS: SFTPTBasicEvent;
    fOnCommandTYPE: SFTPTBasicEvent;
    fOnCommandLIST: SFTPTBasicEvent;
    fOnCommandCDIR: SFTPTBasicEvent;
    fOnCommandKILL: SFTPTBasicEvent;
    fOnCommandNAME: SFTPTBasicEvent;
    fOnCommandTOBE: SFTPTBasicEvent;
    fOnCommandDONE: SFTPTSimpleEvent;
    fOnCommandRETR: SFTPTBasicEvent;
    fOnCommandSTOR: SFTPTComplexEvent;
    fOnCommandOther: SFTPTOtherEvent; {COMMAND parameters...}
  protected
    Procedure SetOnCommandUSER(value:SFTPTBasicEvent);
    Procedure SetOnCommandACCT(value:SFTPTBasicEvent);
    Procedure SetOnCommandPASS(value:SFTPTBasicEvent);
    Procedure SetOnCommandTYPE(value:SFTPTBasicEvent);
    Procedure SetOnCommandLIST(value:SFTPTBasicEvent);
    Procedure SetOnCommandCDIR(value:SFTPTBasicEvent);
    Procedure SetOnCommandKILL(value:SFTPTBasicEvent);
    Procedure SetOnCommandNAME(value:SFTPTBasicEvent);
    Procedure SetOnCommandTOBE(value:SFTPTBasicEvent);
    Procedure SetOnCommandDONE(value:SFTPTSimpleEvent);
    Procedure SetOnCommandRETR(value:SFTPTBasicEvent);
    Procedure SetOnCommandSTOR(value:SFTPTComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:SFTPTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:SFTPTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:SFTPTComplexEvent);
  published
    property OnCommandUSER: SFTPTBasicEvent read fOnCommandUSER
                                            write SetOnCommandUSER;
    property OnCommandACCT: SFTPTBasicEvent read fOnCommandACCT
                                            write SetOnCommandACCT;
    property OnCommandPASS: SFTPTBasicEvent read fOnCommandPASS
                                            write SetOnCommandPASS;
    property OnCommandTYPE: SFTPTBasicEvent read fOnCommandTYPE
                                            write SetOnCommandTYPE;
    property OnCommandLIST: SFTPTBasicEvent read fOnCommandLIST
                                            write SetOnCommandLIST;
    property OnCommandCDIR: SFTPTBasicEvent read fOnCommandCDIR
                                            write SetOnCommandCDIR;
    property OnCommandKILL: SFTPTBasicEvent read fOnCommandKILL
                                            write SetOnCommandKILL;
    property OnCommandNAME: SFTPTBasicEvent read fOnCommandNAME
                                            write SetOnCommandNAME;
    property OnCommandTOBE: SFTPTBasicEvent read fOnCommandTOBE
                                            write SetOnCommandTOBE;
    property OnCommandDONE: SFTPTSimpleEvent read fOnCommandDONE
                                             write SetOnCommandDONE;
    property OnCommandRETR: SFTPTBasicEvent read fOnCommandRETR
                                            write SetOnCommandRETR;
    property OnCommandSTOR: SFTPTComplexEvent read fOnCommandSTOR
                                               write SetOnCommandSTOR;
    property OnCommandOther: SFTPTOtherEvent read fOnCommandOther
                                             write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PSFTPBasicEvent=^TSFTPBasicEvent;
  TSFTPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SFTPTBasicEvent;
  End;
  PSFTPSimpleEvent=^TSFTPSimpleEvent;
  TSFTPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SFTPTSimpleEvent;
  End;
  PSFTPComplexEvent=^TSFTPComplexEvent;
  TSFTPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SFTPTComplexEvent;
  End;

(* from RFC913...
*)

constructor TDXSFTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=115;
end;

destructor TDXSFTPServerCore.Destroy;
Var
   PBasicEvent:PSFTPBasicEvent;
   PSimpleEvent:PSFTPSimpleEvent;
   PComplexEvent:PSFTPComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PSFTPBasicEvent(fEventArray[0]).Tag of
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

Procedure TDXSFTPServerCore.AddBasicEvent(Command:String;EventProc:SFTPTBasicEvent);
Var
   PBasicEvent:PSFTPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSFTPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PSFTPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSFTPServerCore.AddSimpleEvent(Command:String;EventProc:SFTPTSimpleEvent);
Var
   PSimpleEvent:PSFTPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSFTPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PSFTPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSFTPServerCore.AddComplexEvent(Command:String;EventProc:SFTPTComplexEvent);
Var
   PComplexEvent:PSFTPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSFTPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PSFTPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSFTPServerCore.SetOnCommandUSER(value:SFTPTBasicEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandACCT(value:SFTPTBasicEvent);
Begin
   fOnCommandACCT:=Value;
   AddBasicEvent('ACCT',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandPASS(value:SFTPTBasicEvent);
Begin
   fOnCommandPASS:=Value;
   AddBasicEvent('PASS',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandTYPE(value:SFTPTBasicEvent);
Begin
   fOnCommandTYPE:=Value;
   AddBasicEvent('TYPE',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandLIST(value:SFTPTBasicEvent);
Begin
   fOnCommandLIST:=Value;
   AddBasicEvent('LIST',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandCDIR(value:SFTPTBasicEvent);
Begin
   fOnCommandCDIR:=Value;
   AddBasicEvent('CDIR',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandKILL(value:SFTPTBasicEvent);
Begin
   fOnCommandKILL:=Value;
   AddBasicEvent('KILL',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandNAME(value:SFTPTBasicEvent);
Begin
   fOnCommandNAME:=Value;
   AddBasicEvent('NAME',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandTOBE(value:SFTPTBasicEvent);
Begin
   fOnCommandTOBE:=Value;
   AddBasicEvent('TOBE',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandDONE(value:SFTPTSimpleEvent);
Begin
   fOnCommandDONE:=Value;
   AddSimpleEvent('DONE',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandRETR(value:SFTPTBasicEvent);
Begin
   fOnCommandRETR:=Value;
   AddBasicEvent('RETR',Value);
End;

Procedure TDXSFTPServerCore.SetOnCommandSTOR(value:SFTPTComplexEvent);
Begin
   fOnCommandSTOR:=Value;
   AddComplexEvent('STOR',Value);
End;

procedure TDXSFTPServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;
Begin
   If Header<>'' then Ws:='+ '+Header
   Else Ws:='+ SFTP Server (ready).';
   ClientThread.Socket.Writeln(Ws);
End;

procedure TDXSFTPServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('+ OK '+Footer)
   Else
      ClientThread.Socket.Writeln('+ OK Goodbye');
End;

procedure TDXSFTPServerCore.ProcessSession(ClientThread: TDXClientThread);
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
         if S='' then continue;
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
            If PSFTPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PSFTPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PSFTPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       SFTPTBasicEvent(PSFTPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PSFTPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       SFTPTSimpleEvent(PSFTPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PSFTPComplexEvent(fEventArray[Loop]).EventProcedure) then
                       SFTPTComplexEvent(PSFTPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
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

