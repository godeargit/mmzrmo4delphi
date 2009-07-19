unit DXIMAP4ServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXIMAP4ServerCore
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
//  Description: implements IMAP4 Email Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  IMAPTSimpleEvent = procedure(ClientThread: TDXClientThread; Tag: string) of object;
  IMAPTBasicEvent = procedure(ClientThread: TDXClientThread; Tag: string; Parm: string) of object;
  IMAPTComplexEvent = procedure(ClientThread: TDXClientThread; Tag: string; Parm1, Parm2: string) of object;
  IMAPTOtherEvent = procedure(ClientThread: TDXClientThread; Tag: string; Command: string; Parm: string;Var Handled: Boolean) of object;

  TDXIMAP4ServerCore = class(TDXServerCore)
  private
    fOnCommandCAPABILITY: IMAPTSimpleEvent;
    fOnCommandNOOP: IMAPTSimpleEvent;
    fOnCommandLOGOUT: IMAPTSimpleEvent;
    fOnCommandAUTHENTICATE: IMAPTBasicEvent;
    fOnCommandLOGIN: IMAPTComplexEvent;
    fOnCommandSELECT: IMAPTBasicEvent;
    fOnCommandEXAMINE: IMAPTBasicEvent;
    fOnCommandCREATE: IMAPTBasicEvent;
    fOnCommandDELETE: IMAPTBasicEvent;
    fOnCommandRENAME: IMAPTComplexEvent;
    fOnCommandSUBSCRIBE: IMAPTBasicEvent;
    fOnCommandUNSUBSCRIBE: IMAPTBasicEvent;
    fOnCommandLIST: IMAPTComplexEvent;
    fOnCommandLSUB: IMAPTComplexEvent;
    fOnCommandSTATUS: IMAPTComplexEvent;
    fOnCommandAPPEND: IMAPTComplexEvent;
    fOnCommandCHECK: IMAPTSimpleEvent;
    fOnCommandCLOSE: IMAPTSimpleEvent;
    fOnCommandEXPUNGE: IMAPTSimpleEvent;
    fOnCommandSEARCH: IMAPTComplexEvent;
    fOnCommandFETCH: IMAPTComplexEvent;
    fOnCommandSTORE: IMAPTComplexEvent;
    fOnCommandCOPY: IMAPTComplexEvent;
    fOnCommandUID: IMAPTComplexEvent;
    fOnCommandOther: IMAPTOtherEvent;
  protected
    Procedure SetOnCommandCAPABILITY(value: IMAPTSimpleEvent);
    Procedure SetOnCommandNOOP(value: IMAPTSimpleEvent);
    Procedure SetOnCommandLOGOUT(value: IMAPTSimpleEvent);
    Procedure SetOnCommandAUTHENTICATE(value: IMAPTBasicEvent);
    Procedure SetOnCommandLOGIN(value: IMAPTComplexEvent);
    Procedure SetOnCommandSELECT(value: IMAPTBasicEvent);
    Procedure SetOnCommandEXAMINE(value: IMAPTBasicEvent);
    Procedure SetOnCommandCREATE(value: IMAPTBasicEvent);
    Procedure SetOnCommandDELETE(value: IMAPTBasicEvent);
    Procedure SetOnCommandRENAME(value: IMAPTComplexEvent);
    Procedure SetOnCommandSUBSCRIBE(value: IMAPTBasicEvent);
    Procedure SetOnCommandUNSUBSCRIBE(value: IMAPTBasicEvent);
    Procedure SetOnCommandLIST(value: IMAPTComplexEvent);
    Procedure SetOnCommandLSUB(value: IMAPTComplexEvent);
    Procedure SetOnCommandSTATUS(value: IMAPTComplexEvent);
    Procedure SetOnCommandAPPEND(value: IMAPTComplexEvent);
    Procedure SetOnCommandCHECK(value: IMAPTSimpleEvent);
    Procedure SetOnCommandCLOSE(value: IMAPTSimpleEvent);
    Procedure SetOnCommandEXPUNGE(value: IMAPTSimpleEvent);
    Procedure SetOnCommandSEARCH(value: IMAPTComplexEvent);
    Procedure SetOnCommandFETCH(value: IMAPTComplexEvent);
    Procedure SetOnCommandSTORE(value: IMAPTComplexEvent);
    Procedure SetOnCommandCOPY(value: IMAPTComplexEvent);
    Procedure SetOnCommandUID(value: IMAPTComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddSimpleEvent(Command:String;EventProc:IMAPTSimpleEvent);
    Procedure AddBasicEvent(Command:String;EventProc:IMAPTBasicEvent);
    Procedure AddComplexEvent(Command:String;EventProc:IMAPTComplexEvent);
  published
    property OnCommandCAPABILITY: IMAPTSimpleEvent read fOnCommandCAPABILITY
                                                   write SetOnCommandCAPABILITY;
    property OnCommandNOOP: IMAPTSimpleEvent read fOnCommandNOOP
                                             write SetOnCommandNOOP;
    property OnCommandLOGOUT: IMAPTSimpleEvent read fOnCommandLOGOUT
                                               write SetOnCommandLOGOUT;
    property OnCommandAUTHENTICATE: IMAPTBasicEvent read fOnCommandAUTHENTICATE
                                                    write SetOnCommandAUTHENTICATE;
    property OnCommandLOGIN: IMAPTComplexEvent read fOnCommandLOGIN
                                               write SetOnCommandLOGIN;
    property OnCommandSELECT: IMAPTBasicEvent read fOnCommandSELECT
                                              write SetOnCommandSELECT;
    property OnCommandEXAMINE: IMAPTBasicEvent read fOnCommandEXAMINE
                                               write SetOnCommandEXAMINE;
    property OnCommandCREATE: IMAPTBasicEvent read fOnCommandCREATE
                                              write SetOnCommandCREATE;
    property OnCommandDELETE: IMAPTBasicEvent read fOnCommandDELETE
                                              write SetOnCommandDELETE;
    property OnCommandRENAME: IMAPTComplexEvent read fOnCommandRENAME
                                                write SetOnCommandRENAME;
    property OnCommandSUBSCRIBE: IMAPTBasicEvent read fOnCommandSUBSCRIBE
                                                 write SetOnCommandSUBSCRIBE;
    property OnCommandUNSUBSCRIBE: IMAPTBasicEvent read fOnCommandUNSUBSCRIBE
                                                   write SetOnCommandUNSUBSCRIBE;
    property OnCommandLIST: IMAPTComplexEvent read fOnCommandLIST
                                              write SetOnCommandLIST;
    property OnCommandLSUB: IMAPTComplexEvent read fOnCommandLSUB
                                              write SetOnCommandLSUB;
    property OnCommandSTATUS: IMAPTComplexEvent read fOnCommandSTATUS
                                                write SetOnCommandSTATUS;
    property OnCommandAPPEND: IMAPTComplexEvent read fOnCommandAPPEND
                                                write SetOnCommandAPPEND;
    property OnCommandCHECK: IMAPTSimpleEvent read fOnCommandCHECK
                                              write SetOnCommandCHECK;
    property OnCommandCLOSE: IMAPTSimpleEvent read fOnCommandCLOSE
                                              write SetOnCommandCLOSE;
    property OnCommandEXPUNGE: IMAPTSimpleEvent read fOnCommandEXPUNGE
                                                write SetOnCommandEXPUNGE;
    property OnCommandSEARCH: IMAPTComplexEvent read fOnCommandSEARCH
                                                write SetOnCommandSEARCH;
    property OnCommandFETCH: IMAPTComplexEvent read fOnCommandFETCH
                                               write SetOnCommandFETCH;
    property OnCommandSTORE: IMAPTComplexEvent read fOnCommandSTORE
                                               write SetOnCommandSTORE;
    property OnCommandCOPY: IMAPTComplexEvent read fOnCommandCOPY
                                              write SetOnCommandCOPY;
    property OnCommandUID: IMAPTComplexEvent read fOnCommandUID
                                             write SetOnCommandUID;
    property OnCommandOther: IMAPTOtherEvent read fOnCommandOther
                                             write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PIMAPBasicEvent=^TIMAPBasicEvent;
  TIMAPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IMAPTBasicEvent;
  End;
  PIMAPSimpleEvent=^TIMAPSimpleEvent;
  TIMAPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IMAPTSimpleEvent;
  End;
  PIMAPComplexEvent=^TIMAPComplexEvent;
  TIMAPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IMAPTComplexEvent;
  End;

///////////////////////////////////////////////////////////////////////////////
//   {Any state}
//   'CAPABILITY',                   {1}
//   'NOOP',                         {2}
//   'LOGOUT',                       {3}
//   {Non-authenticated state}
//   'AUTHENTICATE',                 {4}
//   'LOGIN',                        {5}
//   {Authenticated state}
//   'SELECT',                       {6}
//   'EXAMINE',                      {7}
//   'CREATE',                       {8}
//   'DELETE',                       {9}
//   'RENAME',                       {10}
//   'SUBSCRIBE',                    {11}
//   'UNSUBSCRIBE',                  {12}
//   'LIST',                         {13}
//   'LSUB',                         {14}
//   'STATUS',                       {15}
//   'APPEND',                       {16}
//   {Selected state}
//   'CHECK',                        {17}
//   'CLOSE',                        {18}
//   'EXPUNGE',                      {19}
//   'SEARCH',                       {20}
//   'FETCH',                        {21}
//   'STORE',                        {22}
//   'COPY',                         {23}
//   'UID'                           {24}
///////////////////////////////////////////////////////////////////////////////

constructor TDXIMAP4ServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=143;
end;

destructor TDXIMAP4ServerCore.Destroy;
Var
   PBasicEvent:PIMAPBasicEvent;
   PSimpleEvent:PIMAPSimpleEvent;
   PComplexEvent:PIMAPComplexEvent;
begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PIMAPBasicEvent(fEventArray[0]).Tag of
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

Procedure TDXIMAP4ServerCore.AddBasicEvent(Command:String;EventProc:IMAPTBasicEvent);
Var
   PBasicEvent:PIMAPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIMAPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PIMAPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXIMAP4ServerCore.AddSimpleEvent(Command:String;EventProc:IMAPTSimpleEvent);
Var
   PSimpleEvent:PIMAPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIMAPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PIMAPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXIMAP4ServerCore.AddComplexEvent(Command:String;EventProc:IMAPTComplexEvent);
Var
   PComplexEvent:PIMAPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIMAPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PIMAPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXIMAP4ServerCore.SetOnCommandCAPABILITY(value: IMAPTSimpleEvent);
Begin
   fOnCommandCAPABILITY:=Value;
   AddSimpleEvent('CAPABILITY',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandNOOP(value: IMAPTSimpleEvent);
Begin
   fOnCommandNOOP:=Value;
   AddSimpleEvent('NOOP',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandLOGOUT(value: IMAPTSimpleEvent);
Begin
   fOnCommandLOGOUT:=Value;
   AddSimpleEvent('LOGOUT',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandAUTHENTICATE(value: IMAPTBasicEvent);
Begin
   fOnCommandAUTHENTICATE:=Value;
   AddBasicEvent('AUTHENTICATE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandLOGIN(value: IMAPTComplexEvent);
Begin
   fOnCommandLOGIN:=Value;
   AddComplexEvent('LOGIN',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandSELECT(value: IMAPTBasicEvent);
Begin
   fOnCommandSELECT:=Value;
   AddBasicEvent('SELECT',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandEXAMINE(value: IMAPTBasicEvent);
Begin
   fOnCommandEXAMINE:=Value;
   AddBasicEvent('EXAMINE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandCREATE(value: IMAPTBasicEvent);
Begin
   fOnCommandCREATE:=Value;
   AddBasicEvent('CREATE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandDELETE(value: IMAPTBasicEvent);
Begin
   fOnCommandDELETE:=Value;
   AddBasicEvent('DELETE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandRENAME(value: IMAPTComplexEvent);
Begin
   fOnCommandRENAME:=Value;
   AddComplexEvent('RENAME',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandSUBSCRIBE(value: IMAPTBasicEvent);
Begin
   fOnCommandSUBSCRIBE:=Value;
   AddBasicEvent('SUBSCRIBE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandUNSUBSCRIBE(value: IMAPTBasicEvent);
Begin
   fOnCommandUNSUBSCRIBE:=Value;
   AddBasicEvent('UNSUBSCRIBE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandLIST(value: IMAPTComplexEvent);
Begin
   fOnCommandLIST:=Value;
   AddComplexEvent('LIST',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandLSUB(value: IMAPTComplexEvent);
Begin
   fOnCommandLSUB:=Value;
   AddComplexEvent('LSUB',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandSTATUS(value: IMAPTComplexEvent);
Begin
   fOnCommandSTATUS:=Value;
   AddComplexEvent('STATUS',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandAPPEND(value: IMAPTComplexEvent);
Begin
   fOnCommandAPPEND:=Value;
   AddComplexEvent('APPEND',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandCHECK(value: IMAPTSimpleEvent);
Begin
   fOnCommandCHECK:=Value;
   AddSimpleEvent('CHECK',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandCLOSE(value: IMAPTSimpleEvent);
Begin
   fOnCommandCLOSE:=Value;
   AddSimpleEvent('CLOSE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandEXPUNGE(value: IMAPTSimpleEvent);
Begin
   fOnCommandEXPUNGE:=Value;
   AddSimpleEvent('EXPUNGE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandSEARCH(value: IMAPTComplexEvent);
Begin
   fOnCommandSEARCH:=Value;
   AddComplexEvent('SEARCH',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandFETCH(value: IMAPTComplexEvent);
Begin
   fOnCommandFETCH:=Value;
   AddComplexEvent('FETCH',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandSTORE(value: IMAPTComplexEvent);
Begin
   fOnCommandSTORE:=Value;
   AddComplexEvent('STORE',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandCOPY(value: IMAPTComplexEvent);
Begin
   fOnCommandCOPY:=Value;
   AddComplexEvent('COPY',Value);
End;

Procedure TDXIMAP4ServerCore.SetOnCommandUID(value: IMAPTComplexEvent);
Begin
   fOnCommandUID:=Value;
   AddComplexEvent('UID',Value);
End;

procedure TDXIMAP4ServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Begin
   If Header<>'' then ClientThread.Socket.Writeln('* OK '+Header)
   Else ClientThread.Socket.Writeln('* OK IMAP4 Server (ready).');
End;

procedure TDXIMAP4ServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then ClientThread.Socket.Writeln('* BYE '+Footer)
   Else ClientThread.Socket.Writeln('* BYE Goodbye.');
End;

procedure TDXIMAP4ServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, sTag, sCmd: string;
  WasHandled: Boolean;
  Loop:Integer;
  OutData:Pointer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         If fbForceAbort then Exit;
         S:=ReadLn(Timeout);
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
         sTag:=UpperCase(Fetch(S,#32,False));
         sCmd:=UpperCase(Fetch(S,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PIMAPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PIMAPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PIMAPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       IMAPTBasicEvent(PIMAPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread, sTag, S);
                  2:if Assigned(PIMAPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       IMAPTSimpleEvent(PIMAPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread, sTag);
                  3:if Assigned(PIMAPComplexEvent(fEventArray[Loop]).EventProcedure) then
                       IMAPTComplexEvent(PIMAPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread, sTag, Fetch(S,#32,False), S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If sCMD='QUIT' then Exit;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread, sTag, sCmd, s, WasHandled);
         end;
         if not WasHandled then
            Writeln('500 command not recognized (' + sCMD + ')');
      end;
   end;
end;

end.

