unit DXSMTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSMTPServerCore
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
//  Description: implements SMTP (Simple Mail Transport Protocol)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  SMTPTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  SMTPTBasicEvent = procedure(ClientThread: TDXClientThread;Parm:String) of object;
  SMTPTComplexEvent = procedure(ClientThread: TDXClientThread;Parm1,OptionalParm2:String) of object;
  SMTPTOtherEvent = procedure(ClientThread: TDXClientThread;Command:String;Parm:String;Var Handled:Boolean) of object;

  TDXSMTPServerCore = class(TDXServerCore)
  private
    fOnCommandHELO:SMTPTBasicEvent;        {HELO || HELO modem1.myisp.com}
    fOnCommandEHLO:SMTPTBasicEvent;        {ELHO || ELHO modem1.myisp.com}
    fOnCommandRset:SMTPTSimpleEvent;       {RSET}
    fOnCommandMail:SMTPTComplexEvent;       {MAIL FROM: John Doe <jd@myhost.com> SIZE=xxx (optional)}
    fOnCommandRcpt:SMTPTComplexEvent;        {RCPT TO: John Doe <jd@myhost.com>}
    fOnCommandData:SMTPTSimpleEvent;       {DATA}
    fOnCommandQuit:SMTPTSimpleEvent;       {QUIT}
    fOnCommandHelp:SMTPTBasicEvent;        {HELP || HELP [command]}
    fOnCommandSend:SMTPTBasicEvent;        {SEND FROM: John Doe <jd@myhost.com>}
    fOnCommandSaml:SMTPTBasicEvent;        {SAML FROM: John Doe <jd@myhost.com>}
    fOnCommandSoml:SMTPTBasicEvent;        {SOML FROM: John Doe <jd@myhost.com>}
    fOnCommandNoop:SMTPTSimpleEvent;       {NOOP}
    fOnCommandExpn:SMTPTBasicEvent;        {EXPN [string]}
    fOnCommandVrfy:SMTPTBasicEvent;        {VRFY [string]}
    fOnCommandTurn:SMTPTSimpleEvent;       {TURN}
    fOnCommandEtrn:SMTPTBasicEvent;        {ETRN [zone]}
    fOnCommandOther:SMTPTOtherEvent;       {COMMAND parameters...}
  protected
    Procedure SetOnCommandHELO(value:SMTPTBasicEvent);
    Procedure SetOnCommandEHLO(value:SMTPTBasicEvent);
    Procedure SetOnCommandRSET(value:SMTPTSimpleEvent);
    Procedure SetOnCommandMAIL(value:SMTPTComplexEvent);
    Procedure SetOnCommandRCPT(value:SMTPTComplexEvent);
    Procedure SetOnCommandDATA(value:SMTPTSimpleEvent);
    Procedure SetOnCommandQUIT(value:SMTPTSimpleEvent);
    Procedure SetOnCommandHELP(value:SMTPTBasicEvent);
    Procedure SetOnCommandSEND(value:SMTPTBasicEvent);
    Procedure SetOnCommandSAML(value:SMTPTBasicEvent);
    Procedure SetOnCommandSOML(value:SMTPTBasicEvent);
    Procedure SetOnCommandNOOP(value:SMTPTSimpleEvent);
    Procedure SetOnCommandEXPN(value:SMTPTBasicEvent);
    Procedure SetOnCommandVRFY(value:SMTPTBasicEvent);
    Procedure SetOnCommandTURN(value:SMTPTSimpleEvent);
    Procedure SetOnCommandETRN(value:SMTPTBasicEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure SayOK(ClientThread:TDXClientThread;OptionalNote:String);
    procedure SayNotImplemented(ClientThread:TDXClientThread);
    procedure SayRcptUnknown(ClientThread:TDXClientThread);
    procedure SayMailUnknown(ClientThread:TDXClientThread);
    procedure SayReadyForMessage(ClientThread:TDXClientThread);
    procedure SayReceivedMessage(ClientThread:TDXClientThread;OptionalSize:Integer);
{$IFDEF VER100}
    function GetMessage(ClientThread:TDXClientThread;Stream:TStream;MaxSize:Integer):Boolean;
{$ELSE}
    function GetMessage(ClientThread:TDXClientThread;Stream:TStream;MaxSize:Int64):Boolean;
{$ENDIF}
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:SMTPTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:SMTPTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:SMTPTComplexEvent);
  published
    property OnCommandHelo: SMTPTBasicEvent read fOnCommandHelo
                                        write SetOnCommandHelo;
    property OnCommandEhlo: SMTPTBasicEvent read fOnCommandEhlo
                                        write SetOnCommandEhlo;
    property OnCommandRset: SMTPTSimpleEvent read fOnCommandRset
                                         write SetOnCommandRset;
    property OnCommandMail: SMTPTComplexEvent read fOnCommandMail
                                        write SetOnCommandMail;
    property OnCommandRcpt: SMTPTComplexEvent read fOnCommandRcpt
                                        write SetOnCommandRcpt;
    property OnCommandData: SMTPTSimpleEvent read fOnCommandData
                                         write SetOnCommandData;
    property OnCommandQuit: SMTPTSimpleEvent read fOnCommandQuit
                                         write SetOnCommandQuit;
    property OnCommandHelp: SMTPTBasicEvent read fOnCommandHelp
                                        write SetOnCommandHelp;
    property OnCommandSend: SMTPTBasicEvent read fOnCommandSend
                                        write SetOnCommandSend;
    property OnCommandSaml: SMTPTBasicEvent read fOnCommandSaml
                                        write SetOnCommandSaml;
    property OnCommandSoml: SMTPTBasicEvent read fOnCommandSoml
                                        write SetOnCommandSoml;
    property OnCommandNoop: SMTPTSimpleEvent read fOnCommandNoop
                                         write SetOnCommandNoop;
    property OnCommandExpn: SMTPTBasicEvent read fOnCommandExpn
                                        write SetOnCommandExpn;
    property OnCommandVrfy: SMTPTBasicEvent read fOnCommandVrfy
                                        write SetOnCommandVrfy;
    property OnCommandTurn: SMTPTSimpleEvent read fOnCommandTurn
                                         write SetOnCommandTurn;
    property OnCommandEtrn: SMTPTBasicEvent read fOnCommandEtrn
                                         write SetOnCommandEtrn;
    property OnCommandOther: SMTPTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PSMTPBasicEvent=^TSMTPBasicEvent;
  TSMTPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SMTPTBasicEvent;
  End;
  PSMTPSimpleEvent=^TSMTPSimpleEvent;
  TSMTPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SMTPTSimpleEvent;
  End;
  PSMTPComplexEvent=^TSMTPComplexEvent;
  TSMTPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:SMTPTComplexEvent;
  End;

(* from RFC821...
       'HELO', {1}
       'EHLO', {2}
       'RSET', {3}
       'MAIL', {4}
       'RCPT', {5}
       'DATA', {6}
       'QUIT', {7}
       'HELP', {8}
       'SEND', {9}
       'SAML', {10}
       'SOML', {11}
       'NOOP', {12}
       'EXPN', {13}
       'VRFY', {14}
       'TURN', {15}
       'ETRN'  {16}
*)

constructor TDXSMTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=25;
end;

destructor TDXSMTPServerCore.Destroy;
Var
   PBasicEvent:PSMTPBasicEvent;
   PSimpleEvent:PSMTPSimpleEvent;
   PComplexEvent:PSMTPComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PSMTPBasicEvent(fEventArray[0]).Tag of
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

Procedure TDXSMTPServerCore.AddBasicEvent(Command:String;EventProc:SMTPTBasicEvent);
Var
   PBasicEvent:PSMTPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSMTPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PSMTPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSMTPServerCore.AddSimpleEvent(Command:String;EventProc:SMTPTSimpleEvent);
Var
   PSimpleEvent:PSMTPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSMTPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PSMTPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSMTPServerCore.AddComplexEvent(Command:String;EventProc:SMTPTComplexEvent);
Var
   PComplexEvent:PSMTPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PSMTPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PSMTPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXSMTPServerCore.SetOnCommandHELO(value:SMTPTBasicEvent);
Begin
   fOnCommandHELO:=Value;
   AddBasicEvent('HELO',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandEHLO(value:SMTPTBasicEvent);
Begin
   fOnCommandEHLO:=Value;
   AddBasicEvent('EHLO',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandRSET(value:SMTPTSimpleEvent);
Begin
   fOnCommandRSET:=Value;
   AddSimpleEvent('RSET',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandMAIL(value:SMTPTComplexEvent);
Begin
   fOnCommandMAIL:=Value;
   AddComplexEvent('MAIL',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandRCPT(value:SMTPTComplexEvent);
Begin
   fOnCommandRCPT:=Value;
   AddComplexEvent('RCPT',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandDATA(value:SMTPTSimpleEvent);
Begin
   fOnCommandDATA:=Value;
   AddSimpleEvent('DATA',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandQUIT(value:SMTPTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandHELP(value:SMTPTBasicEvent);
Begin
   fOnCommandHELP:=Value;
   AddBasicEvent('HELP',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandSEND(value:SMTPTBasicEvent);
Begin
   fOnCommandSEND:=Value;
   AddBasicEvent('SEND',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandSAML(value:SMTPTBasicEvent);
Begin
   fOnCommandSAML:=Value;
   AddBasicEvent('SAML',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandSOML(value:SMTPTBasicEvent);
Begin
   fOnCommandSOML:=Value;
   AddBasicEvent('SOML',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandNOOP(value:SMTPTSimpleEvent);
Begin
   fOnCommandNOOP:=Value;
   AddSimpleEvent('NOOP',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandEXPN(value:SMTPTBasicEvent);
Begin
   fOnCommandEXPN:=Value;
   AddBasicEvent('EXPN',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandVRFY(value:SMTPTBasicEvent);
Begin
   fOnCommandVRFY:=Value;
   AddBasicEvent('VRFY',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandTURN(value:SMTPTSimpleEvent);
Begin
   fOnCommandTURN:=Value;
   AddSimpleEvent('TURN',Value);
End;

Procedure TDXSMTPServerCore.SetOnCommandETRN(value:SMTPTBasicEvent);
Begin
   fOnCommandETRN:=Value;
   AddBasicEvent('ETRN',Value);
End;

procedure TDXSMTPServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Var
   Ws:String;

Begin
   If Header<>'' then WS:=Header
   Else Ws:='SMTP Server (ready).';
   ClientThread.Socket.WriteResultCode(220,Ws);
End;

procedure TDXSMTPServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Var
   Ws:String;

Begin
   If Footer<>'' then Ws:=Footer
   Else Ws:='Goodbye.';
   ClientThread.Socket.WriteResultCode(221,Ws);
   ClientThread.Socket.CloseGracefully;
End;

procedure TDXSMTPServerCore.SayOK(ClientThread:TDXClientThread;OptionalNote:String);
Var
   Ws:String;

Begin
   If OptionalNote<>'' then Ws:='OK - '+OptionalNote
   Else Ws:='OK';
   ClientThread.Socket.WriteResultCode(250,Ws);
End;

procedure TDXSMTPServerCore.SayNotImplemented(ClientThread:TDXClientThread);
Begin
   ClientThread.Socket.WriteResultCode(502,'command not implemented');
End;

procedure TDXSMTPServerCore.SayRcptUnknown(ClientThread:TDXClientThread);
Begin
   ClientThread.Socket.WriteResultCode(503,'No recipients: need RCPT');
End;

procedure TDXSMTPServerCore.SayMailUnknown(ClientThread:TDXClientThread);
Begin
   ClientThread.Socket.WriteResultCode(503,'No originator: need MAIL');
End;

procedure TDXSMTPServerCore.SayReadyForMessage(ClientThread:TDXClientThread);
Begin
   ClientThread.Socket.WriteResultCode(354,'Send Data. End with CRLF.CRLF');
End;

procedure TDXSMTPServerCore.SayReceivedMessage(ClientThread:TDXClientThread;OptionalSize:Integer);
Begin
   If OptionalSize>0 then SayOk(ClientThread,'')
   Else SayOk(ClientThread,'stored '+IntToCommaStr(OptionalSize)+' octets');
End;

{$IFDEF VER100}
function TDXSMTPServerCore.GetMessage(ClientThread:TDXClientThread;Stream:TStream;MaxSize:Integer):Boolean;
{$ELSE}
function TDXSMTPServerCore.GetMessage(ClientThread:TDXClientThread;Stream:TStream;MaxSize:Int64):Boolean;
{$ENDIF}
Var
   Done:Boolean;

Begin
   SayReadyForMessage(ClientThread);
   With ClientThread.Socket do Begin
      Done:=False;
      While Connected and Not Done do Begin
{$IFDEF VER100}
         Done:=SaveToStream(Stream,Timeout);
{$ELSE}
         Done:=SaveTo(Stream,Timeout);
{$ENDIF}
      End;
   End;
   Result:=Stream.Size>0;
// after 3.0 add - If Result then OnFilter Stream!!
   SayReceivedMessage(ClientThread,Stream.Size);
End;

procedure TDXSMTPServerCore.ProcessSession(ClientThread:TDXClientThread);
var
  s, sCmd: string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;

  procedure NotHandled(CMD: string);
  Var
     Ws:String;

  begin
    if (CMD<>'') then Begin
       Ws:='500 command not recognized ('+CMD+')';
       ClientThread.Socket.Writeln(Ws);
    End;
  end;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while Connected do begin
         if fbForceAbort then Exit;
         s:=ReadLn(Timeout);
         If LastReadTimeout or Not Connected then Exit;
         If LastCommandStatus=10060 then continue
         else if LastCommandStatus<>0 then Exit;
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
            If PSMTPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PSMTPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PSMTPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       SMTPTBasicEvent(PSMTPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PSMTPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       SMTPTSimpleEvent(PSMTPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PSMTPComplexEvent(fEventArray[Loop]).EventProcedure) then begin
                       FetchByChar(S,':',False);
                       sCMD:=FetchByChar(S,#32,False);
                       SMTPTComplexEvent(PSMTPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,sCMD,S);
                  End;
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End; {while}
         If sCMD='QUIT' then Exit; {Terminate}
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then NotHandled(sCmd);
      end; {while}
   end; {with}
end; {doExecute}

end.

