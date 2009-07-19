unit DXMessageBaseServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXMessageBaseServerCore
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
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
//  Description: *Example* Of how to build a centralized MessageBase Server
// ========================================================================
// MessageBase Listener, Top Layer descendant of DXserverCore, modified to
// support different message base formats for our server components. You can
// use it as it stands and support NNTP, SMTP, POP3, IMAP4 etc. Or you can
// add your own hooks to expand to also support proprietary messaging systems!
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

Type
  MessageBaseTBasicEvent = procedure(ClientThread:TDXClientThread;Parm1:string) of object;
  MessageBaseTRangeEvent = procedure(ClientThread:TDXClientThread;First,Last:string) of object;
  MessageBaseTSimpleEvent = procedure(ClientThread:TDXClientThread) of object;
  MessageBaseTOtherEvent = procedure(ClientThread:TDXClientThread;Command,Parms:String;var Handled:Boolean) of object;

  TDXMessageBaseServerCore = class(TDXServerCore)
  private
    fOnCommandLOGIN:MessageBaseTBasicEvent;       // LOGIN ACCOUNT PASSWORD}
    fOnCommandFORMAT:MessageBaseTBasicEvent;      // FORMAT [NNTP|EMAIL]}
    fOnCommandMESSAGES:MessageBaseTSimpleEvent;   // MESSAGES}
    fOnCommandGETMSG:MessageBaseTBasicEvent;      // GETMSG NUMBER}
    fOnCommandDELMSG:MessageBaseTBasicEvent;      // DELMSG NUMBER}
    fOnCommandADDMSG:MessageBaseTSimpleEvent;     // ADDMSG}
    fOnCommandMSGHDRS:MessageBaseTRangeEvent;     // MSGHDRS LONUMBER HINUMBER}
    fOnCommandMSGBODY:MessageBaseTBasicEvent;     // MSGBODY NUMBER}
    fOnCommandEDITMSG:MessageBaseTBasicEvent;     // EDITMSG NUMBER}
    fOnCommandRESCAN:MessageBaseTSimpleEvent;     // RESCAN}
    fOnCommandAREAS:MessageBaseTSimpleEvent;      // AREAS}
    fOnCommandNEWAREA:MessageBaseTBasicEvent;     // NEWAREA FULLFOLDERNAME}
    fOnCommandDELAREA:MessageBaseTBasicEvent;     // DELAREA FULLFOLDERNAME}
    fOnCommandGETAREA:MessageBaseTBasicEvent;     // GETAREA FULLFOLDERNAME}
    fOnCommandHELP:MessageBaseTSimpleEvent;       // HELP}
    fOnCommandOther:MessageBaseTOtherEvent;
    fEventArray:TList;
    fiTimeout:Cardinal;
    fbForceAbort:Boolean;
  protected
    Procedure SetOnCommandLOGIN(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandFORMAT(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandMESSAGES(value:MessageBaseTSimpleEvent);
    Procedure SetOnCommandGETMSG(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandDELMSG(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandADDMSG(value:MessageBaseTSimpleEvent);
    Procedure SetOnCommandMSGHDRS(value:MessageBaseTRangeEvent);
    Procedure SetOnCommandMSGBODY(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandEDITMSG(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandRESCAN(value:MessageBaseTSimpleEvent);
    Procedure SetOnCommandAREAS(value:MessageBaseTSimpleEvent);
    Procedure SetOnCommandNEWAREA(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandDELAREA(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandGETAREA(value:MessageBaseTBasicEvent);
    Procedure SetOnCommandHELP(value:MessageBaseTSimpleEvent);
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:MessageBaseTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:MessageBaseTSimpleEvent);
    Procedure AddRangeEvent(Command:String;EventProc:MessageBaseTRangeEvent);
    Procedure ForceAbort;
  published
    property OnCommandLOGIN: MessageBaseTBasicEvent read fOnCommandLOGIN
                                                  write SetOnCommandLOGIN;
    property OnCommandFORMAT: MessageBaseTBasicEvent read fOnCommandFORMAT
                                                  write SetOnCommandFORMAT;
    property OnCommandMESSAGES: MessageBaseTSimpleEvent read fOnCommandMESSAGES
                                                  write SetOnCommandMESSAGES;
    property OnCommandGETMSG: MessageBaseTBasicEvent read fOnCommandGETMSG
                                                  write SetOnCommandGETMSG;
    property OnCommandDELMSG: MessageBaseTBasicEvent read fOnCommandDELMSG
                                                  write SetOnCommandDELMSG;
    property OnCommandADDMSG: MessageBaseTSimpleEvent read fOnCommandADDMSG
                                                  write SetOnCommandADDMSG;
    property OnCommandMSGHDRS: MessageBaseTRangeEvent read fOnCommandMSGHDRS
                                                  write SetOnCommandMSGHDRS;
    property OnCommandMSGBODY: MessageBaseTBasicEvent read fOnCommandMSGBODY
                                                  write SetOnCommandMSGBODY;
    property OnCommandEDITMSG: MessageBaseTBasicEvent read fOnCommandEDITMSG
                                                  write SetOnCommandEDITMSG;
    property OnCommandRESCAN: MessageBaseTSimpleEvent read fOnCommandRESCAN
                                                  write SetOnCommandRESCAN;
    property OnCommandAREAS: MessageBaseTSimpleEvent read fOnCommandAREAS
                                                  write SetOnCommandAREAS;
    property OnCommandNEWAREA: MessageBaseTBasicEvent read fOnCommandNEWAREA
                                                  write SetOnCommandNEWAREA;
    property OnCommandDELAREA: MessageBaseTBasicEvent read fOnCommandDELAREA
                                                  write SetOnCommandDELAREA;
    property OnCommandGETAREA: MessageBaseTBasicEvent read fOnCommandGETAREA
                                                  write SetOnCommandGETAREA;
    property OnCommandHELP: MessageBaseTSimpleEvent read fOnCommandHELP
                                                  write SetOnCommandHELP;
    property OnCommandOther: MessageBaseTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PMessageBaseBasicEvent=^TMessageBaseBasicEvent;
  TMessageBaseBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MessageBaseTBasicEvent;
  End;
  PMessageBaseSimpleEvent=^TMessageBaseSimpleEvent;
  TMessageBaseSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MessageBaseTSimpleEvent;
  End;
  PMessageBaseRangeEvent=^TMessageBaseRangeEvent;
  TMessageBaseRangeEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:MessageBaseTRangeEvent;
  End;

///////////////////////////////////////////////////////////////////////////////
// COMMANDS INTERNAL TO THE SYSTEM
//   LOGIN    {server to server login - checks deny list}
//   FORMAT   {selects the messagebase format to use}
//   MESSAGES {number of messages available}
//   GETMSG   {download message number}
//   DELMSG   {delete message number}
//   ADDMSG   {upload message}
//   MSGHDRS  {returns a range of message headers - based upon FORMAT command}
//   MSGBODY  {returns a selected message body}
//   EDITMSG  {expects a message body to replace an existing msgbody}
//   RESCAN   {report changes from other threads/sessions}
//   AREAS    {report folders available - based upon FORMAT command}
//   NEWAREA  {creates a new folder if allowed}
//   DELAREA  {deletes a folder if allowed}
//   GETAREA  {selects a folder to access - if allowed}
//   HELP     {sends a list of "events/keywords" from fEventArray}
//
//   TRANSACTION MARKERS FOR COMMUNICATIONS:
//   200         {sucessful command - server is read/write mode}
//   201         {sucessful command - server is readonly}
//   202         {NEXT PARAMETER IS AN INTERGER (can have multple integers)}
//   205 <bytes> {server sending 'x bytes' to client}
//   206 <lines> {server sending 'x lines' to client}
//   500         {command failed - with a reason message}
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//CREATE:
//       Define the Default Port number to Listen On.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OBJECTS_ONLY}
constructor TDXMessageBaseServerCore.Create;
{$ELSE}
constructor TDXMessageBaseServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=9001;
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////
destructor TDXMessageBaseServerCore.Destroy;
Var
   PBasicEvent:PMessageBaseBasicEvent;
   PSimpleEvent:PMessageBaseSimpleEvent;
   PRangeEvent:PMessageBaseRangeEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PMessageBaseBasicEvent(fEventArray[0]).Tag of
            1:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            2:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
            3:Begin
              PRangeEvent:=fEventArray[0];
              Dispose(PRangeEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXMessageBaseServerCore.AddBasicEvent(Command:String;EventProc:MessageBaseTBasicEvent);
Var
   PBasicEvent:PMessageBaseBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMessageBaseBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PMessageBaseBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXMessageBaseServerCore.AddSimpleEvent(Command:String;EventProc:MessageBaseTSimpleEvent);
Var
   PSimpleEvent:PMessageBaseSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMessageBaseSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PMessageBaseSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXMessageBaseServerCore.AddRangeEvent(Command:String;EventProc:MessageBaseTRangeEvent);
Var
   PRangeEvent:PMessageBaseRangeEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMessageBaseRangeEvent(fEventArray[Loop]).Command=Command then Begin
         PMessageBaseRangeEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PRangeEvent);
   PRangeEvent.Tag:=3;      // Denotes Event in fEventArray is a TRangeEvent!
   PRangeEvent.Command:=Command;
   PRangeEvent.EventProcedure:=EventProc;
   fEventArray.Add(PRangeEvent);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandLOGIN(value:MessageBaseTBasicEvent);
Begin
   fOnCommandLOGIN:=Value;
   AddBasicEvent('LOGIN',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandFORMAT(value:MessageBaseTBasicEvent);
Begin
   fOnCommandFORMAT:=Value;
   AddBasicEvent('FORMAT',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandMESSAGES(value:MessageBaseTSimpleEvent);
Begin
   fOnCommandMESSAGES:=Value;
   AddSimpleEvent('MESSAGES',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandGETMSG(value:MessageBaseTBasicEvent);
Begin
   fOnCommandGETMSG:=Value;
   AddBasicEvent('GETMSG',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandDELMSG(value:MessageBaseTBasicEvent);
Begin
   fOnCommandDELMSG:=Value;
   AddBasicEvent('DELMSG',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandADDMSG(value:MessageBaseTSimpleEvent);
Begin
   fOnCommandADDMSG:=Value;
   AddSimpleEvent('ADDMSG',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandMSGHDRS(value:MessageBaseTRangeEvent);
Begin
   fOnCommandMSGHDRS:=Value;
   AddRangeEvent('MSGHDRS',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandMSGBODY(value:MessageBaseTBasicEvent);
Begin
   fOnCommandMSGBODY:=Value;
   AddBasicEvent('MSGBODY',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandEDITMSG(value:MessageBaseTBasicEvent);
Begin
   fOnCommandEDITMSG:=Value;
   AddBasicEvent('EDITMSG',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandRESCAN(value:MessageBaseTSimpleEvent);
Begin
   fOnCommandRESCAN:=Value;
   AddSimpleEvent('RESCAN',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandAREAS(value:MessageBaseTSimpleEvent);
Begin
   fOnCommandAREAS:=Value;
   AddSimpleEvent('AREAS',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandNEWAREA(value:MessageBaseTBasicEvent);
Begin
   fOnCommandNEWAREA:=Value;
   AddBasicEvent('NEWAREA',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandDELAREA(value:MessageBaseTBasicEvent);
Begin
   fOnCommandDELAREA:=Value;
   AddBasicEvent('DELAREA',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandGETAREA(value:MessageBaseTBasicEvent);
Begin
   fOnCommandGETAREA:=Value;
   AddBasicEvent('GETAREA',Value);
End;

Procedure TDXMessageBaseServerCore.SetOnCommandHELP(value:MessageBaseTSimpleEvent);
Begin
   fOnCommandHELP:=Value;
   AddSimpleEvent('HELP',Value);
End;

procedure TDXMessageBaseServerCore.ProcessSession(ClientThread:TDXClientThread);
var
  s,sCmd:string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;

  procedure NotHandled(Cmd:String);
  begin
     ClientThread.Socket.Writeln('500 UNKNOWN COMMAND:'+Cmd);
  end;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         if fbForceAbort then Exit;
         s:=Readln(Timeout);
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
         sCmd:=UpperCase(Fetch(S,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PMessageBaseBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PMessageBaseBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PMessageBaseBasicEvent(fEventArray[Loop]).EventProcedure) then
                       MessageBaseTBasicEvent(PMessageBaseBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,s);
                  2:if Assigned(PMessageBaseSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       MessageBaseTSimpleEvent(PMessageBaseSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PMessageBaseRangeEvent(fEventArray[Loop]).EventProcedure) then
                       MessageBaseTRangeEvent(PMessageBaseRangeEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If sCMD='QUIT' then Exit; // 2.0
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,S,WasHandled);
         end;
         if not WasHandled then NotHandled(sCMD);
      end;
   end;
end;

Procedure TDXMessageBaseServerCore.ForceAbort;
Begin
   fbForceAbort:=True;
End;

end.

