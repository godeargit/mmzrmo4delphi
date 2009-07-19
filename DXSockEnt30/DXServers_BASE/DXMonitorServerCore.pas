unit DXMonitorServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXMonitorServerCore
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2002
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2002
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
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description: *NOT FINISHED IN V3.0 YET*
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXString,
  DXServerCore;

{$I DXSOCK.DEF} {2.0}

type
  MonitorTOtherEvent = procedure(ClientThread: TDXServerThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXMonitorServerCore = class(TDXServerCore)
  private
    fOnCommandOther: MonitorTOtherEvent; {COMMAND parameters...}
    fEventArray:TList;
    fiTimeout:DWord;
  protected
    Procedure SetOnCommandUSER(value:MonitorTSimpleEvent);
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXServerThread);
    Procedure AddBasicEvent(Command:String;EventProc:MonitorTSimpleEvent);
  published
    property Timeout:DWord read fiTimeout
                           write fiTimeout;
    property OnCommandOther: MonitorTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Type
  PMonitorSimpleEvent=^TMonitorSimpleEvent;
  TMonitorSimpleEvent=record
     Tag:Integer;
     Command:MySmallString;
     EventProcedure:MonitorTSimpleEvent;
  End;

(******************************************************************************
CREATE:
       Define the Default Port number to Listen On.
******************************************************************************)
{$IFDEF OBJECTS_ONLY}
constructor TDXMonitorServerCore.Create;
{$ELSE}
constructor TDXMonitorServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=53;
   ProtocolToBind:=wpUDPOnly;
   fEventArray:=TList.Create;
   fiTimeout:=30000;
end;

(******************************************************************************
DESTROY:
        Destory this object.
******************************************************************************)
destructor TDXMonitorServerCore.Destroy;
Var
   PSimpleEvent:PMonitorSimpleEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PMonitorSimpleEvent(fEventArray[0]).Tag of
            1:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
      fEventArray.Free;
      fEventArray:=Nil;
   End;
   inherited Destroy;
end;

(******************************************************************************
ADDSimpleEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddSimpleEvent('CDROM',MySpecialEvent);
******************************************************************************)
Procedure TDXMonitorServerCore.AddBasicEvent(Command:String;EventProc:MonitorTSimpleEvent);
Var
   PSimpleEvent:PMonitorSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PMonitorSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PMonitorSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=1;      // Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXMonitorServerCore.SetOnCommandUSER(value:MonitorTSimpleEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

(******************************************************************************
PROCESSSESSION:
               If you want this CORE to process the parsing, you should call
               this from your "OnNewConnect" implementation. This should be
               right after your call to SayHello (optional).
******************************************************************************)
procedure TDXMonitorServerCore.ProcessSession(ClientThread: TDXServerThread);
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
   with ClientThread.Socket do begin
      s:=ReadLn(fiTimeout);
      If LastReadTimeout or Not ValidSocket then Exit;
      If Assigned(OnFilter) then Begin
         Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
         SetLength(S,Loop);
         If Assigned(OutData) then Begin
            Move(TDXBSArray(OutData^),S[1],Loop);
            OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled);
         End;
      End;
      sCmd:=UpperCase(Fetch(s,#32,False));
      Loop:=0;
      WasHandled:=False;
      While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
         If PMonitorSimpleEvent(fEventArray[Loop]).Command=sCMD then Begin
            Case PMonitorSimpleEvent(fEventArray[Loop]).Tag of
               1:if Assigned(PMonitorSimpleEvent(fEventArray[Loop]).EventProcedure) then
                    MonitorTSimpleEvent(PMonitorSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
            End;
            WasHandled:=True;
         End
         Else Inc(Loop);
      End; {while}
      If Not WasHandled then Begin
         if assigned(OnCommandOther) then
            OnCommandOther(ClientThread,sCmd,s,WasHandled);
      end;
      if not WasHandled then NotHandled(sCmd);
   end; {with}
end; {doExecute}

end.

