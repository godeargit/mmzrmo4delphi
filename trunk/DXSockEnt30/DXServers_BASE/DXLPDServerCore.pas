unit DXLPDServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXLPDServerCore
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
//  Description: implements LPD (Line Printer Daemon) protocol
// ========================================================================
// Tests have shown that HP JetDirect boxes use this protocol. Some people
// refer to this protocol as LPD other refer to it as SPOOLER. The design is
// a print job is sent from remote machines to this service. This service can
// accept or reject the requests. The requests can be to print, to return the
// status of the printer or print job, or delete a print job.
//
// We use this protocol in-house as one router to printers on different
// networks and protocols. All of our machines can see the "Queue", and dump
// output to this service, who in-turn actually spools to the printer. This
// is not a "toy" protocol that you simply through into place. It will require
// a detailed understanding of the protocol, printer output format, and how
// to stream data to the destination printer(s).
//
// RFC: 1179
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def} 

type
  PLPDControlFile=^TLPDControlFile;
  TLPDControlFile=Record
     ClassStr:String[31]; {31 or less}
     Host:String[31];  {31 or less}
     Indent:Integer;
     Job:String[99];   {99 or less}
     UserBanner:String;
     MailResultsTo:String;
     SourceFileName:String[131]; {131 or less}
     UserID:String[31]; {31 or less}
     SymbolicLinkData:String;
     Title:String[79];  {79 or less}
     Unlink:String;        // windows uses this if "clear immediate" is enabled.
     Width:Integer; {default=132}
// font filenames:
     TroffRFont:String;
     TroffIFont:String;
     TroffBFont:String;
     TroffSFont:String;
     PlotCIF:String;
     PrintDVI:String;
     FormattedFile:String;
     PlotFile:String;
     Kerberized:String;
     PrintRAW:String;       // windows uses this!
     DITroff:String;
     PostScript:String;
     PRFormat:String;
     Fortran:String;
     TroffOutput:String;
     Raster:String;
     Palladium:String;
  End;
  LPDTControlFileEvent = procedure(ClientThread:TDXClientThread;ControlFile:PLPDControlFile) of object;
  LPDTSpoolFileEvent = procedure(ClientThread:TDXClientThread;Size:Integer;PathFileName:String) of object;
  LPDTQueueEvent = procedure(ClientThread:TDXClientThread;Queue:String) of object;
  LPDTQueueListEvent = procedure(ClientThread:TDXClientThread;Queue,List:String) of object;
  LPDTQueueRemoveEvent = procedure(ClientThread:TDXClientThread;Queue,Agent,List:String) of object;
  LPDTQueueQueryEvent = procedure(ClientThread:TDXClientThread;Queue:String) of object;
  LPDTOtherEvent = procedure(ClientThread:TDXClientThread;Command:string;Parm:string;Var Handled:Boolean) of object;

  TDXLPDServerCore = class(TDXServerCore)
  private
    fOnCommandPrintWaiting:LPDTQueueEvent;     //$01
    fOnCommandReceiveJob:LPDTQueueEvent;       //$02
    fOnCommandSendJob:LPDTQueueListEvent;      //$03
    fOnCommandQueueQuery:LPDTQueueQueryEvent;  //$04
    fOnCommandRemoveJob:LPDTQueueRemoveEvent;  //$05
    fOnCommandOther: LPDTOtherEvent;
    fOnJobControlFile:LPDTControlFileEvent;    //Sub $02
    fOnJobSpoolFile:LPDTSpoolFileEvent;        //Sub $03
    fSpoolPath:String;
    fControlPath:String;
  protected
    Procedure SetOnCommandPrintWaiting(value:LPDTQueueEvent);
    Procedure SetOnCommandReceiveJob(value:LPDTQueueEvent);
    Procedure SetOnCommandSendJob(value:LPDTQueueListEvent);
    Procedure SetOnCommandQueueQuery(value:LPDTQueueQueryEvent);
    Procedure SetOnCommandRemoveJob(value:LPDTQueueRemoveEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure SayOK(ClientThread:TDXClientThread);
    procedure ProcessReceiveJobSession(ClientThread:TDXClientThread);
    Procedure AddQueueEvent(Command:Char;EventProc:LPDTQueueEvent);
    Procedure AddListEvent(Command:Char;EventProc:LPDTQueueListEvent);
    Procedure AddRemoveEvent(Command:Char;EventProc:LPDTQueueRemoveEvent);
    Procedure AddQueryEvent(Command:Char;EventProc:LPDTQueueQueryEvent);
  published
    property OnCommandPrintWaiting:LPDTQueueEvent read fOnCommandPrintWaiting
                                                  write SetOnCommandPrintWaiting;
    property OnCommandReceiveJob:LPDTQueueEvent read fOnCommandReceiveJob
                                                write SetOnCommandReceiveJob;
    property OnCommandSendJob:LPDTQueueListEvent read fOnCommandSendJob
                                                 write SetOnCommandSendJob;
    property OnCommandQueueQuery:LPDTQueueQueryEvent read fOnCommandQueueQuery
                                                     write SetOnCommandQueueQuery;
    property OnCommandRemoveJob:LPDTQueueRemoveEvent read fOnCommandRemoveJob
                                                     write SetOnCommandRemoveJob;
    property OnCommandOther: LPDTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
// usually implemented to store the pointer in fpSessionData,
// and then call SayOk(ClientThread)
    property OnJobControlFile:LPDTControlFileEvent read fOnJobControlFile
                                                   write fOnJobControlFile;
// must be implemented!
// this is the path/filename for application to push to printer
    property OnJobSpoolFile:LPDTSpoolFileEvent read fOnJobSpoolFile
                                               write fOnJobSpoolFile;
    property InternalSpoolPath:String read fSpoolPath
                                      write fSpoolPath;
    property InternalControlPath:String read fControlPath
                                        write fControlPath;
  end;

implementation

Uses
   DXSock,
   SysUtils,
{$IFNDEF LINUX}
   FileCtrl,
{$ENDIF}
   DXString;

Type
  PLPDQueueEvent=^TLPDQueueEvent;
  TLPDQueueEvent=record
     Tag:Integer;
     Command:Char;
     EventProcedure:LPDTQueueEvent;
  End;
  PLPDListEvent=^TLPDListEvent;
  TLPDListEvent=record
     Tag:Integer;
     Command:Char;
     EventProcedure:LPDTQueueListEvent;
  End;
  PLPDRemoveEvent=^TLPDRemoveEvent;
  TLPDRemoveEvent=record
     Tag:Integer;
     Command:Char;
     EventProcedure:LPDTQueueRemoveEvent;
  End;
  PLPDQueryEvent=^TLPDQueryEvent;
  TLPDQueryEvent=record
     Tag:Integer;
     Command:Char;
     EventProcedure:LPDTQueueQueryEvent;
  End;

constructor TDXLPDServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=515;
   ProtocolToBind:=wpTCPOnly;
   fSpoolPath:='\default\spoolpath\';
   fControlPath:='\default\controlpath\';
end;

destructor TDXLPDServerCore.Destroy;
Var
   PQueueEvent:PLPDQueueEvent;
   PListEvent:PLPDListEvent;
   PRemoteEvent:PLPDRemoveEvent;
   PQueryEvent:PLPDQueryEvent;

begin
   if Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PLPDQueueEvent(fEventArray[0]).Tag of
            1:Begin
              PQueueEvent:=fEventArray[0];
              Dispose(PQueueEvent);
            End;
            2:Begin
              PListEvent:=fEventArray[0];
              Dispose(PListEvent);
            End;
            3:Begin
              PRemoteEvent:=fEventArray[0];
              Dispose(PRemoteEvent);
            End;
            4:Begin
              PQueryEvent:=fEventArray[0];
              Dispose(PQueryEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

(******************************************************************************
ADDQueueEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddQueueEvent('CDROM',MySpecialEvent);
******************************************************************************)
Procedure TDXLPDServerCore.AddQueueEvent(Command:Char;EventProc:LPDTQueueEvent);
Var
   PQueueEvent:PLPDQueueEvent;
   Loop:Integer;

Begin
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PLPDQueueEvent(fEventArray[Loop]).Command=Command then Begin
         PLPDQueueEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PQueueEvent);
   PQueueEvent.Tag:=1;      // Denotes Event in fEventArray is a TSimpleEvent!
   PQueueEvent.Command:=Command;
   PQueueEvent.EventProcedure:=EventProc;
   fEventArray.Add(PQueueEvent);
End;

(******************************************************************************
ADDListEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddListEvent('CDROM',MySpecialEvent);
******************************************************************************)
Procedure TDXLPDServerCore.AddListEvent(Command:Char;EventProc:LPDTQueueListEvent);
Var
   PListEvent:PLPDListEvent;
   Loop:Integer;

Begin
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PLPDListEvent(fEventArray[Loop]).Command=Command then Begin
         PLPDListEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PListEvent);
   PListEvent.Tag:=2;      // Denotes Event in fEventArray is a TSimpleEvent!
   PListEvent.Command:=Command;
   PListEvent.EventProcedure:=EventProc;
   fEventArray.Add(PListEvent);
End;

(******************************************************************************
ADDRemoveEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddRemoveEvent('CDROM',MySpecialEvent);
******************************************************************************)
Procedure TDXLPDServerCore.AddRemoveEvent(Command:Char;EventProc:LPDTQueueRemoveEvent);
Var
   PRemoveEvent:PLPDRemoveEvent;
   Loop:Integer;

Begin
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PLPDRemoveEvent(fEventArray[Loop]).Command=Command then Begin
         PLPDRemoveEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PRemoveEvent);
   PRemoveEvent.Tag:=3;      // Denotes Event in fEventArray is a TSimpleEvent!
   PRemoveEvent.Command:=Command;
   PRemoveEvent.EventProcedure:=EventProc;
   fEventArray.Add(PRemoveEvent);
End;

(******************************************************************************
ADDQueryEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddRemoveEvent('CDROM',MySpecialEvent);
******************************************************************************)
Procedure TDXLPDServerCore.AddQueryEvent(Command:Char;EventProc:LPDTQueueQueryEvent);
Var
   PQueryEvent:PLPDQueryEvent;
   Loop:Integer;

Begin
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PLPDQueryEvent(fEventArray[Loop]).Command=Command then Begin
         PLPDQueryEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PQueryEvent);
   PQueryEvent.Tag:=4;      // Denotes Event in fEventArray is a TSimpleEvent!
   PQueryEvent.Command:=Command;
   PQueryEvent.EventProcedure:=EventProc;
   fEventArray.Add(PQueryEvent);
End;

Procedure TDXLPDServerCore.SetOnCommandPrintWaiting(value:LPDTQueueEvent);
Begin
   fOnCommandPrintWaiting:=Value;
   AddQueueEvent(#1,Value);
End;

Procedure TDXLPDServerCore.SetOnCommandReceiveJob(value:LPDTQueueEvent);
Begin
   fOnCommandReceiveJob:=Value;
   AddQueueEvent(#2,Value);
End;

Procedure TDXLPDServerCore.SetOnCommandSendJob(value:LPDTQueueListEvent);
Begin
   fOnCommandSendJob:=Value;
   AddListEvent(#3,Value);
End;

Procedure TDXLPDServerCore.SetOnCommandQueueQuery(value:LPDTQueueQueryEvent);
Begin
   fOnCommandQueueQuery:=Value;
   AddQueryEvent(#4,Value);
End;

Procedure TDXLPDServerCore.SetOnCommandRemoveJob(value:LPDTQueueRemoveEvent);
Begin
   fOnCommandRemoveJob:=Value;
   AddRemoveEvent(#5,Value);
End;

Procedure TDXLPDServerCore.SayOK(ClientThread:TDXClientThread);
Begin
   ClientThread.Socket.Write(#0);
End;

procedure TDXLPDServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  S:string;
  sCmd:Char;
  Loop:Integer;
  WasHandled:Boolean;

begin
   with ClientThread.Socket do begin
      s:=ReadLn(Timeout);
      If LastReadTimeout or Not ValidSocket then Exit;
      sCmd:=S[1];
      Delete(s,1,1);
      Loop:=0;
      WasHandled:=False;
      While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
         If PLPDQueueEvent(fEventArray[Loop]).Command=sCMD then Begin
            Case PLPDQueueEvent(fEventArray[Loop]).Tag of
               1:if Assigned(PLPDQueueEvent(fEventArray[Loop]).EventProcedure) then
                    LPDTQueueEvent(PLPDQueueEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
               2:if Assigned(PLPDListEvent(fEventArray[Loop]).EventProcedure) then
                    LPDTQueueListEvent(PLPDListEvent(fEventArray[Loop]).EventProcedure)(ClientThread,FetchByChar(S,#32,False),S);
               3:if Assigned(PLPDRemoveEvent(fEventArray[Loop]).EventProcedure) then
                    LPDTQueueRemoveEvent(PLPDRemoveEvent(fEventArray[Loop]).EventProcedure)(ClientThread,FetchByChar(S,#32,False),FetchByChar(S,#32,False),S);
               4:if Assigned(PLPDQueryEvent(fEventArray[Loop]).EventProcedure) then
                    LPDTQueueQueryEvent(PLPDQueryEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
            End;
            WasHandled:=True;
         End
         Else Inc(Loop);
      End; {while}
      If Not WasHandled then Begin
         if assigned(OnCommandOther) then
            OnCommandOther(ClientThread,sCmd,s,WasHandled);
      end;
      If Not WasHandled then ClientThread.Socket.Write(#255);
   end; {with}
end; {doExecute}

procedure TDXLPDServerCore.ProcessReceiveJobSession(ClientThread:TDXClientThread);
var
   S:string;
   sCmd:Char;
   Chars:Integer;
   Ws:String;
   ControlFile:PLPDControlFile;
   Ts:String;
   FHandle:Integer;
   MaxChars:Integer;
   OutData:Pointer;
   Loop:Integer;
   WasHandled:Boolean;

Begin
   with ClientThread.Socket do begin
      s:=ReadLn(Timeout);
      If LastReadTimeout or Not ValidSocket then Exit;
      if S='' then exit;
      If Assigned(OnFilter) then Begin
         Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
         SetLength(S,Loop);
         If Assigned(OutData) then Begin
            Move(TDXBSArray(OutData^),S[1],Loop);
            OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
         End;
      End;
      sCmd:=S[1];
      Delete(s,1,1);
      Case Ord(sCmd) of
         1:Begin // request for abort
            SayOK(ClientThread);
         End;
         2:Begin
            SayOK(ClientThread);
            Chars:=StrToInt(FetchByChar(S,#32,False));
            Ws:=S;
            If fControlPath<>'' then Begin
               ForceDirectories(fControlPath);
               fHandle:=FileCreate(AddBackSlash(fControlPath)+Ws);
            End
            Else FHandle:=-1;
            New(ControlFile);
            While Chars>0 do Begin
               Ts:=Readln(5000);
               If (fControlPath<>'') and (fHandle>-1) then
                  FileWrite(fHandle,Ts[1],Length(Ts));
               Chars:=Chars-(Length(Ts)+1);
               sCmd:=Ts[1];
               Delete(Ts,1,1);
               Case sCmd of
                  'C':ControlFile^.ClassStr:=TS;
                  'H':ControlFile^.Host:=TS;
                  'I':ControlFile^.Indent:=StrToInt(TS);
                  'J':ControlFile^.Job:=TS;
                  'L':ControlFile^.UserBanner:=TS;
                  'M':ControlFile^.MailResultsTo:=TS;
                  'N':ControlFile^.SourceFileName:=TS;
                  'P':ControlFile^.UserID:=TS;
                  'S':ControlFile^.SymbolicLinkData:=TS;
                  'T':ControlFile^.Title:=TS;
                  'U':ControlFile^.Unlink:=TS;
                  'W':ControlFile^.Width:=StrToInt(Ts);
// font filenames:
                  '1':ControlFile^.TroffRFont:=Ts;
                  '2':ControlFile^.TroffIFont:=Ts;
                  '3':ControlFile^.TroffBFont:=Ts;
                  '4':ControlFile^.TroffSFont:=Ts;
                  'c':ControlFile^.PlotCIF:=Ts;
                  'd':ControlFile^.PrintDVI:=Ts;
                  'f':ControlFile^.FormattedFile:=Ts;
                  'g':ControlFile^.PlotFile:=Ts;
                  'k':ControlFile^.Kerberized:=Ts;
                  'l':ControlFile^.PrintRAW:=Ts;
                  'n':ControlFile^.DITroff:=Ts;
                  'o':ControlFile^.PostScript:=Ts;
                  'p':ControlFile^.PRFormat:=Ts;
                  'r':ControlFile^.Fortran:=Ts;
                  't':ControlFile^.TroffOutput:=Ts;
                  'v':ControlFile^.Raster:=Ts;
                  'z':ControlFile^.Palladium:=Ts;
               End;
            End;
            GetChar; {should have been Null}
            If (fControlPath<>'') and (fHandle>-1) then
               FileClose(fHandle);
            If Assigned(fOnJobControlFile) then
               fOnJobControlFile(ClientThread,ControlFile)
            Else
               SayOK(ClientThread); // try to limp along!
         End;
         3:Begin
            SayOK(ClientThread);
            Chars:=StrToInt(FetchByChar(S,#32,False));
            MaxChars:=Chars;
            Ws:=S;
            If fSpoolPath<>'' then Begin
               ForceDirectories(fSpoolPath);
               fHandle:=FileCreate(AddBackSlash(fSpoolPath)+Ws);
            End
            Else FHandle:=-1;
            While Chars>0 do Begin
               Ts:=ReadStr(-1);
               If (fSpoolPath<>'') and (fHandle>-1) then
                  FileWrite(fHandle,Ts[1],Length(Ts));
               ProcessWindowsMessageQueue;
            End;
            If (fSpoolPath<>'') and (fHandle>-1) then
               FileClose(fHandle);
            If Assigned(fOnJobSpoolFile) then
               fOnJobSpoolFile(ClientThread,MaxChars,AddBackSlash(fSpoolPath)+Ws)
            Else
               SayOK(ClientThread); // try to limp along!
         End;
      End;
   End;
End;

end.

