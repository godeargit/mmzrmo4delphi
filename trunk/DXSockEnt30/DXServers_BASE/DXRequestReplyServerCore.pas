unit DXRequestReplyServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXRequestReplyServerCore
//       Author: David Weisbrod (davidw@macrosysinc.com) - 2000/08/31
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
//  Description: implements MacroSys's Object Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   Classes,
   DXServerCore;

{$I DXSock.def}

type
   RequestReplyTBasicEvent=procedure (ClientThread:TDXClientThread;
      Parm1:string) of object;
   RequestReplyTSimpleEvent=procedure (ClientThread:TDXClientThread) of object;
   RequestReplyTComplexEvent=procedure (ClientThread:TDXClientThread;
      Parm1,Parm2:string) of object;

   TDXRequestReplyServerCore=class (TDXServerCore)
   private
   protected
   public
      constructor Create (AOwner:TComponent) ;
{$IFNDEF OBJECTS_ONLY}override;
{$ENDIF}
      destructor Destroy;override;
      procedure ProcessSession (ClientThread:TDXClientThread) ;
      procedure AddBasicEvent (Command:string;EventProc:RequestReplyTBasicEvent) ;
      procedure AddSimpleEvent (Command:string;EventProc:RequestReplyTSimpleEvent) ;
      procedure AddComplexEvent (Command:string;EventProc:RequestReplyTComplexEvent) ;
   published
{
    property OnLogon: RequestReplyTSimpleEvent
      read fOnLogon
      write fOnLogon;
    property OnLogoff: RequestReplyTSimpleEvent
      read fOnLogoff
      write fOnLogoff;
    property OnUpload: RequestReplyTSimpleEvent
      read fOnUpload
      write fOnUpload;
    property OnDownload: RequestReplyTSimpleEvent
      read fOnDownload
      write fOnDownload;
    property OnCommand: RequestReplyTBasicEvent
      read fOnCommand
      write fOnCommand;
}
   end;

const
   PACKET_SIZE=2048;
   MAX_TIMEOUT=120000;
  // Requests
   LOGON_REQUEST='LogonRequest';
   LOGOFF_REQUEST='LogoffRequest';
   COMMAND_REQUEST='CommandRequest';
   UPLOAD_REQUEST='UploadRequest';
   DOWNLOAD_REQUEST='DownloadRequest';

implementation

uses
   DXSock,
   Sysutils,
   DXString;

type
   PRequestReplyBasicEvent=^TRequestReplyBasicEvent;
   TRequestReplyBasicEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:RequestReplyTBasicEvent;
   end;
   PRequestReplySimpleEvent=^TRequestReplySimpleEvent;
   TRequestReplySimpleEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:RequestReplyTSimpleEvent;
   end;
   PRequestReplyComplexEvent=^TRequestReplyComplexEvent;
   TRequestReplyComplexEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:RequestReplyTComplexEvent;
   end;

constructor TDXRequestReplyServerCore.Create (AOwner:TComponent) ;
begin
   inherited Create (AOwner) ;
   ServerPort:=6160;
end;

destructor TDXRequestReplyServerCore.Destroy;
var
   PBasicEvent:PRequestReplyBasicEvent;
   PSimpleEvent:PRequestReplySimpleEvent;
   PComplexEvent:PRequestReplyComplexEvent;

begin
   if Assigned (fEventArray) then begin
      while fEventArray.Count>0 do begin
         case PRequestReplyBasicEvent (fEventArray[0]) .Tag of
            1:begin
                  PBasicEvent:=fEventArray[0];
                  Dispose (PBasicEvent) ;
               end;
            2:begin
                  PSimpleEvent:=fEventArray[0];
                  Dispose (PSimpleEvent) ;
               end;
            3:begin
                  PComplexEvent:=fEventArray[0];
                  Dispose (PComplexEvent) ;
               end;
         end;
         fEventArray.Delete (0) ;
      end;
   end;

   inherited Destroy;
end;

procedure TDXRequestReplyServerCore.AddBasicEvent (Command:string;EventProc:RequestReplyTBasicEvent) ;
var
   PBasicEvent:PRequestReplyBasicEvent;
   Loop:Integer;

begin
   Command:=Uppercase (Command) ;
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PRequestReplyBasicEvent (fEventArray[Loop]) .Command=Command then begin
         PRequestReplyBasicEvent (fEventArray[Loop]) .EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc (Loop) ;
   end;
   New (PBasicEvent) ;
   PBasicEvent.Tag:=1;// Denotes Event in fEventArray is a TBasicEvent!
   PBasicEvent.Command:=Command;
   PBasicEvent.EventProcedure:=EventProc;
   fEventArray.Add (PBasicEvent) ;
end;

procedure TDXRequestReplyServerCore.AddSimpleEvent (Command:string;EventProc:RequestReplyTSimpleEvent) ;
var
   PSimpleEvent:PRequestReplySimpleEvent;
   Loop:Integer;

begin
   Command:=Uppercase (Command) ;
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PRequestReplySimpleEvent (fEventArray[Loop]) .Command=Command then begin
         PRequestReplySimpleEvent (fEventArray[Loop]) .EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc (Loop) ;
   end;
   New (PSimpleEvent) ;
   PSimpleEvent.Tag:=2;// Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add (PSimpleEvent) ;
end;

procedure TDXRequestReplyServerCore.AddComplexEvent (Command:string;EventProc:RequestReplyTComplexEvent) ;
var
   PComplexEvent:PRequestReplyComplexEvent;
   Loop:Integer;

begin
   Command:=Uppercase (Command) ;
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PRequestReplyComplexEvent (fEventArray[Loop]) .Command=Command then begin
         PRequestReplyComplexEvent (fEventArray[Loop]) .EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc (Loop) ;
   end;
   New (PComplexEvent) ;
   PComplexEvent.Tag:=3;// Denotes Event in fEventArray is a TComplexEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add (PComplexEvent) ;
end;

procedure TDXRequestReplyServerCore.ProcessSession (ClientThread:TDXClientThread) ;
var
   s,sCmd:string;
   WasHandled:Boolean;
   Loop:Integer;
   OutData:Pointer;

begin
   with ClientThread.Socket do begin
      while connected do begin
         if fbForceAbort then Exit;
         S:=ReadLn (Timeout) ;
         if LastReadTimeout then Exit;
         if not ValidSocket then Exit;
         if s='' then continue;
         if Assigned (OnFilter) then begin
            Loop:=FilterRead (@S[1],OutData,Length (S) ,ClientThread) ;
            SetLength (S,Loop) ;
            if Assigned (OutData) then begin
               Move (TDXBSArray (OutData^) ,S[1],Loop) ;
               OnFilter (ddFreePointer,nil,OutData,Loop,Loop,WasHandled,ClientThread) ;
            end;
         end;
         sCmd:=UpperCase (Fetch (S,#32,False) ) ;
         Loop:=0;
         WasHandled:=False;
         while (Loop<fEventArray.Count) and (not WasHandled) do begin
            if PRequestReplyBasicEvent (fEventArray[Loop]) .Command=sCMD then begin
               case PRequestReplyBasicEvent (fEventArray[Loop]) .Tag of
                  1:
                     if Assigned (PRequestReplyBasicEvent (fEventArray[Loop]) .EventProcedure) then begin
                        WriteLn ('OK') ;
                        RequestReplyTBasicEvent (PRequestReplyBasicEvent (fEventArray[Loop]) .EventProcedure) (ClientThread,S) ;
                        WasHandled:=True;
                     end;
                  2:
                     if Assigned (PRequestReplySimpleEvent (fEventArray[Loop]) .EventProcedure) then begin
                        WriteLn ('OK') ;
                        RequestReplyTSimpleEvent (PRequestReplySimpleEvent (fEventArray[Loop]) .EventProcedure) (ClientThread) ;
                        WasHandled:=True;
                     end;
                  3:
                     if Assigned (PRequestReplyComplexEvent (fEventArray[Loop]) .EventProcedure) then begin
                        WriteLn ('OK') ;
                        RequestReplyTComplexEvent (PRequestReplyComplexEvent (fEventArray[Loop]) .EventProcedure) (ClientThread,Fetch (S,#32,False) ,S) ;
                        WasHandled:=True;
                     end;
               end;
            end
            else begin
               Inc (Loop) ;
            end;
         end;// while
         if not WasHandled then begin
            Writeln ('500 command not recognized ('+sCMD+')') ;
         end;
      end;// while
   end;// with
end;

end.

