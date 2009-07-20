unit DXUnicastDataQueue;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXUnicastDataQueue
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
//  Description:
// ========================================================================
// Unicast Delivery Component - Self Threaded, background deliver to each socket
// Introduced in 2.3
///////////////////////////////////////////////////////////////////////////////

uses
   Winsock,
   DXString,
   Classes;

{$I DXAddons.def}

type
   //  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, pTimeCritical);
   TDXUnicastDataQueue=class(TDXComponent)
   private
      fArray:TList;
      ArrayCritical:TDXCritical;
      fString:TList;
      StringCritical:TDXCritical;
      FPriority:TThreadPriority;
      bStop:Boolean;
      bRunning:Boolean;
      FEnabled:Boolean;
      FIgnoreOverruns:Boolean;
      procedure setEnabled(b:boolean);
   protected
      procedure Start;
      procedure Stop;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure AddSock(Sock:u_int);
      procedure DelSock(Sock:u_int);
      function KnownSockets:Integer;
      procedure AddToQueue(Buf:string);
      function WaitingQueues:Integer;
   published
      property ThreadPriority:TThreadPriority read FPriority
         write FPriority default tpNormal;
      property IgnoreOverruns:Boolean read FIgnoreOverruns
         write FIgnoreOverruns;
   end;

implementation

uses
   SysUtils,
   DXSocket;

type
   PDXPacket=^TDXPacket;
   TDXPacket=record
      Data:string;
   end;
   PDXClient=^TDXClient;
   TDXClient=record
      SocketHandle:Integer;
   end;

type
   TDataPumpThread=class(TThread)
   private
      TT:TDXUnicastDataQueue;
   protected
   public
      constructor CreateTimerThread(TT:TDXUnicastDataQueue);
      procedure Execute; override;
   end;

constructor TDXUnicastDataQueue.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fArray:=TList.Create;
   fString:=TList.Create;
   ArrayCritical:=TDXCritical.Create;
   StringCritical:=TDXCritical.Create;
   FPriority:=tpNormal;
   FIgnoreOverruns:=True;
   bRunning:=False;
end;

destructor TDXUnicastDataQueue.Destroy;
var
   Packet:PDXPacket;
   Client:PDXClient;

begin
   SetEnabled(False);
   ArrayCritical.StartingWrite;
   while fArray.Count>0 do begin
      Client:=fArray[0];
      Dispose(Client);
      fArray.Delete(0);
   end;
   fArray.Free;
   ArrayCritical.FinishedWrite;
   StringCritical.StartingWrite;
   while fString.Count>0 do begin
      Packet:=fString[0];
      Dispose(Packet);
      fString.Delete(0);
   end;
   fString.Free;
   StringCritical.FinishedWrite;
   inherited Destroy;
end;

procedure TDXUnicastDataQueue.AddSock(Sock:u_int);
var
   Client:PDXClient;

begin
   DelSock(Sock);
   New(Client);
   Client^.SocketHandle:=Sock;
   ArrayCritical.StartingWrite;
   fArray.Add(Client);
   ArrayCritical.FinishedWrite;
end;

procedure TDXUnicastDataQueue.DelSock(Sock:u_int);
var
   Count:Integer;
   Client:PDXClient;

begin
   Count:=fArray.Count;
   while Count>0 do begin
      if PDXClient(fArray[Count-1])^.SocketHandle=Sock then begin
         ArrayCritical.StartingWrite;
         Client:=fArray[Count-1];
         Dispose(Client);
         fArray.Delete(Count-1);
         ArrayCritical.FinishedWrite;
         Exit;
      end;
      Dec(Count);
   end;
end;

function TDXUnicastDataQueue.KnownSockets:Integer;
begin
   Result:=fArray.Count;
end;

procedure TDXUnicastDataQueue.AddToQueue(Buf:string);
var
   Packet:PDXPacket;

begin
   if (Length(Buf)<1)or(fArray.Count<1) then Exit;
   New(Packet);
   Packet^.Data:=Buf;
   StringCritical.StartingWrite;
   fString.Add(Packet);
   StringCritical.FinishedWrite;
   if not bRunning then SetEnabled(True);
end;

function TDXUnicastDataQueue.WaitingQueues:Integer;
begin
   Result:=fString.Count;
end;

///////////////////////////////////////////////////////////////////////////////
// Changing the Enabled property calls either Start or Stop protected methods.
///////////////////////////////////////////////////////////////////////////////
procedure TDXUnicastDataQueue.setEnabled(b:boolean);
begin
   case B of
      True:Start
   else Stop
   end;
   FEnabled:=bRunning;
end;

///////////////////////////////////////////////////////////////////////////////
// Starting the timer creates an instance of TTimerThread and launches the
// thread.
///////////////////////////////////////////////////////////////////////////////
procedure TDXUnicastDataQueue.Start;
begin
   if bRunning then Exit;
   bStop:=false;
   {$IFNDEF OBJECTS_ONLY}
   if not(csDesigning in ComponentState) then begin
      {$ENDIF}
      with TDataPumpThread.CreateTimerThread(self) do begin
         Resume;
      end;
      {$IFNDEF OBJECTS_ONLY}
   end;
   {$ENDIF}
   bRunning:=true;
end;

procedure TDXUnicastDataQueue.Stop;
begin
   bStop:=true;
   bRunning:=false;
end;

constructor TDataPumpThread.CreateTimerThread(TT:TDXUnicastDataQueue);
begin
   inherited Create(true);
   self.tt:=tt;
   FreeOnTerminate:=true;
end;

procedure TDataPumpThread.Execute;
var
   ErrCode:Integer;
   Loop:Integer;
   Client:PDXClient;
   Packet:PDXPacket;

begin
   while not tt.bStop do begin
      while TT.fString.Count>0 do begin
         Packet:=TT.fString[0];
         TT.StringCritical.StartingWrite;
         TT.fString.Delete(0);
         TT.StringCritical.FinishedWrite;
         Loop:=TT.fArray.Count;
         while Loop>0 do begin
            Client:=TT.fArray[Loop-1];
            if TT.FIgnoreOverruns then begin
               BasicSend(Client^.SocketHandle, Packet^.Data[1],
                  Length(Packet^.Data), 0, ErrCode);
               if (ErrCode<>WSAENOBUFS)and
                  (ErrCode<>WSAEWOULDBLOCK)and
                  (ErrCode<>WSAETIMEDOUT)and
                  (ErrCode<>0) then begin
                  TT.DelSock(Client^.SocketHandle);
               end;
            end
            else begin
               ErrCode:=-1;
               while ErrCode<>0 do begin
                  BasicSend(Client^.SocketHandle, Packet^.Data[1],
                     Length(Packet^.Data), 0, ErrCode);
                  if (ErrCode=WSAECONNABORTED)or
                     (ErrCode=WSAECONNRESET) then begin
                     TT.DelSock(Client^.SocketHandle);
                     ErrCode:=0;
                  end
                  else begin
                     if (ErrCode<>WSAENOBUFS)and
                        (ErrCode<>WSAEWOULDBLOCK)and
                        (ErrCode<>WSAETIMEDOUT) then begin
                        ErrCode:=0;
                     end;
                  end;
               end;
            end;
            ProcessWindowsMessageQueue;
            DoSleepEx(1);
            Dec(Loop);
         end;
         Dispose(Packet);
         DoSleepEx(1);
      end;
      ProcessWindowsMessageQueue;
      DoSleepEx(1);
   end;
   tt.bRunning:=False;
   Terminate;
end;

end.
