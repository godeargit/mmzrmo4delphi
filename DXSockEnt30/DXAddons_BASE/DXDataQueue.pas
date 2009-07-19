unit DXDataQueue;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDataQueue
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
///////////////////////////////////////////////////////////////////////////////

uses
   Winsock,
   DXString,
   Classes;

{$I DXAddons.def}

type
   //  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, pTimeCritical);
   PDXDataQueueCache=^TDXDataQueueCache;
   TDXDataQueueCache=record
      Sock:u_int;
      Buffer:Pointer;
      Size:Integer;
   end;
   TDXDataQueue=class(TDXComponent)
   private
      FPriority:TThreadPriority;
      bStop:Boolean;
      bRunning:Boolean;
      FEnabled:Boolean;
      fCache:TList;
      fQueuedPackets:Integer;
      procedure setEnabled(b:boolean);
   protected
      procedure Start;
      procedure Stop;
      procedure DeleteFirstFromQueue;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure AddToQueue(Sock:u_int; Buf:Pointer; Size:Integer);
   published
      property ThreadPriority:TThreadPriority read FPriority
         write FPriority default tpNormal;
      property isDelivering:Boolean read brunning;
      property QueuedPackets:Integer read fQueuedPackets;
   end;

implementation

uses
   DXSocket;

type
   TDataPumpThread=class(TThread)
   private
      TT:TDXDataQueue;
   protected
   public
      constructor CreateTimerThread(TT:TDXDataQueue);
      procedure Execute; override;
   end;

constructor TDXDataQueue.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FPriority:=tpNormal;
   fCache:=nil;
end;

destructor TDXDataQueue.Destroy;
var
   DXDataQueueCache:PDXDataQueueCache;

begin
   SetEnabled(False);
   if Assigned(fCache) then begin
      while fCache.Count>0 do begin
         DXDataQueueCache:=fCache[0];
         FreeMem(DXDataQueueCache^.Buffer, DXDataQueueCache^.Size);
         Dispose(DXDataQueueCache);
         fCache.Delete(0);
      end;
      fCache.Free;
      fCache:=nil;
   end;
   inherited Destroy;
end;

procedure TDXDataQueue.AddToQueue(Sock:u_int; Buf:Pointer; Size:Integer);
var
   DXDataQueueCache:PDXDataQueueCache;

begin
   if Size<1 then Exit;
   New(DXDataQueueCache);
   try
      DXDataQueueCache^.Sock:=Sock;
      GetMem(DXDataQueueCache^.Buffer, Size);
      Move(Buf^, DXDataQueueCache^.Buffer^, Size);
      DXDataQueueCache^.Size:=Size;
      if not Assigned(fCache) then fCache:=TList.Create;
      MyCriticalSection.StartingWrite;
      try
         fCache.Add(DXDataQueueCache);
         fQueuedPackets:=fCache.Count;
      except
      end;
      MyCriticalSection.FinishedWrite;
   except
      dispose(DXDataQueueCache);
   end;
   SetEnabled(True);
end;

procedure TDXDataQueue.DeleteFirstFromQueue;
var
   DXDataQueueCache:PDXDataQueueCache;

begin
   MyCriticalSection.StartingWrite;
   try
      DXDataQueueCache:=fCache[0];
      FreeMem(DXDataQueueCache^.Buffer, DXDataQueueCache^.Size);
      Dispose(DXDataQueueCache);
   except
   end;
   fCache.Delete(0);
   fQueuedPackets:=fCache.Count;
   MyCriticalSection.FinishedWrite;
end;

///////////////////////////////////////////////////////////////////////////////
// Changing the Enabled property calls either Start or Stop protected methods.
///////////////////////////////////////////////////////////////////////////////

procedure TDXDataQueue.setEnabled(b:boolean);
begin
   case B of
      True:
         Start
   else
      Stop
   end;
   FEnabled:=bRunning;
end;

///////////////////////////////////////////////////////////////////////////////
// Starting the timer creates an instance of TTimerThread and launches the
// thread.
///////////////////////////////////////////////////////////////////////////////

procedure TDXDataQueue.Start;
begin
   if bRunning then Exit;
   bStop:=false;
   {$IFNDEF OBJECTS_ONLY}
   if not(csDesigning in ComponentState) then begin
      {$ENDIF}
      with TDataPumpThread.CreateTimerThread(self) do begin
         Priority:=tt.FPriority;
         Resume;
      end;
      {$IFNDEF OBJECTS_ONLY}
   end;
   {$ENDIF}
   bRunning:=true;
end;

procedure TDXDataQueue.Stop;
begin
   bStop:=true;
   bRunning:=false;
end;

constructor TDataPumpThread.CreateTimerThread(TT:TDXDataQueue);
begin
   inherited Create(true);
   self.tt:=tt;
   FreeOnTerminate:=true;
end;

procedure TDataPumpThread.Execute;
var
   DXDataQueueCache:PDXDataQueueCache;
   ErrCode:Integer;

begin
   while not tt.bStop do begin
      if tt.fCache.Count>0 then begin
         DXDataQueueCache:=tt.fCache[0];
         if Assigned(DXDataQueueCache) then
            BasicSend(DXDataQueueCache^.Sock, DXDataQueueCache^.Buffer^,
               DXDataQueueCache^.Size, 0, ErrCode);
         tt.DeleteFirstFromQueue;
      end
      else
         DoSleepEx(1);
      tt.bStop:=tt.fCache.Count=0;
   end;
   tt.bRunning:=False;
   Terminate;
end;

end.

