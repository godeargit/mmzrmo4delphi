unit DXFileDataQueue;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFileDataQueue
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
///////////////////////////////////////////////////////////////////////////////

uses
   DXWebFat,
   Winsock,
   SyncObjs,
   Classes;

{$I DXAddons.def}

type
   //  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, pTimeCritical);
   PDXFileDataQueueCache=^TDXFileDataQueueCache;
   TDXFileDataQueueCache=record
      Sock:u_int;
      PathFileName:string;
      TextFile:Boolean;
   end;
   {$IFDEF OBJECTS_ONLY}
   TDXFileDataQueue=class
      {$ELSE}
   TDXFileDataQueue=class(TComponent)
      {$ENDIF}
   private
      MyCriticalSection:TCriticalSection;
      FPriority:TThreadPriority;
      bStop:Boolean;
      bRunning:Boolean;
      FEnabled:Boolean;
      fCache:TList;
      procedure setEnabled(b:boolean);
   protected
      procedure Start;
      procedure Stop;
      procedure DeleteFirstFromQueue;
   public
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure AddToBinaryQueue(Sock:u_int; PathFileName:string);
      procedure AddToTextFileQueue(Sock:u_int; PathFileName:string);
   published
      property ThreadPriority:TThreadPriority read FPriority write FPriority
         default tpNormal;
   end;

implementation

uses
   DXSocket,
   DXString;

type
   TTimerThread=class(TThread)
   private
      TT:TDXFileDataQueue;
   protected
   public
      constructor CreateTimerThread(TT:TDXFileDataQueue);
      procedure Execute; override;
   end;

   {$IFDEF OBJECTS_ONLY}

constructor TDXFileDataQueue.Create;
{$ELSE}

constructor TDXFileDataQueue.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
   FPriority:=tpNormal;
   fCache:=nil;
   MyCriticalSection:=TCriticalSection.Create;
end;

destructor TDXFileDataQueue.Destroy;
var
   DXFileDataQueueCache:PDXFileDataQueueCache;

begin
   SetEnabled(False);
   if Assigned(fCache) then begin
      while fCache.Count>0 do begin
         DXFileDataQueueCache:=fCache[0];
         Dispose(DXFileDataQueueCache);
      end;
      fCache.Free;
      fCache:=nil;
   end;
   MyCriticalSection.Free;
   inherited Destroy;
end;

procedure TDXFileDataQueue.AddToBinaryQueue(Sock:u_int; PathFileName:string);
var
   DXFileDataQueueCache:PDXFileDataQueueCache;

begin
   New(DXFileDataQueueCache);
   try
      DXFileDataQueueCache^.Sock:=Sock;
      DXFileDataQueueCache^.PathFileName:=PathFileName;
      DXFileDataQueueCache^.TextFile:=False;
      if not Assigned(fCache) then fCache:=TList.Create;
      MyCriticalSection.Acquire;
      fCache.Add(DXFileDataQueueCache);
      MyCriticalSection.Release;
   except
      Dispose(DXFileDataQueueCache);
   end;
   SetEnabled(True);
end;

procedure TDXFileDataQueue.AddToTextFileQueue(Sock:u_int; PathFileName:string);
var
   DXFileDataQueueCache:PDXFileDataQueueCache;

begin
   MyCriticalSection.Acquire;
   try
      New(DXFileDataQueueCache);
      DXFileDataQueueCache^.Sock:=Sock;
      DXFileDataQueueCache^.PathFileName:=PathFileName;
      DXFileDataQueueCache^.TextFile:=True;
      if not Assigned(fCache) then fCache:=TList.Create;
      fCache.Add(DXFileDataQueueCache);
   except
   end;
   MyCriticalSection.Release;
   SetEnabled(True);
end;

procedure TDXFileDataQueue.DeleteFirstFromQueue;
var
   DXFileDataQueueCache:PDXFileDataQueueCache;

begin
   MyCriticalSection.Acquire;
   try
      DXFileDataQueueCache:=fCache[0];
      Dispose(DXFileDataQueueCache);
      fCache.Delete(0);
   except
   end;
   MyCriticalSection.Release;
end;

///////////////////////////////////////////////////////////////////////////////
// Changing the Enabled property calls either Start or Stop protected methods.
///////////////////////////////////////////////////////////////////////////////

procedure TDXFileDataQueue.setEnabled(b:boolean);
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

procedure TDXFileDataQueue.Start;
begin
   if bRunning then Exit;
   bStop:=false;
   with TTimerThread.CreateTimerThread(self) do begin
      Resume;
   end;
   bRunning:=true;
end;

///////////////////////////////////////////////////////////////////////////////
// Stopping the timer just sets the stop flag to true, the TTimerThread's
// Execute method will then end and the thread will be destroyed.
///////////////////////////////////////////////////////////////////////////////

procedure TDXFileDataQueue.Stop;
begin
   bStop:=true;
   bRunning:=false;
end;

///////////////////////////////////////////////////////////////////////////////
// Create the thread for the timer, self contained.
///////////////////////////////////////////////////////////////////////////////

constructor TTimerThread.CreateTimerThread(TT:TDXFileDataQueue);
begin
   inherited Create(true);
   self.tt:=tt;
   FreeOnTerminate:=true;
end;

///////////////////////////////////////////////////////////////////////////////
// Execute method for the spawned thread. Just repeats while the timer is
// enabled, and calls the timer object's OnTimer event.
///////////////////////////////////////////////////////////////////////////////

procedure TTimerThread.Execute;
var
   DXFileDataQueueCache:PDXFileDataQueueCache;
   ErrCode:Integer;
   TFH:TextFile;
   BFH:file;
   Ws:string;

begin
   while not tt.bStop do begin
      if tt.fCache.Count>0 then begin
         DXFileDataQueueCache:=tt.fCache[0];
         if DXFileDataQueueCache^.TextFile then begin
            AssignFile(TFH, DXFileDataQueueCache^.PathFileName);
            {$I-}Reset(TFH); {$I+}
            while not Eof(TFH) do begin
               {$I-}Readln(TFH, Ws); {$I+}
               if IOResult=0 then begin
                  Ws:=Ws+#13#10;
                  BasicSend(DXFileDataQueueCache^.Sock,
                     Ws[1], Length(Ws), 0, ErrCode);
                  ProcessWindowsMessageQueue;
               end;
            end;
            CloseFile(TFH);
         end
         else begin
            AssignFile(BFH, DXFileDataQueueCache^.PathFileName);
            {$I-}Reset(BFH, 1); {$I+}
            while not Eof(BFH) do begin
               SetLength(Ws, 2048);
               {$I-}BlockRead(BFH, Ws[1], 2048, ErrCode); {$I+}
               if IOResult=0 then begin
                  if ErrCode<>2048 then Setlength(Ws, ErrCode);
                  if Ws<>'' then
                     BasicSend(DXFileDataQueueCache^.Sock,
                        Ws[1], Length(Ws), 0, ErrCode);
                  ProcessWindowsMessageQueue;
               end;
            end;
            CloseFile(BFH);
         end;
         tt.DeleteFirstFromQueue;
      end;
      tt.bStop:=tt.fCache.Count=0;
   end;
   tt.bRunning:=False;
   Terminate;
end;

end.

