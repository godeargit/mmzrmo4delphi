unit DXServerCore;

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXServerCore
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
//  Description: Ancestor to all our Protocol Implementations.
// ========================================================================
// Parent Thread Object and "Defacto" Server Object.
// The Server Object (DXServerCore) is the heart of all protocols. It is used
// to initiate winsock listening, and start the Parent Thread
// (DXServerCoreThread). The Parent Thread constantly checks if the listener
// has received a connection request, accepts the connection and spawns a
// (DXServerThread) Session. And continues checking for more connections.
///////////////////////////////////////////////////////////////////////////////

interface

{$I DXSock.DEF}

uses
{$IFDEF CODE_TRACER}
   DXCodeTracer,
{$ENDIF}
   DXSessionTracker,
   DXSock,
   DXString,
   Classes;

type
   TWhichProtocol=(wpUDPOnly,wpTCPOnly);
   TServerType=(stNonBlocking,stThreadBlocking);// 4.0 implemented!

///////////////////////////////////////////////////////////////////////////////
// Events:
///////////////////////////////////////////////////////////////////////////////
   TDXClientThread=class;//forward
   TDX_NewConnect=procedure(ClientThread:TDXClientThread) of object;
{$WARNINGS OFF}
   TDX_DestroySessionData=procedure(ClientThread:TDXClientThread;SessionData:Pointer) of object;
{$WARNINGS ON}

///////////////////////////////////////////////////////////////////////////////
// This thread is the spawned "Session" thread. Every connection detected by
// the "Listener" thread is created as a TDXServerThread, and then that new
// thread takes over communications with the new session.
///////////////////////////////////////////////////////////////////////////////
   TDXClientThread=class(TThread)
   private
{$IFDEF CODE_TRACER}
      CodeTracer:TDXCodeTracer;
{$ENDIF}
      Client:TDXSock;
      feNewConnect:TDX_NewConnect;
      feDestroySessionData:TDX_DestroySessionData;
      fBlockSizeFlags:TDXBlockSizeFlags;
      ListenerThreadObject:TThread;
   protected
      procedure Execute;override;
      function GetSessionID:Integer;// making move from INDY easier!
   public
{$WARNINGS OFF}
      fpSessionData:Pointer;
{$WARNINGS ON}
      constructor Create(CreateSuspended:Boolean);
      destructor Destroy;override;
      procedure SetSocketLater(Socket:TDXSock);
   published
      property SessionID:Integer read GetSessionID;
      property Socket:TDXSock read Client
         write Client;
      property OnNewConnect:TDX_Newconnect read feNewConnect
         write feNewConnect;
      property OnDestroySessionData:TDX_DestroySessionData read feDestroySessionData
         write feDestroySessionData;
{$IFDEF CODE_TRACER}
      property DXCodeTracer:TDXCodeTracer read CodeTracer
         write CodeTracer;
{$ENDIF}
      property Terminated;
   end;

///////////////////////////////////////////////////////////////////////////////
// All protocols use this thread to listen for incoming connections. This is
// the "Listener" thread.
///////////////////////////////////////////////////////////////////////////////
type
   TDXServerCoreThread=class;//forward
   TDX_ListenerFailed=procedure(ErrorCode:Integer) of object;
   TDX_MaxConnects=procedure(ServerCoreThread:TDXServerCoreThread) of object;
   TDX_Idle=procedure(ServerCoreThread:TDXServerCoreThread) of object;
   TDX_Sleep=procedure(ServerCoreThread:TDXServerCoreThread) of object;
   TDX_WakeUp=procedure(ServerCoreThread:TDXServerCoreThread) of object;
{$WARNINGS OFF}
   TDX_UDPData=procedure(Data:Pointer;PeerIP:string;PeerPort,DataLen:Integer) of object;
{$WARNINGS ON}
   TDXServerCoreThread=class(TThread)
   private
{$IFDEF CODE_TRACER}
      CodeTracer:TDXCodeTracer;
{$ENDIF}
      fbSuspend:Boolean;
      fbBufferCreates:Boolean;
      ListenerSocket:TDXSock;
      fSessionTracker:TDXSessionTracker;
      fsBindTo:string;
      fiServerPort:Integer;
      fiMaxConn:Integer;
      fbAnnouncedIdle:Byte; // 0 active, 1 going idle, 2 asleep
      feNewConnect:TDX_NewConnect;
      feMaxConnects:TDX_MaxConnects;
      feListenerFailed:TDX_ListenerFailed;
      feIdle:TDX_Idle;
      feSleep:TDX_Sleep;
      feWakeUp:TDX_WakeUp;
      feUDPData:TDX_UDPData;
{$IFNDEF LINUX}
      fstPriority:TThreadPriority;
{$ENDIF}
      fThreadPool:TList;
      fWhichprotocol:TWhichProtocol;
      FActiveConnections:Integer;
      fBlockSizeFlags:TDXBlockSizeFlags;
      fUseBlocking:Boolean;
      fUseNagle:Boolean;
      feListenerStarted:TNotifyEvent;
      feListenerStopped:TNotifyEvent;
      fListenerQueueSize:Integer;
   protected
      MyCriticalSection:TDXCritical;
      procedure Execute;override;
      procedure SetBufferCreates(value:Boolean);
      procedure SetSuspend(value:Boolean);
      procedure SetBlocking(value:Boolean);
      procedure SetNagle(value:Boolean);
      function GetSocket:TDXSock;
   public
      constructor Create(CreateSuspended:Boolean);
      destructor Destroy;override;
      function ActiveNumberOfConnections:Integer;
   published
      property MainSocket:TDXSock read GetSocket;
{$IFNDEF LINUX}
      property SpawnedThreadPriority:TThreadPriority read fstPriority
         write fstPriority;
{$ENDIF}
      property BlockingListener:Boolean read fUseBlocking
         write SetBlocking;
      property NagleListener:Boolean read fUseNagle
         write SetNagle;
      property BufferCreates:Boolean read fbBufferCreates
         write SetBufferCreates;
      property SuspendListener:Boolean read fbSuspend
         write SetSuspend;
      property BindTo:string read fsBindTo
         write fsBindTo;
      property ServerPort:Integer read fiServerPort
         write fiServerPort;
      property ThreadCacheSize:Integer read fiMaxConn
         write fiMaxConn;
      property OnNewConnect:TDX_NewConnect read feNewConnect
         write feNewConnect;
      property OnMaxConnects:TDX_MaxConnects read feMaxConnects
         write feMaxConnects;
      property OnGoingIdle:TDX_Idle read feIdle
         write feIdle;
      property OnAsleep:TDX_Sleep read feSleep
         write feSleep;
      property OnWakeUp:TDX_WakeUp read feWakeUp
         write feWakeUp;
      property OnListenerFailed:TDX_ListenerFailed read feListenerFailed
         write feListenerFailed;
      property OnListenerStarted:TNotifyEvent read feListenerStarted
         write feListenerStarted;
      property OnListenerStopped:TNotifyEvent read feListenerStopped
         write feListenerStopped;
      property OnUDPDataNoPool:TDX_UDPData read feUDPData
         write feUDPData;
      property ProtocolToBind:TWhichProtocol read fWhichprotocol
         write fWhichprotocol;
{$IFDEF CODE_TRACER}
      property DXCodeTracer:TDXCodeTracer read CodeTracer
         write CodeTracer;
{$ENDIF}
   end;

///////////////////////////////////////////////////////////////////////////////
// All protocols are descendants of this object/component. When making changes
// that need to flow to the actual protocol, like supporting a change to the
// spawned thread. You would make the "property" in this piece of code, and
// then when this piece of code creates the listener thread you can pass the
// information to the listener. At that point, you could pass it down to the
// protocol thread TDXServerThread.
///////////////////////////////////////////////////////////////////////////////
type
   TDXServerCore=class(TDXComponent)
   private
{$IFDEF CODE_TRACER}
      CodeTracer:TDXCodeTracer;
{$ENDIF}
      fbSSL:Boolean;
      fbActive:Boolean;
      fbSuspend:Boolean;
      fbBufferCreates:Boolean;
      fsBindTo:string;
      fiServerPort:Integer;
      fiMaxConn:Integer;
      feNewConnect:TDX_NewConnect;
      feMaxConnects:TDX_MaxConnects;
      feListenerFailed:TDX_ListenerFailed;
      feIdle:TDX_Idle;
      feSleep:TDX_Sleep;
      feWakeUp:TDX_WakeUp;
      feUDPData:TDX_UDPData;
{$IFNDEF LINUX}
      fltPriority:TThreadPriority;
      fstPriority:TThreadPriority;
{$ENDIF}
      fWhichprotocol:TWhichProtocol;
      fBlockSizeFlags:TDXBlockSizeFlags;
      fServerType:TServerType;
      fDummy:string;
      fbNagle:Boolean;
      fiTimeout:Cardinal;
      fListenerStarted:TNotifyEvent;
      fListenerStopped:TNotifyEvent;
      fiListenerQueueSize:Integer;
   protected
      fbForceAbort:Boolean;
      ListenerThread:TDXServerCoreThread;
      fEventArray:TList;
      procedure SetActive(value:boolean);
      procedure SetSuspend(value:boolean);
      function GetThreadCacheSize:Integer;
      procedure SetThreadCacheSize(value:Integer);
      function GetSocket:TDXSock;
      procedure SetfiMaxConn(Value:Integer);
   public
      constructor Create(AOwner:TComponent);
{$IFNDEF OBJECTS_ONLY}override;
{$ENDIF}
      destructor Destroy;override;
      function ActiveNumberOfConnections:Integer;
      procedure Start;virtual;
      procedure Stop;virtual;
      procedure Open;virtual;
      procedure Close;virtual;
      procedure Pause;virtual;
      procedure Resume;virtual;
      Procedure ForceAbort;
      property Socket:TDXSock read GetSocket;
      function InternalSessionTracker:TDXSessionTracker; virtual;
   published
{$IFNDEF LINUX}
      property ListenerThreadPriority:TThreadPriority read fltPriority
         write fltPriority;
      property SpawnedThreadPriority:TThreadPriority read fstPriority
         write fstPriority;
{$ENDIF}
      property ProtocolToBind:TWhichProtocol read fWhichprotocol
         write fWhichprotocol;
      property BindTo:string read fsBindTo
         write fsBindTo;
      property ServerPort:Integer read fiServerPort
         write fiServerPort;
      property IsActive:Boolean read fbActive;
      property Suspend:Boolean read fbSuspend
         write SetSuspend;
      property UseSSL:Boolean read fbSSL
         write fbSSL;
      property UseNagle:Boolean read fbNagle
         write fbNagle;
      property UseThreadPool:Boolean read fbBufferCreates
         write fbBufferCreates;
      property SocketOutputBufferSize:TDXBlockSizeFlags read fBlockSizeFlags
         write fBlockSizeFlags;
      property SocketQueueSize:Integer read fiListenerQueueSize
         write fiListenerQueueSize;
      property ServerType:TServerType read fServerType
         write fServerType;
      property Service:string read fDummy
         write fDummy;
      property ThreadCacheSize:Integer read GetThreadCacheSize
         write SetThreadCacheSize;
      property Timeout:Cardinal read fiTimeout
         write fiTimeout;

      property OnNewConnect:TDX_NewConnect read feNewConnect
         write feNewConnect;
      property OnUDPDataNoPool:TDX_UDPData read feUDPData
         write feUDPData;
      property OnMaxConnects:TDX_MaxConnects read feMaxConnects
         write feMaxConnects;
      property OnGoingIdle:TDX_Idle read feIdle
         write feIdle;
      property OnAsleep:TDX_Sleep read feSleep
         write feSleep;
      property OnWakeUp:TDX_WakeUp read feWakeUp
         write feWakeUp;
      property OnListenerFailed:TDX_ListenerFailed read feListenerFailed
         write feListenerFailed;
      property OnListenerStarted:TNotifyEvent read fListenerStarted
         write fListenerStarted;
      property OnListenerStopped:TNotifyEvent read fListenerStopped
         write fListenerStopped;
{$IFDEF CODE_TRACER}
      property DXCodeTracer:TDXCodeTracer read CodeTracer
         write CodeTracer;
{$ENDIF}
   end;

implementation

uses
{$IFNDEF LINUX}
   Windows,// InterlockedXX thanks to EYAL!
{$ENDIF}
   SysUtils,
   DXSocket;

constructor TDXClientThread.Create(CreateSuspended:Boolean);
begin
   inherited Create(CreateSuspended);
   FreeOnTerminate:=True;
{$WARNINGS OFF}
   Client:=nil;
   fpSessionData:=nil;
{$WARNINGS ON}
end;

destructor TDXClientThread.Destroy;
begin
{$IFDEF CODE_TRACER}
   if Assigned(CodeTracer) then begin
      CodeTracer.SendMessage(dxctInfo,'ClientThread.Terminating');
   end;
{$ENDIF}
   try
      if Assigned(ListenerThreadObject) then
         if Assigned(TDXServerCoreThread(ListenerThreadObject).fSessionTracker) then
            TDXServerCoreThread(ListenerThreadObject).fSessionTracker.UnregisterSession(Self);
   except
      on E:Exception do begin
//         E.Message;
      end;
   end;
{$WARNINGS OFF}
   try
      if Assigned(fpSessionData) then
         if assigned(feDestroySessionData) then
            feDestroySessionData(Self,fpSessionData);
   except
      on E:Exception do begin
//         E.Message;
      end;
   end;
   try
      if Assigned(Client) then begin
         Client.Free;
         Client:=nil;
      end;
   except
//      on E:Exception do begin
////         E.Message;
//      end;
   end;
{$WARNINGS ON}
{$IFDEF CODE_TRACER}
   if Assigned(CodeTracer) then begin
      CodeTracer.SendMessage(dxctInfo,'ClientThread.Terminated!');
   end;
{$ENDIF}
   inherited Destroy;
end;

function TDXClientThread.GetSessionID:Integer;
begin
   Result:=ThreadID;
end;

///////////////////////////////////////////////////////////////////////////////
//EXECUTE:
//        Once this thread has "Resume"d, execute is called by TThread. This will
//        fire the OnExecute (where the server protocol processes the connection)
//        and once that is finished it fires the OnDisconnet event.
///////////////////////////////////////////////////////////////////////////////

procedure TDXClientThread.Execute;
begin
   try
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXClientThread.Execute Starting '+
            IntToStr(TDXServerCoreThread(ListenerThreadObject).FActiveConnections));
      end;
{$ENDIF}
      while not Terminated do begin
         if Assigned(Client) then begin
            Client.OutputBufferSize:=fBlockSizeFlags;
            if Client.ValidSocket then begin
{$IFDEF CODE_TRACER}
               if Assigned(CodeTracer) then begin
                  CodeTracer.SendMessage(dxctInfo,'TDXClientThread.Execute ONNEWCONNECT '+
                     IntToStr(TDXServerCoreThread(ListenerThreadObject).FActiveConnections));
               end;
{$ENDIF}
               try
                  if Assigned(feNewConnect) then Begin
                     feNewConnect(Self);
                     ProcessWindowsMessageQueue;// have to do incase event was GUI based.
                  end;
               finally
{$IFDEF LINUX}
                  TDXServerCoreThread(ListenerThreadObject).MyCriticalSection.StartingWrite;
                  Dec(TDXServerCoreThread(ListenerThreadObject).FActiveConnections);
                  TDXServerCoreThread(ListenerThreadObject).MyCriticalSection.FinishedWrite;
{$ELSE}
                  InterlockedDecrement(TDXServerCoreThread(ListenerThreadObject).FActiveConnections);
{$ENDIF}
               end;
{$IFDEF CODE_TRACER}
               if Assigned(CodeTracer) then begin
                  CodeTracer.SendMessage(dxctInfo,'TDXClientThread.Execute ONNEWCONNECT-DONE '+
                     IntToStr(TDXServerCoreThread(ListenerThreadObject).FActiveConnections));
               end;
{$ENDIF}
            end;
         end
         else begin
            FreeOnTerminate:=True;
            Break;
         end;
      // incase user destoryed my DXSock client object, test client.
         if Assigned(Client) then begin
            try
               if Client.IsUDPMode then
                  Client.Sock:=INVALID_SOCKET
               else
                  if Client.Sock<>INVALID_SOCKET then Client.CloseGracefully;
            except
               try
                  Client.Free;
               finally
                  Client:=nil;
               end;
            end;
         end;
         if FreeOnTerminate then Break;
         if not Terminated then Suspend;
         if Terminated then FreeOnTerminate:=True;
      end;
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXClientThread.Execute Exiting.'+
            IntToStr(TDXServerCoreThread(ListenerThreadObject).FActiveConnections));
      end;
{$ENDIF}
   finally
      Terminate;
   end;
end;

///////////////////////////////////////////////////////////////////////////////
//SETSOCKETLATER:
//               Simple implementation of this "Thread" is to pass the Create the
//               recently "Accept"ed Socket during the create. But, if you are
//               producing speed by pre-creating the threads, this procedure will
//               allow you to pre-create threads, and pass the "Socket" when the
//               new accept is valid.
///////////////////////////////////////////////////////////////////////////////

procedure TDXClientThread.SetSocketLater(Socket:TDXSock);
begin
   if Assigned(Client) then begin
      Client.Free;
      Client:=nil;
   end;
   Client:=Socket;
   Client.PeerIPAddress:=Socket.PeerIPAddress;
   Client.PeerPort:=Socket.PeerPort;
end;

///////////////////////////////////////////////////////////////////////////////
// 'LISTENER' THREAD
// =================
//       This is the "Outter" Thread of a server. It does the listening loop, and
//       creates the thread that interacts with the client. Think of this as a
//       while active listen for a connection, and on connection spawn a thread
//       for the client connection (DXServerThread).
///////////////////////////////////////////////////////////////////////////////

constructor TDXServerCoreThread.Create(CreateSuspended:Boolean);
begin
   inherited Create(CreateSuspended);
   FreeOnTerminate:=False;
   ListenerSocket:=TDXSock.Create(Nil); // RC2
   fsBindTo:='';
   fiServerPort:=0;
   fiMaxConn:=200;
   FActiveConnections:=0;
   fbAnnouncedIdle:=0;
   fbBufferCreates:=True;
   fThreadPool:=TList.Create;
   MyCriticalSection:=TDXCritical.Create;
   fSessionTracker:=TDXSessionTracker.Create(nil);
end;

destructor TDXServerCoreThread.Destroy;
begin
{$WARNINGS OFF}
   if Assigned(fThreadPool) then begin
      MyCriticalSection.StartingWrite;
      while fThreadPool.Count>0 do begin
         if Assigned(fThreadPool[0]) then begin
            try
               TDXClientThread(fThreadPool[0]).ListenerThreadObject:=nil;
               fSessionTracker.UnRegisterSession(TDXClientThread(fThreadPool[0]));
            finally
               try
// this will raise an exception if it is already set "FreeOnTerminate"!
                  if TDXClientThread(fThreadPool[0]).Suspended then begin
                     TDXClientThread(fThreadPool[0]).FreeOnTerminate:=True;
                     TDXClientThread(fThreadPool[0]).Terminate;
                     TDXClientThread(fThreadPool[0]).Resume;
                  end
                  else begin
                     TDXClientThread(fThreadPool[0]).FreeOnTerminate:=True;
                     TDXClientThread(fThreadPool[0]).Terminate;
                  end;
               except
                  ;
               end;
            end;
         end;
         fThreadPool.Delete(0);
      end;
      fThreadPool.Free;
      fThreadPool:=nil;
      MyCriticalSection.FinishedWrite;
   end;
   fSessionTracker.Free;
   fSessionTracker:=nil;
{$WARNINGS ON}
   try
      if Assigned(ListenerSocket) then begin
         ListenerSocket.CloseNow;
         ListenerSocket.Free;
      end;
   finally
      ListenerSocket:=nil;
   end;
   MyCriticalSection.Free;
   inherited Destroy;
end;

function TDXServerCoreThread.ActiveNumberOfConnections:Integer;
begin
   Result:=fActiveConnections;
end;

procedure TDXServerCoreThread.SetBlocking(value:Boolean);
begin
   fUseBlocking:=Value;
   if ListenerSocket.ValidSocket then
      ListenerSocket.SetBlocking(Value);
end;

procedure TDXServerCoreThread.SetNagle(value:Boolean);
begin
   fUseNagle:=Value;
   if ListenerSocket.ValidSocket then
      ListenerSocket.SetNagle(Value);
end;

procedure TDXServerCoreThread.SetBufferCreates(value:Boolean);
begin
   if fActiveConnections>0 then Exit;
   fbBufferCreates:=Value;
   if not Value then
      if fiMaxConn<1 then fiMaxConn:=100;
end;

procedure TDXServerCoreThread.SetSuspend(value:Boolean);
begin
   fbSuspend:=Value;
end;

function TDXServerCoreThread.GetSocket:TDXSock;
begin
   Result:=ListenerSocket;
end;

///////////////////////////////////////////////////////////////////////////////
//EXECUTE:
//        This is the "Loop" for the server. It listens on the specified port and
//        IP address(es). As a connection comes in, it creates a DXServerThread
//        and gives it a new instance of the client socket. At that point the new
//        DXServerThread runs independant of this thread. If you wish to have a
//        pool of DXServerThreads suspended this is the section to change! When
//        a new DXServerThread is created, all of the "Events" are passed to it,
//        which means you "Server" events better be threadsafe!
//
//        Events: OnIdle is fired one the server has stopped receiving connection
//        requests. It is fired everytime the server goes idle from connection(s)
//        OnSleep is fired every two seconds after this loop has gone Idle.
///////////////////////////////////////////////////////////////////////////////

procedure TDXServerCoreThread.Execute;
var
   toggleSleep:Integer;
   DXClientThread:TDXClientThread;
   newClient:TDXSock;

   function InitializeThreading(UDP:Boolean):Boolean;
   var
      NewListen:PNewListen;
      Ws:string;

   begin
      toggleSleep:=0;
      Result:=False;
      if (Length(fsBindTo)>7) then ListenerSocket.BindTo:=fsBindTo;
      New(NewListen);
      with NewListen^ do begin
         Port:=fiServerPort;
         UseNAGLE:=fUseNagle;
         UseBlocking:=fUseBlocking;
         UseUDP:=UDP;
         WinsockQueue:=fListenerQueueSize;
         ConnectionLess:=UDP;
      end;
      if not ListenerSocket.Listen(NewListen) then begin
         Dispose(NewListen);
{$IFDEF CODE_TRACER}
         if Assigned(CodeTracer) then begin
            CodeTracer.SendMessage(dxctInfo,'Listener Initialization Error: '+
               ListenerSocket.GetErrorDesc(ListenerSocket.LastCommandStatus));
         end;
{$ENDIF}
         if Assigned(feListenerFailed) then begin
            feListenerFailed(ListenerSocket.LastCommandStatus);
            ProcessWindowsMessageQueue;// have to do incase event was GUI based.
         end
         else begin
            Str(fiServerPort,Ws);
{$IFNDEF LINUX}
            if IsConsole then begin
               Beep;
               Beep;
               Beep;
               Writeln('Listener on port '+Ws+', '+
                  ListenerSocket.GetErrorDesc(ListenerSocket.LastCommandStatus));
               DoSleepEx(1000);
            end
            else
{$ENDIF}
               ShowMessageWindow('Listener on port '+Ws,
                  ListenerSocket.GetErrorDesc(ListenerSocket.LastCommandStatus));
         end;
         Exit;
      end;
      Dispose(NewListen);
      Result:=True;
      if Assigned(feListenerStarted) then begin
         feListenerStarted(Self);
         ProcessWindowsMessageQueue;// have to do incase event was GUI based.
      end;
   end;

   procedure EventsOrSleep;
   begin
      if toggleSleep<2000 then begin
         Inc(toggleSleep);
         if (toggleSleep>1000)and (fbAnnouncedIdle<1) then begin
{$IFDEF CODE_TRACER}
            if Assigned(CodeTracer) then begin
               CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.GoingIdle');
            end;
{$ENDIF}
            fbAnnouncedIdle:=1;
            if assigned(feIdle) then Begin
               feIdle(Self);
               ProcessWindowsMessageQueue;// have to do incase event was GUI based.
            end;
         end;
//         DoSleepEx(1); // 4RC2
      end
      else begin
         if fbAnnouncedIdle=1 then begin
{$IFDEF CODE_TRACER}
            if Assigned(CodeTracer) then begin
               CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.GoingAsleep');
            end;
{$ENDIF}
            if assigned(feSleep) then Begin
               feSleep(Self);
               ProcessWindowsMessageQueue;// have to do - incase event was GUI based.
            end;
            fbAnnouncedIdle:=2;
         end;
         if toggleSleep<5000{3.0 - was 50} then Begin
            Inc(toggleSleep);
//            DoSleepEx(1); // 4RC2
         End
         else begin
            DoSleepEx(1);
            ProcessWindowsMessageQueue;
         end;
      end;
   end;

   procedure ThreadAtATime;
   begin
      DXClientThread:=TDXClientThread.Create(True);
      DXClientThread.Client:=TDXSock.Create(Nil); // RC2
{$IFNDEF LINUX}
      DXClientThread.Priority:=fstPriority;
{$ENDIF}
      DXClientThread.OnNewConnect:=feNewConnect;
      DXClientThread.fBlockSizeFlags:=fBlockSizeFlags;
      DXClientThread.ListenerThreadObject:=Self;
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.Execute.ThreadAtATime Ready');
      end;
{$ENDIF}
      while not Terminated do begin
         if (FActiveConnections<fiMaxConn) then begin
            if fbSuspend then
               DoSleepEx(20)
            else
               if ListenerSocket.Accept(DXClientThread.Client) then begin
{$IFDEF CODE_TRACER}
                  if Assigned(CodeTracer) then begin
                     DXClientThread.CodeTracer:=CodeTracer;
                  end;
{$ENDIF}
                  fSessionTracker.RegisterSession(DXClientThread);
                  DXClientThread.Resume;
{$IFDEF LINUX}
                  MyCriticalSection.StartingWrite;
                  Inc(FActiveConnections);
                  MyCriticalSection.FinishedWrite;
{$ELSE}
                  InterlockedIncrement(FActiveConnections);
{$ENDIF}
                  if (fbAnnouncedIdle>0) then begin
{$IFDEF CODE_TRACER}
                     if Assigned(CodeTracer) then begin
                        CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.WakeUp');
                     end;
{$ENDIF}
                     if Assigned(feWakeUp) then Begin
                        feWakeUp(Self);
                        ProcessWindowsMessageQueue;// have to do incase event was GUI based.
                     end;
                  end;
                  DXClientThread:=TDXClientThread.Create(True);
                  DXClientThread.Client:=TDXSock.Create(Nil);  // RC2
{$IFNDEF LINUX}
                  DXClientThread.Priority:=fstPriority;
{$ENDIF}
                  DXClientThread.OnNewConnect:=feNewConnect;
                  DXClientThread.fBlockSizeFlags:=fBlockSizeFlags;
                  DXClientThread.ListenerThreadObject:=Self;
                  fbAnnouncedIdle:=0;
                  toggleSleep:=0;
               end
               else
                  EventsOrSleep;
         end
         else begin
            if assigned(feMaxConnects) then begin
               feMaxConnects(Self);
               ProcessWindowsMessageQueue;// have to do incase event was GUI based.
            end
            else
               DoSleepEx(1);
         end;
      end;
      ListenerSocket.CloseNow;
      fSessionTracker.UnRegisterSession(DXClientThread);
      DXClientThread.FreeOnTerminate:=True;
      DXClientThread.Terminate;
   end;

   procedure ThreadPool;
   var
      NextThread:Integer;
      LoopCount:Integer;
      Done:Boolean;

   begin
      while fThreadPool.Count<fiMaxConn-1 do begin
         DXClientThread:=TDXClientThread.Create(True);
         DXClientThread.FreeOnTerminate:=False;// flags "Is ThreadPool".
{$IFNDEF LINUX}
         DXClientThread.Priority:=fstPriority;
{$ENDIF}
         DXClientThread.OnNewConnect:=feNewConnect;
         DXClientThread.fBlockSizeFlags:=fBlockSizeFlags;
         DXClientThread.ListenerThreadObject:=Self;
         DXClientThread.Client:=nil;
         fThreadPool.Add(DXClientThread);
         fSessionTracker.RegisterSession(DXClientThread);
      end;
      LoopCount:=fThreadPool.Count;
      NextThread:=0;
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.Execute.ThreadPool Ready');
      end;
{$ENDIF}
{$WARNINGS OFF}
      while not Terminated do begin
         if fbSuspend then
            DoSleepEx(20)
         else
            if ListenerSocket.Accept(TDXClientThread(fThreadPool[NextThread]).Client) then begin
{$IFDEF CODE_TRACER}
               if Assigned(CodeTracer) then begin
                  DXClientThread.CodeTracer:=CodeTracer;
               end;
{$ENDIF}
               TDXClientThread(fThreadPool[NextThread]).Resume;
{$IFDEF LINUX}
               MyCriticalSection.StartingWrite;
               Inc(FActiveConnections);
               MyCriticalSection.FinishedWrite;
{$ELSE}
               InterlockedIncrement(FActiveConnections);
{$ENDIF}
               Inc(NextThread);
               if NextThread>=LoopCount then NextThread:=0;
               Done:=False;
               while (not Terminated)and(not fbSuspend)and(not done) do begin
                  if TDXClientThread(fThreadPool[NextThread]).Client=nil then
                     Done:=True
                  else
                     if not TDXClientThread(fThreadPool[NextThread]).Client.ValidSocket then
                        Done:=True
                     else begin
                        Inc(NextThread);
                        if NextThread>=LoopCount then NextThread:=0;
                     end;
               end;
               toggleSleep:=0;
               if (fbAnnouncedIdle>0) then begin
{$IFDEF CODE_TRACER}
                  if Assigned(CodeTracer) then begin
                     CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.WakeUp');
                  end;
{$ENDIF}
                  if Assigned(feWakeUp) then begin
                     feWakeUp(Self);
                     ProcessWindowsMessageQueue;// have to do incase event was GUI based.
                  end;
               end;
               fbAnnouncedIdle:=0;
            end
            else
               EventsOrSleep;
      end;
      ListenerSocket.CloseNow;
{$WARNINGS ON}
   end;

{$WARNINGS OFF}
   procedure UDPSingleThreaded;
   var
      Data:Pointer;
      DataSize:Integer;

   begin
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.Execute.UDPSingleThread Ready');
      end;
{$ENDIF}
      if Assigned(feUDPData) then GetMem(Data,TDXMaxSocketBuffer);
      while not Terminated do begin
         if (FActiveConnections<fiMaxConn) then begin
            if fbSuspend then
               DoSleepEx(20)
            else
               if ListenerSocket.Readable then begin
                  if Assigned(feUDPData) then begin
{$IFDEF VER100}
                     DataSize:=ListenerSocket.BlockRead(Data,TDXMaxSocketBuffer);
{$ELSE}
                     DataSize:=ListenerSocket.Read(Data,TDXMaxSocketBuffer);
{$ENDIF}
                     feUDPData(Data,inet_ntoa(ListenerSocket.SockAddr.sin_addr),
                        ntohs(ListenerSocket.SockAddr.sin_port),DataSize);
                     ProcessWindowsMessageQueue;// have to do incase event was GUI based.
                  end
                  else begin
                     newClient:=TDXSock.Create(Nil); // RC2
                     newClient.Sock:=ListenerSocket.Sock;
                     newClient.IsUDPMode:=True;
                     DXClientThread:=TDXClientThread.Create(True);
                     DXClientThread.SetSocketLater(newClient);
{$IFNDEF LINUX}
                     DXClientThread.Priority:=fstPriority;
{$ENDIF}
                     DXClientThread.OnNewConnect:=feNewConnect;
                     DXClientThread.fBlockSizeFlags:=fBlockSizeFlags;
                     DXClientThread.ListenerThreadObject:=Self;
                     fSessionTracker.RegisterSession(DXClientThread);
{$IFDEF CODE_TRACER}
                     if Assigned(CodeTracer) then begin
                        DXClientThread.CodeTracer:=CodeTracer;
                     end;
{$ENDIF}
                     DXClientThread.Resume;
                  end;
                  if (fbAnnouncedIdle>0) then begin
{$IFDEF CODE_TRACER}
                     if Assigned(CodeTracer) then begin
                        CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.WakeUp');
                     end;
{$ENDIF}
                     if Assigned(feWakeUp) then begin
                        feWakeUp(Self);
                        ProcessWindowsMessageQueue;// have to do incase event was GUI based.
                     end;
                  end;
                  toggleSleep:=0;
                  fbAnnouncedIdle:=0;
               end
               else
                  EventsOrSleep;
         end
         else begin
            if assigned(feMaxConnects) then begin
               feMaxConnects(Self);
               ProcessWindowsMessageQueue;// have to do incase event was GUI based.
            end
            else begin
               DoSleepEx(1);
            end;
         end;
      end;
      if Assigned(feUDPData) then FreeMem(Data,TDXMaxSocketBuffer);
   end;
{$WARNINGS ON}

begin
   if terminated then exit;
   if not Assigned(ListenerSocket) then begin
      ShowMessageWindow(
         'Developer Trap #2',
         'You must pass the listerner socket. You have coded around '+
         'our fail-safe routines!');
      Exit;
   end;
{$IFDEF CODE_TRACER}
   if Assigned(CodeTracer) then begin
      CodeTracer.SendMessage(dxctInfo,'TDXServerCoreThread.Execute');
   end;
{$ENDIF}
   try
      if not InitializeThreading({False} fWhichprotocol=wpUDPOnly) then Exit;
      if fWhichprotocol=wpUDPOnly then
         UDPSinglethreaded
      else begin
         case fbBufferCreates of
            False:begin
                  ThreadAtATime;
               end;
         else begin
               ThreadPool;
            end;
         end;
      end;
   finally
{$IFDEF CODE_TRACER}
      if Assigned(CodeTracer) then begin
         CodeTracer.SendMessage(dxctInfo,'TDXClientThread.Execute Terminating');
      end;
{$ENDIF}
      FreeOnTerminate:=True;
      if Assigned(feListenerStopped) then begin
         feListenerStopped(Self);
         ProcessWindowsMessageQueue;// have to do incase event was GUI based.
      end;
//      Terminate;
   end;
end;

///////////////////////////////////////////////////////////////////////////////
// ANCESTOR TO ALL PROTOCOLS
// =========================
// DXSERVERCORE - Mainly Allows for "Properties" to be defined. And to Start,
// Stop, and Suspend the 'LISTNER' Thread.
//
//       Protocols use this object to interact with the DXSock environment. This
//       Section allows the protocol to specify the status of the server.
///////////////////////////////////////////////////////////////////////////////

constructor TDXServerCore.Create(AOwner:TComponent);
begin
   inherited create(aowner);
   fbActive:=False;
   fiMaxConn:=200;
   fbBufferCreates:=True;
{$IFNDEF LINUX}
   fstPriority:=tpNormal;
   fltPriority:=tpNormal;
{$ENDIF}
   fWhichProtocol:=wpTCPOnly;
   fBlockSizeFlags:=bsfHuge;
   fServerType:=stThreadBlocking;
   fbNagle:=True;
   fiTimeout:=120000;
   fEventArray:=TList.Create;
   fiListenerQueueSize:=100;
end;

destructor TDXServerCore.Destroy;
begin
   if fbActive then SetActive(False);
   If Assigned(fEventArray) then Begin
      fEventArray.Free;
      fEventArray:=Nil;
   End;
   inherited destroy;
end;

function TDXServerCore.GetSocket:TDXSock;
begin
   if Assigned(ListenerThread) then
      Result:=ListenerThread.ListenerSocket
   else
      Result:=nil;
end;

function TDXServerCore.ActiveNumberOfConnections:Integer;
begin
   if Assigned(ListenerThread) then
      Result:=ListenerThread.ActiveNumberOfConnections
   else
      Result:=0;
end;

procedure TDXServerCore.SetActive(value:boolean);
begin
{$IFNDEF OBJECTS_ONLY}
   if (csDesigning in ComponentState) then exit;
{$ENDIF}
   fbSuspend:=False;
   if Value<>fbActive then begin
      if Value then begin
         if not Assigned(feNewConnect) then begin
            if not Assigned(feUDPData) then begin
               ShowMessageWindow(
                  'Developer Trap #1!',
                  'You must specify a OnNewConnect Event!');
               Exit;
            end;
         end;
         fbActive:=Value;
{$IFDEF CODE_TRACER}
         if Assigned(CodeTracer) then begin
            CodeTracer.StartTransaction;
            CodeTracer.SendMessage(dxctDebug,'TDXSServerCore.Active=True');
            CodeTracer.SendObject(dxctDebug,Self);
         end;
{$ENDIF}
         fbSuspend:=False;
         ListenerThread:=TDXServerCoreThread.Create(True);
         ListenerThread.fsBindTo:=fsBindTo;
         ListenerThread.fiServerPort:=fiServerPort;
         ListenerThread.fiMaxConn:=fiMaxConn;
         ListenerThread.feNewConnect:=feNewConnect;
         ListenerThread.feMaxConnects:=feMaxConnects;
         ListenerThread.feListenerFailed:=feListenerFailed;
         ListenerThread.feListenerStarted:=fListenerStarted;
         ListenerThread.feListenerStopped:=fListenerStopped;
         ListenerThread.feIdle:=feIdle;
         ListenerThread.feSleep:=feSleep;
         ListenerThread.feWakeUp:=feWakeUp;
         ListenerThread.fListenerQueueSize:=fiListenerQueueSize; // RC2
{$IFNDEF LINUX}
         ListenerThread.Priority:=fltPriority;
         ListenerThread.SpawnedThreadPriority:=fstPriority;
{$ENDIF}
         ListenerThread.fBlockSizeFlags:=fBlockSizeFlags;
         ListenerThread.fbSuspend:=False;
         ListenerThread.feUDPData:=feUDPData;
{$WARNINGS OFF}
         if DebugHOOK<>0 then
            ListenerThread.fbBufferCreates:=False
         else
            ListenerThread.fbBufferCreates:=fbBufferCreates;
{$WARNINGS ON}
{$IFDEF CODE_TRACER}
         if Assigned(CodeTracer) then begin
            ListenerThread.CodeTracer:=CodeTracer;
         end;
{$ENDIF}
         ListenerThread.fWhichProtocol:=fWhichProtocol;
         ListenerThread.NagleListener:=fbNagle;
         ListenerThread.BlockingListener:=fServerType=stThreadBlocking;
         ListenerThread.Resume;
      end
      else begin
         fbActive:=Value;
         if Assigned(ListenerThread) then begin
{$IFDEF CODE_TRACER}
            if Assigned(CodeTracer) then begin
               CodeTracer.SendMessage(dxctInfo,'DXServerCore.ListenerThread still existed - terminating');
            end;
{$ENDIF}
            if ListenerThread.Suspended then begin
//               ListenerThread.FreeOnTerminate:=True;
               ListenerThread.Terminate;
               ListenerThread.Resume;
            end
            else begin
//               ListenerThread.FreeOnTerminate:=True;
               ListenerThread.Terminate;
            end;
         end;
{$IFDEF CODE_TRACER}
         if Assigned(CodeTracer) then begin
            CodeTracer.SendObject(dxctDebug,Self);
            CodeTracer.SendMessage(dxctDebug,'TDXSServerCore.Active=False');
            CodeTracer.EndTransaction;
         end;
{$ENDIF}
      end;
   end;
end;

procedure TDXServerCore.Start;
begin
   SetActive(True);
end;

procedure TDXServerCore.Stop;
begin
   SetActive(False);
end;

procedure TDXServerCore.Open;
begin
   Start;
end;

procedure TDXServerCore.Close;
begin
   Stop;
end;

procedure TDXServerCore.Pause;
begin
   SetSuspend(True);
end;

procedure TDXServerCore.Resume;
begin
   SetSuspend(False);
end;

procedure TDXServerCore.SetSuspend(value:boolean);
begin
   if fbActive then begin
      fbSuspend:=Value;
      ListenerThread.SuspendListener:=Value;
   end;
end;

procedure TDXServerCore.SetfiMaxConn(Value:Integer);
begin
   if Value<1 then begin
{$WARNINGS OFF}
      if DebugHOOK<>0 then Exit;{cant enable this from within DELPHI live!}
{$WARNINGS ON}
      fiMaxConn:=-1;
      fbBufferCreates:=True;
   end
   else
      fiMaxConn:=Value;
end;

function TDXServerCore.GetThreadCacheSize:Integer;
begin
   Result:=fiMaxConn;
end;

procedure TDXServerCore.SetThreadCacheSize(value:Integer);
begin
   SetfiMaxConn(Value);
end;

function TDXServerCore.InternalSessionTracker:TDXSessionTracker;
begin
   if Assigned(ListenerThread) then
      Result:=ListenerThread.fSessionTracker
   else
      Result:=nil;
end;

procedure TDXServerCore.ForceAbort;
begin
   fbForceAbort:=True;
end;

end.

