unit DXThreads4Components;

interface

///////////////////////////////////////////////////////////////////////////////
//      Threads: TDXComponentThread, TDXComponentSpawn
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
// Introduced March 31st 2000
// TDXComponentThread - is a lopping thread
// TDXComponentSpawn - is a "new thread", fire ontimer, then terminate
// TDXFork - is like ComponentSpawn but designed to fire a procedure on create
///////////////////////////////////////////////////////////////////////////////

uses
  Classes;

Type
  // TDXComponentThread works very much like our TDXBasicTimer,
// except it is not a component/object. It provides the ability
// to run over and over as a timer would. However, we extended
// it to support Synchronized (forcing a threadsafe OnTimer
// event) or non-Synchronized (requiring you to develop the
// OnTimer event using thread safe code). Making it a very
// flexible thread that can work in GUI and CONSOLE
// applications.                                               
TDXComponentThread=class(TThread)
  private
    fSleepFirst:Boolean;
    fSynchronized:Boolean;
    TT:TComponent;
    fInterval:Cardinal;
    fOnTimer:TNotifyEvent;
  protected
    procedure DoExecute;
  public
    constructor CreateThread(TT:TComponent);
    procedure Execute; override;
  published
    // Interval is the number of milliseconds that the thread pauses
    // between calls to OnTimer. It should always be higher than the
    // amount of time consumed by the actual OnTimer code. If you
    // are not sure how low is too low, you should enable
    // RunSynchronized to avoid an internal overrun.
    // 
    // 
    // 
    // \See Also
    // 
    // <LINK TDXComponentThread.OnTimer, OnTimer>, <LINK TDXComponentThread.SleepFirst, SleepFirst>,
    // <LINK TDXComponentThread.RunSynchronized, RunSynchronized>                                   
property Interval:Cardinal read fInterval
                               write fInterval
                               Default 60000;
    // OnTimer is a standard OnTimer event, however the AOwner is
    // the component that was assigned during the CreateThread
    // process. Which means it does not actually have to be the
    // Owner of the thread!
    // 
    //
    // 
    // \See Also
    // 
    // <LINK TDXComponentThread.CreateThread, CreateThread>, <LINK TDXComponentThread.Interval, Interval>,
    // <LINK TDXComponentThread.SleepFirst, SleepFirst>, <LINK TDXComponentThread.RunSynchronized, RunSynchronized>
property OnTimer:TNotifyEvent read fOnTimer
                                  write fOnTimer;
    // RunSynchronized provides the ability to Synchronize() the
    // OnTimer event. Synchronize is not available to console
    // applications, as it interacts with the VCL thread, which is
    // only available to GUI applications. It should only be enabled
    // if your code in the OnTimer event is not threadsafe. If you
    // are developing, and trying to see how low to set the
    // Interval. You can set this to True. Develop your OnTimer
    // code, then set to false and see if the OnTimer event stops
    // firing. This means the thread tried to call OnTimer while a
    // previous instance of OnTimer was still running.<B>
    //
    // 
    // 
    // \Note</B> all of Borland's GUI components are NOT threadsafe!
    // 
    // 
    //
    // \See Also
    // 
    // <LINK TDXComponentThread.Interval, Interval>, <LINK TDXComponentThread.OnTimer, OnTimer>
property RunSychronized:Boolean read fSynchronized
                                    write fSynchronized;
    // To be flexible like our TDXBasicTimer, yuo can elect to fire
    // OnTimer instantly when you first resume the thread. Or to
    // pause first then fire the OnTimer event. After the first call
    // to OnTimer, the code is the same, it will sleep upto the
    // Interval period and fire OnTimer again.
    // 
    //
    // 
    // \See Also
    // 
    // <LINK TDXComponentThread.Interval, Interval>, <LINK TDXComponentThread.OnTimer, OnTimer>
property SleepFirst:Boolean read fSleepFirst
                                write fSleepFirst;
  end;

  TDX_OnTimerSpawn=procedure(fpSessionData:Pointer) of object;
  // TDXComponentSpawn is a special TThread descendant that was
  // developed to allow your application or descendant protocols
  // to spawn another thread quickly. The concept behind this
  // thread was to allow an application to spawn a thread which
  // contains all the "session" information for the current
  // client. The thread inturn fires a common OnTimer
  // imlpementation.
  // 
  //
  //
  // One example of using this would be to synchronize information
  // to a database in the background without effecting the current
  // client session. Primary examlpe would be an IRC server, which
  // could be sending data relayed to another server. Instead of
  // causing the client thread to pause while the message is
  // forwarded on to other servers. You would simply implement a
  // common OnTimer event that handles a DataQueue for delivery.
  // When a message comes from the client to a forum that needs to
  // echo to other servers, you could create a session pointer
  // with the channel name for examlpe. Then spawn this thread,
  // and in the OnTimer event, reference the GetSessionData
  // function. Look up the channel in your server array, and with
  // DataQueue pump that message over to other servers in the
  // background.
  // 
  // 
  //
  // There are many other uses for this type of concept. So we
  // provide a couple of our TThread wrappers in this unit.       
TDXComponentSpawn=class(TThread)
  private
    fpPointer:Pointer;
    fOnTimer:TDX_OnTimerSpawn;
    fID:Integer;
  protected
  public
    constructor CreateThread(fpSessionData:Pointer);
    procedure Execute; override;
    // SetSessionData allows you to update the fpSessionData. It was
    // provided to make your code easier to follow, if someone was
    // not familiar with the flexible design of pointers. Normally
    // you would never use this procedure.
    // 
    // 
    //
    // \See Also
    // 
    // <LINK TDXComponentSpawn.CreateThread, CreateThread>, <LINK TDXComponentSpawn.GetSessionData, GetSessionData>,
    // <LINK TDXComponentSpawn.OnTimer, OnTimer>                                                                    
Procedure SetSessionData(fpSessionData:Pointer);
    // GetSessionData allows you to retrieve the fpSessionData
    // assigned during create, or SetSessionData. Normally not
    // needed, but provided so this thread can be flexible to your
    // needs.<B>
    // 
    // 
    // 
    // \Note</B> The fpSessionData pointer is a pointer to the
    // supplied pointer. So if you destroy the pointer externally,
    // you will inturn destroy this pointer!
    //
    // 
    // 
    // \See Also
    // 
    // <LINK TDXComponentSpawn.SetSessionData, SetSessionData>, <LINK TDXComponentSpawn.CreateThread, CreateThread>,
    // <LINK TDXComponentSpawn.OnTimer, OnTimer>
Function  GetSessionData:Pointer;
  published
    // OnTimer is a non-synchronized event, meaning all the code
    // contained in your event implementation MUST be thread safe!
    // The fpSessionData is provided as a property to this event.
    //
    // 
    // 
    // \See Also
    // 
    // <LINK TDXComponentSpawn.CreateThread, CreateThread>, <LINK TDXComponentSpawn.GetSessionData, GetSessionData>,
    // <LINK TDXComponentSpawn.SetSessionData, SetSessionData>                                                      
property OnTimer:TDX_OnTimerSpawn read fOnTimer
                                      write fOnTimer;
    // ID by default is always zero. But we provided this Interget
// incase you had a need to track something outside of the
// fpSessionData. One example, that we have done in the past. Is
// to assign this to the socket handle of the current client
// session. Normally you will not use this property.            
property ID:Integer read fID
                        write fID;
  end;

TDXFork=class(TThread)
  protected
    fProc:TThreadMethod;
  public
    constructor CreateThread(Proc:TThreadMethod);
    procedure Execute; override;
End;

implementation

Uses
   DXString;

// Create the thread and optionally assign a component to float
// around within the thread. This routine sets the thread to <B><I>Suspended</B></I>
// and <B><I>FreeOnTerminate</B></I>. The goal of this type of
// thread was to provide a way to do tasks in the background
// over and over, like a timer. But pass in the OnTimer event, a
// component that was created globally by the routine that
// created the thread.
//
// 
// 
// One reason you might want to imlpement this type of threading
// design would be a server that is expecting data to be
// available at anytime, and you want to check if it is
// abailable periodically. For example, a database server could
// loop constantly checking each idle client for incoming data.
// But that would slow down the overall performance of the
// engine. With this thread, you could create the thread, pass
// it the current ClientThread, with an interval of 500ms. The
// OnTimer event would fire after 500ms, passing you the
// ClientThread in the OnTimer event. At that point you could do
// a read, collect the data into a session buffer. This allows
// the primary thread to monitor the buffer only, and push back
// results to the client. Without having to monitor both the
// inbound data and outbound results.
// 
// 
// 
// This is an extremely advanced design, and is not something
// for a novice!
// 
// 
// 
// \See Also
// 
// <LINK TDXComponentThread.OnTimer, OnTimer>                                       
constructor TDXComponentThread.CreateThread(TT:TComponent);
begin
   inherited Create(True);
   Self.TT:=TT;
   FreeOnTerminate:=True;
   fSynchronized:=False;
end;

///////////////////////////////////////////////////////////////////////////////
// EXECUTE:
//         Fires the internal call to DoExecute, and based upon the time
// consumed waits the remaining time until it repeats or detects termination.
// We use synchronize since we do not know if the called code in
// DoExecute->OnTimer is actually thread-safe. This forces the execution to be
// processed in the main VCL's thread.
///////////////////////////////////////////////////////////////////////////////
procedure TDXComponentThread.Execute;
var
   SleepTime:Integer;
   Last:Comp;
   B:Boolean;

begin
   B:=fSleepFirst;
   while not Terminated do begin
      Last:=DXString.TimeCounter;
      If Not B then Begin
         If fSynchronized then Synchronize(DoExecute)
         Else DoExecute;
         B:=False;
      End;
      If DXString.TimeCounter-Last>FInterval then SleepTime:=10
      Else SleepTime:=Trunc(FInterval-(DXString.TimeCounter-Last));
      If SleepTime<10 then SleepTime:=10;
      If not Terminated then DoSleepEx(SleepTime);
   end;
end;

///////////////////////////////////////////////////////////////////////////////
// DOEXECUTE:
//           Actually calls the OnTimer event.
///////////////////////////////////////////////////////////////////////////////
procedure TDXComponentThread.DoExecute;
begin
   with TT do
      if Assigned(FOnTimer) then FOnTimer(TT);
end;

// Create the thread and pass it any session data as a pointer.
// This routine sets the thread to <B><I>suspended</B></I> and <B><I>FreeOnTerminate</B></I>.
// The fpSessionData can be any form of data as long as you
// reference it as a pointer.
// 
// 
//
// \See Also
// 
// <LINK TDXComponentSpawn.GetSessionData, GetSessionData>, <LINK TDXComponentSpawn.SetSessionData, SetSessionData>,
// <LINK TDXComponentSpawn.OnTimer, OnTimer>
constructor TDXComponentSpawn.CreateThread(fpSessionData:Pointer);
begin
   inherited Create(True);
   FreeOnTerminate:=True;
   fpPointer:=fpSessionData;
   fID:=0;
end;

Procedure TDXComponentSpawn.SetSessionData(fpSessionData:Pointer);
Begin
   fpPointer:=fpSessionData;
End;

Function TDXComponentSpawn.GetSessionData:Pointer;
Begin
   Result:=fpPointer;
End;

// Called Internally when you Resume the thread. You should not
// suspend this type of thread, as it was designed to go off and
// do something in the backrgound, and terminate/free.          
procedure TDXComponentSpawn.Execute;
begin
   if Assigned(FOnTimer) then FOnTimer(fpPointer);
end;

constructor TDXFork.CreateThread(Proc:TThreadMethod);
begin
   FreeOnTerminate:=True;
   fProc:=Proc;
   inherited Create(False);
End;

procedure TDXFork.Execute;
Begin
   Synchronize(fProc);
End;

end.
