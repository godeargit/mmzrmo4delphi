
unit DXBasicTimer;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXBasicTimer
//       Thread: TTimerThread
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
//  Description: Component Version of a Thread Based Timer.
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   Controls,
   Classes;

{$I DXAddons.def}

type
   {$IFDEF OBJECTS_ONLY}
   // TDXBasicTimer is a thread based timer incorporated into the
   // suite for internal usage and is available for your
   // applications. Since the event engine is thread based instead
   // of the standard Multimedia implementation from Borland, it is
   // more accurate to the millisecond, even on busy systems.
   //
   //
   //
   //
   //
   // Summary
   // Thread Based Timer
   TDXBasicTimer=class
      {$ELSE}
   {$IFDEF OCX_ONLY}
   TDXOCXBasicTimer=class(TWinControl)
      {$ELSE}
   TDXBasicTimer=class(TComponent)
      {$ENDIF}
      {$ENDIF}
   private
      FInterval:Cardinal;
      FPriority:TThreadPriority;
      FOnTimer:TNotifyEvent;
      bStop:Boolean;
      bRunning:Boolean;
      FEnabled:Boolean;
      fbSleepBefore:Boolean;
   protected
      procedure setEnabled(b:boolean);
      procedure SetFInterval(value:Cardinal);
      procedure Start;
      procedure Stop;
   public
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
   published
      // The TDXBasicTimer allows you to design your timer to fire the
      // OnTimer event first then pause, or pause then fire the
      // OnTimer event. Depending upon your imlpementation one theory
      // may work better than the other.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXBasicTimer.Enabled, Enabled>, <LINK TDXBasicTimer.Interval, Interval>,
      // <LINK TDXBasicTimer.OnTimer, OnTimer>
      //
      // <LINK TDXBasicTimer.OnTimer>
      //
      // Summary
      // How should the first event fire, instantly or after sleeping
      // once.
      property SleepBeforeFire:Boolean read fbSleepBefore
         write fbSleepBefore;
      // True starts the basic timer, False signals the basic timer to
      // stop as soon as possible.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXBasicTimer.Interval, Interval>, <LINK TDXBasicTimer.OnTimer, OnTimer>,
      // <LINK TDXBasicTimer.SleepBeforeFire, SleepBeforeFire>
      //
      // <LINK TDXBasicTimer.SleepBeforeFire>
      //
      // Summary
      // Enabled/Disable the Timer.
      property Enabled:boolean read FEnabled write setEnabled;
      // The number of milliseconds paused between calls to the
      // OnTimer event.<B>
      //
      //
      //
      // \Note</B> since this is a threaded timer, you should make
      // sure your Interval is longer than the OnTimer process.
      // Otherwise the timer will shutdown if it is busy processing a
      // previous OnTimer event, and will no longer function!
      //
      //
      //
      // \See Also
      //
      // <LINK TDXBasicTimer.Enabled, Enabled>, <LINK TDXBasicTimer.OnTimer, OnTimer>,
      // <LINK TDXBasicTimer.SpeedBeforeFire, SleepBeforeFire>
      //
      // <LINK TDXBasicTimer.SpeedBeforeFire>
      //
      // Summary
      // Define the delay between OnTimer events
      property Interval:Cardinal read FInterval write SetFInterval;
      // With all of our components, you are able to control the
      // priority level of the thread. The default is usually perfect
      // for most applications.
      //
      //
      //
      // Summary
      // Control the priority of the timer thread.
      property ThreadPriority:TThreadPriority read FPriority write FPriority
         default tpNormal;
      // To adhere to standard timer events, the OnTimer event in
      // TDXBasicTimer is of TNotifyEvent type.<B>
      //
      //
      //
      // \Note</B> you should take all precautions to make sure the
      // code and objects you use in the OnTimer event are threadsafe.
      // As we do not call Synchronize() for the OnTimer event. Doing
      // so would limit this component to only working in GUI
      // applications.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXBasicTimer.Enabled, Enabled>, <LINK TDXBasicTimer.Interval, Interval>,
      // <LINK TDXBasicTimer.SleepBeforeFire, SleepBeforeFire>
      //
      // <LINK TDXBasicTimer.SleepBeforeFire>
      //
      // Summary
      // Actual event hook fired by the timer, after the sleep
      // interval has elapsed.
      property OnTimer:TNotifyEvent read FOnTimer write FOnTimer;
      // Used internally to show the date of this release in the
      // object inspector. Only useful when reporting bugs to
      // development team.
      {$IFDEF OCX_ONLY}
      // hide these from the object inspectors
      property align:string write SetReleaseDate;
      property cursor:string write SetReleaseDate;
      property dragcursor:string write SetReleaseDate;
      property dragmode:string write SetReleaseDate;
      property helpcontext:string write SetReleaseDate;
      property hint:string write SetReleaseDate;
      property parentshowhint:string write SetReleaseDate;
      property popupmenu:string write SetReleaseDate;
      property showhint:string write SetReleaseDate;
      property taborder:string write SetReleaseDate;
      property tabstop:string write SetReleaseDate;
      {$ENDIF}
   end;

   TTimerThread=class(TThread)
   private
      {$IFDEF OCX_ONLY}
      TT:TDXOCXBasicTimer;
      {$ELSE}
      TT:TDXBasicTimer;
      {$ENDIF}
   protected
      procedure DoExecute;
   public
      {$IFDEF OCX_ONLY}
      constructor CreateTimerThread(TT:TDXOCXBasicTimer);
      {$ELSE}
      constructor CreateTimerThread(TT:TDXBasicTimer);
      {$ENDIF}
      procedure Execute; override;
   end;

   {$IFDEF OCX_ONLY}
procedure Register;
{$ENDIF}

implementation

uses
   DXString;

{$IFDEF OBJECTS_ONLY}

constructor TDXBasicTimer.Create;
{$ELSE}
{$IFDEF OCX_ONLY}

constructor TDXOCXBasicTimer.Create(AOwner:TComponent);
{$ELSE}

constructor TDXBasicTimer.Create(AOwner:TComponent);
{$ENDIF}
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
   FPriority:=tpNormal;
   fInterval:=5000;
   fbSleepBefore:=False;
end;

{$IFDEF OCX_ONLY}

destructor TDXOCXBasicTimer.Destroy;
{$ELSE}

destructor TDXBasicTimer.Destroy;
{$ENDIF}
begin
   inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////////
// Changing the Enabled property calls either Start or Stop protected methods.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OCX_ONLY}

procedure TDXOCXBasicTimer.setEnabled(b:boolean);
{$ELSE}

procedure TDXBasicTimer.setEnabled(b:boolean);
{$ENDIF}
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
{$IFDEF OCX_ONLY}

procedure TDXOCXBasicTimer.Start;
{$ELSE}

procedure TDXBasicTimer.Start;
{$ENDIF}
begin
   if bRunning then Exit;
   bStop:=false;
   {$IFNDEF OBJECTS_ONLY}
   if not(csDesigning in ComponentState) then begin
      {$ENDIF}
      with TTimerThread.CreateTimerThread(self) do begin
         Resume;
      end;
      {$IFNDEF OBJECTS_ONLY}
   end;
   {$ENDIF}
   bRunning:=true;
end;

///////////////////////////////////////////////////////////////////////////////
// Stopping the timer just sets the stop flag to true, the TTimerThread's
// Execute method will then end and the thread will be destroyed.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OCX_ONLY}

procedure TDXOCXBasicTimer.Stop;
{$ELSE}

procedure TDXBasicTimer.Stop;
{$ENDIF}
begin
   bStop:=true;
   bRunning:=false;
end;

///////////////////////////////////////////////////////////////////////////////
// Create the thread for the timer, self contained.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OCX_ONLY}

constructor TTimerThread.CreateTimerThread(TT:TDXOCXBasicTimer);
{$ELSE}

constructor TTimerThread.CreateTimerThread(TT:TDXBasicTimer);
{$ENDIF}
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
   SleepTime:Cardinal;
   Last:Comp;
   BB:Boolean;

begin
   with TT do begin
      BB:=fbSleepBefore;
      while not bStop do begin
         Last:=TimeCounter;
         if not BB then begin
            if IsConsole then
               DoExecute
            else
               Synchronize(DoExecute);
         end
         else
            BB:=False;
         SleepTime:=Trunc(FInterval-(TimeCounter-Last));
         if SleepTime<10 then SleepTime:=10;
         if not bStop then DoSleepEX(SleepTime);
      end;
   end;
   Terminate;
end;

///////////////////////////////////////////////////////////////////////////////
// This method is called within the TTimerThread's Execute, using the
// Synchronize method. This is because we need to call the event handler from
// the main VCL thread.
///////////////////////////////////////////////////////////////////////////////

procedure TTimerThread.DoExecute;
begin
   with tt do
      if Assigned(FOnTimer) then FOnTimer(tt);
end;

{$IFDEF OCX_ONLY}

procedure TDXOCXBasicTimer.SetFInterval(value:Cardinal);
{$ELSE}

procedure TDXBasicTimer.SetFInterval(value:Cardinal);
{$ENDIF}
begin
   if Value>5000 then
      fInterval:=Value
   else
      fInterval:=5000;
end;

{$IFDEF OCX_ONLY}

procedure register;
begin
   RegisterComponents('BPDX OCX Addons', [
      TDXOCXBasicTimer]);
end;
{$ENDIF}

end.

