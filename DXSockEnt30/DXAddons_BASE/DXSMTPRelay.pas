unit DXSMTPRelay;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSMTPRelay
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
// This component is a bulk sender component. It is dumb in a way, where it
// has been designed to send a stream of data provided through a call-back
// event. The original design was to do everything, which restricted development
// to what we invisioned as the correct way to do things.
//
// This new design is the component is to be called using the AddToQueue with
// the parameter of who the message is to, who the message is from, and an ID
// to the message that makes sense to the calling layer. This engine will group
// outbound connections to group the connections to the same server. When a
// connection is made all applicable messages to send are fired as call back
// hooks. This component will get the server into the DATA mode to receive the
// header and body. Firing the event with the socket information. The code in
// this hook will send the DATA until finished and return back to this layer.
// Which will send the "." DATA terminator. If another message is pending for
// the same server, an RSET is sent, the new preamble information to get the
// server into DATA mode, and the event is fired again.
//
// The suite supports a SMART HOST, where ALL email is forwarded to instead of
// being sent to the SMTP servers directly.
//
// TO-DO:
// 1. Need to add a "disk" copy of the fQueue - incase of power-outtage.
// 2. Group the outbound to the same mail-server - and use one connection.
///////////////////////////////////////////////////////////////////////////////

uses
   DXMXResolver,
   DXSMTPSender,
   DXString,
   Classes;

{$I DXAddons.def}

type
   PDXSMTPRelayControl=^TDXSMTPRelayControl;
   TDXSMTPRelayControl=record
      LastMailServerTried:Integer;
      ConnectTries:Integer; // -1 means sent
      // 0 no attempts made yet
      SessionErrors:Integer; // 0 is default!
      ResolveErrors:Integer; // 0 is default!
      MailServer:TStringList;
      MailTo:string;
      MailFrom:string;
      MessageInfo:string;
      MailToDomain:string;
      RecordKey:string;
      {$IFDEF VER100}
      ID:DWord;
      {$ELSE}
      ID:LongWord;
      {$ENDIF}
   end;
   TDX_CleanOutQueue=procedure(RecordKey:string) of object;
   TDX_ConnectFailed=procedure(HostAddress, MessageInfo:string) of object;
   TDX_Connected=procedure(HostAddress, MessageInfo:string) of object;
   TDX_SendDATA=procedure(SMTPSender:TDXSMTPSender; MessageInfo:string; var
      Handled:Boolean) of object;
   TDX_MsgSent=procedure(MessageInfo:string) of object;
   TDX_SessionError=procedure(HostAddress, MessageInfo, SessionMessage:string) of
      object;
   TDX_TriesExceeded=procedure(RelayInfo:PDXSMTPRelayControl) of object;
   TDX_QueueCall=procedure(DXSMTPRelayControlList:TList) of object;
   TDX_Resolve=procedure(MailTo, MessageInfo:string) of object;
   TDX_ResolveFailed=procedure(MailTo, MessageInfo:string) of object;
   TDX_SMTPSenderMessage=procedure(MessageFromServer:string) of object;
   //  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, pTimeCritical);

   TDXSMTPRelay=class(TDXComponent)
   private
      MyRemoveCriticalSection:TDXCritical;
      MyCleanupCriticalSection:TDXCritical;
      MyPendingCriticalSection:TDXCritical;
      MyAddCriticalSection:TDXCritical;
      fDXMXResolver:TDXMXResolver;
      fQueue:TList;
      InExecute:Boolean;
      FInterval:integer;
      FEnabled:boolean;
      fRunning:Boolean;
      fAlternativeDNS:string;
      fTimeout:Cardinal;
      fHELO:string;
      fMaxTries:Integer;
      fSessionErrors:Integer;
      fResolveErrors:Integer;
      FPriority:TThreadPriority;
      fSmartHostAddress:string;
      fSmartHostPort:Integer;
      fConnectFailed:TDX_ConnectFailed;
      fConnected:TDX_Connected;
      fMessageSent:TDX_MsgSent;
      fSessionMessage:TDX_SessionError;
      fSendDataNow:TDX_SendDATA;
      fTriesExceeded:TDX_TriesExceeded;
      fSaveQueue:TDX_QueueCall;
      fLoadQueue:TDX_QueueCall;
      fResolve:TDX_Resolve;
      fResolveFailed:TDX_ResolveFailed;
      fCleaningQueue:TNotifyEvent;
      fDoneProcessing:TNotifyEvent;
      fSMTPSenderMessage:TDX_SMTPSenderMessage;
      fConcurrent:Integer;
      fDeliveryThread:TList;
      fCleanOutBoundQueue:TDX_CleanOutQueue;
   protected
      procedure SMTPSenderMessages(MessageFromServer:string);
      procedure setEnabled(b:boolean);
      procedure setAlternativeDNS(value:string);
      procedure setTimeout(value:cardinal);
      procedure Execute;
      function MailServer(RC:PDXSMTPRelayControl):Boolean;
      procedure TrySendingEmail(RC:Pointer);
      procedure RemoveThread(Sender:TObject);
      procedure DNSResolveAll;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure AddToQueue(const MailFrom, MailTo, MessageInfo:string);
      procedure ResumeProcessing;
      function ConnectionsPending:Integer;
   published
      property IsRunningStill:Boolean read fRunning;
      property AlternativeDNS:string read fAlternativeDNS
         write SetAlternativeDNS;
      property Enabled:boolean read FEnabled
         write setEnabled;
      property SmartHostAddress:string read fSmartHostAddress
         write fSmartHostAddress;
      property SmartHostPort:Integer read fSmartHostPort
         write fSmartHostPort;
      property HELO:string read fHELO
         write fHELO;
      property MaxTries:Integer read FMaxTries
         write FMaxtries;
      property MaxSessionErrors:Integer read FSessionErrors
         write FSessionErrors;
      property MaxResolverErrors:Integer read FResolveErrors
         write FResolveErrors;
      property Interval:integer read FInterval
         write FInterval;
      property Timeout:Cardinal read fTimeout
         write SetTimeout;
      property ThreadPriority:TThreadPriority read FPriority
         write FPriority default tpNormal;
      property ConnectFailed:TDX_ConnectFailed read fConnectFailed
         write fConnectFailed;
      property Connected:TDX_Connected read fConnected
         write fConnected;
      property MessageSent:TDX_MsgSent read fMessageSent
         write fMessageSent;
      property SessionError:TDX_SessionError read fSessionMessage
         write fSessionMessage;
      property SendDataNow:TDX_SendDATA read fSendDataNow
         write fSendDataNow;
      property TriesExceeded:TDX_TriesExceeded read fTriesExceeded
         write fTriesExceeded;
      property LoadQueue:TDX_QueueCall read fLoadQueue
         write fLoadQueue;
      property SaveQueue:TDX_QueueCall read fSaveQueue
         write fSaveQueue;
      property Resolving:TDX_Resolve read fResolve
         write fResolve;
      property ResolveFailed:TDX_ResolveFailed read fResolveFailed
         write fResolveFailed;
      property DoneProcessing:TNotifyEvent read fDoneProcessing
         write fDoneProcessing;
      property CleaningQueue:TNotifyEvent read fCleaningQueue
         write fCleaningQueue;
      property ServerResults:TDX_SMTPSenderMessage read fSMTPSenderMessage
         write fSMTPSenderMessage;
      property MaxConcurrentSessions:Integer read fConcurrent
         write fConcurrent;
      property CleanOutboundQueue:TDX_CleanOutQueue read fCleanOutBoundQueue
         write fCleanOutBoundQueue;
   end;

implementation

uses
   DXThreads4Components,
   SysUtils;

type
   TSMTPRelayThread=class(TThread)
   private
      TT:TDXSMTPRelay;
   protected
   public
      constructor CreateTimerThread(TT:TDXSMTPRelay);
      procedure Execute; override;
   end;
   PSMTPRelaySessionThread=^TSMTPRelaySessionThread;
   TSMTPRelaySessionThread=record
      DeliverySession:TDXComponentSpawn;
      DXSMTPSender:TDXSMTPSender;
      ID:DWord;
      DXSMTPRelayControl:PDXSMTPRelayControl;
   end;

constructor TDXSMTPRelay.Create(AOwner:TComponent);
begin
   fTimeout:=120000;
   inherited Create(AOwner);
   MyRemoveCriticalSection:=TDXCritical.Create;
   MyCleanupCriticalSection:=TDXCritical.Create;
   MyPendingCriticalSection:=TDXCritical.Create;
   MyAddCriticalSection:=TDXCritical.Create;
   fDXMXResolver:=TDXMXResolver.Create(nil);
   fQueue:=TList.Create;
   fDXMXResolver.UseUDP:=True;
   fDeliveryThread:=TList.Create;
   fEnabled:=False;
   fRunning:=False;
   fInterval:=15000;
   fAlternativeDNS:='';
   fMaxTries:=30;
   fSessionErrors:=10;
   fResolveErrors:=20;
   fConcurrent:=3;
   fSmartHostPort:=25;
end;

destructor TDXSMTPRelay.Destroy;
var
   SMTPRelayControl:PDXSMTPRelayControl;
   SMTPRelaySessionThread:PSMTPRelaySessionThread;

begin
   fEnabled:=False;
   while fRunning do
      DoSleepEx(fInterval);
   if Assigned(fDXMXResolver) then begin
      fDXMXResolver.Free;
      fDXMXResolver:=nil;
   end;
   if Assigned(fQueue) then begin
      while fQueue.Count>0 do begin
         SMTPRelayControl:=fQueue[0];
         if Assigned(SMTPRelayControl^.MailServer) then begin
            SMTPRelayControl^.MailServer.Free;
            SMTPRelayControl^.MailServer:=nil;
         end;
         Dispose(SMTPRelayControl);
         fQueue.Delete(0);
      end;
      fQueue.Free;
      fQueue:=nil;
   end;
   if Assigned(fDeliveryThread) then begin
      while fDeliveryThread.Count>0 do begin
         SMTPRelaySessionThread:=fDeliveryThread[0];
         if Assigned(SMTPRelaySessionThread.DeliverySession) then begin
            SMTPRelaySessionThread.DeliverySession.Terminate;
            SMTPRelaySessionThread.DeliverySession.WaitFor;
         end;
         SMTPRelaySessionThread.DeliverySession.Free;
         SMTPRelaySessionThread.DXSMTPSender.DisconnectFromSMTPServer;
         SMTPRelaySessionThread.DXSMTPSender.Free;
         fDeliveryThread.Delete(0);
      end;
      fDeliveryThread.Free;
      fDeliveryThread:=nil;
   end;
   MyRemoveCriticalSection.Free;
   MyCleanupCriticalSection.Free;
   MyPendingCriticalSection.Free;
   MyAddCriticalSection.Free;
   inherited Destroy;
end;

procedure TDXSMTPRelay.setEnabled(b:boolean);
begin
   {$IFNDEF OBJECTS_ONLY}
   if (csDesigning in ComponentState) then
      exit
   else
      {$ENDIF}
      FEnabled:=b;
   if B and not fRunning and InExecute=False then begin
      fRunning:=True;
      with TSMTPRelayThread.CreateTimerThread(self) do begin
         Resume;
      end;
   end;
end;

function TDXSMTPRelay.MailServer(RC:PDXSMTPRelayControl):Boolean;
var
   StrList:TStringList;
   Ws:string;

begin
   Result:=False;
   StrList:=TStringList.Create;
   Ws:=GetActualEmailAddress(RC^.MailTo, '*');
   Delete(Ws, 1, CharPos('@', Ws));
   if fDXMXResolver.Resolve(Ws, StrList) then begin
      RC^.MailServer:=TStringList.Create;
      Result:=True;
      while StrList.Count>0 do begin
         Ws:=StrList[0];
         Delete(Ws, 1, CharPos(#32, Ws));
         RC^.MailServer.Add(Ws);
         StrList.Delete(0);
      end;
   end;
   StrList.Free;
   StrList:=nil;
end;

procedure TDXSMTPRelay.TrySendingEmail(RC:Pointer);
var
   Ws:string;
   Handled:Boolean;
   SMTPRelaySessionThread:PSMTPRelaySessionThread;

begin
   SMTPRelaySessionThread:=fDeliveryThread[PDXSMTPRelayControl(RC)^.ID-1];
   with PDXSMTPRelayControl(RC)^ do begin
      if MailServer.Count=0 then begin
         Ws:=GetActualEmailAddress(MailTo, '*');
         Delete(Ws, 1, CharPos('@', Ws));
         MailServer.Add(Ws);
      end;
      if LastMailServerTried>=MailServer.Count then LastMailServerTried:=0;
      {$IFDEF SMTP_SESSION_FEATURE}
      SMTPRelaySessionThread^.DXSMTPSender.SessionID:=MessageInfo;
      {$ENDIF}
      if SMTPRelaySessionThread^.DXSMTPSender.ConnectToSMTPServer(
         MailServer[LastMailServerTried], 25, fHELO) then begin
         if Copy(SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage, 1,
            1)<>'2' then begin
            if Assigned(fSessionMessage) then begin
               fSessionMessage(MailServer[LastMailServerTried], MessageInfo,
                  SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage);
               ProcessWindowsMessageQueue;
            end;
            SMTPRelaySessionThread^.DXSMTPSender.DisconnectFromSMTPServer;
            Inc(LastMailServerTried);
            Inc(SessionErrors);
            Exit;
         end;
         if Assigned(fConnected) then begin
            fConnected(MailServer[LastMailServerTried], MessageInfo);
            ProcessWindowsMessageQueue;
         end;
         if SMTPRelaySessionThread^.DXSMTPSender.MailFROM(MailFrom) then begin
            if SMTPRelaySessionThread^.DXSMTPSender.RcptTo(MailTo) then begin
               Handled:=False;
               if Assigned(fSendDataNow) then begin
                  fSendDataNow(SMTPRelaySessionThread^.DXSMTPSender,
                     MessageInfo, Handled);
                  ProcessWindowsMessageQueue;
               end;
               if (Handled)and
                  (Copy(SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage,
                     1, 1)='2') then begin
                  ConnectTries:=-1;
                  SMTPRelaySessionThread^.DXSMTPSender.DisconnectFromSMTPServer;
                  if Assigned(fCleanOutBoundQueue) then
                     fCleanOutBoundQueue(RecordKey);
               end
               else begin
                  if Assigned(fSessionMessage) then begin
                     fSessionMessage(MailServer[LastMailServerTried],
                        MessageInfo,
                        SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage);
                     ProcessWindowsMessageQueue;
                  end;
                  SMTPRelaySessionThread^.DXSMTPSender.DisconnectFromSMTPServer;
                  Inc(LastMailServertried);
                  Inc(SessionErrors);
               end;
            end// rcpt
            else begin
               if Assigned(fSessionMessage) then begin
                  fSessionMessage(MailServer[LastMailServerTried],
                     MessageInfo,
                     SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage);
                  ProcessWindowsMessageQueue;
               end;
               Inc(SessionErrors);
            end;
         end// mailfrom
         else begin
            if Assigned(fSessionMessage) then begin
               fSessionMessage(MailServer[LastMailServerTried], MessageInfo,
                  SMTPRelaySessionThread^.DXSMTPSender.LastSenderMessage);
               ProcessWindowsMessageQueue;
            end;
            Inc(SessionErrors);
         end;
      end// connect
      else begin
         if Assigned(fConnectFailed) then begin
            fConnectFailed(MailServer[LastMailServerTried], MessageInfo);
            ProcessWindowsMessageQueue;
         end;
         Inc(LastMailServerTried);
      end;
   end; // with
end;

procedure TDXSMTPRelay.RemoveThread(Sender:TObject);
var
   ID:DWord;

begin
   MyRemoveCriticalSection.StartingWrite;
   ID:=TDXComponentSpawn(Sender).ID;
   if ID=0 then Exit;
   Dec(ID);
   with PSMTPRelaySessionThread(fDeliveryThread[ID])^ do begin
      if Assigned(DeliverySession) then begin
         DeliverySession:=TDXComponentSpawn.CreateThread(nil);
         DeliverySession.OnTimer:=TrySendingEmail;
         DeliverySession.OnTerminate:=RemoveThread;
         DeliverySession.Priority:=fPriority;
      end;
      DXSMTPSender.DisconnectFromSMTPServer;
      ID:=0;
   end;
   MyRemoveCriticalSection.FinishedWrite;
end;

procedure TDXSMTPRelay.DNSResolveAll;
var
   Loop, MaxLoop:Integer;

begin
   Loop:=0;
   MaxLoop:=fQueue.Count;
   while (Loop<MaxLoop)and(fEnabled) do begin
      if not Assigned(PDXSMTPRelayControl(fQueue[Loop])^.MailServer) then
         if fSmartHostAddress<>'' then begin
            PDXSMTPRelayControl(fQueue[Loop])^.MailServer:=TStringList.Create;
            PDXSMTPRelayControl(fQueue[Loop])^.MailServer.Add(fSmartHostAddress);
         end
         else if MailServer(PDXSMTPRelayControl(fQueue[Loop])) then begin
            if Assigned(fResolve) then begin
               fResolve(PDXSMTPRelayControl(fQueue[Loop])^.MailTo,
                  PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo);
               ProcessWindowsMessageQueue;
            end;
         end
         else begin
            if Assigned(fResolveFailed) then begin
               Inc(PDXSMTPRelayControl(fQueue[Loop])^.ResolveErrors);
               fResolveFailed(PDXSMTPRelayControl(fQueue[Loop])^.MailTo,
                  PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo);
               ProcessWindowsMessageQueue;
            end;
         end;
      Inc(loop);
   end;
end;

procedure TDXSMTPRelay.Execute;
var
   Loop,
      MaxLoop:Integer;
   TmpAddress:TStringList;
   IsBusy:Boolean;
   SMTPRelaySessionThread:PSMTPRelaySessionThread;
   FindLoop:Integer;
   Running:Boolean;

begin
   if InExecute=True then exit;
   InExecute:=True;
   if fQueue.Count<fConcurrent then fConcurrent:=fQueue.Count;
   while (fDeliveryThread.Count<fConcurrent)and(fQueue.Count>0) do begin
      New(SMTPRelaySessionThread);
      with SMTPRelaySessionThread^ do begin
         DeliverySession:=TDXComponentSpawn.CreateThread(nil);
         DeliverySession.OnTimer:=TrySendingEmail;
         DeliverySession.OnTerminate:=RemoveThread;
         DeliverySession.Priority:=fPriority;
         DXSMTPSender:=TDXSMTPSender.Create(nil);
         DXSMTPSender.Timeout:=fTimeout;
         DXSMTPSender.SMTPSenderMessage:=SMTPSenderMessages;
         ID:=0;
      end;
      fDeliveryThread.Add(SMTPRelaySessionThread);
   end;
   while (fEnabled)and(fQueue.Count>0) do begin
      MaxLoop:=fQueue.Count;
      FindLoop:=0;
      for Loop:=0 to MaxLoop-1 do begin
         if PDXSMTPRelayControl(fQueue[Loop])^.ConnectTries>-1 then begin
            if not Assigned(PDXSMTPRelayControl(fQueue[Loop])^.MailServer) then
               begin
               if MailServer(PDXSMTPRelayControl(fQueue[Loop])) then begin
                  if Assigned(fResolve) then begin
                     fResolve(PDXSMTPRelayControl(fQueue[Loop])^.MailTo,
                        PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo);
                     ProcessWindowsMessageQueue;
                  end;
               end
               else begin
                  if Assigned(fResolveFailed) then begin
                     Inc(PDXSMTPRelayControl(fQueue[Loop])^.ResolveErrors);
                     fResolveFailed(PDXSMTPRelayControl(fQueue[Loop])^.MailTo,
                        PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo);
                     ProcessWindowsMessageQueue;
                  end;
               end;
            end;
         end;
         with PDXSMTPRelayControl(fQueue[Loop])^ do begin
            if ConnectTries>-1 then begin
               if (ConnectTries<fMaxTries)and
                  (SessionErrors<fSessionErrors)and
                  (ResolveErrors<fResolveErrors)and
                  Assigned(MailServer) then begin
                  Running:=False;
                  while not Running do begin
                     SMTPRelaySessionThread:=fDeliveryThread[FindLoop];
                     with SMTPRelaySessionThread^ do begin
                        if DeliverySession.Suspended then begin
                           DeliverySession.ID:=FindLoop+1;
                           PDXSMTPRelayControl(fQueue[Loop])^.ID:=FindLoop+1;
                           DeliverySession.SetSessionData(fQueue[Loop]);
                           DeliverySession.Resume;
                           SMTPRelaySessionThread^.ID:=FindLoop+1;
                           Running:=True;
                        end;
                        Inc(FindLoop);
                        if FindLoop>=fDeliveryThread.Count then begin
                           FindLoop:=0;
                           if not PSMTPRelaySessionThread(
                              fDeliveryThread[FindLoop])^.
                              DeliverySession.
                              Suspended then DoSleepEx(fTimeout);
                        end;
                     end;
                  end;
               end; {if}
            end; {if}
         end; {with}
      end; {for}
      IsBusy:=False;
      FindLoop:=0;
      while (FindLoop<fDeliveryThread.Count)and(fEnabled) do begin
         SMTPRelaySessionThread:=fDeliveryThread[FindLoop];
         if SMTPRelaySessionThread^.ID<>0 then IsBusy:=True;
         Inc(FindLoop);
         if (FindLoop=fDeliveryThread.Count)and(IsBusy) then begin
            IsBusy:=False;
            FindLoop:=0;
            DoSleepEx(1000);
         end;
      end;
      if Assigned(fCleaningQueue) then begin
         fCleaningQueue(Self);
         ProcessWindowsMessageQueue;
      end;
      for Loop:=0 to MaxLoop-1 do begin
         if (PDXSMTPRelayControl(fQueue[Loop])^.ConnectTries>=fMaxTries)or
            (PDXSMTPRelayControl(fQueue[Loop])^.SessionErrors>=fSessionErrors)or
            (PDXSMTPRelayControl(fQueue[Loop])^.ResolveErrors>=fResolveErrors)
               then begin
            if Assigned(fTriesExceeded) then begin
               fTriesExceeded(PDXSMTPRelayControl(fQueue[Loop]));
               ProcessWindowsMessageQueue;
            end;
            PDXSMTPRelayControl(fQueue[Loop])^.ConnectTries:=-1;
         end;
      end;
      MyCleanupCriticalSection.StartingWrite;
      try
         TmpAddress:=TStringList.Create;
         TmpAddress.Sorted:=True;
         Loop:=0;
         while Loop<fQueue.Count do begin
            if PDXSMTPRelayControl(fQueue[Loop])^.ConnectTries=-1 then begin
               if
                  TmpAddress.IndexOf(PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo)=-1 then
                  TmpAddress.Add(PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo);
               if Assigned(PDXSMTPRelayControl(fQueue[Loop])^.MailServer) then
                  begin
                  PDXSMTPRelayControl(fQueue[Loop])^.MailServer.Free;
                  PDXSMTPRelayControl(fQueue[Loop])^.MailServer:=nil;
               end;
               Dispose(PDXSMTPRelayControl(fQueue[Loop]));
               fQueue.Delete(Loop);
            end
            else
               Inc(Loop);
         end;
         MaxLoop:=fQueue.Count;
         Loop:=0;
         while (Loop<MaxLoop)and(TmpAddress.Count>0) do begin
            if
               TmpAddress.IndexOf(PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo)>-1
               then
               TmpAddress.Delete(TmpAddress.IndexOf(PDXSMTPRelayControl(fQueue[Loop])^.MessageInfo));
            Inc(Loop);
         end;
         TmpAddress.Sorted:=False;
         while (TmpAddress.Count)>0 do begin
            if Assigned(fMessageSent) then begin
               fMessageSent(TmpAddress[0]);
               ProcessWindowsMessageQueue;
            end;
            TmpAddress.Delete(0);
         end;
         if Assigned(fSaveQueue) then fSaveQueue(fQueue);
         if fQueue.Count>0 then begin
            while fQueue.Count>0 do begin
               if Assigned(PDXSMTPRelayControl(fQueue[0])^.MailServer) then begin
                  PDXSMTPRelayControl(fQueue[0])^.MailServer.Free;
                  PDXSMTPRelayControl(fQueue[0])^.MailServer:=nil;
               end;
               Dispose(PDXSMTPRelayControl(fQueue[0]));
               fQueue.Delete(0);
            end;
            if Assigned(fLoadQueue) then begin
               fLoadQueue(fQueue);
               DNSResolveAll;
            end;
         end;
      except
      end;
      MyCleanupCriticalSection.FinishedWrite;
      DoSleepEx(FInterval);
   end;
   fRunning:=False;
   while fDeliveryThread.Count>0 do begin
      SMTPRelaySessionThread:=fDeliveryThread[0];
      SMTPRelaySessionThread^.DeliverySession.Free;
      SMTPRelaySessionThread^.DXSMTPSender.Free;
      fDeliveryThread.Delete(0);
   end;
   InExecute:=False;
end;

procedure TDXSMTPRelay.setAlternativeDNS(value:string);
begin
   fAlternativeDNS:=Value;
   {$IFNDEF OBJECTS_ONLY}
   if (csDesigning in ComponentState) then exit;
   {$ENDIF}
   fDXMXResolver.AlternativeDNS:=Value;
end;

procedure TDXSMTPRelay.setTimeout(value:cardinal);
begin
   fTimeout:=Value;
end;

function TDXSMTPRelay.ConnectionsPending:Integer;
var
   Loop,
      MaxLoop:Integer;

begin
   Result:=0;
   if Assigned(fQueue) then begin
      Loop:=0;
      MyPendingCriticalSection.StartingWrite;
      try
         MaxLoop:=fQueue.Count;
         while Loop<MaxLoop do begin
            if PDXSMTPRelayControl(fQueue[Loop])^.ConnectTries>-1 then
               Inc(Result);
            Inc(loop);
         end;
      except
      end;
      MyPendingCriticalSection.FinishedWrite;
   end
   else
      Result:=0;
end;

procedure TDXSMTPRelay.AddToQueue(const MailFrom, MailTo, MessageInfo:string);
var
   SMTPRelayControl:PDXSMTPRelayControl;
   MailAddress:string;
   I:Integer;

begin
   MailAddress:=StringReplace(MailTo, ',', ';', [rfReplaceAll])+';';
   I:=CharPos(';', MailAddress);
   while I>0 do begin
      New(SMTPRelayControl);
      SMTPRelayControl^.MailTo:=Copy(MailAddress, 1, CharPos(';',
         MailAddress)-1);
      SMTPRelayControl^.MailFrom:=MailFrom;
      SMTPRelayControl^.MessageInfo:=MessageInfo;
      SMTPRelayControl^.ConnectTries:=0;
      SMTPRelayControl^.LastMailServerTried:=0;
      SMTPRelayControl^.SessionErrors:=0;
      SMTPRelayControl^.ResolveErrors:=0;
      SMTPRelayControl^.MailServer:=nil;
      MyAddCriticalSection.StartingWrite;
      try
         fQueue.Add(SMTPRelayControl);
      except
         ;
      end;
      MyAddCriticalSection.FinishedWrite;
      Delete(MailAddress, 1, I);
      I:=CharPos(';', MailAddress);
   end;
end;

procedure TDXSMTPRelay.ResumeProcessing;
begin
   if fQueue.Count<1 then
      if Assigned(fLoadQueue) then fLoadQueue(fQueue);
   SetEnabled(True);
end;

constructor TSMTPRelayThread.CreateTimerThread(TT:TDXSMTPRelay);
begin
   inherited Create(true);
   self.tt:=tt;
   FreeOnTerminate:=true;
end;

procedure TSMTPRelayThread.Execute;
begin
   TT.Execute;
   if Assigned(TT.fDoneProcessing) then
      TT.fDoneProcessing(Self);
   TT.fRunning:=False;
   Terminate;
end;

procedure TDXSMTPRelay.SMTPSenderMessages(MessageFromServer:string); 
begin
   if Assigned(fSMTPSenderMessage) then begin
      fSMTPSenderMessage(MessageFromServer);
      ProcessWindowsMessageQueue;
   end;
end;

end.

