unit DXSMTPSender;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSMTPSender
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
   DXString,
   Classes,
   DXSock;

{$I DXAddons.def}

type
   TDXSMTPSenderMessage=procedure(MessageFromServer:string) of object;
   TDXSMTPSender=class(TDXComponent)
   private
      fSMTPSenderMessage:TDXSMTPSenderMessage;
      fLastSenderMessage:string;
      Socket:TDXSock;
      fiTimeout:Cardinal;
      fSessionID:string;
   protected
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function ConnectToSMTPServer(Host:string; Port:Integer;
         HELO:string):Boolean;
      function MailFROM(Name:string):Boolean;
      function RcptTo(Name:string):Boolean;
      function SendMessage(Stream:TStream):Boolean;
      function SendMessageFromWeb(EncodedString:string):Boolean;
      function Reset:Boolean;
      procedure DisconnectFromSMTPServer;
   published
      property Timeout:Cardinal read fiTimeout
         write fiTimeout;
      {$IFDEF SMTP_SESSION_FEATURE}
      property SessionID:string read fSessionID
         write fSessionID;
      {$ENDIF}
      property SMTPSenderMessage:TDXSMTPSenderMessage read fSMTPSenderMessage
         write fSMTPSenderMessage;
      property LastSenderMessage:string read fLastSenderMessage;
   end;

implementation

uses
   SysUtils,
   DXSocket;

///////////////////////////////////////////////////////////////////////////////
//CREATE:
//       Define the Default Port number to Listen On.
///////////////////////////////////////////////////////////////////////////////
constructor TDXSMTPSender.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   Socket:=TDXSock.Create(Nil);
   Socket.OutputBufferSize:=bsfHUGE;
   fiTimeout:=30000;
   fSessionID:='';
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Destory this object.
///////////////////////////////////////////////////////////////////////////////
destructor TDXSMTPSender.Destroy;
begin
   if Assigned(Socket) then begin
      Socket.Free;
      Socket:=nil;
   end;
   inherited Destroy;
end;

function TDXSMTPSender.ConnectToSMTPServer(Host:string; Port:Integer;
   HELO:string):Boolean;
var
   NewConnect:PNewConnect;
   Ws:string;

begin
   New(NewConnect);
   NewConnect.Port:=Port;
   NewConnect.UseNAGLE:=True;
   NewConnect.UseUDP:=False;
   NewConnect.UseBlocking:=True;
   NewConnect.ipAddress:=Host;
   if Socket.Connect(NewConnect) then begin
      Result:=True;
      repeat
         Ws:=Socket.Readln(fiTimeout);
         if Ws<>'' then begin
            fLastSenderMessage:=Ws;
            if Assigned(fSMTPSenderMessage) then
               if fSessionID<>'' then
                  fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (introduction)')
               else
                  fSMTPSenderMessage(Ws+' (introduction)');
         end;
      until (Socket.CharactersToRead<1);
      if Copy(Ws, 1, 1)='2' then begin
         if HELO='' then HELO:='localhost';
         Socket.Writeln('HELO '+HELO);
         Ws:=Socket.Readln(fiTimeout);
         if Ws<>'' then begin
            fLastSenderMessage:=Ws;
            if Assigned(SMTPSenderMessage) then
               SMTPSenderMessage(Ws+' (helo)');
         end;
      end
   end
   else
      Result:=False;
   Dispose(NewConnect);
end;

function TDXSMTPSender.MailFROM(Name:string):Boolean;
var
   Ws:string;

begin
   Socket.Writeln('MAIL FROM: <'+Name+'>');
   Ws:=Socket.Readln(fiTimeout);
   if Ws<>'' then begin
      fLastSenderMessage:=Ws;
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (mail)')
         else
            fSMTPSenderMessage(Ws+' (mail)');
   end;
   if Copy(Ws, 1, 1)='2' then
      Result:=True
   else
      Result:=False;
end;

function TDXSMTPSender.RcptTo(Name:string):Boolean;
var
   Ws:string;

begin
   Socket.Writeln('RCPT TO: <'+Name+'>');
   Ws:=Socket.Readln(fiTimeout);
   if Ws<>'' then begin
      fLastSenderMessage:=Ws;
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (rcpt)')
         else
            fSMTPSenderMessage(Ws+' (rcpt)');
   end;
   if Copy(Ws, 1, 1)='2' then
      Result:=True
   else
      Result:=False;
end;

function TDXSMTPSender.SendMessage(Stream:TStream):Boolean;
var
   Ws:string;

begin
   Socket.Writeln('DATA');
   Ws:=Socket.Readln(fiTimeout);
   if Ws<>'' then begin
      fLastSenderMessage:=Ws;
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (data)')
         else
            fSMTPSenderMessage(Ws+' (data)');
   end;
   if Copy(Ws, 1, 1)<>'3' then begin
      Result:=False;
      Exit;
   end;
   Stream.Seek(0, 0);
   {$IFDEF VER100}
   if Socket.SendFromStream(Stream) then begin
      {$ELSE}
   if Socket.SendFrom(Stream) then begin
      {$ENDIF}
      Socket.Writeln(#13#10+'.');
   end
   else begin
      if Socket.LastCommandStatus<>0 then
         fLastSenderMessage:=Socket.GetErrorDesc(Socket.LastCommandStatus)
      else
         fLastSenderMessage:='Socket Dead (offset='+IntToCommaStr(Stream.Position)+','+IntToCommaStr(Stream.Size)+')';
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+fLastSenderMessage+' (error)')
         else
            fSMTPSenderMessage(fLastSenderMessage+' (error)');
      Result:=False;
      Exit;
   end;
   Ws:=Socket.Readln(fiTimeout);
   fLastSenderMessage:=Ws+' (.)';
   if Ws<>'' then begin
      if Assigned(SMTPSenderMessage) then
         SMTPSenderMessage(fLastSenderMessage);
   end;
   if Copy(Ws, 1, 1)='2' then
      Result:=True
   else
      Result:=False;
end;

function TDXSMTPSender.SendMessageFromWeb(EncodedString:string):Boolean;
var
   Ws:string;

begin
   Socket.Writeln('DATA');
   Ws:=Socket.Readln(fiTimeout);
   if Ws<>'' then begin
      fLastSenderMessage:=Ws;
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (data)')
         else
            fSMTPSenderMessage(Ws+' (data)');
   end;
   if Copy(Ws, 1, 1)<>'3' then begin
      Result:=False;
      Exit;
   end;
   EncodedString:=EscapeDecode(EncodedString);
   EncodedString:=StringReplace(EncodedString, '&', #13#10#13#10,
      [rfReplaceAll]);
   EncodedString:=StringReplace(EncodedString, '=', #13#10, [rfReplaceAll]);
   Socket.Writeln(EncodedString+#13#10+'.');
   if Socket.LastCommandStatus<>0 then begin
      fLastSenderMessage:=Socket.GetErrorDesc(Socket.LastCommandStatus);
      if Assigned(fSMTPSenderMessage) then
         if fSessionID<>'' then
            fSMTPSenderMessage('[SessionID='+fSessionID+'] '+fLastSenderMessage+' (error)')
         else
            fSMTPSenderMessage(fLastSenderMessage+' (error)');
   end;
   Ws:=Socket.Readln(fiTimeout);
   fLastSenderMessage:=Ws+' (.)';
   if Ws<>'' then begin
      if Assigned(SMTPSenderMessage) then
         SMTPSenderMessage(fLastSenderMessage);
   end;
   if Copy(Ws, 1, 1)='2' then
      Result:=True
   else
      Result:=False;
end;

function TDXSMTPSender.Reset:Boolean;
var
   Ws:string;

begin
   Socket.WriteLn('RSET');
   Ws:=Socket.Readln(fiTimeout);
   if Copy(Ws, 1, 1)='2' then
      Result:=True
   else
      Result:=False;
end;

procedure TDXSMTPSender.DisconnectFromSMTPServer;
var
   Ws:string;

begin
   Socket.WriteLn('QUIT');
   DoSleepEx(1);
   if Socket.CharactersToRead>0 then begin
      Ws:=Socket.Readln(fiTimeout);
      if Ws<>'' then begin
         fLastSenderMessage:=Ws;
         if Assigned(fSMTPSenderMessage) then
            if fSessionID<>'' then
               fSMTPSenderMessage('[SessionID='+fSessionID+'] '+Ws+' (quit)')
            else
               fSMTPSenderMessage(Ws+' (quit)');
      end;
   end
   else
      fLastSenderMessage:='<<Disconnected>>';
   Socket.CloseGracefully; 
end;

end.

