unit DXTelnetServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXTelnetServerCore
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
//  Description: implements TELNET protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

Const
  TELNET_IAC   = #255;                                 { Interpret as Command }
  TELNET_DONT  = #254;     { Stop performing, or not expecting him to perform }
  TELNET_DO    = #253;                    { Perform, or expect him to perform }
  TELNET_WONT  = #252;                                   { Refusal to perform }
  TELNET_WILL  = #251;                                    { Desire to perform }

  TELNET_SB    = #250;   { What follow is sub-negotiation of indicated option }
  TELNET_GA    = #249;                                      { Go ahead signal }
  TELNET_EL    = #248;                                  { Erase Line function }
  TELNET_EC    = #247;                             { Erase Character function }
  TELNET_AYT   = #246;                               { Are You There function }
  TELNET_AO    = #245;                                { Abort Output function }
  TELNET_IP    = #244;                           { Interrupt Process function }
  TELNET_BRK   = #243;                                  { NVT break character }
  TELNET_DM    = #242;                       { Data stream portion of a Synch }
  TELNET_NOP   = #241;                                         { No operation }
  TELNET_SE    = #240;                    { End of sub-negotiation parameters }
  TELNET_EOR   = #239;                                        { End of record }
  TELNET_ABORT = #238;                                        { Abort process }
  TELNET_SUSP  = #237;                              { Suspend current process }
  TELNET_EOF   = #236;                                          { End of file }

  TELNETOPT_BINARY = #0;                                    { Transmit binary }
  TELNETOPT_ECHO   = #1;                                          { Echo mode }
  TELNETOPT_SUPGA  = #3;                                  { Suppress Go-Ahead }
  TELNETOPT_TERM   = #24;                                     { Terminal Type }
  TELNETOPT_SPEED  = #32;                                    { Terminal Speed }

Type
  TDXTelnetServerCore = class(TDXServerCore)
  private
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    Procedure DoNegotiation(ClientThread:TDXClientThread);
    Procedure SayWill(ClientThread:TDXClientThread;Option:Char);
    Procedure SayWont(ClientThread:TDXClientThread;Option:Char);
    Procedure SayDo(ClientThread:TDXClientThread;Option:Char);
    Procedure SayDont(ClientThread:TDXClientThread;Option:Char);
  published
  end;

implementation

Uses
   DXString, DXSock;

constructor TDXTelnetServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=23;
end;

destructor TDXTelnetServerCore.Destroy;
begin
  inherited Destroy;
end;

Procedure TDXTelnetServerCore.SayWill(ClientThread:TDXClientThread;Option:Char);
Begin
   ClientThread.Socket.Write(TELNET_IAC+TELNET_WILL+Option);
End;

Procedure TDXTelnetServerCore.SayWont(ClientThread:TDXClientThread;Option:Char);
Begin
   ClientThread.Socket.Write(TELNET_IAC+TELNET_WONT+Option);
End;

Procedure TDXTelnetServerCore.SayDo(ClientThread:TDXClientThread;Option:Char);
Begin
   ClientThread.Socket.Write(TELNET_IAC+TELNET_DO+Option);
End;

Procedure TDXTelnetServerCore.SayDont(ClientThread:TDXClientThread;Option:Char);
Begin
   ClientThread.Socket.Write(TELNET_IAC+TELNET_DONT+Option);
End;

Procedure TDXTelnetServerCore.DoNegotiation(ClientThread:TDXClientThread);
Var
   Ch:Char;
   StartTime:Comp;
   Ws:String;
   Done:Boolean;

Begin
   ClientThread.Socket.Write(Telnet_IAC+Telnet_Do+TELNETOPT_TERM);
   StartTime:=TimeCounter+5000;
   While not DXString.Timeout(StartTime) and
      (ClientThread.Socket.CharactersToRead<1) do Begin
      ProcessWindowsMessageQueue;
      DoSleepEx(1);
   End;
   Done:=False;
   Repeat
      Ch:=ClientThread.Socket.PeekChar;
      if Ch<>Telnet_IAC then Exit
      Else Begin
         // what do they have for us?
         ClientThread.Socket.Read; // digest IAC
         Ch:=ClientThread.Socket.Read;
         Case Ch of
            TELNET_DONT:Begin
            End;
            TELNET_DO:Begin
            End;
            TELNET_WONT:Begin
            End;
            TELNET_WILL:Begin
            End;
         End;
         Ws:=ClientThread.Socket.PeekString;
         Ws:=HexDump(Ws);
         Ws:='';
      End;
   Until Done;
End;

end.

