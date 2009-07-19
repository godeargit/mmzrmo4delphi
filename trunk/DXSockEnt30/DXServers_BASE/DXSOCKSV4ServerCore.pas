unit DXSOCKSV4ServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSOCKSV4ServerCore
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
//  Description: implements SOCKS v4 protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXString,
  DXServerCore;

{$I DXSock.def}

type
  SOCKSV4TConnectEvent = procedure(ClientThread:TDXClientThread;DestAddr:String;DestIP:Longint;DestPort:Word;UserID:String) of object;
  SOCKSV4TBindEvent = procedure(ClientThread:TDXClientThread;DestAddr:String;DestIP:Longint;DestPort:Word;UserID:String) of object;

  TDXSOCKSV4ServerCore = class(TDXServerCore)
  private
    fOnCommandConnect:SOCKSV4TConnectEvent;
    fOnCommandBind:SOCKSV4TBindEvent;
    fVersionID:Char;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    procedure RequestGranted(ClientThread:TDXClientThread;DestPort:Word;DestIP:Longint);
    procedure RequestFailed(ClientThread:TDXClientThread;DestPort:Word;DestIP:Longint);
  published
    property OnCommandConnect:SOCKSV4TConnectEvent read fOnCommandConnect
                                                   write fOnCommandConnect;
    property OnCommandBind:SOCKSV4TBindEvent read fOnCommandBind
                                             write fOnCommandBind;
  end;

implementation

Uses
   DXSock,
{$IFDEF LINUX}
   Libc,
{$ELSE}
   Winsock,
{$ENDIF}
   SysUtils,
   DXSocket;

constructor TDXSOCKSV4ServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=1080;
   ProtocolToBind:=wpTCPOnly;
   fVersionID:='4';
end;

destructor TDXSOCKSV4ServerCore.Destroy;
begin
   inherited Destroy;
end;

procedure TDXSOCKSV4ServerCore.RequestGranted(ClientThread:TDXClientThread;DestPort:Word;DestIP:Longint);
Var
   S:String;

Begin
   Setlength(S,6);
   DestPort:=ByteSwap2(DestPort);
   Move(DestPort,S[1],2);
   Move(DestIP,S[3],4);
   ClientThread.Socket.Writeln(fVersionID+#90+S);
End;

procedure TDXSOCKSV4ServerCore.RequestFailed(ClientThread:TDXClientThread;DestPort:Word;DestIP:Longint);
Var
   S:String;

Begin
   Setlength(S,6);
   DestPort:=ByteSwap2(DestPort);
   Move(DestPort,S[1],2);
   Move(DestIP,S[3],4);
   ClientThread.Socket.Writeln(fVersionID+#91+S);
End;

procedure TDXSOCKSV4ServerCore.ProcessSession(ClientThread: TDXClientThread);
var
   S,
   DestAddr,
   UserName:string;
   sCMD:Char;
   DestPort:Word;
   DestSockAddr:Longint;
{$IFDEF VER100}
   InAddr:TinAddr;
{$ELSE}
   InAddr:in_addr;
{$ENDIF}
   OutData:Pointer;
   Loop:Integer;
   WasHandled:Boolean;

begin
   with ClientThread.Socket do begin
      s:=ReadNull(Timeout);
      If LastReadTimeout or Not ValidSocket then Exit;
      if s='' then exit;
      If Assigned(OnFilter) then Begin
         Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
         SetLength(S,Loop);
         If Assigned(OutData) then Begin
            Move(TDXBSArray(OutData^),S[1],Loop);
            OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread); 
         End;
      End;
      If Length(s)>8 then Begin
         sCmd:=S[2];
         Move(S[3],DestPort,2);
         DestPort:=ByteSwap2(DestPort);
         Move(S[5],DestSockAddr,4);
         InAddr.S_addr:=DestSockAddr;
         UserName:=Copy(S,9,Length(S));
         If DestSockAddr<256 then Begin {SOCKSV4A}
            S:=ReadNull(Timeout);
            If Not isNumericString(S) then Begin
               DestAddr:=GetIPAddressByHost(S,1);
               If DestAddr='' then Exit;
            End
            Else DestAddr:=S;
         End
         Else
            DestAddr:=IntToStr(Ord(InAddr.S_un_b.s_b1))+'.'+
               IntToStr(Ord(InAddr.S_un_b.s_b2))+'.'+
               IntToStr(Ord(InAddr.S_un_b.s_b3))+'.'+
               IntToStr(Ord(InAddr.S_un_b.s_b4));
      End
      Else sCmd:=#0;
      Case sCmd of
         #1:If Assigned(fOnCommandConnect) then
                fOnCommandConnect(ClientThread,DestAddr,DestSockAddr,DestPort,UserName);
         #2:If Assigned(fOnCommandBind) then
                fOnCommandBind(ClientThread,DestAddr,DestSockAddr,DestPort,UserName);
      End;
   end; {with}
end; {doExecute}

end.

