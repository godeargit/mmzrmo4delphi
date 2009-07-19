unit DXSOCKSV5ServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSOCKSV5ServerCore
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
//  Description: implements SOCKS v5 protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXString,
  DXServerCore;

{$I DXSOCK.DEF}

type
  SOCKSV5TConnectEvent = procedure(ClientThread:TDXServerThread;DestAddr:String;DestIP:Longint;DestPort:Word;UserID:String) of object;
  SOCKSV5TBindEvent = procedure(ClientThread:TDXServerThread;DestAddr:String;DestIP:Longint;DestPort:Word;UserID:String) of object;

  TDXSOCKSV5ServerCore = class(TDXServerCore)
  private
    fOnCommandConnect:SOCKSV5TConnectEvent;
    fOnCommandBind:SOCKSV5TBindEvent;
    fiTimeout:DWord;
    fVersionID:Char;
  protected
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXServerThread);
    procedure RequestGranted(ClientThread:TDXServerThread;DestPort:Word;DestIP:Longint);
    procedure RequestFailed(ClientThread:TDXServerThread;DestPort:Word;DestIP:Longint);
  published
    property Timeout:DWord read fiTimeout
                           write fiTimeout;
    property OnCommandConnect:SOCKSV5TConnectEvent read fOnCommandConnect
                                                   write fOnCommandConnect;
    property OnCommandBind:SOCKSV5TBindEvent read fOnCommandBind
                                             write fOnCommandBind;
  end;

implementation

Uses
   Winsock,
   SysUtils,
   DXSocket;

{$IFDEF OBJECTS_ONLY}
constructor TDXSOCKSV5ServerCore.Create;
{$ELSE}
constructor TDXSOCKSV5ServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=1080;
   ProtocolToBind:=wpTCPOnly;
   fiTimeout:=120000;
   fVersionID:='5';
end;

destructor TDXSOCKSV5ServerCore.Destroy;
begin
   inherited Destroy;
end;

procedure TDXSOCKSV5ServerCore.RequestGranted(ClientThread:TDXServerThread;DestPort:Word;DestIP:Longint);
Var
   S:String;

Begin
   Setlength(S,6);
   Move(DestPort,S[1],2);
   Move(DestIP,S[3],4);
   ClientThread.Socket.Writeln(fVersionID+#90+S);
End;

procedure TDXSOCKSV5ServerCore.RequestFailed(ClientThread:TDXServerThread;DestPort:Word;DestIP:Longint);
Var
   S:String;

Begin
   Setlength(S,6);
   Move(DestPort,S[1],2);
   Move(DestIP,S[3],4);
   ClientThread.Socket.Writeln(fVersionID+#91+S);
End;

procedure TDXSOCKSV5ServerCore.ProcessSession(ClientThread: TDXServerThread);
var
   S,
   DestAddr,
   UserName:string;
   sCMD:Char;
   sLEN:Word;
   DestPort:Word;
   DestSockAddr:Longint;
   HostEnt:PHostEnt;
   FErrStatus:Integer;
   InAddr:in_addr;
   PacketVersion:Byte;
   OutData:Pointer;

begin
   with ClientThread.Socket do begin
      s:=ReadNull(fiTimeout);
      If LastReadTimeout or Not ValidSocket then Exit;
      if s='' then continue;
      If Assigned(OnFilter) then Begin
         Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
         SetLength(S,Loop);
         If Assigned(OutData) then Begin
            Move(TDXBSArray(OutData^),S[1],Loop);
            OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled);
         End;
      End;
      If Length(s)>8 then Begin
         PacketVersion:=Ord(S[1]);
         sCmd:=S[2];
         If sCMD in ['1','2'] then Begin
            Move(S[3],DestPort,2);
            Move(S[5],DestSockAddr,4);
            InAddr.S_addr:=DestSockAddr;
            UserName:=Copy(S,9,Length(S));
            If DestSockAddr<256 then Begin {SOCKSV4A}
               S:=ReadNull(fiTimeout);
               If Not isNumericString(S) then Begin
                  HostEnt:=GetAnyHostAndDomainNamebyHostName(S,FErrStatus);
                  If FErrStatus<>0 then Exit;
                  DestAddr:=Decode_HostEnt_IPAddress(HostEnt,1);
               End
               Else DestAddr:=S;
            End
            Else DestAddr:=IntToStr(Ord(InAddr.S_un_b.s_b1))+'.'+
                  IntToStr(Ord(InAddr.S_un_b.s_b2))+'.'+
                  IntToStr(Ord(InAddr.S_un_b.s_b3))+'.'+
                  IntToStr(Ord(InAddr.S_un_b.s_b4));
         End
         Else Begin
            If sCMD=#1 then Begin
               Move(S[3],sLEN,2);
               Delete(S,1,4);
               UserName:=S;
            End
            Else If sCMD=#10 then Begin {list command}
            End
            Else If sCMD=#3 then Begin {CHAP}
            End
            Else If sCMD=Chr($85) then Begin {MD5}
            End;
         End;
      End
      Else sCmd:=#0;
      Case sCmd of
         #1:If Assigned(fOnCommandConnect) then
                fOnCommandConnect(ClientThread,DestAddr,DestSockAddr,DestPort,UserName);
         #2:If Assigned(fOnCommandBind) then
                fOnCommandBind(ClientThread,DestAddr,DestSockAddr,DestPort,UserName);
         Else Begin
            S:='';
         End;
      End;
   end; {with}
end; {doExecute}

end.

