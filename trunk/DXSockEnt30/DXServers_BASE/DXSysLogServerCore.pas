unit DXSysLogServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSysLogServerCore
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
//  Description: implements SysLog protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  SysUtils,
  DXServerCore;

{$I DXSock.def}

type
  SysLogTOtherEvent = procedure(ClientThread: TDXClientThread; Priority,Facility,LogString:string) of object;
  SysLogTDetailEvent = procedure(AppAndPriorityByte:Byte;Priority,Facility,RFCHeader,LogString,SenderIP:string;SenderPort:Integer) of object;

  TDXSysLogServerCore = class(TDXServerCore)
  private
    fOnCommandOther:SysLogTOtherEvent;
    fOnCommandQuick:SysLogTDetailEvent;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
    procedure SpecialHandler(ClientThread:TDXClientThread;PeerIP:String;PeerPort:Integer;Data:Pointer;DataLen:Integer);
  published
    property OnCommandSyslog: SysLogTOtherEvent read fOnCommandOther
                                                write fOnCommandOther;
    property OnCommandDetailSyslog: SysLogTDetailEvent read fOnCommandQuick
                                                write fOnCommandQuick;
  end;

implementation

uses
   DXSock,
   DXString;

constructor TDXSysLogServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=514;
   ProtocolToBind:=wpUDPOnly;
end;

destructor TDXSysLogServerCore.Destroy;
begin
   inherited Destroy;
end;

procedure TDXSysLogServerCore.SpecialHandler(ClientThread:TDXClientThread;
   PeerIP:String;PeerPort:Integer;Data:Pointer;DataLen:Integer);
Const
   cPriority:Array[0..7] of String=('Emergency',
                                    'Alert',
                                    'Critical',
                                    'Error',
                                    'Warning',
                                    'Notice',
                                    'Info',
                                    'Debug');
   cFacility:Array[0..23] of String=('Kernel',
                                     'User',
                                     'Mail',
                                     'Daemon',
                                     'Auth',
                                     'Syslog',
                                     'LPR',
                                     'News',
                                     'UUCP',
                                     'Cron',
                                     'Security',
                                     'FTP',
                                     'NTP',
                                     'Log Audit',
                                     'Log Alert',
                                     'Clock Daemon',
                                     'Local0',
                                     'Local1',
                                     'Local2',
                                     'Local3',
                                     'Local4',
                                     'Local5',
                                     'Local6',
                                     'Local7');

Var
   iFacility:Integer;
   iPriority:Integer;
   Code:Integer;
   Ws,Ts:String;
   RFCHeader:String;

Begin
   If (DataLen=-1) or (Data=Nil) then Exit;
   iFacility:=0;
   iPriority:=0;
   Setlength(Ws,DataLen);
   Move(Data^,Ws[1],DataLen);
   If Ws[1]='<' then Begin
      Ts:=Copy(Ws,2,Charpos('>',Ws)-2);
      Delete(Ws,1,Length(Ts)+2);
      Code:=StringToInteger(TS);
      iPriority:=(Code and $0007);
      iFacility:=(Code and $03f8) shr 3;
   End
   Else Code:=0;
   Ws:=Trim(Ws);
   if assigned(fOnCommandQuick) then Begin
      If (Copy(Ws,4,1)=#32) and (Copy(Ws,7,1)=#32) and (Copy(Ws,16,1)=#32) then Begin
         RFCHeader:=Copy(Ws,1,16);
         Delete(Ws,1,16);
         RFCHeader:=RFCHeader+FetchByChar(Ws,#32,False);
      End
      Else
         RFCHeader:='';
      fOnCommandQuick(Code,cPriority[iPriority],cFacility[iFacility],RFCHeader,WS,PeerIP,PeerPort);
   End
   Else 
   if assigned(fOnCommandOther) then
      fOnCommandOther(ClientThread,cPriority[iPriority],cFacility[iFacility],WS);
End;

procedure TDXSysLogServerCore.ProcessSession(ClientThread:TDXClientThread);
Var
   S:String;

begin
   S:=ClientThread.Socket.ReadStr(-1);
   If S<>'' then SpecialHandler(ClientThread,ClientThread.Socket.PeerIPAddress,
      ClientThread.Socket.PeerPort,@S[1],Length(S));
end;

end.

