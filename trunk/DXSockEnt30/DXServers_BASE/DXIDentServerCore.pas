unit DXIDentServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXIDentServerCore
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
//  Description: implement IDENT/AUTH protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

Type
  IDentTBasicEvent = procedure(ClientThread:TDXClientThread;Query:string) of object;

  TDXIDentServerCore = class(TDXServerCore)
  private
    fOnCommandQuery:IDentTBasicEvent;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
    property OnCommandQuery: IDentTBasicEvent read fOnCommandQuery
                                         write fOnCommandQuery;
  end;

implementation

Uses
   DXSock,
   DXString;

///////////////////////////////////////////////////////////////////////////////
// RFC 1413...
//   The query format to valid the connection:
//   <port on server>, <port on client>
//
//   If a client on host A wants to ask a server on host B about a
//   connection specified locally (on the client's machine) as 23, 6191
//   (an inbound TELNET connection), the client must actually ask about
//   6191, 23 - which is how the connection would be specified on host B.
//
//   The responce to the query is:
//   <port on server>, <port on client> : <userid> : OS : NAME
//   or
//   <port on server>, <port on client> : ERROR : NO-USER
///////////////////////////////////////////////////////////////////////////////

constructor TDXIDentServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=113;
end;

destructor TDXIDentServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXIDentServerCore.ProcessSession(ClientThread:TDXClientThread);
var
  s,sCmd:string;
  OutData:Pointer;
  Loop:Integer;
  WasHandled:Boolean;

begin
   with ClientThread.Socket do begin
      if ValidSocket then begin
         s:=Readln(Timeout);
         If LastReadTimeout or Not ValidSocket then Exit;
         if S='' then exit;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(Outdata) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         If Assigned(fOnCommandQuery) then fOnCommandQuery(ClientThread,S);
      end;
   end;
end;

end.

