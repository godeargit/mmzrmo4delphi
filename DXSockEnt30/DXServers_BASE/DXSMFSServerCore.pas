unit DXSMFSServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSMFSServerCore
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
//  Description: *NOT FINISHED IN V3.0 YET*
// ========================================================================
// RFC122,399 - Simple Minded File Protocol
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXSMFSServerCore = class(TDXServerCore)
  private
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
  end;

implementation

Uses
   SysUtils,
   DXString;

constructor TDXSMFSServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=123;
   ProtocolToBind:=wpTCPOnly;
end;

destructor TDXSMFSServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXSMFSServerCore.ProcessSession(ClientThread:TDXClientThread);
begin
   with ClientThread.Socket do
      If ValidSocket then
         Writeln('');
end;

end.

