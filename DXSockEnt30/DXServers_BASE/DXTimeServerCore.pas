unit DXTimeServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXTIMEServerCore
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
//  Description: Implements Time protocol
// ========================================================================
// RFC868 - Time Protocol
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXTimeServerCore = class(TDXServerCore)
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

constructor TDXTimeServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=37;
   ProtocolToBind:=wpUDPOnly;
end;

destructor TDXTimeServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXTimeServerCore.ProcessSession(ClientThread:TDXClientThread);
Var
   WI:Integer;
   Ws:String[4];

begin
   with ClientThread.Socket do
      If Connected then Begin
         WI:=((Trunc(Date)+2)*(24*60*60))+DateTimeToTimeStamp(Now).Time;
         SetLength(Ws,4);
         Move(WI,WS[1],4);
         Writeln(Ws);
      End;
End;

end.

