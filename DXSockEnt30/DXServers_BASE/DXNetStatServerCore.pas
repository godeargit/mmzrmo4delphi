unit DXNetStatServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXNetStatServerCore
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
//  Description: To be redesigned after 3.0
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXNetStatServerCore = class(TDXServerCore)
  private
//    fsDateFormat:String;
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
   DXCGI,
   DXString;

constructor TDXNetStatServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=15;
end;

destructor TDXNetStatServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXNetStatServerCore.ProcessSession(ClientThread:TDXClientThread);
Var
   Shell:TDXCGI;
   Ws:String;
   Err:String;

begin
   with ClientThread.Socket do
      If ValidSocket then Begin
{$IFDEF OBJECTS_ONLY}
         Shell:=TDXCGI.Create;
{$ELSE}
         Shell:=TDXCGI.Create(Nil);
{$ENDIF}
         Shell.ExecuteScript('netstat.exe','C:\Winnt\System32\',Nil,Ws,Err);
         Shell.Free;
         If Ws<>'' then ClientThread.Socket.Writeln(Ws);
         If Err<>'' then ClientThread.Socket.Writeln(Err);
      End;
end;

end.

