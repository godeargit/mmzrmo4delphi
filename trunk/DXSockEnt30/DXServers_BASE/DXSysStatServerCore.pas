unit DXSysStatServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSysStatServerCore
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
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I dxsock.def}

type
  TDXSysStatServerCore = class(TDXServerCore)
  private
    fsDateFormat:String;
  protected
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXServerThread);
  published
  end;

implementation

Uses
   SysUtils,
   DXString;

///////////////////////////////////////////////////////////////////////////////
// CREATE:
//        Define the Default Port number to Listen On.
///////////////////////////////////////////////////////////////////////////////
{$IFDEF OBJECTS_ONLY}
constructor TDXSysStatServerCore.Create;
{$ELSE}
constructor TDXSysStatServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=11;
end;

///////////////////////////////////////////////////////////////////////////////
// DESTROY:
//         Destory this object.
///////////////////////////////////////////////////////////////////////////////
destructor TDXSysStatServerCore.Destroy;
begin
  inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////////
// PROCESSSESSION:
//               If you want this CORE to process the parsing, you should call
//               this from your "OnNewConnect" implementation.
///////////////////////////////////////////////////////////////////////////////
procedure TDXSysStatServerCore.ProcessSession(ClientThread:TDXServerThread);
begin
   with ClientThread.Socket do
      If ValidSocket then Begin
         // Show a list of active connections to this server.
      End;
end;

end.

