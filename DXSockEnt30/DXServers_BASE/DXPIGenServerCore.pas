unit DXPIGenServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXPIGenServerCore
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
//  Description: implements PIGEN protocl
// ========================================================================
// RFC 3091 - also requires file found in PIGEN Demo!
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXPIGenServerCore = class(TDXServerCore)
  private
    fiDelay:Integer;
    fCharCtr:Integer;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
    property DelayBetweenChars:integer read fiDelay
                                       write fiDelay;
  end;

implementation

Uses
   SysUtils,
   DXString;

constructor TDXPIGenServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=314159;
   fiDelay:=0;
   fCharCtr:=0;
end;

destructor TDXPIGenServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXPIGenServerCore.ProcessSession(ClientThread: TDXClientThread);
var
   FH:Integer;
   Wi:Integer;
   Ws:String;

begin
   FH:=FileOpen('dx_pi_data.txt',fmShareDenyWrite or fmOpenRead);
   Setlength(Ws,50);
   If FH>0 then Begin
      FileSeek(FH,50,0);
      fbForceAbort:=False;
      with ClientThread.Socket do begin
         while ValidSocket do begin
            if fbForceAbort then exit;
            If FileSeek(FH,2,1)>=0 then Begin
               Wi:=FileRead(FH,Ws[1],50);
               If Wi=50 then Begin
                  Write(Ws);
               End
               Else Begin
                  DoSleepEx(2000);
                  CloseGracefully;
               End;
            End
            Else Begin
               DoSleepEx(1000);
               CloseGracefully;
            End;
         end;
      end;
      FileClose(FH);
   End;
end;

end.

