unit DXDiscardServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDiscardServerCore
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
//  Description: implements the Discard protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXDiscardServerCore = class(TDXServerCore)
  private
    fbCloseOnTimeout:Boolean;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
    property CloseOnTimeout:Boolean read fbCloseOnTimeout
                                    write fbCloseOnTimeout;
  end;

implementation

Uses
   DXString;


constructor TDXDiscardServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=9;
   fbCloseOnTimeout:=True;
end;

destructor TDXDiscardServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXDiscardServerCore.ProcessSession(ClientThread: TDXClientThread);
Var
   startTime:Comp;
   S:String;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         if fbForceAbort then Exit;
         StartTime:=TimeCounter;
         S:=#0;
         While (TimeCounter<startTime+TimeOut) and (S=#0) do Begin
            If (ReadAble) then Begin
               If (CharactersToRead=0) then Exit
               Else Begin
                  S:=ReadStr(-1);
                  If LastReadTimeout then S:=#0;
               End;
            End
            Else DoSleepEx(1);
         End;
         If (TimeCounter>startTime+TimeOut) then
            If fbCloseOnTimeout then Exit;
      end;
   end;
end;

end.

