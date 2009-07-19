unit DXSessionTracker;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSessionTracker
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
//  Description:
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  DXString,
  Classes;

{$I DXSock.def}

{$DEFINE BYPASS}

type
  TDXSessionTracker = class(TDXComponent)
  private
    fSessionArray:TList;
  protected
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    Procedure RegisterSession(ClientThread:TThread);
    Procedure UnregisterSession(ClientThread:TThread);
    Procedure CloseAllSessions;
    Procedure ClearAllSessions;
  published
  end;

implementation

Uses
   DXServerCore;

constructor TDXSessionTracker.Create(AOwner:TComponent);
begin
   inherited;
   fSessionArray:=TList.Create;
end;

destructor TDXSessionTracker.Destroy;
begin
   CloseAllSessions;
   ClearAllSessions;
   fSessionArray.Free;
   inherited Destroy;
end;

Procedure TDXSessionTracker.RegisterSession(ClientThread:TThread);
Begin
{$IFNDEF BYPASS}
   MyCriticalSection.StartingWrite;
   fSessionArray.Add(TDXClientThread(ClientThread));
   MyCriticalSection.FinishedWrite;
{$ENDIF}
End;

Procedure TDXSessionTracker.UnregisterSession(ClientThread:TThread);
{$IFNDEF BYPASS}
Var
   Loop:Integer;
   LookingFor:Cardinal;
{$ENDIF}

Begin
{$IFNDEF BYPASS}
   LookingFor:=TDXClientThread(ClientThread).ThreadID;
   MyCriticalSection.StartingRead;
   Loop:=0;
   While Loop<fSessionArray.Count do Begin
      If TDXClientThread(fSessionArray[Loop]).ThreadID=LookingFor then Begin
         MyCriticalSection.FinishedRead;
         MyCriticalSection.StartingWrite;
         fSessionArray.Delete(Loop);
         MyCriticalSection.FinishedWrite;
         Exit;
      End
      Else Inc(Loop);
   End;
   MyCriticalSection.FinishedRead;
{$ENDIF}
End;

Procedure TDXSessionTracker.CloseAllSessions;
{$IFNDEF BYPASS}
Var
   Loop:Integer;
{$ENDIF}

Begin
{$IFNDEF BYPASS}
   MyCriticalSection.StartingWrite;
   Loop:=0;
   While Loop<fSessionArray.Count do Begin
      If Assigned(TDXClientThread(fSessionArray[Loop]).Socket) then
         TDXClientThread(fSessionArray[Loop]).Socket.CloseGracefully;
      Inc(Loop);
   End;
   MyCriticalSection.FinishedWrite;
{$ENDIF}
End;

Procedure TDXSessionTracker.ClearAllSessions;
Begin
{$IFNDEF BYPASS}
   MyCriticalSection.StartingWrite;
   If Assigned(fSessionArray) then Begin
      While fSessionArray.Count>0 do Begin
         fSessionArray.Delete(0);
      End;
   End;
   MyCriticalSection.FinishedWrite;
{$ENDIF}   
End;

end.

