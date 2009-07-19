unit DXFingerServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFingerServerCore
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
//  Description: implements FINGER protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  FINGERTBasicEvent = procedure(ClientThread: TDXClientThread; Lookup: string) of object;

  TDXFingerServerCore = class(TDXServerCore)
  private
    fOnCommandGET: FINGERTBasicEvent;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
    property OnCommandGET: FINGERTBasicEvent read fOnCommandGET
                                             write fOnCommandGET;
  end;

implementation

Uses
   DXSock,
   DXString;

constructor TDXFingerServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=79;
end;

destructor TDXFingerServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXFingerServerCore.ProcessSession(ClientThread: TDXClientThread);
var
   Ws:string;
   OutData:Pointer;
   Loop:Integer;
   WasHandled:Boolean;

begin
   with ClientThread.Socket do begin
      if Connected then begin
         Ws:=ReadLn(Timeout);
         If LastReadTimeout or Not Connected then Exit
         Else If Ws<>'' then Begin
            If Assigned(OnFilter) then Begin
                Loop:=FilterRead(@WS[1],OutData,Length(WS),ClientThread);
                SetLength(WS,Loop);
                If Assigned(Outdata) then Begin
                   Move(TDXBSArray(OutData^),WS[1],Loop);
                   OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
                End;
            End;
            If Assigned(OnCommandGET) then OnCommandGET(ClientThread,Ws);
         End;
      end;
   end;
end;

end.

