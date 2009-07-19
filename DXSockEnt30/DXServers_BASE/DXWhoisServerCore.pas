unit DXWhoisServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXWhoisServerCore
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
//  Description: supports Whois and Whois++ protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  WHOISTBasicEvent = procedure(ClientThread: TDXClientThread; Lookup: string) of object;

  TDXWhoisServerCore = class(TDXServerCore)
  private
    fOnCommandGET: WHOISTBasicEvent;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
  published
    property OnCommandGET: WHOISTBasicEvent read fOnCommandGET
                                       write fOnCommandGET;
  end;

implementation

Uses
   DXSock,
   DXString;


constructor TDXWhoisServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=43;
end;

destructor TDXWhoisServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXWhoisServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s: string;
  OutData:Pointer;
  Loop:Integer;
  WasHandled:Boolean;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while Connected do begin
         if fbForceAbort then Exit;
         s:=ReadLn(Timeout);
         If LastReadTimeout or Not ValidSocket then Exit;
         If S='' then continue;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         if assigned(OnCommandGET) then OnCommandGET(ClientThread,S);
      end; {while}
   end; {with}
end; {doExecute}

end.

