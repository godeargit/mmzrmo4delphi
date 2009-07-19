unit DXGopherServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXGopherServerCore
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
//  Description: implements Gopher Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

Type
  GopherTBasicEvent = procedure(ClientThread:TDXClientThread;Parm1:string) of object;
  GopherTSimpleEvent = procedure(ClientThread:TDXClientThread) of object;

  TDXGopherServerCore = class(TDXServerCore)
  private
    fOnCommandWhatDoYouHave:GopherTSimpleEvent;   // <CR><LF> Received
    fOnCommandRequestedDocument:GopherTBasicEvent;// DocumentName<CR><LF> Received
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
    procedure SAYIHave(ClientThread:TDXClientThread;MenuType:Char;Title,DocumentName,Server:String;Port:Integer);
  published
    property OnWhatDoYouHave: GopherTSimpleEvent read fOnCommandWhatDoYouHave
                                                 write fOnCommandWhatDoYouHave;
    property OnRequestedDocument: GopherTBasicEvent read fOnCommandRequestedDocument
                                                    write fOnCommandRequestedDocument;
  end;

implementation

Uses
   DXSock,
   Sysutils,
   DXString;

///////////////////////////////////////////////////////////////////////////////
// RFC 1436...
//   OnConnect say nothing
//   If Received <CR><LF>
//   then Fire Event OnWhatDoYouHave
//   If Received "text"<cr><lf>
//   then Fire Event OnRequestedDocument
//
//   OnWhatDoYouHave result tstringlist
//   <0..9><title of document><#9><document name><#9><server name><#9><port>
//   0=Document Name is a File
//   1=Document Name is a Directory aka Menu Item
//   2=Document Name requires CSO protocol Phone-Book Server
//   3=Error
//   4=Document Name is a BinHexed MacIntosh File
//   5=Document Name is a DOS Binary File (client reads until socket closes)
//   6=Document Name is a UNIX UUEncoded File
//   7=Document Name is a Index-Search Server
//   8=Document Name is a Text-Based Telnet Session
//   9=Document Name is a Binary File (client reads until socket closes)
//   +=Document Name is a Redundant Server
//   T=Document Name is a Text-Based TN3270 Session
//   g=Document Name is a GIF file
//   I=Document Name is other image file (client decides how to display)
//   s=Document Name is a -law sound data.
//   M=Document Name is a MIME data.
//   h=Document Name is a HTML data.
//   i=Document Name is a In-line text type.
///////////////////////////////////////////////////////////////////////////////

constructor TDXGopherServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=70;
end;

destructor TDXGopherServerCore.Destroy;
begin
   inherited Destroy;
end;

procedure TDXGopherServerCore.SAYIHave(ClientThread:TDXClientThread;MenuType:Char;Title,DocumentName,Server:String;Port:Integer);
Begin
   ClientThread.Socket.WriteLn(MenuType+Title+#9+DocumentName+#9+Server+IntToStr(Port));
End;

procedure TDXGopherServerCore.ProcessSession(ClientThread:TDXClientThread);
var
  s:string;
  OutData:Pointer;
  Loop:Integer;
  WasHandled:Boolean;

begin
   with ClientThread.Socket do begin
      If ValidSocket then begin
         s:=Readln(Timeout);
         If LastReadTimeout or Not ValidSocket then Exit;
         If s='' then Begin
            If Assigned(fOnCommandWhatDoYouHave)
               then fOnCommandWhatDoYouHave(ClientThread);
         End
         Else Begin
            If Assigned(OnFilter) then Begin
               Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
               SetLength(S,Loop);
               If Assigned(Outdata) then Begin
                  Move(TDXBSArray(OutData^),S[1],Loop);
                  OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
               End;
            End;
            If Assigned(fOnCommandRequestedDocument)
               then fOnCommandRequestedDocument(ClientThread,S);
         End;
      end;
   end;
end;

end.

