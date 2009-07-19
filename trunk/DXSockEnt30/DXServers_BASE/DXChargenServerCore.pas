unit DXChargenServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCharGenServerCore
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
//  Description: Implementation of Character Generation Debug Protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  TDXChargenServerCore = class(TDXServerCore)
  private
    fiDelay:Integer;
    fiDelay2:Integer;
    fLineCtr:Integer;
    fCharCtr:Integer;
  protected
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
  published
    property DelayBetweenChars:integer read fiDelay
                                       write fiDelay;
    property DelayBetweenLines:integer read fiDelay2
                                       write fiDelay2;
    property LinesSent:integer read fLineCtr;
    property CharCurLine:integer read fCharCtr;
  end;

implementation

Uses
   DXString;

Const
   szBuf:Array[1..94] of Char =
   (#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,
    #47,#48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,
    #61,#62,#63,#64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,
    #75,#76,#77,#78,#79,#80,#81,#82,#83,#84,#85,#86,#87,#88,
    #89,#90,#91,#92,#93,#94,#95,#96,#97,#98,#99,#100,#101,
    #102,#103,#104,#105,#106,#107,#108,#109,#110,#111,#112,
    #113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,
    #124,#125,#126);

constructor TDXChargenServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=19;
   fiDelay:=0;
   fiDelay2:=0;
   fLineCtr:=0;
   fCharCtr:=0;
end;

destructor TDXChargenServerCore.Destroy;
begin
  inherited Destroy;
end;

procedure TDXChargenServerCore.ProcessSession(ClientThread: TDXClientThread);
var
   Counter:Integer;
   Base:Integer;
   fiLineCtr:Integer;
   fiCharCtr:Integer;

begin
   fbForceAbort:=False;
   fiLineCtr:=0;
   Base:=1;
   Counter:=1;
   fiCharCtr:=1;
   with ClientThread.Socket do begin
      while Connected do begin
         if fbForceAbort then Exit;
         fLineCtr:=fiLineCtr;
         fCharCtr:=fiCharCtr;
         Write(szBuf[Counter]);
         If LastCommandStatus<>0 then Exit;
         Inc(Counter);
         Inc(fiCharCtr);
         If fiCharCtr=72 then Begin
            If ValidSocket then Begin
               Writeln('');
               If LastCommandStatus<>0 then Exit;
               If fiDelay2>0 then DoSleepEX(fiDelay2);
               Inc(fiLineCtr);
            End;
            fiCharCtr:=1;
            Inc(Base);
            If Base=95 then Base:=1;
            Counter:=Base;
         End
         Else If fiDelay>0 then DoSleepEX(fiDelay);
         If Counter=95 then Counter:=1;
      end;
   end;
end;

end.

