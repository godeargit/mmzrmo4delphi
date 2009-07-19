unit DXServersRegister;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXServersRegister
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
//  Description: CENTRAL REGISTRATION OF COMPONENTS TO COMPILER
// ========================================================================
// NOTE TO CUSTOMERS WHO ACTUALLY LOOK AT THIS FILE!
// Code commented is in BETA, it may be included in the release, but not
// registered as we do not feel it is 100% mature!
///////////////////////////////////////////////////////////////////////////////

{$DEFINE WINDOWS_NT_SUPPORT}
{$WARNINGS OFF}

uses
{Popular Servers}
  DXHTTPServerCore,           // oblivion (WWS)
  DXNNTPServerCore,           // oblivion (WNGS)
  DXSMTPServerCore,           // oblivion
  DXFTPServerCore,            // oblivion (WFTPS)
  DXPOP3ServerCore,           // oblivion
  DXIMAP4ServerCore,          // oblivion
  DXFingerServerCore,         // oblivion (WFINGERS)
  DXIDentServerCore,          // oblivion
  DXIRCServerCore,            // oblivion (WIRCS)
  DXChargenServerCore,        // oblivion (WCGS)
  DXDayTimeServerCore,        // oblivion (WDTS)
  DXDiscardServerCore,        // oblivion (WDISGARDS)
  DXEchoServerCore,           // oblivion (WECHOS)
  DXQOTDServerCore,           // oblivion (WQOTDS)
  DXWhoisServerCore,          // oblivion (WWHOISS)
  DXTelnetServerCore,         // oblivion
  DXGopherServerCore,         // oblivion (WGOPHERS)
  DXDictionaryServerCore,     // oblivion
{$ifndef bcb}
  DXLPDServerCore,            // oblivion
{$endif}
  DXPOP2ServerCore,           // oblivion
  DXSOCKSV4ServerCore,        // oblivion (WSOCKSV4S)
  DXDNSServerCore,            // oblivion
  DXSFTPServerCore,           // oblivion (WSFTPS)
  DXTFTPServerCore,           // oblivion (WTFTPS)
  DXNTPServerCore,            // NOT FINISHED YET //
  DXTimeServerCore,           // oblivion (WTIMES)
  DXSMFSServerCore,           // NOT FINISHED YET //

{custom}
  DXMessageBaseServerCore,    // oblivion (WMBASES)
  DXDatabaseServerCore,       // oblivion (WDBASES)
  DXMUDServerCore,            // oblivion
  DXRequestReplyServerCore,   // oblivion
  DXPIGenServerCore,          // oblivion (WPIS)
  DXDataSetServerCore,        // NOT FINISHED YET //
//  DXDispatchDatasetServerCore, // new product line, called DXMultiTier
  DXPOPPassDServerCore,       // oblivion (WPOPPASSDS)
  DXBBSServerCore,            // oblivion
  DXSysLogServerCore,         // oblivion (WSYSLOGS)
  DXNetLinkServerCore, // AT&T! // oblivion
{ISAPI 5.1 PIECES}
  DXISAPI,
  DXISAPIFilter,
  DXSecurity,
{HTTP PIECES}
  DXHTTPHeaderTools
  ;

{$I DXSock.def}

{$IFNDEF OBJECTS_ONLY}
procedure Register;
{$ENDIF}

Implementation

Uses
   Classes;

{$IFNDEF OBJECTS_ONLY}
procedure Register;
Begin
{Servers}
  RegisterComponents('BPDX Popular Servers', [
     TDXHTTPServerCore,
     TDXFTPServerCore,
     TDXSMTPServerCore,
     TDXPOP3ServerCore,
     TDXIMAP4ServerCore,
     TDXNNTPServerCore,
     TDXFingerServerCore,
     TDXGopherServerCore,
     TDXDNSServerCore,
     TDXSOCKSV4ServerCore,
     TDXIDentServerCore,
     TDXIRCServerCore]);

  RegisterComponents('BPDX Misc. Servers', [
     TDXChargenServerCore,
     TDXDayTimeServerCore,
     TDXNTPServerCore,
     TDXTimeServerCore,
     TDXDiscardServerCore,
     TDXEchoServerCore,
     TDXQOTDServerCore,
     TDXWhoisServerCore,
     TDXTelnetServerCore,
     TDXDictionaryServerCore,
     TDXPOP2ServerCore,
     TDXSMFSServerCore,
     TDXTFTPServerCore,
     TDXSFTPServerCore]);

{Custom}
  RegisterComponents('BPDX Custom Servers', [
        TDXMUDServerCore,
        TDXMessageBaseServerCore,
        TDXDatabaseServerCore,
        TDXRequestReplyServerCore,
        TDXPIGenServerCore,
{$IFNDEF LINUX}
{$IFDEF DXADDONS}
        TDXNetStatServerCore,
        TDXStatServerCore,
{$ENDIF}
{$ENDIF}
        TDXPOPPassDServerCore,
        TDXBBSServerCore,
        TDXSysLogServerCore,
{$ifndef bcb}
        TDXLPDServerCore,
{$endif}
        TDXNetLinkServerCore,
        TDXDataSetServerCore
//        TDXDispatchDatasetServerCore
        ]);

{Addons}
  RegisterComponents('BPDX Addons', [
        TDXISAPI,
        TDXHTTPHeaderTools
        ]);

{Wizard}
// Not ready for public EYES!
End;
{$ENDIF}

end.
