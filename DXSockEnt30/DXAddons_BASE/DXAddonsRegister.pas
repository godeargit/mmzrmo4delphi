unit dxaddonsregister;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXAddonsRegister
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
// Code Version: (4th Generation Code)
// ========================================================================
//  Description:
// ========================================================================
// NOTE TO CUSTOMERS WHO ACTUALLY LOOK AT THIS FILE!
// Code commented is in BETA, it may be included in the release, but not
// registered as we do not feel it is 100% mature!
///////////////////////////////////////////////////////////////////////////////

{$DEFINE WINDOWS_NT_SUPPORT}
{$WARNINGS OFF}

uses
   {Servers}
{$IFNDEF LINUX}
   DXNetStatServerCore,        // oblivion
   DXStatServerCore,           // oblivion
{$ENDIF}

   {Addons}
   DXFileAllocationTable,
   DXWebFAT,
   DXHTMLCompressor,
   DXFileBuffer,
   DXDLLManager,
   DXFile,
   DXSSI,
   DXSSIPlus,
   DXMXResolver,
   DXDNSQuery,
   DXReverseDNSTable,
   DXBasicTimer,
   DXURL,
   DXSMTPSender,
   DXSMTPRelay,
   DXRDCompression,
   DXCGI,

   {Queues}
   DXDataQueue,
   DXUDPUnicastDataQueue,
   DXUnicastDataQueue,
   DXFileDataQueue,

   {Messages}
   DXCoders,
   DXMIMEDecode,
   DXMIMEEncode,
   DXUUDecode,
   DXUUEncode,
   DXGeneralMsgObject,
   DXFullMsgObject,
   DXRFC822MsgObject,
   DXCompressedMsgObject,

   {Logging}
   DXCachedLogging,
   DXCachedFlexibleLogging,
   DXCachedCERNLogging,
   DXCachedDebugLogging,
   DXCachedNCSALogging,
   DXCachedW3SVCv30Logging,
   DXCachedWEBSITELogging,
   DXCachedApacheLogging,
   DXCachedEMWACLogging,
   DXCachedNetscapeLogging,
   DXCachedPurveyorLogging,
   DXCachedSpryLogging,
   DXCachedIMSLogging,
   DXCachedSocketErrorLogging,
   DXCachedProxyErrorLogging,
   DXCachedBROWSERLogging,
   DXCachedREFERRERLogging,
   DXCachedRealAudioLogging,
   DXCachedSYSLOGLogging,
   DXCachedBBSLogging,
   DXPrinterLogging
   ;

{$I DXAddons.def}

{$IFNDEF OBJECTS_ONLY}
procedure Register;
{$ENDIF}

implementation

uses
   Classes;

{$IFNDEF OBJECTS_ONLY}

procedure Register;
begin
   {Servers}
   {Addons}
   RegisterComponents('BPDX Addons', [
      TDXDLLManager,
         TDXFileAllocationTable,
         TDXWebFAT,
         TDXHTMLCompressor,
         TDXSSI,
         TDXSSIPlus,
         TDXCGI,
         TDXURL,
         TDXFile,
         TDXFileBuffer,
         TDXDNSQuery,
         TDXMXResolver,
         TDXReverseDNSTable,
         TDXSMTPSender,
         TDXSMTPRelay,
         TDXBasicTimer,
         TDXRDCompression
         ]);

   RegisterComponents('BPDX Queues', [
      TDXDataQueue,
         TDXUnicastDataQueue,
         TDXFileDataQueue,
         TDXUDPUnicastDataQueue
         ]);

   {Messages}
   RegisterComponents('BPDX Messaging', [
      TDXCodeProcessor,
         TDXGeneralMsgObject,
         TDXFullMsgObject,
         TDXRFC822MsgObject,
         TDXCompressedMsgObject]);

   {Logging}
   RegisterComponents('BPDX Logging', [
      TDXCachedLogging,
         TDXCachedFlexibleLogging,
         TDXCachedDebugLogging,
         TDXCachedCERNLogging,
         TDXCachedNCSALogging,
         TDXCachedW3SVCv30Logging,
         TDXCachedWEBSITELogging,
         TDXCachedWEBSITELogging,
         TDXCachedApacheLogging,
         TDXCachedEMWACLogging,
         TDXCachedNetscapeLogging,
         TDXCachedPurveyorLogging,
         TDXCachedSpryLogging,
         TDXCachedIMSLogging,
         TDXCachedREFERRERLogging,
         TDXCachedBROWSERLogging,
         TDXCachedSocketErrorLogging,
         TDXCachedProxyErrorLogging,
         TDXCachedRealAudioLogging,
         TDXCachedSYSLOGLogging,
         TDXCachedBBSLogging,
         TDXPrinterLogging
         ]);
   {Servers}
   RegisterComponents('BPDX Custom Servers', [
         TDXNetStatServerCore,        // oblivion
         TDXStatServerCore
         ]);
end;
{$ENDIF}

end.

