unit DXEMSIObject;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXEMSIObject
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
//  Description: Component Version implementing just the basics of EMSI
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXSock,
   Classes;

// EMSI
// ====
// Client Connects
//  Client Sends EMSI_INQ
//   Server Replies EMSI_REQ
//  Client Sends EMSI_DAT
//   Server Replies EMSI_ACK
//  Server Sends EMSI_DAT
//   Client Replies EMSI_ACK
// -- session is establish
//
// EMSI_XFER
// =========
// (A) Side Sends EMSI_SSN
//  (B) Side Replies EMSI_ACK
//  (A) Sise Send EMSI_BLK
//   (B) Side Replies EMSI_ACK
//    -- loop until done sending
//  (A) Side Sends EMSI_ESN
//   (B) Side Sends EMSI_ACK
// -- XFER is complete

type
   TDXEMSI_XFER_Signature=(isEMSI_SSN,
      isEMSI_ESN,
      isEMSI_BLK,
      isEMSI_ACK,
      isEMSI_NAK,
      isEMSI_INQ,
      isEMSI_Invalid);

   TDXEMSIObject=class(TComponent)
   private
      { Private declarations }
   protected
      { Protected declarations }
   public
      { Public declarations }
  // Standard EMSI:
      function Establish_EMSI(Socket:TDXSock):Boolean;
      function Create_EMSI_DAT(SystemAddress,
         Password,
         ConnectionCodes,
         SupportedProtocols,
         ProductCode,
         ProductName,
         ProductVersion,
         SerialNumber:string):string;
      function Parse_EMSI_DAT(SourceString:string;
         var SystemAddress,
         Password,
         ConnectionCodes,
         SupportedProtocols,
         ProductCode,
         ProductName,
         ProductVersion,
         SerialNumber:string):Boolean;
      function Send_EMSI_DAT(Socket:TDXSock;
         EMSI_DAT_LENGTH,
         EMSI_DAT_DATA,
         EMSI_DAT_CRC:string):Boolean;
      function Recv_EMSI_DAT(Socket:TDXSock;
         var EMSI_DAT_LENGTH,
         EMSI_DAT_DATA,
         EMSI_DAT_CRC:string):Boolean;
      function Send_EMSI_ACK(Socket:TDXSock):Boolean;
      function Recv_EMSI_ACK(Socket:TDXSock):Boolean;
      function Send_EMSI_NAK(Socket:TDXSock):Boolean;
      function Send_EMSI_REQ(Socket:TDXSock):Boolean;
      // Not Standard EMSI:
      function Send_XFER_Introduction(Socket:TDXSock):Boolean;
      function Send_XFER_Block(Socket:TDXSock; Block:string):Boolean;
      function Recv_XFER_Block(Socket:TDXSock; var Block:string):Boolean;
      function Send_XFER_Complete(Socket:TDXSock):Boolean;
      function XFER_Signature(Str:string):TDXEMSI_XFER_Signature;
   published
      { Published declarations }
   end;

procedure Register;

implementation

uses
   DXString,
   SysUtils;

const
   EMSINegotiationTimeout=10000;
   EMSI_INQ='**EMSI_INQC816';
   EMSI_REQ='**EMSI_REQA77E';
   EMSI_ACK='**EMSI_ACKA490';
   EMSI_NAK='**EMSI_NAKEEC3';
   EMSI_CLI='**EMSI_CLIFA8C';
   EMSI_ICI='**EMSI_ICI2D73';
   EMSI_HBT='**EMSI_HBTEAEE';
   EMSI_IRQ='**EMSI_IRQ8E08';
   EMSI_DAT='**EMSI_DAT';
   // CUSTOM:
   EMSI_BLK='**EMSI_BLK';
   EMSI_SSN='**EMSI_SSN0000';
   EMSI_ESN='**EMSI_ESNFFFF';

const
   FingerPrint='{EMSI}';

procedure Register;
begin
   RegisterComponents('BPDX Addons', [TDXEMSIObject]);
end;

function TDXEMSIObject.Create_EMSI_DAT(SystemAddress,
   Password,
   ConnectionCodes,
   SupportedProtocols,
   ProductCode,
   ProductName,
   ProductVersion,
   SerialNumber:string):string;
begin
   Result:=FingerPrint+
      '{'+SystemAddress+'}'+
      '{'+PassWord+'}'+
      '{'+ConnectionCodes+'}'+
      '{'+SupportedProtocols+'}'+
      '{'+ProductCode+'}'+
      '{'+ProductName+'}'+
      '{'+ProductVersion+'}'+
      '{'+SerialNumber+'}';
end;

function TDXEMSIObject.Parse_EMSI_DAT(SourceString:string;
   var SystemAddress,
   Password,
   ConnectionCodes,
   SupportedProtocols,
   ProductCode,
   ProductName,
   ProductVersion,
   SerialNumber:string):Boolean;
var
   StartPos:Integer;

begin
   StartPos:=QuickPOS(FingerPrint, SourceString);
   if StartPos>0 then begin
      Delete(SourceString, 1, (StartPos-1)+Length(FingerPrint));
      Delete(SourceString, 1, 1);
      Password:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      ConnectionCodes:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      SupportedProtocols:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      ProductCode:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      ProductName:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      ProductVersion:=FetchByChar(SourceString, '}', False);
      Delete(SourceString, 1, 1);
      SerialNumber:=FetchByChar(SourceString, '}', False);
      Result:=True;
   end
   else
      Result:=False;
end;

function TDXEMSIObject.Send_EMSI_DAT(Socket:TDXSock;
   EMSI_DAT_LENGTH,
   EMSI_DAT_DATA,
   EMSI_DAT_CRC:string):Boolean;
begin
   Socket.Write(EMSI_DAT+
      EMSI_DAT_LENGTH+
      EMSI_DAT_DATA+
      EMSI_DAT_CRC);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Recv_EMSI_DAT(Socket:TDXSock;
   var EMSI_DAT_LENGTH,
   EMSI_DAT_DATA,
   EMSI_DAT_CRC:string):Boolean;
var
   StartTime:Cardinal;
   Ws:string;
   Len:Integer;

begin
   Result:=False;
   StartTime:=TimeCounter+EMSINegotiationTimeout;
   while (not Result)and(StartTime>TimeCounter) do begin
      if Socket.Readable then begin
         if Socket.CharactersToRead=0 then Exit;
         Ws:='';
         while Ws<>EMSI_DAT do begin
            Ws:=Ws+Socket.Read;
            if Socket.LastReadTimeout then Exit;
            if Length(Ws)>Length(EMSI_DAT) then Delete(Ws, 1, 1);
         end;
         EMSI_DAT_LENGTH:=Socket.ReadStr(4);
         try
            Len:=StrToInt('x'+EMSI_DAT_LENGTH);
         except
            Exit;
         end;
         EMSI_DAT_DATA:=Socket.ReadStr(Len);
         EMSI_DAT_CRC:=Socket.ReadStr(4);
         Result:=Socket.LastCommandStatus=0;
      end
      else begin
         ProcessWindowsMessageQueue;
         DoSleepEx(0);
      end;
   end;
end;

function TDXEMSIObject.Send_EMSI_ACK(Socket:TDXSock):Boolean;
begin
   Socket.Write(EMSI_ACK);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Recv_EMSI_ACK(Socket:TDXSock):Boolean;
var
   StartTime:Cardinal;
   Ws:string;

begin
   Result:=False;
   StartTime:=TimeCounter+EMSINegotiationTimeout;
   while (not Result)and(StartTime>TimeCounter) do begin
      if Socket.Readable then begin
         if Socket.CharactersToRead=0 then Exit;
         Ws:='';
         while Ws<>EMSI_ACK do begin
            Ws:=Ws+Socket.Read;
            if Socket.LastReadTimeout then Exit;
            if Length(Ws)>Length(EMSI_ACK) then Delete(Ws, 1, 1);
         end;
         Result:=Socket.LastCommandStatus=0;
      end
      else begin
         ProcessWindowsMessageQueue;
         DoSleepEx(0);
      end;
   end;
end;

function TDXEMSIObject.Establish_EMSI(Socket:TDXSock):Boolean;
var
   StartTime:Cardinal;
   LastSent:Cardinal;
   Ws:string;

begin
   Result:=False;
   LastSent:=0;
   Ws:='';
   StartTime:=TimeCounter+EMSINegotiationTimeout;
   while (not Result)and(StartTime>TimeCounter) do begin
      if LastSent+EMSINegotiationTimeout<TimeCounter then begin
         Socket.Write(EMSI_INQ);
         LastSent:=TimeCounter;
      end;
      if Socket.Readable then begin
         if Socket.CharactersToRead=0 then Exit;
         WS:=Socket.ReadStr(-1);
         Result:=QuickPos(EMSI_REQ, Ws)>0;
      end
      else begin
         ProcessWindowsMessageQueue;
         DoSleepEx(0);
      end;
   end;
end;

function TDXEMSIObject.Send_EMSI_NAK(Socket:TDXSock):Boolean;
begin
   Socket.Write(EMSI_NAK);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Send_EMSI_REQ(Socket:TDXSock):Boolean;
begin
   Socket.Write(EMSI_REQ);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Send_XFER_Introduction(Socket:TDXSock):Boolean;
begin
   Socket.Write(EMSI_SSN);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Send_XFER_Block(Socket:TDXSock; Block:string):Boolean;
var
   BlockLength:string;
   BlockCRC:string;

begin
   BlockLength:=IntToHex(Length(Block), 4);
   BlockCRC:=IntToHex(CRC16ByString('EMSI_BLK'+BlockLength+Block, $FFFF), 4);
   Socket.Write(EMSI_BLK+
      BlockLength+
      Block+
      BlockCRC);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.Recv_XFER_Block(Socket:TDXSock; var
   Block:string):Boolean;
var
   StartTime:Cardinal;
   Ws:string;
   Len:Integer;
   BlockCRC:string;

begin
   Result:=False;
   StartTime:=TimeCounter+EMSINegotiationTimeout;
   while (not Result)and(StartTime>TimeCounter) do begin
      if Socket.Readable then begin
         if Socket.CharactersToRead=0 then Exit;
         if Copy(Block, 1, 10)<>EMSI_BLK then begin
            Ws:='';
            while Ws<>EMSI_BLK do begin
               Ws:=Ws+Socket.Read;
               if Socket.LastReadTimeout then Exit;
               if Length(Ws)>Length(EMSI_BLK) then Delete(Ws, 1, 1);
            end;
            Ws:=Socket.ReadStr(4);
         end
         else
            Ws:=Copy(Block, 11, 4);
         try
            Len:=StrToInt('x'+Ws);
         except
            Exit;
         end;
         Block:=Socket.ReadStr(Len);
         BlockCRC:=Socket.ReadStr(4);
         if BlockCRC<>IntToHex(CRC16ByString('EMSI_BLK'+Ws+Block, $FFFF), 4) then
            Exit;
         Result:=Socket.LastCommandStatus=0;
      end
      else begin
         ProcessWindowsMessageQueue;
         DoSleepEx(0);
      end;
   end;
end;

function TDXEMSIObject.Send_XFER_Complete(Socket:TDXSock):Boolean;
begin
   Socket.Write(EMSI_ESN);
   Result:=Socket.LastCommandStatus=0;
end;

function TDXEMSIObject.XFER_Signature(Str:string):TDXEMSI_XFER_Signature;
begin
   if QuickPos(EMSI_BLK, Str)=1 then
      Result:=isEMSI_BLK
   else if Str=EMSI_ACK then
      Result:=isEMSI_ACK
   else if Str=EMSI_NAK then
      Result:=isEMSI_NAK
   else if Str=EMSI_SSN then
      Result:=isEMSI_SSN
   else if Str=EMSI_ESN then
      Result:=isEMSI_ESN
   else if QuickPos(EMSI_INQ, Str)>0 then
      Result:=isEMSI_INQ
   else
      Result:=isEMSI_Invalid;
end;

end.

