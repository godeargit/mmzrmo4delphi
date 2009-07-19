unit DXCoders;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCodeProcessor
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
//Original Code: Donated to DX, Inc. for trade of some of our technology.
//               Donation by George Lambert (President) Netmasters, LLC.
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
//  Description: Re-write to optimally Support Encoding/Decoding of
//               MIME/BASE64, UU, Printed Quotable Text
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

type
   //
 //     *   <B>emMime</B> internally enables the MIME/BASE64
 //         encoder/decoder logic.
 //     *   <B>emUU</B> internally enables the UU encoder/decoder
 //         logic.
 //     *   <B>emQuoted</B> internally enabled the "Quoted
 //         Printable Text" encoder/decoder logic.
   TDXCodeProcessorMethods=(emMIME, emUU, emQuoted);

   // TDXCodeProcessor allows you to encode or decode streams of
   // data. It has been designed so it can be incorporated at our
   // socket layer via the OnFilter events, or as part of message
   // attachment system. Our version supports any size of input and
   // \output stream. And is extremely fast via our own 32bit
   // assembly, compared to any other coder on the market.<B>
   //
   //
   //
   // \Note</B> original code was donated from Netmasters, LLC.
   // makers of FastNet. It has since went through some major
   // cleaning and performance tweaks. And has matured into our own
   // design, but we are still thankful for their support!
   //
   //
   //
   // Summary
   // Support Encodeing/Decoding from your application.
   TDXCodeProcessor=class(TDXComponent)
   private
      FOnBeginEncode:TNotifyEvent;
      FOnEndEncode:TNotifyEvent;
      FOnBeginDecode:TNotifyEvent;
      FOnEndDecode:TNotifyEvent;
      FInputStream:TStream;
      FOutputStream:TStream;
   protected
      FMethod:TDXCodeProcessorMethods;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      procedure Encode;
      procedure Decode;
      // InputStream is the source data to processor.<B>
      //
      //
      //
      // \Note</B> you must clean the stream, so it only contains the
      // raw data to be processed. Removing any attachment
      // information, like SIZE, BEGIN, END, etc.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCodeProcessor.OutputStream, OutputStream>, <LINK TDXCodeProcessor.Encode, Encode>,
      // <LINK TDXCodeProcessor.Decode, Decode>
      //
      //
      //
      // Summary
      // Source Data Stream.
      property InputStream:TStream read FInputStream write FInputStream;
      // OutputStream is the destination for the data after it has
      // been processed.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCodeProcessor.InputStream, InputStream>, <LINK TDXCodeProcessor.Decode, Decode>,
      // <LINK TDXCodeProcessor.Encode, Encode>
      //
      //
      //
      // Summary
      // Destination Data Stream.
      property OutputStream:TStream read FOutputStream write FOutputStream;
   published
      property Method:TDXCodeProcessorMethods read FMethod write FMethod default
         emMIME;
      property OnBeginEncode:TNotifyEvent read FOnBeginEncode write
         FOnBeginEncode;
      property OnEndEncode:TNotifyEvent read FOnEndEncode write FOnEndEncode;
      property OnBeginDecode:TNotifyEvent read FOnBeginDecode write
         FOnBeginDecode;
      property OnEndDecode:TNotifyEvent read FOnEndDecode write FOnEndDecode;
   end;

implementation
uses
   DXMIMEEncode,
   DXMIMEDecode,
   DXUUEncode,
   DXUUDecode,
   DXQuotedEncode,
   DXQuotedDecode;

///////////////////////////////////////////////////////////////////////////////
// Create NMUUE and set code method to MIME by default
///////////////////////////////////////////////////////////////////////////////

constructor TDXCodeProcessor.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fMethod:=emMIME;
end;

// Encode calls the actual encoder logic to process the
// InputStream, and place the encoded results in the
// OutputStream.
//
//
//
// \See Also
//
// <LINK TDXCodeProcessor.Decode, Decode>, <LINK TDXCodeProcessor.InputStream, InputStream>,
// <LINK TDXCodeProcessor.OutputStream, OutputStream>
//
//
//
// Summary
// Execute the Encoder Logic.

procedure TDXCodeProcessor.Encode;
begin
   FInputStream.Position:=0;
   if assigned(FOnBeginEncode) then FOnBeginEncode(self);
   case fmethod of
      emMIME:B64Encode(FInputStream, FOutputStream);
      emUU:UUEEncode(FInputStream, FOutputStream);
      emQuoted:QuotedEncode(FInputStream, FOutputStream);
   end;
   FInputStream.Position:=0;
   FOutputStream.Position:=0;
   if assigned(FOnEndEncode) then FOnEndEncode(self);
end;

// Decode calls the actual decoder logic to process the
// InputStream, and place the decoded results in the
// OutputStream.
//
//
//
// \See Also
//
// <LINK TDXCodeProcessor.Encode, Encode>, <LINK TDXCodeProcessor.InputStream, InputStream>,
// <LINK TDXCodeProcessor.OutputStream, OutputStream>
//
//
//
// Summary
// Execute the Decoder Logic

procedure TDXCodeProcessor.Decode;
begin
   FInputStream.Position:=0;
   if assigned(FOnBeginDecode) then FOnBeginDecode(self);
   case fmethod of
      emMIME:B64Decode(FInputStream, FOutputStream);
      emUU:UUEDecode(FInputStream, FOutputStream);
      emQuoted:QuotedDecode(FInputStream, FOutputStream);
   end;
   FInputStream.Position:=0;
   FOutputStream.Position:=0;
   if assigned(FOnEndDecode) then FOnEndDecode(self);
end;

end.

