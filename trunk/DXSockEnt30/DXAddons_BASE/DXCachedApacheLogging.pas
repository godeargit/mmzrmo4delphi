unit DXCachedApacheLogging;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCachedApacheLogging
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
//  Description: Descendant Cache Logging Component, support Apache Web Format
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes; // TComponent

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements the standard APACHE Web
   // Server Log Format. While inheriting all the benefits of our
   // cached logging techniques.
   //
   //
   //
   // \See Also:
   //
   // <LINK TDXCachedLogging, TDXCachedLogging>
   //
   //
   //
   // Summary
   // Apache Web Server Log Format
   TDXCachedApacheLogging=class(TDXCachedLogging)
   private
      // Private declarations
      fsDateFormat:string;
      fInternalDF:string;
   protected
      // Protected declarations
      procedure SetDateFormat(value:string);
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      // <B>ClientAddress</B> can either be the
      // ClientThread.Socket.PeerIPAddress or if you are using the
      // DXReverseDNSTable component, a
      // .Find(ClientThread.Socket.PeerIPAddress).<B>
      //
      //
      //
      // RemoteIdent</B> should only be stored when the client has
      // sent the HeaderInfo.AuthName<B>
      //
      //
      //
      // AuthUser</B> should be set to HeaderInfo.AuthName<B>
      //
      //
      //
      // ReceivedRequest</B> should be set to
      // HeaderInfo.Method+#32+HeaderInfo.URI<B>
      //
      //
      //
      // Cookie_ID</B> should be set to HeaderInfo.Cookie<B>
      //
      //
      //
      // StatusCode</B> should be set to the HTTP Response you are
      // sending back for this request<B>
      //
      //
      //
      // BytesTransfered</B> should be set to the number of bytes you
      // sent to the client for this request. 0 (zero) for error
      // Responses.
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(ClientAddress, RemoteIdent, AuthUser, ReceivedRequest,
         Cookie_ID:string; StatusCode, ByteTransfered:Integer);
   published
      // The Apache log supports a time stamp for each log entry. We
      // allow you to modify the date format to your flavor.
      // Internally we use the Borland DateTimeFormat command, so your
      // mask should adhere to that functions' rules.<B>
      //
      //
      //
      // \Note</B> the default vaue has been tested and proven to work
      // with WebTrends analysis software.
      //
      //
      //
      // Summary
      // Timestamp format for each line of data in the log file.
      property TimeStampFormat:string read fsDateFormat
         write SetDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

constructor TDXCachedApacheLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss @GMT@';
end;

destructor TDXCachedApacheLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedApacheLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedApacheLogging.Writeln(ClientAddress, RemoteIdent, AuthUser,
   ReceivedRequest, Cookie_ID:string; StatusCode, ByteTransfered:Integer);
var
   Value:string;

begin
   if AuthUser='' then AuthUser:='-';
   if RemoteIdent='' then RemoteIdent:='-';
   Value:=FormatDateTime(fInternalDF, Now);
   if Cookie_ID<>'' then
      if Uppercase(Copy(Cookie_ID, 1, 6))<>'COOKIE' then
         Cookie_ID:='cookie_id='+Cookie_ID;
   Value:=ClientAddress+#32+RemoteIdent+#32+AuthUser+' ['+Value+'] "'+
      ReceivedRequest+'" '+IntToStr(StatusCode)+#32+IntToStr(ByteTransfered)+
      ' "'+Cookie_ID+'"';
   inherited Writeln(Value);
end;

end.

