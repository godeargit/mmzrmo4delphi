unit DXCachedNetscapeLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedNetscapeLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// Netscape log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements the standard Netscape (now
   // called iPlanet) Web Server Log Format. While inheriting all
   // the benefits of our cached logging techniques.
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
   // Netscape Web Server Log Format
   TDXCachedNetscapeLogging=class(TDXCachedLogging)
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
      // StatusCode</B> should be set to the HTTP Response you are
      // sending back for this request<B>
      //
      //
      //
      // BytesTransfered</B> should be set to the number of bytes you
      // sent to the client for this request. 0 (zero) for error
      // Responses.<B>
      //
      //
      //
      // Referer</B> should be set to HeaderInfo.Referer<B>
      //
      //
      //
      // UserAgent</B> should be set to HeaderInfo.UserAgent
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(ClientAddress, RemoteIdent, AuthUser,
         ReceivedRequest:string; StatusCode, ByteTransfered:Integer; Referer,
         UserAgent:string);
   published
      // The Netscape log supports a time stamp for each log entry. We
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

constructor TDXCachedNetscapeLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss @GMT@';
end;

destructor TDXCachedNetscapeLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedNetscapeLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedNetscapeLogging.Writeln(ClientAddress, RemoteIdent, AuthUser,
   ReceivedRequest:string; StatusCode, ByteTransfered:Integer; Referer,
   UserAgent:string);
var
   Value:string;

begin
   if AuthUser='' then AuthUser:='-';
   if RemoteIdent='' then RemoteIdent:='-';
   if Referer='' then Referer:='-';
   if UserAgent='' then UserAgent:='-';
   Value:=FormatDateTime(fInternalDF, Now);
   Value:=ClientAddress+#32+RemoteIdent+#32+AuthUser+' ['+Value+'] "'+
      ReceivedRequest+'" '+IntToStr(StatusCode)+#32+IntToStr(ByteTransfered);
   inherited Writeln(Value);
   Value:=Referer+#32+UserAgent+' -';
   inherited Writeln(Value);
end;

end.

