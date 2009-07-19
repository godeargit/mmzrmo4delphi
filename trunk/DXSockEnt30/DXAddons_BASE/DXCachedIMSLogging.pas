unit DXCachedIMSLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedIMSLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// IMS (Internet Mail Server) log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements a standard EMail Server Log
   // Format. While inheriting all the benefits of our cached
   // logging techniques.
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
   // IMS EMail Server Log Format
   TDXCachedIMSLogging=class(TDXCachedLogging)
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
      // <B>MessageID</B> should be set to the Identification of the
      // Message being sent or received.<B>
      //
      //
      //
      // RemoteIP</B> could be set to either
      // ClientThread.Socket.PeerIPAddress or if you are using the
      // DXReverseDNSTable component, a
      // .Find(ClientThread.Socket.PeerIPAddress).<B>
      //
      //
      //
      // HELO</B> should be set to the HELO string received from the
      // client during the initial login process.<B>
      //
      //
      //
      // MailFROM</B> should be set to the MAIL From string received
      // from the client during the initial message upload process.<B>
      //
      //
      //
      // RCPTTo</B> should be set to the RCPT To string received from
      // the client during the initial message upload process.
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(MessageID, RemoteIP, HELO, MailFROM, RCPTTo:string);
   published
      // The IMS log supports a time stamp for each log entry. We
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

constructor TDXCachedIMSLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss @GMT@';
end;

destructor TDXCachedIMSLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedIMSLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedIMSLogging.Writeln(MessageID, RemoteIP, HELO, MailFROM,
   RCPTTo:string);
var
   Value:string;

begin
   Value:=FormatDateTime(fInternalDF, Now);
   Value:='<'+MessageID+'> ['+Value+'] '+RemoteIP+#32+HELO+#32+MailFrom+#32+RCPTTo;
   inherited Writeln(Value);
end;

end.

