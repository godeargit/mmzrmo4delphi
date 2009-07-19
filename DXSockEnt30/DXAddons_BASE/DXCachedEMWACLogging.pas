unit DXCachedEMWACLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedEMWACLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// EMWAC log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements the standard EMWAC Web Server
   // Log Format. While inheriting all the benefits of our cached
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
   // EMWAC Web Server Log Format
   TDXCachedEMWACLogging=class(TDXCachedLogging)
   private
      // Private declarations
      fsDateFormat:string;
   protected
      // Protected declarations
      procedure SetDateFormat(value:string);
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      // <B>LocalIPAddressr</B> should be set to
      // ClientThread.Socket.LocalIPAddress<B>
      //
      //
      //
      // PeerIPAddress</B> should be set to
      // ClientThread.Socket.PeerIPAddress<B>
      //
      //
      //
      // ReceivedRequest</B> shuold be set to
      // HeaderInfo.Method+#32+HeaderInfo.URI or HeaderInfo.Raw
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(LocalIPAddress, PeerIPAddress, ReceivedRequest:string);
   published
      // The EMWAC log supports a time stamp for each log entry. We
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

constructor TDXCachedEMWACLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='ddd mmm dd hh:nn:ss yyyy';
end;

destructor TDXCachedEMWACLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedEMWACLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
end;

procedure TDXCachedEMWACLogging.Writeln(LocalIPAddress, PeerIPAddress,
   ReceivedRequest:string);
var
   Value:string;

begin
   Value:=FormatDateTime(fsDateFormat, Now)+#32+LocalIPAddress+#32+
      PeerIPAddress+#32+ReceivedRequest;
   inherited Writeln(Value);
end;

end.

