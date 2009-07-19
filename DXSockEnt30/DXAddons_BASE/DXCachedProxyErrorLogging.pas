unit DXCachedProxyErrorLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedErrorLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// Socket Error log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements a standard Proxy Server Error
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
   // Proxy Server "Error" Log Format
   TDXCachedProxyErrorLogging=class(TDXCachedLogging)
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
      {$IFDEF VER100}
      procedure Writeln(SocketErrorCode:Integer; Host:string);
      // <B>Also Note</B> this is the Delphi 2 and Delphi 3 command,
      // new versions of Delphi suipport overload, and you will only
      // see difference versions of Add.
      //
      //
      //
      // See <LINK TDXCachedProxyErrorLogging.Writeln, Writeln>
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure GeneralWriteln(Message:string);
      {$ELSE}
      // <B>SocketErrorCode</B> should be set to the
      // TDXSock.LastCommandStatus<B>
      //
      //
      //
      // Host</B> should be set to the server which the
      // connection/session error occured.<B>
      //
      //
      //
      // \Note</B> normally the next line in the log file would
      // contain the issue, if implemented.
      //
      //
      //
      // See
      //
      // <LINK TDXCachedProxErrorLogging.GeneralWriteln, GeneralWriteln>
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(SocketErrorCode:Integer; Host:string); overload;
      // After reporting the socket and host, the next action should
      // be posting the actual issue via this command.<B>
      //
      //
      //
      // \Note</B> this command is not available to Delphi 2 or Delphi
      // 3.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCachedProxErrorLogging.GeneralWriteln, GeneralWriteln>,
      // <LINK TDXCachedProxErrorLogging.Writeln, Writeln>
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(Message:string); overload;
      {$ENDIF}
   published
      // The Proxy Error log supports a time stamp for each log entry.
      // We allow you to modify the date format to your flavor.
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
   DXSocket,
   DXString,
   SysUtils;

constructor TDXCachedProxyErrorLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='mm-dd-yyyy hh:nn:ss';
end;

destructor TDXCachedProxyErrorLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedProxyErrorLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

{$IFDEF VER100}
procedure TDXCachedProxyErrorLogging.Writeln(SocketErrorCode:Integer;
   Host:string);
{$ELSE}
procedure TDXCachedProxyErrorLogging.Writeln(SocketErrorCode:Integer;
   Host:string);
{$ENDIF}
var
   Value:string;

begin
   if fsDateFormat<>'' then
      Value:=fsDateFormat+#32
   else
      Value:='';
   Value:=Value+GetErrorDesc(SocketErrorCode)+' ('+IntToStr(SocketErrorCode)+')';
   if Host<>'' then Value:=Value+' (HOST: '+uppercase(HOST)+')';
   inherited Writeln(Value);
end;

{$IFDEF VER100}
procedure TDXCachedProxyErrorLogging.GeneralWriteln(Message:string);
{$ELSE}
procedure TDXCachedProxyErrorLogging.Writeln(Message:string);
{$ENDIF}
var
   Value:string;

begin
   if fsDateFormat<>'' then
      Value:=fsDateFormat+#32
   else
      Value:='';
   Value:=Value+Message;
   inherited Writeln(Value);
end;

end.

