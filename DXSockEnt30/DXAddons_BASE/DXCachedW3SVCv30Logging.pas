unit DXCachedW3SVCv30Logging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedW3SVCv30Logging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// IIS 3.0 log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedW3SVCv30Logging=class(TDXCachedLogging)
   private
      // Private declarations
      fsDateFormat:string;
      HostName:string;
   protected
      // Protected declarations
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure Writeln(ClientAddress, RemoteIdent, ServiceName, Method, URI,
         QueryString:string; StatusCode, ByteTransfered, TimeTaken:Integer);
   published
      // Published declarations
      property TimeStampFormat:string read fsDateFormat
         write fsDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXSocket,
   DXString,
   SysUtils;

const
   NextField=', ';

constructor TDXCachedW3SVCv30Logging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mm/yy, hh:nn:ss';
   HostName:=GetLocalHostName;
end;

destructor TDXCachedW3SVCv30Logging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedW3SVCv30Logging.Writeln(ClientAddress, RemoteIdent,
   ServiceName, Method, URI, QueryString:string; StatusCode, ByteTransfered,
   TimeTaken:Integer);
var
   Value:string;

begin
   if RemoteIdent='' then RemoteIdent:='-';
   Value:=FormatDateTime(TimeStampFormat, Now);
   Value:=ClientAddress+NextField+
      RemoteIdent+NextField+
      Value+NextField+
      ServiceName+NextField+
      HOSTNAME+NextField+
      IntToStr(ByteTransfered)+NextField+
      '0'+NextField+
      IntToStr(TimeTaken)+NextField+
      IntToStr(StatusCode)+NextField+
      '0'+NextField+
      Method+NextField+
      URI+NextField+
      QueryString+NextField;
   inherited Writeln(Value);
end;

end.

