unit DXCachedWEBSITELogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedWEBSITELogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// WEBSITE log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedWEBSITELogging=class(TDXCachedLogging)
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
      procedure Writeln(ClientAddress, ServiceName, RemoteIdent, Method, URI,
         Host,
         UserAgent:string; StatusCode, ByteTransfered, Timetaken:Integer);
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
   NextField=#9;

constructor TDXCachedWEBSITELogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fsDateFormat:='mm/dd/yy hh:mm:ss';
   HostName:=GetLocalHostName;
end;

destructor TDXCachedWEBSITELogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedWEBSITELogging.Writeln(ClientAddress, ServiceName,
   RemoteIdent, Method, URI, Host, UserAgent:string; StatusCode, ByteTransfered,
      Timetaken:Integer);
var
   Value:string;

begin
   if RemoteIdent='' then RemoteIdent:='-';
   if UserAgent='' then UserAgent:='-';
   Value:=FormatDateTime(fsDateFormat, Now);
   Value:=Value+NextField+
      ClientAddress+#32+
      ServiceName+NextField+
      HOSTNAME+NextField+
      RemoteIdent+NextField+
      Method+NextField+
      URI+NextField+
      Host+NextField+
      UserAgent+NextField+
      IntToStr(StatusCode)+NextField+
      IntToStr(ByteTransfered)+NextField+
      IntToStr(TimeTaken);
   inherited Writeln(Value);
end;

end.

