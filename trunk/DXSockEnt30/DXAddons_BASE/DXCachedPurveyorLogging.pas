unit DXCachedPurveyorLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedPurveyorLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// Purveyor log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedPurveyorLogging=class(TDXCachedLogging)
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
      procedure Writeln(ClientAddress, RemoteIdent, AuthUser,
         ReceivedRequest:string; StatusCode, ByteTransfered:Integer;
         RequestedDomain:string);
   published
      // Published declarations
      property TimeStampFormat:string read fsDateFormat
         write SetDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

constructor TDXCachedPurveyorLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss @GMT@';
end;

destructor TDXCachedPurveyorLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedPurveyorLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedPurveyorLogging.Writeln(ClientAddress, RemoteIdent, AuthUser,
   ReceivedRequest:string; StatusCode, ByteTransfered:Integer;
   RequestedDomain:string);
var
   Value:string;

begin
   if AuthUser='' then AuthUser:='-';
   if RemoteIdent='' then RemoteIdent:='-';
   Value:=FormatDateTime(fInternalDF, Now);
   Value:=ClientAddress+#32+RemoteIdent+#32+AuthUser+' ['+Value+'] "'+
      ReceivedRequest+'" '+IntToStr(StatusCode)+#32+IntToStr(ByteTransfered)+
      #32+RequestedDomain;
   inherited Writeln(Value);
end;

end.

