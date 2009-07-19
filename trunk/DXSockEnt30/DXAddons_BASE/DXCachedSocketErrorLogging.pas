unit DXCachedSocketErrorLogging;

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
   TDXCachedSocketErrorLogging=class(TDXCachedLogging)
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
      procedure Writeln(SocketErrorCode:Integer);
   published
      // Published declarations
      property TimeStampFormat:string read fsDateFormat
         write SetDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXSocket,
   DXString,
   SysUtils;

constructor TDXCachedSocketErrorLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='mm-dd-yyyy hh:nn:ss';
end;

destructor TDXCachedSocketErrorLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedSocketErrorLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedSocketErrorLogging.Writeln(SocketErrorCode:Integer);
var
   Value:string;

begin
   if fsDateFormat<>'' then
      Value:=FormatDateTime(fInternalDF, Now)+#32
   else
      Value:='';
   Value:=Value+GetErrorDesc(SocketErrorCode)+' ('+IntToStr(SocketErrorCode)+')';
   inherited Writeln(Value);
end;

end.

