unit DXCachedREFERRERLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedREFERRERLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// REFERRER log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedREFERRERLogging=class(TDXCachedLogging)
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
      procedure Writeln(Host, URI:string);
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

constructor TDXCachedREFERRERLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss';
end;

destructor TDXCachedREFERRERLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedREFERRERLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
end;

procedure TDXCachedREFERRERLogging.Writeln(Host, URI:string);
var
   Value:string;

begin
   Value:='['+FormatDateTime(fInternalDF, Now)+'] ';
   Value:=Value+Host+' -> '+URI;
   inherited Writeln(Value);
end;

end.

