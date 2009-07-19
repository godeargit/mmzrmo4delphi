unit DXCachedFlexibleLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedFlexibleLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is descendant to CachedLogging that adds some nice features to the
// like Hex Dump, Time Stamping, Etc.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements a flexible Log Format. While
   // inheriting all the benefits of our cached logging techniques.
   // <COLOR Cool Blue>This is a proprietary log we use very often
   // in many of our applications where a standard log format is
   // not available.</COLOR>
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
   // A proprietary Log Forma that is very flexible.
   TDXCachedFlexibleLogging=class(TDXCachedLogging)
   private
      // Private declarations
      FPreTime:Boolean;
      fsDateFormat:string;
      fuseHEX:Boolean;
   protected
      // Protected declarations
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      // <B>Value</B> could be any debug information you wish to log.<B>
      //
      //
      //
      // \Note</B> depending upon the properties this can function
      // exactly like our TDXCachedDebugLogging component, or some
      // other type of logging format. This is why it is called
      // flexible.
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(Value:string); virtual;
   published
      // Option to enable or disable the internal applying of the
      // timestamp to each line written to the log file.
      //
      //
      //
      // Summary
      // Enable/Disable timestampinng log lines.
      property PrefixTimeStamp:Boolean read FPreTime
         write FPretime;
      // The log optionally supports a time stamp for each log entry.
      // We allow you to modify the date format to your flavor.
      // Internally we use the Borland DateTimeFormat command, so your
      // mask should adhere to that functions' rules.
      //
      //
      //
      // \See Also
      //
      // <LINK TDXCachedFlexibleLogging.PrefixTimeStamp, PrefixTimeStamp>
      //
      //
      //
      // Summary
      // Timestamp format for each line of data in the log file.
      property TimeStampFormat:string read fsDateFormat
         write fsDateFormat;
      // Option to enable or disable internally writing the string out
      // put in HEX. When enabled this output is very close to that of
      // TDXCachedDEBUGLogging.
      //
      //
      //
      // Summary
      // Enabled/Disable log to be stored in HEX.
      property SaveInHEX:Boolean read fuseHEX
         write fuseHEX;
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

constructor TDXCachedFlexibleLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fsDateFormat:='dddd, mmmm dd, yyyy hh:nn:ss';
   fusehex:=false;
   FPreTime:=True;
end;

destructor TDXCachedFlexibleLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedFlexibleLogging.Writeln(Value:string);
begin
   if fuseHex then Value:=HexDump(Value);
   if FPreTime then Value:=FormatDateTime(fsDateFormat, Now)+' '+Value;
   inherited Writeln(Value);
end;

end.

