unit DXCachedDebugLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedDebugLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is descendant to CachedLogging that offers a simple way to dump data
// in a 'hex' dump for debugging data. We use this in-house when hacking an
// undocumented protocol. Dumping all information down to disk in hex and clean
// ascii. Allowing us to see the command, and characters around those commands.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements a standard "DEBUG" Log
   // Format. While inheriting all the benefits of our cached
   // logging techniques. <COLOR Cool Blue>This is a proprietary
   // log we use very often when dumping inbound data to disk - for
   // debugging.</COLOR>
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
   // A proprietary Log Forma for Debugging.
   TDXCachedDebugLogging=class(TDXCachedLogging)
   private
      // Private declarations
      fLineBreak:string;
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
      // \Note</B> there are no specifications of what you should or
      // should not log. Information is automatically written out in
      // lines of16 characters, and in two columns. First column being
      // a HEX dump of the 16 characters, the next column being a
      // clean string of the 16 raw characters. Use this to track
      // packet errors, or other information that would be important
      // to the person(s) operating the server. Our only wanring to
      // you, if to think long term. The operator may forget about
      // this file, and be very upset if your server fills the drive
      // with too much useless junk.
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(Value:string); virtual;
   published
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

constructor TDXCachedDebugLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fLinebreak:=StringOfChar('-', 78);
end;

destructor TDXCachedDebugLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedDebugLogging.Writeln(Value:string);
var
   Ws:string;

   function CleanAscii(S:string):string;
   var
      Loop:Integer;

   begin
      Result:=S;
      for Loop:=1 to Length(Result) do
         if (Ord(Result[Loop])<32)or(Ord(Result[Loop])>127) then
            Result[Loop]:='.';
   end;

begin
   if Value='' then Exit;
   while Value<>'' do begin
      Ws:=Copy(Value, 1, 16);
      Delete(Value, 1, 16);
      inherited Writeln(HexDump(Ws)+
         StringOfChar(#32, (16-length(Ws))+(16-length(Ws))*2)+
         #32#32#32+CleanAscii(Ws));
   end;
   inherited Writeln(fLineBreak);
end;

end.

