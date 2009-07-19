unit DXCachedBROWSERLogging;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCachedBROWSERLogging
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2002
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2002
//               DX, Inc. Source may not be distributed without
//               written permission from both Brain Patchwork DX,
//               and DX, Inc.
//      License: (Reminder), None of this code can be added to other
//               developer products without permission. This includes
//               but not limited to DCU's, DCP's, DLL's, OCX's, or
//               any other form of merging our technologies. All of
//               your products released to a public consumer be it
//               shareware, freeware, commercial, etc. must contain a
//               license notification somewhere visible in the
//               application.
//               Example is Internet Explorer - Help->About screen
//               shows the licensed code contained in the application.
// Code Version: (3rd Generation Code)
// ========================================================================
//  Description: Descendant Cached Logging, Implementing BROWSER format.
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   // A Log Component that implements the standard APACHE "BROWSER"
   // Web Server Log Format. While inheriting all the benefits of
   // our cached logging techniques.
   //
   //
   //
   // \See Also:
   //
   // <LINK TDXCachedLogging, TDXCachedLogging>, <LINK TDXCachedApacheLogging, TDXCachedApacheLogging>
   //
   //
   //
   // Summary
   // Apache Web Server "Brwoser" Log Format
   TDXCachedBROWSERLogging=class(TDXCachedLogging)
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
      // <B>User_Agent</B> should be set to HeaderInfo.UserAgent
      //
      //
      //
      // Summary
      // Append data to the log.
      procedure Writeln(User_Agent:string);
   published
      // Used internally to show the date of this release in the
      // object inspector. Only useful when reporting bugs to
      // development team.
      // The Browser log supports a time stamp for each log entry. We
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

constructor TDXCachedBROWSERLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss';
end;

destructor TDXCachedBROWSERLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedBROWSERLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
end;

procedure TDXCachedBROWSERLogging.Writeln(User_Agent:string);
var
   Value:string;

begin
   Value:='['+FormatDateTime(fInternalDF, Now)+'] ';
   Value:=Value+User_Agent;
   inherited Writeln(Value);
end;

end.

