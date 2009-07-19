unit DXCachedBBSLogging;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCachedBBSLogging
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
//  Description: *Example* Descendant Cached Logging, for a BBS or Client/Server
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes; // TComponent

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedBBSLogging=class(TDXCachedLogging)
   private
      // Private declarations
      fsDateFormat:string;
   protected
      // Protected declarations
      procedure SetDateFormat(value:string);
   public
      // Public declarations
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure Writeln(LineType:Char; Line:string);
   published
      property TimeStampFormat:string read fsDateFormat
         write SetDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

{$IFDEF OBJECTS_ONLY}

constructor TDXCachedBBSLogging.Create;
{$ELSE}

constructor TDXCachedBBSLogging.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
   TimeStampFormat:='dd-mmm-yyyy hh:nn:ss';
end;

destructor TDXCachedBBSLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedBBSLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
end;

procedure TDXCachedBBSLogging.Writeln(LineType:Char; Line:string);
var
   Value:string;

begin
   Value:=FormatDateTime(fsDateFormat, Now);
   inherited Writeln(LineType+#32+Value+#32+Line);
end;

end.

