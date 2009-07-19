unit DXCachedSYSLOGLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedSYSLOGLogging
//    Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999,2002 Brain Patchwork DX.
//   Version: 2.3 (2nd Generation Code)
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// SYSLOG files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXCachedSYSLOGLogging=class(TDXCachedLogging)
   private
      // Private declarations
   protected
      // Protected declarations
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure Writeln(AppAndPriorityByte:Byte; Output:string);
   published
      // Published declarations
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

constructor TDXCachedSYSLOGLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
end;

destructor TDXCachedSYSLOGLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedSYSLOGLogging.Writeln(AppAndPriorityByte:Byte;
   Output:string);
begin
   inherited Writeln('<'+IntToStr(AppAndPriorityByte)+'> '+Output);
end;

end.

