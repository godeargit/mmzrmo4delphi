unit DXCompressedMsgObject;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCompressedMsgObject
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
//  Description: Descendant MsgObject supporting on-the-fly Compression/Encryption.
// ========================================================================
// This is a descendant of a General message components. The message can be
// loaded from a file or a stream. The concept of this component is when a
// inbound message is received from a socket it comes line by line, you should
// "collect" that in either a stringstream or something like that. Past the
// stream to this component, and then use this component to drop it to disk.
// This component only works with HEADERS, so if you send it a header/body
// collection form the internet as a Stream it tries internally to drop the
// body. We do this so you can develop a body object that does virus or content
// filtering.
//
// This piece uses our modified version of PosInStrArray found in the Winshoes
// library.
///////////////////////////////////////////////////////////////////////////////

uses
   DXGeneralMsgObject,
   Classes;

{$I DXAddons.def}

type
   TDX_CompressForMe=procedure(Stream:TStream) of object;
   TDX_DecompressForMe=procedure(Stream:TStream) of object;

type
   TDXCompressedMsgObject=class(TDXGeneralMsgObject)
   private
      // Private declarations
      DXMsgObject_Body:TStringList;
      FCompressIt:TDX_CompressForMe;
      FDecompressIt:TDX_DecompressForMe;
   protected
      // Protected declarations
   public
      // Public declarations
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure Clear;
      procedure LoadFromStream(Stream:TStream);
      procedure LoadFromFile(const Filename:string);
      procedure SaveToStream(Stream:TStream);
      procedure SaveToFile(const Filename:string);
      procedure DeleteFile(const Filename:string);
   published
      // Published declarations
      property OnCompressStream:TDX_CompressForMe read FCompressIt
         write FCompressIt;
      property OnDecompressStream:TDX_CompressForMe read FCompressIt
         write FCompressIt;
   end;

implementation

uses
   DXString,
   SysUtils;

{$IFDEF OBJECTS_ONLY}

constructor TDXCompressedMsgObject.Create;
{$ELSE}

constructor TDXCompressedMsgObject.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   DXMsgObject_Body:=TStringList.Create
      {$ELSE}
   inherited Create(AOwner);
   if not(csDesigning in ComponentState) then
      DXMsgObject_Body:=TStringList.Create
   else
      DXMsgObject_Body:=nil;
   {$ENDIF}
end;

destructor TDXCompressedMsgObject.Destroy;
begin
   if Assigned(DXMsgObject_Body) then begin
      DXMsgObject_Body.Free;
      DXMsgObject_Body:=nil;
   end;
   inherited Destroy;
end;

procedure TDXCompressedMsgObject.LoadFromStream(Stream:TStream);
var
   TSL:TStringList;
   Loop:Integer;
   LoopMax:Integer;

begin
   TSL:=TStringList.Create;
   TSL.LoadFromStream(Stream);
   Loop:=0;
   HeaderInitialize;
   HeaderProcess(TSL);
   DXMsgObject_Body.Clear;
   LoopMax:=TSL.Count;
   while (Loop<LoopMax)and(TSL[Loop]<>'') do
      Inc(Loop);
   while Loop<LoopMax do begin
      DXMsgObject_Body.Add(TSL[Loop]);
      Inc(loop);
   end;
   TSL.Free;
   TSL:=nil;
end;

procedure TDXCompressedMsgObject.LoadFromFile(const Filename:string);
var
   TSL:TStringList;
   Loop:Integer;
   LoopMax:Integer;
   MemoryStream:TMemoryStream;

begin
   TSL:=TStringList.Create;
   TSL.LoadFromFile(FileName);
   MemoryStream:=TMemoryStream.Create;
   TSL.SaveToStream(MemoryStream);
   if Assigned(FDecompressIt) then begin
      FDecompressIt(MemoryStream);
   end;
   MemoryStream.Seek(0, 0);
   TSL.LoadFromStream(MemoryStream);
   MemoryStream.Free;
   MemoryStream:=nil;
   Loop:=0;
   HeaderInitialize;
   HeaderProcess(TSL);
   DXMsgObject_Body.Clear;
   LoopMax:=TSL.Count;
   while (Loop<LoopMax)and(TSL[Loop]<>'') do
      Inc(Loop);
   while Loop<LoopMax do begin
      DXMsgObject_Body.Add(TSL[Loop]);
      Inc(loop);
   end;
   TSL.Free;
   TSL:=nil;
end;

procedure TDXCompressedMsgObject.SaveToStream(Stream:TStream);
var
   TSL:TStringList;
   Loop:Integer;

begin
   TSL:=TStringList.Create;
   SaveHeaderToList(TSL);
   for Loop:=1 to DXMsgObject_Body.count do
      TSL.Append(DXMsgObject_Body[Loop-1]);
   Stream.Seek(0, 0);
   TSL.SaveToStream(Stream);
   Stream.Seek(0, 0);
   TSL.Free;
   TSL:=nil;
end;

procedure TDXCompressedMsgObject.SaveToFile(const Filename:string);
var
   MemoryStream:TMemoryStream;

begin
   MemoryStream:=TMemoryStream.Create;
   SaveToStream(MemoryStream);
   if Assigned(FCompressIt) then begin
      FCompressIt(MemoryStream);
      MemoryStream.Seek(0, 0);
   end;
   MemoryStream.SaveToFile(FileName);
   MemoryStream.Free;
   MemoryStream:=nil;
end;

procedure TDXCompressedMsgObject.DeleteFile(const Filename:string);
begin
   DeleteFile(FileName);
end;

procedure TDXCompressedMsgObject.Clear;
begin
   DXMsgObject_Body.Clear;
   FillChar(DXMsgObject_Record, Sizeof(DXMsgObject_Record^), #0);
end;

end.

