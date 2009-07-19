unit DXFileBuffer;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFileBuffer
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
//  Description:
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

{$WARNINGS OFF}
type
   TDXFileBuffer=class(TDXComponent)
   private
      FSource:TStream;
      FSourceSize:LongInt;
      FBuffer:PChar;
      FBufPos:PChar;
      FBufEnd:PChar;
      FBufSize:LongInt;
   protected
   public
      constructor Create(const aSource:TStream; AOwner:TComponent);
      destructor Destroy; override;

      function NextMemoryBuffer(const Ptr:PChar; const Counter:LongInt):Boolean;
      property BufPos:PChar read FBufPos;
      property BufEnd:PChar read FBufEnd;
      property BufSize:LongInt read FBufSize;
   published
   end;
   {$WARNINGS ON}

implementation

uses
   SysUtils;

const
   MaxBufSize=$FFFD;

constructor TDXFileBuffer.Create(const aSource:TStream; AOwner:TComponent);
begin
   inherited Create(AOwner);
   FBuffer:=AllocMem(MaxBufSize+2);
   if Assigned(aSource) then begin
      FSource:=aSource;
      FSourceSize:=FSource.Size;
      FSource.Position:=0;
      NextMemoryBuffer(FBufPos, 0);
   end;
end;

destructor TDXFileBuffer.Destroy;
begin
   if Assigned(FBuffer) then FreeMem(FBuffer);
end;

function TDXFileBuffer.NextMemoryBuffer(const Ptr:PChar; const
   Counter:LongInt):Boolean;
var
   BytesRead:LongInt;
   FillPos:PChar;
begin
   if FSource.Position<FSourceSize then begin
      FBufPos:=FBuffer+1;
      FillPos:=FBufPos;
      if Counter>0 then begin
         System.Move(Ptr^, FillPos^, Counter);
         inc(FillPos, Counter);
      end;
      BytesRead:=FSource.Read(FillPos^, MaxBufSize-Counter);
      FBufEnd:=FillPos+BytesRead;
      FBufSize:=MaxBufSize;
      if BytesRead<MaxBufSize-Counter then begin
         (FillPos+BytesRead)^:=#0;
         FBufSize:=BytesRead;
      end;
      Result:=True;
   end
   else
      Result:=False;
end;

end.

