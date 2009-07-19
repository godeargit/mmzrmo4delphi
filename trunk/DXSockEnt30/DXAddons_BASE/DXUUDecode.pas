unit DXUUDecode;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXUUDecoder
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
   Classes;

{$I DXAddons.def}

function UUEDecode(const Source:TStream; var Destination:TStream):Boolean;

implementation

const
   EOL=#0;
   EOB=#0;
   CR=#13;
   LF=#10;
   Invalid=$FF;
   MaxBufSize=$FFFE;

function UUEDecode(const Source:TStream; var Destination:TStream):Boolean;
var
   Stream_Ptr:PChar;

   function Decode_Char(c:Char):Byte;
   begin
      Result:=((Byte(c)-$20)and $3F);
   end;

   procedure decode;
   var
      Char1:Byte;
      Char2:Byte;
      Char3:Byte;
      Char4:Byte;
      Line_Ptr:PChar;
      Line_Length:LongInt;
      Line:string;

   begin
      Line_Length:=Decode_Char(Stream_Ptr^);
      inc(Stream_Ptr);
      SetLength(Line, Line_Length);
      Line_Ptr:=PChar(Line);
      while Line_Length>0 do begin
         if Stream_Ptr^>EOL then begin
            Char1:=Decode_Char(Stream_Ptr^);
            inc(Stream_Ptr);
         end
         else char1:=Invalid;
         if Stream_Ptr^>EOL then begin
            Char2:=Decode_Char(Stream_Ptr^);
            inc(Stream_Ptr);
         end
         else char2:=Invalid;
         if Stream_Ptr^>EOL then begin
            Char3:=Decode_Char(Stream_Ptr^);
            inc(Stream_Ptr);
         end
         else char3:=Invalid;
         if Stream_Ptr^>EOL then begin
            Char4:=Decode_Char(Stream_Ptr^);
            inc(Stream_Ptr);
         end
         else char4:=Invalid;
         Line_Ptr^:=Char((Char1 shl 2)or(Char2 shr 4));
         inc(Line_Ptr);
         Line_Ptr^:=Char((Char2 shl 4)or(Char3 shr 2));
         inc(Line_Ptr);
         Line_Ptr^:=Char((Char3 shl 6)or(Char4));
         inc(Line_Ptr);
         dec(Line_Length, 3);
      end;
      Destination.Write(Pointer(Line)^, Length(Line));
   end;

var
   Buffer, FillPos, BufPos:PChar;
   Counter, BytesRead:LongInt;
   SourceSize:LongInt;

begin
   Result:=TRUE;
   SourceSize:=Source.Size;
   destination.Seek(0, soFromEnd);
   Counter:=0;
   GetMem(Buffer, MaxBufSize+1);
   FillPos:=Buffer;
   inc(FillPos, MaxBufSize+1);
   FillPos^:=EOB;
   try
      while Source.Position<SourceSize do begin
         FillPos:=Buffer;
         inc(FillPos, Counter);
         BytesRead:=Source.Read(FillPos^, MaxBufSize-Counter);
         inc(counter, BytesRead);
         BufPos:=Buffer;
         inc(FillPos, Counter);
         FillPos^:=EOB;
         Counter:=0;
         while BufPos^<>EOB do begin
            Stream_Ptr:=BufPos;
            while not(BufPos^in [EOB, LF, CR]) do Inc(BufPos);
            if BufPos^<>EOB then begin
               BufPos^:=EOL;
               Decode;
               if BufPos^=EOB then Inc(BufPos);
               if BufPos^=CR then Inc(BufPos);
               if BufPos^=LF then Inc(BufPos);
            end
            else begin
               Counter:=BufPos-Stream_Ptr;
               System.Move(Stream_Ptr^, Buffer^, Counter+1);
               Break;
            end;
         end;
      end;
      if Counter>0 then begin
         Stream_Ptr:=Buffer;
         Decode;
      end;
   finally
      FreeMem(Buffer, MaxBufSize);
   end;
end;

end.

