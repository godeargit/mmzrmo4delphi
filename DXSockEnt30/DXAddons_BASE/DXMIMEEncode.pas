unit DXMIMEEncode;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXMIMEEncode
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

function B64Encode(const source, destination:TStream):Boolean;

implementation

uses
   DXFileBuffer;

var
   B64Table:array[0..63] of Char=
      ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
      'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
      'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
      'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
      'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
      'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
      'w', 'x', 'y', 'z', '0', '1', '2', '3',
      '4', '5', '6', '7', '8', '9', '+', '/');

function B64Encode(const source, destination:TStream):Boolean;
var
   Counter:LongInt;
   TotalBytes:LongInt;
   EOD:Char;
   B64Char:Char;
   Char1:Char;
   Char2:Char;
   Char3:Char;
   InputPtr, OutputPtr, BufferEnd:PChar;
   MemStuff:TDXFileBuffer;
   Output:string;
   Len:Integer;

begin
   Counter:=0;
   TotalBytes:=source.Size;
   EOD:='=';
   Len:=62;
   Output:='';
   SetLength(Output, Len);
   OutputPtr:=PChar(Output);
   destination.Seek(0, soFromEnd);
   {$IFDEF OBJECTS_ONLY}
   MemStuff:=TDXFileBuffer.Create(Source);
   {$ELSE}
   MemStuff:=TDXFileBuffer.Create(Source, nil);
   {$ENDIF}
   try
      InputPtr:=MemStuff.BufPos;
      BufferEnd:=MemStuff.BufEnd;
      while True do begin
         if InputPtr>=BufferEnd then
            if not MemStuff.NextMemoryBuffer(InputPtr, 0) then
               Break
            else begin
               InputPtr:=MemStuff.BufPos;
               BufferEnd:=MemStuff.BufEnd;
            end;
         Char1:=InputPtr^;
         inc(InputPtr);
         B64Char:=B64Table[Byte(Char1)shr 2];
         OutputPtr^:=B64Char;
         inc(OutputPtr);
         inc(Counter);
         if Counter>=TotalBytes then begin
            Char2:=#0;
            B64Char:=B64Table[(((Byte(Char1)and $03)shl 4)or((Byte(Char2)and
               $F0)shr 4))];
            OutputPtr^:=B64Char;
            inc(OutputPtr);
            OutputPtr^:=EOD;
            inc(OutputPtr);
            OutputPtr^:=EOD;
            inc(OutputPtr);
         end
         else begin
            if InputPtr>=BufferEnd then
               if not MemStuff.NextMemoryBuffer(InputPtr, 0) then
                  Break
               else begin
                  InputPtr:=MemStuff.BufPos;
                  BufferEnd:=MemStuff.BufEnd;
               end;
            Char2:=InputPtr^;
            inc(InputPtr);
            B64Char:=B64Table[(((Byte(Char1)and $03)shl 4)or((Byte(Char2)and
               $F0)shr 4))];
            OutputPtr^:=B64Char;
            inc(OutputPtr);
            inc(Counter);
            if Counter>=TotalBytes then begin
               Char3:=#0;
               B64Char:=B64Table[(((Byte(Char2)and $0F)shl 2)or((Byte(Char3)and
                  $C0)shr 6))];
               OutputPtr^:=B64Char;
               inc(OutputPtr);
               OutputPtr^:=EOD;
               inc(OutputPtr);
            end
            else begin
               if InputPtr>=BufferEnd then
                  if not MemStuff.NextMemoryBuffer(InputPtr, 0) then
                     Break
                  else begin
                     InputPtr:=MemStuff.BufPos;
                     BufferEnd:=MemStuff.BufEnd;
                  end;
               Char3:=InputPtr^;
               inc(InputPtr);
               B64Char:=B64Table[(((Byte(Char2)and $0F)shl 2)or((Byte(Char3)and
                  $C0)shr 6))];
               OutputPtr^:=B64Char;
               inc(OutputPtr);
               inc(Counter);
               B64Char:=B64Table[(Byte(Char3)and $3F)];
               OutputPtr^:=B64Char;
               inc(OutputPtr);
            end;
         end;
         if (Counter mod 45=0)or((OutputPtr-1)^=EOD) then begin
            if ((OutputPtr-1)^<>EOD) then begin
               OutputPtr^:=#13;
               inc(OutputPtr);
               OutputPtr^:=#10;
               inc(OutputPtr);
               OutputPtr^:=#0;
            end
            else begin
               len:=LongInt(OutputPtr)-LongInt(Output);
               SetLength(Output, len);
            end;
            destination.Write(PChar(Output)^, Len);
            OutputPtr:=PChar(Output);
         end;
      end;
   finally
      MemStuff.Free;
   end;
   len:=LongInt(OutputPtr)-LongInt(Output);
   if len>0 then begin
      SetLength(Output, len);
      destination.Write(PChar(Output)^, Len);
   end;
   destination.Write(EOD, 1);
   result:=TRUE;
end;

end.

