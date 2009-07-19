unit DXUUEncode;

interface

///////////////////////////////////////////////////////////////////////////////
//         Unit: DXUUEncoder
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

function UUEEncode(const source, destination:TStream):Boolean;

implementation

uses
   DXFileBuffer;

function UUEEncode(const source, destination:TStream):Boolean;

   function EncodeAByte(c:Byte):Byte;
   begin
      if c>0 then Result:=(c and $3F)+$20
      else Result:=ord('`');
   end;

const
   CRLF=#13#10;
   ENCODELINELENGTH=45;
   EOB=#0;
   INVALID=$00;

var
   TotalBytes:Integer;
   index:Integer;
   Output:string;
   line_index:Integer;
   LinePtr:PChar;
   Len:Integer;
   Len1:Integer;
   OutputLength:Integer;
   Char1:Byte;
   Char2:Byte;
   Char3:Byte;
   OutChar1:Byte;
   OutChar2:Byte;
   OutChar3:Byte;
   OutChar4:Byte;
   InputPtr:PChar;
   BufferEnd:PChar;
   MemStuff:TDXFileBuffer;
   Process:Boolean;

begin
   Process:=True;
   destination.Seek(0, soFromEnd);
   TotalBytes:=source.Size;
   index:=0;
   Char1:=0;
   Char2:=0;
   Char3:=0;
   Output:='';
   OutputLength:=ENCODELINELENGTH div 3*4;
   {$IFDEF OBJECTS_ONLY}
   MemStuff:=TDXFileBuffer.Create(Source);
   {$ELSE}
   MemStuff:=TDXFileBuffer.Create(Source, nil);
   {$ENDIF}
   try
      InputPtr:=MemStuff.BufPos;
      BufferEnd:=MemStuff.BufEnd;
      while Process do begin
         line_index:=0;
         if ((TotalBytes-index)>=ENCODELINELENGTH) then begin
            SetLength(Output, OutputLength+1);
            LinePtr:=PChar(Output);
            OutChar1:=EncodeAByte(ENCODELINELENGTH);
            LinePtr^:=Chr(OutChar1);
            inc(LinePtr);
            while (line_index<ENCODELINELENGTH) do begin
               if InputPtr>=BufferEnd then
                  if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                  else begin
                     InputPtr:=MemStuff.BufPos;
                     BufferEnd:=MemStuff.BufEnd;
                  end;
               Char1:=Byte(InputPtr^);
               inc(InputPtr);
               inc(line_index);
               inc(index);
               if InputPtr>=BufferEnd then
                  if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                  else begin
                     InputPtr:=MemStuff.BufPos;
                     BufferEnd:=MemStuff.BufEnd;
                  end;
               Char2:=Byte(InputPtr^);
               inc(InputPtr);
               inc(line_index);
               inc(index);
               if InputPtr>=BufferEnd then
                  if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                  else begin
                     InputPtr:=MemStuff.BufPos;
                     BufferEnd:=MemStuff.BufEnd;
                  end;
               Char3:=Byte(InputPtr^);
               inc(InputPtr);
               inc(line_index);
               inc(index);
               OutChar1:=Char1 shr 2;
               OutChar2:=(((Char1 shl 4)and $30)or((Char2 shr 4)and $0F));
               OutChar3:=(((Char2 shl 2)and $3C)or((Char3 shr 6)and $03));
               OutChar4:=Char3 and $3F;
               OutChar1:=EncodeAByte(OutChar1);
               OutChar2:=EncodeAByte(OutChar2);
               OutChar3:=EncodeAByte(OutChar3);
               OutChar4:=EncodeAByte(OutChar4);
               LinePtr^:=Chr(OutChar1);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar2);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar3);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar4);
               inc(LinePtr);
            end;
            if InputPtr>=BufferEnd then
               if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
               else begin
                  InputPtr:=MemStuff.BufPos;
                  BufferEnd:=MemStuff.BufEnd;
               end;
         end
         else begin
            Len:=TotalBytes-index;
            Len1:=Len div 3;
            if frac(Len/3)>0.0 then inc(Len1);
            Len1:=Len1*4;
            SetLength(Output, Len1+1);
            LinePtr:=PChar(Output);
            OutChar1:=EncodeAByte(Len);
            LinePtr^:=Chr(OutChar1);
            inc(LinePtr);
            while (index<TotalBytes) do begin
               if (index<TotalBytes) then begin
                  if InputPtr>=BufferEnd then
                     if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                     else begin
                        InputPtr:=MemStuff.BufPos;
                        BufferEnd:=MemStuff.BufEnd;
                     end;
                  Char1:=Byte(InputPtr^);
                  inc(InputPtr);
               end
               else begin
                  Char1:=INVALID;
               end;
               inc(index);
               if (index<TotalBytes) then begin
                  if InputPtr>=BufferEnd then
                     if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                     else begin
                        InputPtr:=MemStuff.BufPos;
                        BufferEnd:=MemStuff.BufEnd;
                     end;
                  Char2:=Byte(InputPtr^);
                  inc(InputPtr);
               end
               else begin
                  Char2:=INVALID;
               end;
               inc(index);
               if (index<TotalBytes) then begin
                  if InputPtr>=BufferEnd then
                     if not MemStuff.NextMemoryBuffer(InputPtr, 0) then Break
                     else begin
                        InputPtr:=MemStuff.BufPos;
                        BufferEnd:=MemStuff.BufEnd;
                     end;
                  Char3:=Byte(InputPtr^);
                  inc(InputPtr);
               end
               else begin
                  Char3:=INVALID;
               end;
               inc(index);
               OutChar1:=Char1 shr 2;
               OutChar2:=(((Char1 shl 4)and $30)or((Char2 shr 4)and $0F));
               OutChar3:=(((Char2 shl 2)and $3C)or((Char3 shr 6)and $03));
               OutChar4:=Char3 and $3F;
               OutChar1:=EncodeAByte(OutChar1);
               OutChar2:=EncodeAByte(OutChar2);
               OutChar3:=EncodeAByte(OutChar3);
               OutChar4:=EncodeAByte(OutChar4);
               LinePtr^:=Chr(OutChar1);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar2);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar3);
               inc(LinePtr);
               LinePtr^:=Chr(OutChar4);
               inc(LinePtr);
            end;
            Process:=False;
         end;
         LinePtr^:=#0;
         destination.Write(PChar(Output)^, Length(Output));
         destination.Write(CRLF, 2);
      end;
   finally
      MemStuff.Free;
   end;
   Result:=TRUE; 
end;

end.

