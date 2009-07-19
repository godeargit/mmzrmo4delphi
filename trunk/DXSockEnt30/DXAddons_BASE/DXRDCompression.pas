unit DXRDCompression;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXRDCompression
//    C++Author: Ed Ross
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
// The code is based upon source code found in "The C Users Journal", 1/92. It
// has been ported to Delphi by different people over the past 3 years, mainly
// Ozz Nixon and Richard Griffin. And has been incorporated into the BPDX
// socket suites by request from many customers who need compressed data. Since
// we do not own the original design rights, we include this component freely
// for use with any of our component suites.
///////////////////////////////////////////////////////////////////////////////

uses
   Classes;

{$I DXAddons.def}

type
   {$IFDEF OBJECTS_ONLY}
   TDXRDCompression=class
      {$ELSE}
   TDXRDCompression=class(TComponent)
      {$ENDIF}
   private
      fOnBlockLengthError:TNotifyEvent;
      fOnDataUnderrun:TNotifyEvent;
   protected
   public
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure CompressStreams(inStream, OutStream:TStream);
      procedure DecompressStreams(inStream, OutStream:TStream);
      procedure CompressString(const OriginalStr:string; var OutStr:string);
      procedure DecompressString(const OriginalStr:string; var OutStr:string);
      procedure CompressWindowsFileToFile(var infile, outfile:Integer);
      procedure DecompressWindowsFileToFile(var infile, outfile:Integer);
      procedure CompressBorlandFileToFile(var infile, outfile:file);
      procedure DecompressBorlandFileToFile(var infile, outfile:file);
   published
      property OnBlockLengthError:TNotifyEvent read fOnBlockLengthError
         write fOnBlockLengthError;
      property OnDataUnderrun:TNotifyEvent read fOnDataUnderrun
         write fOnDataUnderrun;
   end;

implementation

uses
   SysUtils,
   DXString;

const
   HASH_LEN=4096;
   HASH_SIZE=HASH_LEN*Sizeof(word);
   BUFF_LEN=8192;

type
   PByte=^Byte;
   ByteArray=array[0..65000] of Byte;
   PByteArray=^ByteArray;

   PWord=^Word;
   WordArray=array[0..32000] of Word;
   PWordArray=^WordArray;

   {$IFDEF OBJECTS_ONLY}

constructor TDXRDCompression.Create;
{$ELSE}

constructor TDXRDCompression.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
end;

destructor TDXRDCompression.Destroy;
begin
   inherited Destroy;
end;

function compress(ibuff:PByte; inbuff_len:Smallint; obuff:PByte;
   htable:Pointer):SmallInt;
var
   inbuff:PByte absolute ibuff;
   outbuff:PByte absolute obuff;
   hash_tbl:PWordArray absolute htable;
   in_idx:Pbyte;
   in_idxa:PByteArray absolute in_idx;
   inbuff_end,
      anchor,
      out_idx,
      outbuff_end,
      pat_idx:PByte;
   cnt,
      gap,
      c,
      hash,
      hashlen,
      ctrl_cnt,
      ctrl_bits:Word;
   ctrl_idx:PWord;

begin
   in_idx:=inbuff;
   inbuff_end:=Pointer(LongInt(inbuff)+inbuff_len);
   ctrl_idx:=Pointer(outbuff);
   ctrl_cnt:=0;
   ctrl_bits:=0;
   out_idx:=Pointer(longint(outbuff)+Sizeof(Word));
   outbuff_end:=Pointer(LongInt(outbuff)+(inbuff_len-48));
   if inbuff_len<=18 then begin
      Move(inbuff^, outbuff^, inbuff_len);
      Result:=0-inbuff_len;
      Exit;
   end;
   hashlen:=HASH_LEN-1;
   while LongInt(in_idx)<LongInt(inbuff_end) do begin
      if ctrl_cnt=16 then begin
         ctrl_idx^:=ctrl_bits;
         ctrl_cnt:=1;
         ctrl_idx:=Pointer(out_idx);
         Inc(out_idx, 2);
         if LongInt(out_idx)>LongInt(outbuff_end) then begin
            Move(inbuff^, outbuff^, inbuff_len);
            Result:=0-inbuff_len;
            Exit;
         end;
      end
      else
         Inc(ctrl_cnt);
      anchor:=in_idx;
      c:=in_idx^;
      Inc(in_idx);
      while (LongInt(in_idx)<longint(inbuff_end))
         and(in_idx^=c)
         and(LongInt(in_idx)-LongInt(anchor)<(HASH_LEN+18)) do
         Inc(in_idx);
      cnt:=LongInt(in_idx)-LongInt(anchor);
      if cnt>2 then begin
         if cnt<=18 then begin
            out_idx^:=cnt-3;
            Inc(out_idx);
            out_idx^:=c;
            Inc(out_idx);
         end
         else begin
            Dec(cnt, 19);
            out_idx^:=16+(cnt and $0F);
            Inc(out_idx);
            out_idx^:=cnt shr 4;
            Inc(out_idx);
            out_idx^:=c;
            Inc(out_idx);
         end;
         ctrl_bits:=(ctrl_bits shl 1)or 1;
         Continue;
      end;
      in_idx:=anchor;
      if (LongInt(inbuff_end)-LongInt(in_idx))>2 then begin
         hash:=((((in_idxa^[0]and 15)shl 8)or in_idxa^[1])xor
            ((in_idxa^[0]shr 4)or(in_idxa^[2]shl 4)))
            and hashlen;
         pat_idx:=in_idx;
         move(hash_tbl^[hash], pat_idx, 2);
         hash_tbl^[hash]:=Word(in_idx);
         gap:=LongInt(in_idx)-LongInt(pat_idx);
         if (gap<=HASH_LEN+2) then begin
            while (LongInt(in_idx)<LongInt(inbuff_end))
               and(LongInt(pat_idx)<LongInt(anchor))
               and(pat_idx^=in_idx^)
               and(LongInt(in_idx)-LongInt(anchor)<271) do begin
               Inc(in_idx);
               Inc(pat_idx);
            end;
            cnt:=LongInt(in_idx)-LongInt(anchor);
            if cnt>2 then begin
               Dec(gap, 3);
               if cnt<=15 then begin
                  out_idx^:=(cnt shl 4)+(gap and $0F);
                  Inc(out_idx);
                  out_idx^:=gap shr 4;
                  Inc(out_idx);
               end
               else begin
                  out_idx^:=32+(gap and $0F);
                  Inc(out_idx);
                  out_idx^:=gap shr 4;
                  Inc(out_idx);
                  out_idx^:=cnt-16;
                  Inc(out_idx);
               end;
               ctrl_bits:=(ctrl_bits shl 1)or 1;
               Continue;
            end;
         end;
      end;
      out_idx^:=c;
      Inc(out_idx);
      Inc(anchor);
      in_idx:=anchor;
      ctrl_bits:=ctrl_bits shl 1;
   end;
   ctrl_bits:=ctrl_bits shl(16-ctrl_cnt);
   ctrl_idx^:=ctrl_bits;
   Result:=LongInt(out_idx)-LongInt(outbuff);
end;

function Decompress(inbuff:PByte; inbuff_len:Word;
   outbuff:PByte):Integer;
var
   ctrl_bits,
      cmd,
      cnt,
      ofs,
      ctrl_mask:Word;
   inbuff_idx,
      outbuff_idx,
      inbuff_end,
      outbuff_src:PByte;

begin
   ctrl_bits:=0;
   ctrl_mask:=0;
   inbuff_idx:=inbuff;
   outbuff_idx:=outbuff;
   inbuff_end:=Pointer(LongInt(inbuff)+inbuff_len);
   while LongInt(inbuff_idx)<LongInt(inbuff_end) do begin
      ctrl_mask:=ctrl_mask shr 1;
      if ctrl_mask=0 then begin
         ctrl_bits:=PWord(inbuff_idx)^;
         Inc(inbuff_idx, 2);
         ctrl_mask:=$8000;
      end;
      if (ctrl_bits and ctrl_mask)=0 then begin
         outbuff_idx^:=inbuff_idx^;
         Inc(outbuff_idx);
         Inc(inbuff_idx);
         Continue;
      end;
      cmd:=(inbuff_idx^shr 4)and $0F;
      cnt:=inbuff_idx^and $0F;
      Inc(inbuff_idx);
      case cmd of
         0:begin
               Inc(cnt, 3);
               FillChar(outbuff_idx^, cnt, inbuff_idx^);
               Inc(inbuff_idx);
               Inc(outbuff_idx, cnt);
            end;
         1:begin
               Inc(cnt, inbuff_idx^shl 4);
               Inc(inbuff_idx);
               Inc(cnt, 19);
               FillChar(outbuff_idx^, cnt, inbuff_idx^);
               Inc(inbuff_idx);
               Inc(outbuff_idx, cnt);
            end;
         2:begin
               ofs:=cnt+3;
               Inc(ofs, inbuff_idx^shl 4);
               Inc(inbuff_idx);
               cnt:=inbuff_idx^;
               Inc(inbuff_idx);
               Inc(cnt, 16);
               outbuff_src:=Pointer(LongInt(outbuff_idx)-ofs);
               Move(outbuff_src^, outbuff_idx^, cnt);
               Inc(outbuff_idx, cnt);
            end;
      else begin
            ofs:=cnt+3;
            Inc(ofs, inbuff_idx^shl 4);
            Inc(inbuff_idx);
            outbuff_src:=Pointer(LongInt(outbuff_idx)-ofs);
            Move(outbuff_src^, outbuff_idx^, cmd);
            Inc(outbuff_idx, cmd);
         end;
      end; {case}
   end;
   Result:=LongInt(outbuff_idx)-LongInt(outbuff);
end;

procedure TDXRDCompression.CompressBorlandFileToFile(var infile, outfile:file);
var
   code,
      bytes_read,
      compress_len:Integer;
   HashPtr:PWordArray;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   Getmem(HashPtr, HASH_SIZE);
   Fillchar(hashPtr^, HASH_SIZE, #0);
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   bytes_read:=BUFF_LEN;
   while bytes_read=BUFF_LEN do begin
      Blockread(infile, inputbuffer^, BUFF_LEN, bytes_read);
      compress_len:=Compress(PByte(inputbuffer), bytes_read,
         PByte(outputbuffer), HashPtr);
      Blockwrite(outfile, compress_len, 2, code);
      if compress_len<0 then compress_len:=0-compress_len;
      Blockwrite(outfile, outputbuffer^, compress_len, code);
   end;
   compress_len:=0;
   Blockwrite(outfile, compress_len, 2, code);
   Freemem(HashPtr, HASH_SIZE);
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.CompressWindowsFileToFile(var infile,
   outfile:Integer);
var
   compress_len:Integer;
   bytes_read:DWord;
   HashPtr:PWordArray;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   Getmem(HashPtr, HASH_SIZE);
   Fillchar(hashPtr^, HASH_SIZE, #0);
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   bytes_read:=BUFF_LEN;
   while bytes_read=BUFF_LEN do begin
      bytes_read:=FileRead(infile, inputbuffer^, BUFF_LEN);
      compress_len:=Compress(PByte(inputbuffer), bytes_read,
         PByte(outputbuffer), HashPtr);
      {code:=} FileWrite(outfile, compress_len, 2);
      if compress_len<0 then compress_len:=0-compress_len;
      {code:=} FileWrite(outfile, outputbuffer^, compress_len);
   end;
   compress_len:=0;
   {code:=} FileWrite(outfile, compress_len, 2);
   Freemem(HashPtr, HASH_SIZE);
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.CompressString(const OriginalStr:string; var
   OutStr:string);
var
   compress_len:Integer;
   bytes_read:DWord;
   HashPtr:PWordArray;
   inputbuffer,
      outputbuffer:PByteArray;
   InStr:string;

begin
   InStr:=OriginalStr;
   OutStr:='';
   Getmem(HashPtr, HASH_SIZE);
   Fillchar(hashPtr^, HASH_SIZE, #0);
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   bytes_read:=BUFF_LEN;
   while bytes_read=BUFF_LEN do begin
      if Length(InStr)>BUFF_LEN then
         bytes_read:=BUFF_LEN
      else
         bytes_read:=Length(InStr);
      Move(inStr[1], inputbuffer^, bytes_read);
      Delete(inStr, 1, bytes_read);
      compress_len:=Compress(PByte(inputbuffer), bytes_read,
         PByte(outputbuffer), HashPtr);
      SetLength(OutStr, Length(OutStr)+2);
      Move(compress_len, OutStr[Length(OutStr)-1], 2);
      if compress_len<0 then compress_len:=0-compress_len;
      SetLength(OutStr, Length(OutStr)+compress_len);
      Move(outputbuffer^, OutStr[(Length(OutStr)-compress_len)+1],
         compress_len);
   end;
   compress_len:=0;
   SetLength(OutStr, Length(OutStr)+2);
   Move(compress_len, OutStr[Length(OutStr)-1], 2);
   Freemem(HashPtr, HASH_SIZE);
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

type
   AccessProtected=class(TStream);

procedure TDXRDCompression.CompressStreams(inStream, OutStream:TStream);
var
   bytes_read,
      compress_len:Integer;
   HashPtr:PWordArray;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   {$IFDEF VER90}
   if OutStream.Size>0 then begin
      // Size should have been zero coming in - Delphi2 requires this!
      Exit;
   end;
   {$ELSE}
   OutStream.Size:=0;
   {$ENDIF}
   inStream.Seek(0, 0);
   Getmem(HashPtr, HASH_SIZE);
   Fillchar(hashPtr^, HASH_SIZE, #0);
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   bytes_read:=BUFF_LEN;
   while bytes_read=BUFF_LEN do begin
      if inStream.Size-inStream.Position>=BUFF_LEN then
         bytes_read:=BUFF_LEN
      else
         bytes_read:=inStream.Size-inStream.Position;
      inStream.Read(inputbuffer^, bytes_read);
      compress_len:=Compress(PByte(inputbuffer), bytes_read,
         PByte(outputbuffer), HashPtr);
      OutStream.Write(compress_len, 2);
      if compress_len<0 then compress_len:=0-compress_len;
      OutStream.Write(outputbuffer^, compress_len);
   end;
   compress_len:=0;
   OutStream.Write(compress_len, 2);
   Freemem(HashPtr, HASH_SIZE);
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.DecompressBorlandFileToFile(var infile,
   outfile:file);
var
   block_len:SmallInt;
   code,
      decomp_len:Integer;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   block_len:=1;
   while block_len<>0 do begin
      Blockread(infile, block_len, 2, code);
      if Code<>2 then begin
         if Assigned(fOnBlockLengthError) then
            fOnBlockLengthError(nil);
         Freemem(inputbuffer, BUFF_LEN);
         Freemem(outputbuffer, BUFF_LEN);
         Exit;
      end;
      if block_len<>0 then begin
         if (block_len<0) then begin
            decomp_len:=0-block_len;
            Blockread(infile, outputbuffer^, decomp_len, code);
            if Code<>decomp_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
         end
         else begin
            Blockread(infile, inputbuffer^, block_len, code);
            if Code<>block_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
            decomp_len:=Decompress(PByte(inputbuffer), block_len,
               PByte(outputbuffer));
         end;
         Blockwrite(outfile, outputbuffer^, decomp_len, code);
         if Code<>decomp_len then begin
            if Assigned(fOnDataUnderrun) then
               fOnDataUnderrun(nil);
            Freemem(inputbuffer, BUFF_LEN);
            Freemem(outputbuffer, BUFF_LEN);
            Exit;
         end;
      end;
   end;
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.DecompressWindowsFileToFile(var infile,
   outfile:Integer);
var
   block_len:SmallInt;
   code:Integer;
   decomp_len:Integer;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   block_len:=1;
   while block_len<>0 do begin
      code:=FileRead(infile, block_len, 2);
      if Code<>2 then begin
         if Assigned(fOnBlockLengthError) then
            fOnBlockLengthError(nil);
         Freemem(inputbuffer, BUFF_LEN);
         Freemem(outputbuffer, BUFF_LEN);
         Exit;
      end;
      if block_len<>0 then begin
         if (block_len<0) then begin
            decomp_len:=0-block_len;
            code:=FileRead(infile, outputbuffer^, decomp_len);
            if Code<>decomp_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
         end
         else begin
            code:=FileRead(infile, inputbuffer^, block_len);
            if Code<>block_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
            decomp_len:=Decompress(PByte(inputbuffer), block_len,
               PByte(outputbuffer));
         end;
         code:=FileWrite(outfile, outputbuffer^, decomp_len);
         if Code<>decomp_len then begin
            if Assigned(fOnDataUnderrun) then
               fOnDataUnderrun(nil);
            Freemem(inputbuffer, BUFF_LEN);
            Freemem(outputbuffer, BUFF_LEN);
            Exit;
         end;
      end;
   end;
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.DecompressString(const OriginalStr:string; var
   OutStr:string);
var
   block_len:SmallInt;
   code,
      decomp_len:Integer;
   outputbuffer,
      inputbuffer:PByteArray;
   InStr:string;

begin
   inStr:=OriginalStr;
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   OutStr:='';
   block_len:=1;
   while block_len<>0 do begin
      if Length(inStr)>=2 then begin
         Move(inStr[1], block_len, 2);
         Code:=2;
         Delete(inStr, 1, 2);
      end
      else begin
         Code:=0;
         block_len:=0;
      end;
      if Code<>2 then begin
         if Assigned(fOnBlockLengthError) then
            fOnBlockLengthError(nil);
         Freemem(inputbuffer, BUFF_LEN);
         Freemem(outputbuffer, BUFF_LEN);
         Exit;
      end;
      if block_len<>0 then begin
         if (block_len<0) then begin
            decomp_len:=0-block_len;
            if Length(inStr)>=decomp_len then begin
               OutStr:=OutStr+Copy(InStr, 1, decomp_len);
               Code:=decomp_len;
               Delete(inStr, 1, decomp_len);
            end
            else
               Code:=decomp_len-1;
            if Code<>decomp_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
         end
         else begin
            if Length(inStr)>=block_len then begin
               Move(inStr[1], inputbuffer^, block_len);
               Code:=block_len;
               Delete(inStr, 1, block_len);
            end
            else
               Code:=1-block_len;
            if Code<>block_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
            decomp_len:=Decompress(PByte(inputbuffer), block_len,
               PByte(outputbuffer));
            Code:=decomp_len;
            SetLength(Outstr, Length(OutStr)+decomp_len);
            Move(outputbuffer^, OutStr[(Length(OutStr)-decomp_len)+1],
               decomp_len);
         end;
         if Code<>decomp_len then begin
            if Assigned(fOnDataUnderrun) then
               fOnDataUnderrun(nil);
            Freemem(inputbuffer, BUFF_LEN);
            Freemem(outputbuffer, BUFF_LEN);
            Exit;
         end;
      end;
   end;
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

procedure TDXRDCompression.DecompressStreams(inStream, OutStream:TStream);
var
   block_len:SmallInt;
   code:Integer;
   decomp_len:Integer;
   inputbuffer,
      outputbuffer:PByteArray;

begin
   {$IFDEF VER90}
   if OutStream.Size>0 then begin
      // Size should have been zero coming in - Delphi2 requires this!
      Exit;
   end;
   {$ELSE}
   OutStream.Size:=0;
   {$ENDIF}
   inStream.Seek(0, 0);
   Getmem(inputbuffer, BUFF_LEN);
   Getmem(outputbuffer, BUFF_LEN);
   block_len:=1;
   while block_len<>0 do begin
      if inStream.Size-inStream.Position>=2 then begin
         inStream.Read(block_len, 2);
         Code:=2;
      end
      else
         Code:=0;
      if Code<>2 then begin
         if Assigned(fOnBlockLengthError) then
            fOnBlockLengthError(nil);
         Freemem(inputbuffer, BUFF_LEN);
         Freemem(outputbuffer, BUFF_LEN);
         Exit;
      end;
      if block_len<>0 then begin
         if (block_len<0) then begin
            decomp_len:=0-block_len;
            if inStream.Size-inStream.Position>=decomp_len then begin
               Code:=decomp_len;
               inStream.Read(outputbuffer^, decomp_len);
            end
            else
               Code:=decomp_len-1;
            if Code<>decomp_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
         end
         else begin
            decomp_len:=block_len;
            if inStream.Size-inStream.Position>=block_len then begin
               Code:=block_len;
               inStream.Read(inputbuffer^, decomp_len);
            end
            else
               Code:=block_len-1;
            if Code<>block_len then begin
               if Assigned(fOnDataUnderrun) then
                  fOnDataUnderrun(nil);
               Freemem(inputbuffer, BUFF_LEN);
               Freemem(outputbuffer, BUFF_LEN);
               Exit;
            end;
            decomp_len:=Decompress(PByte(inputbuffer), block_len,
               PByte(outputbuffer));
         end;
         OutStream.Write(outputbuffer^, decomp_len);
      end;
   end;
   Freemem(inputbuffer, BUFF_LEN);
   Freemem(outputbuffer, BUFF_LEN);
end;

end.

