unit DXTCCompression;
interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXTCCompression
//       Author: Toupao Chieng
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
// Turbo Compressor ver 0.1 by Toupao Chieng Dec 31, 1990, 3:27pm
///////////////////////////////////////////////////////////////////////////////

uses
   Classes;

{$I DXSock.DEF}

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
      procedure CompressString(var inStr, OutStr:string);
      procedure DecompressString(var inStr, OutStr:string);
      procedure CompressWindowsFileToFile(var infile, outfile:Integer);
      procedure DecompressWindowsFileToFile(var infile, outfile:Integer);
      procedure CompressBorlandFileToFile(var infile, outfile:file);
      procedure DecompressBorlandFileToFile(var infile, outfile:file);
   published
      property ReleaseDate:string read GetReleaseDate write SetReleaseDate;
      property OnBlockLengthError:TNotifyEvent read fOnBlockLengthError
         write fOnBlockLengthError;
      property OnDataUnderrun:TNotifyEvent read fOnDataUnderrun
         write fOnDataUnderrun;
   end;

implementation

uses
   SysUtils;

const
   NumOfChars=256;
   NumOfSyms=NumOfChars+1;
   MaxFreq=16383;
   Adaptive:Boolean=True;
   CodeValueBits=16;
   EOFSymbol=NumOfChars+1;
   BufSize=$A000;
   HdrLen:Integer=32;
   FreqTable:array[0..NumOfSyms+1] of Word=
      (0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 124, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1236, 1, 21, 9, 3, 1, 25, 15, 2, 2,
      2, 1, 79, 19, 60, 1, 15, 15, 8, 5, 4, 7, 5, 4, 4, 6, 3, 2, 1, 1, 1,
      1, 1, 24, 15, 22, 12, 15, 10, 9, 16, 16, 8, 6, 12, 23, 13, 11, 14, 1,
      14, 28, 29, 6, 3, 11, 1, 3, 1, 1, 1, 1, 1, 3, 1, 491, 85, 173, 232,
      744, 127, 110, 293, 418, 6, 39, 250, 139, 429, 446, 111, 5, 388, 375,
      531, 152, 57, 97, 12, 101, 5, 2, 1, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1);

type
   BufPtr=^Buffer;
   Buffer=array[1..BufSize] of Byte;
   CodeValue=Longint;

var
   CharToIndex:array[0..NumOfChars] of Integer;
   IndexToChar:array[0..NumOfSyms+1] of Integer;
   CumFreq:array[0..NumOfSyms] of Integer;
   OrigFileSize, ByteCnt:Real;
   IFile, OFile:file;
   EOFile, Decompression:Boolean;
   InBufPtr, OutBufPtr:BufPtr;
   Symbol, InBufCnt, InBufPos, OutBufPos:Word;
   BitBuffer, BitsToGo:Byte;
   Low, High:Codevalue;
   BitsToFollow, FileIndex, Int, FileCount, HdrCnt:Integer;
   Ch, GarbageBits, Bite:Byte;
   Mode:Char;
   OutFileName, Header:string;
   Value:CodeValue;

procedure StartModel;
var
   I:Integer;
begin
   for I:=0 to NumOfChars-1 do begin
      CharToIndex[I]:=I+1;
      IndexToChar[I+1]:=I;
   end;
   if not Adaptive then begin
      CumFreq[NumOfSyms]:=0;
      for I:=NumOfSyms downto 1 do
         CumFreq[I-1]:=CumFreq[I]+FreqTable[I];
      if CumFreq[0]>MaxFreq then begin
         Writeln(^G' Cumulative frequency count too high.');
            Halt;
      end;
   end
   else begin
      for I:=0 to NumOfSyms do begin
         FreqTable[I]:=1;
         CumFreq[I]:=NumOfSyms-I;
      end;
      FreqTable[0]:=0;
   end;
end;

procedure UpdateModel(Symbol:Integer);
var
   I:Integer;
   C1, C2:Integer;
begin
   if not Adaptive then begin
   end
   else begin
      if CumFreq[0]=MaxFreq then begin
         C1:=0;
         for I:=NumOfSyms downto 0 do begin
            FreqTable[I]:=(FreqTable[I]+1)shr 1;
            CumFreq[I]:=C1;
            C1:=C1+FreqTable[I];
         end;
      end;
      I:=Symbol;
      while FreqTable[I]=FreqTable[I-1] do
         Dec(I);
      if I<symbol then begin
         C1:=IndexToChar[I];
         C2:=IndexToChar[Symbol];
         IndexToChar[I]:=C2;
         IndexToChar[Symbol]:=C1;
         CharToIndex[C1]:=Symbol;
         CharToIndex[C2]:=I;
      end;
      Inc(FreqTable[I]);
      while I>0 do begin
         Dec(I);
         Inc(CumFreq[I]);
      end;
   end;
end;

procedure StoreByte(B:Byte);
begin
   OutBufPtr^[OutBufPos]:=B;
   Inc(OutBufPos);
   if OutBufPos>BufSize then WriteOutBuf;
end;

function GetByte:Byte;
begin
   if not EOFile then begin
      GetByte:=InBufPtr^[InBufPos];
      if InBufPos=InBufCnt then
         FillInputBuf
      else
         Inc(InBufPos);
   end;
end;

procedure StartOutputingBits;
begin
   BitBuffer:=0;
   BitsToGo:=8;
   ByteCnt:=0;
end;

procedure OutputBit(B:Byte);
begin
   BitBuffer:=BitBuffer shr 1;
   if B=0 then
      BitBuffer:=BitBuffer and $7F
   else
      BitBuffer:=BitBuffer or $80;
   Dec(BitsToGo);
   if BitsToGo=0 then begin
      StoreByte(BitBuffer);
      BitsToGo:=8;
      ByteCnt:=ByteCnt+1;
   end;
end;

procedure StartEncoding;
begin
   Low:=0;
   High:=TopValue;
   BitsToFollow:=0;
   OrigFileSize:=0;
end;

function InputBit:Word;
var
   T:Word;
begin
   if BitsToGo=0 then begin
      BitBuffer:=GetByte;
      if EOFile then begin
         Inc(GarbageBits);
         if GarbageBits>CodeValueBits-2 then begin
            Writeln(^G' Bad input file.');
               Halt;
         end;
      end;
      BitsToGo:=8;
   end;
   T:=BitBuffer and $01;
   BitBuffer:=BitBuffer shr 1;
   Dec(BitsToGo);
   InputBit:=T;
end;

procedure StartDecoding;
var
   I:Byte;
begin
   I:=GetByte;
   Mode:=Chr(I);
   if UpCase(Mode)='A' then
      Adaptive:=True
   else
      Adaptive:=False;
   Value:=0;
   for I:=0 to CodeValueBits-1 do begin
      Value:=2*Value+InputBit;
   end;
   Low:=0;
   High:=TopValue;
end;

procedure BitPlusFollow(B:Byte);
begin
   OutputBit(B);
   while BitsToFollow>0 do begin
      if B=1 then
         OutPutBit(0)
      else
         OutputBit(1);
      Dec(BitsToFollow);
   end;
end;

procedure EncodeSymbol(Sym:Word);
var
   Range:Longint;
begin
   Range:=Longint((High-Low)+1);
   High:=Low+(Range*CumFreq[Sym-1])div CumFreq[0]-1;
   Low:=Low+(Range*CumFreq[Sym])div CumFreq[0];
   repeat
      if High<Half then begin
         BitPlusFollow(0);
      end
      else if Low>=Half then begin
         BitPlusFollow(1);
         Low:=Low-Half;
         High:=High-Half;
      end
      else if (Low>=FirstQrtr)and(High<ThirdQrtr) then begin
         Inc(BitsToFollow);
         Low:=Low-FirstQrtr;
         High:=High-FirstQrtr;
      end
      else
         Exit;
      Low:=2*Low;
      High:=2*High+1;
   until 0<>0;
end;

procedure DoneEncoding;
begin
   Inc(BitsToFollow);
   if (Low<FIrstQrtr) then
      BitPlusFollow(0)
   else
      BitPlusFollow(1);
end;

procedure DoneOutputingBits;
begin
   BitBuffer:=BitBuffer shr BitsToGo;
   StoreByte(BitBuffer);
   ByteCnt:=ByteCnt+1;
end;

procedure Compress(F:string);
const
   HdrLen=32;
   Blanks='                        ';
var
   OName:string;
   FSize:string;
   Header:string;
   I:Byte;
begin
   Assign(IFile, F);
   Reset(IFile, 1);
   if Pos('.', F)>0 then
      OName:=Copy(F, 1, Pos('.', F))+'TCC'
   else
      OName:=F+'.TCC';
   Assign(OFile, OName);
   Rewrite(OFile, 1);
   FillInputBuf;
   OutBufPos:=1;
   StoreByte(Ord('A'));
   Write('Compressing: ', F);
   StartOutPutingBits;
   StartEncoding;
   Str(FileSize(IFile), FSize);
   Header:=F+'|'+FSize;
   Header:=Header+Copy(Blanks, 1, HdrLen-Length(Header));
   for I:=1 to Length(Header) do begin
      Symbol:=CharToIndex[Ord(Header[I])];
      EncodeSymbol(Symbol);
      UpdateModel(Symbol);
   end;
   repeat
      Bite:=GetByte;
      OrigFileSize:=OrigFileSize+1;
      if not EOFile then begin
         Symbol:=CharToIndex[Bite];
         EncodeSymbol(Symbol);
         UpdateModel(Symbol);
      end;
   until EOFile;
   EncodeSymbol(EOFSymbol);
   Inc(BitsToFollow);
   if (Low<FIrstQrtr) then
      BitPlusFollow(0)
   else
      BitPlusFollow(1);
   BitBuffer:=BitBuffer shr BitsToGo;
   StoreByte(BitBuffer);
   ByteCnt:=ByteCnt+1;
   WriteOutBuf;
   Close(IFile);
   Close(OFile);
   Writeln(' (', ((ByteCnt/OrigFileSize)*100):4:2, '%) done.');
end;

function DecodeSymbol:Word;
var
   Range:Longint;
   Cum:Word;
   Sym:Word;
   Done:Boolean;
begin
   Range:=Longint((High-Low)+1);
   Cum:=(((Value-Low)+1)*CumFreq[0]-1)div Range;
   Sym:=1;
   Done:=False;
   while CumFreq[Sym]>Cum do
      Inc(Sym);
   High:=Low+(Range*CumFreq[Sym-1])div CumFreq[0]-1;
   Low:=Low+(Range*CumFreq[Sym])div CumFreq[0];
   repeat
      if High<Half then
      else if (Low>=Half) then begin
         Value:=Value-Half;
         Low:=Low-Half;
         High:=High-Half;
      end
      else if (Low>=FirstQrtr)and(High<ThirdQrtr) then begin
         Value:=Value-FirstQrtr;
         Low:=Low-FirstQrtr;
         High:=High-FirstQrtr;
      end
      else
         Done:=True;
      if not Done then begin
         Low:=2*Low;
         High:=2*High+1;
         Value:=2*Value+InputBit;
      end;
   until Done;
   DecodeSymbol:=Sym;
end;

procedure Decompress(F:string);
begin
   Assign(IFile, F);
   Reset(IFile, 1);
   FillInputBuf;
   HdrCnt:=1;
   BitsToGo:=0;
   GarbageBits:=0;
   ByteCnt:=0;
   StartDecoding;
   repeat
      Symbol:=DecodeSymbol;
      if Symbol<>EOFSymbol then begin
         Ch:=IndexToChar[Symbol];
         if HdrCnt<HdrLen then begin
            Header[HdrCnt]:=Chr(Ch);
            Inc(HdrCnt);
         end
         else if HdrCnt=HdrLen then begin
            Header[0]:=Chr(HdrLen);
            OutFileName:=Copy(Header, 1, Pos('|', Header)-1);
            Assign(OFile, OutFileName);
            Rewrite(OFile, 1);
            Writeln('Decompressing: ', OutFileName);
            Inc(HdrCnt);
         end
         else
            StoreByte(Ch);
         UpdateModel(Symbol);
      end;
   until EOFile;
   WriteOutBuf;
   Close(OFile);
   Close(IFile);
end;

{$IFDEF OBJECTS_ONLY}

constructor TDXTCCompression.Create;
{$ELSE}

constructor TDXTCCompression.Create(AOwner:TComponent); override;
{$ENDIF}
begin
end;

destructor TDXTCCompression.Destroy; override;
begin
end;

procedure Compress(inPtr, OutPtr:Pointer);
const
   HdrLen=32;
   Blanks='                        ';

var
   TopValue, FirstQrtr, Half, ThirdQrtr:Longint;

   function Initialize:Boolean;
   var
      I:Integer;

   begin
      Result:=True;
      for I:=0 to NumOfChars-1 do begin
         CharToIndex[I]:=I+1;
         IndexToChar[I+1]:=I;
      end;
      if not Adaptive then begin
         CumFreq[NumOfSyms]:=0;
         for I:=NumOfSyms downto 1 do
            CumFreq[I-1]:=CumFreq[I]+FreqTable[I];
         if CumFreq[0]>MaxFreq then begin
            Result:=False;
            Exit;
         end;
      end
      else begin
         for I:=0 to NumOfSyms do begin
            FreqTable[I]:=1;
            CumFreq[I]:=NumOfSyms-I;
         end;
         FreqTable[0]:=0;
      end;
   end;

begin
   TopValue:=$FFFE;
   FirstQrtr:=(TopValue div 4)+1;
   Half:=2*FirstQrtr;
   ThirdQrtr:=3*FirstQrtr;
   Adaptive:=True;
   New(InBufPtr);
   New(OutBufPtr);
   EOFile:=False;
   if not Initialize then begin
      Adapative:=False;
      Initialize;
   end;
   OutBufPos:=1;
   StoreByte(Ord('A'));
   StartOutPutingBits;
   StartEncoding;
   Str(FileSize(IFile), FSize);
   Header:=F+'|'+FSize;
   Header:=Header+Copy(Blanks, 1, HdrLen-Length(Header));
   for I:=1 to Length(Header) do begin
      Symbol:=CharToIndex[Ord(Header[I])];
      EncodeSymbol(Symbol);
      UpdateModel(Symbol);
   end;
   repeat
      Bite:=GetByte;
      OrigFileSize:=OrigFileSize+1;
      if not EOFile then begin
         Symbol:=CharToIndex[Bite];
         EncodeSymbol(Symbol);
         UpdateModel(Symbol);
      end;
   until EOFile;
   EncodeSymbol(EOFSymbol);
   Inc(BitsToFollow);
   if (Low<FIrstQrtr) then
      BitPlusFollow(0)
   else
      BitPlusFollow(1);
   BitBuffer:=BitBuffer shr BitsToGo;
   StoreByte(BitBuffer);
   ByteCnt:=ByteCnt+1;
end;

procedure TDXTCCompression.CompressStreams(inStream, OutStream:TStream);
begin
end;

procedure TDXTCCompression.DecompressStreams(inStream, OutStream:TStream);
begin
end;

procedure TDXTCCompression.CompressString(var inStr, OutStr:string);
begin
end;

procedure TDXTCCompression.DecompressString(var inStr, OutStr:string);
begin
end;

procedure TDXTCCompression.CompressWindowsFileToFile(var infile,
   outfile:Integer);
begin
end;

procedure TDXTCCompression.DecompressWindowsFileToFile(var infile,
   outfile:Integer);
begin
end;

procedure TDXTCCompression.CompressBorlandFileToFile(var infile, outfile:file);
begin
end;

procedure TDXTCCompression.DecompressBorlandFileToFile(var infile,
   outfile:file);
begin
end;

begin
   if not Decompression then begin
      LoadFiles;
      FP:=FileListHead;
      repeat
         SetCompressor;
         Compress(FP^.Name);
         FP:=FP^.Next;
      until FP=nil;
   end
   else begin
      LoadFiles;
      FP:=FileListHead;
      repeat
         SetDecompressor;
         Decompress(FP^.Name);
         FP:=FP^.Next;
      until FP=nil;
   end;
end.

