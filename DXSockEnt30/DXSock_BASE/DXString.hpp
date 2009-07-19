// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXString.pas' rev: 6.00

#ifndef DXStringHPP
#define DXStringHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <SyncObjs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxstring
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDXCritical;
class PASCALIMPLEMENTATION TDXCritical : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Syncobjs::TCriticalSection* Synchronizer;
	
public:
	__fastcall TDXCritical(void);
	__fastcall virtual ~TDXCritical(void);
	void __fastcall StartingRead(void);
	void __fastcall FinishedRead(void);
	void __fastcall StartingWrite(void);
	void __fastcall FinishedWrite(void);
};


class DELPHICLASS TDXComponent;
class PASCALIMPLEMENTATION TDXComponent : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	void __fastcall SetReleaseDate(AnsiString value);
	AnsiString __fastcall GetReleaseDate();
	
public:
	TDXCritical* MyCriticalSection;
	__fastcall virtual TDXComponent(Classes::TComponent* AOwner);
	__fastcall virtual ~TDXComponent(void);
	
__published:
	__property AnsiString ReleaseDate = {read=GetReleaseDate, write=SetReleaseDate};
};


typedef int *PInteger;

typedef unsigned DWORD;

typedef Set<char, 0, 255>  CharSet;

//-- var, const, procedure ---------------------------------------------------
#define BPDX_RELEASE_DATE "2003-04-15"
extern PACKAGE void __fastcall ProcessWindowsMessageQueue(void);
extern PACKAGE Byte __fastcall HiByteOfWord(const Word W);
extern PACKAGE Word __fastcall MakeBytesToWord(const Byte A, const Byte B);
extern PACKAGE bool __fastcall WindowsWriteFile(unsigned hFile, const void *Buffer, unsigned nNumberOfBytesToWrite, unsigned &lpNumberOfBytesWritten);
extern PACKAGE void __fastcall ShowMessageWindow(const AnsiString Caption, const AnsiString Message);
extern PACKAGE void __fastcall DoSleepEX(const unsigned Interval);
extern PACKAGE AnsiString __fastcall RegistryStringGet(const HKEY Key, AnsiString SubKey);
extern PACKAGE bool __fastcall RegistryStringSet(const HKEY Key, AnsiString SubKey, AnsiString Value);
extern PACKAGE AnsiString __fastcall GetRegistryString(const HKEY Key, const AnsiString SubKey);
extern PACKAGE bool __fastcall SetRegistryString(const HKEY Key, AnsiString SubKey, AnsiString Value);
extern PACKAGE bool __fastcall LaunchWebBrowser(const AnsiString URL, const int WindowState);
extern "C" HRESULT __stdcall CoCreateGuid(GUID &guid);
extern PACKAGE AnsiString __fastcall MakeUUID();
extern PACKAGE AnsiString __fastcall RawUUID();
extern PACKAGE AnsiString __fastcall MakeUUIDPacked();
extern PACKAGE bool __fastcall IsCharAlphaNumeric(const char c);
extern PACKAGE bool __fastcall IsCharAlpha(const char c);
extern PACKAGE bool __fastcall IsNumeric(const char c);
extern PACKAGE bool __fastcall isNumericString(const AnsiString S);
extern PACKAGE int __fastcall Min(const int I1, const int I2);
extern PACKAGE int __fastcall Max(const int I1, const int I2);
extern PACKAGE int __fastcall StringToInteger(const AnsiString S);
extern PACKAGE void __fastcall SwapMove(Word Source, void *Dest);
extern PACKAGE AnsiString __fastcall IntToCommaStr(const int Number);
extern PACKAGE AnsiString __fastcall BinaryToString(const Byte Number);
extern PACKAGE Byte __fastcall StringToBinary(AnsiString S);
extern PACKAGE int __fastcall QuickPos(const AnsiString aFindString, const AnsiString aSourceString);
extern PACKAGE int __fastcall CharPos(const char C, const AnsiString aSource);
extern PACKAGE AnsiString __fastcall Fetch(AnsiString &S, const AnsiString Sub, const bool IgnoreCase);
extern PACKAGE AnsiString __fastcall FetchByChar(AnsiString &S, const char Sub, const bool IgnoreCase);
extern PACKAGE AnsiString __fastcall Uppercase(const AnsiString S);
extern PACKAGE AnsiString __fastcall Lowercase(const AnsiString S);
extern PACKAGE AnsiString __fastcall ProperCase(const AnsiString S);
extern PACKAGE AnsiString __fastcall Trim(const AnsiString S);
extern PACKAGE AnsiString __fastcall NoCRLF(const AnsiString S);
extern PACKAGE AnsiString __fastcall NoAngleBrackets(const AnsiString S);
extern PACKAGE int __fastcall InStrArray(const AnsiString SearchStr, const AnsiString * KnownCommands, const int KnownCommands_Size);
extern PACKAGE void __fastcall InverseString(AnsiString &S, int Count);
extern PACKAGE AnsiString __fastcall HexDump(const AnsiString S);
extern PACKAGE AnsiString __fastcall ReplaceChar(const AnsiString Source, const char OldChar, const char NewChar);
extern PACKAGE AnsiString __fastcall ExtractLeft(const AnsiString aSourceString, const int Size);
extern PACKAGE AnsiString __fastcall ExtractRight(const AnsiString aSourceString, const int Size);
extern PACKAGE AnsiString __fastcall ExtractWordAt(const AnsiString Text, int Position);
extern PACKAGE AnsiString __fastcall LeftJustify(const AnsiString S, const int MaxLength);
extern PACKAGE AnsiString __fastcall RightJustify(const AnsiString S, const int MaxLength);
extern PACKAGE char __fastcall CleanChar(const char InChar);
extern PACKAGE AnsiString __fastcall CleanStr(const AnsiString InStr);
extern PACKAGE int __fastcall PosLastChar(const char Ch, const AnsiString S);
extern PACKAGE AnsiString __fastcall AsciiToOem(const AnsiString ax);
extern PACKAGE AnsiString __fastcall OemToAscii(const AnsiString ax);
extern PACKAGE int __fastcall WordCount(const AnsiString S);
extern PACKAGE int __fastcall CRC32ByChar(const char Ch, const int starting_crc);
extern PACKAGE int __fastcall CRC32ByString(const AnsiString S, const int starting_crc);
extern PACKAGE Word __fastcall CRC16ByChar(const char Ch, const Word starting_crc);
extern PACKAGE Word __fastcall CRC16ByString(const AnsiString S, const Word starting_crc);
extern PACKAGE Word __fastcall CRCARCByChar(const char Ch, const Word starting_crc);
extern PACKAGE Word __fastcall CRCARCByString(const AnsiString S, const Word starting_crc);
extern PACKAGE void __fastcall SetLongBit(int &L, const Byte Bit, const bool Setting);
extern PACKAGE bool __fastcall GetLongBit(const int L, const Byte Bit);
extern PACKAGE void __fastcall SetWordBit(Word &L, const Byte Bit, const bool Setting);
extern PACKAGE bool __fastcall GetWordBit(const Word L, const Byte Bit);
extern PACKAGE void __fastcall SetByteBit(Byte &L, const Byte Bit, const bool Setting);
extern PACKAGE bool __fastcall GetByteBit(const Byte L, const Byte Bit);
extern PACKAGE AnsiString __fastcall Replicate(const AnsiString Source, int NumberOfTimes);
extern PACKAGE bool __fastcall IsWildCard(const AnsiString Source);
extern PACKAGE int __fastcall GetIndex(const char c);
extern PACKAGE AnsiString __fastcall Base64ToString(const AnsiString S);
extern PACKAGE AnsiString __fastcall StringToBase64(const AnsiString S1);
extern PACKAGE AnsiString __fastcall FixDottedIP(const AnsiString S);
extern PACKAGE AnsiString __fastcall IPStringFormated(AnsiString S);
extern PACKAGE AnsiString __fastcall IPAddressFormatted(const int I1, const int I2, const int I3, const int I4);
extern PACKAGE AnsiString __fastcall EscapeDecode(const AnsiString S);
extern PACKAGE AnsiString __fastcall EscapeEncode(const AnsiString S);
extern PACKAGE AnsiString __fastcall EncodeDomain(AnsiString S);
extern PACKAGE AnsiString __fastcall EncodeAddress(AnsiString S);
extern PACKAGE AnsiString __fastcall DecodeDomain(AnsiString S);
extern PACKAGE AnsiString __fastcall GetActualEmailAddress(AnsiString Parm, AnsiString Command);
extern PACKAGE int __fastcall DayOfTheYear(const System::TDateTime DT);
extern PACKAGE int __fastcall DaysLeftThisYear(const System::TDateTime DT);
extern PACKAGE int __fastcall DaysThisMonth(const System::TDateTime DT);
extern PACKAGE int __fastcall DaysLeftThisMonth(const System::TDateTime DT);
extern PACKAGE bool __fastcall IsTimeAM(const System::TDateTime DT);
extern PACKAGE bool __fastcall IsTimePM(const System::TDateTime DT);
extern PACKAGE bool __fastcall IsTimeNoon(const System::TDateTime DT);
extern PACKAGE bool __fastcall IsTimeMidnight(const System::TDateTime DT);
extern PACKAGE System::TDateTime __fastcall DateTimeToGMT(const System::TDateTime DT);
extern PACKAGE System::TDateTime __fastcall DateTimeToLocal(const System::TDateTime DT);
extern PACKAGE bool __fastcall IsLeapYear(const Word Year);
extern PACKAGE int __fastcall LocalTimeZoneBias(void);
extern PACKAGE AnsiString __fastcall TimeZone();
extern PACKAGE AnsiString __fastcall ShortTimeZone();
extern PACKAGE AnsiString __fastcall TimeZoneBias();
extern PACKAGE AnsiString __fastcall ToUnixSlashes(const AnsiString S);
extern PACKAGE AnsiString __fastcall ToDOSSlashes(const AnsiString S);
extern PACKAGE AnsiString __fastcall ToOSSlashes(const AnsiString S);
extern PACKAGE AnsiString __fastcall ChangeDir(const AnsiString S, const AnsiString RP);
extern PACKAGE AnsiString __fastcall DateTimeToGMTRFC822(const System::TDateTime DT);
extern PACKAGE AnsiString __fastcall DateTimeToGMTRFC850(const System::TDateTime DT);
extern PACKAGE AnsiString __fastcall DateTimeToRFC850(const System::TDateTime DT);
extern PACKAGE AnsiString __fastcall DateTimeToRFC850Bias(const System::TDateTime DT);
extern PACKAGE System::TDateTime __fastcall RFCToDateTime(AnsiString S);
extern PACKAGE void __fastcall Unpacktime(const int P, System::TDateTime &DT);
extern PACKAGE void __fastcall Packtime(System::TDateTime &DT, int &P);
extern PACKAGE int __fastcall GetDosDate(void);
extern PACKAGE Word __fastcall GetDOW(void);
extern PACKAGE bool __fastcall TimeOut(const System::Comp MyTime);
extern PACKAGE System::Comp __fastcall TimeCounter(void);
extern PACKAGE AnsiString __fastcall AddBackSlash(const AnsiString S);
extern PACKAGE AnsiString __fastcall NoBackSlash(const AnsiString S);
extern PACKAGE int __fastcall PCharLen(char * Str);
extern PACKAGE Word __fastcall LRot16(Word X, int c);
extern PACKAGE Word __fastcall RRot16(Word X, int c);
extern PACKAGE unsigned __fastcall LRot32(unsigned X, int c);
extern PACKAGE unsigned __fastcall RRot32(unsigned X, int c);
extern PACKAGE unsigned __fastcall SwapDWord(unsigned X);
extern PACKAGE AnsiString __fastcall Center(AnsiString S, int MaxWidth);
extern PACKAGE AnsiString __fastcall LeftJustifyCh(const AnsiString S, const char Ch, const int MaxLength);
extern PACKAGE AnsiString __fastcall RightJustifyCh(const AnsiString S, const char Ch, const int MaxLength);
extern PACKAGE AnsiString __fastcall EncodeTabs(AnsiString S, Byte TabSize);
extern PACKAGE AnsiString __fastcall DecodeTabs(AnsiString S, Byte TabSize);
extern PACKAGE AnsiString __fastcall Filter(AnsiString S, const CharSet &CS);
extern PACKAGE AnsiString __fastcall SoundEx(AnsiString S);
extern PACKAGE bool __fastcall WildCompare(AnsiString LookingFor, AnsiString SourceStr);
extern PACKAGE AnsiString __fastcall SizeStamp(int CPS);
extern PACKAGE void __fastcall DivMod(int Dividend, Word Divisor, Word &Result, Word &Remainder);

}	/* namespace Dxstring */
using namespace Dxstring;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXString
