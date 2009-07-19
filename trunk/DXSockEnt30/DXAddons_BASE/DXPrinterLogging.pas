unit DXPrinterLogging;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXPrinterLogging
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
// Pages are cached, the flushed.
///////////////////////////////////////////////////////////////////////////////

uses
   DXBasicTimer,
   DXString,
   Classes;

{$I DXAddons.def}

type
   TDX_WriteRedirect=procedure(WriteData:TStream) of object;
   TDXPrinterLogging=class(TDXComponent)
   private
      // Private declarations
      fCacheArray:TList;
      FPrinterName:string;
      fLinesPerPage:Integer;
      fCharactersPerLine:Integer;
      fLastRun:TDateTime;
      feOnDayRollover:TNotifyEvent;
      feWriteRedirect:TDX_WriteRedirect;
      fIsFlushing:Boolean;
      fMemInUse:Cardinal;
      procedure FlushLog(Sender:TObject);
   protected
      // Protected declarations
      function GetMemInUse:Cardinal;
      function GetEntries:Integer;
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure Writeln(Value:string); virtual;
      procedure FlushNow;
   published
      // Published declarations
      property Printer:string read FPrinterName
         write FPrinterName;
      property LinesPerPage:Integer read fLinesPerPage
         write fLinesPerPage;
      property CharactersPerLine:Integer read fCharactersPerLine
         write fCharactersPerLine;
      property OnDayRollover:TNotifyEvent read feOnDayRollover
         write feOnDayRollover;
      property OnWriteRedirect:TDX_WriteRedirect read feWriteRedirect
         write feWriteRedirect;
      property MemInUse:Cardinal read GetMemInUse;
      property Entries:Integer read GetEntries;
   end;

implementation

uses
   SysUtils;

type
   PListString=^TListString;
   TListString=record
      Str:string;
   end;

constructor TDXPrinterLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fCacheArray:=TList.Create;
   fIsFlushing:=False;
   fLastRun:=Trunc(Date);
   feWriteRedirect:=nil;
   fLinesPerPage:=58;
   fCharactersPerLine:=78;
end;

destructor TDXPrinterLogging.Destroy;
begin
   if Assigned(fCacheArray) then begin
      if fCacheArray.Count>0 then FlushLog(nil);
      fCacheArray.Free;
      fCacheArray:=nil;
   end;
   inherited Destroy;
end;

procedure TDXPrinterLogging.Writeln(Value:string);
var
   ListString:PListString;

begin
   if (Trunc(Date)<>fLastRun)and(Assigned(feOnDayRollover)) then begin
      FlushLog(nil);
      feOnDayRollover(Self);
      fLastRun:=Trunc(Date);
   end;
   New(ListString);
   ListString.Str:=Value;
   fCacheArray.Add(ListString);
   Inc(fMemInUse, Length(Value));
   if fCacheArray.Count>=fLinesPerPage then FlushLog(nil);
end;

///////////////////////////////////////////////////////////////////////////////
// FLUSHLOG:
//           Used internally to do the actual disk I/O, based upon the Timeout
//           and the Maximum Size. This produces great performance, especially
//           on very busy applications.
///////////////////////////////////////////////////////////////////////////////

procedure TDXPrinterLogging.FlushLog;
var
   TFH:Integer;
   MemoryStream:TMemoryStream;
   TmpStr:string;

begin
   if fIsFlushing or(fCacheArray.Count<1) then
      Exit
   else
      fIsFlushing:=True;
   fMemInUse:=0;
   try
      if Assigned(feWriteRedirect) then begin
         MemoryStream:=TMemoryStream.Create;
         MemoryStream:=TMemoryStream.Create;
         while fCacheArray.Count>0 do begin
            MemoryStream.Write(PListString(fCacheArray[0])^.Str[1],
               Length(PListString(fCacheArray[0])^.Str));
            fCacheArray.Delete(0);
         end;
         MemoryStream.Position:=0;
         feWriteRedirect(MemoryStream);
         MemoryStream.Free;
      end
      else begin
         TFH:=FileOpen(FPrinterName, fmOpenWrite or fmShareDenyNone);
         if TFH<0 then
            TFH:=FileCreate(FPrinterName)
         else
            FileSeek(TFH, 0, 2);
         while fCacheArray.Count>0 do begin
            FileWrite(TFH, PListString(fCacheArray[0])^.Str[1],
               Length(PListString(fCacheArray[0])^.Str));
            fCacheArray.Delete(0);
         end;
         FileClose(TFH);
      end;
   except
   end;
   fIsFlushing:=False;
end;

procedure TDXPrinterLogging.FlushNow;
begin
   FlushLog(nil);
end;

function TDXPrinterLogging.GetMemInUse:Cardinal;
begin
   Result:=fMemInUse;
end;

function TDXPrinterLogging.GetEntries:Integer;
begin
   Result:=fCacheArray.Count;
end;

end.

