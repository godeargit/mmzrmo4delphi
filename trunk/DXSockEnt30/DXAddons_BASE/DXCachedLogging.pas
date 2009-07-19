unit DXCachedLogging;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXCachedLogging
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
   DXBasicTimer,
   DXString,
   Classes;

{$I DXAddons.def}
{$WARNINGS OFF}
type
   TDX_WriteRedirect=procedure(WriteData:TStream) of object;
   // TDXCachedLogging is the ancestor to all of our logging
   // components. It incorporates two types of caching techniques.
   //
   //
   //     1.   Based upon the amount of data stored in memory,
   //          flushing at this threshhold reduces the amount of
   //          disk operations.
   //     2.   Based upon the age of the data stored in memory,
   //          flushing after a given period of time also reduces
   //          the amount of disk operations.
   // These two technoqies are used jointly, producing the ultimate
   // style of buffered logging. No matter how busy or inactive a
   // server is - this design improves the overall performance of
   // the server and hosting PC. While incorporating the ability to
   // manually flush the content, the ability to redirect the
   // \output to another resource (like a database), and the
   // ability to rename the log file for a new day.
   //
   //
   //
   // Summary
   // Ancestor Logging Component.
   TDXCachedLogging=class(TDXComponent)
   private
      // Private declarations
      fCacheArray:TList;
      FFilename:string;
      FInterval:Cardinal;
      FMaxSize:Integer;
      MyTimer:TDXBasicTimer;
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
      property FileName:string read FFileName
         write FFileName;
      property CacheInterval:Cardinal read FInterval
         write FInterval;
      property CacheMaxSize:Integer read FMaxSize
         write FMaxSize;
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
      Len:Integer;
   end;

constructor TDXCachedLogging.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   {$IFDEF OBJECTS_ONLY}
   MyTimer:=TDXBasicTimer.Create;
   {$ELSE}
   MyTimer:=TDXBasicTimer.Create(nil);
   {$ENDIF}
   fCacheArray:=TList.Create;
   FInterval:=6000;
   FMaxSize:=2048;
   fIsFlushing:=False;
   MyTimer.Enabled:=False;
   MyTimer.Interval:=FInterval;
   MyTimer.OnTimer:=FlushLog;
   fLastRun:=Trunc(Date);
   feWriteRedirect:=nil;
   fMemInUse:=0;
end;

destructor TDXCachedLogging.Destroy;
begin
   if Assigned(MyTimer) then begin
      MyTimer.Enabled:=False;
      MyTimer.Free;
      MyTimer:=nil;
   end;
   if Assigned(fCacheArray) then begin
      if fCacheArray.Count>0 then FlushLog(nil);
      fCacheArray.Free;
      fCacheArray:=nil;
   end;
   inherited Destroy;
end;

procedure TDXCachedLogging.Writeln(Value:string);
var
   ListString:PListString;
   VLen:Integer;

begin
   if (Trunc(Date)<>fLastRun) then begin
      FlushLog(nil);
      if Assigned(feOnDayRollover) then feOnDayRollover(Self);
      fLastRun:=Trunc(Date);
   end;
   New(ListString);
   {$IFDEF LINUX}
   Value:=Value+#13;
   {$ELSE}
   Value:=Value+#13#10;
   {$ENDIF}
   VLen:=Length(Value);
   ListString^.Str:=Value;
   ListString^.Len:=VLen;
{   SetLength(ListString^.Str, VLen);
   Move(Value[1], ListString^.Str[1], VLen);}
   Inc(fMemInUse, VLen);
   MyCriticalSection.StartingWrite;
   fCacheArray.Add(ListString);
   MyCriticalSection.FinishedWrite;
   with MyTimer do begin
      if FInterval<>Interval then Interval:=FInterval;
      if not Enabled then Enabled:=True;
   end;
   if fMemInUse>FMaxSize then FlushLog(nil);
end;

procedure TDXCachedLogging.FlushLog;
var
   TFH:Integer;
   MemoryStream:TMemoryStream;
   ListString:PListString;

begin
   fMemInUse:=0;
   MyCriticalSection.StartingWrite;
   if (FFileName='')or
      (not Assigned(fCacheArray))or
      fIsFlushing or
      (fCacheArray.Count<1) then begin
      if (fCacheArray.Count<1) then MyTimer.Enabled:=False;
      MyCriticalSection.FinishedWrite;
      Exit;
   end
   else
      fIsFlushing:=True;
   try
      if Assigned(feWriteRedirect) then begin
         MemoryStream:=TMemoryStream.Create;
         while fCacheArray.Count>0 do begin
            MemoryStream.Write(PListString(fCacheArray[0])^.Str[1],
               PListString(fCacheArray[0])^.Len);
            fCacheArray.Delete(0);
         end;
         MemoryStream.Position:=0;
         feWriteRedirect(MemoryStream);
         MemoryStream.Free;
      end
      else begin
         TFH:=FileOpen(FFileName, fmOpenWrite or fmShareDenyNone);
         if TFH<0 then TFH:=FileCreate(FFileName)
         else FileSeek(TFH, 0, 2);
         while fCacheArray.Count>0 do begin
            ListString:=PListString(fCacheArray[0]);
            FileWrite(TFH, ListString^.Str[1], ListString^.Len);
            Dispose(ListString);
            fCacheArray.Delete(0);
         end;
         FileClose(TFH);
      end;
   finally
      fIsFlushing:=False;
   end;
   MyCriticalSection.FinishedWrite;
end;

procedure TDXCachedLogging.FlushNow;
begin
   FlushLog(nil);
end;

function TDXCachedLogging.GetMemInUse:Cardinal;
begin
   Result:=fMemInUse;
end;

function TDXCachedLogging.GetEntries:Integer;
begin
   Result:=fCacheArray.Count;
end;

end.

