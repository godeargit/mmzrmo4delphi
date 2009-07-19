unit DXSSI;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSSI
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
// This is an Ancestor component, that does basic scripting based upon the
// public SSI standards addressed on the w3.org site. This is used to parse
// the passed html file, dynamically changing the content. If you wish to
// produce optimal output please send your .html stream through this parser
// then through the htmlcompressor. That will produce very optimal output. If
// you wish to support database connectivity, or other server to server type
// connections you will want to use TDXSSIPlus instead of TDXSSI. The plus
// descendant supports all the calls included in TDXSSI.
//
// UniqueID is passed as a parameter throughout the parser engine, this is used
// by commands that report the current session variables, like remote IP address
// or authentication user name or current physical path. This gives the freedom
// to the parser to leave session variables up to you the developer, and leaves
// the design of the SSI engine generic enough to work as a script engine for
// other protocols (override the SSIStart and SSIEnd, or make a descendant).
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

const
   SSIStart:string='<!--#';
   SSIEnd:string='-->';

type
   SSITNotifyEvent=procedure(const UniqueID:Cardinal) of object;
   SSITStringEvent=procedure(const UniqueID:Cardinal; var Answer:string) of
      object;
   SSITEchoEvent=procedure(const UniqueID:Cardinal; Parameters:string; var
      Answer:string) of object;
   SSITExecEvent=procedure(const UniqueID:Cardinal; Parameters:string;
      Output:TStream) of object;
   SSITIncludeEvent=procedure(const UniqueID:Cardinal; Parameters:string;
      Output:TStream) of object;
   SSITChangeSettingEvent=procedure(const UniqueID:Cardinal; Parameters:string)
      of object;
   SSITOtherEvent=procedure(const UniqueID:Cardinal; FullString:string; var
      Answer:string; var Handled:Boolean) of object;

   TDXSSI=class(TDXComponent)
   private
      { Private declarations }
      fDateFormat:string;
      fSizeFormat:string;
      fOnEmbeddedEnabledEXEC:SSITChangeSettingEvent;
      fOnEmbeddedDisableEXEC:SSITChangeSettingEvent;
      fOnEmbeddedPageCounter:SSITStringEvent;
      fOnEmbeddedSiteCounter:SSITStringEvent;
      fOnEmbeddedSiteBytesServed:SSITStringEvent;
      fOnEmbeddedServerBytesServed:SSITStringEvent;
      fOnEmbeddedFileLastModified:SSITStringEvent;
      fOnEmbeddedFileSize:SSITStringEvent;
      fOnCommandECHO:SSITEchoEvent;
      fOnCommandEXEC:SSITExecEvent;
      fOnCommandINCLUDE:SSITIncludeEvent;
      fOnCommandCONFIG:SSITChangeSettingEvent;
      fOnCommandOther:SSITOtherEvent;
      fOnNewLine:SSITNotifyEvent;
      fOnLineDone:SSITNotifyEvent;
      fEventArray:TList;
      fintLoop:Integer;
      fsCurrentLine:string;
   protected
      { Protected declarations }
      function GetCurrentLine:Integer;
      procedure SetCurrentLine(value:integer);
      function PreviewCurrentLine:string;
      procedure SetOnEmbeddedEnableEXEC(value:SSITChangeSettingEvent);
      procedure SetOnEmbeddedDisableEXEC(value:SSITChangeSettingEvent);
      procedure SetOnEmbeddedPageCounter(value:SSITStringEvent);
      procedure SetOnEmbeddedSiteCounter(value:SSITStringEvent);
      procedure SetOnEmbeddedSiteBytesServed(value:SSITStringEvent);
      procedure SetOnEmbeddedServerBytesServed(value:SSITStringEvent);
      procedure SetOnEmbeddedFileLastModified(value:SSITStringEvent);
      procedure SetOnEmbeddedFileSize(value:SSITStringEvent);
      procedure SetOnCommandECHO(value:SSITEchoEvent);
      procedure SetOnCommandEXEC(value:SSITExecEvent);
      procedure SetOnCommandINCLUDE(value:SSITIncludeEvent);
      procedure SetOnCommandCONFIG(value:SSITChangeSettingEvent);
      procedure SetTimeFormat(value:string);
      procedure SetSizeFormat(value:string);
   public
      { Public declarations }
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
      {$ENDIF}
      destructor Destroy; override;
      function ProcessStream(const UniqueID:Cardinal; Stream:TStream):Boolean;
         virtual;
      function ProcessLine(const UniqueID:Cardinal; const S:string):string;
         virtual;
      procedure GetSSIToken(SSI:string; WhichOne:Integer; var Token,
         Value:string);
      procedure AddStringEvent(Command:string; EventProc:SSITStringEvent);
      procedure AddChangeSettingEvent(Command:string;
         EventProc:SSITChangeSettingEvent);
      procedure AddEchoEvent(Command:string; EventProc:SSITEchoEvent);
      procedure AddExecEvent(Command:string; EventProc:SSITExecEvent);
      procedure AddIncludeEvent(Command:string; EventProc:SSITIncludeEvent);
      function FormattedSize(Size:Integer):string;
      function FormattedTime(DateTime:TDateTime):string;
   published
      { Published declarations }
      property OnEmbeddedEnableEXEC:SSITChangeSettingEvent read
         fOnEmbeddedEnabledEXEC
         write SetOnEmbeddedEnableEXEC;
      property OnEmbeddedDisableEXEC:SSITChangeSettingEvent read
         fOnEmbeddedDisableEXEC
         write SetOnEmbeddedDisableEXEC;
      property OnEmbeddedPageCounter:SSITStringEvent read fOnEmbeddedPageCounter
         write SetOnEmbeddedPageCounter;
      property OnEmbeddedSiteCounter:SSITStringEvent read fOnEmbeddedSiteCounter
         write SetOnEmbeddedSiteCounter;
      property OnEmbeddedSiteBytesServed:SSITStringEvent read
         fOnEmbeddedSiteBytesServed
         write SetOnEmbeddedSiteBytesServed;
      property OnEmbeddedServerBytesServed:SSITStringEvent read
         fOnEmbeddedServerBytesServed
         write SetOnEmbeddedServerBytesServed;
      property OnEmbeddedFileLastModified:SSITStringEvent read
         fOnEmbeddedFileLastModified
         write SetOnEmbeddedFileLastModified;
      property OnEmbeddedFileSize:SSITStringEvent read fOnEmbeddedFileSize
         write SetOnEmbeddedFileSize;
      property OnCommandECHO:SSITEchoEvent read fOnCommandECHO
         write SetOnCommandECHO;
      property OnCommandEXEC:SSITExecEvent read fOnCommandEXEC
         write SetOnCommandEXEC;
      property OnCommandINCLUDE:SSITIncludeEvent read fOnCommandINCLUDE
         write SetOnCommandINCLUDE;
      property OnCommandCONFIG:SSITChangeSettingEvent read fOnCommandCONFIG
         write SetOnCommandCONFIG;
      property OnCommandOther:SSITOtherEvent read fOnCommandOther
         write fOnCommandOther;
      property CurrentLine:Integer read GetCurrentLine
         write SetCurrentLine;
      property ParserStartNewLine:SSITNotifyEvent read fOnNewLine
         write fOnNewLine;
      property ParserLineIsParsed:SSITNotifyEvent read fOnLineDone
         write fOnLineDone;
      property DateTimeFormat:string read fDateFormat
         write SetTimeFormat;
      property SizeFormat:string read fSizeFormat
         write SetSizeFormat;
   end;

implementation

uses
   SysUtils;

type
   PSSIStringEvent=^TSSIStringEvent;
   TSSIStringEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:SSITStringEvent;
   end;
   PSSIEchoEvent=^TSSIEchoEvent;
   TSSIEchoEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:SSITEchoEvent;
   end;
   PSSIExecEvent=^TSSIExecEvent;
   TSSIExecEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:SSITExecEvent;
   end;
   PSSIincludeEvent=^TSSIincludeEvent;
   TSSIincludeEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:SSITincludeEvent;
   end;
   PSSIChangeSettingEvent=^TSSIChangeSettingEvent;
   TSSIChangeSettingEvent=record
      Tag:Integer;
      Command:string;
      EventProcedure:SSITChangeSettingEvent;
   end;

constructor TDXSSI.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fEventArray:=TList.Create;
   fDateFormat:='%d %b %Y';
   fSizeFormat:='bytes';
end;

destructor TDXSSI.Destroy;
var
   PStringEvent:PSSIStringEvent;
   PEchoEvent:PSSIEchoEvent;
   PExecEvent:PSSIExecEvent;
   PIncludeEvent:PSSIIncludeEvent;
   PChangeSettingEvent:PSSIChangeSettingEvent;

begin
   if Assigned(fEventArray) then begin
      while fEventArray.Count>0 do begin
         case PSSIStringEvent(fEventArray[0]).Tag of
            1:begin
                  PStringEvent:=fEventArray[0];
                  Dispose(PStringEvent);
               end;
            2:begin
                  PEchoEvent:=fEventArray[0];
                  Dispose(PEchoEvent);
               end;
            3:begin
                  PExecEvent:=fEventArray[0];
                  Dispose(PExecEvent);
               end;
            4:begin
                  PIncludeEvent:=fEventArray[0];
                  Dispose(PIncludeEvent);
               end;
            5:begin
                  PChangeSettingEvent:=fEventArray[0];
                  Dispose(PChangeSettingEvent);
               end;
         end;
         fEventArray.Delete(0);
      end;
      fEventArray.Free;
      fEventArray:=nil;
   end;
   inherited Destroy;
end;

(******************************************************************************
ADDSTRINGEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddStringEvent('CDROM',MySpecialEvent);
******************************************************************************)

procedure TDXSSI.AddStringEvent(Command:string; EventProc:SSITStringEvent);
var
   PStringEvent:PSSIStringEvent;
   Loop:Integer;

begin
   Command:=Uppercase(Command);
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PSSIStringEvent(fEventArray[Loop]).Command=Command then begin
         PSSIStringEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc(Loop);
   end;
   New(PStringEvent);
   PStringEvent.Tag:=1;
   PStringEvent.Command:=Command;
   PStringEvent.EventProcedure:=EventProc;
   fEventArray.Add(PStringEvent);
end;

(******************************************************************************
ADDCHANGESETTINGEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddChangeSettingEvent('CDROM',MySpecialEvent);
******************************************************************************)

procedure TDXSSI.AddChangeSettingEvent(Command:string;
   EventProc:SSITChangeSettingEvent);
var
   PChangeSettingEvent:PSSIChangeSettingEvent;
   Loop:Integer;

begin
   Command:=Uppercase(Command);
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PSSIChangeSettingEvent(fEventArray[Loop]).Command=Command then begin
         PSSIChangeSettingEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc(Loop);
   end;
   New(PChangeSettingEvent);
   PChangeSettingEvent.Tag:=5;
   PChangeSettingEvent.Command:=Command;
   PChangeSettingEvent.EventProcedure:=EventProc;
   fEventArray.Add(PChangeSettingEvent);
end;

(******************************************************************************
ADDECHOEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddEchoEvent('CDROM',MySpecialEvent);
******************************************************************************)

procedure TDXSSI.AddEchoEvent(Command:string; EventProc:SSITEchoEvent);
var
   PEchoEvent:PSSIEchoEvent;
   Loop:Integer;

begin
   Command:=Uppercase(Command);
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PSSIEchoEvent(fEventArray[Loop]).Command=Command then begin
         PSSIEchoEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc(Loop);
   end;
   New(PEchoEvent);
   PEchoEvent.Tag:=2;
   PEchoEvent.Command:=Command;
   PEchoEvent.EventProcedure:=EventProc;
   fEventArray.Add(PEchoEvent);
end;

(******************************************************************************
ADDEXECEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddExecEvent('CDROM',MySpecialEvent);
******************************************************************************)

procedure TDXSSI.AddExecEvent(Command:string; EventProc:SSITExecEvent);
var
   PExecEvent:PSSIExecEvent;
   Loop:Integer;

begin
   Command:=Uppercase(Command);
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PSSIExecEvent(fEventArray[Loop]).Command=Command then begin
         PSSIExecEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc(Loop);
   end;
   New(PExecEvent);
   PExecEvent.Tag:=3;
   PExecEvent.Command:=Command;
   PExecEvent.EventProcedure:=EventProc;
   fEventArray.Add(PExecEvent);
end;

(******************************************************************************
ADDINCLUDEEVENT:
              Allows you to dynamically assign a new command to the internal
              parser. This allows the servercore to support the 'pre-defined'
              OnCommand* events, plus you can add other commands dynamically
              at run-time in your application without requiring a source code
              modification to our components!

              To make support easier for us, we ask that you use the Add*Event
              procedures to expand our code, reducing code changes when an
              upgrade is released!

              See documentation for complete information on how this works.

              Example Usage: AddIncludeEvent('CDROM',MySpecialEvent);
******************************************************************************)

procedure TDXSSI.AddIncludeEvent(Command:string; EventProc:SSITIncludeEvent);
var
   PIncludeEvent:PSSIIncludeEvent;
   Loop:Integer;

begin
   Command:=Uppercase(Command);
   Loop:=0;
   while Loop<fEventArray.Count do begin
      if PSSIIncludeEvent(fEventArray[Loop]).Command=Command then begin
         PSSIIncludeEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      end
      else
         Inc(Loop);
   end;
   New(PIncludeEvent);
   PIncludeEvent.Tag:=4;
   PIncludeEvent.Command:=Command;
   PIncludeEvent.EventProcedure:=EventProc;
   fEventArray.Add(PIncludeEvent);
end;

procedure TDXSSI.SetOnEmbeddedEnableEXEC(value:SSITChangeSettingEvent);
begin
   fOnEmbeddedEnabledEXEC:=Value;
   AddChangeSettingEvent('ALLOWEXECUTE', Value);
end;

procedure TDXSSI.SetOnEmbeddedDisableEXEC(value:SSITChangeSettingEvent);
begin
   fOnEmbeddedDisableEXEC:=Value;
   AddChangeSettingEvent('DISALLOWEXECUTE', Value);
end;

procedure TDXSSI.SetOnEmbeddedPageCounter(value:SSITStringEvent);
begin
   fOnEmbeddedPageCounter:=Value;
   AddStringEvent('HITCOUNT', Value);
   AddStringEvent('PAGECOUNT', Value);
end;

procedure TDXSSI.SetOnEmbeddedSiteCounter(value:SSITStringEvent);
begin
   fOnEmbeddedSiteCounter:=Value;
   AddStringEvent('SITECOUNT', Value);
end;

procedure TDXSSI.SetOnEmbeddedSiteBytesServed(value:SSITStringEvent);
begin
   fOnEmbeddedSiteBytesServed:=Value;
   AddStringEvent('BYTES_SERVED', Value);
end;

procedure TDXSSI.SetOnEmbeddedServerBytesServed(value:SSITStringEvent);
begin
   fOnEmbeddedServerBytesServed:=Value;
   AddStringEvent('TOTAL_BYTES_SERVED', Value);
end;

procedure TDXSSI.SetOnEmbeddedFileLastModified(value:SSITStringEvent);
begin
   fOnEmbeddedFileLastModified:=Value;
   AddStringEvent('FLASTMOD', Value);
end;

procedure TDXSSI.SetOnEmbeddedFileSize(value:SSITStringEvent);
begin
   fOnEmbeddedFileSize:=Value;
   AddStringEvent('FSIZE', Value);
end;

procedure TDXSSI.SetOnCommandECHO(value:SSITEchoEvent);
begin
   fOnCommandEcho:=Value;
   AddEchoEvent('ECHO', Value);
end;

procedure TDXSSI.SetOnCommandEXEC(value:SSITExecEvent);
begin
   fOnCommandExec:=Value;
   AddExecEvent('EXEC', Value);
end;

procedure TDXSSI.SetOnCommandINCLUDE(value:SSITIncludeEvent);
begin
   fOnCommandInclude:=Value;
   AddIncludeEvent('INCLUDE', Value);
end;

procedure TDXSSI.SetOnCommandCONFIG(value:SSITChangeSettingEvent);
begin
   fOnCommandConfig:=Value;
   AddChangeSettingEvent('CONFIG', Value);
end;

function TDXSSI.GetCurrentLine:Integer;
begin
   Result:=fIntLoop;
end;

procedure TDXSSI.SetCurrentLine(value:integer);
begin
   fIntLoop:=Value;
end;

function TDXSSI.PreviewCurrentLine:string;
begin
   Result:=fsCurrentLine;
end;

function TDXSSI.ProcessStream(const UniqueID:Cardinal; Stream:TStream):Boolean;
var
   Strlist:TStringList;

begin
   Result:=False;
   if not Assigned(Stream) then Exit;
   Stream.Position:=0;
   StrList:=TStringList.Create;
   try
      StrList.LoadFromStream(Stream);
      Stream.Size:=0;
      fIntLoop:=0;
      while fIntLoop<StrList.Count do begin
         fsCurrentLine:=StrList[fIntLoop];
         if Assigned(fOnNewLine) then fOnNewLine(UniqueID);
         if QuickPos(SSIStart, StrList[fIntLoop])>0 then
            StrList[fIntLoop]:=ProcessLine(UniqueID, StrList[fIntLoop]);
         fsCurrentLine:=StrList[fIntLoop];
         if Assigned(fOnLineDone) then fOnLineDone(UniqueID);
         Inc(fIntLoop);
      end;
      Stream.Position:=0;
      StrList.SaveToStream(Stream);
   finally
      StrList.Free;
   end;
   Stream.Position:=0;
   Result:=True;
end;

function TDXSSI.ProcessLine(const UniqueID:Cardinal; const S:string):string;
var
   BeforeSSI, AfterSSI:string;
   SSICommand:string;
   SSILine:string;

   {$HINTS OFF}

   function ServerSideInclude(sCmd, ExtraStuff:string):string;
   var
      Loop:Integer;
      WasHandled:Boolean;
      Answer:string;
      MemoryStream:TStream;
      MemoryString:TStringList;
      SubLoop:Integer;

   begin
      Result:='';
      Loop:=0;
      WasHandled:=False;
      while (Loop<fEventArray.Count)and(not WasHandled) do begin
         if PSSIStringEvent(fEventArray[Loop]).Command=sCMD then begin
            Answer:='';
            case PSSIStringEvent(fEventArray[Loop]).Tag of
               1:
                  if Assigned(PSSIStringEvent(fEventArray[Loop]).EventProcedure)
                     then
                     SSITStringEvent(PSSIStringEvent(fEventArray[Loop]).EventProcedure)(UniqueID, Answer);
               2:
                  if Assigned(PSSIEchoEvent(fEventArray[Loop]).EventProcedure)
                     then
                     SSITEchoEvent(PSSIEchoEvent(fEventArray[Loop]).EventProcedure)(UniqueID, ExtraStuff, Answer);
               3:begin
                     MemoryStream:=TMemoryStream.Create;
                     MemoryString:=TStringList.Create;
                     if Assigned(PSSIExecEvent(fEventArray[Loop]).EventProcedure)
                        then
                        SSITExecEvent(PSSIExecEvent(fEventArray[Loop]).EventProcedure)(UniqueID, ExtraStuff, MemoryStream);
                     MemoryString.LoadFromStream(MemoryStream);
                     MemoryStream.Free;
                     MemoryStream:=nil;
                     SubLoop:=0;
                     while SubLoop<MemoryString.Count do begin
                        Answer:=Answer+MemoryString[SubLoop]+#13#10;
                        Inc(SubLoop);
                     end;
                     MemoryString.Free;
                     MemoryString:=nil;
                  end;
               4:begin
                     MemoryStream:=TMemoryStream.Create;
                     MemoryString:=TStringList.Create;
                     if
                        Assigned(PSSIIncludeEvent(fEventArray[Loop]).EventProcedure) then
                        SSITIncludeEvent(PSSIIncludeEvent(fEventArray[Loop]).
                           EventProcedure)(UniqueID, ExtraStuff, MemoryStream);
                     MemoryString.LoadFromStream(MemoryStream);
                     MemoryStream.Free;
                     MemoryStream:=nil;
                     while MemoryString.Count>0 do begin
                        Answer:=Answer+MemoryString[0]+#13#10;
                        MemoryString.Delete(0);
                     end;
                     MemoryString.Free;
                     MemoryString:=nil;
                  end;
               5:
                  if
                     Assigned(PSSIChangeSettingEvent(fEventArray[Loop]).EventProcedure) then
                     SSITChangeSettingEvent(PSSIChangeSettingEvent(fEventArray[Loop]).EventProcedure)(UniqueID, ExtraStuff);
            end;
            WasHandled:=True;
         end
         else
            Inc(Loop);
      end;
      if not WasHandled then begin
         Answer:='';
         if assigned(OnCommandOther) then
            OnCommandOther(UniqueID, sCmd, Answer, WasHandled);
      end;
      Result:=Answer;
   end;
   {$HINTS ON}

   function SSICodes(Str:string):Integer;
   begin
      Result:=0;
      while QuickPos(SSIStart, Str)>0 do begin
         Inc(Result);
         Delete(Str, 1, QuickPos(SSIStart, Str)+Length(SSIStart));
      end;
   end;

begin
   SSILine:=S;
   while SSICodes(SSILine)>0 do begin
      BEFORESSI:=Copy(SSILine, 1, QuickPos(SSIStart, SSILine)-1);
      Delete(SSILine, 1, Length(BEFORESSI)+Length(SSIStart));
      AFTERSSI:=Copy(SSILine, QuickPos(SSIEnd, SSILine)+Length(SSIEnd),
         Length(SSILine));
      Delete(SSILine, QuickPos(SSIEnd, SSILine), Length(SSILine));
      while QuickPos(SSIStart, SSILine)>0 do begin
         BEFORESSI:=BEFORESSI+SSIStart+Copy(SSILine, 1, QuickPos(SSIStart,
            SSILine)-1);
         Delete(SSILine, 1, Length(BEFORESSI)+Length(SSIStart));
      end;
      SSICommand:=Fetch(SSILine, #32, False);
      SSILine:=BEFORESSI+ServerSideInclude(Uppercase(SSICommand),
         SSILine)+AFTERSSI;
   end;
   Result:=SSILine;
end;

procedure TDXSSI.GetSSIToken(SSI:string; WhichOne:Integer; var Token,
   Value:string);
var
   Loop:Integer;

begin
   Token:='';
   Value:='';
   Delete(SSI, 1, QuickPos(#32, SSI));
   while Copy(SSI, 1, 1)=#32 do
      Delete(SSI, 1, 1);
   SSI:=Copy(SSI, 1, QuickPos(SSIEnd, SSI)-1);
   Loop:=0;
   while Loop<WhichOne do begin
      Token:=Uppercase(Copy(SSI, 1, QuickPos('=', SSI)-1));
      if Token='' then Token:=SSI;
      Delete(SSI, 1, Length(Token)+1);
      Trim(SSI);
      Trim(Token);
      Value:=SSI;
      if Copy(Value, 1, 1)='"' then begin
         Delete(Value, 1, 1);
         Delete(Value, QuickPos('"', Value), Length(Value));
         Delete(SSI, 1, 1);
         Delete(SSI, 1, QuickPos('"', SSI));
      end
      else begin
         Value:=Fetch(SSI, #32, False);
      end;
      Inc(Loop);
   end;
end;

procedure TDXSSI.SetTimeFormat(value:string);
begin
   fDateFormat:=Value;
end;

procedure TDXSSI.SetSizeFormat(value:string);
begin
   if Uppercase(Copy(Value, 1, 1))='B' then
      fSizeFormat:='bytes'
   else
      fSizeFormat:='abbrev';
end;

function TDXSSI.FormattedSize(Size:Integer):string;
begin
   if fSizeFormat='bytes' then
      Result:=IntToCommaStr(Size)
   else begin
      if Size>1024 then begin
         Size:=Size div 1024;
         if Size>=1000 then begin
            Size:=Size div 1000;
            if Size>=1000 then begin
               Size:=Size div 1000;
               if Size>=1000 then begin
                  Result:=IntToCommaStr(Size)+'tb';
               end
               else
                  Result:=IntToStr(Size)+'gb';
            end
            else
               Result:=IntToStr(Size)+'mb';
         end
         else
            Result:=IntToStr(Size)+'kb';
      end
      else
         Result:=IntToCommaStr(Size);
   end;
end;

function TDXSSI.FormattedTime(DateTime:TDateTime):string;
begin
   Result:=DateTimeToStr(DateTime);
end;

end.

