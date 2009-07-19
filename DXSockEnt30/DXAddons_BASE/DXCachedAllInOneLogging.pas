unit DXCachedAllInOneLogging;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXCachedAllInOneLogging
//    Author: G.E. Ozz Nixon Jr. (usasupport@brainpatchworkdx.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999 Brain Patchwork DX.
//   Version: 2.0 (2nd Generation Code)
//     Build: June 1st 2000
// ==========================================================================
// This is a descendant to TDXCachedLogging that has been setup for writing
// AllInOne log files.
///////////////////////////////////////////////////////////////////////////////

uses
   DXCachedLogging,
   Classes;

{$I DXSOCK.DEF}// 2.0

{$WARNINGS OFF}
type
   TDXLogFormats=(lfGeneric, lfFlexible, lfDEBUG, lfAPACHE, lfCERN, lfNCSA,
      lfW3SVC, lfWEBSITE,
      lfEMWAC, lfNETSCAPE, lfPURVEYOR, lfSPRY, lfREFERRER, lfBROWSER, lfIMS,
      lfRealAudio, lfSocketError, lsProxyError);
   TDXCachedAllInOneLogging=class(TDXCachedLogging)
   private
      // Private declarations
      fsDateFormat:string;
      fInternalDF:string;
      fLogFormat:TDXLogFormats;
      FPreTime:Boolean;
      fuseHEX:Boolean;
      fLineBreak:string;
   protected
      // Protected declarations
      procedure SetDateFormat(value:string);
   public
      // Public declarations
      {$IFDEF OBJECTS_ONLY}
      constructor Create; override;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure Writeln(
         ClientAddress,
         RemoteIdent,
         AuthUser,
         ReceivedRequest,
         Cookie_ID,
         User_Agent,
         LocalIPAddress,
         Referer,
         Host,
         URI,
         RequestedDomain,
         HTTPResult,
         ServiceName,
         Method,
         QueryString,
         PlayerAgent,
         StatusLength,
         MessageID,
         RemoteIP,
         HELO,
         MailFROM,
         RCPTTo:string;
         StatusCode,
         ByteTransfered,
         TimeTaken,
         SocketError:Integer);
   published
      // Published declarations
      property WhichFormat:TDXLogFormats read fLogFormat
         write fLogFormat;
      property PrefixTimeStamp:Boolean read FPreTime
         write FPretime;
      property SaveInHEX:Boolean read fuseHEX
         write fuseHEX;
      property ReleaseDate:string read GetReleaseDate
         write SetReleaseDate;
      property TimeStampFormat:string read fsDateFormat
         write SetDateFormat;
   end;
   {$WARNINGS ON}

implementation

uses
   DXString,
   SysUtils;

{$IFDEF OBJECTS_ONLY}

constructor TDXCachedAllInOneLogging.Create;
{$ELSE}

constructor TDXCachedAllInOneLogging.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
   fLogFormat:=lfNCSA;
   TimeStampFormat:='dd/mmm/yyyy:hh:nn:ss @GMT@';
   FPreTime:=True;
   fuseHEX:=False;
   fLinebreak:=StringOfChar('-', 78);
end;

destructor TDXCachedAllInOneLogging.Destroy;
begin
   inherited Destroy;
end;

procedure TDXCachedAllInOneLogging.SetDateFormat(value:string);
begin
   fsDateFormat:=Value;
   fInternalDF:=StringReplace(Value, '@GMT@', TimeZoneBias, [rfReplaceAll]);
end;

procedure TDXCachedAllInOneLogging.Writeln(ClientAddress, RemoteIdent, AuthUser,
   ReceivedRequest, Cookie_ID, User_Agent, LocalIPAddress, Referer, Host,
   URI, RequestedDomain, HTTPResult, ServiceName, Method, QueryString,
   PlayerAgent, StatusLength, MessageID, RemoteIP, HELO, MailFROM,
      RCPTTo:string;
   StatusCode, ByteTransfered, TimeTaken, SocketError:Integer);
var
   Value:string;
   Ws:string;

   function CleanAscii(S:string):string;
   var
      Loop:Integer;

   begin
      Result:=S;
      for Loop:=1 to Length(Result) do
         if (Ord(Result[Loop])<32)or(Ord(Result[Loop])>127) then
            Result[Loop]:='.';
   end;

begin
   if AuthUser='' then AuthUser:='-';
   if RemoteIdent='' then RemoteIdent:='-';
   case fLogFormat of
      lfApache:begin
            Value:=FormatDateTime(fInternalDF, Now);
            Value:=ClientAddress+#32+RemoteIdent+#32+AuthUser+' ['+Value+'] "'+
               ReceivedRequest+'" '+IntToStr(StatusCode)+#32+IntToStr(ByteTransfered);
            inherited Writeln(Value);
         end;
      lfBrowser:begin
            Value:='['+FormatDateTime(fInternalDF, Now)+'] ';
            Value:=Value+User_Agent;
            inherited Writeln(Value);
         end;
      lfCERN:begin
            Value:=FormatDateTime(fInternalDF, Now);
            Value:=ClientAddress+#32+RemoteIdent+#32+AuthUser+' ['+Value+'] "'+
               ReceivedRequest+'" '+IntToStr(StatusCode)+#32+IntToStr(ByteTransfered);
            inherited Writeln(Value);
         end;
      lfDebug:begin
            while Value<>'' do begin
               Ws:=Copy(Value, 1, 16);
               Delete(Value, 1, 16);
               inherited Writeln(HexDump(Ws)+
                  StringOfChar(#32, (16-length(Ws))+(16-length(Ws))*2)+
                  #32#32#32+CleanAscii(Ws));
            end;
            inherited Writeln(fLineBreak);
         end;
   end;
end;

end.

