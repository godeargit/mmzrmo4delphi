unit DXGeneralMsgObject;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXGeneralMsgObject
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
// This is the ancestor to all of our message components. The message can be
// loaded from a file or a stream. The concept of this component is when a
// inbound message is received from a socket it comes line by line, you should
// "collect" that in either a stringstream or something like that. Past the
// stream to this component, and then use this component to drop it to disk.
// This component only works with HEADERS, so if you send it a header/body
// collection form the internet as a Stream it tries internally to drop the
// body. We do this so you can develop a body object that does virus or content
// filtering.
//
// This piece uses our modified version of PosInStrArray found in the Winshoes
// library.
///////////////////////////////////////////////////////////////////////////////

uses
   Classes;

{$I DXAddons.def}

type
   PDXMsgObjectRecord=^TDXMsgObjectRecord;
   TDXMsgObjectRecord=record
      esTO:string;
      esFROM:string;
      esRETURNPATH:string;
      esPATH:string;
      esRECEIVED:string;
      esDATE:string;
      esSENDER:string;
      esCC:string;
      esBCC:string;
      esREPLYTO:string;
      esMESSAGEID:string;
      esINREPLYTO:string;
      esREFERENCES:string;
      esKEYWORDS:string;
      esENCRYPTED:string;
      esRESENTREPLYTO:string;
      esRESENTFROM:string;
      esRESENTSENDER:string;
      esRESENTDATE:string;
      esRESENTTO:string;
      esRESENTCC:string;
      esRESENTBCC:string;
      esRESENTMESSAGEID:string;
      esMIMEVERSION:string;
      esCONTENTTYPE:string;
      esCONTENTTRANSFERENCODING:string;
      esLINES:string;
      esNEWSGROUPS:string;
      esDISTRIBUTION:string;
      esORGANIZATION:string;
      esRELAYVERSION:string;
      esNNTPPOSTINGHOST:string;
      esEXPIRES:string;
      esFOLLOWUPTO:string;
      esCONTROL:string;
      esAPPROVED:string;
      esSUBJECT:TStringList;
      esCOMMENTS:TStringList;
      esUnknown:TStringList;
   end;

type
   {$IFDEF OBJECTS_ONLY}
   TDXGeneralMsgObject=class
      {$ELSE}
   TDXGeneralMsgObject=class(TComponent)
      {$ENDIF}
   private
      // Private declarations
   protected
      // Protected declarations
      DXMsgObject_Record:PDXMsgObjectRecord;
      procedure HeaderInitialize;
      procedure HeaderDestroy;
   public
      // Public declarations
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure LoadFromStream(Stream:TStream);
      procedure LoadFromFile(const Filename:string);
      procedure SaveToStream(Stream:TStream);
      procedure SaveToFile(const Filename:string);
      procedure DeleteFile(const Filename:string);
      procedure HeaderProcess(header:tstringlist);
      procedure HeaderClear;
      procedure SaveHeaderToList(var header:tstringlist);
      function ActualHeader:PDXMsgObjectRecord;
   published
      // Published declarations
   end;

implementation

uses
   DXString,
   SysUtils;

const
   // DO NOT CHANGE THE ORDER, OR YOU WILL BREAK THE CODE!
   KnownCommands:array[1..39] of string=
      (
      'TO', 'FROM', 'RETURN-PATH', 'RECEIVED', 'DATE', 'SENDER', 'CC', 'BCC',
      'REPLY-TO', 'MESSAGE-ID', 'IN-REPLY-TO', 'REFERENCES', 'KEYWORDS',
      'ENCRYPTED', 'SUBJECT', 'COMMENTS', 'RESENT-REPLY-TO', 'RESENT-FROM',
      'RESENT-SENDER', 'RESENT-DATE', 'RESENT-TO', 'RESENT-CC', 'RESENT-BCC',
      'RESENT-MESSAGE-ID', 'SENT', 'MIME-VERSION', 'CONTENT-TYPE',
      'CONTENT-TRANSFER-ENCODING', 'PATH', 'LINES', 'NEWSGROUPS',
      'DISTRIBUTION', 'ORGANIZATION', 'RELAY-VERSION', 'NNTP-POSTING-HOST',
      'EXPIRES', 'FOLLOWUP-TO', 'CONTROL', 'APPROVED'
      );

   {$IFDEF OBJECTS_ONLY}

constructor TDXGeneralMsgObject.Create;
{$ELSE}

constructor TDXGeneralMsgObject.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
   DXMsgObject_Record:=nil;
   HeaderInitialize;
end;

destructor TDXGeneralMsgObject.Destroy;
begin
   HeaderDestroy;
   inherited Destroy;
end;

procedure TDXGeneralMsgObject.HeaderInitialize;
begin
   HeaderDestroy;
   {$IFNDEF OBJECTS_ONLY}
   if not(csDesigning in ComponentState) then begin
      {$ENDIF}
      New(DXMsgObject_Record);
      FillChar(DXMsgObject_Record^, sizeof(DxMSGObject_Record^), 0);
      DXMsgObject_Record^.esSubject:=TStringList.Create;
      DXMsgObject_Record^.esComments:=TStringList.Create;
      DXMsgObject_Record^.esUnknown:=TStringList.Create;
      {$IFNDEF OBJECTS_ONLY}
   end;
   {$ENDIF}
end;

procedure TDXGeneralMsgObject.HeaderDestroy;

   procedure ClearStringList(sl:TStringList);
   var
      cnt:integer;
   begin
      if assigned(sl) then begin
         sl.BeginUpdate;
         try
            cnt:=sl.Count;
            while cnt>0 do begin
               sl.Delete(0);
               cnt:=sl.count;
            end;
         finally
            sl.Clear;
            sl.endupdate;
         end;
      end;
   end;

begin
   try
      if Assigned(DXMsgObject_Record) then begin
         if Assigned(DxMsgObject_Record^.esSUBJECT) then begin
            ClearStringList(DxMsgObject_Record^.esSUBJECT);
            DxMsgObject_Record^.esSubject.Free;
            DxMsgObject_Record^.esSubject:=nil;
         end;
         if Assigned(DxMsgObject_Record^.esComments) then begin
            ClearStringList(DxMsgObject_Record^.esComments);
            DxMsgObject_Record^.esComments.Free;
            DxMsgObject_Record^.esComments:=nil;
         end;
         if Assigned(DxMsgObject_Record^.esUnknown) then begin
            ClearStringList(DxMsgObject_Record^.esUnknown);
            DxMsgObject_Record^.esUnknown.Free;
            DxMsgObject_Record^.esUnknown:=nil;
         end;
         FillChar(DXMsgObject_Record^, sizeof(DxMSGObject_Record^), 0);
         Dispose(DxMsgObject_Record);
      end;
   finally
      DxMsgObject_Record:=nil;
   end;
end;

procedure TDXGeneralMsgObject.HeaderProcess(header:tstringlist);
var
   CurrentParseCode:string;
   UseCurrentParseCode:Boolean;
   WorkString:string;
   cnt:integer;
begin
   CurrentParseCode:='';
   header.BeginUpdate;
   try
      cnt:=header.count;
      while cnt>0 do begin
         WorkString:=Header[0]+#32;
         if WorkString=#32 then Exit;
         UseCurrentParseCode:=WorkString[1]in [#32, #9];
         WorkString:=Trim(WorkString);
         if not UseCurrentParseCode then begin
            CurrentParseCode:=Uppercase(Copy(WorkString, 1, Pos(':',
               WorkString)-1));
            Trim(CurrentParseCode);
            Delete(WorkString, 1, pos(':', WorkString));
            WorkString:=Trim(WorkString);
         end
         else
            WorkString:=#32+WorkString;
         case InStrArray(CurrentParseCode, KnownCommands)+1 of
            1:// 'TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esTO:=DXMsgObject_Record.esTO+WorkString
               else
                  DXMsgObject_Record.esTO:=WorkString;
            2:// 'FROM'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esFROM:=DXMsgObject_Record.esFROM+WorkString
               else
                  DXMsgObject_Record.esFROM:=WorkString;
            3:// 'RETURN-PATH'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRETURNPATH:=DXMsgObject_Record.esRETURNPATH+WorkString
               else
                  DXMsgObject_Record.esRETURNPATH:=WorkString;
            4:// 'RECEIVED'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRECEIVED:=DXMsgObject_Record.esRECEIVED+WorkString
               else
                  DXMsgObject_Record.esRECEIVED:=WorkString;
            5, 25:// 'DATE','SENT'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esDATE:=DXMsgObject_Record.esDATE+WorkString
               else
                  DXMsgObject_Record.esDATE:=WorkString;
            6:// 'SENDER'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esSENDER:=DXMsgObject_Record.esSENDER+WorkString
               else
                  DXMsgObject_Record.esSENDER:=WorkString;
            7:// 'CC'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esCC:=DXMsgObject_Record.esCC+WorkString
               else
                  DXMsgObject_Record.esCC:=WorkString;
            8:// 'BCC'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esBCC:=DXMsgObject_Record.esBCC+WorkString
               else
                  DXMsgObject_Record.esBCC:=WorkString;
            9:// 'REPLY-TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esREPLYTO:=DXMsgObject_Record.esREPLYTO+WorkString
               else
                  DXMsgObject_Record.esREPLYTO:=WorkString;
            10:// 'MESSAGE-ID'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esMESSAGEID:=DXMsgObject_Record.esMESSAGEID+WorkString
               else
                  DXMsgObject_Record.esMESSAGEID:=WorkString;
            11:// 'IN-REPLY-TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esINREPLYTO:=DXMsgObject_Record.esINREPLYTO+WorkString
               else
                  DXMsgObject_Record.esINREPLYTO:=WorkString;
            12:// 'REFERENCES'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esREFERENCES:=DXMsgObject_Record.esREFERENCES+WorkString
               else
                  DXMsgObject_Record.esREFERENCES:=WorkString;
            13:// 'KEYWORDS'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esKEYWORDS:=DXMsgObject_Record.esKEYWORDS+WorkString
               else
                  DXMsgObject_Record.esKEYWORDS:=WorkString;
            14:// 'ENCRYPTED'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esENCRYPTED:=DXMsgObject_Record.esENCRYPTED+WorkString
               else
                  DXMsgObject_Record.esENCRYPTED:=WorkString;
            15:// 'SUBJECT'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esSubject[DXMsgObject_Record.esSubject.Count-1]:=
                     DXMsgObject_Record.esSubject[DXMsgObject_Record.esSubject.Count-1]+WorkString
               else
                  DXMsgObject_Record.esSubject.Append(WorkString);
            16:// 'COMMENTS'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esComments[DXMsgObject_Record.esComments.Count-1]:=
                     DXMsgObject_Record.esComments[DXMsgObject_Record.esComments.Count-1]+WorkString
               else
                  DXMsgObject_Record.esComments.Append(WorkString);
            17:// 'RESENT-REPLY-TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTREPLYTO:=DXMsgObject_Record.esRESENTREPLYTO+WorkString
               else
                  DXMsgObject_Record.esRESENTREPLYTO:=WorkString;
            18:// 'RESENT-FROM'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTFROM:=DXMsgObject_Record.esRESENTFROM+WorkString
               else
                  DXMsgObject_Record.esRESENTFROM:=WorkString;
            19:// 'RESENT-SENDER'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTSENDER:=DXMsgObject_Record.esRESENTSENDER+WorkString
               else
                  DXMsgObject_Record.esRESENTSENDER:=WorkString;
            20:// 'RESENT-DATE'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTDATE:=DXMsgObject_Record.esRESENTDATE+WorkString
               else
                  DXMsgObject_Record.esRESENTDATE:=WorkString;
            21:// 'RESENT-TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTTO:=DXMsgObject_Record.esRESENTTO+WorkString
               else
                  DXMsgObject_Record.esRESENTTO:=WorkString;
            22:// 'RESENT-CC'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTCC:=DXMsgObject_Record.esRESENTCC+WorkString
               else
                  DXMsgObject_Record.esRESENTCC:=WorkString;
            23:// 'RESENT-BCC'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTBCC:=DXMsgObject_Record.esRESENTBCC+WorkString
               else
                  DXMsgObject_Record.esRESENTBCC:=WorkString;
            24:// 'RESENT-MESSAGE-ID'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRESENTMESSAGEID:=DXMsgObject_Record.esRESENTMESSAGEID+WorkString
               else
                  DXMsgObject_Record.esRESENTMESSAGEID:=WorkString;
            26:// 'MIME-VERSION'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esMIMEVERSION:=DXMsgObject_Record.esMIMEVERSION+WorkString
               else
                  DXMsgObject_Record.esMIMEVERSION:=WorkString;
            27:// 'CONTENT-TYPE'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esCONTENTTYPE:=DXMsgObject_Record.esCONTENTTYPE+WorkString
               else
                  DXMsgObject_Record.esCONTENTTYPE:=WorkString;
            28:// 'CONTENT-TRANSFER-ENCODING'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esCONTENTTRANSFERENCODING:=DXMsgObject_Record.esCONTENTTRANSFERENCODING+WorkString
               else
                  DXMsgObject_Record.esCONTENTTRANSFERENCODING:=WorkString;
            29:// 'PATH'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esPATH:=DXMsgObject_Record.esPATH+WorkString
               else
                  DXMsgObject_Record.esPATH:=WorkString;
            30:// 'LINES'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esLINES:=DXMsgObject_Record.esLINES+WorkString
               else
                  DXMsgObject_Record.esLINES:=WorkString;
            31:// 'NEWSGROUPS'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esNEWSGROUPS:=DXMsgObject_Record.esNEWSGROUPS+WorkString
               else
                  DXMsgObject_Record.esNEWSGROUPS:=WorkString;
            32:// 'DISTRIBUTION'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esDISTRIBUTION:=DXMsgObject_Record.esDISTRIBUTION+WorkString
               else
                  DXMsgObject_Record.esDISTRIBUTION:=WorkString;
            33:// 'ORGANIZATION'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esORGANIZATION:=DXMsgObject_Record.esORGANIZATION+WorkString
               else
                  DXMsgObject_Record.esORGANIZATION:=WorkString;
            34:// 'RELAY-VERSION'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esRELAYVERSION:=DXMsgObject_Record.esRELAYVERSION+WorkString
               else
                  DXMsgObject_Record.esRELAYVERSION:=WorkString;
            35:// 'NNTP-POSTING-HOST'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esNNTPPOSTINGHOST:=DXMsgObject_Record.esNNTPPOSTINGHOST+WorkString
               else
                  DXMsgObject_Record.esNNTPPOSTINGHOST:=WorkString;
            36:// 'EXPIRES'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esEXPIRES:=DXMsgObject_Record.esEXPIRES+WorkString
               else
                  DXMsgObject_Record.esEXPIRES:=WorkString;
            37:// 'FOLLOWUP-TO'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esFOLLOWUPTO:=DXMsgObject_Record.esFOLLOWUPTO+WorkString
               else
                  DXMsgObject_Record.esFOLLOWUPTO:=WorkString;
            38:// 'CONTROL'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esCONTROL:=DXMsgObject_Record.esCONTROL+WorkString
               else
                  DXMsgObject_Record.esCONTROL:=WorkString;
            39:// 'APPROVED'
               if UseCurrentParseCode then
                  DXMsgObject_Record.esAPPROVED:=DXMsgObject_Record.esAPPROVED+WorkString
               else
                  DXMsgObject_Record.esAPPROVED:=WorkString;
         else begin// unknown command - merge into "unknown stringlist"
               if WorkString<>'' then begin
                  if UseCurrentParseCode then
                     DXMsgObject_Record.esUnknown[DXMsgObject_Record.esUnknown.Count-1]:=
                        DXMsgObject_Record.esUnknown[DXMsgObject_Record.esUnknown.Count-1]+WorkString
                  else
                     DXMsgObject_Record.esUnknown.Append(CurrentParseCode+': '+WorkString);
               end;
            end;
         end;
         Header.Delete(0);
         cnt:=Header.Count;
      end;
   finally
      header.EndUpdate;
   end;
end;

procedure TDXGeneralMsgObject.HeaderClear; // 2.0
begin
   HeaderInitialize;
end;

{$HINTS OFF}

procedure TDXGeneralMsgObject.LoadFromStream(Stream:TStream);
var
   TSL:TStringList;
   Loop:Integer;

begin
   TSL:=TStringList.Create;
   try
      TSL.LoadFromStream(Stream);
      Loop:=0;
      while (Loop<TSL.Count)and(TSL[Loop]<>'') do
         Inc(Loop);

      while TSL.Count>Loop do
         TSL.Delete(TSL.Count-1);
      if TSL[Loop-1]='' then TSL.Delete(Loop-1);
      HeaderInitialize;
      HeaderProcess(TSL);
   finally
      TSL.Free;
      TSL:=nil;
   end;
end;

procedure TDXGeneralMsgObject.LoadFromFile(const Filename:string);
var
   TSL:TStringList;
   Loop:Integer;

begin
   TSL:=TStringList.Create;
   TSL.LoadFromFile(FileName);
   Loop:=0;
   while (Loop<TSL.Count)and(TSL[Loop]<>'') do
      Inc(Loop);
   while TSL.Count>Loop do
      TSL.Delete(TSL.Count-1);
   if TSL[Loop-1]='' then TSL.Delete(Loop-1);
   HeaderInitialize;
   HeaderProcess(TSL);
   TSL.Free;
   TSL:=nil;
end;
{$HINTS OFF}

procedure TDXGeneralMsgObject.SaveHeaderToList(var header:tstringlist);
var
   Loop:Integer;

begin
   if not Assigned(Header) then Exit;
   with DXMsgObject_Record^ do begin
      if esRECEIVED<>'' then Header.Append('Received: '+esReceived);
      if esRELAYVERSION<>'' then
         Header.Append('Relay-Version: '+esRelayVersion);
      if esMESSAGEID<>'' then Header.Append('Message-ID: '+esMessageID);
      if esPATH<>'' then Header.Add('Path: '+esPATH);
      if esTO<>'' then Header.Append('To: '+esTo);
      if esFROM<>'' then Header.Append('From: '+esFrom);
      if esRETURNPATH<>'' then Header.Append('Return-Path: '+esRETURNPATH);
      if esDATE<>'' then Header.Append('Date: '+esDATE);
      if esSENDER<>'' then Header.Append('Sender: '+esSENDER);
      if esCC<>'' then Header.Append('CC: '+esCC);
      if esBCC<>'' then Header.Append('BCC: '+esBCC);
      if esREPLYTO<>'' then Header.Append('Reply-To: '+esREPLYTO);
      if esLINES<>'' then Header.Add('Lines: '+esLINES);
      if esNEWSGROUPS<>'' then Header.Add('Newsgroups: '+esNEWSGROUPS);
      if esINREPLYTO<>'' then Header.Append('In-Reply-To: '+esINReplyTO);
      if esREFERENCES<>'' then Header.Append('References: '+esReferences);
      if esKEYWORDS<>'' then Header.Append('Keywords: '+esKEYWORDS);
      if esENCRYPTED<>'' then Header.Append('ENCRYPTED: '+esENCRYPTED);
      if esRESENTREPLYTO<>'' then
         Header.Append('Resent-Reply-To: '+esResentReplyTo);
      if esRESENTFROM<>'' then Header.Append('Resent-From: '+esRESENTFROM);
      if esRESENTSENDER<>'' then
         Header.Append('Resent-Sender: '+esRESENTSENDER);
      if esRESENTDATE<>'' then Header.Append('Resent-Date: '+esRESENTDATE);
      if esRESENTTO<>'' then Header.Append('Resent-TO: '+esRESENTTO);
      if esRESENTCC<>'' then Header.Append('Resent-CC: '+esRESENTCC);
      if esRESENTBCC<>'' then Header.Append('Resent-BCC: '+esRESENTBCC);
      if esRESENTMESSAGEID<>'' then
         Header.Append('Resent-Message-ID: '+esRESENTMESSAGEID);
      if esMIMEVERSION<>'' then Header.Append('MIME-Version: '+esMIMEVERSION);
      if esCONTENTTYPE<>'' then Header.Append('Content-Type: '+esCONTENTTYPE);
      if esCONTENTTRANSFERENCODING<>'' then
         Header.Append('Content-Transfer-Encoding: '+esCONTENTTRANSFERENCODING);
      if esDISTRIBUTION<>'' then Header.Append('Distribution: '+esDistribution);
      if esORGANIZATION<>'' then Header.Append('Organization: '+esOrganization);
      if esNNTPPOSTINGHOST<>'' then
         Header.Append('NNTP-Posting-Host: '+esNNTPPOSTINGHOST);
      if esEXPIRES<>'' then Header.Append('Expires: '+esEXPIRES);
      if esFOLLOWUPTO<>'' then Header.Append('Followup-To: '+esFOLLOWUPTO);
      if esCONTROL<>'' then Header.Append('Control: '+esCONTROL);
      if esAPPROVED<>'' then Header.Append('Approved: '+esAPPROVED);
      for Loop:=1 to esSUBJECT.count do
         Header.Append('Subject: '+esSUBJECT[Loop-1]);
      for Loop:=1 to esCOMMENTS.count do
         Header.Append('Comments: '+esCOMMENTS[Loop-1]);
      for Loop:=1 to esUNKNOWN.count do
         Header.Append(esUNKNOWN[Loop-1]);
   end;
end;

procedure TDXGeneralMsgObject.SaveToStream(Stream:TStream);
var
   TSL:TStringList;

begin
   TSL:=TStringList.Create;
   Stream.Seek(0, 0);
   SaveHeaderToList(TSL);
   TSL.SaveToStream(Stream);
   Stream.Seek(0, 0);
   TSL.Free;
   TSL:=nil;
end;

procedure TDXGeneralMsgObject.SaveToFile(const Filename:string);
var
   MemoryStream:TMemoryStream;

begin
   MemoryStream:=TMemoryStream.Create;
   SaveToStream(MemoryStream);
   MemoryStream.SaveToFile(FileName);
   MemoryStream.Free;
   MemoryStream:=nil;
end;

procedure TDXGeneralMsgObject.DeleteFile(const Filename:string);
begin
   DeleteFile(FileName);
end;

function TDXGeneralMsgObject.ActualHeader:PDXMsgObjectRecord;
begin
   Result:=DXMsgObject_Record;
end;

end.

