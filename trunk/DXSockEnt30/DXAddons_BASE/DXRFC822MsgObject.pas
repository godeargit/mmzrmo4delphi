unit DXRFC822MsgObject;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXRFC822MsgObject
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
   DXGeneralMsgObject,
   Classes;

{$I DXAddons.def}

type
   TDXRFC822WhichPart=(wpHeaderAndBody, wpHeaderOnly, wpBodyOnly);

   TDXRFC822MsgObject=class(TDXGeneralMsgObject)
   private
      { Private declarations }
      DXMsgObject_Body:TStringList;
      fHeaderExtension:string;
      fBodyExtension:string;
   protected
      { Protected declarations }
   public
      { Public declarations }
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      procedure Clear;
      procedure LoadFromStringlist(myTSL:TStringlist;
         WhichPart:TDXRFC822WhichPart);
      procedure LoadFromStream(Stream:TStream; WhichPart:TDXRFC822WhichPart);
      procedure LoadFromFile(const Filename:string;
         WhichPart:TDXRFC822WhichPart);
      procedure SaveToStream(Stream:TStream; WhichPart:TDXRFC822WhichPart);
      procedure SaveToFile(const Filename:string; WhichPart:TDXRFC822WhichPart);
      procedure DeleteFile(const Filename:string);
   published
      { Published declarations }
      property HeaderFileExtension:string read fHeaderExtension
         write fHeaderExtension;
      property BodyFileExtension:string read fBodyExtension
         write fBodyExtension;
   end;

implementation

uses
   DXString,
   SysUtils;

{$IFDEF OBJECTS_ONLY}
constructor TDXRFC822MsgObject.Create;
{$ELSE}
constructor TDXRFC822MsgObject.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   DXMsgObject_Body:=TStringList.Create;
   {$ELSE}
   inherited Create(AOwner);
   if not(csDesigning in ComponentState) then
      DXMsgObject_Body:=TStringList.Create
   else
      DXMsgObject_Body:=nil;
   {$ENDIF}
   fHeaderExtension:='.hdr';
   fBodyExtension:='.bdy';
end;

destructor TDXRFC822MsgObject.Destroy;
begin
   if Assigned(DXMsgObject_Body) then begin
      DXMsgObject_Body.Clear;
      FreeAndNil(DxMsgObject_Body);
   end;
   inherited Destroy;
end;

procedure TDXRFC822MsgObject.LoadFromStream(Stream:TStream;
   WhichPart:TDXRFC822WhichPart);
var
   TSL:TStringList;
   Loop:Integer;
   LoopMax:Integer;
   cnt:integer;

begin
   TSL:=nil;
   TSL:=TStringList.Create;
   try
      TSL.LoadFromStream(Stream);
      Loop:=0;
      if (WhichPart=wpHeaderAndBody)or
         (WhichPart=wpHeaderOnly) then begin
         HeaderInitialize;
         HeaderProcess(TSL);
      end;
      LoopMax:=TSL.Count;
      cnt:=DxMsgObject_Body.Count;
      while cnt>0 do begin
         DxMsgObject_Body.Delete(0);
         cnt:=DxMsgObject_Body.Count;
      end;
      DXMsgObject_Body.Clear;
      if (WhichPart=wpHeaderAndBody) then
         while (Loop<LoopMax)and(TSL[Loop]<>'') do Inc(Loop);
      if (WhichPart=wpHeaderAndBody)or
         (WhichPart=wpBodyOnly) then begin
         LoopMax:=TSL.Count;
         while Loop<LoopMax do begin
            DXMsgObject_Body.Add(TSL[Loop]);
            Inc(loop);
         end;
      end;
   finally
      TSL.BeginUpdate;
      try
         LoopMax:=TSL.Count;
         while loopmax>0 do begin
            TSL.Strings[0]:='';
            TSL.Delete(0);
            LoopMax:=TSL.Count;
         end;
         TSL.Clear;
      finally
         TSL.EndUpdate;
      end;
      FreeAndNil(TSL);
   end;
end;

procedure TDXRFC822MsgObject.LoadFromFile(const Filename:string;
   WhichPart:TDXRFC822WhichPart);
var
   TSL:TStringList;
   Loop:Integer;
   LoopMax:Integer;

begin
   TSL:=TStringList.Create;
   Loop:=0;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpHeaderOnly) then begin
      TSL.LoadFromFile(FileName+fHeaderExtension);
      HeaderInitialize;
      HeaderProcess(TSL);
   end;
   DXMsgObject_Body.Clear;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpBodyOnly) then begin
      TSL.LoadFromFile(FileName+fBodyExtension);
      LoopMax:=TSL.Count;
      while Loop<LoopMax do begin
         DXMsgObject_Body.Add(TSL[Loop]);
         Inc(loop);
      end;
   end;
   TSL.Free;
   TSL:=nil;
end;

procedure TDXRFC822MsgObject.SaveToStream(Stream:TStream;
   WhichPart:TDXRFC822WhichPart);
var
   TSL:TStringList;
   Loop:Integer;

begin
   TSL:=TStringList.Create;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpHeaderOnly) then begin
      SaveHeaderToList(TSL);
   end;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpBodyOnly) then begin
      for Loop:=1 to DXMsgObject_Body.count do
         TSL.Append(DXMsgObject_Body[Loop-1]);
   end;
   Stream.Seek(0, 0);
   TSL.SaveToStream(Stream);
   Stream.Seek(0, 0);
   TSL.Free;
   TSL:=nil;
end;

procedure TDXRFC822MsgObject.SaveToFile(const Filename:string;
   WhichPart:TDXRFC822WhichPart);
var
   TSL:TStringList;
   Loop:Integer;

begin
   TSL:=TStringList.Create;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpHeaderOnly) then begin
      SaveHeaderToList(TSL);
      TSL.SaveToFile(FileName+fHeaderExtension);
      TSL.Clear;
   end;
   if (WhichPart=wpHeaderAndBody)or
      (WhichPart=wpBodyOnly) then begin
      for Loop:=1 to DXMsgObject_Body.count do
         TSL.Append(DXMsgObject_Body[Loop-1]);
      TSL.SaveToFile(FileName+fBodyExtension);
   end;
   TSL.Free;
   TSL:=nil;
end;

procedure TDXRFC822MsgObject.DeleteFile(const Filename:string);
begin
   DeleteFile(FileName+fHeaderExtension);
   DeleteFile(FileName+fBodyExtension);
end;

procedure TDXRFC822MsgObject.Clear;
var
   cnt:integer;
begin
   if Assigned(DXMsgObject_BODY) then begin
      DxMsgObject_Body.BeginUpdate;
      try
         cnt:=DxMsgObject_Body.Count;
         while cnt>0 do begin
            DxMsgObject_Body.Delete(0);
            cnt:=DxMsgObject_Body.Count;
         end;
      finally
         DXMsgObject_Body.Clear;
         DxMsgObject_Body.EndUpdate;
      end;
   end;
end;

procedure TDXRFC822MsgObject.LoadFromStringlist(myTSL:TStringlist;
   WhichPart:TDXRFC822WhichPart);
var
   TSL:TStringList;
   Loop:Integer;
   LoopMax:Integer;
   cnt:integer;

begin
   TSL:=nil;
   TSL:=TStringList.Create;
   try
      Clear;
      TSL.BeginUpdate;
      TSL.Assign(myTSL);
      TSL.EndUpdate;
      Loop:=0;
      if (WhichPart=wpHeaderAndBody)or
         (WhichPart=wpHeaderOnly) then begin
         HeaderInitialize;
         HeaderProcess(TSL);
      end;
      LoopMax:=TSL.Count;
      cnt:=DxMsgObject_Body.Count;
      while cnt>0 do begin
         DxMsgObject_Body.Delete(0);
         cnt:=DxMsgObject_Body.Count;
      end;
      DXMsgObject_Body.Clear;
      if (WhichPart=wpHeaderAndBody) then
         while (Loop<LoopMax)and(TSL[Loop]<>'') do Inc(Loop);
      if (WhichPart=wpHeaderAndBody)or
         (WhichPart=wpBodyOnly) then begin
         LoopMax:=TSL.Count;
         while Loop<LoopMax do begin
            DXMsgObject_Body.Add(TSL[Loop]);
            Inc(loop);
         end;
      end;
   finally
      TSL.BeginUpdate;
      try
         LoopMax:=TSL.Count;
         while loopmax>0 do begin
            TSL.Strings[0]:='';
            TSL.Delete(0);
            LoopMax:=TSL.Count;
         end;
         TSL.Clear;
      finally
         TSL.EndUpdate;
      end;
      FreeAndNil(TSL);
   end;
end;

end.

