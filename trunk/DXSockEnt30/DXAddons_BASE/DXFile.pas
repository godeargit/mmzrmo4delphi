unit DXFile;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFile
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
// TDXFile allows you to load data (files) into memory, then read the data.
// An example would be to read the whole RFC822 msg into memory, then pass that
// memory to this component, and do readln's against the header.
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

type
   TDXFile=class(TDXComponent)
   private
      fBuf:TMemoryStream;
      fFound:Boolean;
   protected
      procedure SetTextPos(value:Longint);
      function GetTextPos:LongInt;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function Readln:string;
      function ReadUnixLN:string;
      function ReadMacintoshLN:string;
      procedure LoadFromFile(const Filename:string);
      procedure LoadFromStream(Stream:TStream);
      procedure LoadFromPointer(Data, DataSize:Integer);
   published
      property StringFound:Boolean read FFound;
      property TextPosition:Longint read GetTextPos
         write SetTextPos;
   end;

implementation

function TDXFile.Readln:string;
var
   TempStr:string;
   GDone:Boolean;
   Ch:Char;

begin
   TempStr:='';
   GDone:=False;
   FFound:=False;
   while not GDone do begin
      if FBuf.Position<FBuf.Size then
         FBuf.Read(Ch, 1)
      else
         Ch:=#0;
      case Ch of
         #0:begin
               GDone:=FBuf.Position>=FBuf.Size-1;
               if not GDone then begin
                  TempStr:=TempStr+Ch;
                  FFound:=True;
               end;
            end;
         #26:;
         #13:begin
               GDone:=True;
               FFound:=True;
               Fbuf.Read(Ch, 1);
               if Ch<>#10 then FBuf.Position:=FBuf.Position-1;
            end;
      else begin
            TempStr:=TempStr+Ch;
            FFound:=True;
         end;
      end;
   end;
   Result:=TempStr;
end;

function TDXFile.ReadUnixLN:string;
var
   TempStr:string;
   GDone:Boolean;
   Ch:Char;

begin
   TempStr:='';
   GDone:=False;
   FFound:=False;
   while not GDone do begin
      if FBuf.Position<FBuf.Size then
         FBuf.Read(Ch, 1)
      else
         Ch:=#0;
      case Ch of
         #0:
            if FBuf.Position>=FBuf.Size-1 then
               GDone:=True
            else begin
               TempStr:=TempStr+Ch;
               FFound:=True;
            end;
         #26:;
         #10:begin
               GDone:=True;
               FFound:=True;
            end;
      else begin
            TempStr:=TempStr+Ch;
            FFound:=True;
         end;
      end;
   end;
   Result:=TempStr;
end;

function TDXFile.ReadMacintoshLN:string;
var
   TempStr:string;
   GDone:Boolean;
   Ch:Char;

begin
   TempStr:='';
   GDone:=False;
   FFound:=False;
   while not GDone do begin
      if FBuf.Position<FBuf.Size then
         FBuf.Read(Ch, 1)
      else
         Ch:=#0;
      case Ch of
         #0:
            if FBuf.Position>=FBuf.Size-1 then
               GDone:=True
            else begin
               TempStr:=TempStr+Ch;
               FFound:=True;
            end;
         #26:;
         #13:begin
               GDone:=True;
               FFound:=True;
            end;
      else begin
            TempStr:=TempStr+Ch;
            FFound:=True;
         end;
      end;
   end;
   Result:=TempStr;
end;

procedure TDXFile.SetTextPos(Value:Longint);
begin
   FBuf.Position:=Value;
end;

function TDXFile.GetTextPos:LongInt;
begin
   Result:=FBuf.Position;
end;

procedure TDXFile.LoadFromFile(const Filename:string);
begin
   FBuf.LoadFromFile(Filename);
end;

procedure TDXFile.LoadFromStream(Stream:TStream);
begin
   FBuf.LoadFromStream(Stream);
end;

procedure TDXFile.LoadFromPointer(Data, DataSize:Integer);
begin
   FBuf.Clear;
   FBuf.Write(Data, DataSize);
end;

constructor TDXFile.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   FBuf:=TMemoryStream.Create;
   FFound:=False;
end;

destructor TDXFile.Destroy;
begin
   if Assigned(FBuf) then begin
      FBuf.Free;
      FBuf:=nil;
   end;
   inherited destroy;
end;

end.

