unit DXWebFAT;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXWebFAT
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
// The concept of this component is to improve upon the speed features of the
// DXFileAllocationTable object by keeping a memory resident copy of the
// actual file contents. Major performance booster for files that are static!
///////////////////////////////////////////////////////////////////////////////

uses
   DXFileAllocationTable, 
   DXBinaryTree,
   Classes;

{$I DXAddons.def}

type
   PWebSRec=^TWebSRec;
   TWebSRec=packed record
      DateTime:TDateTime;
      FileSize:Integer;
      Attribute:Integer;
      Requests:Integer;
      AddedToCache:TDateTime;
      PathFileNameCRC:Longint;
      FileContents:TMemoryStream;
   end;

   TDXWebFAT=class(TDXFileAllocationTable)
   private
      { Private declarations }
      FMaxKB:Integer;
      FMemUsed:Integer;
      FHitsSaved:Integer;
      fSupportDynamicRefresh:Boolean;
      BinTree:TDXBinarySearchTree;
      FMaxCachedEntries:Integer;
      function AddNewWebEntry(const PathFileName:string):PWebSRec;
   protected
      { Protected declarations }
      function GetFATableCount:Integer;
      procedure SetMaxCachedEntries(Value:Integer);
   public
      { Public declarations }
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override; {$ENDIF}
      destructor Destroy; override;
      procedure Clear;
      function WebFileInfo(const PathFileName:string; var WebSRec:PWebSRec):Boolean;
      function WebFileContent(const WebSRec:PWebSRec; MemoryStream:TStream):Boolean;
   published
      property MaxKBPerFile:Integer read FMaxKB
         write FMaxKB;
      property MemUsed:Integer read FMemUsed;
      property HitsSaved:Integer read FHitsSaved;
      property SupportDynamicRefresh:Boolean read fSupportDynamicRefresh
         write fSupportDynamicRefresh;
      property CachedEntries:Integer read GetFATableCount;
      property MaxCachedEntries:Integer read FMaxCachedEntries
         write SetMaxCachedEntries;
   end;

implementation

uses
   DXString,
   SysUtils;

function CompareMyRec(aItem1, aItem2:pointer):integer; far;
begin
   if PWebSRec(aItem1)^.PathFileNameCRC<PWebSRec(aItem2)^.PathFileNameCRC then Result:=-1
   else if PWebSRec(aItem1)^.PathFileNameCRC>PWebSRec(aItem2)^.PathFileNameCRC then Result:=1
   else Result:=0;
end;

procedure DisposeMyRec(aItem:pointer); far;
begin
   Dispose(PWebSRec(aItem));
end;

function TDXWebFAT.GetFATableCount:Integer;
begin
   Result:=BinTree.Count;
end;

procedure TDXWebFAT.SetMaxCachedEntries(Value:Integer);
begin
   if Value<BinTree.Count then BinTree.Clear;
   fMaxCachedEntries:=Value;
end;

function TDXWebFAT.AddNewWebEntry(const PathFileName:string):PWebSRec;
var
   Tmp:PWebSrec;
   SRec:TSearchRec;
   Err:Integer;
   Stream:TFileStream;

begin
   Result:=nil;
   Err:=FindFirst(PathFileName, faAnyFile, SRec);
   if (Err>0)or(SRec.Attr and faDirectory<>0) then begin
      FindClose(SRec);
      Exit;
   end;
   if FMaxKB>0 then begin
      if SRec.Size<(FMaxKB*1024) then begin
         New(Tmp);
         Tmp^.FileSize:=SRec.Size;
         Tmp^.PathFileNameCRC:=DXString.CRC32ByString(Uppercase(PathFileName), $FFFF);
         Tmp^.FileContents:=nil;
         Inc(FMemUsed, Tmp^.FileSize);
         Tmp^.FileContents:=TMemoryStream.Create;
         MyCriticalSection.StartingWrite;
         try
            Stream:=TFileStream.Create(PathFileName, fmOpenRead or fmShareDenyNone);
            Tmp^.FileContents.LoadFromStream(Stream);
            Stream.Free;
         except
            Tmp^.FileContents.Free;
            Dispose(Tmp);
            MyCriticalSection.FinishedWrite;
            Exit;
         end;
         MyCriticalSection.FinishedWrite;
         Tmp^.FileContents.SetSize(Tmp^.FileSize);
      end
      else begin
         FindClose(SRec);
         Exit;
      end;
   end
   else begin
      FindClose(SRec);
      Exit;
   end;
   Tmp^.DateTime:=FileDateToDateTime(SRec.Time);
   Tmp^.Attribute:=SRec.Attr;
   Tmp^.Requests:=1;
   Tmp^.AddedToCache:=Now;
   FindClose(SRec);
   Result:=Tmp;
   SetMaxCachedEntries(fMaxCachedEntries);
   BinTree.Insert(Tmp);
end;

///////////////////////////////////////////////////////////////////////////////
// WEBFILECONTENT:
//                Returns the actual content of the requested file, if it was
//                allocated. Controlled by "File Size".
///////////////////////////////////////////////////////////////////////////////
function TDXWebFAT.WebFileContent(const WebSRec:PWebSRec; MemoryStream:TStream):Boolean;
begin
   Result:=False;
   if Assigned(MemoryStream) then begin
      if Assigned(WebSRec.FileContents) then begin
         Result:=True;
         MemoryStream.Size:=0; 
         MyCriticalSection.StartingRead; 
         WebSRec.FileContents.SaveToStream(MemoryStream); 
         MyCriticalSection.FinishedRead;
         MemoryStream.Seek(0, 0);
      end;
   end;
end;

///////////////////////////////////////////////////////////////////////////////
// CREATE:
//        Allocate the "Cache" and some default settings for operation.
///////////////////////////////////////////////////////////////////////////////
constructor TDXWebFAT.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   FMaxKB:=40; 
   FMemUsed:=0;
   FHitsSaved:=0;
   FMaxCachedEntries:=512;
   fSupportDynamicRefresh:=True;
   BinTree:=TDXBinarySearchTree.Create(CompareMyRec, DisposeMyRec);
end;

///////////////////////////////////////////////////////////////////////////////
// DESTROY:
//         Deallocate the "Cache".
///////////////////////////////////////////////////////////////////////////////
destructor TDXWebFAT.Destroy;
begin
   BinTree.Free;
   inherited destroy;
end;

///////////////////////////////////////////////////////////////////////////////
// WEBFILEINFO:
//             Find a "Path + Filename" entry in the "Cache". If it does not
//             exist this routine calls "AddNewWEBEntry".
///////////////////////////////////////////////////////////////////////////////
function TDXWebFAT.WebFileInfo(const PathFileName:string; var WebSRec:PWebSRec):Boolean;
var
   QueryPSRec:PSRec;
   Wi:Integer;

begin
   Wi:=DXString.CRC32ByString(DXString.Uppercase(PathFileName), $FFFF);
   New(QueryPSRec);
   QueryPSRec^.PathFileNameCRC:=Wi;
   WebSRec:=BinTree.Find(QueryPSRec);
   Dispose(QueryPSRec);
   if WebSRec<>nil then begin
      WebSRec.Requests:=WebSRec.Requests+1;
      Result:=True;
      if fSupportDynamicRefresh then
         if WebSRec^.DateTime<>FileDateToDateTime(FileAge(DXString.Uppercase(PathFileName))) then begin
            BinTree.Delete(WebSRec);
            WebSRec:=AddNewWebEntry(DXString.Uppercase(PathFileName));
            Result:=WebSRec<>nil;
            Exit;
         end;
      Inc(FHitsSaved);
      Exit;
   end;
   WebSRec:=AddNewWebEntry(DXString.Uppercase(PathFileName));
   Result:=WebSRec<>nil; 
end;

procedure TDXWebFAT.Clear;
begin
   BinTree.Clear;
end;

end.

