unit DXFileAllocationTable;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFileAllocationTable
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
// The concept of this component is to centralize all 'FindFirst' requests to
// one area, and caching the results to optimize future requests for the same
// information.
///////////////////////////////////////////////////////////////////////////////

uses
   Classes,
   DXBinaryTree,
   DXString;

{$I DXAddons.DEF}

type
   PSRec=^TSRec;
   TSRec=packed record
      DateTime:TDateTime;
      FileSize:Integer;
      Attribute:Integer;
      Requests:Integer;
      AddedToCache:TDateTime;
      PathFileNameCRC:Longint;
   end;

   //
   //     *   <B>fsNone</B> stores the information in the cache
   //         engine in FIFO (First In First Out) sequence.
   //     *   <B>fsFileAge</B> stores the information from newest
   //         to oldest sequence based upon the timestamp of the
   //         file when it was added to the cache engine.
   //     *   <B>fsNewestAdd</B> stores the newest file lookup at
   //         the beginning of the list. <B>FASTEST</B>
   //     *   <B>fsFileName</B> stores the information sorted by an
   //         uppercase instance of the filename, like files are
   //         stored in newest first.
   //     *   <B>fsMostRequested</B> stores the information sorted
   //         by the most reuqested files first, like requests are
   //         stored in newest first.
   // <B>Note </B>MostRequested generates the most burden on your
   // application, especially on a busy multithreaded
   // implementation. As the sort routine for all of these types
   // require the cache engine to be locked until finished. The
   // programmer has tried to design the component as optimal as
   // possible, so you should not switch from one sort type to
   // another. As the code does not resort from scratch when
   // changed.
   //
   //
   //
   // Summary
   // Define the sorting method used by the internal caching
   // engine.
   TDXFileAllocationTable=class(TDXComponent)
   private
      // Private declarations
      BinTree:TDXBinarySearchTree;
      FMaxCachedEntries:Integer;
      function AddNewEntry(const PathFileName:string;Const CRC:Longint):PSRec;
   protected
      // Protected declarations
      function GetFATableCount:Integer;
      procedure SetMaxCachedEntries(Value:Integer);
   public
      // Public declarations
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      procedure Clear;
      function FileInformation(const PathFileName:string):PSRec;
      procedure AllFilesMatching(const PathFileName:string; var
         Results:TStringList);
      procedure AllDirsMatching(const PathFileName:string; var
         Results:TStringList); {2.0}
   published
      // Published declarations
      property CachedEntries:Integer read GetFATableCount;
      property MaxCachedEntries:Integer read FMaxCachedEntries
         write SetMaxCachedEntries;
   end;

implementation

uses
   SysUtils;

function CompareMyRec(aItem1, aItem2:pointer):integer; far;
begin
   if PSRec(aItem1)^.PathFileNameCRC<PSRec(aItem2)^.PathFileNameCRC then
      Result:=-1
   else if PSRec(aItem1)^.PathFileNameCRC>PSRec(aItem2)^.PathFileNameCRC then
      Result:=1
   else
      Result:=0;
end;

procedure DisposeMyRec(aItem:pointer); far;
begin
   Dispose(PSRec(aItem));
end;

function TDXFileAllocationTable.GetFATableCount:Integer;
begin
   Result:=BinTree.Count;
end;

procedure TDXFileAllocationTable.SetMaxCachedEntries(Value:Integer);
begin
   if Value<BinTree.Count then BinTree.Clear;
   fMaxCachedEntries:=Value;
end;

function TDXFileAllocationTable.AddNewEntry(const PathFileName:string;Const CRC:Longint):PSRec;
var
   Tmp:PSrec;
   SRec:TSearchRec;
   Err:Integer;

begin
   Result:=nil;
   Err:=FindFirst(PathFileName, faAnyFile, SRec);
   if (Err>0)or(SRec.Attr and faDirectory<>0) then begin
      FindClose(SRec);
      Exit;
   end;
   New(Tmp);
   Tmp.DateTime:=FileDateToDateTime(SRec.Time);
   Tmp.FileSize:=SRec.Size;
   Tmp.Attribute:=SRec.Attr;
   Tmp.Requests:=1;
   Tmp.AddedToCache:=Now;
   Tmp.PathFileNameCRC:=CRC;
   {DXString.CRC32ByString(DXString.Uppercase(PathFileName),
      $FFFF);}
   FindClose(SRec);
   Result:=Tmp;
   SetMaxCachedEntries(fMaxCachedEntries);
   BinTree.Insert(Tmp);
end;

{$IFDEF OBJECTS_ONLY}

constructor TDXFileAllocationTable.Create;
{$ELSE}

constructor TDXFileAllocationTable.Create(AOwner:TComponent);
{$ENDIF}
begin
   inherited create(AOwner);
   BinTree:=TDXBinarySearchTree.Create(CompareMyRec, DisposeMyRec);
   FMaxCachedEntries:=500;
end;

///////////////////////////////////////////////////////////////////////////////
//DESTROY:
//        Deallocate the "Cache".
///////////////////////////////////////////////////////////////////////////////

destructor TDXFileAllocationTable.Destroy;
begin
   BinTree.Free;
   inherited destroy;
end;

///////////////////////////////////////////////////////////////////////////////
//FILEINFORMATION:
//                Find a "Path + Filename" entry in the "Cache". If it does not
//                exist this routine calls "AddNewEntry".
///////////////////////////////////////////////////////////////////////////////

function TDXFileAllocationTable.FileInformation(const
   PathFileName:string):PSRec;
var
   QueryPSRec:PSRec;
   Wi:Integer;

begin
   Wi:=DXString.CRC32ByString(DXString.Uppercase(PathFileName), $FFFF);
   New(QueryPSRec);
   QueryPSRec^.PathFileNameCRC:=Wi;
   Result:=BinTree.Find(QueryPSRec);
   Dispose(QueryPSRec);
   if Result<>nil then Result.Requests:=Result.Requests+1
   else Result:=AddNewEntry(DXString.Uppercase(PathFileName),Wi);
end;

///////////////////////////////////////////////////////////////////////////////
//ALLFILESMATCHING:
//                 This routine does not use the cache, and supports wildcards.
//                 It is included to centralize all "FindFirst" type calls to
//                 one unit, optimizing development use calls. It can be used
//                 to do a "FileExists" too.
///////////////////////////////////////////////////////////////////////////////

procedure TDXFileAllocationTable.AllFilesMatching(const PathFileName:string; var
   Results:TStringList);
var
   SRec:TSearchRec;
   Err:Integer;

begin
   if not Assigned(Results) then Exit;
   Results.Clear;
   Err:=FindFirst(PathFileName, faAnyFile, SRec);
   while Err=0 do begin
      if SRec.Attr and faDirectory=0 then Results.Add(SRec.Name);
      Err:=FindNext(SRec);
   end;
   FindClose(SRec);
   TStringList(Results).Sort;
end;

procedure TDXFileAllocationTable.AllDirsMatching(const PathFileName:string; var
   Results:TStringList);
var
   SRec:TSearchRec;
   Err:Integer;

begin
   if not Assigned(Results) then Exit;
   Results.Clear;
   Err:=FindFirst(PathFileName, faAnyFile, SRec);
   while Err=0 do begin
      if (SRec.Attr and faDirectory<>0) then
         if (SRec.Name<>'.')and(SRec.Name<>'..') then Results.Add(SRec.Name);
      Err:=FindNext(SRec);
   end;
   FindClose(SRec);
   TStringList(Results).Sort;
end;

procedure TDXFileAllocationTable.Clear;
begin
   BinTree.Clear;
end;

end.

