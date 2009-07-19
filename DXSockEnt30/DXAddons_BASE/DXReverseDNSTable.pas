unit DXReverseDNSTable;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXReverseDNSTable
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
// The concept of this component is cache redundant reverse DNS calls. This
// allows servers to support Reverse DNS without having to worry about the
// speed lose associated with doing Reverse DNS. After the first reverse, the
// results are stored in a memory array. Making subsequent calls instantaneous.
//
// Also - you should call .Cleanup once the servercore fires an OnAsleep event.
// The cleanup routine will remove stale information from the memory array.
///////////////////////////////////////////////////////////////////////////////

uses
   DXString,
   Classes;

{$I DXAddons.def}

type
   PRDNSRec=^TRDNSRec;
   TRDNSRec=packed record
      LastRequestDateTime:TDateTime;
      LookUp:string;
      IPResult:string;
      HostNameResult:string;
   end;

   TDXReverseDNSTable=class(TDXComponent)
   private
      { Private declarations }
      RDNSTable:TList;
      fMaxCached:Integer;
      function AddNewEntry(const Lookup:string):PRDNSRec;
   protected
      { Protected declarations }
      function GetTableCount:Integer;
   public
      { Public declarations }
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function Find(const ByHostNameOrIPAddress:string):PRDNSRec;
      function FindString(const ByHostNameOrIPAddress:string):string;
      procedure Cleanup(Days:Integer);
      procedure ViewEntries(List:TStrings);
   published
      { Published declarations }
      property CachedEntries:Integer read GetTableCount;
      property MaxCachedEntries:Integer read fMaxCached
         write fMaxCached;
   end;

implementation

uses
   DXSocket,
   Winsock, // PHOSTENT, AF_INET
   SysUtils;

(******************************************************************************
GETFATTABLECOUNT:
                 Useful for displaying statistical/debug information within
                 your application(s). Returns the number of active entries in
                 in the "Cache".
******************************************************************************)
function TDXReverseDNSTable.GetTableCount:Integer;
begin
   if Assigned(RDNSTable) then
      Result:=RDNSTable.Count
   else
      Result:=0;
end;

(******************************************************************************
ADDNEWENTRY:
            This function is called internally when the requested DNS does
            note exist in the "Cache". This routine does the actual call
            to find the address information returned by Reverse DNS.
******************************************************************************)
function TDXReverseDNSTable.AddNewEntry(const Lookup:string):PRDNSRec;
var
   Tmp:PRDNSrec;
   NotFound:Boolean;

begin
   if IsNumericString(Lookup) then begin
      NotFound:=GetAddressCountByIP(Lookup)=0;
   end
   else begin
      NotFound:=GetAddressCountByHost(Lookup)=0;
   end;
   New(Tmp);
   Tmp^.LastRequestDateTime:=Now;
   Tmp^.LookUp:=Lookup;
   if NotFound then begin
      Tmp^.IPResult:=Lookup;
      Tmp^.HostNameResult:=Lookup;
   end
   else begin
      Tmp^.IPResult:=GetIPAddressByHost(Lookup, 1);
      if Tmp^.IPResult='' then Tmp^.IPResult:=Lookup;
      Tmp^.HostNameResult:=GetHostByIPAddress(Tmp^.IPResult);
   end;
   Result:=Tmp;
   MyCriticalSection.StartingWrite;
   RDNSTable.Add(Tmp);
   while RDNSTable.Count>fMaxCached do
      RDNSTable.Delete(0); // RC3
   MyCriticalSection.FinishedWrite;
end;

(******************************************************************************
CREATE:
       Allocate the "Cache" and some default settings for operation.
******************************************************************************)
constructor TDXReverseDNSTable.Create(AOwner:TComponent);
begin
   inherited create(AOwner);
   RDNSTable:=TList.Create;
   fMaxCached:=1024;
end;

(******************************************************************************
DESTROY:
        Deallocate the "Cache".
******************************************************************************)
destructor TDXReverseDNSTable.Destroy;
var
   Tmp:PRDNSrec;

begin
   MyCriticalSection.StartingWrite;
   if Assigned(RDNSTable) then begin
      while RDNSTable.Count>0 do begin
         Tmp:=PRDNSRec(RDNSTable.Items[0]);
         Dispose(Tmp);
         RDNSTable.Delete(0);
      end;
      RDNSTable.Free;
      RDNSTable:=nil;
   end;
   MyCriticalSection.FinishedWrite;
   inherited destroy;
end;

(******************************************************************************
FIND:
******************************************************************************)
function TDXReverseDNSTable.Find(const ByHostNameOrIPAddress:string):PRDNSRec;
var
   Loop:Integer;
   TS:string;

begin
   MyCriticalSection.StartingRead;
   TS:=lowercase(ByHostNameOrIPAddress);
   with RDNSTable do begin
      for Loop:=1 to Count do begin
         if PRDNSRec(Items[Loop-1])^.LookUp=TS then begin
            Result:=PRDNSRec(Items[Loop-1]);
            MyCriticalSection.FinishedRead;
            MyCriticalSection.StartingWrite;
            Result.LastRequestDateTime:=Now;
            Items[Loop-1]:=Result;
            MyCriticalSection.FinishedWrite;
            Exit;
         end;
      end;
   end;
   MyCriticalSection.FinishedRead;
   Result:=AddNewEntry(TS);
end;

function TDXReverseDNSTable.FindString(const
   ByHostNameOrIPAddress:string):string;
var
   RDNSRec:PRDNSRec;

begin
   RDNSRec:=Find(ByHostNameOrIPAddress);
   if Assigned(RDNSRec) then
      Result:=RDNSRec^.HostNameResult
   else
      Result:='';
end;

(******************************************************************************
CLEANUP:
******************************************************************************)
procedure TDXReverseDNSTable.Cleanup(Days:Integer);
var
   Loop:Integer;
   OffSetDate:TDateTime;
   Tmp:PRDNSrec;

begin
   OffSetDate:=Now-Days;
   Loop:=0;
   MyCriticalSection.StartingWrite;
   try
      with RDNSTable do begin
         while Loop<Count do begin
            if PRDNSRec(Items[Loop])^.LastRequestDateTime<=OffSetDate then begin
               Tmp:=PRDNSRec(Items[Loop]);
               Dispose(Tmp);
               Delete(Loop);
            end
            else
               Inc(Loop);
         end;
      end;
   except
   end;
   MyCriticalSection.FinishedWrite;
end;

procedure TDXReverseDNSTable.ViewEntries(List:TStrings);
var
   Loop:Integer;

begin
   List.Clear;
   Loop:=0;
   MyCriticalSection.StartingRead;
   try
      while Loop<RDNSTable.Count do begin
         List.Add(
            FormatDateTime('dd/mm/yyyy hh:nn:ss',
               PRDNSRec(RDNSTable.Items[Loop])^.LastRequestDateTime)+' - '+
            PRDNSRec(RDNSTable.Items[Loop])^.HostNameResult);
         Inc(Loop);
      end;
   except
   end;
   MyCriticalSection.FinishedRead;
end;

end.

