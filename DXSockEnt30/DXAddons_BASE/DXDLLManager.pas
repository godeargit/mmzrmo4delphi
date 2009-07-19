unit DXDLLManager;

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDLLManager
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
// This component is an array of DLL files loaded in ram, tracking the instance
// count. Since windows will not load it multiple times, but does track the
// number of instances of the DLL. This allows developers track the instance
// count of the loaded libraries.
//
// This is mainly used by the ISAPI and FILTER engines.
///////////////////////////////////////////////////////////////////////////////

interface

uses
   DXString,
   Classes,
   Windows;

{$I DXAddons.DEF}

type
   PDLLInfo=^TDLLInfo;
   TDLLInfo=record
      Handle:Integer;
      InstanceCount:Integer;
      DLLName:string;
   end;

   TDXDLLManager=class(TDXComponent)
   private
      DLLList:TList;
   protected
      function GetDLLsLoaded:Integer;
      function GetTotalInstances:Integer;
   public
      constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY}override;
         {$ENDIF}
      destructor Destroy; override;
      function LoadThisDLL(DLLName:string):Boolean;
      function UnloadThisDLL(DLLName:string):Boolean;
      function GetDLLHandle(DLLName:string):Integer;
      function GetDLLName(DLLHandle:Integer):string;
      function GetDLLNameInstances(DLLName:string):Integer;
   published
      property DLLsLoaded:Integer read GetDLLsLoaded;
      property TotalInstances:Integer read GetTotalInstances;
   end;

implementation

constructor TDXDLLManager.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   DLLList:=TList.Create;
end;

destructor TDXDLLManager.Destroy;
var
   DLLInfo:PDLLInfo;

begin
   MyCriticalSection.StartingWrite;
   if Assigned(DLLList) then begin
      while DLLList.Count>0 do begin
         DLLInfo:=DLLList.Items[0];
         while DLLInfo^.InstanceCount>0 do begin
            FreeLibrary(DLLInfo^.Handle);
            Dec(DLLInfo^.InstanceCount);
         end;
         Dispose(DLLInfo);
         DLLList.Delete(0);
      end;
      DLLList.Free;
      DLLList:=nil;
   end;
   MyCriticalSection.FinishedWrite;
   inherited destroy;
end;

function TDXDLLManager.LoadThisDLL(DLLName:string):Boolean;
var
   Handle:Integer;
   I:Integer;
   DLLInfo:PDLLInfo;
   Err:PChar;

begin
   Result:=False;
   Handle:=LoadLibrary(PChar(dllName));
   try
      if Handle<>0 then begin
         dllName:=Uppercase(dllName);
         MyCriticalSection.StartingWrite;
         for i:=DLLList.Count-1 downto 0 do begin
            DLLInfo:=DLLList.Items[i];
            if DLLInfo^.DLLName=dllName then begin
               Result:=True;
               Inc(DLLInfo^.InstanceCount);
               MyCriticalSection.FinishedWrite;
               Exit;
            end;
         end;
         MyCriticalSection.FinishedWrite;
         if not Result then begin
            New(DLLInfo);
            DLLInfo^.Handle:=Handle;
            DLLInfo^.DLLName:=dllName;
            DLLInfo^.InstanceCount:=1;
            MyCriticalSection.StartingWrite;
            DLLList.Add(DLLInfo);
            MyCriticalSection.FinishedWrite;
            Result:=True;
         end;
      end
      else begin
         GetMem(Err, 256);
         FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, Err,
            256, nil);
         FreeMem(Err, 256);
      end;
   except
   end;
end;

function TDXDLLManager.UnloadThisDLL(DLLName:string):Boolean;
var
   I:Integer;
   DLLInfo:PDLLInfo;

begin
   DLLName:=UpperCase(DLLName);
   Result:=False;
   MyCriticalSection.StartingWrite;
   try
      for i:=DLLList.Count-1 downto 0 do begin
         DLLInfo:=DLLList.Items[i];
         if Assigned(DLLInfo) then begin
            if DLLInfo^.DLLName=DLLName then begin
               Result:=FreeLibrary(DLLInfo^.Handle);
               Dec(DLLInfo^.InstanceCount);
               if DLLInfo^.InstanceCount=0 then begin
                  Dispose(DLLInfo);
                  DLLList.Delete(i);
                  Break;
               end;
            end;
         end;
      end;
   except
   end;
   MyCriticalSection.FinishedWrite;
end;

function TDXDLLManager.GetDLLHandle(DLLName:string):Integer;
var
   I:Integer;
   DLLInfo:PDLLInfo;

begin
   DLLName:=Uppercase(DLLName);
   Result:=0;
   MyCriticalSection.StartingRead;
   for i:=DLLList.Count-1 downto 0 do begin
      DLLInfo:=DLLList.Items[i];
      if Assigned(DLLInfo) then begin
         if DLLInfo^.DLLName=DLLName then begin
            Result:=DLLInfo^.Handle;
            Break;
         end;
      end;
   end;
   MyCriticalSection.FinishedRead;
end;

function TDXDLLManager.GetDLLName(DLLHandle:Integer):string;
var
   I:Integer;
   DLLInfo:PDLLInfo;

begin
   Result:='';
   MyCriticalSection.StartingRead;
   for i:=DLLList.Count-1 downto 0 do begin
      DLLInfo:=DLLList.Items[i];
      if Assigned(DLLInfo) then begin
         if DLLInfo^.Handle=DLLHandle then begin
            Result:=DLLInfo^.DLLName;
            Break;
         end;
      end;
   end;
   MyCriticalSection.FinishedRead;
end;

function TDXDLLManager.GetDLLsLoaded:Integer;
begin
   MyCriticalSection.StartingRead;
   if Assigned(DLLList) then
      Result:=DLLList.Count
   else
      Result:=0;
   MyCriticalSection.FinishedWrite;
end;

function TDXDLLManager.GetTotalInstances:Integer;
var
   I:Integer;

begin
   Result:=0;
   if not Assigned(DLLList) then Exit;
   MyCriticalSection.StartingRead;
   for i:=0 to DLLList.Count-1 do
      if Assigned(DLLList.Items[i]) then
         Result:=Result+(PDLLInfo(DLLList.Items[i]).InstanceCount);
   MyCriticalSection.FinishedRead;
end;

function TDXDLLManager.GetDLLNameInstances(DLLName:string):Integer;
var
   I:Integer;
   DLLInfo:PDLLInfo;

begin
   Result:=0;
   if not Assigned(DLLList) then Exit;
   DLLName:=Uppercase(DLLName);
   MyCriticalSection.StartingRead;
   for i:=0 to DLLList.Count-1 do begin
      DLLInfo:=DLLList.Items[i];
      if DLLInfo^.DLLName=DLLName then begin
         Result:=Result+DLLInfo^.InstanceCount;
      end;
   end;
   MyCriticalSection.FinishedRead;
end;

end.

