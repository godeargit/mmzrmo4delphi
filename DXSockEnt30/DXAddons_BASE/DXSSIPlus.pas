unit DXSSIPlus;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXSSIPlus
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
// This is descendnat to DXSSI, that introduces more advanced syntax to the
// SSI scripting engine. The design is to incorporate more 'hooks' between this
// layer and the ancestor. Making all the routines seamless to this component.
//
// UniqueID is passed as a parameter throughout the parser engine, this is used
// by commands that report the current session variables, like remote IP address
// or authentication user name or current physical path. This gives the freedom
// to the parser to leave session variables up to you the developer, and leaves
// the design of the SSI engine generic enough to work as a script engine for
// other protocols (override the SSIStart and SSIEnd, or make a descendant).
///////////////////////////////////////////////////////////////////////////////

uses
   DXSSI,
   Classes;

{$I DXAddons.def}

type
   TDXSSIPlus=class(TDXSSI)
   private
      { Private declarations }
      fOnDBInit:SSITChangeSettingEvent;
      fOnDBOpen:SSITChangeSettingEvent;
      fOnDBDone:SSITChangeSettingEvent;
      fOnSQLExecute:SSITChangeSettingEvent;
      fOnResultFirst:SSITChangeSettingEvent;
      fOnResultNext:SSITChangeSettingEvent;
      fOnResultPrior:SSITChangeSettingEvent;
      fOnResultLAST:SSITChangeSettingEvent;
      fOnResultBOF:SSITStringEvent;
      fOnResultEOF:SSITStringEvent;
      fOnResultRowCount:SSITStringEvent;
      fOnResultRecord:SSITStringEvent;
      fOnResultFieldCount:SSITStringEvent;
      fOnDataSetEdit:SSITChangeSettingEvent;
      fOnDataSetDelete:SSITChangeSettingEvent;
      fOnDataSetInsert:SSITChangeSettingEvent;
      fOnDataSetAppend:SSITChangeSettingEvent;
      fOnFieldGetString:SSITStringEvent;
      fOnFieldGetInteger:SSITStringEvent;
      fOnFieldGetDateTime:SSITStringEvent;
      fOnFieldGetName:SSITStringEvent;
      fOnFieldGetBlob:SSITIncludeEvent;
      fOnFieldPutString:SSITChangeSettingEvent;
      fOnFieldPutInteger:SSITChangeSettingEvent;
      fOnFieldPutDateTime:SSITChangeSettingEvent;
      fOnFieldPutBlob:SSITChangeSettingEvent;
   protected
      { Protected declarations }
      procedure SetOnDBInit(value:SSITChangeSettingEvent);
      procedure SetOnDBOpen(value:SSITChangeSettingEvent);
      procedure SetOnDBDone(value:SSITChangeSettingEvent);
      procedure SetOnSQLExecute(value:SSITChangeSettingEvent);
      procedure SetResultFirst(value:SSITChangeSettingEvent);
      procedure SetResultNext(value:SSITChangeSettingEvent);
      procedure SetResultPrior(value:SSITChangeSettingEvent);
      procedure SetResultLAST(value:SSITChangeSettingEvent);
      procedure SetResultBOF(value:SSITStringEvent);
      procedure SetResultEOF(value:SSITStringEvent);
      procedure SetResultRowCount(value:SSITStringEvent);
      procedure SetResultRecord(value:SSITStringEvent);
      procedure SetResultFieldCount(value:SSITStringEvent);
      procedure SetDataSetEdit(value:SSITChangeSettingEvent);
      procedure SetDataSetDelete(value:SSITChangeSettingEvent);
      procedure SetDataSetInsert(value:SSITChangeSettingEvent);
      procedure SetDataSetAppend(value:SSITChangeSettingEvent);
      procedure SetFieldGetString(value:SSITStringEvent);
      procedure SetFieldGetInteger(value:SSITStringEvent);
      procedure SetFieldGetDateTime(value:SSITStringEvent);
      procedure SetFieldGetName(value:SSITStringEvent);
      procedure SetFieldGetBlob(value:SSITIncludeEvent);
      procedure SetFieldPutString(value:SSITChangeSettingEvent);
      procedure SetFieldPutInteger(value:SSITChangeSettingEvent);
      procedure SetFieldPutDateTime(value:SSITChangeSettingEvent);
      procedure SetFieldPutBlob(value:SSITChangeSettingEvent);
   public
      { Public declarations }
      {$IFDEF OBJECTS_ONLY}
      constructor Create;
      {$ELSE}
      constructor Create(AOwner:TComponent); override;
      {$ENDIF}
      destructor Destroy; override;
      function ProcessStream(const UniqueID:Cardinal; Stream:TStream):Boolean;
         override;
      function ProcessLine(const UniqueID:Cardinal; const S:string):string;
         override;
   published
      { Published declarations }
      property OnDatabaseConnectionInit:SSITChangeSettingEvent read fOnDBInit
         write SetOnDBInit;
      property OnDatabaseOpen:SSITChangeSettingEvent read fOnDBOpen
         write SetOnDBOpen;
      property OnDatabaseConnectionDone:SSITChangeSettingEvent read fOnDBDone
         write SetOnDBDone;
      {2.0}
      property OnSQLExecute:SSITChangeSettingEvent read fOnSQLExecute
         write SetOnSQLExecute;
      property OnResultFirst:SSITChangeSettingEvent read fOnResultFirst
         write SetResultFirst;
      property OnResultNext:SSITChangeSettingEvent read fOnResultNext
         write SetResultNext;
      property OnResultPrior:SSITChangeSettingEvent read fOnResultPrior
         write SetResultPrior;
      property OnResultLAST:SSITChangeSettingEvent read fOnResultLAST
         write SetResultLAST;
      property OnResultBOF:SSITStringEvent read fOnResultBOF
         write SetResultBOF;
      property OnResultEOF:SSITStringEvent read fOnResultEOF
         write SetResultEOF;
      property OnResultRowCount:SSITStringEvent read fOnResultRowCount
         write SetResultRowCount;
      property OnResultRecord:SSITStringEvent read fOnResultRecord
         write SetResultRecord;
      property OnResultFieldCount:SSITStringEvent read fOnResultFieldCount
         write SetResultFieldCount;
      property OnDatasetEdit:SSITChangeSettingEvent read fOnDataSetEdit
         write SetDataSetEdit;
      property OnDatasetDelete:SSITChangeSettingEvent read fOnDataSetDelete
         write SetDataSetDelete;
      property OnDatasetInsert:SSITChangeSettingEvent read fOnDataSetInsert
         write SetDataSetInsert;
      property OnDatasetAppend:SSITChangeSettingEvent read fOnDataSetAppend
         write SetDataSetAppend;
      property OnFieldGetString:SSITStringEvent read fOnFieldGetString
         write SetFieldGetString;
      property OnFieldGetInteger:SSITStringEvent read fOnFieldGetInteger
         write SetFieldGetInteger;
      property OnFieldGetDateTime:SSITStringEvent read fOnFieldGetDateTime
         write SetFieldGetDateTime;
      property OnFieldGetName:SSITStringEvent read fOnFieldGetName
         write SetFieldGetName;
      property OnFieldGetBlob:SSITIncludeEvent read fOnFieldGetBlob
         write SetFieldGetBlob;
      property OnFieldPutString:SSITChangeSettingEvent read fOnFieldPutString
         write SetFieldPutString;
      property OnFieldPutInteger:SSITChangeSettingEvent read fOnFieldPutInteger
         write SetFieldPutInteger;
      property OnFieldPutDateTime:SSITChangeSettingEvent read fOnFieldPutDateTime
         write SetFieldPutDateTime;
      property OnFieldPutBlob:SSITChangeSettingEvent read fOnFieldPutBlob
         write SetFieldPutBlob;
   end;

implementation

uses
   DXString,
   SysUtils;

{$IFDEF OBJECTS_ONLY}

constructor TDXSSIPlus.Create;
{$ELSE}

constructor TDXSSIPlus.Create(AOwner:TComponent);
{$ENDIF}
begin
   {$IFDEF OBJECTS_ONLY}
   inherited Create;
   {$ELSE}
   inherited Create(AOwner);
   {$ENDIF}
end;

destructor TDXSSIPlus.Destroy;
begin
   inherited Destroy;
end;

procedure TDXSSIPlus.SetOnDBInit(value:SSITChangeSettingEvent);
begin
   fOnDBInit:=Value;
   AddChangeSettingEvent('DBINIT', Value);
end;

procedure TDXSSIPlus.SetOnDBOpen(value:SSITChangeSettingEvent);
begin
   fOnDBOPEN:=Value;
   AddChangeSettingEvent('DBOPEN', Value);
end;

procedure TDXSSIPlus.SetOnDBDone(value:SSITChangeSettingEvent);
begin
   fOnDBDONE:=Value;
   AddChangeSettingEvent('DBDONE', Value);
end;

procedure TDXSSIPlus.SetOnSQLExecute(value:SSITChangeSettingEvent);
begin
   fOnSQLEXECUTE:=Value;
   AddChangeSettingEvent('SQL', Value);
end;

procedure TDXSSIPlus.SetResultFirst(value:SSITChangeSettingEvent);
begin
   fOnRESULTFIRST:=Value;
   AddChangeSettingEvent('FIRST', Value);
end;

procedure TDXSSIPlus.SetResultNext(value:SSITChangeSettingEvent);
begin
   fOnRESULTNEXT:=Value;
   AddChangeSettingEvent('NEXT', Value);
end;

procedure TDXSSIPlus.SetResultPrior(value:SSITChangeSettingEvent);
begin
   fOnRESULTPRIOR:=Value;
   AddChangeSettingEvent('PRIOR', Value);
end;

procedure TDXSSIPlus.SetResultLAST(value:SSITChangeSettingEvent);
begin
   fOnRESULTLAST:=Value;
   AddChangeSettingEvent('LAST', Value);
end;

procedure TDXSSIPlus.SetResultBOF(value:SSITStringEvent);
begin
   fOnRESULTBOF:=Value;
   AddStringEvent('ISBOF', Value);
end;

procedure TDXSSIPlus.SetResultEOF(value:SSITStringEvent);
begin
   fOnRESULTEOF:=Value;
   AddStringEvent('ISEOF', Value);
end;

procedure TDXSSIPlus.SetResultRowCount(value:SSITStringEvent);
begin
   fOnRESULTROWCOUNT:=Value;
   AddStringEvent('ROWCOUNT', Value);
end;

procedure TDXSSIPlus.SetResultRecord(value:SSITStringEvent);
begin
   fOnRESULTRECORD:=Value;
   AddStringEvent('RECORD', Value);
end;

procedure TDXSSIPlus.SetResultFieldCount(value:SSITStringEvent);
begin
   fOnRESULTFIELDCOUNT:=Value;
   AddStringEvent('ISFIELDCOUNT', Value);
end;

procedure TDXSSIPlus.SetDataSetEdit(value:SSITChangeSettingEvent);
begin
   fOnDatasetEdit:=Value;
   AddChangeSettingEvent('ROWEDIT', Value);
end;

procedure TDXSSIPlus.SetDataSetDelete(value:SSITChangeSettingEvent);
begin
   fOnDatasetDelete:=Value;
   AddChangeSettingEvent('ROWDELETE', Value);
end;

procedure TDXSSIPlus.SetDataSetInsert(value:SSITChangeSettingEvent);
begin
   fOnDatasetInsert:=Value;
   AddChangeSettingEvent('ROWINSERT', Value);
end;

procedure TDXSSIPlus.SetDataSetAppend(value:SSITChangeSettingEvent);
begin
   fOnDatasetAppend:=Value;
   AddChangeSettingEvent('ROWAPPEND', Value);
   AddChangeSettingEvent('ROWADD', Value);
end;

procedure TDXSSIPlus.SetFieldGetString(value:SSITStringEvent);
begin
   fOnFieldGetString:=Value;
   AddStringEvent('GETSTRING', Value);
end;

procedure TDXSSIPlus.SetFieldGetInteger(value:SSITStringEvent);
begin
   fOnFieldGetInteger:=Value;
   AddStringEvent('GETINTEGER', Value);
end;

procedure TDXSSIPlus.SetFieldGetDateTime(value:SSITStringEvent);
begin
   fOnFieldGetDatetime:=Value;
   AddStringEvent('GETDATETIME', Value);
   AddStringEvent('GETDATE', Value);
   AddStringEvent('GETTIME', Value);
end;

procedure TDXSSIPlus.SetFieldGetName(value:SSITStringEvent);
begin
   fOnFieldGetName:=Value;
   AddStringEvent('GETNAME', Value);
end;

procedure TDXSSIPlus.SetFieldGetBlob(value:SSITIncludeEvent);
begin
   fOnFieldGetBlob:=Value;
   AddIncludeEvent('GETBLOB', Value);
end;

procedure TDXSSIPlus.SetFieldPutString(value:SSITChangeSettingEvent);
begin
   fOnFieldPutString:=Value;
   AddChangeSettingEvent('PUTSTRING', Value);
end;

procedure TDXSSIPlus.SetFieldPutInteger(value:SSITChangeSettingEvent);
begin
   fOnFieldPutInteger:=Value;
   AddChangeSettingEvent('PUTINTEGER', Value);
end;

procedure TDXSSIPlus.SetFieldPutDateTime(value:SSITChangeSettingEvent);
begin
   fOnFieldPutDatetime:=Value;
   AddChangeSettingEvent('PUTDATETIME', Value);
   AddChangeSettingEvent('PUTDATE', Value);
   AddChangeSettingEvent('PUTTIME', Value);
end;

procedure TDXSSIPlus.SetFieldPutBlob(value:SSITChangeSettingEvent);
begin
   fOnFieldPutBlob:=Value;
   AddChangeSettingEvent('PUTBLOB', Value);
end;

function TDXSSIPlus.ProcessStream(const UniqueID:Cardinal;
   Stream:TStream):Boolean;
begin
   result:=inherited ProcessStream(uniqueid, stream);
end;

function TDXSSIPlus.ProcessLine(const UniqueID:Cardinal; const S:string):string;
begin
   Result:=inherited ProcessLine(UniqueID, S);
end;

end.

