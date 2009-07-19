unit DXDispatchDatasetServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDispatchDatasetServerCore
//       Author: G.E. Ozz Nixon Jr. (staff@bpdx.com)
// ========================================================================
// Source Owner: DX, Inc. 1995-2003
//    Copyright: All code is the property of DX, Inc. Licensed for
//               resell by Brain Patchwork DX (tm) and part of the
//               DX (r) product lines, which are (c) 1999-2003
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
// Code Version: (4th Generation Code)
// ========================================================================
//  Description: Custom DXServerCore for Police Dispatch Systems
// ========================================================================
// Transaction Flow:
// HELO USER_NAME PASSWORD
// CONNECT SERVER_INFO OPTIONAL_CONNECT_STRING
// SQL SQL_STATEMENT
// OPEN
// CLOSE
// FIELDS
// FETCH ROW_NUMBER
// FETCHROWS COUNT STARTING_ROW
// POST FIELD# DATA
// FIRST
// LAST
// DETAILSQL TABLENAME:SQL_STATEMENT
// DETAILOPEN TABLENAME:
// DETAILCLOSE TABLENAME:
// DETAILFIELDS TABLENAME:
// DETAILFETCH TABLENAME:ROW_NUMBER
//
// Results from Server:
// 200 ACK
// 205 <FIELDS STRUCTURE>
// 206 <FETCH STRUCTURE>
// 500 COMMAND ERROR
///////////////////////////////////////////////////////////////////////////////

uses
  DB,
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  DISPATCHTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  DISPATCHTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  DISPATCHTComplexEvent = procedure(ClientThread: TDXClientThread; Parm1, Parm2: string) of object;
  DISPATCHTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXDispatchDatasetServerCore = class(TDXServerCore)
  private
    fOnCommandHELO: DISPATCHTComplexEvent;
    fOnCommandCONNECT: DISPATCHTComplexEvent;
    fOnCommandSQL: DISPATCHTBasicEvent;
    fOnCommandSQLEXEC: DISPATCHTBasicEvent;
    fOnCommandACKD: DISPATCHTSimpleEvent;
    fOnCommandNACK: DISPATCHTSimpleEvent;
    fOnCommandOPEN: DISPATCHTSimpleEvent;
    fOnCommandCLOSE: DISPATCHTSimpleEvent;
    fOnCommandFIELDS: DISPATCHTSimpleEvent;
    fOnCommandFETCH: DISPATCHTBasicEvent;
    fOnCommandFETCHROWS: DISPATCHTComplexEvent;
    fOnCommandQUIT: DISPATCHTSimpleEvent;
    fOnCommandPOST: DISPATCHTComplexEvent;
    fOnCommandFIRST: DISPATCHTSimpleEvent;
    fOnCommandLAST: DISPATCHTSimpleEvent;
    fOnCommandDETAILSQL: DISPATCHTBasicEvent;
    fOnCommandDETAILOPEN: DISPATCHTBasicEvent;
    fOnCommandDETAILCLOSE: DISPATCHTBasicEvent;
    fOnCommandDETAILFIELDS: DISPATCHTBasicEvent;
    fOnCommandDETAILFETCH: DISPATCHTBasicEvent;
    fOnCommandOther: DISPATCHTOtherEvent; {COMMAND parameters...}
    fEventArray:TList;
    fiTimeout:Cardinal;
    fDataSet:TDataSet;
  protected
    Procedure SetOnCommandHELO(value:DISPATCHTComplexEvent);
    Procedure SetOnCommandCONNECT(value:DISPATCHTComplexEvent);
    Procedure SetOnCommandSQL(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandSQLEXEC(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandACKD(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandNACK(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandOPEN(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandCLOSE(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandFIELDS(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandFETCH(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandFETCHROWS(value:DISPATCHTComplexEvent);
    Procedure SetOnCommandQUIT(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandPOST(value:DISPATCHTComplexEvent);
    Procedure SetOnCommandFIRST(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandLAST(value:DISPATCHTSimpleEvent);
    Procedure SetOnCommandDETAILSQL(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandDETAILOPEN(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandDETAILCLOSE(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandDETAILFIELDS(value:DISPATCHTBasicEvent);
    Procedure SetOnCommandDETAILFETCH(value:DISPATCHTBasicEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Function  WriteResult(ClientThread:TDXClientThread;StatusCode:Integer;Rslt:String):Boolean;
    Function  Writeln(ClientThread:TDXClientThread;Rslt:String):Boolean;
    Procedure AddBasicEvent(Command:String;EventProc:DISPATCHTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:DISPATCHTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:DISPATCHTComplexEvent);
  published
    property DataSet:TDataSet read fDataSet
                              write fDataSet;
    property Timeout:Cardinal read fiTimeout
                           write fiTimeout;
    property OnCommandHELO: DISPATCHTComplexEvent read fOnCommandHELO
                                                  write SetOnCommandHELO;
    property OnCommandCONNECT: DISPATCHTComplexEvent read fOnCommandCONNECT
                                                     write SetOnCommandCONNECT;
    property OnCommandSQL: DISPATCHTBasicEvent read fOnCommandSQL
                                               write SetOnCommandSQL;
    property OnCommandSQLEXEC: DISPATCHTBasicEvent read fOnCommandSQLEXEC
                                                   write SetOnCommandSQLEXEC;
    property OnCommandACKD: DISPATCHTSimpleEvent read fOnCommandACKD
                                                 write SetOnCommandACKD;
    property OnCommandNACK: DISPATCHTSimpleEvent read fOnCommandNACK
                                                 write SetOnCommandNACK;
    property OnCommandOPEN: DISPATCHTSimpleEvent read fOnCommandOPEN
                                                 write SetOnCommandOPEN;
    property OnCommandCLOSE: DISPATCHTSimpleEvent read fOnCommandCLOSE
                                                 write SetOnCommandCLOSE;
    property OnCommandFIELDS: DISPATCHTSimpleEvent read fOnCommandFIELDS
                                                   write SetOnCommandFIELDS;
    property OnCommandFETCH: DISPATCHTBasicEvent read fOnCommandFETCH
                                                 write SetOnCommandFETCH;
    property OnCommandFETCHROWS: DISPATCHTComplexEvent read fOnCommandFETCHROWS
                                                       write SetOnCommandFETCHROWS;
    property OnCommandQUIT: DISPATCHTSimpleEvent read fOnCommandQUIT
                                                 write SetOnCommandQUIT;
    property OnCommandPOST: DISPATCHTComplexEvent read fOnCommandPOST
                                                  write SetOnCommandPOST;
    property OnCommandFIRST: DISPATCHTSimpleEvent read fOnCommandFIRST
                                                  write SetOnCommandFIRST;
    property OnCommandLAST: DISPATCHTSimpleEvent read fOnCommandLAST
                                                 write SetOnCommandLAST;
    property OnCommandDETAILSQL: DISPATCHTBasicEvent read fOnCommandDETAILSQL
                                                     write SetOnCommandDETAILSQL;
    property OnCommandDETAILOPEN: DISPATCHTBasicEvent read fOnCommandDETAILOPEN
                                                      write SetOnCommandDETAILOPEN;
    property OnCommandDETAILCLOSE: DISPATCHTBasicEvent read fOnCommandDETAILCLOSE
                                                       write SetOnCommandDETAILCLOSE;
    property OnCommandDETAILFIELDS: DISPATCHTBasicEvent read fOnCommandDETAILFIELDS
                                                        write SetOnCommandDETAILFIELDS;
    property OnCommandDETAILFETCH: DISPATCHTBasicEvent read fOnCommandDETAILFETCH
                                                       write SetOnCommandDETAILFETCH;
    property OnCommandOther: DISPATCHTOtherEvent read fOnCommandOther
                                                 write fOnCommandOther;
  end;

procedure Register;

implementation

Uses
   DXSock,
   DXString;

Type
  PDISPATCHBasicEvent=^TDISPATCHBasicEvent;
  TDISPATCHBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DISPATCHTBasicEvent;
  End;
  PDISPATCHSimpleEvent=^TDISPATCHSimpleEvent;
  TDISPATCHSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DISPATCHTSimpleEvent;
  End;
  PDISPATCHComplexEvent=^TDISPATCHComplexEvent;
  TDISPATCHComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DISPATCHTComplexEvent;
  End;

constructor TDXDispatchDatasetServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=15220;
   fEventArray:=TList.Create;
   fiTimeout:=120000;
   SocketOutputBufferSize:=bsfNormal;
end;

destructor TDXDispatchDatasetServerCore.Destroy;
Var
   PBasicEvent:PDISPATCHBasicEvent;
   PSimpleEvent:PDISPATCHSimpleEvent;
   PComplexEvent:PDISPATCHComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PDISPATCHBasicEvent(fEventArray[0]).Tag of
            1:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            2:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
            3:Begin
              PComplexEvent:=fEventArray[0];
              Dispose(PComplexEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
      fEventArray.Free;
      fEventArray:=Nil;
   End;
   inherited Destroy;
end;

Procedure TDXDispatchDatasetServerCore.AddBasicEvent(Command:String;EventProc:DISPATCHTBasicEvent);
Var
   PBasicEvent:PDISPATCHBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDISPATCHBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PDISPATCHBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PBasicEvent);
   PBasicEvent.Tag:=1;      // Denotes Event in fEventArray is a TBasicEvent!
   PBasicEvent.Command:=Command;
   PBasicEvent.EventProcedure:=EventProc;
   fEventArray.Add(PBasicEvent);
End;

Procedure TDXDispatchDatasetServerCore.AddSimpleEvent(Command:String;EventProc:DISPATCHTSimpleEvent);
Var
   PSimpleEvent:PDISPATCHSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDISPATCHSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PDISPATCHSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=2;      // Denotes Event in fEventArray is a TBasicEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXDispatchDatasetServerCore.AddComplexEvent(Command:String;EventProc:DISPATCHTComplexEvent);
Var
   PComplexEvent:PDISPATCHComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDISPATCHComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PDISPATCHComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PComplexEvent);
   PComplexEvent.Tag:=3;      // Denotes Event in fEventArray is a TBasicEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PComplexEvent);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandHELO(value:DISPATCHTComplexEvent);
Begin
   fOnCommandHELO:=Value;
   AddComplexEvent('HELO',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandCONNECT(value:DISPATCHTComplexEvent);
Begin
   fOnCommandCONNECT:=Value;
   AddComplexEvent('CONNECT',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandSQL(value:DISPATCHTBasicEvent);
Begin
   fOnCommandSQL:=Value;
   AddBasicEvent('SQL',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandSQLEXEC(value:DISPATCHTBasicEvent);
Begin
   fOnCommandSQLEXEC:=Value;
   AddBasicEvent('SQLEXEC',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandACKD(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandACKD:=Value;
   AddSimpleEvent('ACKD',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandNACK(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandNACK:=Value;
   AddSimpleEvent('NACK',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandOPEN(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandOPEN:=Value;
   AddSimpleEvent('OPEN',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandCLOSE(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandCLOSE:=Value;
   AddSimpleEvent('CLOSE',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandFIELDS(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandFIELDS:=Value;
   AddSimpleEvent('FIELDS',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandFETCH(value:DISPATCHTBasicEvent);
Begin
   fOnCommandFETCH:=Value;
   AddBasicEvent('FETCH',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandFETCHROWS(value:DISPATCHTComplexEvent);
Begin
   fOnCommandFETCHROWS:=Value;
   AddComplexEvent('FETCHROWS',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandQUIT(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandPOST(value:DISPATCHTComplexEvent);
Begin
   fOnCommandPOST:=Value;
   AddComplexEvent('POST',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandFIRST(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandFIRST:=Value;
   AddSimpleEvent('FIRST',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandLAST(value:DISPATCHTSimpleEvent);
Begin
   fOnCommandLAST:=Value;
   AddSimpleEvent('LAST',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandDETAILSQL(value:DISPATCHTBasicEvent);
Begin
   fOnCommandDETAILSQL:=Value;
   AddBasicEvent('DETAILSQL',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandDETAILOPEN(value:DISPATCHTBasicEvent);
Begin
   fOnCommandDETAILOPEN:=Value;
   AddBasicEvent('DETAILOPEN',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandDETAILCLOSE(value:DISPATCHTBasicEvent);
Begin
   fOnCommandDETAILCLOSE:=Value;
   AddBasicEvent('DETAILCLOSE',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandDETAILFIELDS(value:DISPATCHTBasicEvent);
Begin
   fOnCommandDETAILFIELDS:=Value;
   AddBasicEvent('DETAILFIELDS',Value);
End;

Procedure TDXDispatchDatasetServerCore.SetOnCommandDETAILFETCH(value:DISPATCHTBasicEvent);
Begin
   fOnCommandDETAILFETCH:=Value;
   AddBasicEvent('DETAILFETCH',Value);
End;

procedure TDXDispatchDatasetServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, s2, sCmd: string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;
  StartTime:Comp;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      StartTime:=TimeCounter+fiTimeout;
      while ValidSocket do begin
         if fbForceAbort then Break;
         S:=ReadWithSize;
         If Not ValidSocket then Break;
         If S='' then Continue;
         If StartTime<TimeCounter then break; // timeout, no activity.
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(FetchByChar(S,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PDISPATCHBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PDISPATCHBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PDISPATCHBasicEvent(fEventArray[Loop]).EventProcedure) then
                       DISPATCHTBasicEvent(PDISPATCHBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PDISPATCHSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       DISPATCHTSimpleEvent(PDISPATCHSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PDISPATCHComplexEvent(fEventArray[Loop]).EventProcedure) then Begin
                       S2:=FetchByChar(S,#32,False);
                       DISPATCHTComplexEvent(PDISPATCHComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S2,S);
                  End;
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End; {while}
         If sCMD='QUIT' then Break; {terminate}
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,S,WasHandled);
         end;
         if not WasHandled then
            WriteWithSize('500 COMMAND ERROR - command not recognized (' + CMD + ')');
         StartTime:=TimeCounter+fiTimeout;
      end; {while}
   end; {with}
end; {doExecute}

Function TDXDispatchDatasetServerCore.WriteResult(ClientThread:TDXClientThread;StatusCode:Integer;Rslt:String):Boolean;
Begin
   Result:=ClientThread.Socket.WriteWithSize(IntToCommaStr(StatusCode)+#32+Rslt);
End;

Function TDXDispatchDatasetServerCore.Writeln(ClientThread:TDXClientThread;Rslt:String):Boolean;
Begin
   Result:=ClientThread.Socket.WriteWithSize(Rslt);
End;

procedure Register;
begin
   RegisterComponents('BPDX MultiTier', [TDXDispatchDatasetServerCore]);
end;

end.

