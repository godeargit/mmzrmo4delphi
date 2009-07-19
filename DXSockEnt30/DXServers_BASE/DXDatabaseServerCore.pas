unit DXDatabaseServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDatabaseServerCore
//       Author: G.E. Ozz Nixon Jr. (onixon@dxsock.com)
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
//  Description: *Example* One way you could implement a Database Server.
// ========================================================================
// This allows you to develop TDatabase Servers - this is a proprietary
// solutions from Brain Patchwork DX, LLC. It has been designed to allow you
// to make your own hooks at the server for the database connectivity. Later
// we will develop a TDatabase Client and a common TDatabase interface. The
// goal of the interface will be to make this component work seamlessly with
// any Data-Aware component that knows how to talk to Borland's TDatabase.
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
{$IFDEF VER100}
  Longword=Cardinal;
{$ENDIF}
  DatabaseTBasicEvent = procedure(ClientThread: TDXClientThread) of object;
  DatabaseTSimpleEvent = procedure(ClientThread: TDXClientThread;Parm1:String) of object;
  DatabaseTComplexEvent = procedure(ClientThread: TDXClientThread;Parm1,Parm2:String) of object;
  DatabaseTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

// specifications:
//
// AUTH UserName Password
//  Verifies username and password, can be encoded if you implement your own encoding!
//
// USER UserName
//  Part 1 of authentication, when doing in multiple parts versus AUTH is both parts
//
// PASS Password
//  Part 2 of authentication, when doing in multiple parts versus AUTH is both parts
//
// CREATEDATASET MAGIC_WORD
// magic word is up to you to implement, the CreateDataset event lets your server
// know which database the client wishes to interact with. The magic_word can be
// a database name, alias, or if you want a connection string for another database
// server.
//
// DISPOSEDATASET MAGIC_WORD
// magic word is up to you to implement, the DisposeDataset event lets your server
// know which database instance the client is finished working with.
//
// Do not confuse OPEN/CLOSE with CREATEDATASET/DISPOSEDATASET. Your have to issue
// the CREATEDATASET before you can open/close the database, and DISPOSEDATASET
// is used usually when the client is disconnecting. Make sure you design your
// OnNewConnect to also check if your instance has been disposed or not, incase
// the client dropped carrier!!!
//
// OPEN
// opens the currently selected database
//
// CLOSE
// closes the currently selected database
//
// SETACTIVE TRUE|FALSE
// issues internally OPEN when parm1=TRUE or CLOSE when parm1=FALSE
//
// FIRST
// tells the open database to seek to the first row.
//
// NEXT
// tells the open database to seek to the next available row.
//
// PRIOR
// tells the open database to seek to the previous available row.
//
// LAST
// tells the open database to seek to the last row.
//
// MOVEBY +#|-#
// tells the open database to seek +# of rows or -# rows.
//
// ISBOF
// asks the open database if it is on the first row (beginning of file)
//
// ISEOF
// asks the open database if it is on the last row (end of file)
//
// GETFIELDLIST
// asks the open database to turned the field list sturcture.
//
// LOCATECASEINSENSITIVE TRUE|FALSE
// enables or disables Case Sensitive flag for searching.
//
// LOCATEPARTIALKEY TRUE|FALSE
// enables or disables PARTIAL COMPARES on "key" field.
//
// LOCATENOOPTIONS
// short-circuit - sets both of the previous to FALSE.
//
// LOCATEBOTHOPTIONS
// short-circuit - sets noth of the previous to TRUE.
//
// LOCATE "KeyField(s)" "Keyvalue(s)"
// performs the low-level "Locate" command.
//
// FINDFIRST
// asks the open database to seek to the first available row.
//
// FINDLAST
// asks the open database to seek to the last available row.
//
// FINDNEXT
// asks the open database to seek to the next available row.
//
// FINDPRIOR
// asks the open database to seek to the previous available row.
//
// FINDFIELD
// asks the open database to return the specified field if it exists.
//
// FOUND
// returns true if any of the above FIND commands were successful. You should
// use the result from the FIND command, but this is available incase you
// forgot to save the result in your code. And asks the server to return
// the status of the last FIND.
//
// CANMODIFY
// asks the open database if it can be modified.
//
// FIELDCOUNT
// asks the open database for the number of available fields (0=none, 1 based)
//
// RECORDCOUNT
// asks the open database for the number of available rows (0=none, 1 based)
//
// RECORDSIZE
// asks the open database for the size of each row.
//
// MODIFIED
// asks the open database if the current row has changed, and requires a POST
// command, or CANCEL command.
//
// APPEND
// asks the open database to create a new row. Unindexed files this is faster
// than INSERT.
//
// INSERT
// asks the open database to create a new row. Unindexed files *usually*
// attempt to physically insert the new row at the current cursor position!
//
// EDIT
// asks the open database to put the current row into EDIT mode.
//
// DELETE
// asks the open database to delete the current row.
//
// POST
// asks the open database to commit any changes that have not been committed.
//
// CANCEL
// asks the open database to rollback any changes that have not been committed.
//
// REFRESH
// Refresh ensures that an application has the latest data from a database.
// For example, when an application turns off filtering for a dataset, it
// should immediately call Refresh to display all records in the dataset,
// not just those that used to meet the filter condition.
//
// FIELDBYNAME "FieldName" "AsType"
// asks the open database to return the specified field, as the specified
// field type. This is probably the most "tricky" implementation in the
// database server.
//
// 2001 additions:
// ISEMPTY
// basically returns true if RecordCount=0
//

  TDXDatabaseServerCore = class(TDXServerCore)
  private
  // CONNECTION COMMANDS
    fOnCommandUSER: DatabaseTSimpleEvent;
    fOnCommandPASS: DatabaseTSimpleEvent;
    fOnCommandAUTH: DatabaseTComplexEvent;
  // DATASET INFORMATION COMMANDS
    fOnCommandSETACTIVE: DatabaseTSimpleEvent;
    fOnCommandISBOF: DatabaseTBasicEvent;
    fOnCommandISEOF: DatabaseTBasicEvent;
    fOnCommandISEMPTY: DatabaseTBasicEvent;
    fOnCommandCANMODIFY: DatabaseTBasicEvent;
    fOnCommandFIELDCOUNT: DatabaseTBasicEvent;
    fOnCommandRECORDCOUNT: DatabaseTBasicEvent;
    fOnCommandRECORDSIZE: DatabaseTBasicEvent;
    fOnCommandFOUND: DatabaseTBasicEvent;
    fOnCommandMODIFIED: DatabaseTBasicEvent;
  // DATASET COMMANDS
    fOnCommandCREATEDATASET: DatabaseTSimpleEvent;
    fOnCommandDISPOSEDATASET: DatabaseTSimpleEvent;
    fOnCommandISLINKEDTODATASET: DatabaseTSimpleEvent;
  // DATASET NAVIGATION COMMANDS
    fOnCommandOPEN: DatabaseTBasicEvent;
    fOnCommandCLOSE: DatabaseTBasicEvent;
    fOnCommandFIRST: DatabaseTBasicEvent;
    fOnCommandNEXT: DatabaseTBasicEvent;
    fOnCommandPRIOR: DatabaseTBasicEvent;
    fOnCommandLAST: DatabaseTBasicEvent;
    fOnCommandMOVEBY: DatabaseTSimpleEvent;
    fOnCommandFINDFIRST: DatabaseTBasicEvent;
    fOnCommandFINDLAST: DatabaseTBasicEvent;
    fOnCommandFINDNEXT: DatabaseTBasicEvent;
    fOnCommandFINDPRIOR: DatabaseTBasicEvent;
    fOnCommandFINDFIELD: DatabaseTSimpleEvent;
    fOnCommandGETFIELDLIST: DatabaseTBasicEvent;
    fOnCommandLOCATECaseInsensitive: DatabaseTSimpleEvent;
    fOnCommandLOCATEPartialKey: DatabaseTSimpleEvent;
    fOnCommandLOCATENoOptions: DatabaseTBasicEvent;
    fOnCommandLOCATEBothOptions: DatabaseTBasicEvent;
    fOnCommandLOCATE: DatabaseTComplexEvent;
  // DATASET DATA MANIPULATION COMMANDS
    fOnCommandAPPEND: DatabaseTBasicEvent;
    fOnCommandINSERT: DatabaseTBasicEvent;
    fOnCommandEDIT: DatabaseTBasicEvent;
    fOnCommandDELETE: DatabaseTBasicEvent;
    fOnCommandPOST: DatabaseTBasicEvent;
    fOnCommandCANCEL: DatabaseTBasicEvent;
    fOnCommandREFRESH: DatabaseTBasicEvent;
  // DATASET DATA COMMANDS
    fOnCommandFIELDBYNAME: DatabaseTComplexEvent;
    fOnCommandOther: DatabaseTOtherEvent; //COMMAND parameters...
  protected
    Procedure SetOnCommandUSER(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandPASS(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandAUTH(value:DatabaseTComplexEvent);
    Procedure SetOnCommandSETACTIVE(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandISBOF(value:DatabaseTBasicEvent);
    Procedure SetOnCommandISEOF(value:DatabaseTBasicEvent);
    Procedure SetOnCommandISEMPTY(value:DatabaseTBasicEvent);
    Procedure SetOnCommandCANMODIFY(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFIELDCOUNT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandRECORDCOUNT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandRECORDSIZE(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFOUND(value:DatabaseTBasicEvent);
    Procedure SetOnCommandMODIFIED(value:DatabaseTBasicEvent);
    Procedure SetOnCommandCREATEDATASET(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandDISPOSEDATASET(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandOPEN(value:DatabaseTBasicEvent);
    Procedure SetOnCommandCLOSE(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFIRST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandNEXT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandPRIOR(value:DatabaseTBasicEvent);
    Procedure SetOnCommandLAST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandMOVEBY(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandFINDFIRST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFINDLAST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFINDNEXT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFINDPRIOR(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFINDFIELD(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandGETFIELDLIST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandLOCATECASEINSENSITIVE(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandLOCATEPARTIALKEY(value:DatabaseTSimpleEvent);
    Procedure SetOnCommandLOCATENOOPTIONS(value:DatabaseTBasicEvent);
    Procedure SetOnCommandLOCATEBOTHOPTIONS(value:DatabaseTBasicEvent);
    Procedure SetOnCommandLOCATE(value:DatabaseTComplexEvent);
    Procedure SetOnCommandAPPEND(value:DatabaseTBasicEvent);
    Procedure SetOnCommandINSERT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandEDIT(value:DatabaseTBasicEvent);
    Procedure SetOnCommandDELETE(value:DatabaseTBasicEvent);
    Procedure SetOnCommandPOST(value:DatabaseTBasicEvent);
    Procedure SetOnCommandCANCEL(value:DatabaseTBasicEvent);
    Procedure SetOnCommandREFRESH(value:DatabaseTBasicEvent);
    Procedure SetOnCommandFIELDBYNAME(value:DatabaseTComplexEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddSimpleEvent(Command:String;EventProc:DatabaseTSimpleEvent);
    Procedure AddBasicEvent(Command:String;EventProc:DatabaseTBasicEvent);
    Procedure AddComplexEvent(Command:String;EventProc:DatabaseTComplexEvent);
  published
    property OnCommandUSER: DatabaseTSimpleEvent read fOnCommandUSER
                                                 write SetOnCommandUSER;
    property OnCommandPASS: DatabaseTSimpleEvent read fOnCommandPASS
                                                 write SetOnCommandPASS;
    property OnCommandAUTH: DatabaseTComplexEvent read fOnCommandAUTH
                                                  write SetOnCommandAUTH;
    property OnCommandSETACTIVE: DatabaseTSimpleEvent read fOnCommandSETACTIVE
                                                      write SetOnCommandSETACTIVE;
    property OnCommandISBOF: DatabaseTBasicEvent read fOnCommandISBOF
                                                 write SetOnCommandISBOF;
    property OnCommandISEOF: DatabaseTBasicEvent read fOnCommandISEOF
                                                 write SetOnCommandISEOF;
    property OnCommandISEMPTY: DatabaseTBasicEvent read fOnCommandISEMPTY
                                                   write SetOnCommandISEMPTY;
    property OnCommandCANMODIFY: DatabaseTBasicEvent read fOnCommandCANMODIFY
                                                     write SetOnCommandCANMODIFY;
    property OnCommandFIELDCOUNT: DatabaseTBasicEvent read fOnCommandFIELDCOUNT
                                                      write SetOnCommandFIELDCOUNT;
    property OnCommandRECORDCOUNT: DatabaseTBasicEvent read fOnCommandRECORDCOUNT
                                                       write SetOnCommandRECORDCOUNT;
    property OnCommandRECORDSIZE: DatabaseTBasicEvent read fOnCommandRECORDSIZE
                                                      write SetOnCommandRECORDSIZE;
    property OnCommandFOUND: DatabaseTBasicEvent read fOnCommandFOUND
                                                 write SetOnCommandFOUND;
    property OnCommandMODIFIED: DatabaseTBasicEvent read fOnCommandMODIFIED
                                                    write SetOnCommandMODIFIED;
    property OnCommandCREATEDATASET: DatabaseTSimpleEvent read fOnCommandCREATEDATASET
                                                          write SetOnCommandCREATEDATASET;
    property OnCommandDISPOSEDATASET: DatabaseTSimpleEvent read fOnCommandDISPOSEDATASET
                                                           write SetOnCommandDISPOSEDATASET;
    property OnCommandOPEN: DatabaseTBasicEvent read fOnCommandOPEN
                                                write SetOnCommandOPEN;
    property OnCommandCLOSE: DatabaseTBasicEvent read fOnCommandCLOSE
                                                 write SetOnCommandCLOSE;
    property OnCommandFIRST: DatabaseTBasicEvent read fOnCommandFIRST
                                                 write SetOnCommandFIRST;
    property OnCommandNEXT: DatabaseTBasicEvent read fOnCommandNEXT
                                                write SetOnCommandNEXT;
    property OnCommandPRIOR: DatabaseTBasicEvent read fOnCommandPRIOR
                                                 write SetOnCommandPRIOR;
    property OnCommandLAST: DatabaseTBasicEvent read fOnCommandLAST
                                                write SetOnCommandLAST;
    property OnCommandMOVEBY: DatabaseTSimpleEvent read fOnCommandMOVEBY
                                                   write SetOnCommandMOVEBY;
    property OnCommandFINDFIRST: DatabaseTBasicEvent read fOnCommandFINDFIRST
                                                     write SetOnCommandFINDFIRST;
    property OnCommandFINDLAST: DatabaseTBasicEvent read fOnCommandFINDLAST
                                                    write SetOnCommandFINDLAST;
    property OnCommandFINDNEXT: DatabaseTBasicEvent read fOnCommandFINDNEXT
                                                    write SetOnCommandFINDNEXT;
    property OnCommandFINDPRIOR: DatabaseTBasicEvent read fOnCommandFINDPRIOR
                                                     write SetOnCommandFINDPRIOR;
    property OnCommandFINDFIELD: DatabaseTSimpleEvent read fOnCommandFINDFIELD
                                                      write SetOnCommandFINDFIELD;
    property OnCommandGETFIELDLIST: DatabaseTBasicEvent read fOnCommandGETFIELDLIST
                                                        write SetOnCommandGETFIELDLIST;
    property OnCommandLOCATECaseInsensitive: DatabaseTSimpleEvent read fOnCommandLOCATECaseInsensitive
                                                                  write SetOnCommandLOCATECaseInsensitive;
    property OnCommandLOCATEPartialKey: DatabaseTSimpleEvent read fOnCommandLOCATEPartialKey
                                                             write SetOnCommandLOCATEPartialKey;
    property OnCommandLOCATENoOptions: DatabaseTBasicEvent read fOnCommandLOCATENoOptions
                                                           write SetOnCommandLOCATENoOptions;
    property OnCommandLOCATEBothOptions: DatabaseTBasicEvent read fOnCommandLOCATEBothOptions
                                                             write SetOnCommandLOCATEBothOptions;
    property OnCommandLOCATE: DatabaseTComplexEvent read fOnCommandLOCATE
                                                    write SetOnCommandLOCATE;
    property OnCommandAPPEND: DatabaseTBasicEvent read fOnCommandAPPEND
                                                  write SetOnCommandAPPEND;
    property OnCommandINSERT: DatabaseTBasicEvent read fOnCommandINSERT
                                                  write SetOnCommandINSERT;
    property OnCommandEDIT: DatabaseTBasicEvent read fOnCommandEDIT
                                                write SetOnCommandEDIT;
    property OnCommandDELETE: DatabaseTBasicEvent read fOnCommandDELETE
                                                  write SetOnCommandDELETE;
    property OnCommandPOST: DatabaseTBasicEvent read fOnCommandPOST
                                                write SetOnCommandPOST;
    property OnCommandCANCEL: DatabaseTBasicEvent read fOnCommandCANCEL
                                                  write SetOnCommandCANCEL;
    property OnCommandREFRESH: DatabaseTBasicEvent read fOnCommandREFRESH
                                                   write SetOnCommandREFRESH;
    property OnCommandFIELDBYNAME: DatabaseTComplexEvent read fOnCommandFIELDBYNAME
                                                         write SetOnCommandFIELDBYNAME;
    property OnCommandOther: DatabaseTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXString,
   DXSock;

Type
  PDatabaseSimpleEvent=^TDatabaseSimpleEvent;
  TDatabaseSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DatabaseTSimpleEvent;
  End;
  PDatabaseBasicEvent=^TDatabaseBasicEvent;
  TDatabaseBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DatabaseTBasicEvent;
  End;
  PDatabaseComplexEvent=^TDatabaseComplexEvent;
  TDatabaseComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DatabaseTComplexEvent;
  End;

constructor TDXDatabaseServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=5335;
   ProtocolToBind:=wpTCPOnly; // !!!! no UDP !!!!
end;

destructor TDXDatabaseServerCore.Destroy;
Var
   PSimpleEvent:PDatabaseSimpleEvent;
   PBasicEvent:PDatabaseBasicEvent;
   PComplexEvent:PDatabaseComplexEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PDatabaseSimpleEvent(fEventArray[0]).Tag of
            1:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
            2:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            3:Begin
              PComplexEvent:=fEventArray[0];
              Dispose(PComplexEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXDatabaseServerCore.AddSimpleEvent(Command:String;EventProc:DatabaseTSimpleEvent);
Var
   PSimpleEvent:PDatabaseSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDatabaseSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PDatabaseSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=1;      // Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXDatabaseServerCore.AddBasicEvent(Command:String;EventProc:DatabaseTBasicEvent);
Var
   PBasicEvent:PDatabaseBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDatabaseBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PDatabaseBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PBasicEvent);
   PBasicEvent.Tag:=2;      // Denotes Event in fEventArray is a TBasicEvent!
   PBasicEvent.Command:=Command;
   PBasicEvent.EventProcedure:=EventProc;
   fEventArray.Add(PBasicEvent);
End;

Procedure TDXDatabaseServerCore.AddComplexEvent(Command:String;EventProc:DatabaseTComplexEvent);
Var
   PComplexEvent:PDatabaseComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDatabaseComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PDatabaseComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PComplexEvent);
   PComplexEvent.Tag:=2;      // Denotes Event in fEventArray is a TComplexEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PComplexEvent);
End;

Procedure TDXDatabaseServerCore.SetOnCommandUSER(value:DatabaseTSimpleEvent);
Begin
   fOnCommandUSER:=Value;
   AddSimpleEvent('USER',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandPASS(value:DatabaseTSimpleEvent);
Begin
   fOnCommandPASS:=Value;
   AddSimpleEvent('PASS',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandAUTH(value:DatabaseTComplexEvent);
Begin
   fOnCommandAUTH:=Value;
   AddComplexEvent('AUTH',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandSETACTIVE(value:DatabaseTSimpleEvent);
Begin
   fOnCommandSETACTIVE:=Value;
   AddSimpleEvent('SETACTIVE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandISBOF(value:DatabaseTBasicEvent);
Begin
   fOnCommandISBOF:=Value;
   AddBasicEvent('ISBOF',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandISEOF(value:DatabaseTBasicEvent);
Begin
   fOnCommandISEOF:=Value;
   AddBasicEvent('ISEOF',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandISEMPTY(value:DatabaseTBasicEvent);
Begin
   fOnCommandISEMPTY:=Value;
   AddBasicEvent('ISEMPTY',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandCANMODIFY(value:DatabaseTBasicEvent);
Begin
   fOnCommandCANMODIFY:=Value;
   AddBasicEvent('CANMODIFY',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFIELDCOUNT(value:DatabaseTBasicEvent);
Begin
   fOnCommandFIELDCOUNT:=Value;
   AddBasicEvent('FIELDCOUNT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandRECORDCOUNT(value:DatabaseTBasicEvent);
Begin
   fOnCommandRECORDCOUNT:=Value;
   AddBasicEvent('RECORDCOUNT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandRECORDSIZE(value:DatabaseTBasicEvent);
Begin
   fOnCommandRECORDSIZE:=Value;
   AddBasicEvent('RECORDSIZE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFOUND(value:DatabaseTBasicEvent);
Begin
   fOnCommandFOUND:=Value;
   AddBasicEvent('FOUND',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandMODIFIED(value:DatabaseTBasicEvent);
Begin
   fOnCommandMODIFIED:=Value;
   AddBasicEvent('MODIFIED',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandCREATEDATASET(value:DatabaseTSimpleEvent);
Begin
   fOnCommandCREATEDATASET:=Value;
   AddSimpleEvent('CREATEDATASET',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandDISPOSEDATASET(value:DatabaseTSimpleEvent);
Begin
   fOnCommandDISPOSEDATASET:=Value;
   AddSimpleEvent('DISPOSEDATASET',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandOPEN(value:DatabaseTBasicEvent);
Begin
   fOnCommandOPEN:=Value;
   AddBasicEvent('OPEN',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandCLOSE(value:DatabaseTBasicEvent);
Begin
   fOnCommandCLOSE:=Value;
   AddBasicEvent('CLOSE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFIRST(value:DatabaseTBasicEvent);
Begin
   fOnCommandFIRST:=Value;
   AddBasicEvent('FIRST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandNEXT(value:DatabaseTBasicEvent);
Begin
   fOnCommandNEXT:=Value;
   AddBasicEvent('NEXT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandPRIOR(value:DatabaseTBasicEvent);
Begin
   fOnCommandPRIOR:=Value;
   AddBasicEvent('PRIOR',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLAST(value:DatabaseTBasicEvent);
Begin
   fOnCommandLAST:=Value;
   AddBasicEvent('LAST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandMOVEBY(value:DatabaseTSimpleEvent);
Begin
   fOnCommandMOVEBY:=Value;
   AddSimpleEvent('MOVEBY',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFINDFIRST(value:DatabaseTBasicEvent);
Begin
   fOnCommandFINDFIRST:=Value;
   AddBasicEvent('FINDFIRST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFINDLAST(value:DatabaseTBasicEvent);
Begin
   fOnCommandFINDLAST:=Value;
   AddBasicEvent('FINDLAST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFINDNEXT(value:DatabaseTBasicEvent);
Begin
   fOnCommandFINDNEXT:=Value;
   AddBasicEvent('FINDNEXT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFINDPRIOR(value:DatabaseTBasicEvent);
Begin
   fOnCommandFINDPRIOR:=Value;
   AddBasicEvent('FINDPRIOR',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFINDFIELD(value:DatabaseTSimpleEvent);
Begin
   fOnCommandFINDFIELD:=Value;
   AddSimpleEvent('FINDFIELD',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandGETFIELDLIST(value:DatabaseTBasicEvent);
Begin
   fOnCommandGETFIELDLIST:=Value;
   AddBasicEvent('GETFIELDLIST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLOCATECASEINSENSITIVE(value:DatabaseTSimpleEvent);
Begin
   fOnCommandLOCATECaseInsensitive:=Value;
   AddSimpleEvent('LOCATECASEINSENSITIVE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLOCATEPARTIALKEY(value:DatabaseTSimpleEvent);
Begin
   fOnCommandLOCATEPARTIALKEY:=Value;
   AddSimpleEvent('LOCATEPARTIALKEY',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLOCATENOOPTIONS(value:DatabaseTBasicEvent);
Begin
   fOnCommandLOCATENOOPTIONS:=Value;
   AddBasicEvent('LOCATENOOPTIONS',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLOCATEBOTHOPTIONS(value:DatabaseTBasicEvent);
Begin
   fOnCommandLOCATEBOTHOPTIONS:=Value;
   AddBasicEvent('LOCATEBOTHOPTIONS',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandLOCATE(value:DatabaseTComplexEvent);
Begin
   fOnCommandLOCATE:=Value;
   AddComplexEvent('LOCATE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandAPPEND(value:DatabaseTBasicEvent);
Begin
   fOnCommandAPPEND:=Value;
   AddBasicEvent('APPEND',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandINSERT(value:DatabaseTBasicEvent);
Begin
   fOnCommandINSERT:=Value;
   AddBasicEvent('INSERT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandEDIT(value:DatabaseTBasicEvent);
Begin
   fOnCommandEDIT:=Value;
   AddBasicEvent('EDIT',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandDELETE(value:DatabaseTBasicEvent);
Begin
   fOnCommandDELETE:=Value;
   AddBasicEvent('DELETE',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandPOST(value:DatabaseTBasicEvent);
Begin
   fOnCommandPOST:=Value;
   AddBasicEvent('POST',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandCANCEL(value:DatabaseTBasicEvent);
Begin
   fOnCommandCANCEL:=Value;
   AddBasicEvent('CANCEL',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandREFRESH(value:DatabaseTBasicEvent);
Begin
   fOnCommandREFRESH:=Value;
   AddBasicEvent('REFRESH',Value);
End;

Procedure TDXDatabaseServerCore.SetOnCommandFIELDBYNAME(value:DatabaseTComplexEvent);
Begin
   fOnCommandFIELDBYNAME:=Value;
   AddComplexEvent('FIELDBYNAME',Value);
End;

procedure TDXDatabaseServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, sCmd: string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;

  procedure NotHandled(CMD: string);
  Var
     Ws:String;

  begin
     Ws:='500 command not recognized (' + CMD + ')';
     ClientThread.Socket.Writeln(Ws);
  end;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      While Connected do Begin
         s:=ReadLn(Timeout);
         If LastReadTimeout or (Not ValidSocket) or (fbForceAbort) then Exit;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(Outdata) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(s,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PDatabaseSimpleEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PDatabaseSimpleEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PDatabaseSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       DatabaseTSimpleEvent(PDatabaseSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread,s);
                  2:if Assigned(PDatabaseBasicEvent(fEventArray[Loop]).EventProcedure) then
                       DatabaseTBasicEvent(PDatabaseBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PDatabaseComplexEvent(fEventArray[Loop]).EventProcedure) then
                       DatabaseTComplexEvent(PDatabaseComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,FetchByChar(S,#32,False),S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then NotHandled(sCmd);
      end;
   end;
end;

end.

