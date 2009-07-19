unit DXFTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXFTPServerCore
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
//  Description: implements FTP (File Transfer Protocol)
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  FTPTSimpleEvent  = procedure(ClientThread:TDXClientThread) of object;
  FTPTBasicEvent   = procedure(ClientThread:TDXClientThread;Parm:string) of object;
  FTPTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;
  FTPTPassEvent    = procedure(ClientThread:TDXClientThread;Parm:string;Var SuccessfulLogin:Boolean) of object;
  FTPTOtherEvent   = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; var Handled: Boolean) of object;

  TDXFTPServerCore = class(TDXServerCore)
  private
    fOnCommandUSER: FTPTBasicEvent;
    fOnCommandPASS: FTPTPassEvent;
    fOnCommandACCT: FTPTBasicEvent;
    fOnCommandCWD:  FTPTBasicEvent;
    fOnCommandCDUP: FTPTSimpleEvent;
    fOnCommandSMNT: FTPTBasicEvent;
    fOnCommandQUIT: FTPTSimpleEvent;
    fOnCommandREIN: FTPTSimpleEvent;
    fOnCommandPORT: FTPTBasicEvent;
    fOnCommandPASV: FTPTSimpleEvent;
    fOnCommandTYPE: FTPTBasicEvent;
    fOnCommandSTRU: FTPTBasicEvent;
    fOnCommandMODE: FTPTBasicEvent;
    fOnCommandRETR: FTPTBasicEvent;
    fOnCommandSTOR: FTPTBasicEvent;
    fOnCommandSTOU: FTPTSimpleEvent;
    fOnCommandAPPE: FTPTBasicEvent;
    fOnCommandALLO: FTPTComplexEvent;
    fOnCommandREST: FTPTBasicEvent;
    fOnCommandRNFR: FTPTBasicEvent;
    fOnCommandRNTO: FTPTBasicEvent;
    fOnCommandABOR: FTPTSimpleEvent;
    fOnCommandDELE: FTPTBasicEvent;
    fOnCommandRMD:  FTPTBasicEvent;
    fOnCommandMKD:  FTPTBasicEvent;
    fOnCommandPWD:  FTPTSimpleEvent;
    fOnCommandLIST: FTPTBasicEvent;
    fOnCommandNLST: FTPTBasicEvent;
    fOnCommandSITE: FTPTBasicEvent;
    fOnCommandSIZE: FTPTBasicEvent; // 2.0
    fOnCommandSYST: FTPTSimpleEvent;
    fOnCommandSTAT: FTPTBasicEvent;
    fOnCommandHELP: FTPTBasicEvent;
    fOnCommandNOOP: FTPTSimpleEvent;
    fOnCommandOther:FTPTOtherEvent;
  protected
    Procedure SetOnCommandUSER(value:FTPTBasicEvent);
    Procedure SetOnCommandPASS(value:FTPTPassEvent);
    Procedure SetOnCommandACCT(value:FTPTBasicEvent);
    Procedure SetOnCommandCWD(value:FTPTBasicEvent);
    Procedure SetOnCommandCDUP(value:FTPTSimpleEvent);
    Procedure SetOnCommandSMNT(value:FTPTBasicEvent);
    Procedure SetOnCommandQUIT(value:FTPTSimpleEvent);
    Procedure SetOnCommandREIN(value:FTPTSimpleEvent);
    Procedure SetOnCommandPORT(value:FTPTBasicEvent);
    Procedure SetOnCommandPASV(value:FTPTSimpleEvent);
    Procedure SetOnCommandTYPE(value:FTPTBasicEvent);
    Procedure SetOnCommandSTRU(value:FTPTBasicEvent);
    Procedure SetOnCommandMODE(value:FTPTBasicEvent);
    Procedure SetOnCommandRETR(value:FTPTBasicEvent);
    Procedure SetOnCommandSTOR(value:FTPTBasicEvent);
    Procedure SetOnCommandSTOU(value:FTPTSimpleEvent);
    Procedure SetOnCommandAPPE(value:FTPTBasicEvent);
    Procedure SetOnCommandALLO(value:FTPTComplexEvent);
    Procedure SetOnCommandREST(value:FTPTBasicEvent);
    Procedure SetOnCommandRNFR(value:FTPTBasicEvent);
    Procedure SetOnCommandRNTO(value:FTPTBasicEvent);
    Procedure SetOnCommandABOR(value:FTPTSimpleEvent);
    Procedure SetOnCommandDELE(value:FTPTBasicEvent);
    Procedure SetOnCommandRMD(value:FTPTBasicEvent);
    Procedure SetOnCommandMKD(value:FTPTBasicEvent);
    Procedure SetOnCommandPWD(value:FTPTSimpleEvent);
    Procedure SetOnCommandLIST(value:FTPTBasicEvent);
    Procedure SetOnCommandNLST(value:FTPTBasicEvent);
    Procedure SetOnCommandSITE(value:FTPTBasicEvent);
    Procedure SetOnCommandSIZE(value:FTPTBasicEvent);
    Procedure SetOnCommandSYST(value:FTPTSimpleEvent);
    Procedure SetOnCommandSTAT(value:FTPTBasicEvent);
    Procedure SetOnCommandHELP(value:FTPTBasicEvent);
    Procedure SetOnCommandNOOP(value:FTPTSimpleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:TStrings);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:TStrings);
    procedure ProcessSession(ClientThread:TDXClientThread;MOTD:TStrings);
    Procedure AddBasicEvent(Command:String;EventProc:FTPTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:FTPTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:FTPTComplexEvent);
    Procedure AddPassEvent(Command:String;EventProc:FTPTPassEvent);
  published
    property OnCommandUSER: FTPTBasicEvent read fOnCommandUSER
                                           write SetOnCommandUSER;
    property OnCommandPASS: FTPTPassEvent read fOnCommandPASS
                                          write SetOnCommandPASS;
    property OnCommandACCT: FTPTBasicEvent read fOnCommandACCT
                                           write SetOnCommandACCT;
    property OnCommandCWD: FTPTBasicEvent read fOnCommandCWD
                                          write SetOnCommandCWD;
    property OnCommandCDUP: FTPTSimpleEvent read fOnCommandCDUP
                                            write SetOnCommandCDUP;
    property OnCommandSMNT: FTPTBasicEvent read fOnCommandSMNT
                                           write SetOnCommandSMNT;
    property OnCommandQUIT: FTPTSimpleEvent read fOnCommandQUIT
                                            write SetOnCommandQUIT;
    property OnCommandREIN: FTPTSimpleEvent read fOnCommandREIN
                                            write SetOnCommandREIN;
    property OnCommandPORT: FTPTBasicEvent read fOnCommandPORT
                                           write SetOnCommandPORT;
    property OnCommandPASV: FTPTSimpleEvent read fOnCommandPASV
                                            write SetOnCommandPASV;
    property OnCommandTYPE: FTPTBasicEvent read fOnCommandTYPE
                                           write SetOnCommandTYPE;
    property OnCommandSTRU: FTPTBasicEvent read fOnCommandSTRU
                                           write SetOnCommandSTRU;
    property OnCommandMODE: FTPTBasicEvent read fOnCommandMODE
                                           write SetOnCommandMODE;
    property OnCommandRETR: FTPTBasicEvent read fOnCommandRETR
                                           write SetOnCommandRETR;
    property OnCommandSTOR: FTPTBasicEvent read fOnCommandSTOR
                                           write SetOnCommandSTOR;
    property OnCommandSTOU: FTPTSimpleEvent read fOnCommandSTOU
                                            write SetOnCommandSTOU;
    property OnCommandAPPE: FTPTBasicEvent read fOnCommandAPPE
                                           write SetOnCommandAPPE;
    property OnCommandALLO: FTPTComplexEvent read fOnCommandALLO
                                             write SetOnCommandALLO;
    property OnCommandREST: FTPTBasicEvent read fOnCommandREST
                                           write SetOnCommandREST;
    property OnCommandRNFR: FTPTBasicEvent read fOnCommandRNFR
                                           write SetOnCommandRNFR;
    property OnCommandRNTO: FTPTBasicEvent read fOnCommandRNTO
                                           write SetOnCommandRNTO;
    property OnCommandABOR: FTPTSimpleEvent read fOnCommandABOR
                                            write SetOnCommandABOR;
    property OnCommandDELE: FTPTBasicEvent read fOnCommandDELE
                                           write SetOnCommandDELE;
    property OnCommandRMD: FTPTBasicEvent read fOnCommandRMD
                                          write SetOnCommandRMD;
    property OnCommandMKD: FTPTBasicEvent read fOnCommandMKD
                                          write SetOnCommandMKD;
    property OnCommandPWD: FTPTSimpleEvent read fOnCommandPWD
                                           write SetOnCommandPWD;
    property OnCommandLIST: FTPTBasicEvent read fOnCommandLIST
                                           write SetOnCommandLIST;
    property OnCommandNLST: FTPTBasicEvent read fOnCommandNLST
                                           write SetOnCommandNLST;
    property OnCommandSITE: FTPTBasicEvent read fOnCommandSITE
                                           write SetOnCommandSITE;
    property OnCommandSIZE: FTPTBasicEvent read fOnCommandSIZE
                                           write SetOnCommandSIZE; // 2.0
    property OnCommandSYST: FTPTSimpleEvent read fOnCommandSYST
                                            write SetOnCommandSYST;
    property OnCommandSTAT: FTPTBasicEvent read fOnCommandSTAT
                                           write SetOnCommandSTAT;
    property OnCommandHELP: FTPTBasicEvent read fOnCommandHELP
                                           write SetOnCommandHELP;
    property OnCommandNOOP: FTPTSimpleEvent read fOnCommandNOOP
                                            write SetOnCommandNOOP;
    property OnCommandOther: FTPTOtherEvent read fOnCommandOther
                                            write fOnCommandOther;
  end;

implementation

Uses
   DXSocket,
   DXSock,
   DXString;

Type
  PFTPBasicEvent=^TFTPBasicEvent;
  TFTPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:FTPTBasicEvent;
  End;
  PFTPSimpleEvent=^TFTPSimpleEvent;
  TFTPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:FTPTSimpleEvent;
  End;
  PFTPComplexEvent=^TFTPComplexEvent;
  TFTPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:FTPTComplexEvent;
  End;
  PFTPPassEvent=^TFTPPassEvent;
  TFTPPassEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:FTPTPassEvent;
  End;

///////////////////////////////////////////////////////////////////////////////
// from RFC959...
//   'USER', {USER<SP><username><CRLF>}
//   'PASS', {PASS<SP><password><CRLF>}
//   'ACCT', {ACCT<SP><account-information><CRLF>}
//   'CWD',  {CWD<SP><pathname><CRLF>}
//   'CDUP', {CDUP<CRLF>}
//   'SMNT', {SMNT<SP><pathname><CRLF>}
//   'QUIT', {QUIT<CRLF>}
//   'REIN', {REIN<CRLF>}
//   'PORT', {PORT<SP><host-port><CRLF>}
//   'PASV', {PASV<CRLF>}
//   'TYPE', {TYPE<SP><type-code><CRLF>}
//   'STRU', {STRU<SP><structure-code><CRLF>}
//   'MODE', {MODE<SP><mode-code><CRLF>}
//   'RETR', {RETR<SP><pathname><CRLF>}
//   'STOR', {STOR<SP><pathname><CRLF>}
//   'STOU', {STOU<CRLF>}
//   'APPE', {APPE<SP><pathname><CRLF>}
//   'ALLO', {ALLO<SP><decimal-integer>[<SP>R<SP><decimal-integer>]<CRLF>}
//   'REST', {REST<SP><marker><CRLF>}
//   'RNFR', {RNFR<SP><pathname><CRLF>}
//   'RNTO', {RNTO<SP><pathname><CRLF>}
//   'ABOR', {ABOR<CRLF>}
//   'DELE', {DELE<SP><pathname><CRLF>}
//   'RMD',  {RMD<SP><pathname><CRLF>}
//   'MKD',  {MKD<SP><pathname><CRLF>}
//   'PWD',  {PWD<CRLF>}
//   'LIST', {LIST[<SP><pathname>]<CRLF>}
//   'NLST', {NLST[<SP><pathname>]<CRLF>}
//   'SITE', {SITE<SP><string><CRLF>}
//   'SYST', {SYST<CRLF>}
//   'STAT', {STAT[<SP><pathname>]<CRLF>}
//   'HELP', {HELP[<SP><string>]<CRLF>}
//   'NOOP', {NOOP<CRLF>}
//   'XMKD', {same as MKD - RFC775}
//   'XRMD', {same as RMD - RFC775}
//   'XPWD', {same as PWD - RFC775}
//   'XCUP'  {same as CDUP - RFC775}
//   'SIZE', {SIZE<SP><filename><CRLF>}
///////////////////////////////////////////////////////////////////////////////

constructor TDXFTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=21;
end;

destructor TDXFTPServerCore.Destroy;
Var
   PBasicEvent:PFTPBasicEvent;
   PSimpleEvent:PFTPSimpleEvent;
   PComplexEvent:PFTPComplexEvent;
   PPassEvent:PFTPPassEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PFTPBasicEvent(fEventArray[0]).Tag of
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
            4:Begin
              PPassEvent:=fEventArray[0];
              Dispose(PPassEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////////
//ADDBASICEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddBasicEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXFTPServerCore.AddBasicEvent(Command:String;EventProc:FTPTBasicEvent);
Var
   PBasicEvent:PFTPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PFTPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PFTPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

///////////////////////////////////////////////////////////////////////////////
//ADDSIMPLEEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddSimpleEvent('IDlE',MySpecialIdleEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXFTPServerCore.AddSimpleEvent(Command:String;EventProc:FTPTSimpleEvent);
Var
   PSimpleEvent:PFTPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PFTPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PFTPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

///////////////////////////////////////////////////////////////////////////////
//ADDCOMPLEXEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddComplexEvent('ADDUSER',MySpecialAddUserEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXFTPServerCore.AddComplexEvent(Command:String;EventProc:FTPTComplexEvent);
Var
   PComplexEvent:PFTPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PFTPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PFTPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

///////////////////////////////////////////////////////////////////////////////
//ADDPASSEVENT:
//              Allows you to dynamically assign a new command to the internal
//              parser. This allows the servercore to support the 'pre-defined'
//              OnCommand* events, plus you can add other commands dynamically
//              at run-time in your application without requiring a source code
//              modification to our components!
//
//              To make support easier for us, we ask that you use the Add*Event
//              procedures to expand our code, reducing code changes when an
//              upgrade is released!
//
//              See documentation for complete information on how this works.
//
//              Example Usage: AddPassEvent('ADDUSER',MySpecialAddUserEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXFTPServerCore.AddPassEvent(Command:String;EventProc:FTPTPassEvent);
Var
   PPassEvent:PFTPPassEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PFTPPassEvent(fEventArray[Loop]).Command=Command then Begin
         PFTPPassEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PPassEvent);
   PPassEvent.Tag:=4;      // Denotes Event in fEventArray is a TBasicEvent!
   PPassEvent.Command:=Command;
   PPassEvent.EventProcedure:=EventProc;
   fEventArray.Add(PPassEvent);
End;

Procedure TDXFTPServerCore.SetOnCommandUSER(value:FTPTBasicEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandPASS(value:FTPTPassEvent);
Begin
   fOnCommandPASS:=Value;
   AddPassEvent('PASS',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandACCT(value:FTPTBasicEvent);
Begin
   fOnCommandACCT:=Value;
   AddBasicEvent('ACCT',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandCWD(value:FTPTBasicEvent);
Begin
   fOnCommandCWD:=Value;
   AddBasicEvent('CWD',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandCDUP(value:FTPTSimpleEvent);
Begin
   fOnCommandCDUP:=Value;
   AddSimpleEvent('CDUP',Value);
   AddSimpleEvent('XCUP',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSMNT(value:FTPTBasicEvent);
Begin
   fOnCommandSMNT:=Value;
   AddBasicEvent('SMNT',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandQUIT(value:FTPTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandREIN(value:FTPTSimpleEvent);
Begin
   fOnCommandREIN:=Value;
   AddSimpleEvent('REIN',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandPORT(value:FTPTBasicEvent);
Begin
   fOnCommandPORT:=Value;
   AddBasicEvent('PORT',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandPASV(value:FTPTSimpleEvent);
Begin
   fOnCommandPASV:=Value;
   AddSimpleEvent('PASV',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandTYPE(value:FTPTBasicEvent);
Begin
   fOnCommandTYPE:=Value;
   AddBasicEvent('TYPE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSTRU(value:FTPTBasicEvent);
Begin
   fOnCommandSTRU:=Value;
   AddBasicEvent('STRU',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandMODE(value:FTPTBasicEvent);
Begin
   fOnCommandMODE:=Value;
   AddBasicEvent('MODE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandRETR(value:FTPTBasicEvent);
Begin
   fOnCommandRETR:=Value;
   AddBasicEvent('RETR',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSTOR(value:FTPTBasicEvent);
Begin
   fOnCommandSTOR:=Value;
   AddBasicEvent('STOR',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSTOU(value:FTPTSimpleEvent);
Begin
   fOnCommandSTOU:=Value;
   AddSimpleEvent('STOU',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandAPPE(value:FTPTBasicEvent);
Begin
   fOnCommandAPPE:=Value;
   AddBasicEvent('APPE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandALLO(value:FTPTComplexEvent);
Begin
   fOnCommandALLO:=Value;
   AddComplexEvent('ALLO',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandREST(value:FTPTBasicEvent);
Begin
   fOnCommandREST:=Value;
   AddBasicEvent('REST',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandRNFR(value:FTPTBasicEvent);
Begin
   fOnCommandRNFR:=Value;
   AddBasicEvent('RNFR',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandRNTO(value:FTPTBasicEvent);
Begin
   fOnCommandRNTO:=Value;
   AddBasicEvent('RNTO',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandABOR(value:FTPTSimpleEvent);
Begin
   fOnCommandABOR:=Value;
   AddSimpleEvent('ABOR',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandDELE(value:FTPTBasicEvent);
Begin
   fOnCommandDELE:=Value;
   AddBasicEvent('DELE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandRMD(value:FTPTBasicEvent);
Begin
   fOnCommandRMD:=Value;
   AddBasicEvent('RMD',Value);
   AddBasicEvent('XRMD',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandMKD(value:FTPTBasicEvent);
Begin
   fOnCommandMKD:=Value;
   AddBasicEvent('MKD',Value);
   AddBasicEvent('XMKD',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandPWD(value:FTPTSimpleEvent);
Begin
   fOnCommandPWD:=Value;
   AddSimpleEvent('PWD',Value);
   AddSimpleEvent('XPWD',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandLIST(value:FTPTBasicEvent);
Begin
   fOnCommandLIST:=Value;
   AddBasicEvent('LIST',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandNLST(value:FTPTBasicEvent);
Begin
   fOnCommandNLST:=Value;
   AddBasicEvent('NLST',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSITE(value:FTPTBasicEvent);
Begin
   fOnCommandSITE:=Value;
   AddBasicEvent('SITE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSIZE(value:FTPTBasicEvent);
Begin
   fOnCommandSIZE:=Value;
   AddBasicEvent('SIZE',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSYST(value:FTPTSimpleEvent);
Begin
   fOnCommandSYST:=Value;
   AddSimpleEvent('SYST',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandSTAT(value:FTPTBasicEvent);
Begin
   fOnCommandSTAT:=Value;
   AddBasicEvent('STAT',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandHELP(value:FTPTBasicEvent);
Begin
   fOnCommandHELP:=Value;
   AddBasicEvent('HELP',Value);
End;

Procedure TDXFTPServerCore.SetOnCommandNOOP(value:FTPTSimpleEvent);
Begin
   fOnCommandNOOP:=Value;
   AddSimpleEvent('NOOP',Value);
End;

///////////////////////////////////////////////////////////////////////////////
//SAYHELLO:
//         A built-in Routine to Call in your "onNewConnect", reduces problems
//         if you do not know how the protocols work. Simply call this routine
//         with a TStringList of your "Server Header", and a TStringList of your
//         "Message of the DAY". Both are optional, if both a NIL this routine
//         will just respond to the client "FTP Server (Ready)", and the client
//         will begin login.
///////////////////////////////////////////////////////////////////////////////
procedure TDXFTPServerCore.SayHello(ClientThread:TDXClientThread;Header:TStrings);
Var
   Loop:Integer;
   Ws:String;

Begin
   If Assigned(Header) then Begin
      ClientThread.Socket.Writeln('220-');
      For Loop:=1 to Header.Count do Begin
         Ws:=#32+Header[Loop-1];
         ClientThread.Socket.Writeln(Ws);
      End;
   End;
   ClientThread.Socket.Writeln('220 '+GetLocalHostName+' FTP Server (ready).');
End;

///////////////////////////////////////////////////////////////////////////////
//SAYGOODBYE:
//           A built-in Routine to Call at the end of your "onNewConnect", this
//           will send a String "Footer", or the defacto "Goodbye." to the
//           client program. This should be the last piece of code in your
//           onNewConnect. Remember the onDisconnect is not a good place to
//           send any output - as the client could have disconnected already!
///////////////////////////////////////////////////////////////////////////////
procedure TDXFTPServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:TStrings);
Var
   Loop:Integer;
   Ws:String;

Begin
   If Assigned(Footer) then Begin
      ClientThread.Socket.Writeln('221-');
      For Loop:=1 to Footer.Count do Begin
         Ws:=#32+Footer[Loop-1];
         ClientThread.Socket.Writeln(Ws);
      End;
   End;
   ClientThread.Socket.Writeln('221 Goodbye.');
End;

///////////////////////////////////////////////////////////////////////////////
//PROCESSSESSION:
//               If you want this CORE to process the parsing, you should call
//               this from your "OnNewConnect" implementation. This should be
//               right after your call to SayHello (optional).
///////////////////////////////////////////////////////////////////////////////
procedure TDXFTPServerCore.ProcessSession(ClientThread:TDXClientThread;MOTD:TStrings);
var
  s, sCmd: string;
  WasHandled: Boolean;
  Loop,Loop2:Integer;
  MOTDShown:Boolean; // 2.0
  OutData:Pointer;


begin
   MOTDShown:=False;
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while Connected do begin
         If fbForceAbort then Exit;
         S:=ReadLn(Timeout);
         If LastReadTimeout or (not Connected) then Exit;
         if S='' then continue;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(Outdata) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(S,#32,False));
         Loop:=0;
         WasHandled:=False;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PFTPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PFTPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PFTPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       FTPTBasicEvent(PFTPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PFTPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       FTPTSimpleEvent(PFTPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PFTPComplexEvent(fEventArray[Loop]).EventProcedure) then
                       FTPTComplexEvent(PFTPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
                  4:Begin
                    Washandled:=False;
                    if Assigned(PFTPPassEvent(fEventArray[Loop]).EventProcedure) then
                       FTPTPassEvent(PFTPPassEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S,WasHandled);
                    If WasHandled then Begin
                       If Not MOTDShown then Begin
                          If Assigned(MOTD) then Begin
                             Writeln('230-');
                             For Loop2:=1 to MOTD.Count do Begin
                                S:=#32+MOTD[Loop2-1];
                                Writeln(S);
                             End;
                             Writeln('230 You have successfully logged in, you may proceed.');
                             MOTD.Clear;
                          End;
                          MOTDShown:=True;
                       End;
                    End;
                  End;
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If sCMD='QUIT' then Exit;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then
            Writeln('500 command not recognized (' + sCMD + ')');
      end;
   end;
end;

end.

