unit DXNNTPServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXNNTPServerCore
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
//  Description: implements NNTP (network news transport protocol)
// ========================================================================
// Rev 3 - RFC2980 extensions incorporated
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  NNTPTSimpleEvent = procedure(ClientThread: TDXClientThread) of object;
  NNTPTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; var Handled: Boolean) of object;
  NNTPTDoByIDEvent = procedure(ClientThread: TDXClientThread; ActualID: string) of object;
  NNTPTDoByNoEvent = procedure(ClientThread: TDXClientThread; ActualNumber: integer) of object;
  NNTPTBasicEvent = procedure(ClientThread: TDXClientThread; Parm: string) of object;
  NNTPTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;

  TDXNNTPServerCore = class(TDXServerCore)
  private
    fOnCommandAuthInfo: NNTPTComplexEvent; {authinfo user [data]}
                                           {authinfo pass [data]}
                                           {authinfo simple [user] [pass]}
                                           {authinfo generic authenticator arguements}
    fOnCommandArticleID: NNTPTDoByIDEvent; {article <message-id>}
    fOnCommandArticleNO: NNTPTDoByNoEvent; {article 7872}
    fOnCommandBodyID: NNTPTDoByIDEvent; {body <message-id>}
    fOnCommandBodyNO: NNTPTDoByNoEvent; {body 7872}
    fOnCommandHeadID: NNTPTDoByIDEvent; {head <message-id>}
    fOnCommandHeadNO: NNTPTDoByNoEvent; {head 7872}
    fOnCommandStatID: NNTPTDoByIDEvent; {stat <message-id>} {useless!}
    fOnCommandStatNO: NNTPTDoByNoEvent; {stat 7872}
    fOnCommandGroup: NNTPTBasicEvent; {group net.news}
    fOnCommandList: NNTPTBasicEvent; {list [optional parm]}
                                     {list active (RFC2980)}
                                     {list active.times (RFC2980)}
                                     {list distributions (RFC2980)}
                                     {list distrib.pats (RFC2980)}
                                     {list newsgroups (RFC2980)}
                                     {list overview.fmt (RFC2980)}
                                     {list subscriptions (RFC2980)}
    fOnCommandHelp: NNTPTSimpleEvent; {help}
    fOnCommandIHave: NNTPTDoByIDEvent; {ihave <message-id>}
    fOnCommandLast: NNTPTSimpleEvent; {last}
    fOnCommandMode: NNTPTBasicEvent; {mode reader}
                                     {mode stream (RFC2980)}
    fOnCommandNewGroups: NNTPTBasicEvent; {newsgroups yymmdd hhmmss [GMT] <distributions>}
    fOnCommandNewNews: NNTPTBasicEvent; {newnews newsgroups yymmdd hhmmss [GMT] <distributions>}
    fOnCommandNext: NNTPTSimpleEvent; {next}
    fOnCommandPost: NNTPTSimpleEvent; {post}
    fOnCommandQuit: NNTPTSimpleEvent; {quit}
    fOnCommandSlave: NNTPTSimpleEvent; {slave}
{not in RFC977}
    fOnCommandXOver: NNTPTBasicEvent; {xover start#-stop#}
    fOnCommandXHDR: NNTPTBasicEvent; {xhdr header start#-stop#}
    fOnCommandDate: NNTPTSimpleEvent; {date}
{in RFC2980}
    fOnCommandCheck:NNTPTDoByIDEvent; {check <message-id>}
    fOnCommandTakethis:NNTPTDoByIDEvent; {takethis <message-id>}
    fOnCommandXReplic:NNTPTBasicEvent; {xreplic ggg:nnn[,ggg:nnn...]}
    fOnCommandListgroup:NNTPTBasicEvent; {listgroup [ggg]}
    fOnCommandXGTitle:NNTPTBasicEvent; {xgtitle [ggg]}
    fOnCommandXIndex:NNTPTBasicEvent; {xindex ggg}
    fOnCommandXPAT:NNTPTComplexEvent; {xpat header range|<message-id> pattern}
    fOnCommandXPath:NNTPTDoByIDEvent; {xpath <message-id>}
    fOnCommandXThread:NNTPTBasicEvent; {xthread [dbinit|thread]}
    fOnCommandCancel:NNTPTDoByIDEvent; {cancel <message-id>}
    fOnCommandSendMe:NNTPTDoByIDEvent; {sendme <message-id>}
    fOnCommandRMGroup:NNTPTBasicEvent; {rmgroup ggg}
    fOnCommandSendSys:NNTPTSimpleEvent; {sendsys}
    fOnCommandVersion:NNTPTSimpleEvent; {version}
{other support}
    fOnCommandOther: NNTPTOtherEvent;
  protected
    Procedure SetOnCommandAuthInfo(value:NNTPTComplexEvent);
    Procedure SetOnCommandArticleID(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandArticleNO(value:NNTPTDoByNOEvent);
    Procedure SetOnCommandBodyID(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandBodyNO(value:NNTPTDoByNOEvent);
    Procedure SetOnCommandHeadID(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandHeadNO(value:NNTPTDoByNOEvent);
    Procedure SetOnCommandStatID(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandStatNO(value:NNTPTDoByNOEvent);
    Procedure SetOnCommandGroup(value:NNTPTBasicEvent);
    Procedure SetOnCommandList(value:NNTPTBasicEvent);
    Procedure SetOnCommandHelp(value:NNTPTSimpleEvent);
    Procedure SetOnCommandIHave(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandLast(value:NNTPTSimpleEvent);
    Procedure SetOnCommandMode(value:NNTPTBasicEvent);
    Procedure SetOnCommandNewGroups(value:NNTPTBasicEvent);
    Procedure SetOnCommandNewNews(value:NNTPTBasicEvent);
    Procedure SetOnCommandNext(value:NNTPTSimpleEvent);
    Procedure SetOnCommandPost(value:NNTPTSimpleEvent);
    Procedure SetOnCommandQuit(value:NNTPTSimpleEvent);
    Procedure SetOnCommandSlave(value:NNTPTSimpleEvent);
    Procedure SetOnCommandXOver(value:NNTPTBasicEvent);
    Procedure SetOnCommandXHDR(value:NNTPTBasicEvent);
    Procedure SetOnCommandDate(value:NNTPTSimpleEvent);
    Procedure SetOnCommandListGroup(value:NNTPTBasicEvent);
{new as of v2 rev 3}
    Procedure SetOnCommandCheck(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandTakeThis(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandXReplic(value:NNTPTBasicEvent);
    Procedure SetOnCommandXGTitle(value:NNTPTBasicEvent);
    Procedure SetOnCommandXIndex(value:NNTPTBasicEvent);
    Procedure SetOnCommandXPAT(value:NNTPTComplexEvent);
    Procedure SetOnCommandXPATH(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandXThread(value:NNTPTBasicEvent);
    Procedure SetOnCommandCancel(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandSendMe(value:NNTPTDoByIDEvent);
    Procedure SetOnCommandRMGroup(value:NNTPTBasicEvent);
    Procedure SetOnCommandSendSys(value:NNTPTSimpleEvent);
    Procedure SetOnCommandVersion(value:NNTPTSimpleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure SayHello(ClientThread:TDXClientThread;Header:String);
    procedure SayGoodbye(ClientThread:TDXClientThread;Footer:String);
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:NNTPTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:NNTPTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:NNTPTComplexEvent);
    Procedure AddDoByIDEvent(Command:String;EventProc:NNTPTDoByIDEvent);
    Procedure AddDoByNOEvent(Command:String;EventProc:NNTPTDoByNOEvent);
  published
    property OnCommandAuthInfo: NNTPTComplexEvent read fOnCommandAuthInfo
                                            write SetOnCommandAuthInfo;
    property OnCommandArticleID: NNTPTDoByIDEvent read fOnCommandArticleID
                                              write SetOnCommandArticleID;
    property OnCommandArticleNo: NNTPTDoByNoEvent read fOnCommandArticleNo
                                              write SetOnCommandArticleNo;
    property OnCommandBodyID: NNTPTDoByIDEvent read fOnCommandBodyID
                                           write SetOnCommandBodyID;
    property OnCommandBodyNo: NNTPTDoByNoEvent read fOnCommandBodyNo
                                           write SetOnCommandBodyNo;
    property OnCommandHeadID: NNTPTDoByIDEvent read fOnCommandHeadID
                                           write SetOnCommandHeadID;
    property OnCommandHeadNo: NNTPTDoByNoEvent read fOnCommandHeadNo
                                           write SetOnCommandHeadNo;
    property OnCommandStatID: NNTPTDoByIDEvent read fOnCommandStatID
                                           write SetOnCommandStatID;
    property OnCommandStatNo: NNTPTDoByNoEvent read fOnCommandStatNo
                                           write SetOnCommandStatNo;
    property OnCommandGroup: NNTPTBasicEvent read fOnCommandGroup
                                         write SetOnCommandGroup;
    property OnCommandList: NNTPTBasicEvent read fOnCommandList
                                       write SetOnCommandList;
    property OnCommandHelp: NNTPTSimpleEvent read fOnCommandHelp
                                      write SetOnCommandHelp;
    property OnCommandIHave: NNTPTDoByIDEvent read fOnCommandIHave
                                          write SetOnCommandIHave;
    property OnCommandLast: NNTPTSimpleEvent read fOnCommandLast
                                      write SetOnCommandLast;
    property OnCommandMode: NNTPTBasicEvent read fOnCommandMode
                                       write SetOnCommandMode;
    property OnCommandNewGroups: NNTPTBasicEvent read fOnCommandNewGroups
                                            write SetOnCommandNewGroups;
    property OnCommandNewNews: NNTPTBasicEvent read fOnCommandNewNews
                                          write SetOnCommandNewNews;
    property OnCommandNext: NNTPTSimpleEvent read fOnCommandNext
                                      write SetOnCommandNext;
    property OnCommandPost: NNTPTSimpleEvent read fOnCommandPost
                                      write SetOnCommandPost;
    property OnCommandQuit: NNTPTSimpleEvent read fOnCommandQuit
                                      write SetOnCommandQuit;
    property OnCommandSlave: NNTPTSimpleEvent read fOnCommandSlave
                                       write SetOnCommandSlave;
    property OnCommandXOver: NNTPTBasicEvent read fOnCommandXOver
                                        write SetOnCommandXOver;
    property OnCommandXHDR: NNTPTBasicEvent read fOnCommandXHDR
                                       write SetOnCommandXHDR;
    property OnCommandDate: NNTPTSimpleEvent read fOnCommandDate
                                      write SetOnCommandDate;
    property OnCommandListgroup: NNTPTBasicEvent read fOnCommandListGroup
                                                 write SetOnCommandListGroup;
// 2.3
    property OnCommandCheck:NNTPTDoByIDEvent read fOnCommandCheck
                                             write SetOnCommandCheck;
    property OnCommandTakethis:NNTPTDoByIDEvent read fOnCommandTakethis
                                                 write SetOnCommandTakethis;
    property OnCommandXReplic:NNTPTBasicEvent read fOnCommandXReplic
                                               write SetOnCommandXReplic;
    property OnCommandXGTitle:NNTPTBasicEvent read fOnCommandXGTitle
                                               write SetOnCommandXGTitle;
    property OnCommandXIndex:NNTPTBasicEvent read fOnCommandXIndex
                                              write SetOnCommandXIndex;
    property OnCommandXPAT:NNTPTComplexEvent read fOnCommandXPAT
                                              write SetOnCommandXPAT;
    property OnCommandXPath:NNTPTDoByIDEvent read fOnCommandXPath
                                              write SetOnCommandXPath;
    property OnCommandXThread:NNTPTBasicEvent read fOnCommandXThread
                                               write SetOnCommandXThread;
    property OnCommandCancel:NNTPTDoByIDEvent read fOnCommandCancel
                                               write SetOnCommandCancel;
    property OnCommandSendMe:NNTPTDoByIDEvent read fOnCommandSendMe
                                               write SetOnCommandSendMe;
    property OnCommandRMGroup:NNTPTBasicEvent read fOnCommandRMGroup
                                               write SetOnCommandRMGroup;
    property OnCommandSendSys:NNTPTSimpleEvent read fOnCommandSendSys
                                                write SetOnCommandSendSys;
    property OnCommandVersion:NNTPTSimpleEvent read fOnCommandVersion
                                                write SetOnCommandVersion;
    property OnCommandOther: NNTPTOtherEvent read fOnCommandOther
                                             write fOnCommandOther;
  end;

implementation

uses
   DXSock,
   DXString;

Type
  PNNTPBasicEvent=^TNNTPBasicEvent;
  TNNTPBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NNTPTBasicEvent;
  End;
  PNNTPSimpleEvent=^TNNTPSimpleEvent;
  TNNTPSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NNTPTSimpleEvent;
  End;
  PNNTPComplexEvent=^TNNTPComplexEvent;
  TNNTPComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NNTPTComplexEvent;
  End;
  PNNTPDoByIDEvent=^TNNTPDoByIDEvent;
  TNNTPDoByIDEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NNTPTDoByIDEvent;
  End;
  PNNTPDoByNOEvent=^TNNTPDoByNOEvent;
  TNNTPDoByNOEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:NNTPTDoByNOEvent;
  End;

constructor TDXNNTPServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=119;
end;

destructor TDXNNTPServerCore.Destroy;
Var
   PBasicEvent:PNNTPBasicEvent;
   PSimpleEvent:PNNTPSimpleEvent;
   PComplexEvent:PNNTPComplexEvent;
   PDoByIDEvent:PNNTPDoByIDEvent;
   PDoByNOEvent:PNNTPDoByNoEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PNNTPBasicEvent(fEventArray[0]).Tag of
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
              PDoByIDEvent:=fEventArray[0];
              Dispose(PDoByIDEvent);
            End;
            5:Begin
              PDoByNoEvent:=fEventArray[0];
              Dispose(PDoByNOEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXNNTPServerCore.AddBasicEvent(Command:String;EventProc:NNTPTBasicEvent);
Var
   PBasicEvent:PNNTPBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNNTPBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PNNTPBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXNNTPServerCore.AddSimpleEvent(Command:String;EventProc:NNTPTSimpleEvent);
Var
   PSimpleEvent:PNNTPSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNNTPSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PNNTPSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PSimpleEvent);
   PSimpleEvent.Tag:=2;      // Denotes Event in fEventArray is a TSimpleEvent!
   PSimpleEvent.Command:=Command;
   PSimpleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PSimpleEvent);
End;

Procedure TDXNNTPServerCore.AddComplexEvent(Command:String;EventProc:NNTPTComplexEvent);
Var
   PComplexEvent:PNNTPComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNNTPComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PNNTPComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PComplexEvent);
   PComplexEvent.Tag:=3;      // Denotes Event in fEventArray is a TComplexEvent!
   PComplexEvent.Command:=Command;
   PComplexEvent.EventProcedure:=EventProc;
   fEventArray.Add(PComplexEvent);
End;

Procedure TDXNNTPServerCore.AddDoByIDEvent(Command:String;EventProc:NNTPTDoByIDEvent);
Var
   PDoByIDEvent:PNNTPDoByIDEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNNTPDoByIDEvent(fEventArray[Loop]).Command=Command then Begin
         PNNTPDoByIDEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PDoByIDEvent);
   PDoByIDEvent.Tag:=4;      // Denotes Event in fEventArray is a TDoByIDEvent!
   PDoByIDEvent.Command:=Command;
   PDoByIDEvent.EventProcedure:=EventProc;
   fEventArray.Add(PDoByIDEvent);
End;

Procedure TDXNNTPServerCore.AddDoByNOEvent(Command:String;EventProc:NNTPTDoByNOEvent);
Var
   PDoByNOEvent:PNNTPDoByNOEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PNNTPDoByNOEvent(fEventArray[Loop]).Command=Command then Begin
         PNNTPDoByNOEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PDoByNOEvent);
   PDoByNOEvent.Tag:=5;      // Denotes Event in fEventArray is a TDoByNOEvent!
   PDoByNOEvent.Command:=Command;
   PDoByNOEvent.EventProcedure:=EventProc;
   fEventArray.Add(PDoByNOEvent);
End;

Procedure TDXNNTPServerCore.SetOnCommandAuthInfo(value:NNTPTComplexEvent);
Begin
   fOnCommandAuthINFO:=Value;
   AddComplexEvent('AUTHINFO',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandArticleID(value:NNTPTDoByIDEvent);
Begin
   fOnCommandARTICLEID:=Value;
   AddDoByIDEvent('ARTICLEID',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandArticleNO(value:NNTPTDoByNOEvent);
Begin
   fOnCommandARTICLENO:=Value;
   AddDoByNoEvent('ARTICLENO',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandBodyID(value:NNTPTDoByIDEvent);
Begin
   fOnCommandBODYID:=Value;
   AddDoByIDEvent('BODYID',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandBodyNO(value:NNTPTDoByNOEvent);
Begin
   fOnCommandBODYNO:=Value;
   AddDoByNoEvent('BODYNO',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandHeadID(value:NNTPTDoByIDEvent);
Begin
   fOnCommandHEADID:=Value;
   AddDoByIDEvent('HEADID',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandHeadNO(value:NNTPTDoByNOEvent);
Begin
   fOnCommandHEADNO:=Value;
   AddDoByNoEvent('HEADNO',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandStatID(value:NNTPTDoByIDEvent);
Begin
   fOnCommandSTATID:=Value;
   AddDoByIDEvent('STATID',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandStatNO(value:NNTPTDoByNOEvent);
Begin
   fOnCommandSTATNO:=Value;
   AddDoByNoEvent('STATNO',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandGroup(value:NNTPTBasicEvent);
Begin
   fOnCommandGROUP:=Value;
   AddBasicEvent('GROUP',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandList(value:NNTPTBasicEvent);
Begin
   fOnCommandLIST:=Value;
   AddBasicEvent('LIST',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandHelp(value:NNTPTSimpleEvent);
Begin
   fOnCommandHELP:=Value;
   AddSimpleEvent('HELP',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandIHave(value:NNTPTDoByIDEvent);
Begin
   fOnCommandIHAVE:=Value;
   AddDoByIDEvent('IHAVE',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandLast(value:NNTPTSimpleEvent);
Begin
   fOnCommandLAST:=Value;
   AddSimpleEvent('LAST',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandMode(value:NNTPTBasicEvent);
Begin
   fOnCommandMODE:=Value;
   AddBasicEvent('MODE',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandNewGroups(value:NNTPTBasicEvent);
Begin
   fOnCommandNEWGROUPS:=Value;
   AddBasicEvent('NEWGROUPS',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandNewNews(value:NNTPTBasicEvent);
Begin
   fOnCommandNEWNEWS:=Value;
   AddBasicEvent('NEWNEWS',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandNext(value:NNTPTSimpleEvent);
Begin
   fOnCommandNEXT:=Value;
   AddSimpleEvent('NEXT',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandPost(value:NNTPTSimpleEvent);
Begin
   fOnCommandPOST:=Value;
   AddSimpleEvent('POST',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandQuit(value:NNTPTSimpleEvent);
Begin
   fOnCommandQUIT:=Value;
   AddSimpleEvent('QUIT',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandSlave(value:NNTPTSimpleEvent);
Begin
   fOnCommandSLAVE:=Value;
   AddSimpleEvent('SLAVE',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXOver(value:NNTPTBasicEvent);
Begin
   fOnCommandXOVER:=Value;
   AddBasicEvent('XOVER',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXHDR(value:NNTPTBasicEvent);
Begin
   fOnCommandXHDR:=Value;
   AddBasicEvent('XHDR',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandDate(value:NNTPTSimpleEvent);
Begin
   fOnCommandDATE:=Value;
   AddSimpleEvent('DATE',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandListGroup(value:NNTPTBasicEvent);
Begin
   fOnCommandLISTGROUP:=Value;
   AddBasicEvent('LISTGROUP',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandCheck(value:NNTPTDoByIDEvent);
Begin
   fOnCommandCHECK:=Value;
   AddDoByIDEvent('CHECK',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandTakeThis(value:NNTPTDoByIDEvent);
Begin
   fOnCommandTAKETHIS:=Value;
   AddDoByIDEvent('TAKETHIS',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXReplic(value:NNTPTBasicEvent);
Begin
   fOnCommandXREPLIC:=Value;
   AddBasicEvent('XREPLIC',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXGTitle(value:NNTPTBasicEvent);
Begin
   fOnCommandXGTITLE:=Value;
   AddBasicEvent('XGTITLE',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXIndex(value:NNTPTBasicEvent);
Begin
   fOnCommandXINDEX:=Value;
   AddBasicEvent('XINDEX',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXPAT(value:NNTPTComplexEvent);
Begin
   fOnCommandXPAT:=Value;
   AddComplexEvent('XPAT',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXPATH(value:NNTPTDoByIDEvent);
Begin
   fOnCommandXPATH:=Value;
   AddDoByIDEvent('XPATH',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandXThread(value:NNTPTBasicEvent);
Begin
   fOnCommandXTHREAD:=Value;
   AddBasicEvent('XTHREAD',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandCancel(value:NNTPTDoByIDEvent);
Begin
   fOnCommandCANCEL:=Value;
   AddDoByIDEvent('CANCEL',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandSendMe(value:NNTPTDoByIDEvent);
Begin
   fOnCommandSENDME:=Value;
   AddDoByIDEvent('SENDME',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandRMGroup(value:NNTPTBasicEvent);
Begin
   fOnCommandRMGROUP:=Value;
   AddBasicEvent('RMGROUP',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandSendSys(value:NNTPTSimpleEvent);
Begin
   fOnCommandSENDSYS:=Value;
   AddSimpleEvent('SENDSYS',Value);
End;

Procedure TDXNNTPServerCore.SetOnCommandVersion(value:NNTPTSimpleEvent);
Begin
   fOnCommandVERSION:=Value;
   AddSimpleEvent('VERSION',Value);
End;

procedure TDXNNTPServerCore.SayHello(ClientThread:TDXClientThread;Header:String);
Begin
   If Header<>'' then
      ClientThread.Socket.Writeln('200 '+Header)
   Else
      ClientThread.Socket.Writeln('200 NNTP Server (ready).');
End;

procedure TDXNNTPServerCore.SayGoodbye(ClientThread:TDXClientThread;Footer:String);
Begin
   If Footer<>'' then
      ClientThread.Socket.Writeln('205 '+Footer)
   Else
      ClientThread.Socket.Writeln('205 Goodbye.');
End;

procedure TDXNNTPServerCore.ProcessSession(ClientThread:TDXClientThread);
var
   s, sCmd: string;
   Loop:Integer;
   WasHandled:Boolean;
   OutData:Pointer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while Connected and
         ((LastCommandStatus<9999) or
         (LastCommandStatus=10060)) do begin
         if fbForceAbort then Exit;
         s:=ReadLn(Timeout);
         If LastReadTimeout or Not Connected then Exit;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(s,#32,False));
         Loop:=0;
         WasHandled:=False;
         If (sCMD='HEAD') or (sCMD='BODY') or (sCMD='STAT') or (sCMD='ARTICLE') then Begin
            if (isNumericString(s)) or (S='') then sCMD:=sCMD+'NO'
            Else sCMD:=sCMD+'ID';
         End;
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PNNTPBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PNNTPBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PNNTPBasicEvent(fEventArray[Loop]).EventProcedure) then
                       NNTPTBasicEvent(PNNTPBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  2:if Assigned(PNNTPSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       NNTPTSimpleEvent(PNNTPSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PNNTPComplexEvent(fEventArray[Loop]).EventProcedure) then
                       NNTPTComplexEvent(PNNTPComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Uppercase(Fetch(S,#32,False)),S);
                  4:if Assigned(PNNTPDoByIDEvent(fEventArray[Loop]).EventProcedure) then
                       NNTPTDoByIDEvent(PNNTPDoByIDEvent(fEventArray[Loop]).EventProcedure)(ClientThread,NoAngleBrackets(s));
                  5:if Assigned(PNNTPDoByNoEvent(fEventArray[Loop]).EventProcedure) then
                       NNTPTDoByNoEvent(PNNTPDoByNoEvent(fEventArray[Loop]).EventProcedure)(ClientThread,StringToInteger(S));
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End; {while}
         if sCMD='QUIT' then Exit; {terminate!}
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,s,WasHandled);
         end;
         if not WasHandled then
            WriteResultCode(500,'command not recognized (' + sCMD + ')');
      end; {while}
   end; {with}
end; {doExecute}

end.

