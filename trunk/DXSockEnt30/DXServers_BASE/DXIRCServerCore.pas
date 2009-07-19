unit DXIRCServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXIRCServerCore
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
//  Description: implements IRC (Internet Relay Chat) protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

Const
   MaxStatusCodes=45;
   ERR_NOSUCHNICK=401;
   ERR_NOSUCHSERVER=402;
   ERR_NOSUCHCHANNEL=403;
   ERR_CANNOTSENDTOCHAN=404;
   ERR_TOOMANYCHANNELS=405;
   ERR_WASNOSUCHNICK=406;
   ERR_TOOMANYTARGETS=407;
   ERR_NOORIGIN=409;
   ERR_NORECIPIENT=411;
   ERR_NOTEXTTOSEND=412;
   ERR_NOTOPLEVEL=413;
   ERR_WILDTOPLEVEL=414;
   ERR_UNKNOWNCOMMAND=421;
   ERR_NOMOTD=422;
   ERR_NOADMININFO=423;
   ERR_FILEERROR=424;
   ERR_NONICKNAMEGIVEN=431;
   ERR_ERRONEUSNICKNAME=432;
   ERR_NICKNAMEINUSE=433;
   ERR_NICKCOLLISION=436;
   ERR_USERNOTINCHANNEL=441;
   ERR_NOTONCHANNEL=442;
   ERR_USERONCHANNEL=443;
   ERR_NOLOGIN=444;
   ERR_SUMMONDISABLED=445;
   ERR_USERSDISABLED=446;
   ERR_NOTREGISTERED=451;
   ERR_NEEDMOREPARAMS=461;
   ERR_ALREADYREGISTRED=462;
   ERR_NOPERMFORHOST=463;
   ERR_PASSWDMISMATCH=464;
   ERR_YOUREBANNEDCREEP=465;
   ERR_KEYSET=467;
   ERR_CHANNELISFULL=471;
   ERR_UNKNOWNMODE=472;
   ERR_INVITEONLYCHAN=473;
   ERR_BANNEDFROMCHAN=474;
   ERR_BADCHANNELKEY=475;
   ERR_BADCHANMASK=476;
   ERR_NOPRIVILEGES=481;
   ERR_CHANOPRIVSNEEDED=482;
   ERR_CANTKILLSERVER=483;
   ERR_NOOPERHOST=491;
   ERR_UMODEUNKNOWNFLAG=501;
   ERR_USERSDONTMATCH=502;
   StatusCodes:array[0..MaxStatusCodes-1] of record
      Code:Integer;
      Msg:string
   end =
   ((Code:ERR_NOSUCHNICK; Msg:' :No such nick/channel'),
    (Code:ERR_NOSUCHSERVER; Msg:' :No such server'),
    (Code:ERR_NOSUCHCHANNEL; Msg:' :No such channel'),
    (Code:ERR_CANNOTSENDTOCHAN; Msg:' :Cannot send to channel'),
    (Code:ERR_TOOMANYCHANNELS; Msg:' :You have joined too many channels'),
    (Code:ERR_WASNOSUCHNICK; Msg:' :There was no such nickname'),
    (Code:ERR_TOOMANYTARGETS; Msg:' :Duplicate recipients. No message delivered'),
    (Code:ERR_NOORIGIN; Msg:':No origin specified'),
    (Code:ERR_NORECIPIENT; Msg:':No recipient givin'),
    (Code:ERR_NOTEXTTOSEND; Msg:':No text to send'),
    (Code:ERR_NOTOPLEVEL; Msg:' :No toplevel domain specified'),
    (Code:ERR_WILDTOPLEVEL; Msg:' :Wildcard in toplevel domain'),
    (Code:ERR_UNKNOWNCOMMAND; Msg:' :Unknown command'),
    (Code:ERR_NOMOTD; Msg:':MOTD file is missing'),
    (Code:ERR_NOADMININFO; Msg:' :No administrative info available'),
    (Code:ERR_FILEERROR; Msg:':File error doing requested operation'),
    (Code:ERR_NONICKNAMEGIVEN; Msg:':No nickname given'),
    (Code:ERR_ERRONEUSNICKNAME; Msg:' :Erroneus nickname'),
    (Code:ERR_NICKNAMEINUSE; Msg:' :Nickname is already in use'),
    (Code:ERR_NICKCOLLISION; Msg:' :Nickname collision KILL'),
    (Code:ERR_USERNOTINCHANNEL; Msg:' :They are not on that channel'),
    (Code:ERR_NOTONCHANNEL; Msg:' :You are not on that channel'),
    (Code:ERR_USERONCHANNEL; Msg:' :is already on channel'),
    (Code:ERR_NOLOGIN; Msg:' :User not logged in'),
    (Code:ERR_SUMMONDISABLED; Msg:':SUMMON has been disabled'),
    (Code:ERR_USERSDISABLED; Msg:':USERS has been disabled'),
    (Code:ERR_NOTREGISTERED; Msg:':You have not registered'),
    (Code:ERR_NEEDMOREPARAMS; Msg:' :Not enough parameters'),
    (Code:ERR_ALREADYREGISTRED; Msg:':You may not reregister'),
    (Code:ERR_NOPERMFORHOST; Msg:':Your host is not among the privileged'),
    (Code:ERR_PASSWDMISMATCH; Msg:':Password incorrect'),
    (Code:ERR_YOUREBANNEDCREEP; Msg:':You are banned from this server'),
    (Code:ERR_KEYSET; Msg:' :Channel key already set'),
    (Code:ERR_CHANNELISFULL; Msg:' :Cannot join channel (+l)'),
    (Code:ERR_UNKNOWNMODE; Msg:' :is unknown mode char to me'),
    (Code:ERR_INVITEONLYCHAN; Msg:' :Cannot join channel (+i)'),
    (Code:ERR_BANNEDFROMCHAN; Msg:' :Cannot join channel (+b)'),
    (Code:ERR_BADCHANNELKEY; Msg:' :Cannot join channel (+k)'),
    (Code:ERR_BADCHANMASK; Msg:' :Bad Channel Name or Mask'),
    (Code:ERR_NOPRIVILEGES; Msg:':Permission Denied- You are not an IRC operator'),
    (Code:ERR_CHANOPRIVSNEEDED; Msg:' :You are not channel operator'),
    (Code:ERR_CANTKILLSERVER; Msg:':You cannot kill a server!'),
    (Code:ERR_NOOPERHOST; Msg:':No O-lines for your host'),
    (Code:ERR_UMODEUNKNOWNFLAG; Msg:':Unknown MODE flag'),
    (Code:ERR_USERSDONTMATCH; Msg:':Cannot change mode for other users')
    );


Type
  IRCTBasicEvent = procedure(ClientThread:TDXClientThread;Parm1:string) of object;
  IRCTMultipleEvent = procedure(ClientThread:TDXClientThread;Parm1:string) of object;
  IRCTComplexEvent = procedure(ClientThread:TDXClientThread;Parm1,Parm2:string) of object;
  IRCTSimpleEvent = procedure(ClientThread:TDXClientThread) of object;
  IRCTOtherEvent = procedure(ClientThread:TDXClientThread;Command,Parms:String;var Handled:Boolean) of object;

  TDXIRCServerCore = class(TDXServerCore)
  private
    fOnCommandPASS:IRCTBasicEvent;      // PASS <password>}
    fOnCommandNICK:IRCTMultipleEvent;   // NICK <nickname> [hopcount] (hopcount is from server to server)}
    fOnCommandUSER:IRCTMultipleEvent;   // USER <username> <hostname> <servername> <realname>}
    fOnCommandSERVER:IRCTMultipleEvent; // SERVER <servername> <hopcount> <info>}
    fOnCommandOPER:IRCTComplexEvent;    // OPER <user> <password>}
    fOnCommandQUIT:IRCTBasicEvent;      // QUIT [quit message]}
    fOnCommandSQUIT:IRCTComplexEvent;   // SQUIT <server> <comment>}
    // Channel Commands
    fOnCommandJOIN:IRCTMultipleEvent;   // JOIN <channel>[,<channel>...] [<key>[,<key>]]}
    fOnCommandPART:IRCTBasicEvent;      // PART <channel>[,<channel>...]}
    fOnCommandMODE:IRCTMultipleEvent;   // MODE <channel> [[+|-]o|p|s|i|t|n|b|v] [<limits>] [<user>] [<ban mask>]}
                                        // MODE <nickname> [[+|-]i|w|s|o]}
    fOnCommandTOPIC:IRCTComplexEvent;   // TOPIC <channel> [<topic>]}
    fOnCommandNAMES:IRCTBasicEvent;     // NAMES [<channel>[,<channel>...]]}
    fOnCommandLIST:IRCTComplexEvent;    // LIST [<channel>[,<channel>...]] [<server>]}
    fOnCommandINVITE:IRCTComplexEvent;  // INVITE <nickname> <channel>}
    fOnCommandKICK:IRCTMultipleEvent;   // KICK <channel>[,<channel>...] <user>[,<user>...] [<comment>]}
    // Server Commands
    fOnCommandVERSION:IRCTBasicEvent;   // VERSION [<server>]}
    fOnCommandSTATS:IRCTComplexEvent;   // STATS [<query> [<server>]]}
    fOnCommandLINKS:IRCTComplexEvent;   // LINKS [[<remote server>] <server mask>]}
    fOnCommandTIME:IRCTBasicEvent;      // TIME [<server>]}
    fOnCommandCONNECT:IRCTMultipleEvent;// CONNECT <target server> [<port> [<remote server>]]}
    fOnCommandTRACE:IRCTBasicEvent;     // TRACE [<server>]}
    fOnCommandADMIN:IRCTBasicEvent;     // ADMIN [<server>]}
    fOnCommandINFO:IRCTBasicEvent;      // INFO [<server>]}
    // Messages
    fOnCommandPRIVMSG:IRCTComplexEvent; // PRIVMSG <receiver>[,<receiver>...] <text>}
    fOnCommandNOTICE:IRCTComplexEvent;  // NOTICE <nickname> <text>}
    // User Based Queries
    fOnCommandWHO:IRCTComplexEvent;     // WHO [<name> [<o>]]}
    fOnCommandWHOIS:IRCTComplexEvent;   // WHOIS [<server>] <nickname>[,<nickname>...]}
    fOnCommandWHOWAS:IRCTMultipleEvent; // WHOWAS <nickname> [<count> [<server>]]}
    // Miscellaneous
    fOnCommandKILL:IRCTComplexEvent;    // KILL <nickname> <comment>}
    fOnCommandPING:IRCTComplexEvent;    // PING <server1> [<server2>]}
    fOnCommandPONG:IRCTComplexEvent;    // PONG <daemon1> [<daemon2>]}
    fOnCommandAWAY:IRCTBasicEvent;      // AWAY [<message>]}
    fOnCommandREHASH:IRCTSimpleEvent;   // REHASH}
    fOnCommandRESTART:IRCTSimpleEvent;  // RESTART}
    fOnCommandSUMMON:IRCTComplexEvent;  // SUMMON <user> [<server>]}
    fOnCommandUSERS:IRCTBasicEvent;     // USERS [<server>]}
    fOnCommandWALLOPS:IRCTBasicEvent;   // WALLOPS <text>}
    fOnCommandUSERHOST:IRCTComplexEvent;// USERHOST <nickname> [<nickname>]}
    fOnCommandISON:IRCTComplexEvent;    // ISON <nickname> [<nickname>]}
    fOnCommandERROR:IRCTMultipleEvent;  // ERROR <message> from server to server
    fOnCommandOther:IRCTOtherEvent;
    fOnCommandTimeout:IRCTSimpleEvent;  // server should send PING
    // new as of 3.0a:
    fServerName:String;
  protected
    Procedure SetOnCommandPASS(Value:IRCTBasicEvent);
    Procedure SetOnCommandNICK(Value:IRCTMultipleEvent);
    Procedure SetOnCommandUSER(Value:IRCTMultipleEvent);
    Procedure SetOnCommandSERVER(Value:IRCTMultipleEvent);
    Procedure SetOnCommandOPER(Value:IRCTComplexEvent);
    Procedure SetOnCommandQUIT(Value:IRCTBasicEvent);
    Procedure SetOnCommandSQUIT(Value:IRCTComplexEvent);
    Procedure SetOnCommandJOIN(Value:IRCTMultipleEvent);
    Procedure SetOnCommandPART(Value:IRCTBasicEvent);
    Procedure SetOnCommandMODE(Value:IRCTMultipleEvent);
    Procedure SetOnCommandTOPIC(Value:IRCTComplexEvent);
    Procedure SetOnCommandNAMES(Value:IRCTBasicEvent);
    Procedure SetOnCommandLIST(Value:IRCTComplexEvent);
    Procedure SetOnCommandINVITE(Value:IRCTComplexEvent);
    Procedure SetOnCommandKICK(Value:IRCTMultipleEvent);
    Procedure SetOnCommandVERSION(Value:IRCTBasicEvent);
    Procedure SetOnCommandSTATS(Value:IRCTComplexEvent);
    Procedure SetOnCommandLINKS(Value:IRCTComplexEvent);
    Procedure SetOnCommandTIME(Value:IRCTBasicEvent);
    Procedure SetOnCommandCONNECT(Value:IRCTMultipleEvent);
    Procedure SetOnCommandTRACE(Value:IRCTBasicEvent);
    Procedure SetOnCommandADMIN(Value:IRCTBasicEvent);
    Procedure SetOnCommandINFO(Value:IRCTBasicEvent);
    Procedure SetOnCommandPRIVMSG(Value:IRCTComplexEvent);
    Procedure SetOnCommandNOTICE(Value:IRCTComplexEvent);
    Procedure SetOnCommandWHO(Value:IRCTComplexEvent);
    Procedure SetOnCommandWHOIS(Value:IRCTComplexEvent);
    Procedure SetOnCommandWHOWAS(Value:IRCTMultipleEvent);
    Procedure SetOnCommandKILL(Value:IRCTComplexEvent);
    Procedure SetOnCommandPING(Value:IRCTComplexEvent);
    Procedure SetOnCommandPONG(Value:IRCTComplexEvent);
    Procedure SetOnCommandAWAY(Value:IRCTBasicEvent);
    Procedure SetOnCommandREHASH(Value:IRCTSimpleEvent);
    Procedure SetOnCommandRESTART(Value:IRCTSimpleEvent);
    Procedure SetOnCommandSUMMON(Value:IRCTComplexEvent);
    Procedure SetOnCommandUSERS(Value:IRCTBasicEvent);
    Procedure SetOnCommandWALLOPS(Value:IRCTBasicEvent);
    Procedure SetOnCommandUSERHOST(Value:IRCTComplexEvent);
    Procedure SetOnCommandISON(Value:IRCTComplexEvent);
    Procedure SetOnCommandERROR(Value:IRCTMultipleEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread:TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:IRCTBasicEvent);
    Procedure AddSimpleEvent(Command:String;EventProc:IRCTSimpleEvent);
    Procedure AddComplexEvent(Command:String;EventProc:IRCTComplexEvent);
    Procedure AddMultipleEvent(Command:String;EventProc:IRCTMultipleEvent);
    Procedure SayError(ErrorCode:Integer;OptionalText:String;ClientThread:TDXClientThread); // 3.0a
    Procedure SayWelcome(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
    Procedure SayMOTD(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
    Procedure SayNOTICE(Nick,Text:String;ClientThread:TDXClientThread); // 4.0
    Procedure SayChannelList(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
  published
    property OnCommandPASS: IRCTBasicEvent read fOnCommandPASS
                                           write SetOnCommandPASS;
    property OnCommandNICK: IRCTMultipleEvent read fOnCommandNICK
                                              write SetOnCommandNICK;
    property OnCommandUSER: IRCTMultipleEvent read fOnCommandUSER
                                              write SetOnCommandUSER;
    property OnCommandSERVER: IRCTMultipleEvent read fOnCommandSERVER
                                                write SetOnCommandSERVER;
    property OnCommandOPER: IRCTComplexEvent read fOnCommandOPER
                                             write SetOnCommandOPER;
    property OnCommandQUIT: IRCTBasicEvent read fOnCommandQUIT
                                           write SetOnCommandQUIT;
    property OnCommandSQUIT: IRCTComplexEvent read fOnCommandSQUIT
                                              write SetOnCommandSQUIT;
    property OnCommandJOIN: IRCTMultipleEvent read fOnCommandJOIN
                                              write SetOnCommandJOIN;
    property OnCommandPART: IRCTBasicEvent read fOnCommandPART
                                           write SetOnCommandPART;
    property OnCommandMODE: IRCTMultipleEvent read fOnCommandMODE
                                              write SetOnCommandMODE;
    property OnCommandTOPIC: IRCTComplexEvent read fOnCommandTOPIC
                                             write SetOnCommandTOPIC;
    property OnCommandNAMES: IRCTBasicEvent read fOnCommandNAMES
                                             write SetOnCommandNAMES;
    property OnCommandLIST: IRCTComplexEvent read fOnCommandLIST
                                             write SetOnCommandLIST;
    property OnCommandINVITE: IRCTComplexEvent read fOnCommandINVITE
                                               write SetOnCommandINVITE;
    property OnCommandKICK: IRCTMultipleEvent read fOnCommandKICK
                                              write SetOnCommandKICK;
    property OnCommandVERSION: IRCTBasicEvent read fOnCommandVERSION
                                              write SetOnCommandVERSION;
    property OnCommandSTATS: IRCTComplexEvent read fOnCommandSTATS
                                              write SetOnCommandSTATS;
    property OnCommandLINKS: IRCTComplexEvent read fOnCommandLINKS
                                              write SetOnCommandLINKS;
    property OnCommandTIME: IRCTBasicEvent read fOnCommandTIME
                                           write SetOnCommandTIME;
    property OnCommandCONNECT: IRCTMultipleEvent read fOnCommandCONNECT
                                                 write SetOnCommandCONNECT;
    property OnCommandTRACE: IRCTBasicEvent read fOnCommandTRACE
                                            write SetOnCommandTRACE;
    property OnCommandADMIN: IRCTBasicEvent read fOnCommandADMIN
                                            write SetOnCommandADMIN;
    property OnCommandINFO: IRCTBasicEvent read fOnCommandINFO
                                           write SetOnCommandINFO;
    property OnCommandPRIVMSG: IRCTComplexEvent read fOnCommandPRIVMSG
                                                write SetOnCommandPRIVMSG;
    property OnCommandNOTICE: IRCTComplexEvent read fOnCommandNOTICE
                                               write SetOnCommandNOTICE;
    property OnCommandWHO: IRCTComplexEvent read fOnCommandWHO
                                            write SetOnCommandWHO;
    property OnCommandWHOIS: IRCTComplexEvent read fOnCommandWHOIS
                                              write SetOnCommandWHOIS;
    property OnCommandWHOWAS: IRCTMultipleEvent read fOnCommandWHOWAS
                                                write SetOnCommandWHOWAS;
    property OnCommandKILL: IRCTComplexEvent read fOnCommandKILL
                                             write SetOnCommandKILL;
    property OnCommandPING: IRCTComplexEvent read fOnCommandPING
                                             write SetOnCommandPING;
    property OnCommandPONG: IRCTComplexEvent read fOnCommandPONG
                                             write SetOnCommandPONG;
    property OnCommandAWAY: IRCTBasicEvent read fOnCommandAWAY
                                           write SetOnCommandAWAY;
    property OnCommandREHASH: IRCTSimpleEvent read fOnCommandREHASH
                                              write SetOnCommandREHASH;
    property OnCommandRESTART: IRCTSimpleEvent read fOnCommandRESTART
                                               write SetOnCommandRESTART;
    property OnCommandSUMMON: IRCTComplexEvent read fOnCommandSUMMON
                                               write SetOnCommandSUMMON;
    property OnCommandUSERS: IRCTBasicEvent read fOnCommandUSERS
                                            write SetOnCommandUSERS;
    property OnCommandWALLOPS: IRCTBasicEvent read fOnCommandWALLOPS
                                              write SetOnCommandWALLOPS;
    property OnCommandUSERHOST: IRCTComplexEvent read fOnCommandUSERHOST
                                                 write SetOnCommandUSERHOST;
    property OnCommandISON: IRCTComplexEvent read fOnCommandISON
                                             write SetOnCommandISON;
    property OnCommandERROR: IRCTMultipleEvent read fOnCommandERROR
                                             write SetOnCommandERROR;
    property OnCommandOther: IRCTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
    property ServerName:String read fServerName
                               write fServerName; // 3.0a
    property OnReadTimeout:IRCTSimpleEvent read fOnCommandTimeout
                                           write fOnCommandTimeout; // 3.0a
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PIRCBasicEvent=^TIRCBasicEvent;
  TIRCBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IRCTBasicEvent;
  End;
  PIRCSimpleEvent=^TIRCSimpleEvent;
  TIRCSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IRCTSimpleEvent;
  End;
  PIRCComplexEvent=^TIRCComplexEvent;
  TIRCComplexEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IRCTComplexEvent;
  End;
  PIRCMultipleEvent=^TIRCMultipleEvent;
  TIRCMultipleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:IRCTMultipleEvent;
  End;

///////////////////////////////////////////////////////////////////////////////
// RFC 1459...
//   PASS <password>
//   NICK <nickname> [hopcount] (hopcount is from server to server)
//   USER <username> <hostname> <servername> <realname>
//   SERVER <servername> <hopcount> <info>
//   OPER <user> <password>
//   QUIT [quit message]
//   SQUIT <server> <comment>
//
//   // Channel Commands
//   JOIN <channel>[,<channel>...] [<key>[,<key>]]
//   PART <channel>[,<channel>...]
//   MODE <channel> [[+|-]o|p|s|i|t|n|b|v] [<limits>] [<user>] [<ban mask>]
//   MODE <nickname> [[+|-]i|w|s|o]
//   TOPIC <channel> [<topic>]
//   NAMES [<channel>[,<channel>...]]
//   LIST [<channel>[,<channel>...]] [<server>]
//   INVITE <nickname> <channel>
//   KICK <channel>[,<channel>...] <user>[,<user>...] [<comment>]
//
//   // Server Commands
//   VERSION [<server>]
//   STATS [<query> [<server>]]
//   LINKS [[<remote server>] <server mask>]
//   TIME [<server>]
//   CONNECT <target server> [<port> [<remote server>]]
//   TRACE [<server>]
//   ADMIN [<server>]
//   INFO [<server>]
//
//   // Messages
//   PRIVMSG <receiver>[,<receiver>...] <text>
//   NOTICE <nickname> <text>
//
//   // User Based Queries
//   WHO [<name> [<o>]]
//   WHOIS [<server>] <nickname>[,<nickname>...]
//   WHOWAS <nickname> [<count> [<server>]]
//
//   // Miscellaneous
//   KILL <nickname> <comment>
//   PING <server1> [<server2>]
//   PONG <daemon1> [<daemon2>]
//   AWAY [<message>]
//   REHASH
//   RESTART
//   SUMMON <user> [<server>]
//   USERS [<server>]
//   WALLOPS <text>
//   USERHOST <nickname> [<nickname>]
//   ISON <nickname> [<nickname>]
///////////////////////////////////////////////////////////////////////////////

constructor TDXIRCServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=6667;
   fEventArray:=TList.Create;
end;

destructor TDXIRCServerCore.Destroy;
Var
   PBasicEvent:PIRCBasicEvent;
   PSimpleEvent:PIRCSimpleEvent;
   PComplexEvent:PIRCComplexEvent;
   PMultipleEvent:PIRCMultipleEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PIRCBasicEvent(fEventArray[0]).Tag of
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
              PMultipleEvent:=fEventArray[0];
              Dispose(PMultipleEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
      fEventArray.Free;
      fEventArray:=Nil;
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
Procedure TDXIRCServerCore.AddBasicEvent(Command:String;EventProc:IRCTBasicEvent);
Var
   PBasicEvent:PIRCBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIRCBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PIRCBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
//              Example Usage: AddSimpleEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXIRCServerCore.AddSimpleEvent(Command:String;EventProc:IRCTSimpleEvent);
Var
   PSimpleEvent:PIRCSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIRCSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PIRCSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
//              Example Usage: AddComplexEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXIRCServerCore.AddComplexEvent(Command:String;EventProc:IRCTComplexEvent);
Var
   PComplexEvent:PIRCComplexEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIRCComplexEvent(fEventArray[Loop]).Command=Command then Begin
         PIRCComplexEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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
//ADDMULTIPLEEVENT:
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
//              Example Usage: AddMULTIPLEEvent('CDROM',MySpecialEvent);
///////////////////////////////////////////////////////////////////////////////
Procedure TDXIRCServerCore.AddMultipleEvent(Command:String;EventProc:IRCTMultipleEvent);
Var
   PMultipleEvent:PIRCMultipleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PIRCMultipleEvent(fEventArray[Loop]).Command=Command then Begin
         PIRCMultipleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PMultipleEvent);
   PMultipleEvent.Tag:=4;      // Denotes Event in fEventArray is a TBasicEvent!
   PMultipleEvent.Command:=Command;
   PMultipleEvent.EventProcedure:=EventProc;
   fEventArray.Add(PMultipleEvent);
End;

Procedure TDXIRCServerCore.SetOnCommandPASS(Value:IRCTBasicEvent);
Begin
   fOnCommandPASS:=Value;
   AddBasicEvent('PASS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandNICK(Value:IRCTMultipleEvent);
Begin
   fOnCommandNICK:=Value;
   AddMultipleEvent('NICK',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandUSER(Value:IRCTMultipleEvent);
Begin
   fOnCommandUSER:=Value;
   AddMultipleEvent('USER',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandSERVER(Value:IRCTMultipleEvent);
Begin
   fOnCommandSERVER:=Value;
   AddMultipleEvent('SERVER',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandOPER(Value:IRCTComplexEvent);
Begin
   fOnCommandOPER:=Value;
   AddComplexEvent('OPER',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandQUIT(Value:IRCTBasicEvent);
Begin
   fOnCommandQUIT:=Value;
   AddBasicEvent('QUIT',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandSQUIT(Value:IRCTComplexEvent);
Begin
   fOnCommandSQUIT:=Value;
   AddComplexEvent('SQUIT',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandJOIN(Value:IRCTMultipleEvent);
Begin
   fOnCommandJOIN:=Value;
   AddMultipleEvent('JOIN',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandPART(Value:IRCTBasicEvent);
Begin
   fOnCommandPART:=Value;
   AddBasicEvent('PART',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandMODE(Value:IRCTMultipleEvent);
Begin
   fOnCommandMODE:=Value;
   AddMultipleEvent('MODE',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandTOPIC(Value:IRCTComplexEvent);
Begin
   fOnCommandTOPIC:=Value;
   AddComplexEvent('TOPIC',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandNAMES(Value:IRCTBasicEvent);
Begin
   fOnCommandNAMES:=Value;
   AddBasicEvent('NAMES',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandLIST(Value:IRCTComplexEvent);
Begin
   fOnCommandLIST:=Value;
   AddComplexEvent('LIST',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandINVITE(Value:IRCTComplexEvent);
Begin
   fOnCommandINVITE:=Value;
   AddComplexEvent('INVITE',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandKICK(Value:IRCTMultipleEvent);
Begin
   fOnCommandKICK:=Value;
   AddMultipleEvent('KICK',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandVERSION(Value:IRCTBasicEvent);
Begin
   fOnCommandVERSION:=Value;
   AddBasicEvent('VERSION',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandSTATS(Value:IRCTComplexEvent);
Begin
   fOnCommandSTATS:=Value;
   AddComplexEvent('STATS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandLINKS(Value:IRCTComplexEvent);
Begin
   fOnCommandLINKS:=Value;
   AddComplexEvent('LINKS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandTIME(Value:IRCTBasicEvent);
Begin
   fOnCommandTIME:=Value;
   AddBasicEvent('TIME',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandCONNECT(Value:IRCTMultipleEvent);
Begin
   fOnCommandCONNECT:=Value;
   AddMultipleEvent('CONNECT',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandTRACE(Value:IRCTBasicEvent);
Begin
   fOnCommandTRACE:=Value;
   AddBasicEvent('TRACE',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandADMIN(Value:IRCTBasicEvent);
Begin
   fOnCommandADMIN:=Value;
   AddBasicEvent('ADMIN',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandINFO(Value:IRCTBasicEvent);
Begin
   fOnCommandINFO:=Value;
   AddBasicEvent('INFO',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandPRIVMSG(Value:IRCTComplexEvent);
Begin
   fOnCommandPRIVMSG:=Value;
   AddComplexEvent('PRIVMSG',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandNOTICE(Value:IRCTComplexEvent);
Begin
   fOnCommandNOTICE:=Value;
   AddComplexEvent('NOTICE',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandWHO(Value:IRCTComplexEvent);
Begin
   fOnCommandWHO:=Value;
   AddComplexEvent('WHO',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandWHOIS(Value:IRCTComplexEvent);
Begin
   fOnCommandWHOIS:=Value;
   AddComplexEvent('WHOIS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandWHOWAS(Value:IRCTMultipleEvent);
Begin
   fOnCommandWHOWAS:=Value;
   AddMultipleEvent('WHOWAS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandKILL(Value:IRCTComplexEvent);
Begin
   fOnCommandKILL:=Value;
   AddComplexEvent('KILL',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandPING(Value:IRCTComplexEvent);
Begin
   fOnCommandPING:=Value;
   AddComplexEvent('PING',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandPONG(Value:IRCTComplexEvent);
Begin
   fOnCommandPONG:=Value;
   AddComplexEvent('PONG',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandAWAY(Value:IRCTBasicEvent);
Begin
   fOnCommandAWAY:=Value;
   AddBasicEvent('AWAY',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandREHASH(Value:IRCTSimpleEvent);
Begin
   fOnCommandREHASH:=Value;
   AddSimpleEvent('REHASH',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandRESTART(Value:IRCTSimpleEvent);
Begin
   fOnCommandRESTART:=Value;
   AddSimpleEvent('RESTART',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandSUMMON(Value:IRCTComplexEvent);
Begin
   fOnCommandSUMMON:=Value;
   AddComplexEvent('SUMMON',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandUSERS(Value:IRCTBasicEvent);
Begin
   fOnCommandUSERS:=Value;
   AddBasicEvent('USERS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandWALLOPS(Value:IRCTBasicEvent);
Begin
   fOnCommandWALLOPS:=Value;
   AddBasicEvent('WALLOPS',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandUSERHOST(Value:IRCTComplexEvent);
Begin
   fOnCommandUSERHOST:=Value;
   AddComplexEvent('USERHOST',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandISON(Value:IRCTComplexEvent);
Begin
   fOnCommandISON:=Value;
   AddComplexEvent('ISON',Value);
End;

Procedure TDXIRCServerCore.SetOnCommandERROR(Value:IRCTMultipleEvent);
Begin
   fOnCommandERROR:=Value;
   AddMultipleEvent('ERROR',Value);
End;

Procedure TDXIRCServerCore.SayError(ErrorCode:Integer;OptionalText:String;ClientThread:TDXClientThread); // 3.0a
Var
  Ws:String;
  Loop:Integer;

Begin
   For Loop:=High(StatusCodes) downto Low(StatusCodes) do Begin
      If StatusCodes[Loop].Code=ErrorCode then Begin
         Ws:=StatusCodes[Loop].Msg;
         Break;
      End;
   End;
   If Copy(Ws,1,1)=#32 then Ws:=OptionalText+Ws;
   ClientThread.Socket.Writeln(':'+fServerName+#32+Ws);
End;

Procedure TDXIRCServerCore.SayWelcome(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
Var
   Loop:Integer;
   Ws:String;

Begin
   For Loop:=1 to Text.Count do Begin
      Ws:=IntToCommaStr(Loop);
      While Length(Ws)<3 do Ws:='0'+Ws;
      ClientThread.Socket.Writeln(':'+fServername+#32+Ws+#32+Nick+' :'+Text[Loop-1]);
   End;
End;

Procedure TDXIRCServerCore.SayMOTD(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
Var
   Loop:Integer;

Begin
   ClientThread.Socket.Writeln(':'+fServername+' 375 '+Nick+' :- '+fServerName+' Message of the Day');

   For Loop:=1 to Text.Count do Begin
      ClientThread.Socket.Writeln(':'+fServername+' 372 '+Nick+' :'+Text[Loop-1]);
   End;

   ClientThread.Socket.Writeln(':'+fServername+' 376 '+Nick+' :End of /MOTD Command.');
End;

Procedure TDXIRCServerCore.SayNOTICE(Nick,Text:String;ClientThread:TDXClientThread); // 4.0
Begin
   ClientThread.Socket.Writeln(':'+fServername+' NOTICE '+Nick+' :'+Text);
End;

Procedure TDXIRCServerCore.SayChannelList(Nick:String;Text:TStrings;ClientThread:TDXClientThread); // 4.0
Var
   Loop:Integer;

Begin
   ClientThread.Socket.Writeln(':'+fServername+' 321 '+Nick+' Channel :Users  Name');

   For Loop:=1 to Text.Count do Begin
      ClientThread.Socket.Writeln(':'+fServername+' 322 '+Nick+#32+Text[Loop-1]);
   End;

   ClientThread.Socket.Writeln(':'+fServername+' 323 '+Nick+' :End of /LIST Command.');
End;

procedure TDXIRCServerCore.ProcessSession(ClientThread:TDXClientThread);
var
  s,sCmd:string;
  Loop:Integer;
  WasHandled:Boolean;
  OutData:Pointer;
  PINGTries:Integer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while connected do begin
         if fbForceAbort then Exit;
         PINGTries:=0;
         S:='';
         While (S='') do Begin
            s:=Readln(Timeout);
            If Not ValidSocket then Exit;
            If LastReadTimeout then Begin
               If Assigned(fOnCommandTimeout) then
                  fOnCommandTimeout(ClientThread)
               Else Begin
                  If PingTries<1 then Begin
                     ClientThread.Socket.Writeln('PING :'+fServerName);
                     Inc(PingTries);
                  End
                  Else Begin
                     ClientThread.Socket.Writeln('ERROR :Closing Link: ['+
                        ClientThread.Socket.PeerIPAddress+'] 011 (Ping timeout)');
                     Exit;
                  End;
               End;
            End;
         End;
         If Assigned(OnFilter) then Begin
            Loop:=FilterRead(@S[1],OutData,Length(S),ClientThread);
            SetLength(S,Loop);
            If Assigned(OutData) then Begin
               Move(TDXBSArray(OutData^),S[1],Loop);
               OnFilter(ddFreePointer,Nil,OutData,Loop,Loop,WasHandled,ClientThread);
            End;
         End;
         sCmd:=UpperCase(Fetch(S,#32,False));
         Loop:=0;
         WasHandled:=False;
         If sCmd<>'' then {timeout}
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PIRCBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PIRCBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PIRCBasicEvent(fEventArray[Loop]).EventProcedure) then
                       IRCTBasicEvent(PIRCBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread,s);
                  2:if Assigned(PIRCSimpleEvent(fEventArray[Loop]).EventProcedure) then
                       IRCTSimpleEvent(PIRCSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  3:if Assigned(PIRCComplexEvent(fEventArray[Loop]).EventProcedure) then Begin
                       sCmd:=FetchByChar(S,#32,False);
                       IRCTComplexEvent(PIRCComplexEvent(fEventArray[Loop]).EventProcedure)(ClientThread,sCmd,S);
                  End;
                  4:if Assigned(PIRCMultipleEvent(fEventArray[Loop]).EventProcedure) then
                       IRCTMultipleEvent(PIRCMultipleEvent(fEventArray[Loop]).EventProcedure)(ClientThread,s);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If sCMD='QUIT' then Exit;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread,sCmd,S,WasHandled);
         end;
         if not WasHandled then
            Writeln(':'+fServerName+' 421 '+sCmd+' :Unknown command');
      end;
   end;
end;

end.

