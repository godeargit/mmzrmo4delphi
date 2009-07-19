unit DXDictionaryServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
//    Component: TDXDictionaryServerCore
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
//  Description: implements the Dictionary lookup protocol
// ========================================================================
///////////////////////////////////////////////////////////////////////////////

uses
  Classes,
  DXServerCore;

{$I DXSock.def}

type
  DictTBasicEvent = procedure(ClientThread: TDXClientThread) of object;
  DictTOptionEvent = procedure(ClientThread: TDXClientThread; Command,Parm: string) of object;
  DictTDefineEvent = procedure(ClientThread: TDXClientThread; Database,WordToFind:String) of object;
  DictTMatchEvent = procedure(ClientThread: TDXClientThread; Database,Strategy,WordToFind:String) of object;
  DictTShowEvent = procedure(ClientThread: TDXClientThread; Command:String) of object;
  DictTAuthEvent = procedure(ClientThread: TDXClientThread; Username,authstring:String) of object;
  DictTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string; Handled: Boolean) of object;

  TDXDictionaryServerCore = class(TDXServerCore)
  private
    fOnCommandAUTH: DictTAuthEvent;
    fOnCommandCLIENT: DictTShowEvent;
    fOnCommandDEFINE: DictTDefineEvent;
    fOnCommandHELP: DictTBasicEvent;
    fOnCommandMATCH: DictTMatchEvent;
    fOnCommandOPTION: DictTOptionEvent;
    fOnCommandQUIT: DictTBasicEvent;
    fOnCommandSASLAUTH: DictTAuthEvent;
    fOnCommandSHOW: DictTShowEvent;
    fOnCommandSTAT: DictTBasicEvent;
    fOnCommandOther: DictTOtherEvent;
  protected
    Procedure SetOnCommandAUTH(value:DictTAuthEvent);
    Procedure SetOnCommandCLIENT(value:DictTShowEvent);
    Procedure SetOnCommandDEFINE(value:DictTDefineEvent);
    Procedure SetOnCommandHELP(value:DictTBasicEvent);
    Procedure SetOnCommandMATCH(value:DictTMatchEvent);
    Procedure SetOnCommandOPTION(value:DictTOptionEvent);
    Procedure SetOnCommandQUIT(value:DictTBasicEvent);
    Procedure SetOnCommandSASLAUTH(value:DictTAuthEvent);
    Procedure SetOnCommandSHOW(value:DictTShowEvent);
    Procedure SetOnCommandSTAT(value:DictTBasicEvent);
  public
    constructor Create(AOwner:TComponent); {$IFNDEF OBJECTS_ONLY} override; {$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:DictTBasicEvent);
    Procedure AddDefineEvent(Command:String;EventProc:DictTDefineEvent);
    Procedure AddMatchEvent(Command:String;EventProc:DictTMatchEvent);
    Procedure AddShowEvent(Command:String;EventProc:DictTShowEvent);
    Procedure AddAuthEvent(Command:String;EventProc:DictTAuthEvent);
    Procedure AddOptionEvent(Command:String;EventProc:DictTOptionEvent);
  published
    property OnCommandAUTH: DictTAuthEvent read fOnCommandAuth
                                           write SetOnCommandAuth;
    property OnCommandCLIENT: DictTShowEvent read fOnCommandClient
                                             write SetOnCommandClient;
    property OnCommandDEFINE: DictTDefineEvent read fOnCommandDefine
                                                write SetOnCommandDefine;
    property OnCommandHELP: DictTBasicEvent read fOnCommandHelp
                                            write SetOnCommandHelp;
    property OnCommandMATCH: DictTMatchEvent read fOnCommandMatch
                                             write SetOnCommandMatch;
    property OnCommandOPTION: DictTOptionEvent read fOnCommandOption
                                              write SetOnCommandOption;
    property OnCommandQUIT: DictTBasicEvent read fOnCommandQuit
                                          write SetOnCommandQuit;
    property OnCommandSASLAUTH: DictTAuthEvent read fOnCommandSaslAuth
                                               write SetOnCommandSaslAuth;
    property OnCommandSHOW: DictTShowEvent read fOnCommandShow
                                           write SetOnCommandShow;
    property OnCommandSTAT: DictTBasicEvent read fOnCommandStat
                                          write SetOnCommandStat;
    property OnCommandOther: DictTOtherEvent read fOnCommandOther
                                             write fOnCommandOther;
  end;

implementation

Uses
   DXSock,
   DXString;

Type
  PDICTBasicEvent=^TDICTBasicEvent;
  TDICTBasicEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTBasicEvent;
  End;
  PDICTDefineEvent=^TDICTDefineEvent;
  TDICTDefineEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTDefineEvent;
  End;
  PDICTMatchEvent=^TDICTMatchEvent;
  TDICTMatchEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTMatchEvent;
  End;
  PDICTShowEvent=^TDICTShowEvent;
  TDICTShowEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTShowEvent;
  End;
  PDICTAuthEvent=^TDICTAuthEvent;
  TDICTAuthEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTAuthEvent;
  End;
  PDICTOptionEvent=^TDICTOptionEvent;
  TDICTOptionEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DICTTOptionEvent;
  End;

constructor TDXDictionaryServerCore.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ServerPort:=2628;
end;

destructor TDXDictionaryServerCore.Destroy;
Var
   PBasicEvent:PDictBasicEvent;
   PDefineEvent:PDictDefineEvent;
   PMatchEvent:PDictMatchEvent;
   PShowEvent:PDictShowEvent;
   PAuthEvent:PDictAuthEvent;
   POptionEvent:PDictOptionEvent;

begin
  If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PDictBasicEvent(fEventArray[0]).Tag of
            1:Begin
              PBasicEvent:=fEventArray[0];
              Dispose(PBasicEvent);
            End;
            2:Begin
              PDefineEvent:=fEventArray[0];
              Dispose(PDefineEvent);
            End;
            3:Begin
              PMatchEvent:=fEventArray[0];
              Dispose(PMatchEvent);
            End;
            4:Begin
              PShowEvent:=fEventArray[0];
              Dispose(PShowEvent);
            End;
            5:Begin
              PAuthEvent:=fEventArray[0];
              Dispose(PAuthEvent);
            End;
            6:Begin
              POptionEvent:=fEventArray[0];
              Dispose(POptionEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXDictionaryServerCore.AddBasicEvent(Command:String;EventProc:DICTTBasicEvent);
Var
   PBasicEvent:PDICTBasicEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTBasicEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTBasicEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

Procedure TDXDictionaryServerCore.AddDefineEvent(Command:String;EventProc:DICTTDefineEvent);
Var
   PDefineEvent:PDICTDefineEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTDefineEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTDefineEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PDefineEvent);
   PDefineEvent.Tag:=2;      // Denotes Event in fEventArray is a TDefineEvent!
   PDefineEvent.Command:=Command;
   PDefineEvent.EventProcedure:=EventProc;
   fEventArray.Add(PDefineEvent);
End;

Procedure TDXDictionaryServerCore.AddMatchEvent(Command:String;EventProc:DICTTMatchEvent);
Var
   PMatchEvent:PDICTMatchEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTMatchEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTMatchEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PMatchEvent);
   PMatchEvent.Tag:=3;      // Denotes Event in fEventArray is a TMatchEvent!
   PMatchEvent.Command:=Command;
   PMatchEvent.EventProcedure:=EventProc;
   fEventArray.Add(PMatchEvent);
End;

Procedure TDXDictionaryServerCore.AddShowEvent(Command:String;EventProc:DICTTShowEvent);
Var
   PShowEvent:PDICTShowEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTShowEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTShowEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PShowEvent);
   PShowEvent.Tag:=4;      // Denotes Event in fEventArray is a TShowEvent!
   PShowEvent.Command:=Command;
   PShowEvent.EventProcedure:=EventProc;
   fEventArray.Add(PShowEvent);
End;

Procedure TDXDictionaryServerCore.AddAuthEvent(Command:String;EventProc:DICTTAuthEvent);
Var
   PAuthEvent:PDICTAuthEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTAuthEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTAuthEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(PAuthEvent);
   PAuthEvent.Tag:=5;      // Denotes Event in fEventArray is a TAuthEvent!
   PAuthEvent.Command:=Command;
   PAuthEvent.EventProcedure:=EventProc;
   fEventArray.Add(PAuthEvent);
End;

Procedure TDXDictionaryServerCore.AddOptionEvent(Command:String;EventProc:DICTTOptionEvent);
Var
   POptionEvent:PDICTOptionEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDICTOptionEvent(fEventArray[Loop]).Command=Command then Begin
         PDICTOptionEvent(fEventArray[Loop]).EventProcedure:=EventProc;
         Exit;
      End
      Else Inc(Loop);
   End;
   New(POptionEvent);
   POptionEvent.Tag:=6;      // Denotes Event in fEventArray is a TOptionEvent!
   POptionEvent.Command:=Command;
   POptionEvent.EventProcedure:=EventProc;
   fEventArray.Add(POptionEvent);
End;

Procedure TDXDictionaryServerCore.SetOnCommandAUTH(value:DictTAuthEvent);
Begin
   fOnCommandAUTH:=Value;
   AddAuthEvent('AUTH',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandCLIENT(value:DictTShowEvent);
Begin
   fOnCommandCLIENT:=Value;
   AddShowEvent('CLIENT',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandDEFINE(value:DictTDefineEvent);
Begin
   fOnCommandDEFINE:=Value;
   AddDefineEvent('DEFINE',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandHELP(value:DictTBasicEvent);
Begin
   fOnCommandHELP:=Value;
   AddBasicEvent('HELP',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandMATCH(value:DictTMatchEvent);
Begin
   fOnCommandMATCH:=Value;
   AddMatchEvent('MATCH',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandOPTION(value:DictTOptionEvent);
Begin
   fOnCommandOPTION:=Value;
   AddOptionEvent('OPTION',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandQUIT(value:DictTBasicEvent);
Begin
   fOnCommandQUIT:=Value;
   AddBasicEvent('QUIT',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandSASLAUTH(value:DictTAuthEvent);
Begin
   fOnCommandSASLAUTH:=Value;
   AddAuthEvent('SASLAUTH',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandSHOW(value:DictTShowEvent);
Begin
   fOnCommandSHOW:=Value;
   AddShowEvent('SHOW',Value);
End;

Procedure TDXDictionaryServerCore.SetOnCommandSTAT(value:DictTBasicEvent);
Begin
   fOnCommandSTAT:=Value;
   AddBasicEvent('STAT',Value);
   AddBasicEvent('STATUS',Value);
End;

procedure TDXDictionaryServerCore.ProcessSession(ClientThread: TDXClientThread);
var
  s, sCmd: string;
  WasHandled: Boolean;
  Loop:Integer;
  OutData:Pointer;

begin
   fbForceAbort:=False;
   with ClientThread.Socket do begin
      while ValidSocket do begin
         If fbForceAbort then Exit;
         S:=ReadLn(Timeout);
         If LastReadTimeout or Not ValidSocket then Exit;
         if S='' then continue;
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
         While (Loop<fEventArray.Count) and (Not WasHandled) do Begin
            If PDICTBasicEvent(fEventArray[Loop]).Command=sCMD then Begin
               Case PDICTBasicEvent(fEventArray[Loop]).Tag of
                  1:if Assigned(PDICTBasicEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTBasicEvent(PDICTBasicEvent(fEventArray[Loop]).EventProcedure)(ClientThread);
                  2:if Assigned(PDICTDefineEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTDefineEvent(PDICTDefineEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
                  3:if Assigned(PDICTMatchEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTMatchEvent(PDICTMatchEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),Fetch(S,#32,False),S);
                  4:if Assigned(PDICTShowEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTShowEvent(PDICTShowEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
                  5:if Assigned(PDICTAuthEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTAuthEvent(PDICTAuthEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
                  6:if Assigned(PDICTOptionEvent(fEventArray[Loop]).EventProcedure) then
                       DICTTOptionEvent(PDICTOptionEvent(fEventArray[Loop]).EventProcedure)(ClientThread,Fetch(S,#32,False),S);
               End;
               WasHandled:=True;
            End
            Else Inc(Loop);
         End;
         If sCMD='QUIT' then Exit;
         If Not WasHandled then Begin
            if assigned(OnCommandOther) then
               OnCommandOther(ClientThread, sCmd, s, WasHandled);
         end;
         if not WasHandled then
            Writeln('500 command not recognized (' + sCMD + ')');
      end;
   end;
end;

end.

