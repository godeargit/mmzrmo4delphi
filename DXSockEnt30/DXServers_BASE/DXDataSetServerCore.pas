unit DXDatasetServerCore;

interface

///////////////////////////////////////////////////////////////////////////////
// Component: TDXDataSetServerCore
//    Author: G.E. Ozz Nixon Jr. (usasupport@brainpatchworkdx.com)
// Copyright: All code is the property of Brain Patchwork DX (tm) and part of
//            the DXSock (r) product, which are (c) 1999 Brain Patchwork DX.
//   Version: 3.0 (3rd Generation Code)
// ==========================================================================
// This allows you to develop TDataSet Servers - this is a proprietary
// solutions from Brain Patchwork DX, LLC. It has been designed to allow you
// to make your own hooks at the server for the DataSet connectivity.
// TDXDataSetClient was designed to use the "ActiveBuffer" as a tunnel to
// this server. As navigation changes, the resulting communications are of
// the new current ActiveBuffer.
///////////////////////////////////////////////////////////////////////////////

uses
  DXString,
  Classes,
  DXServerCore;

{$I DXSOCK.DEF}

type
  DataSetTBasicEvent = procedure(ClientThread: TDXClientThread) of object;
  DataSetTSimpleEvent = procedure(ClientThread: TDXClientThread;Parm1:String) of object;
  DataSetTComplexEvent = procedure(ClientThread: TDXClientThread;Parm1,Parm2:String) of object;
  DataSetTOtherEvent = procedure(ClientThread: TDXClientThread; Command: string; Parm: string;Var Handled:Boolean) of object;

  TDXDataSetServerCore = class(TDXServerCore)
  private
    fOnCommandOther: DataSetTOtherEvent; //COMMAND parameters...
  protected
  public
{$IFDEF OBJECTS_ONLY}
    constructor Create;
{$ELSE}
    constructor Create(AOwner:TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ProcessSession(ClientThread: TDXClientThread);
    Procedure AddBasicEvent(Command:String;EventProc:DataSetTSimpleEvent);
  published
    property OnCommandOther: DataSetTOtherEvent read fOnCommandOther
                                         write fOnCommandOther;
  end;

implementation

Uses
   DXSock;

Type
  PDataSetSimpleEvent=^TDataSetSimpleEvent;
  TDataSetSimpleEvent=record
     Tag:Integer;
     Command:String;
     EventProcedure:DataSetTSimpleEvent;
  End;


{$IFDEF OBJECTS_ONLY}
constructor TDXDataSetServerCore.Create;
{$ELSE}
constructor TDXDataSetServerCore.Create(AOwner:TComponent);
{$ENDIF}
begin
{$IFDEF OBJECTS_ONLY}
   inherited Create;
{$ELSE}
   inherited Create(AOwner);
{$ENDIF}
   ServerPort:=9901;
   ProtocolToBind:=wpTCPOnly;
end;

destructor TDXDataSetServerCore.Destroy;
Var
   PSimpleEvent:PDataSetSimpleEvent;

begin
   If Assigned(fEventArray) then Begin
      While fEventArray.Count>0 do Begin
         Case PDataSetSimpleEvent(fEventArray[0]).Tag of
            1:Begin
              PSimpleEvent:=fEventArray[0];
              Dispose(PSimpleEvent);
            End;
         End;
         fEventArray.Delete(0);
      End;
   End;
   inherited Destroy;
end;

Procedure TDXDataSetServerCore.AddBasicEvent(Command:String;EventProc:DataSetTSimpleEvent);
Var
   PSimpleEvent:PDataSetSimpleEvent;
   Loop:Integer;

Begin
   Command:=Uppercase(Command);
   Loop:=0;
   While Loop<fEventArray.Count do Begin
      If PDataSetSimpleEvent(fEventArray[Loop]).Command=Command then Begin
         PDataSetSimpleEvent(fEventArray[Loop]).EventProcedure:=EventProc;
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

{
Procedure TDXDataSetServerCore.SetOnCommandUSER(value:DataSetTSimpleEvent);
Begin
   fOnCommandUSER:=Value;
   AddBasicEvent('USER',Value);
End;
}

procedure TDXDataSetServerCore.ProcessSession(ClientThread: TDXClientThread);
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
   with ClientThread.Socket do begin
      s:=ReadLn(Timeout);
      If LastReadTimeout or Not ValidSocket then Exit;
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
         If PDataSetSimpleEvent(fEventArray[Loop]).Command=sCMD then Begin
            Case PDataSetSimpleEvent(fEventArray[Loop]).Tag of
               1:if Assigned(PDataSetSimpleEvent(fEventArray[Loop]).EventProcedure) then
                    DataSetTSimpleEvent(PDataSetSimpleEvent(fEventArray[Loop]).EventProcedure)(ClientThread,S);
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

end.

