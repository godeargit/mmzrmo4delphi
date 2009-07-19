unit DXDos;

interface

procedure Unpacktime(P:Longint; var DT:TDatetime);
procedure Packtime(var DT:TDatetime; var P:Longint);
function GetDosDate:LongInt;
function GetDOW:Word;
function TimeOut(MyTime:LongInt):Boolean;
function TimeCounter:Longint;

implementation

uses
   Windows,
   Sysutils;

procedure UNPACKTIME(P:LONGINT; var DT:TDATETIME);
begin
   DT:=FILEDATETODATETIME(P);
end;

procedure PACKTIME(var DT:TDATETIME; var P:LONGINT);
begin
   P:=DATETIMETOFILEDATE(DT);
end;

function GetDosDate:LongInt;
begin
   Result:=DATETIMETOFILEDATE(Now);
end;

function GetDOW:Word;
begin
   Result:=DayOfWeek(Now);
end;

function TimeOut(MyTime:LongInt):Boolean;
var
   TimeDiff:LongInt;

begin
   TimeDiff:=MyTime-TimeCounter;
   if TimeDiff<0 then
      TimeOut:=True
   else begin
      if (TimeDiff>780000) then Dec(TimeDiff, 1572480);
      TimeOut:=TimeDiff<0;
   end;
end;

function TimeCounter:Longint;
begin
   Result:=GetTickCount;
end;

end.

