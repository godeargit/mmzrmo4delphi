program Dbsvr;

uses
  Forms,
  untMain in 'untMain.pas' {frm_main},
  UntRmodbSvr in '..\GobUnit\UntRmodbSvr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_main, frm_main);
  Application.Run;
end.

