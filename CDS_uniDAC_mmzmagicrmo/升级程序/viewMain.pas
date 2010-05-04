unit viewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RzButton, StdCtrls, RzPrgres, jpeg;

type
  TView_main = class(TForm)
    Image1: TImage;
    lbl_hint: TLabel;
    RzButton1: TRzButton;
    RzProgressBar1: TRzProgressBar;
    tmr1: TTimer;
    btn_close: TRzButton;
    lbl1: TLabel;
    lbl_hi: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btn_closeClick(Sender: TObject);
    procedure RzButton1Click(Sender: TObject);
  private
    { Private declarations }
  public

  end;

var
  View_main: TView_main;

implementation

uses
  untFunctions;


var
  ls: TStrings;

{$R *.dfm}

procedure TView_main.FormCreate(Sender: TObject);

begin
  if FileExists(GetCurrPath + 'up.cfg') = false then
    WarningInfo('没有发现升级所需的文件，请与管理员联系')
  else begin
    ls := TStringList.Create;
    ls.LoadFromFile('up.cfg');
    lbl_hint.Caption := ls[1];
    tmr1.Enabled := true;
  end;
end;

procedure TView_main.tmr1Timer(Sender: TObject);
begin
  tmr1.Enabled := false;
  if ls[0] = '0' then begin
    if QueryInfo('检查到有新的升级程序，是否现在就升级') = false then begin
      Application.Terminate;
      Exit;
    end
    else begin
      RzButton1.Click;
    end;
  end
  else
    RzButton1.Click;
end;

procedure TView_main.btn_closeClick(Sender: TObject);
begin
  Close;
end;

procedure TView_main.RzButton1Click(Sender: TObject);
var
  i, li, LC: Integer;
  lis: string;
begin
  //进行升级
  //先结束原有的程序
  if ParamCount > 0 then begin
    KillTask(ExtractFileName(ParamStr(1)));
  end;
  SleepMy(500);
  li := 0;
  LC := StrToInt(ls[2]);
  for i := 3 to lc + 2 do begin // Iterate
    lis := ifthen(ParamStr(1) = '', GetCurrPath, ParamStr(1)) + 'update\';
    lis := StringReplace(ls[i], lis, '', []);
    lis := GetCurrPath() + lis;
    ForceDirectories(ExtractFilePath(lis));
    CopyFile(pchar(ls[i]), pchar(lis), false);
    SleepMy(10);
    inc(li);
    RzProgressBar1.Percent := li * 100 div Lc;
  end; // for
  //完了就将升级的目录删除掉
  DeleteDir(GetCurrPath() + 'update');
  DeleteFile(GetCurrPath() + 'up.cfg');
  lbl_hi.Caption := '升级完毕';
end;

end.

