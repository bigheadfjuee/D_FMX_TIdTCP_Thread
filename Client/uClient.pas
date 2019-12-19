unit uClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts;

type
  TFormClient = class(TForm)
    Memo1: TMemo;
    edtMsg: TEdit;
    IdTCPClient1: TIdTCPClient;
    tmReadLn: TTimer;
    Layout1: TLayout;
    edtHost: TEdit;
    edtPort: TEdit;
    btnConnect: TButton;
    btnDiscon: TButton;
    btnSend: TButton;
    btnClearMemo: TButton;
    Panel1: TPanel;
    chbUseTimer: TCheckBox;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconClick(Sender: TObject);
    procedure tmReadLnTimer(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure IdTCPClient1Connected(Sender: TObject);
    procedure IdTCPClient1Disconnected(Sender: TObject);
    procedure IdTCPClient1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure btnClearMemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure Connect(var client: TIdTCPClient);
    procedure DisConnect(var client: TIdTCPClient);
    procedure Send(var client: TIdTCPClient);
    procedure ParseProc(msg: AnsiString);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormClient: TFormClient;

implementation

{$R *.fmx}

uses IdException, IdGlobal, uClientThread;

procedure TFormClient.btnClearMemoClick(Sender: TObject);
begin
  Memo1.Lines.Clear();
end;

procedure TFormClient.Connect(var client: TIdTCPClient);
begin
  client.Host := edtHost.Text;
  client.Port := edtPort.Text.ToInteger;
  if not client.Connected then
  begin
    try
      Memo1.Lines.Add('C-Connect');
      client.Connect;
    except
      on E: EIdException do
        Memo1.Lines.Add('== EIdException: ' + E.Message);
    end;
  end
  else
  begin
    Memo1.Lines.Add('C-Connected');
  end;
end;

procedure TFormClient.ParseProc(msg: AnsiString);
begin
  Memo1.Lines.Add('C-ParseProc: ' + msg);
end;

procedure TFormClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IdTCP_Recv.Terminate; // 有使用到 thread 就要注意和處理比較多的事
end;

procedure TFormClient.FormCreate(Sender: TObject);
begin
  // 使用 Thread 的方式
  IdTCP_Recv := TIdTCP_Recv.Create;
  IdTCP_Recv.MyParseProc := ParseProc;
end;

procedure TFormClient.Send(var client: TIdTCPClient);
begin
  if not client.Connected then
  begin
    Memo1.Lines.Add('C-Not Connected');
  end
  else
  begin
    Memo1.Lines.Add('C-Send ' + edtMsg.Text);
    client.IOHandler.WriteLn(edtMsg.Text, IndyTextEncoding_UTF8);
  end;
end;

procedure TFormClient.btnConnectClick(Sender: TObject);
begin

  if chbUseTimer.IsChecked then
    Connect(IdTCPClient1)
  else
  begin

    // 在 main thread 呼叫 IdTCPClient.Connect 一樣會卡住
    // Connect(IdTCP_Recv.IdTCPClient);

    IdTCP_Recv.MyConnect(false);
    IdTCP_Recv.IdTCPClient.Host := edtHost.Text;
    IdTCP_Recv.IdTCPClient.Port := edtPort.Text.ToInteger();
    IdTCP_Recv.MyConnect(true); // thread 裡面呼叫 connect 才不會卡住
  end;
end;

procedure TFormClient.btnDisconClick(Sender: TObject);
begin
  if chbUseTimer.IsChecked then
    DisConnect(IdTCPClient1)
  else
    IdTCP_Recv.MyConnect(false);
end;

procedure TFormClient.DisConnect(var client: TIdTCPClient);
begin
  client.DisConnect; // Tony 註：用 Timer 的方式，才可以直接斷線
end;

procedure TFormClient.btnSendClick(Sender: TObject);
begin
  if chbUseTimer.IsChecked then
    Send(IdTCPClient1)
  else
    Send(IdTCP_Recv.IdTCPClient);
end;

procedure TFormClient.IdTCPClient1Connected(Sender: TObject);
begin
  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('C-Connected');
  tmReadLn.Enabled := true;
end;

procedure TFormClient.IdTCPClient1Disconnected(Sender: TObject);
begin
  tmReadLn.Enabled := false;
  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('C-Disconnected');
end;

procedure TFormClient.IdTCPClient1Status(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('C-Status: ' + AStatusText);
end;

procedure TFormClient.tmReadLnTimer(Sender: TObject);
var
  str: String;
begin
  // Timer ReadLn 的方式
  try
    if IdTCPClient1.IOHandler.InputBufferIsEmpty then
      IdTCPClient1.IOHandler.CheckForDataOnSource(0);
    while not IdTCPClient1.IOHandler.InputBufferIsEmpty do
    begin
      str := IdTCPClient1.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      Memo1.Lines.Add('C-ReadLn: ' + str);
    end;
  except
    on E: EIdException do
    begin
      Memo1.Lines.Add('== tmReadLnTimer' + E.ToString);
      IdTCPClient1.DisConnect;
    end;
  end;

end;

end.
