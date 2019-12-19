unit uServer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdTCPServer, IdContext, FMX.Edit, FMX.Layouts,
  FMX.ListBox, IdSocketHandle, IdThread;

type
  TFormServer = class(TForm)
    Memo1: TMemo;
    IdTCPServer1: TIdTCPServer;
    PanelToClient: TPanel;
    ListBoxClient: TListBox;
    edtSend: TEdit;
    PanelSend: TPanel;
    btnMultiSend: TButton;
    btnSend: TButton;
    PanelServer: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    edtPort: TEdit;
    labPort: TLabel;
    btnClearMemo: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1ContextCreated(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext;
      AException: Exception);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnMultiSendClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnClearMemoClick(Sender: TObject);
  private
    { Private declarations }
    procedure StartServer;
    procedure StopServer;
    procedure SendToOneClient(str: string);
    procedure SendToAllClient(str: string);
    procedure doParseMsg(msg: string);

  public
    { Public declarations }
  end;

var
  FormServer: TFormServer;

implementation

{$R *.fmx}

uses IdException, IdGlobal;

procedure TFormServer.btnClearMemoClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TFormServer.btnMultiSendClick(Sender: TObject);
begin
  SendToAllClient(edtSend.Text);
end;

procedure TFormServer.btnSendClick(Sender: TObject);
begin
  SendToOneClient(edtSend.Text);
end;

procedure TFormServer.btnStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TFormServer.btnStopClick(Sender: TObject);
begin
  StopServer;
end;

procedure TFormServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServer;
end;

procedure TFormServer.IdTCPServer1Connect(AContext: TIdContext);
var
  str: String;
begin
  str := AContext.Binding.PeerIP + '_' + AContext.Binding.PeerPort.ToString;
  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('S-Connect: ' + str);

  ListBoxClient.Items.Add(str);
  // 自動選擇最後新增的
  if ListBoxClient.Count > -1 then
    ListBoxClient.ItemIndex := ListBoxClient.Count - 1;
end;

procedure TFormServer.IdTCPServer1ContextCreated(AContext: TIdContext);
var
  str: String;
begin
  str := AContext.Binding.PeerIP + '_' + AContext.Binding.PeerPort.ToString;
  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('S-ContextCreated ' + str);

end;

procedure TFormServer.IdTCPServer1Disconnect(AContext: TIdContext);
var
  index: Integer;
  str: String;
begin
  str := AContext.Binding.PeerIP + '_' + AContext.Binding.PeerPort.ToString;

  Index := ListBoxClient.Items.IndexOf(str);
  ListBoxClient.Items.Delete(index);

  Memo1.Lines.Add(DateTimeToStr(Now));
  Memo1.Lines.Add('S-Disconnect: ' + str);

end;

procedure TFormServer.IdTCPServer1Exception(AContext: TIdContext;
  AException: Exception);
var
  str: String;
begin
  str := AContext.Binding.PeerIP + '_' + AContext.Binding.PeerPort.ToString;
  Memo1.Lines.Add('IdTCPServer1Exception: ' + str);
  Memo1.Lines.Add('S-Exception:' + AException.Message);
end;

// TIdTCPServer 元件內建的 thread
procedure TFormServer.IdTCPServer1Execute(AContext: TIdContext);
var
  str: string;
begin

  // 直接讀一行 (自動用換行字元區隔)
  str := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);

  // Synchronize(阻塞)
  TThread.Synchronize(TThread.CurrentThread,
    procedure()
    begin
      doParseMsg(str);
    end);

  // Queue (非阻塞)
  TThread.Queue(nil,
    procedure
    begin
//      doParseMsg(str);
    end);

  // echo client
  // AContext.Connection.IOHandler.WriteLn('Server Echo 已收到 : ' + str,
  // IndyTextEncoding_UTF8);
end;

procedure TFormServer.doParseMsg(msg: string);
begin
  Memo1.Lines.Add('S-doParseMsg: ' + msg);
end;

procedure TFormServer.StartServer;
begin

  IdTCPServer1.Bindings.DefaultPort := edtPort.Text.ToInteger;
  Memo1.Lines.Add('S-Bindings.DefaultPort' + edtPort.Text);

  try
    IdTCPServer1.Active := true;
    Memo1.Lines.Add('S-Active = True');
  except
    on E: EIdException do
      Memo1.Lines.Add('== EIdException: ' + E.Message);
  end;

end;

procedure TFormServer.StopServer;
var
  index: Integer;
  Context: TIdContext;
begin
  IdTCPServer1.Active := False;
  Memo1.Lines.Add('S-Active = False');

  // 比較嚴謹的關閉方式
  {
    if IdTCPServer1.Active then
    begin


    IdTCPServer1.OnDisconnect := nil;
    ListBoxClient.Clear;

    with IdTCPServer1.Contexts.LockList do
    begin
    if Count > 0 then
    begin
    try
    for index := 0 to Count - 1 do
    begin
    Context := Items[index];
    if Context = nil then
    continue;

    Context.Connection.IOHandler.WriteBufferClear;
    Context.Connection.IOHandler.InputBuffer.Clear;
    Context.Connection.IOHandler.Close;
    if Context.Connection.Connected then
    Context.Connection.Disconnect;
    end;
    finally
    IdTCPServer1.Contexts.UnlockList;
    end;
    end;
    end;

    try
    IdTCPServer1.StopListening;
    IdTCPServer1.Active := False;
    Memo1.Lines.Add('StopServer');
    except
    on E: EIdException do
    Memo1.Lines.Add('== EIdException: ' + E.Message);
    end;
    end;

    IdTCPServer1.OnDisconnect := IdTCPServer1Disconnect;
  }
end;

procedure TFormServer.SendToOneClient(str: string);
var
  List: TList;
  msg: string;
begin

  if ListBoxClient.ItemIndex = -1 then
  begin
    ShowMessage('請選擇一個 Client');
  end
  else
  begin

    try
      List := IdTCPServer1.Contexts.LockList;
      if List.Count = 0 then
      begin
        exit;
      end;
      TIdContext(List[ListBoxClient.ItemIndex]).Connection.IOHandler.WriteLn
        (edtSend.Text, IndyTextEncoding_UTF8);
    finally
      IdTCPServer1.Contexts.UnlockList;
    end;

    msg := 'SendToOneClient(';
    msg := msg + ListBoxClient.Items[ListBoxClient.ItemIndex];
    msg := msg + '): ' + str;
    Memo1.Lines.Add(msg);
  end;

end;

procedure TFormServer.SendToAllClient(str: string);
var
  List: TList;
  I: Integer;
begin
  List := IdTCPServer1.Contexts.LockList;

  try
    if List.Count = 0 then
    begin
      Memo1.Lines.Add('No Client connected');
      exit;
    end
    else
      Memo1.Lines.Add('SendToAllClient: ' + str);

    for I := 0 to List.Count - 1 do
    begin
      try
        TIdContext(List[I]).Connection.IOHandler.WriteLn(str,
          IndyTextEncoding_UTF8);

      except
        on E: EIdException do
          Memo1.Lines.Add('== EIdException: ' + E.Message);
      end;
    end;

  finally
    IdTCPServer1.Contexts.UnlockList;
  end;

end;

end.
