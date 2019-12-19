unit uClientThread;

interface

uses
  System.Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, System.SysUtils;

type
  TIdTCP_Recv = class(TThread)
  private
    isMyActive: Boolean;
  protected
    procedure Execute; override;
  public

    IdTCPClient: TIdTCPClient;
    msg: String;
    Constructor Create();
    destructor Destroy; override;
    procedure MyConnect(isActive: Boolean);
    procedure ParseMsg;

  end;

var
  IdTCP_Recv: TIdTCP_Recv;

implementation

uses
  uClient;

constructor TIdTCP_Recv.Create();
begin
  inherited Create(True); // suspend

  isMyActive := false;
  IdTCPClient := TIdTCPClient.Create(nil);
end;

destructor TIdTCP_Recv.Destroy;
begin

  if IdTCPClient.Connected then
    IdTCPClient.Disconnect;

  IdTCPClient.Destroy();

  inherited Destroy(); // 別忘了！不然會有 run-time error
end;

procedure TIdTCP_Recv.Execute;
begin

  while not Terminated do
  begin

    if isMyActive then
    begin
      if not IdTCPClient.Connected then
      begin
        try
          IdTCPClient.Connect;
        except
          on E: Exception do;

        end;
      end;
    end;

    if IdTCPClient.Connected then
    begin

      try
        if IdTCPClient.IOHandler.InputBufferIsEmpty then
          IdTCPClient.IOHandler.CheckForDataOnSource(0);

        while not IdTCPClient.IOHandler.InputBufferIsEmpty do
        begin
          msg := IdTCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
          Synchronize(ParseMsg);
          // 視覺元件要用 Synchronize 才不會 hang on;
        end;
      except
        on E: Exception do;
      end;

    end;

    Sleep(200); // 避免 CPU 100%
  end;
end;

procedure TIdTCP_Recv.ParseMsg;
begin
  FormClient.Memo1.Lines.Add(msg);
end;

procedure TIdTCP_Recv.MyConnect(isActive: Boolean);
begin
  if isActive then
    isMyActive := True // 在 Excute 中會持續嘗試連線
  else
  begin
    isMyActive := false;
    IdTCPClient.Disconnect;
  end;
end;

end.
