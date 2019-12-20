unit uClientThread;

interface

uses
  System.Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, System.SysUtils;

type
  TMyParseProc = procedure(msg: AnsiString) of object;

type
  TIdTCP_Recv = class(TThread)
  private
    isMyActive: Boolean;
    msg: String;

  protected
    procedure Execute; override;

  public
    IdTCPClient: TIdTCPClient;
    MyParseProc: TMyParseProc;

    Constructor Create();
    destructor Destroy; override;
    procedure MyConnect(isActive: Boolean);
    procedure doParseMsg;

  end;

var
  IdTCP_Recv: TIdTCP_Recv; // 外部 thread 要寫在這裡
    // 如果寫在別的 class 的 field 中使用，會導致 access error
implementation

constructor TIdTCP_Recv.Create();
begin
  inherited Create(false); // suspend: false

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

          // 視覺元件要用 Synchronize(阻塞) 或 Queue(非阻塞) 才不會 hang on;
          // Synchronize(doParseCMD);
          Queue(doParseMsg);

        end;
      except
        on E: Exception do;
      end;

    end;

    Sleep(200); // 避免 CPU 100%
  end;
end;

procedure TIdTCP_Recv.doParseMsg;
begin
  if Assigned(MyParseProc) then
    MyParseProc(msg); // 使用自訂的方式處理收到的字串
end;

procedure TIdTCP_Recv.MyConnect(isActive: Boolean);
begin
  if isActive then
    isMyActive := True // 在 Excute 中會持續嘗試連線
  else
  begin
    isMyActive := false;

    if IdTCPClient.Connected then
      IdTCPClient.Disconnect;
  end;
end;

end.
