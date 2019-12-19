program MyIdTcpServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uServer in 'uServer.pas' {FormServer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormServer, FormServer);
  Application.Run;
end.
