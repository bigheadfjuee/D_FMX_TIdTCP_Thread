program MyIdTcpClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  uClient in 'uClient.pas' {FormClient},
  uClientThread in 'uClientThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormClient, FormClient);
  Application.Run;
end.
