program plccomm;

uses
  Vcl.Forms,
  UDebug in '..\source\UDebug.pas' {FormDebug},
  Utils in '..\..\lib\Utils.pas',
  Plc in '..\..\lib\plc\Plc.pas',
  UDmPlc in '..\..\lib\plc\UDmPlc.pas' {DMPLC: TDataModule},
  UInfoFrame in '..\..\lib\UInfoFrame.pas' {InfoFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDebug, FormDebug);
  Application.Run;
end.
