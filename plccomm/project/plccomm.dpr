program plccomm;

uses
  Vcl.Forms,
  UDebug in '..\source\UDebug.pas' {FormDebug},
  Utils in '..\..\lib\Utils.pas',
  Plc in '..\..\lib\plc\Plc.pas',
  UDmPlc in '..\..\lib\plc\UDmPlc.pas' {DMPLC: TDataModule},
  UInfoFrame in '..\..\lib\UInfoFrame.pas' {InfoFrame: TFrame},
  CustomControls in '..\..\lib\CustomControls\CustomControls.pas',
  UFrameDebug in '..\..\lib\UFrameDebug.pas' {FrameDebug: TFrame},
  UComPort in '..\..\lib\ComPort\UComPort.pas',
  UDMStartup in '..\source\UDMStartup.pas' {DMStartup: TDataModule};

{$R *.res}

begin
  Application.Initialize;

  // Load configurations from JSON
  LoadConfigurations;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDMStartup, DMStartup);
  Application.CreateForm(TDMPLC, DMPLC);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.Run;
end.
