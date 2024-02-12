program plccomm;

uses
  Vcl.Forms,
  UDebug in '..\source\UDebug.pas' {FormDebug},
  Utils in '..\..\common\Utils.pas',
  Plc in '..\..\common\plc\Plc.pas',
  UDmPlc in '..\..\common\plc\UDmPlc.pas' {DMPLC: TDataModule},
  UInfoFrame in '..\..\common\UInfoFrame.pas' {InfoFrame: TFrame},
  CustomControls in '..\..\common\CustomControls\CustomControls.pas',
  UFrameDebug in '..\..\common\UFrameDebug.pas' {FrameDebug: TFrame},
  UComPort in '..\..\common\ComPort\UComPort.pas',
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
