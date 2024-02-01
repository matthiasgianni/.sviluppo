program plccomm;

uses
  Vcl.Forms,
  UDebug in '..\source\UDebug.pas' {FormDebug},
  Utils in '..\..\lib\Utils.pas',
  Plc in '..\..\lib\plc\Plc.pas',
  UDMPLC in '..\..\lib\plc\UDMPLC.pas' {DMPLC: TDataModule},
  UInfoForm in '..\..\lib\UInfoForm.pas' {InfoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDebug, FormDebug);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.
