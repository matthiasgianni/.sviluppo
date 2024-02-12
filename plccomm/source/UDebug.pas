unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ComCtrls,
  Utils, UFrameDebug, UComPort;

type
  TFormDebug = class(TForm)
    PanelStatus: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFrameDebug: TFrameDebug;
  public
    { Public declarations }
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.dfm}

uses
  UDMStartup;

procedure TFormDebug.FormCreate(Sender: TObject);
var
  LData: string;
begin
  FFrameDebug := TFrameDebug.Create(FormDebug);
  FFrameDebug.Parent := FormDebug;

  //DMStartup.StartComPortPolling;
end;

end.
