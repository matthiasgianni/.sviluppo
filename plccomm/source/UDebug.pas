unit UDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ComCtrls,
  Utils, UFrameDebug;

type
  TFormDebug = class(TForm)
    pnlStatus: TPanel;
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

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  FFrameDebug := TFrameDebug.Create(FormDebug);
  FFrameDebug.Parent := FormDebug;
end;

end.
