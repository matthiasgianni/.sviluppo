unit CustomControls;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.WinXCtrls, Plc;

type
  TAutomationControl = class(TPanel)
  private
    FText: String;
    FValue: Variant;
    FSignalID: Integer;
  published
    procedure SetValue(AValue: Variant);
  public
    property Text: String read FText write FText;
    property Value: Variant read FValue write SetValue;
    property SignalID: Integer read FSignalID write FSignalID;

    constructor Create(AOwner: TComponent); override;
  end;

const
  K_COLOR_TRUE = $00AF581F;
  K_COLOR_FALSE = $00DE8143;

implementation

{ TAutomationControl }

constructor TAutomationControl.Create(AOwner: TComponent);
var
  LBorder: TShape;
begin
  inherited Create(AOwner);

  LBorder := TShape.Create(Self);
  LBorder.Parent := Self;
  LBorder.Align := alClient;
  LBorder.Brush.Style := bsClear;

  Self.BevelInner := bvNone;
  Self.BevelKind := bkNone;
  Self.BevelOuter := bvNone;

  Self.AlignWithMargins := True;
  Self.Margins.Top := 10;
  Self.Margins.Left := 5;
  Self.Margins.Right := 5;

  Self.Font.Color := clWhite;

  Self.ParentBackground := False;
end;

procedure TAutomationControl.SetValue(AValue: Variant);
begin
  Caption := Text;

  if AValue then
    Color := clLime
  else
    Color := clRed;
end;

end.
