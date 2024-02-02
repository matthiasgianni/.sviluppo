unit UInfoFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, Vcl.StdCtrls;

type
  TInfoFrame = class(TFrame)
    Border: TShape;
    LabelMessage: TLabel;
    Img: TImage;
    ImageList: TImageList;
    procedure LabelMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetIcon(AValue: Integer);
    procedure SetMessage(AMessage: String);
  end;

implementation

{$R *.dfm}

procedure TInfoFrame.LabelMessageClick(Sender: TObject);
begin
  Self.Free;
end;

procedure TInfoFrame.SetIcon(AValue: Integer);
begin
  ImageList.GetBitmap(AValue, Img.Picture.Bitmap);
end;

procedure TInfoFrame.SetMessage(AMessage: String);
begin
  LabelMessage.Caption := AMessage;
end;

end.
