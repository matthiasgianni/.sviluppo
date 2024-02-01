unit UInfoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList;

type
  TInfoForm = class(TForm)
    ImageList: TImageList;
    LabelMessage: TLabel;
    Img: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetIcon(AValue: Integer);
    procedure SetMessage(AMessage: String);
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.dfm}

{ TInfoForm }

procedure TInfoForm.SetIcon(AValue: Integer);
begin
  ImageList.GetBitmap(AValue, Img.Picture.Bitmap);
end;

procedure TInfoForm.SetMessage(AMessage: String);
begin
  LabelMessage.Caption := AMessage;
end;

end.
