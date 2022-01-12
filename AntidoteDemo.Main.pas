unit AntidoteDemo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Execute.Antidote, Vcl.Menus;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    RichEdit2: TRichEdit;
    Splitter1: TSplitter;
    RichEdit3: TRichEdit;
    Splitter2: TSplitter;
    MainMenu1: TMainMenu;
    Antidote1: TMenuItem;
    Correcteur1: TMenuItem;
    Dictionnaires1: TMenuItem;
    Guides1: TMenuItem;
    Dfinitions1: TMenuItem;
    Synonymes1: TMenuItem;
    Antonymes1: TMenuItem;
    Coocurrences1: TMenuItem;
    ChampLexical1: TMenuItem;
    Conjugaison1: TMenuItem;
    Famille1: TMenuItem;
    Citations1: TMenuItem;
    Historique1: TMenuItem;
    Illustrations1: TMenuItem;
    Wikipedia1: TMenuItem;
    Orthographe1: TMenuItem;
    Lexique1: TMenuItem;
    Grammaire1: TMenuItem;
    Syntaxe1: TMenuItem;
    Ponctuation1: TMenuItem;
    Style1: TMenuItem;
    Rdaction1: TMenuItem;
    ypographie1: TMenuItem;
    Phontique1: TMenuItem;
    Historique2: TMenuItem;
    PointsdeLangue1: TMenuItem;
    Langue1: TMenuItem;
    Selonlecontexte1: TMenuItem;
    Forcerlanglais1: TMenuItem;
    Forcerlefranais1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Correcteur1Click(Sender: TObject);
    procedure DictionnaireClick(Sender: TObject);
    procedure OnGuide(Sender: TObject);
    procedure RichEdit1Enter(Sender: TObject);
  private
    { Déclarations privées }
    FRichEdit: TRichEdit;
    Antidote: TAntidote;
    function GetSelStart(Sender: TObject; DocId, TextId: Integer): Integer;
    function GetSelStop(Sender: TObject; DocId, TextId: Integer): Integer;
    procedure GetCurrentDocID(Sender: TObject; var DocId: Integer);
    function GetTextFieldCount(Sender: TObject; DocId: Integer): Integer;
    function GetTextFieldId(Sender: TObject; DocId, Index: Integer): Integer;
    function GetTextFieldLength(Sender: TObject; DocId, TextId: Integer): Integer;
    function GetTextFieldText(Sender: TObject; DocId, TextId, Start, Stop: Integer): string;
    procedure SelectTextField(Sender: TObject; DocId, TextId, Start, Stop: Integer);
    procedure ReplaceTextField(Sender: TObject; DocId, TextId, Start, Stop: Integer; const Text: string);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Correcteur1Click(Sender: TObject);
begin
  Antidote.Correcteur;
end;

procedure TForm1.DictionnaireClick(Sender: TObject);
begin
  Antidote.Dictionnaire(TDictionnaire(TMenuItem(Sender).Tag));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Antidote := TAntidote.Create(Self);
  Antidote.OnGetSelStart := GetSelStart;
  Antidote.OnGetSelStop := GetSelStop;
  Antidote.OnGetCurrentDocId := GetCurrentDocID;
  Antidote.OnGetTextFieldCount := GetTextFieldCount;
  Antidote.OnGetTextFieldId := GetTextFieldId;
  Antidote.OnGetTextFieldLength := GetTextFieldLength;
  Antidote.OnGetTextFieldText := GetTextFieldText;
  Antidote.OnSelectTextField := SelectTextField;
  Antidote.OnReplaceTextField := ReplaceTextField;
  Antidote.Active := True;
end;

procedure TForm1.GetCurrentDocID(Sender: TObject; var DocId: Integer);
begin
  DocId := 1; // Un seul document
end;

function TForm1.GetTextFieldCount(Sender: TObject; DocId: Integer): Integer;
begin
  Result := 3; // 3 zones de texte dans le document
end;
function TForm1.GetSelStart(Sender: TObject; DocId, TextId: Integer): Integer;
var
  RE: TRichEdit;
begin
  Result := 0;
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
  end;
  Result := RE.SelStart;
end;

function TForm1.GetSelStop(Sender: TObject; DocId, TextId: Integer): Integer;
var
  RE: TRichEdit;
begin
  Result := 0;
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
  end;
  Result := RE.SelStart + RE.SelLength;
end;

function TForm1.GetTextFieldId(Sender: TObject; DocId, Index: Integer): Integer;
begin
  case Index of
    -1:
    begin
      if FRichEdit <> nil then
        Exit(FRichEdit.Tag);
    end;
    1..3: Exit(Index);
  end;
  Result := -1;
end;

function TForm1.GetTextFieldLength(Sender: TObject; DocId,
  TextId: Integer): Integer;
var
  RE: TRichEdit;
begin
  Result := 0;
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
    Result := RE.Lines.Text.Length;
  end;
end;

function TForm1.GetTextFieldText(Sender: TObject; DocId, TextId, Start,
  Stop: Integer): string;
var
  RE: TRichEdit;
begin
  Result := '';
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
    Result := Copy(RE.Lines.Text, Start, Stop - Start);
  end;
end;

procedure TForm1.OnGuide(Sender: TObject);
begin
  Antidote.Guide(TGuide(TMenuItem(Sender).Tag));
end;

procedure TForm1.ReplaceTextField(Sender: TObject; DocId, TextId, Start,
  Stop: Integer; const Text: string);
var
  RE: TRichEdit;
begin
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
    RE.SelStart := Start;
    RE.SelLength := Stop - Start;
    RE.SelText := Text;
  end;
end;

procedure TForm1.RichEdit1Enter(Sender: TObject);
begin
  FRichEdit := TRichEdit(Sender);
end;

procedure TForm1.SelectTextField(Sender: TObject; DocId, TextId, Start,
  Stop: Integer);
var
  RE: TRichEdit;
begin
  if DocId = 1 then
  begin
    case TextId of
      1: RE := RichEdit1;
      2: RE := RichEdit2;
      3: RE := RichEdit3;
    else
      Exit;
    end;
    RE.SelStart := Start;
    RE.SelLength := Stop - Start;
  end;
end;

end.
