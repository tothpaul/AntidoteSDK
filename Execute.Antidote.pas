unit Execute.Antidote;

{
   Antidote 11 SDK for Delphi (c)2022 Execute SARL <https://www.execute.fr>
}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Execute.AntidoteAPI;

type
  TGetDocIdEvent = procedure(Sender: TObject; var DocId: Integer) of object;
  TGetDocTitleEvent = procedure(Sender: TObject; var Title: string) of object;
  TGetDocIntEvent = function(Sender: TObject; DocId: Integer): Integer of object;
  TGetTextFieldIntEvent = function(Sender: TObject; DocId, TextId: Integer): Integer of object;
  TGetTextFieldLengthEvent = function(Sender: TObject; DocId, TextId: Integer): Integer of object;
  TGetTextFieldTextEvent = function(Sender: TObject; DocId, TextId, Start, Stop: Integer): string of object;
  TGetTextFieldIdEvent = function(Sender: TObject; DocId, Index: Integer): Integer of object;
  TSelectTextFieldEvent = procedure(Sender: TObject; DocId, TextId, Start, Stop: Integer) of object;
  TReplaceTextFieldEvent = procedure(Sender: TObject; DocId, TextId, Start, Stop: Integer; const Text: string) of object;

{$SCOPEDENUMS ON}
  TDictionnaire = (
    Definitions,
    Synonymes,
    Antonymes,
    Cooccurrences,
    ChampLexical,
    Conjugaison,
    Famille,
    Citations,
    Historique,
    Illustrations,
    Wikipedia
  );

  TGuide = (
    Orthographe,
    Lexique,
    Grammaire,
    Syntaxe,
    Ponctuation,
    Style,
    Redaction,
    Typographie,
    Phonetique,
    Historique,
    PointsDeLangue
  );

  TAntidote = class(TComponent)
  private
    FActive: Boolean;
    FApi: TAntidoteAPI;
    FOnGetSelStart: TGetTextFieldIntEvent;
    FOnGetSelStop: TGetTextFieldIntEvent;
    FOnGetCurrentDocID: TGetDocIdEvent;
    FOnGetTextFieldCount: TGetDocIntEvent;
    FOnGetTextFieldId: TGetTextFieldIdEvent;
    FOnGetTextFieldLength: TGetTextFieldLengthEvent;
    FOnGetTextFieldText: TGetTextFieldTextEvent;
    FOnGetCurrentDocTitle: TGetDocTitleEvent;
    FOnSelectTextField: TSelectTextFieldEvent;
    FOnReplaceTextField: TReplaceTextFieldEvent;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure Start;
  public
    destructor Destroy; override;
  published
    function Correcteur: Boolean;
    function Dictionnaire(ADictionnaire: TDictionnaire): Boolean;
    function Guide(AGuide: TGuide): Boolean;
    property Active: Boolean read GetActive write SetActive;
    property OnGetSelStart: TGetTextFieldIntEvent read FOnGetSelStart write FOnGetSelStart;
    property OnGetSelStop: TGetTextFieldIntEvent read FOnGetSelStop write FOnGetSelStop;
    property OnGetCurrentDocId: TGetDocIdEvent read FOnGetCurrentDocId write FOnGetCurrentDocId;
    property OnGetTextFieldId: TGetTextFieldIdEvent read FOnGetTextFieldId write FOnGetTextFieldId;
    property OnGetTextFieldCount: TGetDocIntEvent read FOnGetTextFieldCount write FOnGetTextFieldCount;
    property OnGetTextFieldLength: TGetTextFieldLengthEvent read FOnGetTextFieldLength write FOnGetTextFieldLength;
    property OnGetTextFieldText: TGetTextFieldTextEvent read FOnGetTextFieldText write FOnGetTextFieldText;
    property OnGetCurrentDocTitle: TGetDocTitleEvent read FOnGetCurrentDocTitle write FOnGetCurrentDocTitle;
    property OnSelectTextField: TSelectTextFieldEvent read FOnSelectTextField write FOnSelectTextField;
    property OnReplaceTextField: TReplaceTextFieldEvent read FOnReplaceTextField write FOnReplaceTextField;
  end;

implementation

type
  TAntidoteVcl = class(TAntidoteAPI)
  private
    FAntidote: TAntidote;
  protected
    function DonneIdDocumentCourant(out idDoc: Integer): HRESULT; override;
    function DonneDebutSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT; override;
    function DonneFinSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT; override;
    function DonneNbZonesDeTexte(idDoc: Integer; out retour: Integer): HRESULT; override;
    function DonneIdZoneDeTexte(idDoc, indice: Integer; out retour: Integer): HRESULT; override;
    function DonneIdZoneDeTexteCourante(idDoc: Integer; out retour: Integer): HRESULT; override;
    function DonneLongueurZoneDeTexte(idDoc, idZone: Integer; out retour: Integer): HRESULT; override;
    function DonneIntervalle(idDoc, idZone, leDebut, laFin: Integer; out retour: WideString): HRESULT; override;
    function DonneTitreDocCourant(out retour: WideString): HRESULT; override;
    function RemplaceIntervalle(idDoc, idZone, leDebut, laFin: Integer; const leTexte: WideString): HRESULT; override;
    function SelectionneIntervalle(idDoc, idZone, leDebut, laFin: Integer): HRESULT; override;
  public
    constructor Create(AOwner: TAntidote);
  end;

{ TAntidoteVcl }

constructor TAntidoteVcl.Create(AOwner: TAntidote);
begin
  inherited Create;
  FAntidote := AOwner;
end;

function TAntidoteVcl.DonneIdDocumentCourant(out idDoc: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetCurrentDocID) then
  begin
    FAntidote.FOnGetCurrentDocID(FAntidote, idDoc);
    if idDoc <> -1 then
      Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneIdDocumentCourant = ', idDoc);
end;

function TAntidoteVcl.DonneDebutSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetSelStart) then
  begin
    retour := FAntidote.FOnGetSelStart(FAntidote, idDoc, idZone);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneDebutSelection(', idDoc, ', ', idZone, ') = ', retour);
end;

function TAntidoteVcl.DonneFinSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetSelStop) then
  begin
    retour := FAntidote.FOnGetSelStop(FAntidote, idDoc, idZone);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneFinSelection(', idDoc, ', ', idZone, ') = ', retour);
end;

function TAntidoteVcl.DonneNbZonesDeTexte(idDoc: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetTextFieldCount) then
  begin
    retour := FAntidote.FOnGetTextFieldCount(FAntidote, idDoc);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneNbZonesDeTexte(', IdDoc, ') = ', retour);
end;

function TAntidoteVcl.DonneIdZoneDeTexte(idDoc, indice: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetTextFieldId) then
  begin
    retour := FAntidote.FOnGetTextFieldId(FAntidote, idDoc, indice);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneIdZoneDeTexte(', IdDoc, ', ', indice,') = ', retour);
end;

function TAntidoteVcl.DonneIdZoneDeTexteCourante(idDoc: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetTextFieldId) then
  begin
    retour := FAntidote.FOnGetTextFieldId(FAntidote, idDoc, -1);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneIdZoneDeTexteCourante(', IdDoc, ') = ', retour);
end;

function TAntidoteVcl.DonneLongueurZoneDeTexte(idDoc, idZone: Integer; out retour: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetTextFieldLength) then
  begin
    retour := FAntidote.FOnGetTextFieldLength(FAntidote, idDoc, idZone);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneLongueurZoneDeTexte(', IdDoc, ', ', IdZone, ') = ', retour);
end;

function TAntidoteVcl.DonneIntervalle(idDoc, idZone, leDebut, laFin: Integer; out retour: WideString): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnGetTextFieldText) then
  begin
    retour := FAntidote.FOnGetTextFieldText(FAntidote, idDoc, idZone, leDebut, laFin);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneIntervalle(', IdDoc, ', ', IdZone, ', ', leDebut, ', ', laFin, ') = ', retour);
end;

function TAntidoteVcl.DonneTitreDocCourant(out retour: WideString): HRESULT;
var
  title: string;
begin
  title := Application.Title;
  if title = '' then
    title := Screen.ActiveForm.Caption;
  if Assigned(FAntidote.FOnGetCurrentDocTitle) then
  begin
    FAntidote.FOnGetCurrentDocTitle(FAntidote, title);
  end;
  if title = '' then
  begin
    Result := E_NOTIMPL
  end else begin
    retour := title;
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('DonneTitreDocCourant = ', title);
end;

function TAntidoteVcl.RemplaceIntervalle(idDoc, idZone, leDebut, laFin: Integer; const leTexte: WideString): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnReplaceTextField) then
  begin
    FAntidote.FOnReplaceTextField(FAntidote, idDoc, idZone, leDebut, laFin, leTexte);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('RemplaceIntervalle(', IdDoc, ', ', IdZone, ', ', leDebut, ', ', laFin, ', ', leTexte, ')');
end;

function TAntidoteVcl.SelectionneIntervalle(idDoc, idZone, leDebut, laFin: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
  if Assigned(FAntidote.FOnSelectTextField) then
  begin
    FAntidote.FOnSelectTextField(FAntidote, idDoc, idZone, leDebut, laFin);
    Result := S_OK;
  end;
  if TRACE_ANTIDOTE then
    WriteLn('SelectionneIntervalle(', IdDoc, ', ', IdZone, ', ', leDebut, ', ', laFin, ')');
end;

{ TAntidote }

function TAntidote.Correcteur: Boolean;
begin
  if Active = False then
    Exit(False);
  Result := FApi.Outil(kOutilCorrecteur);
end;

destructor TAntidote.Destroy;
begin
  FApi.Free;
  inherited;
end;

const
  Dictionnaires: array[TDictionnaire] of string = (
    kDictionnaireDefinitions,
    kDictionnaireSynonymes,
    kDictionnaireAntonymes,
    kDictionnaireCooccurrences,
    kDictionnaireChampLexical,
    kDictionnaireConjugaison,
    kDictionnaireFamille,
    kDictionnaireCitations,
    kDictionnaireHistorique,
    kDictionnaireIllustrations,
    kDictionnaireWikipedia
  );

function TAntidote.Dictionnaire(ADictionnaire: TDictionnaire): Boolean;
begin
  if Active = False then
    Exit(False);
  Result := FApi.Outil(Dictionnaires[ADictionnaire]);
end;

function TAntidote.GetActive: Boolean;
begin
  if csDesigning in ComponentState then
    Result := FActive
  else
    Result := Assigned(FApi);
end;

const
  Guides: array[TGuide] of string = (
    kGuideOrthographe,
    kGuideLexique,
    kGuideGrammaire,
    kGuideSyntaxe,
    kGuidePonctuation,
    kGuideStyle,
    kGuideRedaction,
    kGuideTypographie,
    kGuidePhonetique,
    kGuideHistorique,
    kGuidePointsDeLangue
  );
function TAntidote.Guide(AGuide: TGuide): Boolean;
begin
  if Active = False then
    Exit(False);
  Result := FApi.Outil(Guides[AGuide]);
end;

procedure TAntidote.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    if csDesigning in ComponentState then
      FActive := Value
    else
    if Value then
      Start
    else begin
//      FreeAndNil(FApi);
      FApi._Release;
      FApi := nil;
    end;
  end;
end;

procedure TAntidote.Start;
begin
  if TAntidoteAPI.Start then
  begin
    FApi := TAntidoteVcl.Create(Self);
    FApi._AddRef;
  end;
end;

end.
