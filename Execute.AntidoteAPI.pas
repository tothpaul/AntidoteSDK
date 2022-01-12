unit Execute.AntidoteAPI;

{
   Antidote 11 SDK for Delphi (c)2022 Execute SARL <https://www.execute.fr>
}

interface
{$IFDEF DEBUG}
{.$DEFINE TRACE_ANTIDOTE}
{.$DEFINE TRACE_ANTIDOTE2}
{$ENDIF}
uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Win.Registry;

const
  TRACE_ANTIDOTE = {$IFDEF TRACE_ANTIDOTE}TRUE{$ELSE}FALSE{$ENDIF};

  kOutilCorrecteur = 'C';

  kDictionnaireDernierChoisi = 'D';
  kDictionnaireDefaut = 'D_Defaut';
  kDictionnaireDefinitions = 'D_Definitions';
  kDictionnaireSynonymes = 'D_Synonymes';
  kDictionnaireAntonymes = 'D_Antonymes';
  kDictionnaireCooccurrences = 'D_Cooccurrences';
  kDictionnaireChampLexical = 'D_ChampLexical';
  kDictionnaireConjugaison = 'D_Conjugaison';
  kDictionnaireFamille = 'D_Famille';
  kDictionnaireCitations = 'D_Citations';
  kDictionnaireHistorique = 'D_Historique';
  kDictionnaireIllustrations = 'D_Illustrations';
  kDictionnaireWikipedia = 'D_Wikipedia';

  kGuideDernierChoisi = 'G';
  kGuideParDefaut = 'G_Defaut';
  kGuideOrthographe = 'G_Orthographe';
  kGuideLexique = 'G_Lexique';
	kGuideGrammaire = 'G_Grammaire';
	kGuideSyntaxe = 'G_Syntaxe';
	kGuidePonctuation = 'G_Ponctuation';
	kGuideStyle = 'G_Style';
	kGuideRedaction = 'G_Redaction';
	kGuideTypographie = 'G_Typographie';
	kGuidePhonetique = 'G_Phonetique';
	kGuideHistorique = 'G_Historique';
	kGuidePointsDeLangue = 'G_PointsDeLangue';

  kLangueSelonContexte = '';
  kLangueForcerFrancais = 'fr';
  kLangueForcerAnglais = 'en';

type
  IAntidote = dispinterface
  ['{00020400-0000-0000-C000-000000000046}']
    procedure ClientApiEnFermeture(const p_nomDeClasse: WideString); dispid $06;
    procedure ClientApiEnFermetureDispatch(p_dispatch: IDispatch); dispid $0c;
	  procedure LanceOutil(const p_nomDeClasse, p_lOutil, langue, versionAPI: WideString); dispid $0f;
    procedure LanceOutilDispatch2(p_dispatch: IDispatch; const p_lOutil, langue, versionAPI: WideString); dispid $10;
  end;

  IAntidoteCallback = interface(IDispatch)
    function ActiveApplication: HRESULT;
    function ActiveDocument(idDoc: Integer): HRESULT;
    function DonneDebutSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT;
    function DonneFinSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT;
    function DonneIdDocumentCourant(out idDoc: Integer): HRESULT;
    function DonneIdZoneDeTexte(idDoc, indice: Integer; out retour: Integer): HRESULT;
    function DonneIdZoneDeTexteCourante(idDoc: Integer; out retour: Integer): HRESULT;
    function DonneIntervalle(idDoc, idZone, leDebut, laFin: Integer; out retour: WideString): HRESULT;
    function DonneLongueurZoneDeTexte(idDoc, idZone: Integer; out retour: Integer): HRESULT;
    function DonneNbZonesDeTexte(idDoc: Integer; out retour: Integer): HRESULT;
    function DonnePolice(idDoc, idZone: Integer; out retour: WideString): HRESULT;
    function DonneTitreDocCourant(out retour: WideString): HRESULT;
    function RemplaceIntervalle(idDoc, idZone, leDebut, laFin: Integer; const leTexte: WideString): HRESULT;
    function SelectionneIntervalle(idDoc, idZone, leDebut, laFin: Integer): HRESULT;
  end;

  TAntidoteApi = class(TObject, IInterface, IDispatch, IAntidoteCallBack)
  private
    class var Path: string;
    class var Version: string;
    FRefCount: Integer;
    FAntidote: IAntidote;
    function GetAntidote: Boolean;
  public
  // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  // IDispatch
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  // IAntidoteCallBack
    function ActiveApplication: HRESULT; virtual;
    function ActiveDocument(idDoc: Integer): HRESULT; virtual;
    function DonneDebutSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT; virtual;
    function DonneFinSelection(idDoc, idZone: Integer; out retour: Integer): HRESULT; virtual;
    function DonneIdDocumentCourant(out idDoc: Integer): HRESULT; virtual;
    function DonneIdZoneDeTexte(idDoc, indice: Integer; out retour: Integer): HRESULT; virtual;
    function DonneIdZoneDeTexteCourante(idDoc: Integer; out retour: Integer): HRESULT; virtual;
    function DonneIntervalle(idDoc, idZone, leDebut, laFin: Integer; out retour: WideString): HRESULT; virtual;
    function DonneLongueurZoneDeTexte(idDoc, idZone: Integer; out retour: Integer): HRESULT; virtual;
    function DonneNbZonesDeTexte(idDoc: Integer; out retour: Integer): HRESULT; virtual;
    function DonnePolice(idDoc, idZone: Integer; out retour: WideString): HRESULT; virtual;
    function DonneTitreDocCourant(out retour: WideString): HRESULT; virtual;
    function RemplaceIntervalle(idDoc, idZone, leDebut, laFin: Integer; const leTexte: WideString): HRESULT; virtual;
    function SelectionneIntervalle(idDoc, idZone, leDebut, laFin: Integer): HRESULT; virtual;
  public
    function Outil(const Name: string): Boolean;
    class function IsActive: Boolean;
    class function GetInfo: Boolean;
    class function IsAvailable: Boolean;
    class function Start: Boolean;
    class constructor Create;
  end;

implementation

const
  idActiveApplication = 1;
  idActiveDocument = 2;
  idDonneDebutSelection = 3;
  idDonneFinSelection = 4;
  idDonneIdDocumentCourant = 5;
  idDonneIdZoneDeTexte = 6;
  idDonneIdZoneDeTexteCourante = 7;
  idDonneIntervalle = 8;
  idDonneLongueurZoneDeTexte = 9;
  idDonneNbZonesDeTexte = 10;
  idDonnePolice = 11;
  idDonneTitreDocCourant = 12;
  idRemplaceIntervalle = 13;
  idSelectionneIntervalle = 14;

  MapIDs: array[1..14] of string = (
    'ActiveApplication',
    'ActiveDocument',
    'DonneDebutSelection',
    'DonneFinSelection',
    'DonneIdDocumentCourant',
    'DonneIdZoneDeTexte',
    'DonneIdZoneDeTexteCourante',
    'DonneIntervalle',
    'DonneLongueurZoneDeTexte',
    'DonneNbZonesDeTexte',
    'DonnePolice',
    'DonneTitreDocCourant',
    'RemplaceIntervalle',
    'SelectionneIntervalle'
  );


{ TAntidoteApi }

function TAntidoteApi.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TAntidoteApi._AddRef: Integer;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('TAntidoteApi._AddRef');{$ENDIF}
  Result := AtomicIncrement(FRefCount);
end;

function TAntidoteApi._Release: Integer;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('TAntidoteApi._Release');{$ENDIF}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
  {$IFDEF TRACE_ANTIDOTE}WriteLn(' => Destroy');{$ENDIF}
    Destroy;
  end;
end;

function TAntidoteApi.GetTypeInfoCount(out Count: Integer): HResult;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('GetTypeInfoCount');{$ENDIF}
  Count := Length(MapIDs);
  Result := S_OK;
end;

function TAntidoteApi.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('GetTypeInfo');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
var
  Name: PPChar;
  pID: PCardinal;
  Index: Integer;
  ID: Integer;
begin
{$IFDEF TRACE_ANTIDOTE2}WriteLn('GetIDsOfNames');{$ENDIF}
  Name := Names;
  pID := DispIDs;
  for Index := 0 to NameCount - 1 do
  begin
    for ID := Low(MapIDs) to High(MapIDs) do
    begin
      if Name^ = MapIDS[ID] then
      begin
        pID^ := ID;
        Break;
      end;
    end;
    {$IFDEF TRACE_ANTIDOTE2}WriteLn(' ', Name^ , ' = ', pID^);{$ENDIF}
    Inc(Name);
    Inc(pID);
  end;
  Result := S_OK;
end;

function TAntidoteApi.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  LParams:DISPPARAMS absolute Params;
  iRetour: Integer;
  sRetour: WideString;
begin
{$IFDEF TRACE_ANTIDOTE2}WriteLn('Invoke ', DispID, ' ', MapIDs[DispID]);{$ENDIF}
  case DispID of
    idActiveApplication:
    begin
      Assert(LParams.cArgs = 0);
      Result := ActiveApplication();
    end;
    idActiveDocument:
    begin
      Assert(LParams.cArgs = 1);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := ActiveDocument(LParams.rgvarg[0].lVal);
    end;
    idDonneDebutSelection:
    begin
      Assert(LParams.cArgs = 2);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneDebutSelection(LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneFinSelection:
    begin
      Assert(LParams.cArgs = 2);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneFinSelection(LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneIdDocumentCourant:
    begin
      Assert(LParams.cArgs = 0);
      Result := DonneIdDocumentCourant(iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneIdZoneDeTexte:
    begin
      Assert(LParams.cArgs = 2);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneIdZoneDeTexte(LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneIdZoneDeTexteCourante:
    begin
      Assert(LPArams.cArgs = 1);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneIdZoneDeTexteCourante(LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneIntervalle:
    begin
      Assert(LParams.cArgs = 4);
      Assert(LParams.rgvarg[3].vt = VT_I4);
      Assert(LParams.rgvarg[2].vt = VT_I4);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneIntervalle(LParams.rgvarg[3].lVal, LParams.rgvarg[2].lVal, LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, sRetour);
      OleVariant(VarResult^) := sRetour;
    end;
    idDonneLongueurZoneDeTexte:
    begin
      Assert(LParams.cArgs = 2);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneLongueurZoneDeTexte(LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonneNbZonesDeTexte:
    begin
      Assert(LParams.cArgs = 1);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonneNbZonesDeTexte(LParams.rgvarg[0].lVal, iRetour);
      OleVariant(VarResult^) := iRetour;
    end;
    idDonnePolice:
    begin
      Assert(LParams.cArgs = 2);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := DonnePolice(LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal, sRetour);
      OleVariant(VarResult^) := sRetour;
    end;
    idDonneTitreDocCourant:
    begin
      Assert(LParams.cArgs = 0);
      Result := DonneTitreDocCourant(sRetour);
      OleVariant(VarResult^) := sRetour;
    end;
    idRemplaceIntervalle:
    begin
      Assert(LParams.cArgs = 5);
      Assert(LParams.rgvarg[4].vt = VT_I4);
      Assert(LParams.rgvarg[3].vt = VT_I4);
      Assert(LParams.rgvarg[2].vt = VT_I4);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_BSTR or VT_BYREF);
      Result := RemplaceIntervalle(LParams.rgvarg[4].lVal, LParams.rgvarg[3].lVal, LParams.rgvarg[2].lVal, LParams.rgvarg[1].lVal, LParams.rgvarg[0].pbstrval^);
    end;
    idSelectionneIntervalle:
    begin
      Assert(LParams.cArgs = 4);
      Assert(LParams.rgvarg[3].vt = VT_I4);
      Assert(LParams.rgvarg[2].vt = VT_I4);
      Assert(LParams.rgvarg[1].vt = VT_I4);
      Assert(LParams.rgvarg[0].vt = VT_I4);
      Result := SelectionneIntervalle(LParams.rgvarg[3].lVal, LParams.rgvarg[2].lVal, LParams.rgvarg[1].lVal, LParams.rgvarg[0].lVal);
    end
  else
    Result := E_NOTIMPL;
  end;
end;

function TAntidoteApi.ActiveApplication: HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('ActiveApplication');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.ActiveDocument(idDoc: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('ActiveDocument(', idDoc, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneDebutSelection(idDoc, idZone: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneDebutSelection(', idDoc, ', ', idZone,')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneFinSelection(idDoc, idZone: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneFinSelection(', idDoc, ', ', idZone, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneIdDocumentCourant(out idDoc: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneIdDocumentCourant(', idDoc, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneIdZoneDeTexte(idDoc, indice: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneIdZoneDeTexte(', idDoc, ', ', indice, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneIdZoneDeTexteCourante(idDoc: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneIdZoneDeTexteCourante(', idDoc, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneIntervalle(idDoc, idZone, leDebut, laFin: Integer;
  out retour: WideString): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneIntervalle(', idDoc, ', ', idZone, ', ', leDebut, ', ', laFin, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneLongueurZoneDeTexte(idDoc, idZone: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneLongueurZoneDeTexte(', idDoc, ', ', idZone, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneNbZonesDeTexte(idDoc: Integer;
  out retour: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneNbZonesDeTexte(', idDoc, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonnePolice(idDoc, idZone: Integer;
  out retour: WideString): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonnePolice(', idDoc, ', ', idZone, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.DonneTitreDocCourant(out retour: WideString): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('DonneTitreDocCourant(',retour,')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.RemplaceIntervalle(idDoc, idZone, leDebut, laFin: Integer;
  const leTexte: WideString): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('RemplaceIntervalle(', idDoc, ', ', idZone, ', ', leDebut, ', ', laFin, ', ', leTexte, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.SelectionneIntervalle(idDoc, idZone, leDebut,
  laFin: Integer): HRESULT;
begin
{$IFDEF TRACE_ANTIDOTE}WriteLn('SelectionneIntervalle(', idDoc, ', ', idZone, ', ', leDebut, ', ', laFin, ')');{$ENDIF}
  Result := E_NOTIMPL;
end;

function TAntidoteApi.GetAntidote: Boolean;
var
  clsid: TGUID;
  unk: IUnknown;
begin
  Result := Assigned(FAntidote);
  if not Result then
  begin
    if CLSIDFromProgID('Antidote.ApiOle', clsid) <> S_OK then
      Exit;
    if (GetActiveObject(clsid, nil, Unk) <> S_OK) or (Unk = nil) then
      Exit;
    if (Unk.QueryInterface(IDispatch, FAntidote) <> S_OK) or (FAntidote = nil) then
      Exit;
    Result := True;
  end;
end;


class constructor TAntidoteApi.Create;
begin
{$IFDEF TRACE_ANTIDOTE}
  AllocConsole;
{$ENDIF}
  IsAvailable();
end;

class function TAntidoteApi.GetInfo: Boolean;
var
  Reg: TRegistry;
begin
  Path := '';
  Version := '';
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.OpenKey('SOFTWARE\Druide informatique inc.\Antidote', False) then
      Exit;
    Path := Reg.ReadString('DossierAntidote');
    Version := Reg.ReadString('VersionAPI');
    if (Path = '') or (Version = '') then
      Exit;
    if Path[Length(Path)] <> '\' then
      Path := Path + '\';
    Path := Path + 'Antidote.exe';
    Result := FileExists(Path);
  finally
    Reg.Free;
  end;
end;

class function TAntidoteApi.IsActive: Boolean;
begin
  Result := FindWindow('AntQ', nil) <> 0;
end;

class function TAntidoteApi.IsAvailable: Boolean;
begin
  Result := IsActive or GetInfo;
end;

function TAntidoteApi.Outil(const Name: string): Boolean;
begin
  Result := GetAntidote;
  if Result then
    FAntidote.LanceOutilDispatch2(Self, Name, kLangueSelonContexte, Version);
end;

class function TAntidoteApi.Start: Boolean;
var
  Exec: TShellExecuteInfo;
  Status: Cardinal;
begin
  Result := IsActive;
  if (Result = False) and GetInfo then
  begin
    FillChar(Exec, SizeOf(Exec), 0);
    Exec.fMask := SEE_MASK_NOCLOSEPROCESS;
    Exec.cbSize := SizeOf(Exec);
    Exec.lpVerb := 'open';
    Exec.lpFile := PChar(Path);
    Exec.lpParameters := '-activex';
    Exec.nShow := SW_SHOWNORMAL;
    if not ShellExecuteEx(@Exec) then
      Exit(False);
    Result := IsActive;
    while not Result do
    begin
      if (not GetExitCodeProcess(Exec.hProcess, Status)) or (Status <> STILL_ACTIVE) then
        Break;
      Result := IsActive;
    end;
    CloseHandle(Exec.hProcess);
  end;
end;

end.
