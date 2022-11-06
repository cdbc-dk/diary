unit textsearch_dd;
{$mode ObjFPC}{$H+}
interface
uses Classes, SysUtils, bc_bomintf, bom_dd, bc_advstring,bc_textsearch;

type
  { TddTextSearch }
  TddTextSearch = class(TFreeTextSearch)
  protected
    fPatternType: TbcStringType;
  public
    constructor Create(aDataset: TObject); override;
    procedure OnEnumerateItemDD(aSender: TObject;anItem: TDDCollectionItem;UserData: pointer;var aCancel: boolean);
    function SearchDataset(const aPattern: string;aCaseSensitive: boolean;aMatchAll: boolean = true): SizeInt; override;
  end;

implementation
//uses bc_advstring;
{ TddTextSearch }

constructor TddTextSearch.Create(aDataset: TObject);
begin
  if not aDataset.GetInterface(SGUIDIBom_dd,fDataset) then raise exception.CreateFmt('Error! %s does not implement an IBom_dd interface.',[aDataset.ClassName]);
  inherited Create(aDataset);
end;

procedure TddTextSearch.OnEnumerateItemDD(aSender: TObject;
                                          anItem: TDDCollectionItem;
                                          UserData: pointer;
                                          var aCancel: boolean);
var
  I: TBomItem;
  Pt: string;
  SRec: TTextSearchRec;
begin

end;

function TddTextSearch.SearchDataset(const aPattern: string;
                                     aCaseSensitive: boolean;
                                     aMatchAll: boolean): SizeInt;
begin
  { initialize and clear for go! }
  if aPattern = '' then exit; //gId:= 0; // for now bogus ids
  fPatternType:= bcGetStringType(aPattern);
  fCaseSensitive:= aCaseSensitive;
  fMatchAll:= aMatchAll;
  Clear;
  { we're sending our search-pattern along in UserData }
  IBom_dd(fDataset).EnumerateDD(@aPattern[1],@OnEnumerateItemDD);
  { we've now filled our result-set, release excess memory }
  SetLength(fSearchArray,fMatchesCount);
  Result:= fMatchesCount;
end;

end.
(*

begin
if anItem is TBomItem then I:= TBomItem(anItem) else exit;
Pt:= pchar(UserData);
try
  if bcFindMatches(Pt,(I.Date.AsString+' '+I.Description),fCaseSensitive,SRec.srPos,fMatchAll) then begin
    SRec.srId:= I.Id;
    SRec.srKey:= Pt;
    SRec.srCount:= Length(SRec.srPos);
    SRec.srLength:= UTF8Length(Pt);
    SRec.srName:= I.Date.AsString;
    AddMatch(SRec);
  end;
except aCancel:= true; end;
*)
