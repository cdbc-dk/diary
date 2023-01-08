unit textsearch_dd;
{$mode ObjFPC}{$H+}
interface
uses Classes, SysUtils, bc_bomintf, bom_dd, bc_advstring,bc_textsearch;

type
  { TddTextSearch }
  TddTextSearch = class(TFreeTextSearch)
  private
    fDataset: IBom_dd;
  protected
    fPatternType: TbcStringType;
  public
    constructor Create(aDataset: TObject);
    procedure OnEnumerateItemDD(aSender: TObject;anItem: TDDCollectionItem;UserData: pointer;var aCancel: boolean);
    function SearchDataset(const aPattern: string;aCaseSensitive: boolean;aMatchAll: boolean = true): SizeInt; override;
  end;

implementation
uses LazUtf8;
{ TddTextSearch }

constructor TddTextSearch.Create(aDataset: TObject);
begin
  if not aDataset.GetInterface(SGUIDIBom_dd,fDataset) then raise exception.CreateFmt('Error! %s does not implement an IBom_dd interface.',[aDataset.ClassName]);
  Init;
end;

procedure TddTextSearch.OnEnumerateItemDD(aSender: TObject;
                                          anItem: TDDCollectionItem;
                                          UserData: pointer;
                                          var aCancel: boolean);
var
  Pt,S: string;
  SRec: TTextSearchRec;
  Len: SizeInt;
begin
  FillChar(SRec,sizeof(TTextSearchRec),0);
  Pt:= pchar(UserData); { searchpattern }
  anItem.Text.Position:= 0;
  Len:= anItem.Text.Size;
  SetLength(S,Len);
  anItem.Text.Read(S[1],Len); { sourcestring }
  try
    if bcFindMatches(Pt,(S+' '+anItem.Date.AsString),fCaseSensitive,SRec.srPos,fMatchAll) then try
        SRec.srId:= anItem.Id_DD;
        SRec.srKey:= Pt;
        SRec.srCount:= Length(SRec.srPos);
        SRec.srLength:= UTF8Length(Pt);
        SRec.srName:= anItem.Date.AsString;
        AddMatch(SRec);    //maybe?!? -> aCancel:= not fMatchAll; { break on first occurrence }
    except aCancel:= true; end;
  finally
    SetLength(S,0);
    Pt:= pchar(nil);
  end;
end;

function TddTextSearch.SearchDataset(const aPattern: string;
                                     aCaseSensitive: boolean;
                                     aMatchAll: boolean): SizeInt;
begin
  { initialize and clear for go! }
  if ((aPattern = '') or (fDataset.ItemCount = 0)) then exit; { nothing to do }
  fPatternType:= bcGetStringType(aPattern);
  fCaseSensitive:= aCaseSensitive;
  fMatchAll:= aMatchAll;
  Clear;
  { we're sending our search-pattern along in UserData }
  fDataset.EnumerateDD(@aPattern[1],@OnEnumerateItemDD);
  { we've now filled our result-set, release excess memory }
  SetLength(fSearchArray,fMatchesCount);
  Result:= fMatchesCount;
end;

end.
(*

begin
if anItem is TBomItem then I:= TBomItem(anItem) else exit;

  if bcFindMatches(Pt,(I.Date.AsString+' '+I.Description),fCaseSensitive,SRec.srPos,fMatchAll) then begin
    SRec.srId:= I.Id;
    SRec.srKey:= Pt;
    SRec.srCount:= Length(SRec.srPos);
    SRec.srLength:= UTF8Length(Pt);
    SRec.srName:= I.Date.AsString;
    AddMatch(SRec);
  end;

*)
