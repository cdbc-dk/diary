unit lfm_dataaware;
{$mode ObjFPC}{$H+}
{-$define debug}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, LazUtf8,
  LMessages,       { provides messages and records nescesary in msg communicating * }
  LCLIntf,         { provides functions and utilities nescesary in msg communicating * }
  bom_dd,          { provides the business object model }
  bc_observer,     { implements the observer pattern used in tpersistence }
  bc_datetime,     { provides advanced date and time classes + utility functions }
  bc_trvhelp,      { provides versatile function for dealing with ttreeview }
  tfm_settings,    { provides the "settings" frame * }
  bc_utilities;    { provides amongst others stream concatenation }

type
  { state modes represented as a set }
  TFormMode = (fmInsert,fmEdit,fmBrowse,fmDelete,fmInactive);
  TFormModes = set of TFormMode;
  { TbcMemo, a small hack :o) }
  TbcMemo = class(Tmemo)
  public
    function get_Caret: TPoint; { declared protected in TCustomMemo }
    procedure set_Caret(AValue: TPoint); { declared protected in TCustomMemo }
    property Caret: TPoint read get_Caret write set_Caret;
  end;
  { TObserver derivative }
  TDDObserver = class(TObserver)
  public
    Procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer); override;
  end;
  { TfrmDataAware }
  TfrmDataAware = class(TForm)
    btnEditCancel: TSpeedButton;
    btnEditSave: TSpeedButton;
    gbxNavigation: TGroupBox;
    gbxText: TGroupBox;
    ImageList1: TImageList;
    lblName: TLabel;
    memText: TMemo;
    pctPages: TPageControl;
    pnlEditButtons: TPanel;
    pnlTop: TPanel;
    btnClose: TSpeedButton;
    btnReadData: TSpeedButton;
    btnUpdate: TSpeedButton;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnSettings: TSpeedButton;
    Splitter1: TSplitter;
    stbStatus: TStatusBar;
    tabMainDiary: TTabSheet;
    trvNav: TTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditCancelClick(Sender: TObject);
    procedure btnEditSaveClick(Sender: TObject);
    procedure btnReadDataClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memTextClick(Sender: TObject);
    { set tpagecontrol -> options ->  nboShowCloseButtons = true * }
    procedure pctPagesCloseTabClicked(Sender: TObject); { needed for closing tabs, when you're on them * }
    procedure pnlTopClick(Sender: TObject);
    procedure trvNavSelectionChanged(Sender: TObject);
  private
    fBom: TDDCollection;
    fObserver: TDDObserver;
    fDt: IIsoDateTime;
    fRootNode: TTreeNode; { level 0 ~ root }
    fLevel1: TTreeNode;   { level 1 ~ 1.st year }
    fLevel2: TTreeNode;   { level 2 ~ 1.st week }
    fFormModes: TFormModes;
    fSettings: TfraSettings; { * }
    fNewItem: TDDCollectionItem; { for when we'll add a new entry to our dataset }
    fEditItem: TDDCollectionItem; { for editing an entry in our dataset }
    function TestNewNode: string;
    procedure CheckFormMode;
    procedure ConcatenateStreams(aStream1, aStream2: TStream);
    procedure PutFormInEditMode(anEditMode: boolean); { lock down controls and focus on memo }
    procedure ScrollMemoTo(const aMemo: TMemo;anOrigin: TSeekOrigin);
    procedure SetTextboxCaption(const aPrefix: string = ' ';anItem: TDDCollectionItem = nil;const aSuffix: string = ' ');
    function SerializeSet(aSet: TFormModes): string;
  public
    procedure AddNewTreeNode(anItem: TDDCollectionItem);
    procedure DeleteTreeNode(anItem: TDDCollectionItem);
    procedure FindAndAddYearNode(anItem: TDDCollectionItem);
    procedure ShowDataRead(aDataset: TDDCollection);
    procedure ShowEditButtons(aVisible: boolean);
    procedure LMCloseTabsheet(var Msg: TLMCloseTabsheet); message LM_CLOSETABSHEET; { \o/ trickery } {*}
    procedure OnSettingsChange(aSender: TObject);
  end;

var
  frmDataAware: TfrmDataAware;

implementation
uses LCLType{, bc_errorlog_mt}, bc_msgqueue, dd_settings, bc_pcthelp; {*}
{$R *.lfm}

{ TbcMemo }

function TbcMemo.get_Caret: TPoint;
begin
  if Visible then Result:= GetCaretPos; { declared protected in TCustomMemo }
end;

procedure TbcMemo.set_Caret(aValue: TPoint);
begin
  if Visible then SetCaretPos(aValue); { declared protected in TCustomMemo }
end;

{ TDDObserver }
procedure TDDObserver.FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
var
  Ds: TDDCollection;
  DsI: TDDCollectionItem;
  Gui: TfrmDataAware;
begin
  Gui:= TfrmDataAware(ActiveObject); { Gui / ActiveObject is our owner ie. user interface }
  Ds:= TDDCollection(aSender); { sender is our dataset }
  case Operation of
    ooAddItem   : begin
                    DsI:= TDDCollectionItem(Data);
                    Gui.AddNewTreeNode(DsI);
                    Gui.fFormModes:= [fmBrowse,fmInactive];
                    Gui.SetTextboxCaption(' ',DsI,' ');   //bm
                    Gui.CheckFormMode;
                  end;
    ooChange    : begin
                    DsI:= TDDCollectionItem(Data);
                    Gui.fFormModes:= [fmBrowse,fmInactive];
                    Gui.SetTextboxCaption(' ',DsI,' ');
                    Gui.fEditItem:= nil;
                    Gui.CheckFormMode;
                  end;
    ooCustom    : Gui.ShowDataRead(Ds);
    ooDeleteItem: begin
                    DsI:= TDDCollectionItem(Data);
                    Gui.DeleteTreeNode(DsI);
                  end;
  end;
end;

{ TfrmDataAware }
procedure TfrmDataAware.pnlTopClick(Sender: TObject);

begin
//  fBom.BackupData;

end;

procedure TfrmDataAware.trvNavSelectionChanged(Sender: TObject);
var Item: TDDCollectionItem;
begin
  { load the text from the dataset into our memo }
  case trvNav.Selected.Level of
    0: begin
         trvNav.Selected.Expand(false);
         memText.Lines.Text:= 'Root, carries a reference to our dataset';
       end;
    1: begin
         trvNav.Selected.Expand(false);
         SetTextboxCaption(' Year: ',nil,trvNav.Selected.Text);
         memText.Clear;
       end;
    2: begin
         trvNav.Selected.Expand(false);
         SetTextboxCaption(' Week: ',nil,trvNav.Selected.Text);
         memText.Clear;
       end;
    3: begin
         Item:= TDDCollectionItem(trvNav.Selected.Data);
         SetTextboxCaption(' ',Item,' ');
         Item.Text.Position:= 0; { seek to beginning of stream }
         memText.Lines.LoadFromStream(Item.Text);
       end;
  end;
end;

function TfrmDataAware.TestNewNode: string;
//var anItem: TDDCollectionItem;
begin
  Result:= 'Nothing Box';
(*
  anItem:= TDDCollectionItem.Create(nil);
  anItem.Date.AsString:= '30.09.2022';
  AddNewTreeNode(anItem);
  anItem.Date.AsString:= '03.10.2022';
  AddNewTreeNode(anItem);
  anItem.Date.AsString:= '18.11.2022';
  AddNewTreeNode(anItem);
  anItem.Date.AsString:= '10.12.2022';
  AddNewTreeNode(anItem);
  anItem.Date.AsString:= '08.01.2023';
  AddNewTreeNode(anItem);
  Result:= anItem.Date.AsString+', '+anItem.Date.WeekNumberAsString+', '+anItem.Date.DayName;
  anItem.Free;
*)
end;

procedure TfrmDataAware.CheckFormMode;
begin
  if (fmInsert in fFormModes) or (fmEdit in fFormModes) then PutFormInEditMode(true);
  if (fmBrowse in fFormModes) or (fmInactive in fFormModes) then PutFormInEditMode(false);
end;

procedure TfrmDataAware.btnEditCancelClick(Sender: TObject);
begin
  if pnlEditButtons.Tag = 1 then begin
    if fmInsert in fFormModes then if assigned(fNewItem) then FreeAndNil(fNewItem); { throw created item away }
    if fmEdit in fFormModes then if assigned(fEditItem) then begin
      SetTextboxCaption(' ',fEditItem,' ');
      fEditItem:= nil; { we were just borrowing it }
    end;
    fFormModes:= [fmBrowse,fmInactive];
    CheckFormMode;
  end;
end;

procedure TfrmDataAware.btnEditSaveClick(Sender: TObject);
begin
  if fmEdit in fFormModes then if assigned(fEditItem) then begin
    { avoid "overspill" when user deletes from text }
    TMemoryStream(fEditItem.Text).Clear; { safe typecast, i know what it is }
    fEditItem.Text.Position:= 0;
    memText.Lines.SaveToStream(fEditItem.Text);
    fEditItem.Modified:= mAltered;
    fBom.AppendToDelta(fEditItem);
  end;
  if fmInsert in fFormModes then if assigned(fNewItem) then begin
    fNewItem.Text.Position:= 0;
    memText.Lines.SaveToStream(fNewItem.Text);
    fNewItem.Modified:= mAdded;
    fBom.AppendToDelta(fNewItem);
  end;
end;

procedure TfrmDataAware.ConcatenateStreams(aStream1, aStream2: TStream);
begin
  aStream2.Position:= 0;
  aStream1.CopyFrom(aStream2, aStream2.Size);
  aStream1.Position:= 0;
  aStream2.Position:= 0;
end;

procedure TfrmDataAware.PutFormInEditMode(anEditMode: boolean);
begin
  case anEditMode of
    true : begin
             ShowEditButtons(true);
             memText.ReadOnly:= false;
             memText.SetFocus;
             ScrollMemoTo(memText,soEnd);
             gbxNavigation.Enabled:= false;
             pnlTop.Enabled:= false;
           end;
    false: begin
             fFormModes:= [fmBrowse,fmInactive];
             pnlTop.Enabled:= true;
             gbxNavigation.Enabled:= true;
             trvNav.SetFocus;
             ScrollMemoTo(memText,soBeginning);
             memText.ReadOnly:= true;
             ShowEditButtons(false);
           end;
  end; { case }
end;

procedure TfrmDataAware.ScrollMemoTo(const aMemo: TMemo;anOrigin: TSeekOrigin);
begin
  case anOrigin of
    soBeginning: begin { try to position caret a the beginning of text }
                   { trick the memo into scrolling to the beginning :o) }
                   aMemo.SelStart:= 0; //UTF8Length(aMemo.Text)-5;
                 end;
    soEnd:       begin { try to position caret a the end of text }
                   { trick the memo into scrolling to the end :o) }
                   aMemo.SelStart:= UTF8Length(aMemo.Text);
                 end;
  end;
end;

procedure TfrmDataAware.SetTextboxCaption(const aPrefix: string = ' ';anItem: TDDCollectionItem = nil;const aSuffix: string = ' ');
begin
  if assigned(anItem) then gbxText.Caption:= aPrefix+anItem.Date.DayName+' d. '+anItem.Date.AsString+aSuffix
  else gbxText.Caption:= aPrefix+aSuffix;
end;

function TfrmDataAware.SerializeSet(aSet: TFormModes): string;
const
  Modes: array[TFormMode] of String[10] = ('fmInsert','fmEdit','fmBrowse','fmDelete','fmInactive');
var
  Fm: TFormMode;
begin
  Result:= '';
  for Fm:= fmInsert to fmInactive do if Fm in aSet then begin
    if (Result <> '') then Result+= ', ';
    Result+= Modes[Fm]; { shorthand for "result:= result + modes[fm];" }
  end;
end;

procedure TfrmDataAware.AddNewTreeNode(anItem: TDDCollectionItem);
var
  YearNode,WeekNode,DateNode: TTreeNode;
begin
  if trvNav.Items.Count = 0 then exit;
  { insert node before every other year, week or date-nodes, our dataset is sorted in reverse }
  if not assigned(fLevel1) then fLevel1:= trvNav.Items[1]; { first yearnode is level 1 }
  if not assigned(fLevel2) then fLevel2:= trvNav.Items[2]; { first weeknode is level 2 }
  { first check if the year exist or not }
  YearNode:= GetNodeByTextAtLevel(trvNav,anItem.Date.YearAsString,false,1);
  if YearNode <> nil then begin
    { ok, year does exist, figure out if the week does too }
    WeekNode:= GetNodeByTextAtLevel(trvNav,anItem.Date.WeekNumberAsString,false,2);
    if WeekNode <> nil then begin { ok, week does exist, has it got the right year as parent? }
      if WeekNode.Parent = YearNode then begin
        { ok, we've got the right weeknode, now add our new datenode }
        DateNode:= AddFirstChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));
      end; { year and week exists }
    end else begin
      { year exists, week does not, we'll add it and a datenode }
      WeekNode:= AddFirstChildNodeWithData(trvNav,YearNode,anItem.Date.WeekNumberAsString,nil);
      DateNode:= AddFirstChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));
    end; { year exists, no week }
  end else begin
    { it's a new year, we'll add a yearnode, this will become our new flevel1-node }
    fLevel1:= AddFirstChildNodeWithData(trvNav,fRootNode,anItem.Date.YearAsString,nil);
    { right, that means no weeknode with this year as parent exist, we'll add our new flevel2-node }
    fLevel2:= AddFirstChildNodeWithData(trvNav,fLevel1,anItem.Date.WeekNumberAsString,nil);
    { this also means no datenode with this year & week as parents exist, we'll add a new datenode }
    DateNode:= AddFirstChildNodeWithData(trvNav,fLevel2,anItem.Date.AsString,pointer(anItem));
  end; { it's a new year }
  DateNode.Selected:= true;
end;

procedure TfrmDataAware.FindAndAddYearNode(anItem: TDDCollectionItem);
var
  YearNode, WeekNode, DateNode: TTreeNode;
begin
  { find out if we've already added a node with this year, all yearnodes are level 1 }
  YearNode:= GetNodeByTextAtLevel(trvNav,anItem.Date.YearAsString,true,1);
  if YearNode <> nil then begin
    { find out if we've already added a node with this week, all weeknodes are level 2 }
    WeekNode:= GetNodeByTextAtLevel(trvNav,anItem.Date.WeekNumberAsString,true,2);
    if WeekNode <> nil then begin
      if WeekNode.Parent = YearNode then begin
        { ok, we've got the right weeknode, now find out if we've already added a node with this date,
          all datenodes are level 3 }
        DateNode:= GetNodeByTextAtLevel(trvNav,anItem.Date.AsString,true,3);
        if DateNode <> nil then begin
          if DateNode.Parent = WeekNode then begin
            { hmmm, existing datenode, must concatenate the 2 streams... }
            bc_utilities.ConcatenateStreams(TDDCollectionItem(DateNode.Data).Text,anItem.Text,true);
            YearNode.Collapse(true);
          end;
        end else begin
          { add our new datenode }
          DateNode:= AddChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));  // 1.st date
          YearNode.Collapse(true);
        end;
      end;
    end else begin
      { add our new weeknode and datenode }
      WeekNode:= AddChildNodeWithData(trvNav,YearNode,anItem.Date.WeekNumberAsString,nil);  // n.th week
      DateNode:= AddChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));  // n.th date
      YearNode.Collapse(true);
    end;
  end else begin { root assigned, yearnode does not exist = nil }
    if trvNav.Items.Count = 1 then begin
      fLevel1:= AddChildNodeWithData(trvNav,fRootNode,anItem.Date.YearAsString,nil);  // 1.st year
      WeekNode:= AddChildNodeWithData(trvNav,fLevel1,anItem.Date.WeekNumberAsString,nil);  // 1.st week
      DateNode:= AddChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));  // 1.st date
      fLevel1.Collapse(true);
    end else begin
      YearNode:= AddChildNodeWithData(trvNav,fRootNode,anItem.Date.YearAsString,nil);  // n.th year
      WeekNode:= AddChildNodeWithData(trvNav,YearNode,anItem.Date.WeekNumberAsString,nil);  // n.th week
      DateNode:= AddChildNodeWithData(trvNav,WeekNode,anItem.Date.AsString,pointer(anItem));  // n.th date
      YearNode.Collapse(true);
    end;
  end;
end;

procedure TfrmDataAware.ShowDataRead(aDataset: TDDCollection);
var
  Ci: TCollectionItem;
  Item: TDDCollectionItem;
begin
  fFormModes:= [fmBrowse];
  { add a root node with data to the tree, data is a pointer to our dataset }
  trvNav.BeginUpdate; { speeds things up considerably }
  if trvNav.Items.Count = 0 then fRootNode:= AddRootNode(trvNav,'Dates',pointer(aDataset))
  else fRootNode:= trvNav.Items[0];
  { now run through our dataset and populate our treeview }
  for Ci in aDataset do begin
    Item:= TDDCollectionItem(Ci);
    FindAndAddYearNode(Item);
  end;
  fRootNode.Selected:= true;
  fRootNode.Collapse(true);
  fRootNode.Expand(false);
  trvNav.Items[3].Selected:= true; { show the first date on startup }
  trvNav.EndUpdate;
  fFormModes+= [fmInactive,fmBrowse];
end;

procedure TfrmDataAware.DeleteTreeNode(anItem: TDDCollectionItem);
var Tn: TTreeNode;
begin
  Tn:= GetNodeByTextAtLevel(trvNav,anItem.Date.AsString,false,3);
  if Tn <> nil then begin
    trvNav.Selected:= Tn.Parent;
    trvNav.Items.Delete(Tn);
  end;
end;

procedure TfrmDataAware.ShowEditButtons(aVisible: boolean);
begin
  pnlEditButtons.Visible:= aVisible;
  pnlEditButtons.Tag:= ptrint(aVisible);
end;

procedure TfrmDataAware.LMCloseTabsheet(var Msg: TLMCloseTabsheet); { * }
begin
  pctPagesCloseTabClicked(TObject(Msg.WParam));
end;

procedure TfrmDataAware.OnSettingsChange(aSender: TObject); { aSender contains the settings object }
var S: TDDSettings;
begin
  S:= TDDSettings(aSender); // DDSettings;
  fBom.BatchUpdate:= S.BatchUpdates;
  fBom.UpdateCount:= S.BatchCount;
  fBom.DbName:= S.Databasename;
//  fBom.AutoBackup:= S.AutoBackup;
end;

procedure TfrmDataAware.FormCreate(Sender: TObject);
begin
  fDt:= TIsoDateTime.Create(Now);
  fBom:= CreateBom;
  fObserver:= TDDObserver.Create(Self);
  fBom.AttachObserver(fObserver); { refactored 11.10.2022 /bc }
//  fBom.Observed.FPOAttachObserver(fObserver);
//  Caption:= fDt.AsISOString;
  Caption:= fDt.AsString;
  fFormModes:= [fmBrowse,fmInactive];
end;

procedure TfrmDataAware.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDataAware.btnAddClick(Sender: TObject);
var
  Tn: TTreeNode;
begin
  { is this the first entry today or is it n.th? }
  Tn:= GetNodeByTextAtLevel(trvNav,fDt.Date.AsString,false,3);
  if Tn <> nil then begin
    { ok, it's #n, so we'll just update the existing entry }
    Tn.Selected:= true;
    fFormModes:= [fmEdit];
    fEditItem:= TDDCollectionItem(trvNav.Selected.Data);
    SetTextboxCaption(' Editing [',fEditItem,'] ');
    CheckFormMode;
    fNewItem:= nil;
  end else begin
    { hells bells, we've got a new entry, yiihaaa... }
    fFormModes:= [fmInsert];
    fNewItem:= fBom.CreateNew; { CreateNew creates a new item without owner, that comes later }
    SetTextboxCaption(' New [',fNewItem,'] ');
    memText.Clear;
    CheckFormMode;
    fEditItem:= nil;
  end;
end;

procedure TfrmDataAware.btnDeleteClick(Sender: TObject);
var Item: TDDCollectionItem;
begin
  if trvNav.Selected.Level <> 3 then exit; { user can only manipulate level 3 nodes! }
  fFormModes:= [fmDelete];
  Item:= TDDCollectionItem(trvNav.Selected.Data);
  if messagedlg('Delete diary page '+trvNav.Selected.Text+' ?',
                mtConfirmation,
                [mbNo,mbYes],
                0) = mrYes then begin   //bm
    Item.Modified:= mDelete;
    fbom.AppendToDelta(Item);
  end;
  fFormModes:= [fmInactive,fmBrowse];
end;

procedure TfrmDataAware.btnReadDataClick(Sender: TObject);

begin
  fBom.ReadDataWithBlob(false); // read ascending dates
  btnReadData.Enabled:= false; // only run once on app-startup
  fFormModes+= [fmBrowse,fmInactive];
end;

procedure TfrmDataAware.btnSettingsClick(Sender: TObject);
var
  pgIsNew: boolean;
  tabSettings: TTabSheet;
begin
  tabSettings:= bcFindPageByName(pctPages,'Settings',true,true,pgIsNew);
  tabSettings.PageIndex:= 1;
  if pgIsNew then begin
    fSettings:= CreateSettingsFrame(tabSettings,DDSettings);
    fSettings.OnChange:= @OnSettingsChange;
    fSettings.chbCachedUpd.Enabled:= false; { not worth the trouble! }
  end;
end;

procedure TfrmDataAware.btnUpdateClick(Sender: TObject);
begin
  if trvNav.Selected = nil then exit;
  if trvNav.Selected.Level = 3 then begin
    fFormModes:= [fmEdit];
    fEditItem:= TDDCollectionItem(trvNav.Selected.Data);
    SetTextboxCaption(' Editing [',fEditItem,'] ');
    CheckFormMode;
  end;
end;

procedure TfrmDataAware.FormDestroy(Sender: TObject);
begin
  Exclude(fFormModes,fmBrowse);
  fBom.DetachObserver(fObserver); { refactored 11.10.2022 /bc }
  fBom.Clear;
  fBom:= nil; { memory gets released / freed on program end, in bom_dd.pas }
  fObserver.Free;
  fDt:= nil;
end;

procedure TfrmDataAware.FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  case Key of
    $4E: if ssCtrl in Shift then btnAddClick(Sender);      { [ctrl] + [N] ~ VK_N ~ #78 }
    $71: btnUpdateClick(Sender);                           { F2 ~ VK_F2 ~ #113 }
    $1B: btnEditCancelClick(Sender);                       { Esc ~ VK_ESCAPE ~ #27}
    $2E: if not memText.Focused then btnDeleteClick(Sender); { Delete ~ VK_DELETE ~ #46 }
    $53: if memText.Focused then
           if ssCtrl in Shift then btnEditSaveClick(Sender); { VK_S ~ #83 = [ctrl] + [S]}
  end;
end;

procedure TfrmDataAware.FormResize(Sender: TObject);
begin
  btnEditSave.Left:= pnlEditButtons.Width - 32;
  btnEditSave.Top:= 2;
  lblName.Left:= ((pnlEditButtons.Width div 2) - (lblName.Width div 2));
end;

procedure TfrmDataAware.FormShow(Sender: TObject);
begin
  btnEditSave.Left:= pnlEditButtons.Width - 32;
  btnEditSave.Top:= 2;
  lblName.Left:= ((pnlEditButtons.Width div 2) - (lblName.Width div 2));
  btnReadDataClick(Sender);
  if trvNav.CanSetFocus then trvNav.SetFocus;
  stbStatus.SimpleText:= lblName.Caption;
end;

procedure TfrmDataAware.memTextClick(Sender: TObject);
begin
  if fmBrowse in fFormModes then btnUpdateClick(Sender);
end;

procedure TfrmDataAware.pctPagesCloseTabClicked(Sender: TObject); { * }
begin
  (Sender as TTabSheet).Free;
end;

end.

