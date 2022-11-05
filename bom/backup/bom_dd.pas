unit bom_dd;
{$mode objfpc}{$H+}
{$define debug}
{$define id_dd}
interface
uses
  Classes, SysUtils, db,
  dialogs,
  bc_utilities,
  dd_settings,
  daily_diary_const,
  bc_datetime,
  bc_mtlinklist,
  bc_litedb,
  bc_observer;
type
  { TNamedMemorystream }
  TNamedMemorystream = class(TMemoryStream)
  private
    fName: string;
  public
    constructor Create(const aName: string);
    property Name: string read fName write fName;
  end;

  TDDCollection = class; { forward declaration }
  { *** TDDCollectionItem *** }
  TDDCollectionItem = class(TCollectionItem)
  private
    fId_DD: ptruint;       // id from database
    fDate: TIsoDate;       // well duh!
    fDateStr: string;      // could make ordering easier
    fWeekNumber: ptruint;  // week number
    fText: TStream;        // binary large object - can be anything
//    fText: TNamedMemorystream;
    fReserved: string;     // text field reserved for future use
    fModified: byte;       // modification status
  protected
    procedure AssignData(aSource: TDDCollectionItem); // nifty little feature
  public
    constructor Create(aCollection: TDDCollection);
    destructor Destroy; override;
    property Id_DD: ptruint read fId_DD write fId_DD;
    property Date: TIsoDate read fDate write fDate;
    property DateStr: string read fDateStr write fDateStr;
    property WeekNumber: ptruint read fWeekNumber write fWeekNumber;
    property Text: TStream read fText write fText;
//    property Text: TNamedMemorystream read fText write fText;
    property Reserved: string read fReserved write fReserved;
    property Modified: byte read fModified write fModified;
  end; { TDDCollectionItem }

  TDDCollectionItemClass = class of TDDCollectionItem;

  { *** TDDQueue *** }
  TDDQueue = class(TbcQueue)
  public
    function CreateNew: TDDCollectionItem;
    procedure Enqueue(anItem: TDDCollectionItem);
    function Dequeue: TDDCollectionItem;
    function Peek: TDDCollectionItem; // only peeking does not remove from Q
  end;

  { *** TDDCollection *** }
  TDDCollection = class(TCollection)
  private
    fBatch: boolean;
    fSortOrder: integer;
    fDDItemClass: TDDCollectionItemClass;
    function get_DbName: string;
    function get_EngineVersion: string;
    procedure set_DbName(aValue: string);
    procedure DeleteItem(anItem: TDDCollectionItem); { remove item from collection }
  protected
    fDb: TLiteDb;
    fDeltaQueue: TDDQueue;
    fUpdateCount: ptrint;
    { fObserved standalone }
    fObserved: TObserved;
    procedure DoUpdate(HasConnection: boolean = false);  { refactored 29.07.2015 bc }
    function AddRecord(anItem: TDDCollectionItem): ptruint; // result is the new ID for the current record
    procedure UpdateRecord(anItem: TDDCollectionItem;HasConnection: boolean = false);
    procedure DeleteRecord(anItem: TDDCollectionItem;HasConnection: boolean = false); { remove from database backend }
  public
    constructor Create(anItemClass: TDDCollectionItemClass);
    destructor Destroy; override;
    function CheckTable: boolean; { creates a new table in db-file }
    function Add_Dd: TDDCollectionItem;
    function AddNew: TDDCollectionItem;
    procedure BackupDb; { 19.04.2015 bc }
    procedure AppendToDelta(anItem: TDDCollectionItem;HasConnection: boolean = false); { api }
    function GetItemFromID(const anId: ptruint): TDDCollectionItem;
    function IndexOf(anItem: TDDCollectionItem): ptrint; { 11.05.2015 bc, returns -1 on not found else collection ID }
    function UpdateDb(const UpdateNow: boolean;HasConnection: boolean = false): boolean; { refactored 29.07.2015 bc }
    function ReadDb: boolean;
    function ReadBlobDb(const Asc: boolean): boolean;
    { test }
    property Observed: TObserved read fObserved; // ææ
    { test end }
    property DDItemClass: TDDCollectionItemClass read fDDItemClass write fDDItemClass;
    property UpdateCount: ptrint read fUpdateCount write fUpdateCount;
    property DbName: string read get_DbName write set_DbName;
    property BatchUpdate: boolean read fBatch write fBatch;
    property SortOrder: integer read fSortOrder write fSortOrder;
    property EngineVersion: string read get_EngineVersion;
  end; { *** TDDCollection *** }

{ Bom - factory }
function CreateBom: TDDCollection;

implementation
uses bc_memdataset;
var Singleton: TDDCollection;

function CreateBom: TDDCollection;
begin
  if not assigned(Singleton) then Singleton:= TDDCollection.Create(TDDCollectionItem);
  Result:= Singleton;
end;

{ *** TDDCollectionItem *** }
{ nifty feature if you need to clone an item }
procedure TDDCollectionItem.AssignData(aSource: TDDCollectionItem);
begin
  fId_DD:= aSource.Id_DD;           // id from database
  fDate.AsInteger:= aSource.Date.AsInteger; // only copy data, NOT pointers
  fDateStr:= aSource.DateStr;       // searchable datestring
  fWeekNumber:= aSource.WeekNumber; // week number
  aSource.Text.Position:= 0;        // reset to beginning of stream
  fText.Position:= 0;               // reset to beginning of stream
  fText.CopyFrom(aSource.Text,aSource.Text.Size); // copy stream data
  fReserved:= aSource.Reserved;     // text field reserved for future use
  fModified:= aSource.Modified;     // modification states ~ mNone, mAdded, mAltered & mDelete
end;

{ TNamedMemorystream }
constructor TNamedMemorystream.Create(const aName: string);
begin
  inherited Create;
  fName:= aName;
end;

{ ================== TDDQueue ================== }
function TDDQueue.CreateNew: TDDCollectionItem;
begin
  Result:= TDDCollectionItem.Create(nil); // no collection, ie. not appended yet
end;

procedure TDDQueue.Enqueue(anItem: TDDCollectionItem);
begin
  En_Queue(pointer(anItem));
end;

function TDDQueue.Dequeue: TDDCollectionItem;
begin
  Result:= TDDCollectionItem(De_Queue);
end;

function TDDQueue.Peek: TDDCollectionItem;
begin
  Result:= TDDCollectionItem(Examine);
end;

{ ================== TDDQueue ================== }

{ compares first names ie,:
    Result = 1 -> Item1 is greater than Item2
    Result = -1 -> Item1 is smaller than Item2
    else they are equal -> 0 }
function DDCompareDate(Item1, Item2: TCollectionItem): Integer;
begin
  if TDDCollectionItem(Item1).Date.AsInteger > TDDCollectionItem(Item2).Date.AsInteger then Result:= 1
  else if TDDCollectionItem(Item1).Date.AsInteger < TDDCollectionItem(Item2).Date.AsInteger then Result:= -1
  else Result:= 0;
end;

function DDCompareWeekNo(Item1, Item2: TCollectionItem): Integer;
begin
  if TDDCollectionItem(Item1).Date.ISOWeekNumber > TDDCollectionItem(Item2).Date.ISOWeekNumber then Result:= 1
  else if TDDCollectionItem(Item1).Date.ISOWeekNumber < TDDCollectionItem(Item2).Date.ISOWeekNumber then Result:= -1
  else Result:= 0;
end;

function TDDCollection.get_DbName: string;
begin
  Result:= fDb.DbName;
end;

function TDDCollection.get_EngineVersion: string;
begin
  Result:= daily_diary_const.UnitVersion;
end;

procedure TDDCollection.set_DbName(aValue: string);
begin
  fDb.DbName:= aValue;
end;

procedure TDDCollection.DeleteItem(anItem: TDDCollectionItem);
var
  Res,ItemId: ptrint;
begin
  Res:= -1;
  ItemId:= anItem.ID;
  Res:= bcSearch(ItemId,Self);
  if Res > -1 then Delete(Res);
end;

function TDDCollection.CheckTable: boolean; { creates a new db if not existing }
begin
  Result:= true;
  { creates a new db and table if not existing }
  try fDb.RunSQL(daily_diary_const.CreateDb); except  end;
end;

function TDDCollection.Add_Dd: TDDCollectionItem; { ok }
begin
//  Result:= fDeltaQueue.CreateNew;
  Result:= fDDItemClass.Create(Self);      { created with owner collection }
  Result.Id_DD:= 0;
  Result.Date.AsDate:= now;
end;

{ updates the database according to the modified state }
procedure TDDCollection.DoUpdate(HasConnection: boolean = false); { refactored 29.07.2015 bc }
var
  Tmp,New: TDDCollectionItem;
begin
  while not fDeltaQueue.IsEmpty do begin
    Tmp:= fDeltaQueue.Dequeue;
    case Tmp.Modified of
      mAdded:   begin
                  New:= Add_Dd;        { gets an ownership from collection }
                  AddRecord(Tmp);                    { persist in database }
                  New.AssignData(Tmp);         { copy data to the new item }
                  { test }
//                  Observed.FPONotifyObservers(Observed.Subject,ooAddItem,pointer(Tmp));
                  Observed.FPONotifyObservers(Observed.Subject,ooAddItem,pointer(New));
                  { test end }
                end;
      mAltered: UpdateRecord(Tmp);           { persist changes in database }
      mDelete:  begin
                  DeleteRecord(Tmp); { takes care of the database back-end }
                  DeleteItem(Tmp);  { removes the item from our collection }
                end;
    end; { end case }
    FreeAndNil(Tmp);
  end;
end;

{ addrecord persists anitem to database and returns the new row_id as a result }
function TDDCollection.AddRecord(anItem: TDDCollectionItem): ptruint;
begin
  if fDb.Connect then try                  { connect checks for connected }
    if not fDb.Transaction.Active then begin
      fDb.Transaction.StartTransaction;
      fDb.Query.Close;
      fDb.Query.SQL.Text:= InsSql;
      fDb.Query.Prepare;
      fDb.Query.ParamByName('pdate').AsInteger:= anItem.Date.AsInteger;
      fDb.Query.ParamByName('pdatestr').AsString:= anItem.DateStr;
      fDb.Query.ParamByName('pweekno').AsInteger:= anItem.Date.ISOWeekNumber;
      anItem.Text.Position:= 0; { always remember to reset position }
      fDb.Query.ParamByName('ptext').LoadFromStream(anItem.Text,ftBlob);
      fDb.Query.ParamByName('pres').AsString:= anItem.Reserved;
      fDb.Query.ExecSQL;
      {$ifdef id_dd}
      { now get a hold of our last entry Id_DD }
      fDb.Query.Close;
      fDb.Query.SQL.Text:= LastId_ddSql; { 'SELECT id_dd FROM daily_diary;' }
      fDb.Query.Open;
      fDb.Query.Last;
      Result:= fDb.Query.FieldByName('id_dd').AsInteger;
      anItem.Id_DD:= Result;
      fDb.Query.Close;
      {$else}
      { now get a hold of our last db Id primary key }
      fDb.Query.Close;
      fDb.Query.SQL.Text:= LastIdSql; { 'SELECT LAST_INSERT_ROWID() AS Id_Last;' }
      fDb.Query.Open;
      Result:= fDb.Query.FieldByName('Id_Last').AsInteger;
      anItem.Id_DD:= Result;
      fDb.Query.Close;
      {$endif}
      fDb.Transaction.Commit;
      anItem.Modified:= mNone;
    end;
    fDb.DisConnect;                              { no dangling connections }
  except on E:Exception do Showmessage('Error in addrecord '+E.Message); end;
end;

procedure TDDCollection.UpdateRecord(anItem: TDDCollectionItem;
                                     HasConnection: boolean = false);
begin
  if not HasConnection then begin
    fDb.Connect;                            { connect checks for connected }
  end;
  if not fDb.Transaction.Active then begin
    fDb.Transaction.StartTransaction;
    fDb.Query.Close;
    fDb.Query.SQL.Text:= UpdSql;
    fDb.Query.Prepare;
    fDb.Query.ParamByName('pdate').AsInteger:= anItem.Date.AsInteger;
    fDb.Query.ParamByName('pdatestr').AsString:= anItem.DateStr;
    fDb.Query.ParamByName('pweekno').AsInteger:= anItem.Date.ISOWeekNumber;
    anItem.Text.Position:= 0; { always remember to reset position }
    fDb.Query.ParamByName('ptext').LoadFromStream(anItem.Text,ftBlob);
    fDb.Query.ParamByName('pres').AsString:= anItem.Reserved;
    fDb.Query.ParamByName('pid').AsInteger:= anItem.Id_DD;
    fDb.Query.ExecSQL;
    fDb.Transaction.Commit;
    anItem.Modified:= mNone;
  end;
  if not HasConnection then begin
    fDb.DisConnect;                              { no dangling connections }
  end;
end;

procedure TDDCollection.DeleteRecord(anItem: TDDCollectionItem;
                                     HasConnection: boolean = false); { ok }
begin
  if not HasConnection then begin
    fDb.Connect;                            { connect checks for connected }
  end;
  if not fDb.Transaction.Active then begin
    fDb.Transaction.StartTransaction;
    fDb.Query.Close;
    fDb.Query.SQL.Text:= DelSql; { 'DELETE FROM daily_diary WHERE id_dd=:pid;' }
    fDb.Query.Prepare;
    fDb.Query.ParamByName('pid').AsInteger:= anItem.Id_DD;
    fDb.Query.ExecSQL;
    fDb.Transaction.Commit;
  end;
  if not HasConnection then begin
    fDb.DisConnect;                              { no dangling connections }
  end;
//  DeleteItem(anItem);                    { delete item from our collection }
end;

constructor TDDCollection.Create(anItemClass: TDDCollectionItemClass); { ok }
begin
  inherited Create(anItemClass);                { get our collection going }
  fDDItemClass:= anItemClass;
  fDb:= TLiteDb.Create;                       { create our database engine }
  fDb.DbName:= DDSettings.Databasename;                   { 02.02.2021 /bc }
  fDb.Connect;        { connect to our database, if nonexisting create one }
  CheckTable;                               { create table if non existing }
  if fDb.Connected then fDb.DisConnect;              { no idle connections }
  fDeltaQueue:= TDDQueue.Create;       { create a queue for our operations }
  fBatch:= false;
  fBatch:= DDSettings.BatchUpdates;                       { 19.04.2015 /bc }
  fUpdateCount:= DDSettings.BatchCount;                   { 19.04.2015 /bc }
  { test }
  fObserved:= TObserved.Create(Self);
  { test end }
end;

destructor TDDCollection.Destroy;
begin
  if not fDeltaQueue.IsEmpty then UpdateDb(true);
  fDeltaQueue.Free;
  if fDb.Connected then fDb.DisConnect;
  fDb.Free;
  { test }
  FreeAndNil(fObserved);
  { test end }
  inherited Destroy;
end;

function TDDCollection.AddNew: TDDCollectionItem; { ok }
begin
  Result:= fDeltaQueue.CreateNew;
  Result.Id_DD:= 0;
  Result.Date.AsDate:= now;
end;

procedure TDDCollection.BackupDb; { ok }
var
  BackupFilename: string;
  Buffer: array[0..4095] of byte; { 4 Kb buffer }
  InStream,OutStream: TFileStream;
  Cnt,I,Res: Int64;
begin
  BackupFilename:= DDSettings.BackupPath;
  FillChar(Buffer,4096,0); { 11.05.2015 bc }
  if BackupFilename <> 'Not defined' then begin
    { now construct the actual backupname with a date and .bak extension }
    BackupFilename:= ExtractFilePath(DDSettings.BackupPath)+ExtractFileName(DDSettings.Databasename);
    system.insert('_',BackupFilename,length(BackupFilename)-3); // +_
    system.insert(bcDateToStr(now),BackupFilename,length(BackupFilename)-3); // +19.04.2015
    BackupFilename:= ChangeFileExt(BackupFilename,'.bak');  // *.bak
    if FileExists(BackupFilename) then DeleteFile(BackupFilename); { 09.05.2015 bc }
    if fDb.Connected then fDb.DisConnect; { sanity check }
    InStream:= TFileStream.Create(DDSettings.Databasename,fmOpenRead);
    try
      InStream.Seek(0,fsFromBeginning);
      OutStream:= TFileStream.Create(BackupFilename,fmCreate);
      try
        OutStream.Seek(0,fsFromBeginning);
        Cnt:= InStream.Size; Res:= 0;
        { implemented by hand }
        while Cnt > 0 do begin
          if Cnt > 4096 then I:= 4096 else I:= Cnt;
          InStream.ReadBuffer(Buffer,I);
          OutStream.WriteBuffer(Buffer,I);
          dec(Cnt,I);
          inc(Res,I);
        end;
        if Res <> InStream.Size then raise Exception.Create('Backup failed! Db-file and backup-file differs in size!');
        { implemented in TStream, uses much bigger buffer }
//        OutStream.CopyFrom(InStream,InStream.Size);
      finally
        FreeAndNil(OutStream);
      end;
    finally
      FreeAndNil(InStream);
    end;
  end;
end;

procedure TDDCollection.AppendToDelta(anItem: TDDCollectionItem;
                                      HasConnection: boolean = false); { ok }
begin
  if assigned(anItem) then begin
    fDeltaQueue.Enqueue(anItem); // add to delta for db persistence
    { in case of delete, force updatenow 29.07.2015 bc }
    if anItem.Modified = mDelete then UpdateDb(true,HasConnection)
    else UpdateDb(not fBatch,HasConnection); // updates now or every n-th record
  end;
end;

function TDDCollection.GetItemFromID(const anId: ptruint): TDDCollectionItem;
var
  Idx: ptruint;
begin
  Result:= nil;
  if Self.Count = 0 then ShowMessage('ERROR: Collection is emty!');
exit;
  { perform a linear search }
//  for Idx:= 0 to Self.Count-1 do begin
  try
    for Idx:= Count-1 downto 0 do begin              { searching backwards }
      if TDDCollectionItem(Items[Idx]).Id_DD = anId then begin
        Result:= TDDCollectionItem(Items[Idx]);                 { found it }
        break;                                                      { done }
      end;
    end;
  except  end;
end;

function TDDCollection.IndexOf(anItem: TDDCollectionItem): ptrint; { ok }
var
  Idx: longint;
  Tmp: TDDCollectionItem;
begin
  Result:= -1; { not found }
  for Idx:= 0 to Count-1 do begin { linear search (O(n)) }
    Tmp:= TDDCollectionItem(Items[Idx]);
    if ((Tmp.Date.AsInteger = anItem.Date.AsInteger) and (Tmp.WeekNumber = anItem.WeekNumber) and
       (Tmp.Text = anItem.Text) and (Tmp.Reserved = anItem.Reserved)) then begin
      Result:= Idx;
      Break;
    end;
  end;
end;

function TDDCollection.UpdateDb(const UpdateNow: boolean;HasConnection: boolean = false): boolean; { ok }// db writes
begin
  Result:= false;
  if not HasConnection then begin
    if not fDb.Connected then fDb.Connect;
  end;
  if not UpdateNow then begin { cater for batch updates }
    if fDeltaQueue.Count >= fUpdateCount then DoUpdate;
  end else DoUpdate;
  if not HasConnection then begin
    if fDb.Connected then fDb.DisConnect;  //ææ
  end;
  Result:= true;
end; { refactored 29.07.2015 bc -> actual writes moved to "DoUpdate" }

function TDDCollection.ReadDb: boolean; // db reads
var
  Ds: TMemDataset;
  Bci: TDDCollectionItem;
begin
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  BeginUpdate;
  Ds:= TMemDataset.Create(nil);
  try
    fDb.QuerySQL(daily_diary_const.SelSqlAsc,Ds); // fills the dataset with fielddefs and data
    Ds.First;
    while not Ds.EOF do begin
      Bci:= TDDCollectionItem(Add);
      Bci.Id_DD:= Ds.FieldByName('id_dd').AsInteger;
      Bci.Date.AsInteger:= Ds.FieldByName('date_dd').AsInteger;
      Bci.WeekNumber:= Ds.FieldByName('weeknumber_dd').AsInteger;
//      Bci.Text:= Ds.FieldByName('text_dd'); // should not work!
      Bci.Reserved:= Ds.FieldByName('reserved_dd').AsString;
      Ds.Next;
    end;
    Result:= true;
  finally Ds.Free; end;
  { now sort the collection, according to user preference }
  case fSortOrder of
    0: Sort(@DDCompareDate); // sort by date
    1: Sort(@DDCompareWeekNo); // sort by weeknumber
  end;
  EndUpdate;
  if fDb.Connected then fDb.DisConnect;
end;

function TDDCollection.ReadBlobDb(const Asc: boolean): boolean;
var
  anItem: TDDCollectionItem;
  BlobStream: TStream;
begin
  anItem:= nil;
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  if not fDb.Transaction.Active then begin
    fDb.Transaction.StartTransaction;
    BeginUpdate;
    fDb.Query.Close;
    if Asc then fDb.Query.SQL.Text:= SelSqlAsc
    else fDb.Query.SQL.Text:= SelSqlDesc;
    fDb.Query.Open;
    fDb.Query.First;
    while not fDb.Query.EOF do begin

      anItem:= Add_Dd;                               { added to collection }
      anItem.Id_DD:= fDb.Query.FieldByName('id_dd').AsInteger;
      anItem.Date.AsInteger:= fDb.Query.FieldByName('date_dd').AsInteger;
      anItem.DateStr:= fDb.Query.FieldByName('datestr_dd').AsString;
      anItem.WeekNumber:= fDb.Query.FieldByName('weeknumber_dd').AsInteger;
      anItem.Text.Position:= 0;
      BlobStream:= fDb.Query.CreateBlobStream(fDb.Query.FieldByName('text_dd'),bmRead);
      anItem.Text.CopyFrom(BlobStream,BlobStream.Size);
      BlobStream.Free;
      anItem.Reserved:= fDb.Query.FieldByName('reserved_dd').AsString;
      anItem.Modified:= mNone;        { make sure we don't get added twice }
      fDb.Query.Next;
    end;
    EndUpdate;
    fDb.Transaction.Commit;
  end;
ShowMessageFmt('Collection count: %d',[Self.Count]);
  if fDb.Connected then fDb.DisConnect;          { no dangling connections }
  Result:= true;
  Observed.FPONotifyObservers(Observed.Subject,ooCustom,pointer(Count));
end;

(*
var
  BlobStream: TStream;
  M: TFileStream;

BlobStream:= fDb.Query.CreateBlobStream(fDb.Query.FieldByName('content_photos'),bmRead);
M:= TFileStream.Create(SaveDialog1.FileName,fmCreate);
BlobStream.Position:= 0;
M.CopyFrom(BlobStream,BlobStream.Size);
M.Free;
BlobStream.Free;
*)

constructor TDDCollectionItem.Create(aCollection: TDDCollection);
begin
  inherited Create(aCollection);
  fDate:= TIsoDate.Create(now);
  fText:= TMemoryStream.Create;
end;

destructor TDDCollectionItem.Destroy;
begin
  FreeAndNil(fDate);
  FreeAndNil(fText);
  inherited Destroy;
end;

initialization
  Singleton:= nil;
finalization
  FreeAndNil(Singleton);
end.


