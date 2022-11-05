unit bom_dd;
{$mode objfpc}{$H+}
{$define debug}
{$define id_dd}
interface
uses
  Classes, SysUtils, db, {StrUtils,}
  dialogs,
  bc_bomintf,
  bc_utilities,
  dd_settings,
  daily_diary_const,
  bc_datetime,
  bc_mtlinklist,
  bc_litedb,
  bc_observer;

const
  { GUID for IBom_dd }
  SGUIDIBom_dd  = '{10E77905-295D-43C6-AED1-7825B88C002F}';
  { modification states }
  mNone    = 0;
  mAdded   = 3;
  mAltered = 5;
  mDelete  = 7;

type
  { for use with autobackup }
  TFrequency = (daily,weekly,monthly);
  PAutoRec = ^TAutoRec;
  TAutoRec = record
    abId: ptruint;
    abDate: TIsoDate;
    abFrequency: TFrequency;
    abReserved: string;
  end;

  { TNamedMemorystream }
  TNamedMemorystream = class(TMemoryStream)
  private
    fName: string;
  public
    constructor Create(const aName: string);
    property Name: string read fName write fName;
  end;
  {$interfaces corba} { NO reference counting! }
  IBom_dd = interface;
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
    procedure AssignData(aSource: TDDCollectionItem); virtual;// nifty little feature
  public
    constructor Create(aCollection: TDDCollection); overload;
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
  { callback method for enumeration }
  TDDEnumerateEvent = procedure(aSender: TObject;anItem: TDDCollectionItem;aUserData: pointer;var aCancel: boolean) of object;
  TDDEnumerateCallback = procedure(aSender: TObject;anItem: TDDCollectionItem;aUserData: pointer;var aCancel: boolean);

  { *** TDDQueue *** }
  TDDQueue = class(TbcQueue)
  public
    function CreateNew: TDDCollectionItem;
    procedure Enqueue(anItem: TDDCollectionItem);
    function Dequeue: TDDCollectionItem;
    function Peek: TDDCollectionItem; // only peeking does not remove from Q
  end;
  { Ibom_dd interface }
  IBom_dd = interface(IBom) [SGUIDIBom_dd]
    function get_EngineVersion: string;
    { creates a new item, added to and owned by this collection, date is set to now }
    function Add_Dd: TDDCollectionItem;
    { CreateNew creates a new item without owner! date is set to now }
    function CreateNew: TDDCollectionItem;
    { backs up data to a specified location }
    procedure BackupData; { todo refactor }
    { puts data in the processing queue, everything revolves around this method }
    procedure AppendToDelta(anItem: TDDCollectionItem); { api }
    { returns the item with Item.Id_dd = anId or nil if not found }
    function GetItemFromID(const anId: ptruint): TDDCollectionItem;
    { returns -1 on not found else collection ID index }
    function IndexOf(anItem: TDDCollectionItem): ptrint;
    { will process the deltaqueue, normally the AppendToDelta method calls this internally }
    function UpdateData(const UpdateNow: boolean): boolean; overload; { refactored 02.10.2022 bc }
    { enumerator, fires callback on every item, cancel breaks the loop }
    procedure EnumerateDD(aUserData: pointer;aCallback: TDDEnumerateEvent);
    { attach a new observer }
    Procedure AttachObserver(aObserver : TObject);
    { detach an observer }
    Procedure DetachObserver(aObserver : TObject);
    { Notify all observers of a change }
    Procedure NotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
    { leave this miserable world! }
//    procedure Quit; better live without this!
    { which engine is servicing us? }
    property EngineVersion: string read get_EngineVersion;
    { how many items are there? }
    property ItemCount: ptruint read get_ItemCount;
  end; { IBom_dd }
  {$interfaces com} { reference counted! }
  { *** TDDCollection *** }
  TDDCollection = class(TCollection,IBom_dd)
  private
    fAutobackup: boolean;
    fAutoRec: TAutoRec;
    fBatch: boolean;
    fLastBackup: string;
    fSortOrder: integer;
    fDDItemClass: TDDCollectionItemClass;
    fDataBackedUp: boolean;
    function get_DbName: string;
    function get_EngineVersion: string;
    procedure set_DbName(aValue: string);
    procedure DeleteItem(anItem: TDDCollectionItem); { remove item from collection }
    function get_ItemClass: TClass; virtual; { ibom }
    function get_ItemCount: ptruint; { ibom }
    procedure InitAutorec;
    procedure FiniAutorec;
    function CheckAutobackup: string;
    procedure UpdateAutobackup(out aResult: string);
  protected
    fDb: TLiteDb;
    fDeltaQueue: TDDQueue;
    fUpdateCount: ptrint;
    { fObserved standalone }
    fObserved: TObserved;
    procedure DoUpdate;  { refactored 29.07.2015 bc }
    function AddRecord(anItem: TDDCollectionItem): ptruint; // result is the new ID for the current record
    procedure UpdateRecord(anItem: TDDCollectionItem);
    procedure DeleteRecord(anItem: TDDCollectionItem); { remove from database backend }
  public
    constructor Create(anItemClass: TDDCollectionItemClass);
    destructor Destroy; override;
    function CheckTable: boolean; { creates new tablea in db-file if non eisting }
    { creates a new item, added to and owned by this collection, date is set to now }
    function Add_Dd: TDDCollectionItem;
    { CreateNew creates a new item without owner! date is set to now }
    function CreateNew: TDDCollectionItem;
    { BackupData backs up the database }
    procedure BackupData; { todo refactor }
    procedure AppendToDelta(anItem: TDDCollectionItem); { ibom_dd }
    function GetItemObjFromID(const anId: ptruint): TObject; virtual; { ibom }
    function GetItemFromID(const anId: ptruint): TDDCollectionItem; virtual; { ibom_dd }
    function IndexOfObj(anItem: TObject): ptrint; virtual; { ibom }
    function IndexOf(anItem: TDDCollectionItem): ptrint; { ibom_dd, returns -1 on not found else collection ID }
    function UpdateData: boolean; virtual; overload; { ibom }
    function UpdateData(const UpdateNow: boolean): boolean; overload; { refactored 02.10.2022 bc }
    function ReadData: boolean;              { storage agnostic 2022-08-16 /bc }
    function ReadDataWithBlob(const Asc: boolean): boolean; { storage agnostic 2022-08-16 /bc }
    { enumerator, fires callback on every object, cancel breaks the loop, use typecasting }
    procedure Enumerate(UserData: pointer;aCallback: TbomEnumerateEvent); overload; { ibom }
    { enumerator, fires callback on every dditem, cancel breaks the loop }
    procedure EnumerateDD(aUserData: pointer;aCallback: TDDEnumerateEvent); overload; { ibom_dd }
    { attach a new observer }
    Procedure AttachObserver(aObserver : TObject);
    { detach an observer }
    Procedure DetachObserver(aObserver : TObject);
    { Notify all observers of a change }
    Procedure NotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
//    procedure Quit; { bye bye... }
    property DDItemClass: TDDCollectionItemClass read fDDItemClass write fDDItemClass;
    property UpdateCount: ptrint read fUpdateCount write fUpdateCount;
    property DbName: string read get_DbName write set_DbName;
    property Autobackup: boolean read fAutobackup write fAutobackup;
    property BatchUpdate: boolean read fBatch write fBatch;
    property SortOrder: integer read fSortOrder write fSortOrder;
    property ItemCount: ptruint read get_ItemCount;
    property EngineVersion: string read get_EngineVersion;
    property LastBackup: string read fLastBackup;
  end; { *** TDDCollection *** }

{ Bom - factory }
function CreateBom: TDDCollection;

implementation
uses bc_memdataset, bc_msgqueue, DateUtils{, bc_errorlog_mt};  //bm
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
  Result:= daily_diary_const.MainTitle +' v. '+ daily_diary_const.UnitVersion;
end;

procedure TDDCollection.set_DbName(aValue: string);
begin
  fDb.DbName:= aValue;
end;

procedure TDDCollection.DeleteItem(anItem: TDDCollectionItem);
var
  Idx: ptrint;
begin
  Idx:= bcSearchCollection(anItem.ID,Self,nil);
  if Idx <> -1 then begin
    Delete(Idx); { delete frees/removes and notify observers }
    anItem:= nil; { we nil it here to avoid trouble down the road }
  end;
end;

function TDDCollection.get_ItemClass: TClass;
begin
  Result:= fDDItemClass;
end;

function TDDCollection.get_ItemCount: ptruint;
begin
  Result:= Count;
end;

procedure TDDCollection.InitAutorec;
begin
  fAutoRec.abDate:= TIsoDate.Create(now);
  fAutoRec.abFrequency:= TFrequency(DDSettings.BackupFrequency);
end;

procedure TDDCollection.FiniAutorec;
begin
  fAutoRec.abDate.Free;
end;

function TDDCollection.CheckAutobackup: string;
var
  Ds: TMemDataset;
begin
  Result:= ''; fDataBackedUp:= false;
  Ds:= TMemDataset.Create(nil);
  fDb.Connect; { checks for connected :o) }
  try
    if fDb.QuerySQL(daily_diary_const.SelSqlAB,Ds) then begin
      if Ds.RecordCount = 0 then begin
        fAutoRec.abId:= 0;
        fAutoRec.abDate.AsDate:= now-1;
        fAutoRec.abFrequency:= daily;
        fAutoRec.abReserved:= 'First commit';
      end else begin
        Ds.Last;
        fAutoRec.abId:= Ds.FieldByName('id_ab').AsInteger;
        fAutoRec.abDate.AsInteger:= Ds.FieldByName('date_ab').AsInteger;
        fAutoRec.abFrequency:= TFrequency(DDSettings.BackupFrequency);
        fAutoRec.abReserved:= Ds.FieldByName('reserved_ab').AsString;
      end;
      if fAutoRec.abReserved <> 'First commit' then Result:= fAutoRec.abReserved
      else Result:= fAutoRec.abDate.AsString;
    end;
  finally fDb.DisConnect; Ds.Free; end;
  case fAutoRec.abFrequency of
    daily  : if bcDateToInt(now) - fAutoRec.abDate.AsInteger >= 1 then BackupData;
    weekly : if WeekOf(now) - fAutoRec.abDate.ISOWeekNumber >= 1 then BackupData;
    monthly: if MonthOf(now) - fAutoRec.abDate.Month >= 1 then BackupData;
  end;
  if fDataBackedUp then UpdateAutobackup(Result);
end;

procedure TDDCollection.UpdateAutobackup(out aResult: string);
begin
  fAutoRec.abDate.AsDate:= now;
  if not fDb.Connected then fDb.Connect;
  if not fDb.Transaction.Active then fDb.Transaction.StartTransaction;
  fDb.Exec.SQL.Text:= daily_diary_const.InsSqlAB;
  fDb.Exec.Prepare;
  fDb.Exec.ParamByName('pdate').AsInteger:= fAutoRec.abDate.AsInteger;
  fDb.Exec.ParamByName('pfrequency').AsInteger:= integer(fAutoRec.abFrequency);
  fDb.Exec.ParamByName('pres').AsString:= fAutoRec.abDate.AsString;
  fDb.Exec.ExecSQL;
  fDb.Exec.UnPrepare;
  if fDb.Transaction.Active then fDb.Transaction.Commit;
  if fDb.Connected then fDb.DisConnect;
  aResult:= fAutoRec.abDate.AsString;
  DDSettings.LastBackup:= aResult;
  DDSettings.Update;
end;

function TDDCollection.CheckTable: boolean; { creates a new db if not existing }
begin
  Result:= true;
  { creates a new db and table if not existing }
  try fDb.RunSQL(daily_diary_const.CreateDb); except  end;
  { creates a autobackup table if not existing }
  try fDb.RunSQL(daily_diary_const.CreateAB); except  end;
end;

{ creates a new item, owned by this collection }
function TDDCollection.Add_Dd: TDDCollectionItem; { ok }
begin
  Result:= fDDItemClass.Create(Self);      { created with owner collection }
  Result.Id_DD:= 0;
  Result.Date.AsDate:= now;
end;

{ updates the database according to the modified state }
procedure TDDCollection.DoUpdate; { refactored 03.11.2022 /bc }
var
  Tmp,New: TDDCollectionItem;
begin
  while not fDeltaQueue.IsEmpty do begin
    Tmp:= fDeltaQueue.Dequeue;
    case Tmp.Modified of
      mAdded:   begin
                  New:= Add_Dd;            { gets an ownership from collection }
                  AddRecord(Tmp); { persist in database, returns the new Tmp.Id_DD }
                  New.AssignData(Tmp);             { copy data to the new item }
                  { notify observers that we've added a new item to our dataset }
                  NotifyObservers(Self,ooAddItem,pointer(New));
                  FreeAndNil(Tmp); { tmp is created with CreateNew, thus it's done now }
                end;
      mAltered: begin
                  UpdateRecord(Tmp);             { persist changes in database }
                  { notify observers that we've changed }
                  NotifyObservers(Self,ooChange,pointer(Tmp));
                  Tmp.Modified:= mNone;
                  Tmp:= nil;            { DO NOT FREE!, object is still in use }
                end;
      mDelete:  begin
                  { notify observers that we're deleted, then remove in db & in collection }
                  NotifyObservers(Self,ooDeleteItem,pointer(Tmp));
                  DeleteRecord(Tmp);     { takes care of the database back-end }
                  DeleteItem(Tmp); { removes the item from our collection, free and nil's it }
                end;
    end; { end case }
  end;
end;

{ addrecord persists anitem to database and returns the new row_id as a result }
function TDDCollection.AddRecord(anItem: TDDCollectionItem): ptruint;
begin
  if fDb.Connect then try                       { connect checks for connected }
    if not fDb.Transaction.Active then fDb.Transaction.StartTransaction;
    try
      fDb.Exec.Close;
      fDb.Exec.SQL.Text:= InsSql;
      fDb.Exec.Prepare; (* remember they're a pair *)
      fDb.Exec.ParamByName('pdate').AsInteger:= anItem.Date.AsInteger;
      fDb.Exec.ParamByName('pdatestr').AsString:= anItem.DateStr;
      fDb.Exec.ParamByName('pweekno').AsInteger:= anItem.Date.ISOWeekNumber;
      anItem.Text.Position:= 0; { always remember to reset position }
      fDb.Exec.ParamByName('ptext').LoadFromStream(anItem.Text,ftBlob);
      fDb.Exec.ParamByName('pres').AsString:= anItem.Reserved;
      fDb.Exec.ExecSQL;
      Result:= fDb.LastInsertedId;
      fDb.Exec.UnPrepare; (* remember they're a pair *)
      anItem.Id_DD:= Result;
    finally
      fDb.Transaction.Commit;
      anItem.Modified:= mNone;
    end;
    fDb.DisConnect;                                  { no dangling connections }
  except on E:Exception do Showmessage('Error in addrecord '+E.Message); end;
end;

procedure TDDCollection.UpdateRecord(anItem: TDDCollectionItem);
begin
  if not fDb.Connected then fDb.Connect; { connect checks for connected, but... }
  if not fDb.Transaction.Active then fDb.Transaction.StartTransaction;
  fDb.Exec.Close;
  fDb.Exec.SQL.Text:= UpdSql;
  fDb.Exec.Prepare; {*}
  fDb.Exec.ParamByName('pdate').AsInteger:= anItem.Date.AsInteger;
  fDb.Exec.ParamByName('pdatestr').AsString:= anItem.DateStr;
  fDb.Exec.ParamByName('pweekno').AsInteger:= anItem.Date.ISOWeekNumber;
  anItem.Text.Position:= 0; { always remember to reset position }
  fDb.Exec.ParamByName('ptext').LoadFromStream(anItem.Text,ftBlob);
  fDb.Exec.ParamByName('pres').AsString:= anItem.Reserved;
  fDb.Exec.ParamByName('pid').AsInteger:= anItem.Id_DD;
  fDb.Exec.ExecSQL;
  fDb.Exec.UnPrepare; {* ~ they're a pair }
  if fDb.Transaction.Active then fDb.Transaction.Commit;
  if fDb.Connected then fDb.DisConnect;              { no dangling connections }
  anItem.Modified:= mNone;
end;

procedure TDDCollection.DeleteRecord(anItem: TDDCollectionItem); { ok }
begin
  if not fDb.Connected then fDb.Connect; { connect checks for connected, but... }
  if not fDb.Transaction.Active then fDb.Transaction.StartTransaction;
  fDb.Exec.Close;                                                              //*
  fDb.Exec.SQL.Text:= DelSql; { 'DELETE FROM daily_diary WHERE id_dd=:pid;' }  //*
  fDb.Exec.Prepare;
  fDb.Exec.ParamByName('pid').AsInteger:= anItem.Id_DD;                        //*
  fDb.Exec.ExecSQL;                                                            //*
  fDb.Exec.UnPrepare;
  if fDb.Transaction.Active then fDb.Transaction.Commit;
  if fDb.Connected then fDb.DisConnect;              { no dangling connections }
//  DeleteItem(anItem);                      { delete item from our collection }
end;

constructor TDDCollection.Create(anItemClass: TDDCollectionItemClass); { ok }
begin
  inherited Create(anItemClass);                { get our collection going }
  fDDItemClass:= anItemClass;
  fDb:= TLiteDb.Create;                       { create our database engine }
  fDb.DbName:= DDSettings.Databasename;                   { 02.02.2021 /bc }
  fDb.Connect;        { connect to our database, if nonexisting create one }
  CheckTable;                              { create tables if non existing }
  fAutobackup:= DDSettings.AutoBackup;     { sets up our autobackup system }
  if fAutobackup then InitAutorec;
  if fDb.Connected then fDb.DisConnect;              { no idle connections }
  fDeltaQueue:= TDDQueue.Create;       { create a queue for our operations }
  fBatch:= false;
  fBatch:= DDSettings.BatchUpdates;                       { 19.04.2015 /bc }
  fUpdateCount:= DDSettings.BatchCount;                   { 19.04.2015 /bc }
  fObserved:= TObserved.Create(Self);
  if fAutobackup then fLastBackup:= CheckAutobackup else fLastBackup:= '01.01.1970';
end;

destructor TDDCollection.Destroy;
begin
  if not fDeltaQueue.IsEmpty then UpdateData(true);
  fDeltaQueue.Free;
  if fDb.Connected then fDb.DisConnect;
  fDb.Free;
  FreeAndNil(fObserved);
  if fAutobackup then FiniAutorec;
  inherited Destroy;
end;

{ creates a new item without owner! }
function TDDCollection.CreateNew: TDDCollectionItem; { ok }
begin
  Result:= fDeltaQueue.CreateNew;
  Result.Id_DD:= 0;
  Result.Date.AsDate:= now;
end;

procedure TDDCollection.BackupData; { ok }
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
    fDataBackedUp:= true;
  end;
end;

procedure TDDCollection.AppendToDelta(anItem: TDDCollectionItem); { ok }
begin
  if assigned(anItem) then begin
    fDeltaQueue.Enqueue(anItem); // add to delta for db persistence
    { in case of delete, force updatenow 29.07.2015 bc }
    if anItem.Modified = mDelete then UpdateData(true)
    else UpdateData(not fBatch); // updates now or every n-th record
  end;
end;

function TDDCollection.GetItemObjFromID(const anId: ptruint): TObject;
begin
  Result:= GetItemFromID(anId);
end;

{ fed an Id_DD it will search for and return the appropriate item or nil }
function TDDCollection.GetItemFromID(const anId: ptruint): TDDCollectionItem;
var
  Idx: ptruint;
begin     //TODO: support ascending or descending sort order for quickest result
  Result:= nil;
  { perform a linear search backwards }
  try
    for Idx:= Count-1 downto 0 do begin
      if TDDCollectionItem(Items[Idx]).Id_DD = anId then begin
        Result:= TDDCollectionItem(Items[Idx]);                 { found it }
        break;                                                      { done }
      end;
    end;
  except  end;
end;

function TDDCollection.IndexOfObj(anItem: TObject): ptrint;
begin
  Result:= IndexOf(TDDCollectionItem(anItem));
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

function TDDCollection.UpdateData: boolean;
begin
  Result:= UpdateData(true);
end;

function TDDCollection.UpdateData(const UpdateNow: boolean): boolean;
begin
  Result:= false;
  if not fDb.Connected then fDb.Connect;
  if not UpdateNow then begin { cater for batch updates }
    if fDeltaQueue.Count >= fUpdateCount then DoUpdate;
  end else DoUpdate;
  if fDb.Connected then fDb.DisConnect;
  Result:= true;
{  ErrorLogMT.PostMsg(-1,LM_LOGMSG,integer(fBatch),fUpdateCount,'Updatedata: fbatch, updatecount');} //bm
end;

function TDDCollection.ReadData: boolean; // data read, no blob!
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
end; { ReadData }
(*
function TDDCollection.ReadDb: boolean; deprecated;
begin
  Result:= ReadData;
end;
*)
function TDDCollection.ReadDataWithBlob(const Asc: boolean): boolean;
var
  anItem: TDDCollectionItem;
  BlobStream: TStream;
begin
  anItem:= nil;
  Result:= false;
  Clear;
  if not fDb.Connected then fDb.Connect;
  if not fDb.Transaction.Active then fDb.Transaction.StartTransaction;
  try
    BeginUpdate;
    Clear;
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
      BlobStream.Position:= 0;
      anItem.Text.CopyFrom(BlobStream,BlobStream.Size);
      BlobStream.Free;
      anItem.Reserved:= fDb.Query.FieldByName('reserved_dd').AsString;
      anItem.Modified:= mNone;        { make sure we don't get added twice }
      fDb.Query.Next;
    end;
    EndUpdate;
  finally fDb.Transaction.Commit; end;
  if fDb.Connected then fDb.DisConnect;          { no dangling connections }
  Result:= true;
//  Observed.FPONotifyObservers(Observed.Subject,ooCustom,pointer(Count)); { using ooCustom for 1.st dataread }
  NotifyObservers(Self,ooCustom,pointer(Count));          // @Count is not allowed
end;

procedure TDDCollection.Enumerate(UserData: pointer;aCallback: TbomEnumerateEvent);
var
  Idx: ptruint;
  Cancel: boolean;
begin
  if not assigned(aCallback) then exit;
  Cancel:= false;
  for Idx:= 0 to Count-1 do begin
    aCallback(Self,TDDCollectionItem(Items[Idx]),UserData,Cancel);
    if Cancel then break;
  end;
end;

procedure TDDCollection.EnumerateDD(aUserData: pointer;aCallback: TDDEnumerateEvent);
var
  Idx: ptruint;
  Cancel: boolean;
begin
  if not assigned(aCallback) then exit;
  Cancel:= false;
  for Idx:= 0 to Count-1 do begin
    aCallback(Self,TDDCollectionItem(Items[Idx]),aUserData,Cancel);
    if Cancel then break;
  end;
end; { made this one first... }

procedure TDDCollection.AttachObserver(aObserver: TObject);
begin
  fObserved.FPOAttachObserver(aObserver);
end;

procedure TDDCollection.DetachObserver(aObserver: TObject);
begin
  fObserved.FPODetachObserver(aObserver);
end;

procedure TDDCollection.NotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
begin
  fObserved.FPONotifyObservers(aSender,aOperation,Data);
end;
(*
procedure TDDCollection.Quit;
begin
  Free;
end; *)

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
  { beware, FreeAndNil does not check for nil! had a nasty av with this }
  if assigned(Singleton) then FreeAndNil(Singleton);
end.


