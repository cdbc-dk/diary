{****************************************************
* 29.01.2021 bc simple app settings persistence     *
****************************************************}
unit u_dd_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, handykar_const;
type
  { THandykarSettings }
  THandykarSettings = class
  private
    fBatchCount: integer;
    fBatchUpdates: boolean;
    fIni: TIniFile;
    fBackupname: string;
    fDbname: string;
    function ReadInifile: boolean;
    function WriteInifile: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    property Databasename: string read fDbname write fDbname;
    property BackupPath: string read fBackupname write fBackupname;
    property BatchUpdates: boolean read fBatchUpdates write fBatchUpdates;
    property BatchCount: integer read fBatchCount write fBatchCount;
  end;

function AppSettings: THandykarSettings;

implementation
var
  Singleton: THandykarSettings;

function AppSettings: THandykarSettings;
begin
  if not assigned(Singleton) then Singleton:= THandykarSettings.Create;
  Result:= Singleton;
end;

{ THandykarSettings }

function THandykarSettings.ReadInifile: boolean;
begin
  if not FileExists(fIni.FileName) then WriteInifile; { create the darn thing }
  fDbname:= fIni.ReadString('Files','Databasename',HK_Databasename); { defaults to ./db/handykar.db3 }
  fBackupname:= fIni.ReadString('Files','BackupName','Not defined'); { could be empty }
  fBatchUpdates:= fIni.ReadBool('Engine','BatchUpdates',false); { cache updates in db-engine }
  fBatchCount:= fIni.ReadInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  Result:= true;
end;

function THandykarSettings.WriteInifile: boolean;
begin
  if fDbname <> '' then fIni.WriteString('Files','DatabaseName',fDbname)
  else fIni.WriteString('Files','DatabaseName',HK_Databasename); { failsafe }
  if fBackupname <> '' then fIni.WriteString('Files','BackupName',fBackupname)
  else fIni.WriteString('Files','BackupName','Not defined');
  fIni.WriteBool('Engine','BatchUpdates',fBatchUpdates); { initialized on 1.st run to false }
  fIni.WriteInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  fIni.UpdateFile;
  Result:= true;;
end;

constructor THandykarSettings.Create;
begin
  inherited Create;
  fIni:= TIniFile.Create(HK_Inifilename); { reads the ini if it exists... }
  fIni.CacheUpdates:= false; { update file immediately }
  ReadInifile;
end;

destructor THandykarSettings.Destroy;
begin
  FreeAndNil(fIni);
  inherited Destroy;
end;

procedure THandykarSettings.Update;
begin
  WriteInifile;
end;
initialization
  Singleton:= nil;
finalization
  if assigned(Singleton) then FreeAndNil(Singleton);
end.

