{****************************************************
* 01.02.2021 /bc simple app settings persistence    *
****************************************************}
unit dd_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles, daily_diary_const;
type
  { TDDSettings }
  TDDSettings = class
  private
    fAutoBackup: boolean;
    fBatchCount: integer;
    fBatchUpdates: boolean;
    fFrequency: integer;
    fIni: TIniFile;
    fBackupname: string;
    fDbname: string;
    fLastBackup: string;
    function ReadInifile: boolean;
    function WriteInifile: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    property Databasename: string read fDbname write fDbname;
    property BackupPath: string read fBackupname write fBackupname;
    property AutoBackup: boolean read fAutoBackup write fAutoBackup;
    property BackupFrequency: integer read fFrequency write fFrequency;
    property LastBackup: string read fLastBackup write fLastBackup;
    property BatchUpdates: boolean read fBatchUpdates write fBatchUpdates;
    property BatchCount: integer read fBatchCount write fBatchCount;
  end;

function DDSettings: TDDSettings;

implementation
uses bc_errorlog_mt, bc_msgqueue;
var
  Singleton: TDDSettings;

function DDSettings: TDDSettings;
begin
  if not assigned(Singleton) then Singleton:= TDDSettings.Create;
  Result:= Singleton;
end;

{ TDDSettings }

function TDDSettings.ReadInifile: boolean;
begin
  if not FileExists(fIni.FileName) then WriteInifile; { create the darn thing }
  fDbname:= fIni.ReadString('Files','Databasename',DD_Databasename); { defaults to ./db/DD.db3 }
  fBackupname:= fIni.ReadString('Files','BackupName','Not defined'); { could be empty }
  fAutoBackup:= fIni.ReadBool('Files','AutoBackup',false);
  fFrequency:= fIni.ReadInteger('Files','BackupFrequency',0); { daily, weekly or monthly 0..2 }
  fLastBackup:= fIni.ReadString('Files','LastBackup','01.01.1970'); { unix epoch }
  fBatchUpdates:= fIni.ReadBool('Engine','BatchUpdates',false); { cache updates in db-engine }
  fBatchCount:= fIni.ReadInteger('Engine','BatchCount',3); { no of cache updates in db-engine }
  Result:= true;
end;

function TDDSettings.WriteInifile: boolean;
begin
  if fDbname <> '' then fIni.WriteString('Files','DatabaseName',fDbname)
  else fIni.WriteString('Files','DatabaseName',DD_Databasename); { failsafe }
  if fBackupname <> '' then fIni.WriteString('Files','BackupName',fBackupname)
  else fIni.WriteString('Files','BackupName','Not defined');
  fIni.WriteBool('Files','AutoBackup',fAutoBackup);
  fIni.WriteInteger('Files','BackupFrequency',fFrequency);
  fIni.WriteString('Files','LastBackup',fLastBackup);
  fIni.WriteBool('Engine','BatchUpdates',fBatchUpdates); { initialized on 1.st run to false }
  fIni.WriteInteger('Engine','BatchCount',fBatchCount); { no of cache updates in db-engine }
  fIni.UpdateFile;
  Result:= true;;
end;

constructor TDDSettings.Create;
begin
  inherited Create;
  try
    fIni:= TIniFile.Create(DD_Inifilename); { reads the ini if it exists... }
    fIni.CacheUpdates:= false; { update file immediately }
    ReadInifile;
  except on E:Exception do begin
      ErrorLogMT.PostMsg(-1,LM_LOGMSG,97,8723,E.Message); //bm
    end;
  end;
end;

destructor TDDSettings.Destroy;
begin
  FreeAndNil(fIni);
  inherited Destroy;
end;

procedure TDDSettings.Update;
begin
  WriteInifile;
end;
initialization
  Singleton:= nil;
finalization
  if assigned(Singleton) then FreeAndNil(Singleton);
end.

