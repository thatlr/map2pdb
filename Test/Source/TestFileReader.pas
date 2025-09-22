unit TestFileReader;

interface

uses
  Classes, Types,
  FileTestFramework,
  debug.info,
  debug.info.reader;

type
  TCustomMapTest = class(TFileTestCase)
  strict private
    FDebugInfo: TDebugInfo;
  private
  protected
    procedure DoLoadFromFile;
    procedure ProcessDebugInfo(DebugInfo: TDebugInfo); virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestFileReader = class(TCustomMapTest)
  published
    procedure TestLoadFromFile;
  end;

implementation

uses
  Windows,
  SysUtils,
  IOUtils,
  TestFramework,
  debug.info.reader.map,
  debug.info.reader.test,
  debug.info.reader.jdbg;

type
  TInputFormat = (ifMap, ifJdbg, ifTest);
const
  sInputFileTypes: array[TInputFormat] of string = ('.map', '.jdbg', '.test');
  ReaderClasses: array[TInputFormat] of TDebugInfoReaderClass = (TDebugInfoMapReader, TDebugInfoJdbgReader, TDebugInfoSyntheticReader);

function TryStrToInputFormat(const AName: string; var InputFormat: TInputFormat): boolean;
begin
  for var InFormat := Low(TInputFormat) to High(TInputFormat) do
    if (SameText(AName, sInputFileTypes[InFormat])) then
    begin
      InputFormat := InFormat;
      Exit(True);
    end;
  Result := False;
end;

procedure TCustomMapTest.SetUp;
begin
  FDebugInfo := TDebugInfo.Create;
end;

procedure TCustomMapTest.TearDown;
begin
  FDebugInfo.Free;
  FDebugInfo := nil;
end;

procedure TCustomMapTest.DoLoadFromFile;
begin
  (*
  ** Determine source file format
  *)
  var InputFormat: TInputFormat;
  var FileType := TPath.GetExtension(TestFileName);
  if (not TryStrToInputFormat(FileType, InputFormat)) then
  begin
    Check(True);
    Status('Unknown file format');
    exit;
  end;

  (*
  ** Read source file
  *)
  var ReaderClass: TDebugInfoReaderClass := ReaderClasses[InputFormat];

  var Reader := ReaderClass.Create;
  try

    Reader.LoadFromFile(TestFileName, FDebugInfo);

  finally
    Reader.Free;
  end;

  ProcessDebugInfo(FDebugInfo);
end;

procedure TCustomMapTest.ProcessDebugInfo(DebugInfo: TDebugInfo);
begin
  Check(DebugInfo.Segments.Count > 0, 'No segments');
  Check(DebugInfo.Modules.Count > 0, 'No modules');
  if (DebugInfo.SourceFiles.Count = 0) then
    Self.Status('No source files');
end;

procedure TTestFileReader.TestLoadFromFile;
begin
  DoLoadFromFile;
end;


initialization
  var TestSuite := TFolderTestSuite.Create('Load map files', TTestFileReader, '..\..\..\Data', '*.*', True);
  RegisterTest(TestSuite);
end.


