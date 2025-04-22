unit TestFileReader;

interface

uses
  Classes, Types,
  FileTestFramework,
  debug.info,
  debug.info.reader;

type
  TTestFileReader = class(TFileTestCase)
  strict private
    FDebugInfo: TDebugInfo;
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
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

procedure TTestFileReader.SetUp;
begin
  FDebugInfo := TDebugInfo.Create;
end;

procedure TTestFileReader.TearDown;
begin
  FDebugInfo.Free;
  FDebugInfo := nil;
end;

procedure TTestFileReader.TestLoadFromFile;
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

  Check(FDebugInfo.Segments.Count > 0, 'No segments');
  Check(FDebugInfo.Modules.Count > 0, 'No modules');
  if (FDebugInfo.SourceFiles.Count = 0) then
    Self.Status('No source files');
end;


initialization
  var TestSuite := TFolderTestSuite.Create('Load map files', TTestFileReader, '..\..\..\Data', '*.*', True);
  RegisterTest(TestSuite);
end.


