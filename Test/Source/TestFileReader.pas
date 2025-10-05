unit TestFileReader;

interface

uses
  Classes, Types,
  TestFramework,
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

  TTestFileReaderErrors = class(TTestFileReader)
  protected
    procedure RunTest(TestResult: TTestResult); override;
  end;

type
  TFolderTestSuiteSkipErrors = class(TFolderTestSuite)
  protected
    procedure ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string; Recursive: Boolean); override;
  end;

type
  TFolderTestSuiteOnlyErrors = class(TFolderTestSuite)
  protected
    procedure ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string; Recursive: Boolean); override;
  end;

implementation

uses
  Windows,
  SysUtils,
  IOUtils,
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


{ TFolderTestSuiteSkipErrors }

procedure TFolderTestSuiteSkipErrors.ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path,
  FileMask: string; Recursive: Boolean);
begin
  if SameText(TPath.GetFileName(Path), 'errors') then
    exit;

  inherited;
end;

{ TFolderTestSuiteOnlyErrors }

procedure TFolderTestSuiteOnlyErrors.ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path,
  FileMask: string; Recursive: Boolean);
begin
  if SameText(TPath.GetFileName(Path), 'errors') then
  begin
    inherited;
    exit;
  end;

  if Recursive then
    for var Folder in TDirectory.GetDirectories(Path) do
      ProcessFolder(Suite, TestClass, NameOfMethod, Folder, FileMask, true);
end;

{ TTestFileReaderErrors }

procedure TTestFileReaderErrors.RunTest(TestResult: TTestResult);
begin
  try

    inherited;

    Fail('Passed without expected error: '+TPath.GetFileNameWithoutExtension(TestFileName));

  except
    on E: Exception do
    begin
      var Msg := E.Message.ToLower;
      Msg := StringReplace(Msg, '/', '-', [rfReplaceAll]);
      Msg := StringReplace(Msg, '"', '', [rfReplaceAll]);
      if (Msg.Contains(TPath.GetFileNameWithoutExtension(TestFileName).ToLower)) then
        Check(True)
      else
        raise;
    end;
  end;
end;

initialization
  var TestSuite: TTestSuite := TFolderTestSuiteSkipErrors.Create('Load map files', TTestFileReader, '..\..\..\Data', '*.*', True);
  RegisterTest(TestSuite);
  TestSuite := TFolderTestSuiteOnlyErrors.Create('Reader errors', TTestFileReaderErrors, '..\..\..\Data', '*.*', True);
  RegisterTest(TestSuite);
end.


