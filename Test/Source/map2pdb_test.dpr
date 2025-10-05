program map2pdb_test;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  DUnitTestRunner,
  SysUtils,
  Forms,
  debug.info.log,
  TestFileReader in 'TestFileReader.pas',
  FileTestFramework in '..\DUnit\FileTestFramework.pas',
  TestFileWriter in 'TestFileWriter.pas';

{$R *.RES}

type
  TDebugInfoUnitTestLogger = class(TInterfacedObject, IDebugInfoLogger)
  protected
    // IDebugInfoLogger
    procedure Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
  end;

procedure TDebugInfoUnitTestLogger.Log(Category: TDebugInfoLogCategory; LogModule: TDebugInfoLogModule; const Msg: string);
begin
  if (Category = lcFatal) then
    raise Exception.Create(Msg);
end;

begin
  RegisterDebugInfoLogger(TDebugInfoUnitTestLogger.Create);

  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


