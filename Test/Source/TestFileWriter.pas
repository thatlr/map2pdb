unit TestFileWriter;

interface

uses
  Classes, Types,
  FileTestFramework,
  debug.info,
  TestFileReader;

type
  TTestFileWriter = class(TCustomMapTest)
  protected
    procedure ProcessDebugInfo(DebugInfo: TDebugInfo); override;
  published
    procedure TestWritePDB;
  end;

implementation

uses
  Windows,
  SysUtils,
  IOUtils,
  TestFramework,
  debug.info.writer.pdb;


{ TTestFileWriter }

procedure TTestFileWriter.ProcessDebugInfo(DebugInfo: TDebugInfo);
begin
  var BlockSize: Integer := 0;

  var TargetFilename := TPath.ChangeExtension(TestFileName, '.pdb');
(*
  var TargetPath := TPath.GetDirectoryName(TestFileName);
  TargetPath := TPath.Combine(TargetPath, '..\..\Output');

  var TargetFilename := TPath.ChangeExtension(TPath.GetFileName(TestFileName), '.pdb');
  TargetFilename := TPath.Combine(TargetPath, TestFileName);
*)
  try

    var Writer := TDebugInfoPdbWriter.Create(BlockSize);
    try

      Writer.SaveToFile(TargetFilename, DebugInfo);

    finally
      Writer.Free;
    end;

  finally
    TFile.Delete(TargetFilename);
  end;

  Check(True);
end;

procedure TTestFileWriter.TestWritePDB;
begin
  DoLoadFromFile;
end;

initialization
  var TestSuite := TFolderTestSuite.Create('Write PDB files', TTestFileWriter, '..\..\..\Data', '*.*', True);
  RegisterTest(TestSuite);
end.


