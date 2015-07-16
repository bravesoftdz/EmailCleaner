unit EmailCleanerTestCase;

interface

uses
  TestFramework,
  ContentProvider;

type
  // Test methods for class TCalc
  TEmailCleanerTestCase = class(TTestCase)
  strict private
    FContentProvider: TContentProvider;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStringify;
  end;

implementation

uses
  System.Generics.Collections,
  System.Classes;

procedure TEmailCleanerTestCase.SetUp;
begin
  FContentProvider := TContentProvider.Create;
end;

procedure TEmailCleanerTestCase.TearDown;
begin
  FContentProvider := nil;
end;

procedure TEmailCleanerTestCase.TestStringify;
var
  Output: string;
  List: TStringList;
begin
  List := TStringList.Create;
  Output := FContentProvider.Stringify(List, 'aaa');
  Check(Output = 'bbb');
end;

initialization

// Register any test cases with the test runner
RegisterTest(TEmailCleanerTestCase.Suite);

end.
