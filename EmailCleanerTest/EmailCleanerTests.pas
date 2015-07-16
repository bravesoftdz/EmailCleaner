unit EmailCleanerTests;

interface

uses
  DUnitX.TestFramework,
  System.Classes;

type

  [TestFixture]
  TEmailCleanerTests = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure StringifyEmpty;
    procedure StringifySingleElem;
    procedure StringifyThreeElem;
    // Test with TestCase Atribute to supply parameters.
    [Test]
    [TestCase('TestA', '1, 2')]
    [TestCase('TestB', '3, 4')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

uses
  ContentProvider;

procedure TEmailCleanerTests.Setup;
begin
end;

procedure TEmailCleanerTests.TearDown;
begin
end;

procedure TEmailCleanerTests.StringifyEmpty;
var
  Output: string;
  List: TStringList;
  CP: TContentProvider;
begin
  List := TStringList.Create;
  Output := CP.Stringify(List, 'aaa');
  Assert.AreEqual(Output, '');
end;

procedure TEmailCleanerTests.StringifySingleElem;
var
  Output: string;
  List: TStringList;
  CP: TContentProvider;
begin
  List := TStringList.Create;
  List.Add('first elem');
  Output := CP.Stringify(List, 'separ');
  Assert.AreEqual(Output, 'first elem');
end;

procedure TEmailCleanerTests.StringifyThreeElem;
var
  Output: string;
  List: TStringList;
  CP: TContentProvider;
begin
  List := TStringList.Create;
  List.Add('first elem');
  List.Add('second elem');
  List.Add('third elem');
  Output := CP.Stringify(List, ' ');
  Assert.AreEqual(Output, 'first elem second elem third elem');
end;

procedure TEmailCleanerTests.Test2(const AValue1: Integer;
  const AValue2: Integer);
begin
  // Assert.;
end;

initialization

TDUnitX.RegisterTestFixture(TEmailCleanerTests);

end.
