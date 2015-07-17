unit ContentProviderTests;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  System.Types,
  System.sysutils;

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
    [Test]
    procedure StringifySingleElem;
    [Test]
    procedure StringifyThreeElem;
    [Test]
    procedure DropDuplicatesEmpty;
    [Test]
    procedure DropDuplicatesSingle;
    [Test]
    procedure DropDuplicatesTwoEquals;
    [Test]
    procedure DropDuplicatesTwoDifferent;
    [Test]
    procedure DropDuplicatesThree;
    // Test with TestCase Atribute to supply parameters.
    [Test]
    [TestCase('TestA', '1, 2')]
    [TestCase('TestB', '3, 4')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

uses
  ContentProvider;

procedure TEmailCleanerTests.DropDuplicatesEmpty;
var
  list, input: TStringList;
  CP: TContentProvider;
begin
  CP := TContentProvider.Create;
  list := TStringList.Create();
  Assert.AreEqual(CP.dropDuplicates(list).Count, 0);
end;

procedure TEmailCleanerTests.DropDuplicatesSingle;
var
  list, input, output: TStringList;
  CP: TContentProvider;
begin
  CP := TContentProvider.Create;
  list := TStringList.Create();
  list.Add('a');
  output := CP.dropDuplicates(list);
  Assert.AreEqual(output.Count, 1);
  Assert.AreEqual(output[0], 'a');
end;

procedure TEmailCleanerTests.DropDuplicatesTwoEquals;
var
  list, input, output: TStringList;
  CP: TContentProvider;
begin
  CP := TContentProvider.Create;
  list := TStringList.Create();
  list.Add('a');
  list.Add('a');
  output := CP.dropDuplicates(list);
  Assert.AreEqual(output.Count, 1);
  Assert.AreEqual(output[0], 'a');
end;

procedure TEmailCleanerTests.DropDuplicatesTwoDifferent;
var
  list, input, output: TStringList;
  CP: TContentProvider;
begin
  CP := TContentProvider.Create;
  list := TStringList.Create();
  list.Add('a');
  list.Add('b');
  output := CP.dropDuplicates(list);
  Assert.AreEqual(output.Count, 2);
  Assert.isFalse(output.IndexOf('a') = -1, 'Element "a" must be present');
  Assert.isFalse(output.IndexOf('b') = -1, 'Element "b" must be present');
end;

procedure TEmailCleanerTests.DropDuplicatesThree;
var
  list, input, output: TStringList;
  CP: TContentProvider;
begin
  CP := TContentProvider.Create;
  list := TStringList.Create();
  list.Add('a');
  list.Add('b');
  list.Add('a');
  output := CP.dropDuplicates(list);
  Assert.AreEqual(output.Count, 2);
  Assert.isFalse(output.IndexOf('a') = -1, 'Element "a" must be present');
  Assert.isFalse(output.IndexOf('b') = -1, 'Element "b" must be present');
end;

procedure TEmailCleanerTests.Setup;
begin
end;

procedure TEmailCleanerTests.TearDown;
begin
end;

procedure TEmailCleanerTests.StringifyEmpty;
var
  output: string;
  list: TStringList;
  CP: TContentProvider;
begin
  list := TStringList.Create;
  output := CP.Stringify(list, 'aaa');
  Assert.AreEqual(output, '');
end;

procedure TEmailCleanerTests.StringifySingleElem;
var
  output: string;
  list: TStringList;
  CP: TContentProvider;
begin
  list := TStringList.Create;
  list.Add('first elem');
  output := CP.Stringify(list, 'separ');
  Assert.AreEqual(output, 'first elem');
end;

procedure TEmailCleanerTests.StringifyThreeElem;
var
  output: string;
  list: TStringList;
  CP: TContentProvider;
begin
  list := TStringList.Create;
  list.Add('first elem');
  list.Add('second elem');
  list.Add('third elem');
  output := CP.Stringify(list, ' ');
  Assert.AreEqual(output, 'first elem second elem third elem');
end;

procedure TEmailCleanerTests.Test2(const AValue1: Integer;
  const AValue2: Integer);
begin
  // Assert.;
end;

initialization

TDUnitX.RegisterTestFixture(TEmailCleanerTests);

end.
