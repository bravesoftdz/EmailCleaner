unit ContentProvider;

interface

uses
  // System.StrUtils.SplitRect,
  System.sysutils,
  System.Generics.Collections,
  System.IOUtils,
  System.Classes,
  System.RegularExpressions,
  Mapper,
  Vcl.ComCtrls;

type
  TOnProgressEvent = procedure(Sender: TObject; Max, Position: Integer)
    of object;

  TContentProvider = class(TObject)
  private
    FProgressBar: TProgressBar;
    FOnProgress: TOnProgressEvent;
  protected
    procedure DoOnProgress(Sender: TObject; Max, Position: Integer); virtual;

  public
    function LoadLines(const FilePath: string): TStringList;
    function LoadCSV(const FilePath: string; const Separator: char)
      : TDictionary<string, string>;
    function Stringify(const Dict: TDictionary<string, string>;
      Separator1, Separator2: string): string; overload;
    function Stringify(const Source: TStringList; const Separator: string)
      : string; overload;
    function Replace(const Source: TStringList;
      const Replacement: TDictionary<string, string>): TStringList;
    function ReplaceRegExp(const Source: TStringList;
      const Replacement: TDictionary<string, string>): string;
    procedure SetProgressBar(ProgressBar: TProgressBar);
    function dropDuplicates(const list: TStringList): TStringList;

    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;

    // property FProgressBar: TProgressBar read GetProgressBar write SetProgressBar;
  end;

implementation

uses
  System.Types;

{ TContentProvider }

{
  Creates a dictionary from given file.
  Each line of the file corresponds to a key-value dictionary entry, the
  splitting of the line occurs by the separator.
  @param FilePath full path to the file
  @param Separator  a character over which the splitting occurs
  @uses TMapper
}

procedure TContentProvider.DoOnProgress(Sender: TObject;
  Max, Position: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Max, Position);
end;

function TContentProvider.LoadCSV(const FilePath: string; const Separator: char)
  : TDictionary<string, string>;
var
  allLines: TStringList;
  Output: TDictionary<string, string>;
  line, Key, Value: string;
  strArray: TArray<string>;
  Mapper: TMapper;
begin
  Mapper := TMapper.Create;
  allLines := LoadLines(FilePath);
  for line in allLines do
  begin
    strArray := line.Split([Separator]);
    if Length(strArray) = 2 then
      Mapper.insert(Trim(strArray[0]), Trim(strArray[1]));
  end;
  Result := Mapper.getContent;
end;

function TContentProvider.LoadLines(const FilePath: string): TStringList;
var
  Lines: TStringDynArray;
  line: string;
begin
  Lines := System.IOUtils.TFile.ReadAllLines(FilePath);
  for line in Lines do
  begin
    Result.Add(line);
  end;
end;


function TContentProvider.dropDuplicates(const list: TStringList)
  : TStringList;
var
  listTmp: TStringList;
  word: string;
begin
  Result := TStringList.Create;
  for word in list do
  begin
    if Result.IndexOf(word) = -1 then
    begin
      Result.Add(word);
    end;
  end;
end;

function TContentProvider.Replace(const Source: TStringList;
  const Replacement: TDictionary<string, string>): TStringList;
var
  Item, Value: string;
  ShowProgress: boolean;
  SourceSize, Counter: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := TRUE;
  ShowProgress := not(FProgressBar = nil);
  SourceSize := Source.Count;
  Counter := 0;
  DoOnProgress(Self, SourceSize, 0);
  for Item in Source do
  begin
    Counter := Counter + 1;
    DoOnProgress(Self, SourceSize, Counter);
    if Replacement.ContainsKey(Item) then
    begin
      Value := Replacement[Item];
    end
    else
      Value := Item;
    Result.Add(Value);
  end;
end;

function TContentProvider.ReplaceRegExp(const Source: TStringList;
  const Replacement: TDictionary<string, string>): string;
var
  Value, Text: string;
  Item: TPair<string, string>;
begin
  Result := Source.Text;
  for Item in Replacement do
  begin
    Result := Result.Replace(Item.Key, Item.Value, [rfReplaceAll]);
  end;
  Result := Result.Trim;
end;

procedure TContentProvider.SetProgressBar(ProgressBar: TProgressBar);
begin
  FProgressBar := ProgressBar;
end;

function TContentProvider.Stringify(const Source: TStringList;
  const Separator: string): string;
begin
  if Source.Count = 0 then
    Result := ''
  else
    Result := string.Join(Separator, Source.ToStringArray);
end;

function TContentProvider.Stringify(const Dict: TDictionary<string, string>;
  Separator1, Separator2: string): string;
var
  Item: TPair<string, string>;
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  for Item in Dict do
    StringBuilder.Append(Item.Key + Separator1 + Item.Value + Separator2);
  Result := StringBuilder.ToString;
  StringBuilder.Clear;
end;

end.
