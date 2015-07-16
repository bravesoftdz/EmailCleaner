unit Mapper;

interface

uses
  System.Generics.Defaults,

  System.Generics.Collections,
  System.SysUtils,
  System.rtti;

type
  TMapper = class
  private
    KeysToValues: TDictionary<string, string>;
    procedure AddIfNonTrivial(const Key: string; const Value: string);
  public
    constructor Create;
    procedure insert(const Key: string; const Value: string);
    function FindByValue(const Value: string): TArray<string>;
    function getContent: TDictionary<string, string>;
  end;

implementation

procedure TMapper.insert(const Key: string; const Value: string);
var
  OldKey: string;
  OldKeys: TArray<string>;
begin
  if KeysToValues.ContainsKey(Key) then
  begin
    Self.AddIfNonTrivial(Key, Value);
    Exit;
  end;
  OldKeys := Self.FindByValue(Value);
  for OldKey in OldKeys do
  begin
    if true then
      KeysToValues.Remove(OldKey)
    else
      KeysToValues.AddOrSetValue(OldKey, Value);
    KeysToValues.AddOrSetValue(Key, Value);
  end;
  if KeysToValues.ContainsKey(Value) then
    AddIfNonTrivial(Key, KeysToValues[Value])
  else
    AddIfNonTrivial(Key, Value);
end;

constructor TMapper.Create;
begin
  KeysToValues := TDictionary<string, string>.Create();
end;

function TMapper.FindByValue(const Value: string): TArray<string>;
var
  Item: TPair<string, string>;
begin
  for Item in KeysToValues do
  begin
    if Item.Value = Value then
    begin
      Result := Result + [Item.Key];
    end;
  end;
end;

function TMapper.getContent: TDictionary<string, string>;
begin
  Result := KeysToValues;
end;

procedure TMapper.AddIfNonTrivial(const Key: string; const Value: string);
var AreEqual: boolean;
begin
  AreEqual := Key = Value;
  if not(AreEqual) then
    KeysToValues.AddOrSetValue(Key, Value);
end;

end.
