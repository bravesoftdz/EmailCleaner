unit FileSelector;

interface

uses
  System.Classes;

type
  TFileSelector = class
  private
    FilePath: string;
    Component: TComponent;
  public
    function getFilePath: string;
    constructor Create(Component: TComponent);
    procedure save(const Content: string);
    procedure Load;
  end;

implementation

uses
  Vcl.Dialogs;

{ TFileSelector }

constructor TFileSelector.Create(Component: TComponent);
var
  OpenDialog: TOpenDialog;
begin
  Self.Component := Component;
end;

function TFileSelector.getFilePath: string;
begin
  Result := FilePath;
end;

procedure TFileSelector.Load;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Component);
  OpenDialog.Options := [ofFileMustExist];
  if OpenDialog.Execute then
    FilePath := OpenDialog.FileName;
  OpenDialog.Free;
end;

procedure TFileSelector.save(const Content: string);
var
  saveDialog: TSaveDialog;
  Data: TStringList;
begin
  saveDialog := TSaveDialog.Create(Component);

  // Give the dialog a title
  saveDialog.Title := 'Save your text or word file';
  // Allow only .txt and .doc file types to be saved
  // saveDialog.Filter := 'Text file|*.txt|Word file|*.doc';

  // Set the default extension
  // saveDialog.DefaultExt := 'txt';

  // Select text files as the starting filter type
  // saveDialog.FilterIndex := 1;

  // Display the open file dialog
  if not(saveDialog.Execute) then
  begin
    ShowMessage('Save file was cancelled');
    Exit;
  end;
  ShowMessage('File : ' + saveDialog.FileName);
  Data := TStringList.Create;
  Data.Add(Content);
  Data.SaveToFile(saveDialog.FileName);
  // Free up the dialog
  saveDialog.Free;

end;

end.
