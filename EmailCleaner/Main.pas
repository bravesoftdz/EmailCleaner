unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  FileDialog,
  System.Types,
  Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button3: TButton;
    Memo1: TMemo;
    LabelFinishTime: TLabel;
    LabelStartTime: TLabel;
    LabelResult: TLabel;
    Button2: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Button4: TButton;
    Label5: TLabel;
    Button5: TButton;
    Label6: TLabel;
    ProgressBar1: TProgressBar;
    procedure ApplyClick(Sender: TObject);
    procedure ButtonReplacementClick(Sender: TObject);
    procedure ButtonEmailsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoOnProgress(Sender: TObject; Max, Position: Integer);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  ReplacementPath, SourceFilePath, GlobalReplacementFilePath: string;
  Content: TStringList;

implementation

uses
  system.threading,
  ContentProvider,
  System.Generics.Collections;

{$R *.dfm}

procedure TMainForm.ApplyClick(Sender: TObject);

begin

  TTask.Create(
    procedure
    var
      ContentProvider: TContentProvider;
      Replacement, GlobalReplacement: TDictionary<string, string>;
      Source, SourceRefactored: TStringList;
      Text: string;
    begin
       TThread.NameThreadForDebugging('Nuovo');
//      ReplacementPath := 'c:\Users\User\Documents\replacements.txt';
//      GlobalReplacementFilePath := 'c:\Users\User\Documents\regexp.txt';
//      SourceFilePath := 'c:\Users\User\Documents\emails_big.txt';
      ReplacementPath := '.\replacements.txt';
      GlobalReplacementFilePath := '.\regexp.txt';
      SourceFilePath := '.\emails_big.txt';
      if SourceFilePath.IsEmpty then
        Exit;
      ContentProvider := TContentProvider.Create;
      // ContentProvider.OnProgress := DoOnProgress;
      ContentProvider.SetProgressBar(ProgressBar1);
      Source := ContentProvider.LoadLines(SourceFilePath);
      Replacement := ContentProvider.LoadCSV(ReplacementPath, ',');
      GlobalReplacement := ContentProvider.LoadCSV
        (GlobalReplacementFilePath, ',');
      if not(Replacement = nil) then
        Content := ContentProvider.Replace(Source, Replacement)
      else
        Content := Source;
      if not(GlobalReplacement = nil) then
        Text := ContentProvider.ReplaceRegExp(Content, GlobalReplacement);
      //updating form
      TThread.Synchronize(nil,
        procedure
        begin
          Memo1.Text := Text;
        end);

    end).Start;
end;

procedure TMainForm.SaveClick(Sender: TObject);
var
  Data: string;
  ContentProvider: TContentProvider;
  FileDialog: TFileDialog;
begin
  ContentProvider := TContentProvider.Create;
  Data := ContentProvider.Stringify(Content, sLineBreak);
  FileDialog := TFileDialog.Create(self);
  FileDialog.save(Data);
end;

procedure TMainForm.Button5Click(Sender: TObject);
var
  FileDialog: TFileDialog;
begin
  FileDialog := TFileDialog.Create(self);
  FileDialog.Load();
  GlobalReplacementFilePath := FileDialog.getFilePath;
  Label6.Caption := GlobalReplacementFilePath;
end;

procedure TMainForm.ButtonEmailsClick(Sender: TObject);
var
  FileDialog: TFileDialog;
begin
  FileDialog := TFileDialog.Create(self);
  FileDialog.Load();
  SourceFilePath := FileDialog.getFilePath;
  Label3.Caption := SourceFilePath;
end;

procedure TMainForm.ButtonReplacementClick(Sender: TObject);
var
  FileDialog: TFileDialog;
begin
  FileDialog := TFileDialog.Create(self);
  FileDialog.Load();
  ReplacementPath := FileDialog.getFilePath;
  Label4.Caption := ReplacementPath;
end;

procedure TMainForm.DoOnProgress(Sender: TObject; Max, Position: Integer);
begin
  Application.ProcessMessages;
  ProgressBar1.Max := Max;
  ProgressBar1.Position := Position;
  ProgressBar1.Update;
end;

end.
