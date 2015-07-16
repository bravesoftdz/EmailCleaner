program EmailCleaner;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  ContentProvider in 'ContentProvider.pas',
  Mapper in 'Mapper.pas',
  FileDialog in 'FileDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
