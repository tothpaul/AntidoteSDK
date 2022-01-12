program AntidoteDemo;

uses
  Vcl.Forms,
  AntidoteDemo.Main in 'AntidoteDemo.Main.pas' {Form1},
  Execute.Antidote in 'Execute.Antidote.pas',
  Execute.AntidoteAPI in 'Execute.AntidoteAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
