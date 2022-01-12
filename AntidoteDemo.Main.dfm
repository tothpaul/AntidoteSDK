object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 419
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 327
    Width = 685
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 330
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 97
    Width = 685
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 89
    ExplicitWidth = 238
  end
  object RichEdit1: TRichEdit
    Tag = 1
    Left = 0
    Top = 0
    Width = 685
    Height = 97
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    TabOrder = 0
    Zoom = 100
    OnEnter = RichEdit1Enter
  end
  object RichEdit2: TRichEdit
    Tag = 3
    Left = 0
    Top = 100
    Width = 685
    Height = 227
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit2')
    ParentFont = False
    TabOrder = 1
    Zoom = 100
    OnEnter = RichEdit1Enter
  end
  object RichEdit3: TRichEdit
    Tag = 3
    Left = 0
    Top = 330
    Width = 685
    Height = 89
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit3')
    ParentFont = False
    TabOrder = 2
    Zoom = 100
    OnEnter = RichEdit1Enter
  end
  object MainMenu1: TMainMenu
    Left = 233
    Top = 216
    object Antidote1: TMenuItem
      Caption = 'Outils'
      object Correcteur1: TMenuItem
        Caption = 'Correcteur'
        OnClick = Correcteur1Click
      end
      object Dictionnaires1: TMenuItem
        Caption = 'Dictionnaires'
        object Dfinitions1: TMenuItem
          Caption = 'D'#233'finitions'
          OnClick = DictionnaireClick
        end
        object Synonymes1: TMenuItem
          Tag = 1
          Caption = 'Synonymes'
          OnClick = DictionnaireClick
        end
        object Antonymes1: TMenuItem
          Tag = 2
          Caption = 'Antonymes'
          OnClick = DictionnaireClick
        end
        object Coocurrences1: TMenuItem
          Tag = 3
          Caption = 'Coocurrences'
          OnClick = DictionnaireClick
        end
        object ChampLexical1: TMenuItem
          Tag = 4
          Caption = 'Champ Lexical'
          OnClick = DictionnaireClick
        end
        object Conjugaison1: TMenuItem
          Tag = 5
          Caption = 'Conjugaison'
          OnClick = DictionnaireClick
        end
        object Famille1: TMenuItem
          Tag = 6
          Caption = 'Famille'
          OnClick = DictionnaireClick
        end
        object Citations1: TMenuItem
          Tag = 7
          Caption = 'Citations'
          OnClick = DictionnaireClick
        end
        object Historique1: TMenuItem
          Tag = 8
          Caption = 'Historique'
          OnClick = DictionnaireClick
        end
        object Illustrations1: TMenuItem
          Tag = 9
          Caption = 'Illustrations'
          OnClick = DictionnaireClick
        end
        object Wikipedia1: TMenuItem
          Tag = 10
          Caption = 'Wikip'#233'dia'
          OnClick = DictionnaireClick
        end
      end
      object Guides1: TMenuItem
        Caption = 'Guides'
        object Orthographe1: TMenuItem
          Caption = 'Orthographe'
          OnClick = OnGuide
        end
        object Lexique1: TMenuItem
          Tag = 1
          Caption = 'Lexique'
          OnClick = OnGuide
        end
        object Grammaire1: TMenuItem
          Tag = 2
          Caption = 'Grammaire'
          OnClick = OnGuide
        end
        object Syntaxe1: TMenuItem
          Tag = 3
          Caption = 'Syntaxe'
          OnClick = OnGuide
        end
        object Ponctuation1: TMenuItem
          Tag = 4
          Caption = 'Ponctuation'
          OnClick = OnGuide
        end
        object Style1: TMenuItem
          Tag = 5
          Caption = 'Style'
          OnClick = OnGuide
        end
        object Rdaction1: TMenuItem
          Tag = 6
          Caption = 'R'#233'daction'
          OnClick = OnGuide
        end
        object ypographie1: TMenuItem
          Tag = 7
          Caption = 'Typographie'
          OnClick = OnGuide
        end
        object Phontique1: TMenuItem
          Tag = 8
          Caption = 'Phon'#233'tique'
          OnClick = OnGuide
        end
        object Historique2: TMenuItem
          Tag = 9
          Caption = 'Historique'
          OnClick = OnGuide
        end
        object PointsdeLangue1: TMenuItem
          Tag = 10
          Caption = 'Points de Langue'
          OnClick = OnGuide
        end
      end
    end
    object Langue1: TMenuItem
      Caption = 'Langue'
      object Selonlecontexte1: TMenuItem
        AutoCheck = True
        Caption = 'Selon le contexte'
        Checked = True
        RadioItem = True
      end
      object Forcerlanglais1: TMenuItem
        AutoCheck = True
        Caption = 'Forcer l'#39'anglais'
        RadioItem = True
      end
      object Forcerlefranais1: TMenuItem
        AutoCheck = True
        Caption = 'Forcer le fran'#231'ais'
        RadioItem = True
      end
    end
  end
end
