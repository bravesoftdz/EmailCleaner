object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Email Cleaner'
  ClientHeight = 241
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 29
    Height = 13
    Caption = 'Emails'
  end
  object Label2: TLabel
    Left = 19
    Top = 55
    Width = 26
    Height = 13
    Caption = 'Rules'
  end
  object LabelFinishTime: TLabel
    Left = 16
    Top = 200
    Width = 3
    Height = 13
  end
  object LabelStartTime: TLabel
    Left = 16
    Top = 126
    Width = 3
    Height = 13
  end
  object LabelResult: TLabel
    Left = 16
    Top = 168
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 132
    Top = 24
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 132
    Top = 55
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 16
    Top = 80
    Width = 29
    Height = 26
    Caption = 'Global rules'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 136
    Top = 88
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 51
    Top = 19
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = ButtonEmailsClick
  end
  object Button3: TButton
    Left = 8
    Top = 121
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 1
    OnClick = ApplyClick
  end
  object Memo1: TMemo
    Left = 344
    Top = 50
    Width = 265
    Height = 163
    TabOrder = 2
  end
  object Button2: TButton
    Left = 51
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 3
    OnClick = ButtonReplacementClick
  end
  object Button4: TButton
    Left = 534
    Top = 19
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = SaveClick
  end
  object Button5: TButton
    Left = 51
    Top = 81
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 5
    OnClick = Button5Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 232
    Width = 641
    Height = 8
    TabOrder = 6
  end
end
