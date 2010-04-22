object DataModel: TDataModel
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 392
  Top = 377
  Height = 171
  Width = 266
  object coner: TUniConnection
    ProviderName = 'Access'
    Database = 'demo.mdb'
    Left = 58
    Top = 24
  end
  object Gqry: TUniQuery
    Connection = coner
    Left = 99
    Top = 24
  end
  object DP: TDataSetProvider
    DataSet = Gqry
    Left = 140
    Top = 23
  end
  object AccessUniProvider1: TAccessUniProvider
    Left = 34
    Top = 189
  end
  object MySQLUniProvider1: TMySQLUniProvider
    Left = 99
    Top = 190
  end
  object OracleUniProvider1: TOracleUniProvider
    Left = 168
    Top = 198
  end
  object SQLiteUniProvider1: TSQLiteUniProvider
    Left = 292
    Top = 191
  end
  object SQLServerUniProvider1: TSQLServerUniProvider
    Left = 253
    Top = 197
  end
  object UniSQL1: TUniSQL
    Connection = coner
    Left = 59
    Top = 73
  end
end
