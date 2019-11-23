VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   4230
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   6570
   LinkTopic       =   "Form1"
   ScaleHeight     =   4230
   ScaleWidth      =   6570
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
Dim ini As String
Dim temp As String
Dim Linea As String
Dim CodCli As String
Dim CodDest As String
Dim CodArt As String
Dim tempCod As String
Dim Peso As Currency
Dim Tasse As Currency
Dim Piombo As Double
Dim Ritira As Boolean
Dim Destino As Boolean
Dim Colli As Long
Dim DataO As String
Dim Perc As Double
Dim SomPerc As Double
Dim SomPre As Double
Dim Prezzo As Currency
Dim PrezzoTot As Currency
Dim Stringhe(200) As String
Dim Var As Integer
Dim i As Integer
Dim Cod
Var = 1
ini = App.Path & "/Setting.ini"
Open ini For Input As 1
    Input #1, connessione
Close #1
ini = Command
'ini = "C:\rec-prezzi.txt"
SomPerc = 0
tempCod = 0
SomPre = 0
Open ini For Input As 1
        Input #1, Linea
    While Linea <> "XXX"
        CodCli = Trim(Mid(Linea, 2, 5))
        CodDest = Trim(Mid(Linea, 7, 5))
        CodArt = Trim(Mid(Linea, 20, 6))
        temp = Mid(Linea, 12, 8)
        DataO = Left(Right(temp, 4), 2) & "/" & Right(temp, 2) & "/" & Left(temp, 4)
        Peso = Mid(Linea, 32, 8)
        Peso = Peso / 1000
        Colli = Mid(Linea, 48, 8)
        Tasse = Mid(Linea, 57, 8)
        Tasse = Tasse / 100
        Piombo = Mid(Linea, 73, 6)
        Piombo = Piombo / 1000
        If Mid(Linea, 56, 1) = "S" Then
            Ritira = True
        Else
            Ritira = False
        End If
        Perc = Mid(Linea, 79, 5)
        Perc = Perc / 100
        If CodDest = "" Then
            Destino = False
        Else
            Destino = True
        End If
        Dim Tipo As Integer
        Tipo = RecuperaTipo(CodCli)
        If Tipo = 0 Then
            Prezzo = RecuperaPrezzo(CodCli, CodDest, CodArt, DataO, Peso, Colli, Tasse, Piombo, Ritira, Destino)
            Prezzo = Prezzo + 0.005
            If SomPerc < 100 Or CodArt = tempCod Then
                PrezzoTot = Prezzo
                If Perc > 0 Then
                    Prezzo = (Prezzo / 100) * Perc
                    SomPre = SomPre + Prezzo
                    tempCod = CodArt
                    SomPerc = SomPerc + Perc
                End If
            End If
            If SomPerc = 100 Then
                Prezzo = Prezzo + (Int((PrezzoTot - SomPre) * 100) / 100)
                SomPerc = 0
                SomPre = 0
            End If
        Else
            If Tipo = 99 Then
                Prezzo = RecuperaPrezzo3(CodArt)
            Else
                If Tipo = 5 Or Tipo = 6 Then
                    Prezzo = RecuperaPrezzo5(CodArt, Peso, Tasse, Ritira, Tipo)
                Else
                    If Tipo = 7 Then
                        Prezzo = RecuperaPrezzo7(CodArt)
                    Else
                        If Tipo = 14 Then
                            Prezzo = RecuperaPrezzo14(CodArt)
                        Else
                            Prezzo = RecuperaPrezzo2(CodArt)
                        End If
                    End If
                End If
            End If
            If Ritira = True Then
                    If Prezzo <> 0 Then
                        Prezzo = Prezzo - (Peso * 0.08)
                    End If
            End If
            Prezzo = Prezzo + 0.005
        End If
        Prezzo = (Int(Prezzo * 100))
        If Prezzo = 0 Then
            temp = "99999999"
        Else
            temp = Prezzo
            For i = 1 To 8 - Len(temp)
                temp = "0" & temp
            Next
        End If
        If promo = True Then
            Linea = Left(Linea, Len(Linea) - 9) & temp & "S"
        Else
            Linea = Left(Linea, Len(Linea) - 9) & temp & "N"
        End If
        Stringhe(Var) = Linea
        If Not EOF(1) Then
            Input #1, Linea
        Else
            Linea = "XXX"
        End If
        Var = Var + 1
    Wend
    GoTo Finito
Finito:
Stringhe(Var) = Linea
Close #1
Var = 1
Open ini For Output As 1
While Stringhe(Var) <> "XXX"
    Print #1, Stringhe(Var)
    Var = Var + 1
Wend
Close #1
Unload Me
End Sub
