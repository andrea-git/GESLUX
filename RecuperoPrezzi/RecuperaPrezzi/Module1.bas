Attribute VB_Name = "Module1"
Dim Conn As ADODB.Connection
Dim Conn2 As ADODB.Connection
Dim Rs As ADODB.Recordset
Dim RsSoglie As ADODB.Recordset
Dim query As String
Public promo As Boolean
Dim Categoria As Integer
Dim Ragione2 As String
Dim Prezzo As Currency
Public connessione As String

Public Function RecuperaTipo(CodCli As String) As Integer
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT idcategoria,RagioneSoc2 FROM tbragioni WHERE CodRag='" & CodCli & "';"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    Categoria = Rs!idcategoria
    Ragione2 = ""
    If Rs!RagioneSoc2 <> "" Then
        Ragione2 = Rs!RagioneSoc2
    End If
    Conn.Close
    If Ragione2 = "MIDAS" Then
        RecuperaTipo = 99
    Else
        If Categoria = 13 Then
            RecuperaTipo = 13
        Else
            If Categoria = 7 Then
                RecuperaTipo = 7
            Else
                If Categoria = 14 Then
                    RecuperaTipo = 14
                Else
                    If Categoria = 5 Then
                        RecuperaTipo = 5
                    Else
                        If Categoria = 6 Then
                            RecuperaTipo = 6
                        Else
                            RecuperaTipo = 0
                        End If
                    End If
                End If
            End If
        End If
    End If
End Function

Public Function RecuperaPrezzo5(CodArt As String, Peso As Currency, Tasse As Currency, Ritira As Boolean, Tipo As Integer) As Currency
    Dim temp As Date
    temp = Date
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    If Tipo = 5 Then
        query = "SELECT Prezzo FROM tbListiniESTERO WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    Else
        query = "SELECT Prezzo FROM tbListiniAgentiESTERO WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    End If
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    If Rs.EOF = False Then
        Prezzo = Rs!Prezzo
    Else
        Prezzo = 0
    End If
    Conn.Close
    'Prezzo = Prezzo - Tasse
    RecuperaPrezzo5 = Prezzo
End Function
Public Function RecuperaPrezzo14(CodArt As String) As Currency
    Dim temp As Date
    temp = Date
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT Prezzo FROM tbListiniTexaco WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    If Rs.EOF = False Then
        Prezzo = Rs!Prezzo
    Else
        Prezzo = 0
    End If
    Conn.Close
    RecuperaPrezzo14 = Prezzo
End Function

Public Function RecuperaPrezzo7(CodArt As String) As Currency
    Dim temp As Date
    temp = Date
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT Prezzo FROM tbListiniSpecialist WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    If Rs.EOF = False Then
        Prezzo = Rs!Prezzo
    Else
        Prezzo = 0
    End If
    Conn.Close
    RecuperaPrezzo7 = Prezzo
End Function

Public Function RecuperaPrezzo2(CodArt As String) As Currency
    Dim temp As Date
    temp = Date
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT Prezzo FROM tbListiniBP WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    If Rs.EOF = False Then
        Prezzo = Rs!Prezzo
    Else
        Prezzo = 0
    End If
    Conn.Close
    RecuperaPrezzo2 = Prezzo
End Function

Public Function RecuperaPrezzo3(CodArt As String) As Currency
    Dim temp As Date
    temp = Date
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT Prezzo FROM tbListiniMidas WHERE CodArt='" & CodArt & "' AND DataInizio<=#" & temp & "# AND DataFine>=#" & temp & "#;"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    If Rs.EOF = False Then
        Prezzo = Rs!Prezzo
    Else
        Prezzo = 0
    End If
    Conn.Close
    RecuperaPrezzo3 = Prezzo
End Function

Public Function RecuperaPrezzo(CodCli As String, CodDest As String, CodArt As String, DataO As String, Peso As Currency, Colli As Long, Tasse As Currency, Piombo As Double, Ritira As Boolean, Destino As Boolean) As Currency
    Set Conn = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Conn.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    query = "SELECT idcategoria FROM tbragioni WHERE CodRag='" & CodCli & "';"
    Rs.Open query, Conn, adOpenKeyset, adLockOptimistic, adCmdText
    Categoria = Rs!idcategoria
    Conn.Close
    promo = False
    If Categoria = 7 Then
        Colli = 11
        Destino = True
    End If
    If Categoria = 5 Or Categoria = 6 Then
                Prezzo = CercaPrezzo(CodArt, CodCli, CodDest, DataO, True, Colli, Categoria)
    Else
                If Categoria = 1 Then
                    Prezzo = CercaPrezzo(CodArt, CodCli, CodDest, DataO, False, Colli, Categoria)
                Else
                    Prezzo = CercaPrezzo(CodArt, CodCli, CodDest, DataO, Destino, Colli, Categoria)
                End If
    End If
    If Categoria = 5 Or Categoria = 6 Then
            Prezzo = Prezzo - Tasse
            If Categoria = 6 Then
                Prezzo = Prezzo / 0.95
            End If
    End If
    If Ritira = True Then
            If Categoria = 5 Or Categoria = 6 Then
                If Prezzo <> 0 Then
                    Prezzo = Prezzo - (Peso * 0.15)
                End If
            Else
                If Prezzo <> 0 Then
                    Prezzo = Prezzo - (Peso * 0.08)
                End If
            End If
    End If
    Prezzo = (Int((Prezzo + 0.005) * 100)) / 100
    'Prezzo = Prezzo * (1 + (Piombo / 100))
    RecuperaPrezzo = Prezzo
End Function
    
Function CercaPrezzo(ByVal CodProd As String, ByVal CodRag As String, ByVal CodDest As String, ByVal DataOrd As String, ByVal FrDest As Boolean, ByVal Quant As Long, ByVal Categoria As Integer) As Currency
    Dim Soglie(1 To 5) As Boolean
    Dim Conta As Byte
    Dim DataL As Date
    Dim DataLi As String
    Dim QuantSoglia As Long
    Dim Prz As Currency
    Set RsSoglie = New ADODB.Recordset
    Set Conn2 = New ADODB.Connection
    Set Rs = New ADODB.Recordset
    Prz = 0
    QuantSoglia = 0
    Conn2.Open "Provider=Microsoft.Jet.OLEDB.4.0.;Data Source=" & connessione
    If Categoria <> 7 Then
        query = "SELECT FrancoRiv FROM tbDestini WHERE CodCliente ='" & CodRag & "' AND CodDest ='" & CodDest & "';"
        RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
        If RsSoglie.EOF = False Then
            RsSoglie.MoveFirst
            If RsSoglie!FrancoRiv = True Then
                FrDest = False
            End If
        End If
        RsSoglie.Close
    End If
    query = "SELECT * FROM tbRagioni WHERE CodRag ='" & CodRag & "';"
    RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
    RsSoglie.MoveFirst
    If FrDest Then
        Soglie(1) = RsSoglie!Destino1
        Soglie(2) = RsSoglie!Destino2
        Soglie(3) = RsSoglie!Destino3
        Soglie(4) = RsSoglie!Destino4
        Soglie(5) = RsSoglie!Destino5
    Else
        Soglie(1) = RsSoglie!Fascia1
        Soglie(2) = RsSoglie!Fascia2
        Soglie(3) = RsSoglie!Fascia3
        Soglie(4) = RsSoglie!Fascia4
        Soglie(5) = RsSoglie!Fascia5
    End If
    RsSoglie.Close
    query = "SELECT DISTINCT tbDetListini.Inizio FROM tbDetListini WHERE tbDetListini.Inizio<=#" & DataOrd & "# GROUP BY Inizio;"
    RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
    If RsSoglie.EOF = False Then
        RsSoglie.MoveFirst
        DataL = RsSoglie!Inizio
    End If
    While RsSoglie.EOF = False
        If RsSoglie!Inizio > DataL Then
            DataL = RsSoglie!Inizio
        End If
        RsSoglie.MoveNext
    Wend
    RsSoglie.Close
    DataLi = Month(DataL) & "/" & Day(DataL) & "/" & Year(DataL)
    query = "SELECT DISTINCT Soglia FROM tbArticoli INNER JOIN tbDetListini ON tbArticoli.CodArt = tbDetListini.CodArt WHERE tbDetListini.Inizio=#" & DataLi & "# ORDER BY tbDetListini.Soglia;"
    RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
    If Not RsSoglie.BOF Then
        Conta = 1
        RsSoglie.MoveLast
        'QuantSoglia = RsSoglie!Soglia
        RsSoglie.MoveFirst
        While Not RsSoglie.EOF
            If (RsSoglie!Soglia <= Quant) And Soglie(Conta) Then
                QuantSoglia = RsSoglie!Soglia
            End If
            Conta = Conta + 1
            RsSoglie.MoveNext
        Wend
    End If
    RsSoglie.Close
    If QuantSoglia = 0 Then
        query = "SELECT DISTINCT Soglia FROM tbArticoli INNER JOIN tbOfferte ON tbArticoli.CodArt = tbOfferte.CodArtCli WHERE  (DataInizio<=#" & DataOrd & "#) AND " & _
            "(DataFine>=#" & DataOrd & "#) ORDER BY tbOfferte.Soglia;"
        RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
        If Not RsSoglie.BOF Then
            Conta = 1
            RsSoglie.MoveLast
            'QuantSoglia = RsSoglie!Soglia
            RsSoglie.MoveFirst
            While Not RsSoglie.EOF
                If (RsSoglie!Soglia <= Quant) And Soglie(Conta) Then
                    QuantSoglia = RsSoglie!Soglia
                End If
                Conta = Conta + 1
                RsSoglie.MoveNext
            Wend
        End If
        RsSoglie.Close
        If QuantSoglia = 0 Then
            GoTo fine
        End If
    End If
    query = "SELECT * FROM tbOfferte WHERE (CodArtCli = '" & CodProd & _
            "') AND (Cliente = '" & CodRag & "') AND (DataInizio<=#" & DataOrd & "#) AND " & _
            "(DataFine>=#" & DataOrd & "#) AND (Soglia=" & QuantSoglia & ") ORDER BY Soglia,DataInizio;"
    RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
    If Not RsSoglie.BOF Then
        RsSoglie.MoveFirst
        promo = True
        If FrDest Then
            Prz = RsSoglie!PrezzoFD
        Else
            Prz = RsSoglie!PrezzoFR
        End If
        GoTo fine
    End If
    RsSoglie.Close
    query = "SELECT * FROM tbPromo WHERE (CodArtCli = '" & CodProd & _
            "') AND (Cliente = '" & CodRag & "') AND (DataInizio<=#" & DataOrd & "#) AND " & _
            "(DataFine>=#" & DataOrd & "#) AND (Soglia=" & QuantSoglia & ") ORDER BY Soglia,DataInizio;"
    RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
    If Not RsSoglie.BOF Then
        RsSoglie.MoveFirst
        promo = True
        If FrDest Then
            Prz = RsSoglie!PrezzoFD
        Else
            Prz = RsSoglie!PrezzoFR
        End If
    Else
        RsSoglie.Close
        If Categoria = 5 Or Categoria = 6 Then
            query = "SELECT * FROM tbpromo WHERE (CodArtCli = '" & CodProd & _
                 "') AND (Cliente ='000') AND (DataInizio<=#" & DataOrd & "#) AND " & _
                 "(DataFine>=#" & DataOrd & "#) AND (Soglia=" & QuantSoglia & ")"
            RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
            If Not RsSoglie.BOF Then
                promo = True
                RsSorglie.MoveFirst
                If FrDest Then
                    Prz = RsSoglie!PrezzoFD
                Else
                    Prz = RsSoglie!PrezzoFR
                End If
                GoTo fine
            End If
            RsSoglie.Close
        End If
        query = "SELECT tbArticoli.CodArt, tbDetListini.CodArt AS CapoGruppo, tbDetListini.Soglia, tbDetListini.Inizio, tbDetListini.PrezzoFR, tbDetListini.PrezzoFD, tbDetListini.Consigliato FROM tbArticoli INNER JOIN tbDetListini ON tbArticoli.CodArt = tbDetListini.CodArt WHERE tbDetListini.Inizio=#" & DataLi & "# AND tbDetListini.CodArt ='" & CodProd & "' AND Soglia=" & QuantSoglia & " ORDER BY tbDetListini.Soglia;"
        RsSoglie.Open query, Conn2, adOpenKeyset, adLockOptimistic, adCmdText
        If Not RsSoglie.BOF Then
            RsSoglie.MoveFirst
            If FrDest Then
                Prz = RsSoglie!PrezzoFD
            Else
                Prz = RsSoglie!PrezzoFR
            End If
        Else
            Prezzo = 0
        End If
    End If
fine:
    Conn2.Close
    CercaPrezzo = Prz
End Function
