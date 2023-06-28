Attribute VB_Name = "Bierbrouwen"
Option Explicit

Function Plato2Brix(plato As Single) As Single
'Zet de Plato waarde om naar Brix
    Plato2Brix = 1.04 * plato
End Function

Function Brix2Plato(brix As Single) As Single
'Zet de Brixwaarde om naar Plato
    Brix2Plato = brix / 1.04
End Function

Function SG2Plato(sg As Single, Optional ByVal methode As String = "simpel") As Single
'Zet de SG waarde om naar Plato
'Optioneel kan de berekeningsmethode, "simpel" (default) of "clerck" worden meegegeven
    If methode = "simpel" Then SG2Plato = 250 * (sg - 1)
    If methode = "clerck" Then SG2Plato = 259 - 259 / sg
End Function

Function Plato2SG(plato As Single, Optional ByVal methode As String = "simpel") As Single
'Zet de Plato waarde om naar SG
'Optioneel kan de berekeningsmethode, "simpel" (default) of "clerck" worden meegegeven
    If methode = "simpel" Then Plato2SG = 1 + plato / 250
    If methode = "clerck" Then Plato2SG = 259 / (259 - plato)
End Function

Function DHR_Eenvoudig(kooktijd As Single) As Single
'Bepaalt decimaal hoprendement volgens eenvoudige methode
'kooktijd = kooktijd in minuten
    Select Case kooktijd
        Case 0 To 9
            DHR_Eenvoudig = 0
        Case 10 To 30
            DHR_Eenvoudig = 0.1
        Case 31 To 59
            DHR_Eenvoudig = (kooktijd - 10) / 200 'interpolatie
        Case Else
            DHR_Eenvoudig = 0.25
    End Select
End Function

Function DHR_Tinseth(kooktijd As Single, sg As Single) As Single
'Bepaalt decimaal hoprendement volgens Tinseth
'kooktijd = kooktijd in minuten
'sg = relatieve dichtheid
    Dim sg_factor, kooktijd_factor As Single
    kooktijd_factor = (1 - Exp(-0.04 * kooktijd)) / 4.15
    sg_factor = 1.65 * 0.000125 ^ (sg - 1)
    DHR_Tinseth = kooktijd_factor * sg_factor
End Function

Function DHR_Rager(kooktijd As Single, sg As Single) As Single
'Bepaalt decimaal hoprendement volgens Rager
'kooktijd = kooktijd in minuten
'sg = relatieve dichtheid
    Dim DHR_kooktijd As Single
    'Bepaling DHR op basis van alleen kooktijd
    Select Case kooktijd
        Case 0 To 5
            DHR_kooktijd = 0.05
        Case 6 To 10
            DHR_kooktijd = 0.06
        Case 11 To 15
            DHR_kooktijd = 0.08
        Case 16 To 20
            DHR_kooktijd = 0.101
        Case 21 To 25
            DHR_kooktijd = 0.121
        Case 26 To 30
            DHR_kooktijd = 0.153
        Case 31 To 35
            DHR_kooktijd = 0.188
        Case 36 To 40
            DHR_kooktijd = 0.228
        Case 41 To 45
            DHR_kooktijd = 0.269
        Case 46 To 50
            DHR_kooktijd = 0.281
        Case 51 To 60
            DHR_kooktijd = 0.3
        Case Else
            DHR_kooktijd = 0.3
    End Select
    'Corrigeren op basis van SG
    DHR_Rager = DHR_kooktijd * CSG(sg)
End Function

Function CSG(sg As Single) As Single
'SG correctie factor volgens Rager
    If sg <= 1.05 Then
        CSG = 1
    Else
        CSG = 1 / (1 + 5 * (sg - 1.05))
    End If
End Function

Function DHR_Daniels(kooktijd As Single, sg As Single, Optional ByVal hopvorm As String = "bloemen") As Single
'Bepaalt decimaal hoprendement volgens Tinseth
'kooktijd = kooktijd in minuten
'sg = relatieve dichtheid
'hopvorm = bloemen of pellets
    Dim DHR_kooktijd As Single
    'Bepaling DHR op basis van alleen kooktijd voor hopvorm bloemen
    Select Case kooktijd
        Case 0 To 9
            DHR_kooktijd = 0.05
        Case 10 To 19
            DHR_kooktijd = 0.12
        Case 20 To 29
            DHR_kooktijd = 0.15
        Case 30 To 44
            DHR_kooktijd = 0.19
        Case 45 To 59
            DHR_kooktijd = 0.22
        Case 60 To 74
            DHR_kooktijd = 0.24
        Case Else
            DHR_kooktijd = 0.27
    End Select
    'Corrigeren voor pellets
    If hopvorm = "pellets" Then DHR_kooktijd = 1.26 * DHR_kooktijd
   'Corrigeren op basis van SG
    DHR_Daniels = DHR_kooktijd * CSG(sg)
End Function

Function SRM2EBC(srm As Single) As Single
'Zet de SRM kleurwaarde om naar EBC
    SRM2EBC = srm * 1.97
End Function

Function EBC2SRM(ebc As Single) As Single
'Zet de EBC kleurwaarde om naar SRM
    EBC2SRM = ebc / 1.97
End Function

Function SRM2Lovi(srm As Single) As Single
'Zet de SRM kleurwaarde om naar Lovibond
    SRM2Lovi = (srm + 0.76) / 1.3546
End Function

Function Lovi2SRM(lovi As Single) As Single
'Zet de Lovibond kleurwaarde om naar SRM
    Lovi2SRM = 1.3546 * lovi - 0.76
End Function
