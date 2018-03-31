'Author: Dale Webb
'Program: Money Model
'IDE: Microsoft Visual Studio 2010 Beta 1
'Language: Basic

Public Class Form1
    Dim TotalE, TotalA, MondayE, MondayA, TuesdayE, TuesdayA, WednesdayE, WednesdayA, ThursdayE, ThursdayA, FridayE, FridayA, SaturdayE, SaturdayA, SundayE, SundayA As Decimal
    Dim UserName As String
    Dim Selected, Selected1 As Date
    Dim CustomDate As Integer
    Dim Mon1E, Mon2E, Mon3E, Mon4E, Mon5E, Mon1A, Mon2A, Mon3A, Mon4A, Mon5A, Tue1E, Tue2E, Tue3E, Tue4E, Tue5E, Tue1A, Tue2A, Tue3A, Tue4A, Tue5A, Wed1E, Wed2E, Wed3E, Wed4E, Wed5E As Decimal
    Dim Wed1A, Wed2A, Wed3A, Wed4A, Wed5A, Thu1E, Thu2E, Thu3E, Thu4E, Thu5E, Thu1A, Thu2A, Thu3A, Thu4A, Thu5A, Fri1E, Fri2E, Fri3E, Fri4E, Fri5E, Fri1A, Fri2A, Fri3A, Fri4A, Fri5A As Decimal
    Dim Sat1E, Sat2E, Sat3E, Sat4E, Sat5E, Sat1A, Sat2A, Sat3A, Sat4A, Sat5A, Sun1E, Sun2E, Sun3E, Sun4E, Sun5E, Sun1A, Sun2A, Sun3A, Sun4A, Sun5A As Decimal

    'Functions
    Public Sub Calculation()
        If (TextBox6.Text = "") Then TextBox6.Text = "0.00"
        If (TextBox7.Text = "") Then TextBox7.Text = "0.00"
        If (TextBox8.Text = "") Then TextBox8.Text = "0.00"
        If (TextBox9.Text = "") Then TextBox9.Text = "0.00"
        If (TextBox10.Text = "") Then TextBox10.Text = "0.00"

        If (TextBox6.Text < 0) Then TextBox6.Text = "0.00"
        If (TextBox7.Text < 0) Then TextBox7.Text = "0.00"
        If (TextBox8.Text < 0) Then TextBox8.Text = "0.00"
        If (TextBox9.Text < 0) Then TextBox9.Text = "0.00"
        If (TextBox10.Text < 0) Then TextBox10.Text = "0.00"

        Mon1E = TextBox6.Text
        Mon2E = TextBox7.Text
        Mon3E = TextBox8.Text
        Mon4E = TextBox9.Text
        Mon5E = TextBox10.Text

        MondayE = Mon1E + Mon2E + Mon3E + Mon4E + Mon5E
        txtMondayE.Text = MondayE

        If (TextBox6.Text < "") Then TextBox6.Text = "0.00"
        If (TextBox7.Text < "") Then TextBox7.Text = "0.00"
        If (TextBox8.Text < "") Then TextBox8.Text = "0.00"
        If (TextBox9.Text < "") Then TextBox9.Text = "0.00"
        If (TextBox10.Text < "") Then TextBox10.Text = "0.00"

        If (TextBox15.Text = "") Then TextBox15.Text = "0.00"
        If (TextBox14.Text = "") Then TextBox14.Text = "0.00"
        If (TextBox13.Text = "") Then TextBox13.Text = "0.00"
        If (TextBox12.Text = "") Then TextBox12.Text = "0.00"
        If (TextBox11.Text = "") Then TextBox11.Text = "0.00"

        Tue1E = TextBox15.Text
        Tue2E = TextBox14.Text
        Tue3E = TextBox13.Text
        Tue4E = TextBox12.Text
        Tue5E = TextBox11.Text

        TuesdayE = Tue1E + Tue2E + Tue3E + Tue4E + Tue5E
        txtTuesdayE.Text = TuesdayE

        If (TextBox15.Text < "") Then TextBox15.Text = "0.00"
        If (TextBox14.Text < "") Then TextBox14.Text = "0.00"
        If (TextBox13.Text < "") Then TextBox13.Text = "0.00"
        If (TextBox12.Text < "") Then TextBox12.Text = "0.00"
        If (TextBox11.Text < "") Then TextBox11.Text = "0.00"

        If (TextBox25.Text = "") Then TextBox25.Text = "0.00"
        If (TextBox24.Text = "") Then TextBox24.Text = "0.00"
        If (TextBox23.Text = "") Then TextBox23.Text = "0.00"
        If (TextBox22.Text = "") Then TextBox22.Text = "0.00"
        If (TextBox21.Text = "") Then TextBox21.Text = "0.00"

        Wed1E = TextBox25.Text
        Wed2E = TextBox24.Text
        Wed3E = TextBox23.Text
        Wed4E = TextBox22.Text
        Wed5E = TextBox21.Text

        WednesdayE = Wed1E + Wed2E + Wed3E + Wed4E + Wed5E
        txtWednesdayE.Text = WednesdayE

        If (TextBox25.Text < "") Then TextBox25.Text = "0.00"
        If (TextBox24.Text < "") Then TextBox24.Text = "0.00"
        If (TextBox23.Text < "") Then TextBox23.Text = "0.00"
        If (TextBox22.Text < "") Then TextBox22.Text = "0.00"
        If (TextBox21.Text < "") Then TextBox21.Text = "0.00"

        If (TextBox35.Text = "") Then TextBox35.Text = "0.00"
        If (TextBox34.Text = "") Then TextBox34.Text = "0.00"
        If (TextBox33.Text = "") Then TextBox33.Text = "0.00"
        If (TextBox32.Text = "") Then TextBox32.Text = "0.00"
        If (TextBox31.Text = "") Then TextBox31.Text = "0.00"

        Thu1E = TextBox35.Text
        Thu2E = TextBox34.Text
        Thu3E = TextBox33.Text
        Thu4E = TextBox32.Text
        Thu5E = TextBox31.Text

        ThursdayE = Thu1E + Thu2E + Thu3E + Thu4E + Thu5E
        txtThursdayE.Text = ThursdayE

        If (TextBox35.Text < "") Then TextBox35.Text = "0.00"
        If (TextBox34.Text < "") Then TextBox34.Text = "0.00"
        If (TextBox33.Text < "") Then TextBox33.Text = "0.00"
        If (TextBox32.Text < "") Then TextBox32.Text = "0.00"
        If (TextBox31.Text < "") Then TextBox31.Text = "0.00"

        If (TextBox45.Text = "") Then TextBox45.Text = "0.00"
        If (TextBox44.Text = "") Then TextBox44.Text = "0.00"
        If (TextBox43.Text = "") Then TextBox43.Text = "0.00"
        If (TextBox42.Text = "") Then TextBox42.Text = "0.00"
        If (TextBox41.Text = "") Then TextBox41.Text = "0.00"

        Fri1E = TextBox45.Text
        Fri2E = TextBox44.Text
        Fri3E = TextBox43.Text
        Fri4E = TextBox42.Text
        Fri5E = TextBox41.Text

        FridayE = Fri1E + Fri2E + Fri3E + Fri4E + Fri5E
        txtFridayE.Text = FridayE

        If (TextBox45.Text < "") Then TextBox45.Text = "0.00"
        If (TextBox44.Text < "") Then TextBox44.Text = "0.00"
        If (TextBox43.Text < "") Then TextBox43.Text = "0.00"
        If (TextBox42.Text < "") Then TextBox42.Text = "0.00"
        If (TextBox41.Text < "") Then TextBox41.Text = "0.00"

        If (TextBox55.Text = "") Then TextBox55.Text = "0.00"
        If (TextBox54.Text = "") Then TextBox54.Text = "0.00"
        If (TextBox53.Text = "") Then TextBox53.Text = "0.00"
        If (TextBox52.Text = "") Then TextBox52.Text = "0.00"
        If (TextBox51.Text = "") Then TextBox51.Text = "0.00"

        Sat1E = TextBox55.Text
        Sat2E = TextBox54.Text
        Sat3E = TextBox53.Text
        Sat4E = TextBox52.Text
        Sat5E = TextBox51.Text

        SaturdayE = Sat1E + Sat2E + Sat3E + Sat4E + Sat5E
        txtSaturdayE.Text = SaturdayE

        If (TextBox55.Text < "") Then TextBox55.Text = "0.00"
        If (TextBox54.Text < "") Then TextBox54.Text = "0.00"
        If (TextBox53.Text < "") Then TextBox53.Text = "0.00"
        If (TextBox52.Text < "") Then TextBox52.Text = "0.00"
        If (TextBox51.Text < "") Then TextBox51.Text = "0.00"

        If (TextBox65.Text = "") Then TextBox65.Text = "0.00"
        If (TextBox64.Text = "") Then TextBox64.Text = "0.00"
        If (TextBox63.Text = "") Then TextBox63.Text = "0.00"
        If (TextBox62.Text = "") Then TextBox62.Text = "0.00"
        If (TextBox61.Text = "") Then TextBox61.Text = "0.00"

        Sun1E = TextBox65.Text
        Sun2E = TextBox64.Text
        Sun3E = TextBox63.Text
        Sun4E = TextBox62.Text
        Sun5E = TextBox61.Text

        SundayE = Sun1E + Sun2E + Sun3E + Sun4E + Sun5E
        txtSundayE.Text = SundayE

        If (TextBox65.Text < "") Then TextBox65.Text = "0.00"
        If (TextBox64.Text < "") Then TextBox64.Text = "0.00"
        If (TextBox63.Text < "") Then TextBox63.Text = "0.00"
        If (TextBox62.Text < "") Then TextBox62.Text = "0.00"
        If (TextBox61.Text < "") Then TextBox61.Text = "0.00"

        If (TextBox75.Text = "") Then TextBox75.Text = "0.00"
        If (TextBox74.Text = "") Then TextBox74.Text = "0.00"
        If (TextBox73.Text = "") Then TextBox73.Text = "0.00"
        If (TextBox72.Text = "") Then TextBox72.Text = "0.00"
        If (TextBox71.Text = "") Then TextBox71.Text = "0.00"

        Mon1A = TextBox75.Text
        Mon2A = TextBox74.Text
        Mon3A = TextBox73.Text
        Mon4A = TextBox72.Text
        Mon5A = TextBox71.Text

        MondayA = Mon1A + Mon2A + Mon3A + Mon4A + Mon5A
        txtMondayA.Text = MondayA

        If (TextBox75.Text < "") Then TextBox75.Text = "0.00"
        If (TextBox74.Text < "") Then TextBox74.Text = "0.00"
        If (TextBox73.Text < "") Then TextBox73.Text = "0.00"
        If (TextBox72.Text < "") Then TextBox72.Text = "0.00"
        If (TextBox71.Text < "") Then TextBox71.Text = "0.00"

        If (TextBox85.Text = "") Then TextBox85.Text = "0.00"
        If (TextBox84.Text = "") Then TextBox84.Text = "0.00"
        If (TextBox83.Text = "") Then TextBox83.Text = "0.00"
        If (TextBox82.Text = "") Then TextBox82.Text = "0.00"
        If (TextBox81.Text = "") Then TextBox81.Text = "0.00"

        Tue1A = TextBox85.Text
        Tue2A = TextBox84.Text
        Tue3A = TextBox83.Text
        Tue4A = TextBox82.Text
        Tue5A = TextBox81.Text

        TuesdayA = Tue1A + Tue2A + Tue3A + Tue4A + Tue5A
        txtTuesdayA.Text = TuesdayA

        If (TextBox85.Text < "") Then TextBox85.Text = "0.00"
        If (TextBox84.Text < "") Then TextBox84.Text = "0.00"
        If (TextBox83.Text < "") Then TextBox83.Text = "0.00"
        If (TextBox82.Text < "") Then TextBox82.Text = "0.00"
        If (TextBox81.Text < "") Then TextBox81.Text = "0.00"

        If (TextBox95.Text = "") Then TextBox95.Text = "0.00"
        If (TextBox94.Text = "") Then TextBox94.Text = "0.00"
        If (TextBox93.Text = "") Then TextBox93.Text = "0.00"
        If (TextBox92.Text = "") Then TextBox92.Text = "0.00"
        If (TextBox91.Text = "") Then TextBox91.Text = "0.00"

        Wed1A = TextBox95.Text
        Wed2A = TextBox94.Text
        Wed3A = TextBox93.Text
        Wed4A = TextBox92.Text
        Wed5A = TextBox91.Text

        WednesdayA = Wed1A + Wed2A + Wed3A + Wed4A + Wed5A
        txtWednesdayA.Text = WednesdayA

        If (TextBox95.Text < "") Then TextBox95.Text = "0.00"
        If (TextBox94.Text < "") Then TextBox94.Text = "0.00"
        If (TextBox93.Text < "") Then TextBox93.Text = "0.00"
        If (TextBox92.Text < "") Then TextBox92.Text = "0.00"
        If (TextBox91.Text < "") Then TextBox91.Text = "0.00"

        If (TextBox105.Text = "") Then TextBox105.Text = "0.00"
        If (TextBox104.Text = "") Then TextBox104.Text = "0.00"
        If (TextBox103.Text = "") Then TextBox103.Text = "0.00"
        If (TextBox102.Text = "") Then TextBox102.Text = "0.00"
        If (TextBox101.Text = "") Then TextBox101.Text = "0.00"

        Thu1A = TextBox105.Text
        Thu2A = TextBox104.Text
        Thu3A = TextBox103.Text
        Thu4A = TextBox102.Text
        Thu5A = TextBox101.Text

        ThursdayA = Thu1A + Thu2A + Thu3A + Thu4A + Thu5A
        txtThursdayA.Text = ThursdayA

        If (TextBox105.Text < "") Then TextBox105.Text = "0.00"
        If (TextBox104.Text < "") Then TextBox104.Text = "0.00"
        If (TextBox103.Text < "") Then TextBox103.Text = "0.00"
        If (TextBox102.Text < "") Then TextBox102.Text = "0.00"
        If (TextBox101.Text < "") Then TextBox101.Text = "0.00"

        If (TextBox115.Text = "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text = "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text = "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text = "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text = "") Then TextBox111.Text = "0.00"

        Fri1A = TextBox115.Text
        Fri2A = TextBox114.Text
        Fri3A = TextBox113.Text
        Fri4A = TextBox112.Text
        Fri5A = TextBox111.Text

        FridayA = Fri1A + Fri2A + Fri3A + Fri4A + Fri5A
        txtFridayA.Text = FridayA

        If (TextBox115.Text < "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text < "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text < "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text < "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text < "") Then TextBox111.Text = "0.00"

        If (TextBox125.Text = "") Then TextBox125.Text = "0.00"
        If (TextBox124.Text = "") Then TextBox124.Text = "0.00"
        If (TextBox123.Text = "") Then TextBox123.Text = "0.00"
        If (TextBox122.Text = "") Then TextBox122.Text = "0.00"
        If (TextBox121.Text = "") Then TextBox121.Text = "0.00"

        Sat1A = TextBox125.Text
        Sat2A = TextBox124.Text
        Sat3A = TextBox123.Text
        Sat4A = TextBox122.Text
        Sat5A = TextBox121.Text

        SaturdayA = Sat1A + Sat2A + Sat3A + Sat4A + Sat5A
        txtSaturdayA.Text = SaturdayA

        If (TextBox125.Text < "") Then TextBox125.Text = "0.00"
        If (TextBox124.Text < "") Then TextBox124.Text = "0.00"
        If (TextBox123.Text < "") Then TextBox123.Text = "0.00"
        If (TextBox122.Text < "") Then TextBox122.Text = "0.00"
        If (TextBox121.Text < "") Then TextBox121.Text = "0.00"

        If (TextBox135.Text = "") Then TextBox135.Text = "0.00"
        If (TextBox134.Text = "") Then TextBox134.Text = "0.00"
        If (TextBox133.Text = "") Then TextBox133.Text = "0.00"
        If (TextBox132.Text = "") Then TextBox132.Text = "0.00"
        If (TextBox131.Text = "") Then TextBox131.Text = "0.00"

        Sun1A = TextBox135.Text
        Sun2A = TextBox134.Text
        Sun3A = TextBox133.Text
        Sun4A = TextBox132.Text
        Sun5A = TextBox131.Text

        SundayA = Sun1A + Sun2A + Sun3A + Sun4A + Sun5A
        txtSundayA.Text = SundayA

        If (TextBox135.Text < "") Then TextBox135.Text = "0.00"
        If (TextBox134.Text < "") Then TextBox134.Text = "0.00"
        If (TextBox133.Text < "") Then TextBox133.Text = "0.00"
        If (TextBox132.Text < "") Then TextBox132.Text = "0.00"
        If (TextBox131.Text < "") Then TextBox131.Text = "0.00"

        If (txtMondayA.Text = "") Then txtMondayA.Text = "0.00"
        If (txtMondayE.Text = "") Then txtMondayE.Text = "0.00"
        If (txtTuesdayA.Text = "") Then txtTuesdayA.Text = "0.00"
        If (txtTuesdayE.Text = "") Then txtTuesdayE.Text = "0.00"
        If (txtWednesdayA.Text = "") Then txtWednesdayA.Text = "0.00"
        If (txtWednesdayE.Text = "") Then txtWednesdayE.Text = "0.00"
        If (txtThursdayA.Text = "") Then txtThursdayA.Text = "0.00"
        If (txtThursdayE.Text = "") Then txtThursdayE.Text = "0.00"
        If (txtFridayA.Text = "") Then txtFridayA.Text = "0.00"
        If (txtFridayE.Text = "") Then txtFridayE.Text = "0.00"
        If (txtSaturdayA.Text = "") Then txtSaturdayA.Text = "0.00"
        If (txtSaturdayE.Text = "") Then txtSaturdayE.Text = "0.00"
        If (txtSundayA.Text = "") Then txtSundayA.Text = "0.00"
        If (txtSundayE.Text = "") Then txtSundayE.Text = "0.00"

        MondayE = txtMondayE.Text
        TuesdayE = txtTuesdayE.Text
        WednesdayE = txtWednesdayE.Text
        ThursdayE = txtThursdayE.Text
        FridayE = txtFridayE.Text
        SaturdayE = txtSaturdayE.Text
        SundayE = txtSundayE.Text

        TotalE = MondayE + TuesdayE + WednesdayE + ThursdayE + FridayE + SaturdayE + SundayE
        txtTotalE.Text = TotalE.ToString("C")

        If (txtMondayA.Text < "0") Then txtMondayA.Text = "0.00"
        If (txtMondayE.Text < "0") Then txtMondayE.Text = "0.00"
        If (txtTuesdayA.Text < "0") Then txtTuesdayA.Text = "0.00"
        If (txtTuesdayE.Text < "0") Then txtTuesdayE.Text = "0.00"
        If (txtWednesdayA.Text < "0") Then txtWednesdayA.Text = "0.00"
        If (txtWednesdayE.Text < "0") Then txtWednesdayE.Text = "0.00"
        If (txtThursdayA.Text < "0") Then txtThursdayA.Text = "0.00"
        If (txtThursdayE.Text < "0") Then txtThursdayE.Text = "0.00"
        If (txtFridayA.Text < "0") Then txtFridayA.Text = "0.00"
        If (txtFridayE.Text < "0") Then txtFridayE.Text = "0.00"
        If (txtSaturdayA.Text < "0") Then txtSaturdayA.Text = "0.00"
        If (txtSaturdayE.Text < "0") Then txtSaturdayE.Text = "0.00"
        If (txtSundayA.Text < "0") Then txtSundayA.Text = "0.00"
        If (txtSundayE.Text < "0") Then txtSundayE.Text = "0.00"

        MondayA = txtMondayA.Text
        TuesdayA = txtTuesdayA.Text
        WednesdayA = txtWednesdayA.Text
        ThursdayA = txtThursdayA.Text
        FridayA = txtFridayA.Text
        SaturdayA = txtSaturdayA.Text
        SundayA = txtSundayA.Text

        TotalA = MondayA + TuesdayA + WednesdayA + ThursdayA + FridayA + SaturdayA + SundayA

        txtMondayA.Text = "(" + Convert.ToString((MondayA - MondayE)) + ") " + Convert.ToString(MondayA)
        txtTotalA.Text = "(" + Convert.ToString((TotalA - TotalE)) + ") " + Convert.ToString(TotalA)
        txtTuesdayA.Text = "(" + Convert.ToString((TuesdayA - TuesdayE)) + ") " + Convert.ToString(TuesdayA)
        txtWednesdayA.Text = "(" + Convert.ToString((WednesdayA - WednesdayE)) + ") " + Convert.ToString(WednesdayA)
        txtThursdayA.Text = "(" + Convert.ToString((ThursdayA - ThursdayE)) + ") " + Convert.ToString(ThursdayA)
        txtFridayA.Text = "(" + Convert.ToString((FridayA - FridayE)) + ") " + Convert.ToString(FridayA)
        txtSaturdayA.Text = "(" + Convert.ToString((SaturdayA - SaturdayE)) + ") " + Convert.ToString(SaturdayA)
        txtSundayA.Text = "(" + Convert.ToString((SundayA - SundayE)) + ") " + Convert.ToString(SundayA)

        If (TotalA < TotalE) Then txtTotalE.BackColor = Color.Red Else txtTotalE.BackColor = Color.Lime
        If (MondayA < MondayE) Then txtMondayE.BackColor = Color.Red Else txtMondayE.BackColor = Color.Lime
        If (TuesdayA < TuesdayE) Then txtTuesdayE.BackColor = Color.Red Else txtTuesdayE.BackColor = Color.Lime
        If (WednesdayA < WednesdayE) Then txtWednesdayE.BackColor = Color.Red Else txtWednesdayE.BackColor = Color.Lime
        If (ThursdayA < ThursdayE) Then txtThursdayE.BackColor = Color.Red Else txtThursdayE.BackColor = Color.Lime
        If (FridayA < FridayE) Then txtFridayE.BackColor = Color.Red Else txtFridayE.BackColor = Color.Lime
        If (SaturdayA < SaturdayE) Then txtSaturdayE.BackColor = Color.Red Else txtSaturdayE.BackColor = Color.Lime
        If (SundayA < SundayE) Then txtSundayE.BackColor = Color.Red Else txtSundayE.BackColor = Color.Lime
        If (TotalA < TotalE) Then Label21.BackColor = Color.Red Else Label21.BackColor = Color.Lime
        If (MondayA < MondayE) Then Label12.BackColor = Color.Red Else Label12.BackColor = Color.Lime
        If (TuesdayA < TuesdayE) Then Label13.BackColor = Color.Red Else Label13.BackColor = Color.Lime
        If (WednesdayA < WednesdayE) Then Label16.BackColor = Color.Red Else Label16.BackColor = Color.Lime
        If (ThursdayA < ThursdayE) Then Label17.BackColor = Color.Red Else Label17.BackColor = Color.Lime
        If (FridayA < FridayE) Then Label18.BackColor = Color.Red Else Label18.BackColor = Color.Lime
        If (SaturdayA < SaturdayE) Then Label19.BackColor = Color.Red Else Label19.BackColor = Color.Lime
        If (SundayA < SundayE) Then Label20.BackColor = Color.Red Else Label20.BackColor = Color.Lime


        If TotalA < TotalE Then Label15.Text = "Overspent" Else Label15.Text = "Stable"

        If (TotalA < TotalE) Then MessageBox.Show("You have Overspent by £" + Convert.ToString((TotalA - TotalE)), "Financial Status") Else MessageBox.Show("You are £" + Convert.ToString((TotalA - TotalE)) + " within budget", "Financial Status")
    End Sub
    Public Sub MondaySelect()
        Label1.BackColor = Color.LightYellow
        Panel1.BackColor = Color.LightYellow
        LinkLabel1.Visible() = True
        LinkLabel7.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0
        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue
        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub TuesdaySelect()
        Label2.BackColor = Color.LightYellow
        Panel2.BackColor = Color.LightYellow
        LinkLabel2.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel7.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label1.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue

        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub WednesdaySelect()
        Label3.BackColor = Color.LightYellow
        Panel3.BackColor = Color.LightYellow
        LinkLabel3.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel7.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue

        Label1.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub ThursdaySelect()
        Label4.BackColor = Color.LightYellow
        Panel4.BackColor = Color.LightYellow
        LinkLabel4.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel7.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue

        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label1.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub FridaySelect()
        Label5.BackColor = Color.LightYellow
        Panel5.BackColor = Color.LightYellow
        LinkLabel5.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel7.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue

        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label1.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub SaturdaySelect()
        Label7.BackColor = Color.LightYellow
        Panel6.BackColor = Color.LightYellow
        LinkLabel6.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel7.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue

        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label6.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue

        Label1.BackColor = Color.AliceBlue
        Panel7.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub SundaySelect()
        Label6.BackColor = Color.LightYellow
        Panel7.BackColor = Color.LightYellow
        LinkLabel7.Visible() = True
        LinkLabel1.Visible() = False
        LinkLabel2.Visible() = False
        LinkLabel3.Visible() = False
        LinkLabel4.Visible() = False
        LinkLabel5.Visible() = False
        LinkLabel6.Visible() = False
        Panel8.Height() = 0
        Panel9.Height() = 0
        Panel10.Height() = 0
        Panel11.Height() = 0
        Panel12.Height() = 0
        Panel13.Height() = 0
        Panel14.Height() = 0
        Panel15.Height() = 0
        Panel16.Height() = 0
        Panel17.Height() = 0
        Panel18.Height() = 0
        Panel19.Height() = 0
        Panel20.Height() = 0
        Panel21.Height() = 0

        Label2.BackColor = Color.AliceBlue
        Panel2.BackColor = Color.AliceBlue

        Label3.BackColor = Color.AliceBlue
        Panel3.BackColor = Color.AliceBlue

        Label4.BackColor = Color.AliceBlue
        Panel4.BackColor = Color.AliceBlue

        Label5.BackColor = Color.AliceBlue
        Panel5.BackColor = Color.AliceBlue

        Label1.BackColor = Color.AliceBlue
        Panel6.BackColor = Color.AliceBlue

        Label7.BackColor = Color.AliceBlue
        Panel1.BackColor = Color.AliceBlue
        AutoSelect()
    End Sub
    Public Sub AutoSelect()
        If Date.Now.DayOfWeek.ToString() = "Monday" Then Panel1.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Tuesday" Then Panel2.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Wednesday" Then Panel3.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Thursday" Then Panel4.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Friday" Then Panel5.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Saturday" Then Panel6.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Sunday" Then Panel7.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Monday" Then Label1.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Tuesday" Then Label2.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Wednesday" Then Label3.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Thursday" Then Label4.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Friday" Then Label5.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Saturday" Then Label7.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Sunday" Then Label6.BackColor = Color.LightBlue
        If Date.Now.DayOfWeek.ToString() = "Monday" Then Label1.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Tuesday" Then Label2.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Wednesday" Then Label3.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Thursday" Then Label4.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Friday" Then Label5.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Saturday" Then Label7.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Sunday" Then Label6.ForeColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Monday" Then Label22.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Tuesday" Then Label23.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Wednesday" Then Label24.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Thursday" Then Label25.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Friday" Then Label26.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Saturday" Then Label27.BackColor = Color.White
        If Date.Now.DayOfWeek.ToString() = "Sunday" Then Label28.BackColor = Color.White
        If Panel1.BackColor = Color.LightBlue Then LinkLabel1.LinkColor = Color.White
        If Panel2.BackColor = Color.LightBlue Then LinkLabel2.LinkColor = Color.White
        If Panel3.BackColor = Color.LightBlue Then LinkLabel3.LinkColor = Color.White
        If Panel4.BackColor = Color.LightBlue Then LinkLabel4.LinkColor = Color.White
        If Panel5.BackColor = Color.LightBlue Then LinkLabel5.LinkColor = Color.White
        If Panel6.BackColor = Color.LightBlue Then LinkLabel6.LinkColor = Color.White
        If Panel7.BackColor = Color.LightBlue Then LinkLabel7.LinkColor = Color.White
    End Sub



    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Text = "Date Today: " + Date.Now.ToLongDateString
        GroupBox1.Text = "Date: " + Date.Now.ToLongDateString
        AutoSelect()
        Me.Width() = 580
        DateTimePicker1.MaxDate() = Today

    End Sub


    'Buttons
    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Calculation()
        Me.Width() = 833
        Button6.Hide()

    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        txtMondayA.Text = "0.00"
        txtMondayE.Text = "0.00"
        txtTuesdayA.Text = "0.00"
        txtTuesdayE.Text = "0.00"
        txtWednesdayA.Text = "0.00"
        txtWednesdayE.Text = "0.00"
        txtThursdayA.Text = "0.00"
        txtThursdayE.Text = "0.00"
        txtFridayA.Text = "0.00"
        txtFridayE.Text = "0.00"
        txtSaturdayA.Text = "0.00"
        txtSaturdayE.Text = "0.00"
        txtSundayA.Text = "0.00"
        txtSundayE.Text = "0.00"
        txtTotalE.Text = "0.00"
        txtTotalA.Text = "0.00"
        TextBox1.Text = ""
        TextBox2.Text = ""
        TextBox3.Text = ""
        TextBox4.Text = ""
        TextBox6.Text = ""
        TextBox7.Text = ""
        TextBox8.Text = ""
        TextBox9.Text = ""
        TextBox10.Text = ""
        TextBox11.Text = ""
        TextBox12.Text = ""
        TextBox13.Text = ""
        TextBox14.Text = ""
        TextBox15.Text = ""
        TextBox17.Text = ""
        TextBox18.Text = ""
        TextBox19.Text = ""
        TextBox20.Text = ""
        TextBox21.Text = ""
        TextBox22.Text = ""
        TextBox23.Text = ""
        TextBox24.Text = ""
        TextBox25.Text = ""
        TextBox27.Text = ""
        TextBox28.Text = ""
        TextBox29.Text = ""
        TextBox30.Text = ""
        TextBox31.Text = ""
        TextBox32.Text = ""
        TextBox33.Text = ""
        TextBox34.Text = ""
        TextBox35.Text = ""
        TextBox37.Text = ""
        TextBox38.Text = ""
        TextBox39.Text = ""
        TextBox40.Text = ""
        TextBox41.Text = ""
        TextBox42.Text = ""
        TextBox43.Text = ""
        TextBox44.Text = ""
        TextBox45.Text = ""
        TextBox47.Text = ""
        TextBox48.Text = ""
        TextBox49.Text = ""
        TextBox50.Text = ""
        TextBox51.Text = ""
        TextBox52.Text = ""
        TextBox53.Text = ""
        TextBox54.Text = ""
        TextBox55.Text = ""
        TextBox57.Text = ""
        TextBox58.Text = ""
        TextBox59.Text = ""
        TextBox60.Text = ""
        TextBox61.Text = ""
        TextBox62.Text = ""
        TextBox63.Text = ""
        TextBox64.Text = ""
        TextBox65.Text = ""
        TextBox67.Text = ""
        TextBox68.Text = ""
        TextBox69.Text = ""
        TextBox70.Text = ""
        TextBox71.Text = ""
        TextBox72.Text = ""
        TextBox73.Text = ""
        TextBox74.Text = ""
        TextBox75.Text = ""
        TextBox77.Text = ""
        TextBox78.Text = ""
        TextBox79.Text = ""
        TextBox80.Text = ""
        TextBox81.Text = ""
        TextBox82.Text = ""
        TextBox83.Text = ""
        TextBox84.Text = ""
        TextBox85.Text = ""
        TextBox87.Text = ""
        TextBox88.Text = ""
        TextBox89.Text = ""
        TextBox90.Text = ""
        TextBox91.Text = ""
        TextBox92.Text = ""
        TextBox93.Text = ""
        TextBox94.Text = ""
        TextBox95.Text = ""
        TextBox97.Text = ""
        TextBox98.Text = ""
        TextBox99.Text = ""
        TextBox100.Text = ""
        TextBox101.Text = ""
        TextBox102.Text = ""
        TextBox103.Text = ""
        TextBox104.Text = ""
        TextBox105.Text = ""
        TextBox107.Text = ""
        TextBox108.Text = ""
        TextBox109.Text = ""
        TextBox110.Text = ""
        TextBox111.Text = ""
        TextBox112.Text = ""
        TextBox113.Text = ""
        TextBox114.Text = ""
        TextBox115.Text = ""
        TextBox117.Text = ""
        TextBox118.Text = ""
        TextBox119.Text = ""
        TextBox120.Text = ""
        TextBox121.Text = ""
        TextBox122.Text = ""
        TextBox123.Text = ""
        TextBox124.Text = ""
        TextBox125.Text = ""
        TextBox127.Text = ""
        TextBox128.Text = ""
        TextBox129.Text = ""
        TextBox130.Text = ""
        TextBox131.Text = ""
        TextBox132.Text = ""
        TextBox133.Text = ""
        TextBox134.Text = ""
        TextBox135.Text = ""
        Me.Width() = 580
        Button6.Text() = "Exit"
        Button6.Show()

    End Sub
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        If Button6.Text = "Exit" Then Button5.PerformClick() Else Me.Width() = 833
        If Button6.Text = "Show Status" Then Button6.Hide()
    End Sub
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Close()
        Dialog1.Close()

    End Sub
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Button6.Text = "Show Status"
        Width() = 580
        Button6.Show()
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        SaveFileDialog1.FileName() = UserName + " Week " + Convert.ToString(DateFormat.ShortDate * Month(Date.Now()) * 7 * 2 / 7 + 1) + ".mmd"
        SaveFileDialog1.ShowDialog()

    End Sub

    'Days
    'Monday
    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
        MondaySelect()
    End Sub
    Private Sub Panel1_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel1.Click
        MondaySelect()
    End Sub
    Private Sub txtMondayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtMondayE.Click
        MondaySelect()
        If Panel8.Height() < 171 Then Panel8.Height() = 171 Else Panel8.Height() = 0
    End Sub
    Private Sub txtMondayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtMondayA.Click
        MondaySelect()
        If Panel15.Height() < 171 Then Panel15.Height() = 171 Else Panel15.Height() = 0
    End Sub
    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        If LinkLabel1.Text = "More Detail" Then

            Panel8.Height() = 171
            Panel15.Height() = 171
            LinkLabel1.Text = "Less Detail"
        Else
            Panel8.Height() = 0
            Panel15.Height() = 0
            LinkLabel1.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel8_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel8.Click

        If (TextBox6.Text = "") Then TextBox6.Text = "0.00"
        If (TextBox7.Text = "") Then TextBox7.Text = "0.00"
        If (TextBox8.Text = "") Then TextBox8.Text = "0.00"
        If (TextBox9.Text = "") Then TextBox9.Text = "0.00"
        If (TextBox10.Text = "") Then TextBox10.Text = "0.00"

        If (TextBox6.Text < 0) Then TextBox6.Text = "0.00"
        If (TextBox7.Text < 0) Then TextBox7.Text = "0.00"
        If (TextBox8.Text < 0) Then TextBox8.Text = "0.00"
        If (TextBox9.Text < 0) Then TextBox9.Text = "0.00"
        If (TextBox10.Text < 0) Then TextBox10.Text = "0.00"

        Mon1E = TextBox6.Text
        Mon2E = TextBox7.Text
        Mon3E = TextBox8.Text
        Mon4E = TextBox9.Text
        Mon5E = TextBox10.Text

        MondayE = Mon1E + Mon2E + Mon3E + Mon4E + Mon5E
        txtMondayE.Text = MondayE

        If (TextBox6.Text < "") Then TextBox6.Text = "0.00"
        If (TextBox7.Text < "") Then TextBox7.Text = "0.00"
        If (TextBox8.Text < "") Then TextBox8.Text = "0.00"
        If (TextBox9.Text < "") Then TextBox9.Text = "0.00"
        If (TextBox10.Text < "") Then TextBox10.Text = "0.00"
    End Sub
    Private Sub LinkLabel29_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel29.Click
        If (TextBox75.Text = "") Then TextBox75.Text = "0.00"
        If (TextBox74.Text = "") Then TextBox74.Text = "0.00"
        If (TextBox73.Text = "") Then TextBox73.Text = "0.00"
        If (TextBox72.Text = "") Then TextBox72.Text = "0.00"
        If (TextBox71.Text = "") Then TextBox71.Text = "0.00"

        Mon1A = TextBox75.Text
        Mon2A = TextBox74.Text
        Mon3A = TextBox73.Text
        Mon4A = TextBox72.Text
        Mon5A = TextBox71.Text

        MondayA = Mon1A + Mon2A + Mon3A + Mon4A + Mon5A
        txtMondayA.Text = MondayA

        If (TextBox75.Text < "") Then TextBox75.Text = "0.00"
        If (TextBox74.Text < "") Then TextBox74.Text = "0.00"
        If (TextBox73.Text < "") Then TextBox73.Text = "0.00"
        If (TextBox72.Text < "") Then TextBox72.Text = "0.00"
        If (TextBox71.Text < "") Then TextBox71.Text = "0.00"
    End Sub

    'Tuesday
    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click
        TuesdaySelect()
    End Sub
    Private Sub Panel2_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel2.Click

        TuesdaySelect()
    End Sub
    Private Sub txtTuesdayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtTuesdayE.Click
        TuesdaySelect()
        If Panel9.Height() < 171 Then Panel9.Height() = 171 Else Panel9.Height() = 0
    End Sub
    Private Sub txtTuesdayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtTuesdayA.Click
        TuesdaySelect()
        If Panel16.Height() < 171 Then Panel16.Height() = 171 Else Panel16.Height() = 0
    End Sub
    Private Sub LinkLabel2_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        If LinkLabel2.Text = "More Detail" Then

            Panel9.Height() = 171
            Panel16.Height() = 171
            LinkLabel2.Text = "Less Detail"
        Else
            Panel9.Height() = 0
            Panel16.Height() = 0
            LinkLabel2.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel23_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel23.Click

        If (TextBox15.Text = "") Then TextBox15.Text = "0.00"
        If (TextBox14.Text = "") Then TextBox14.Text = "0.00"
        If (TextBox13.Text = "") Then TextBox13.Text = "0.00"
        If (TextBox12.Text = "") Then TextBox12.Text = "0.00"
        If (TextBox11.Text = "") Then TextBox11.Text = "0.00"

        Tue1E = TextBox15.Text
        Tue2E = TextBox14.Text
        Tue3E = TextBox13.Text
        Tue4E = TextBox12.Text
        Tue5E = TextBox11.Text

        TuesdayE = Tue1E + Tue2E + Tue3E + Tue4E + Tue5E
        txtTuesdayE.Text = TuesdayE

        If (TextBox15.Text < "") Then TextBox15.Text = "0.00"
        If (TextBox14.Text < "") Then TextBox14.Text = "0.00"
        If (TextBox13.Text < "") Then TextBox13.Text = "0.00"
        If (TextBox12.Text < "") Then TextBox12.Text = "0.00"
        If (TextBox11.Text < "") Then TextBox11.Text = "0.00"
    End Sub
    Private Sub LinkLabel30_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel30.Click
        If (TextBox85.Text = "") Then TextBox85.Text = "0.00"
        If (TextBox84.Text = "") Then TextBox84.Text = "0.00"
        If (TextBox83.Text = "") Then TextBox83.Text = "0.00"
        If (TextBox82.Text = "") Then TextBox82.Text = "0.00"
        If (TextBox81.Text = "") Then TextBox81.Text = "0.00"

        Tue1A = TextBox85.Text
        Tue2A = TextBox84.Text
        Tue3A = TextBox83.Text
        Tue4A = TextBox82.Text
        Tue5A = TextBox81.Text

        TuesdayA = Tue1A + Tue2A + Tue3A + Tue4A + Tue5A
        txtTuesdayA.Text = TuesdayA

        If (TextBox85.Text < "") Then TextBox85.Text = "0.00"
        If (TextBox84.Text < "") Then TextBox84.Text = "0.00"
        If (TextBox83.Text < "") Then TextBox83.Text = "0.00"
        If (TextBox82.Text < "") Then TextBox82.Text = "0.00"
        If (TextBox81.Text < "") Then TextBox81.Text = "0.00"
    End Sub

    'Wednesday
    Private Sub Label3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label3.Click
        WednesdaySelect()
    End Sub
    Private Sub Panel3_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel3.Click
        WednesdaySelect()
    End Sub
    Private Sub txtWednesdayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtWednesdayE.Click
        WednesdaySelect()
        If Panel10.Height() < 171 Then Panel10.Height() = 171 Else Panel10.Height() = 0
    End Sub
    Private Sub txtWednesdayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtWednesdayA.Click
        WednesdaySelect()
        If Panel17.Height() < 171 Then Panel17.Height() = 171 Else Panel17.Height() = 0
    End Sub
    Private Sub LinkLabel3_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel3.LinkClicked
        If LinkLabel3.Text = "More Detail" Then

            Panel10.Height() = 171
            Panel17.Height() = 171
            LinkLabel3.Text = "Less Detail"
        Else
            Panel10.Height() = 0
            Panel17.Height() = 0
            LinkLabel3.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel24_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel24.LinkClicked
        If (TextBox25.Text = "") Then TextBox25.Text = "0.00"
        If (TextBox24.Text = "") Then TextBox24.Text = "0.00"
        If (TextBox23.Text = "") Then TextBox23.Text = "0.00"
        If (TextBox22.Text = "") Then TextBox22.Text = "0.00"
        If (TextBox21.Text = "") Then TextBox21.Text = "0.00"

        Wed1E = TextBox25.Text
        Wed2E = TextBox24.Text
        Wed3E = TextBox23.Text
        Wed4E = TextBox22.Text
        Wed5E = TextBox21.Text

        WednesdayE = Wed1E + Wed2E + Wed3E + Wed4E + Wed5E
        txtWednesdayE.Text = WednesdayE

        If (TextBox25.Text < "") Then TextBox25.Text = "0.00"
        If (TextBox24.Text < "") Then TextBox24.Text = "0.00"
        If (TextBox23.Text < "") Then TextBox23.Text = "0.00"
        If (TextBox22.Text < "") Then TextBox22.Text = "0.00"
        If (TextBox21.Text < "") Then TextBox21.Text = "0.00"
    End Sub
    Private Sub LinkLabel31_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel31.Click
        If (TextBox95.Text = "") Then TextBox95.Text = "0.00"
        If (TextBox94.Text = "") Then TextBox94.Text = "0.00"
        If (TextBox93.Text = "") Then TextBox93.Text = "0.00"
        If (TextBox92.Text = "") Then TextBox92.Text = "0.00"
        If (TextBox91.Text = "") Then TextBox91.Text = "0.00"

        Wed1A = TextBox95.Text
        Wed2A = TextBox94.Text
        Wed3A = TextBox93.Text
        Wed4A = TextBox92.Text
        Wed5A = TextBox91.Text

        WednesdayA = Wed1A + Wed2A + Wed3A + Wed4A + Wed5A
        txtWednesdayA.Text = WednesdayA

        If (TextBox95.Text < "") Then TextBox95.Text = "0.00"
        If (TextBox94.Text < "") Then TextBox94.Text = "0.00"
        If (TextBox93.Text < "") Then TextBox93.Text = "0.00"
        If (TextBox92.Text < "") Then TextBox92.Text = "0.00"
        If (TextBox91.Text < "") Then TextBox91.Text = "0.00"
    End Sub

    'Thursday
    Private Sub Label4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label4.Click
        ThursdaySelect()
    End Sub
    Private Sub Panel4_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel4.Click
        ThursdaySelect()
    End Sub
    Private Sub txtThursdayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtThursdayE.Click
        ThursdaySelect()
        If Panel11.Height() < 171 Then Panel11.Height() = 171 Else Panel11.Height() = 0
    End Sub
    Private Sub txtThursdayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtThursdayA.Click
        ThursdaySelect()
        If Panel18.Height() < 171 Then Panel18.Height() = 171 Else Panel18.Height() = 0
    End Sub
    Private Sub LinkLabel4_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel4.LinkClicked
        If LinkLabel4.Text = "More Detail" Then

            Panel11.Height() = 171
            Panel18.Height() = 171
            LinkLabel4.Text = "Less Detail"
        Else
            Panel11.Height() = 0
            Panel18.Height() = 0
            LinkLabel4.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel25_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel25.Click
        If (TextBox35.Text = "") Then TextBox35.Text = "0.00"
        If (TextBox34.Text = "") Then TextBox34.Text = "0.00"
        If (TextBox33.Text = "") Then TextBox33.Text = "0.00"
        If (TextBox32.Text = "") Then TextBox32.Text = "0.00"
        If (TextBox31.Text = "") Then TextBox31.Text = "0.00"

        Thu1E = TextBox35.Text
        Thu2E = TextBox34.Text
        Thu3E = TextBox33.Text
        Thu4E = TextBox32.Text
        Thu5E = TextBox31.Text

        ThursdayE = Thu1E + Thu2E + Thu3E + Thu4E + Thu5E
        txtThursdayE.Text = ThursdayE

        If (TextBox35.Text < "") Then TextBox35.Text = "0.00"
        If (TextBox34.Text < "") Then TextBox34.Text = "0.00"
        If (TextBox33.Text < "") Then TextBox33.Text = "0.00"
        If (TextBox32.Text < "") Then TextBox32.Text = "0.00"
        If (TextBox31.Text < "") Then TextBox31.Text = "0.00"
    End Sub
    Private Sub LinkLabel32_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel32.Click
        If (TextBox105.Text = "") Then TextBox105.Text = "0.00"
        If (TextBox104.Text = "") Then TextBox104.Text = "0.00"
        If (TextBox103.Text = "") Then TextBox103.Text = "0.00"
        If (TextBox102.Text = "") Then TextBox102.Text = "0.00"
        If (TextBox101.Text = "") Then TextBox101.Text = "0.00"

        Thu1A = TextBox105.Text
        Thu2A = TextBox104.Text
        Thu3A = TextBox103.Text
        Thu4A = TextBox102.Text
        Thu5A = TextBox101.Text

        ThursdayA = Thu1A + Thu2A + Thu3A + Thu4A + Thu5A
        txtThursdayA.Text = ThursdayA

        If (TextBox105.Text < "") Then TextBox105.Text = "0.00"
        If (TextBox104.Text < "") Then TextBox104.Text = "0.00"
        If (TextBox103.Text < "") Then TextBox103.Text = "0.00"
        If (TextBox102.Text < "") Then TextBox102.Text = "0.00"
        If (TextBox101.Text < "") Then TextBox101.Text = "0.00"
    End Sub

    'Friday
    Private Sub Label5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label5.Click
        FridaySelect()
    End Sub
    Private Sub Panel5_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel5.Click
        FridaySelect()
    End Sub
    Private Sub txtFridayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtFridayE.Click
        FridaySelect()
        If Panel12.Height() < 171 Then Panel12.Height() = 171 Else Panel12.Height() = 0
    End Sub
    Private Sub txtFridayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtFridayA.Click
        FridaySelect()
        If Panel19.Height() < 171 Then Panel19.Height() = 171 Else Panel19.Height() = 0
    End Sub
    Private Sub LinkLabel5_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel5.LinkClicked
        If LinkLabel5.Text = "More Detail" Then

            Panel12.Height() = 171
            Panel19.Height() = 171
            LinkLabel5.Text = "Less Detail"
        Else
            Panel12.Height() = 0
            Panel19.Height() = 0
            LinkLabel5.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel26_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel26.Click
        If (TextBox45.Text = "") Then TextBox45.Text = "0.00"
        If (TextBox44.Text = "") Then TextBox44.Text = "0.00"
        If (TextBox43.Text = "") Then TextBox43.Text = "0.00"
        If (TextBox42.Text = "") Then TextBox42.Text = "0.00"
        If (TextBox41.Text = "") Then TextBox41.Text = "0.00"

        Fri1E = TextBox45.Text
        Fri2E = TextBox44.Text
        Fri3E = TextBox43.Text
        Fri4E = TextBox42.Text
        Fri5E = TextBox41.Text

        FridayE = Fri1E + Fri2E + Fri3E + Fri4E + Fri5E
        txtFridayE.Text = FridayE

        If (TextBox45.Text < "") Then TextBox45.Text = "0.00"
        If (TextBox44.Text < "") Then TextBox44.Text = "0.00"
        If (TextBox43.Text < "") Then TextBox43.Text = "0.00"
        If (TextBox42.Text < "") Then TextBox42.Text = "0.00"
        If (TextBox41.Text < "") Then TextBox41.Text = "0.00"
    End Sub
    Private Sub LinkLabel33_Click(ByVal sender As Object, ByVal e As System.EventArgs)
        If (TextBox115.Text = "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text = "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text = "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text = "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text = "") Then TextBox111.Text = "0.00"

        Fri1A = TextBox115.Text
        Fri2A = TextBox114.Text
        Fri3A = TextBox113.Text
        Fri4A = TextBox112.Text
        Fri5A = TextBox111.Text

        FridayA = Fri1A + Fri2A + Fri3A + Fri4A + Fri5A
        txtFridayA.Text = FridayA

        If (TextBox115.Text < "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text < "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text < "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text < "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text < "") Then TextBox111.Text = "0.00"
    End Sub

    'Saturday
    Private Sub Label7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label7.Click
        SaturdaySelect()
    End Sub
    Private Sub Panel6_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel6.Click
        SaturdaySelect()
    End Sub
    Private Sub txtSaturdayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtSaturdayE.Click
        SaturdaySelect()
        If Panel13.Height() < 171 Then Panel13.Height() = 171 Else Panel13.Height() = 0
    End Sub
    Private Sub txtSaturdayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtSaturdayA.Click
        SaturdaySelect()
        If Panel20.Height() < 171 Then Panel20.Height() = 171 Else Panel20.Height() = 0
    End Sub
    Private Sub LinkLabel6_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel6.LinkClicked
        If LinkLabel6.Text = "More Detail" Then

            Panel13.Height() = 171
            Panel20.Height() = 171
            LinkLabel6.Text = "Less Detail"
        Else
            Panel13.Height() = 0
            Panel20.Height() = 0
            LinkLabel6.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel27_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel27.Click
        If (TextBox55.Text = "") Then TextBox55.Text = "0.00"
        If (TextBox54.Text = "") Then TextBox54.Text = "0.00"
        If (TextBox53.Text = "") Then TextBox53.Text = "0.00"
        If (TextBox52.Text = "") Then TextBox52.Text = "0.00"
        If (TextBox51.Text = "") Then TextBox51.Text = "0.00"

        Sat1E = TextBox55.Text
        Sat2E = TextBox54.Text
        Sat3E = TextBox53.Text
        Sat4E = TextBox52.Text
        Sat5E = TextBox51.Text

        SaturdayE = Sat1E + Sat2E + Sat3E + Sat4E + Sat5E
        txtSaturdayE.Text = SaturdayE

        If (TextBox55.Text < "") Then TextBox55.Text = "0.00"
        If (TextBox54.Text < "") Then TextBox54.Text = "0.00"
        If (TextBox53.Text < "") Then TextBox53.Text = "0.00"
        If (TextBox52.Text < "") Then TextBox52.Text = "0.00"
        If (TextBox51.Text < "") Then TextBox51.Text = "0.00"
    End Sub
    Private Sub LinkLabel34_Click(ByVal sender As Object, ByVal e As System.EventArgs)
        If (TextBox125.Text = "") Then TextBox125.Text = "0.00"
        If (TextBox124.Text = "") Then TextBox124.Text = "0.00"
        If (TextBox123.Text = "") Then TextBox123.Text = "0.00"
        If (TextBox122.Text = "") Then TextBox122.Text = "0.00"
        If (TextBox121.Text = "") Then TextBox121.Text = "0.00"

        Sat1A = TextBox125.Text
        Sat2A = TextBox124.Text
        Sat3A = TextBox123.Text
        Sat4A = TextBox122.Text
        Sat5A = TextBox121.Text

        SaturdayA = Sat1A + Sat2A + Sat3A + Sat4A + Sat5A
        txtSaturdayA.Text = SaturdayA

        If (TextBox125.Text < "") Then TextBox125.Text = "0.00"
        If (TextBox124.Text < "") Then TextBox124.Text = "0.00"
        If (TextBox123.Text < "") Then TextBox123.Text = "0.00"
        If (TextBox122.Text < "") Then TextBox122.Text = "0.00"
        If (TextBox121.Text < "") Then TextBox121.Text = "0.00"
    End Sub


    'Sunday
    Private Sub Label6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label6.Click
        SundaySelect()
    End Sub
    Private Sub Panel7_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel7.Click
        SundaySelect()
    End Sub
    Private Sub txtSundayE_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtSundayE.Click
        SundaySelect()
        If Panel14.Height() < 171 Then Panel14.Height() = 171 Else Panel14.Height() = 0
    End Sub
    Private Sub txtSundayA_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles txtSundayA.Click
        SundaySelect()
        If Panel21.Height() < 171 Then Panel21.Height() = 171 Else Panel21.Height() = 0
    End Sub
    Private Sub LinkLabel7_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel7.LinkClicked
        If LinkLabel7.Text = "More Detail" Then

            Panel14.Height() = 171
            Panel21.Height() = 171
            LinkLabel7.Text = "Less Detail"
        Else
            Panel14.Height() = 0
            Panel21.Height() = 0
            LinkLabel7.Text = "More Detail"
        End If
    End Sub
    Private Sub LinkLabel28_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel28.Click
        If (TextBox65.Text = "") Then TextBox65.Text = "0.00"
        If (TextBox64.Text = "") Then TextBox64.Text = "0.00"
        If (TextBox63.Text = "") Then TextBox63.Text = "0.00"
        If (TextBox62.Text = "") Then TextBox62.Text = "0.00"
        If (TextBox61.Text = "") Then TextBox61.Text = "0.00"

        Sun1E = TextBox65.Text
        Sun2E = TextBox64.Text
        Sun3E = TextBox63.Text
        Sun4E = TextBox62.Text
        Sun5E = TextBox61.Text

        SundayE = Sun1E + Sun2E + Sun3E + Sun4E + Sun5E
        txtSundayE.Text = SundayE

        If (TextBox65.Text < "") Then TextBox65.Text = "0.00"
        If (TextBox64.Text < "") Then TextBox64.Text = "0.00"
        If (TextBox63.Text < "") Then TextBox63.Text = "0.00"
        If (TextBox62.Text < "") Then TextBox62.Text = "0.00"
        If (TextBox61.Text < "") Then TextBox61.Text = "0.00"
    End Sub
    Private Sub LinkLabel35_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel35.Click
        If (TextBox135.Text = "") Then TextBox135.Text = "0.00"
        If (TextBox134.Text = "") Then TextBox134.Text = "0.00"
        If (TextBox133.Text = "") Then TextBox133.Text = "0.00"
        If (TextBox132.Text = "") Then TextBox132.Text = "0.00"
        If (TextBox131.Text = "") Then TextBox131.Text = "0.00"

        Sun1A = TextBox135.Text
        Sun2A = TextBox134.Text
        Sun3A = TextBox133.Text
        Sun4A = TextBox132.Text
        Sun5A = TextBox131.Text

        SundayA = Sun1A + Sun2A + Sun3A + Sun4A + Sun5A
        txtSundayA.Text = SundayA

        If (TextBox135.Text < "") Then TextBox135.Text = "0.00"
        If (TextBox134.Text < "") Then TextBox134.Text = "0.00"
        If (TextBox133.Text < "") Then TextBox133.Text = "0.00"
        If (TextBox132.Text < "") Then TextBox132.Text = "0.00"
        If (TextBox131.Text < "") Then TextBox131.Text = "0.00"
    End Sub

    Private Sub DateTimePicker1_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DateTimePicker1.ValueChanged
        CustomDate = DateTimePicker1.Value.Date.Month * 7 * 2 / 7 + 1
        GroupBox1.Text = "Date: " + DateTimePicker1.Text + "  (Week: " + Convert.ToString(Convert.ToInt16(DateTimePicker1.Value.DayOfYear / 7 + 1)) + ")"
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        DateTimePicker1.Text = Today
        GroupBox1.Text = "Date: " + DateTimePicker1.Text + "  (Week: " + Convert.ToString(Convert.ToInt16(DateTimePicker1.Value.DayOfYear / 7 + 1)) + ")"
    End Sub

    Private Sub CheckBox9_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox9.CheckedChanged
        If CheckBox9.Checked() = True Then DateTimePicker1.Enabled() = False Else DateTimePicker1.Enabled() = True
        If CheckBox9.Checked() = True Then Button10.Enabled() = False Else Button10.Enabled() = True
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Me.Close()
        Dialog1.Show()
    End Sub
    'File Save
    Private Sub SaveFileDialog1_FileOk(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk
        SaveFileDialog1.OpenFile()
    End Sub
    'File Open
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        OpenFileDialog1.ShowDialog()

    End Sub
    Private Sub OpenFileDialog1_FileOk(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles OpenFileDialog1.FileOk
        OpenFileDialog1.OpenFile()
    End Sub

    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        Button4.PerformClick()
    End Sub


    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged
        TextBox80.Text = TextBox1.Text
    End Sub

    Private Sub TextBox2_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox2.TextChanged
        TextBox79.Text = TextBox2.Text
    End Sub

    Private Sub TextBox3_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox3.TextChanged
        TextBox78.Text = TextBox3.Text
    End Sub

    Private Sub TextBox4_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox4.TextChanged
        TextBox77.Text = TextBox4.Text
    End Sub

    Private Sub TextBox20_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox20.TextChanged
        TextBox90.Text = TextBox20.Text
    End Sub

    Private Sub TextBox19_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox19.TextChanged
        TextBox89.Text = TextBox19.Text
    End Sub

    Private Sub TextBox18_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox18.TextChanged
        TextBox88.Text = TextBox18.Text
    End Sub
    Private Sub TextBox17_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox17.TextChanged
        TextBox87.Text = TextBox17.Text
    End Sub
    Private Sub TextBox30_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox30.TextChanged
        TextBox100.Text = TextBox30.Text
    End Sub
    Private Sub TextBox29_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox29.TextChanged
        TextBox99.Text = TextBox29.Text
    End Sub
    Private Sub TextBox28_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox28.TextChanged
        TextBox98.Text = TextBox28.Text
    End Sub
    Private Sub TextBox27_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox27.TextChanged
        TextBox97.Text = TextBox27.Text
    End Sub
    Private Sub TextBox40_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox40.TextChanged
        TextBox110.Text = TextBox40.Text
    End Sub
    Private Sub TextBox39_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox39.TextChanged
        TextBox109.Text = TextBox39.Text
    End Sub
    Private Sub TextBox38_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox38.TextChanged
        TextBox108.Text = TextBox38.Text
    End Sub
    Private Sub TextBox37_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox38.TextChanged
        TextBox107.Text = TextBox37.Text
    End Sub
    Private Sub TextBox50_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox50.TextChanged
        TextBox120.Text = TextBox50.Text
    End Sub
    Private Sub TextBox49_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox49.TextChanged
        TextBox119.Text = TextBox49.Text
    End Sub
    Private Sub TextBox48_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox48.TextChanged
        TextBox118.Text = TextBox48.Text
    End Sub
    Private Sub TextBox47_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox47.TextChanged
        TextBox117.Text = TextBox47.Text
    End Sub
    Private Sub TextBox60_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox60.TextChanged
        TextBox130.Text = TextBox60.Text
    End Sub
    Private Sub TextBox59_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox59.TextChanged
        TextBox129.Text = TextBox59.Text
    End Sub
    Private Sub TextBox58_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox58.TextChanged
        TextBox128.Text = TextBox58.Text
    End Sub
    Private Sub TextBox57_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox57.TextChanged
        TextBox127.Text = TextBox57.Text
    End Sub
    Private Sub TextBox70_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox70.TextChanged
        TextBox140.Text = TextBox70.Text
    End Sub
    Private Sub TextBox69_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox69.TextChanged
        TextBox139.Text = TextBox69.Text
    End Sub
    Private Sub TextBox68_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox68.TextChanged
        TextBox138.Text = TextBox68.Text
    End Sub
    Private Sub TextBox67_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox67.TextChanged
        TextBox137.Text = TextBox67.Text
    End Sub
    Private Sub TextBox80_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox80.TextChanged
        TextBox1.Text = TextBox80.Text
    End Sub

    Private Sub TextBox79_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox79.TextChanged
        TextBox2.Text = TextBox79.Text
    End Sub

    Private Sub TextBox78_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox78.TextChanged
        TextBox3.Text = TextBox78.Text
    End Sub

    Private Sub TextBox77_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox77.TextChanged
        TextBox4.Text = TextBox77.Text
    End Sub

    Private Sub TextBox90_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox90.TextChanged
        TextBox20.Text = TextBox90.Text
    End Sub

    Private Sub TextBox89_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox89.TextChanged
        TextBox19.Text = TextBox89.Text
    End Sub

    Private Sub TextBox88_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox88.TextChanged
        TextBox18.Text = TextBox88.Text
    End Sub
    Private Sub TextBox87_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox87.TextChanged
        TextBox17.Text = TextBox87.Text
    End Sub
    Private Sub TextBox100_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox100.TextChanged
        TextBox30.Text = TextBox100.Text
    End Sub
    Private Sub TextBox99_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox99.TextChanged
        TextBox29.Text = TextBox99.Text
    End Sub
    Private Sub TextBox98_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox98.TextChanged
        TextBox28.Text = TextBox98.Text
    End Sub
    Private Sub TextBox97_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox97.TextChanged
        TextBox27.Text = TextBox97.Text
    End Sub
    Private Sub TextBox110_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox110.TextChanged
        TextBox40.Text = TextBox110.Text
    End Sub
    Private Sub TextBox109_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox109.TextChanged
        TextBox39.Text = TextBox109.Text
    End Sub
    Private Sub TextBox108_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox108.TextChanged
        TextBox38.Text = TextBox108.Text
    End Sub
    Private Sub TextBox107_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox108.TextChanged
        TextBox37.Text = TextBox107.Text
    End Sub
    Private Sub TextBox120_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox120.TextChanged
        TextBox50.Text = TextBox120.Text
    End Sub
    Private Sub TextBox119_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox119.TextChanged
        TextBox49.Text = TextBox119.Text
    End Sub
    Private Sub TextBox118_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox118.TextChanged
        TextBox48.Text = TextBox118.Text
    End Sub
    Private Sub TextBox117_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox117.TextChanged
        TextBox47.Text = TextBox117.Text
    End Sub
    Private Sub TextBox130_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox130.TextChanged
        TextBox60.Text = TextBox130.Text
    End Sub
    Private Sub TextBox129_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox129.TextChanged
        TextBox59.Text = TextBox129.Text
    End Sub
    Private Sub TextBox128_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox128.TextChanged
        TextBox58.Text = TextBox128.Text
    End Sub
    Private Sub TextBox127_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox127.TextChanged
        TextBox57.Text = TextBox127.Text
    End Sub
    Private Sub TextBox140_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox140.TextChanged
        TextBox70.Text = TextBox140.Text
    End Sub
    Private Sub TextBox139_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox139.TextChanged
        TextBox69.Text = TextBox139.Text
    End Sub
    Private Sub TextBox138_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox138.TextChanged
        TextBox68.Text = TextBox138.Text
    End Sub
    Private Sub TextBox137_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles TextBox137.TextChanged
        TextBox67.Text = TextBox137.Text
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        PrintDialog1.ShowDialog()
    End Sub

    Private Sub LinkLabel34_Click1(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel34.Click
        If (TextBox115.Text < "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text < "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text < "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text < "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text < "") Then TextBox111.Text = "0.00"

        If (TextBox125.Text = "") Then TextBox125.Text = "0.00"
        If (TextBox124.Text = "") Then TextBox124.Text = "0.00"
        If (TextBox123.Text = "") Then TextBox123.Text = "0.00"
        If (TextBox122.Text = "") Then TextBox122.Text = "0.00"
        If (TextBox121.Text = "") Then TextBox121.Text = "0.00"

        Sat1A = TextBox125.Text
        Sat2A = TextBox124.Text
        Sat3A = TextBox123.Text
        Sat4A = TextBox122.Text
        Sat5A = TextBox121.Text

        SaturdayA = Sat1A + Sat2A + Sat3A + Sat4A + Sat5A
        txtSaturdayA.Text = SaturdayA
    End Sub
    Private Sub LinkLabel33_Click1(ByVal sender As Object, ByVal e As System.EventArgs) Handles LinkLabel33.Click
        If (TextBox115.Text = "") Then TextBox115.Text = "0.00"
        If (TextBox114.Text = "") Then TextBox114.Text = "0.00"
        If (TextBox113.Text = "") Then TextBox113.Text = "0.00"
        If (TextBox112.Text = "") Then TextBox112.Text = "0.00"
        If (TextBox111.Text = "") Then TextBox111.Text = "0.00"

        Fri1A = TextBox115.Text
        Fri2A = TextBox114.Text
        Fri3A = TextBox113.Text
        Fri4A = TextBox112.Text
        Fri5A = TextBox111.Text

        FridayA = Fri1A + Fri2A + Fri3A + Fri4A + Fri5A
        txtFridayA.Text = FridayA

    End Sub
End Class