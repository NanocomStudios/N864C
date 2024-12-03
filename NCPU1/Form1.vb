Public Class Form1
    Dim tick As Integer = 0
    Dim ctick = 0
    Dim ram(65536) As Byte
    Dim reader As Byte = 0
    Dim i As Integer = 0
    Dim asl As Boolean = False
    Dim eq As Boolean = False

    Dim db As Byte                  'Data Bus
    Dim AHI As Boolean = False      'Address High Byte Output
    Dim ALI As Boolean = False
    Dim ALO As Boolean = False
    Dim ABO As Boolean = False
    Dim ABI As Boolean = False
    Dim AO As Boolean = False
    Dim XO As Boolean = False
    Dim YO As Boolean = False
    Dim AI As Boolean = False
    Dim XI As Boolean = False
    Dim YI As Boolean = False
    Dim SI As Boolean = False       'Stack in
    Dim SO As Boolean = False       'Stack out
    Dim INC As Boolean = False
    Dim DEC As Boolean = False
    Dim CFS As Boolean = False
    Dim ZFS As Boolean = False
    Dim CFC As Boolean = False
    Dim ZFC As Boolean = False
    Dim PCINC As Boolean = False
    Dim ABINC As Boolean = False
    Dim SPINC As Boolean = False
    Dim SPDEC As Boolean = False
    Dim RST As Boolean = False
    Dim HALT As Boolean = False
    Dim CTH As Boolean = False
    Dim PCINCE As Boolean = False
    Dim PCO As Boolean = False
    Dim OPTI As Boolean = False
    Dim OPTE As Boolean = False
    Dim TPI As Boolean = False      'Tempory byte in
    Dim ZERO As Boolean = False     '0 in data bus
    Dim ONE As Boolean = False      '1 in data bus
    Dim SPO As Boolean = False      'Stack pointer out
    Dim SPI As Boolean = False      'Stack pointer in
    Dim SPINCE As Boolean = False
    Dim SPDECE As Boolean = False
    Dim PCI As Boolean = False
    Dim CPI As Boolean = False
    Dim ROR As Boolean = False
    Dim ROL As Boolean = False


    Sub timer()
        tick = Val(tc_tb.Text)
        If tick Mod 2 = 0 Then
            CT()
            tc_tb.Text = 1
        Else

            AT()
            If OPTE = True Then
                ctick = Tick_EEPROM(Val(opt_tb.Text))
            End If
            add_tb.Text = Val(temp_tb.Text) + Val(a_tb.Text)

            If Val(add_tb.Text) > 255 Then
                add_tb.Text = Val(add_tb.Text) - 256
                c_tb.Text = "1"
                z_tb.Text = "0"
            End If

            sub_tb.Text = Val(a_tb.Text) - Val(temp_tb.Text)

            If Val(sub_tb.Text) < 0 Then
                sub_tb.Text = Val(sub_tb.Text) + 256
                z_tb.Text = "1"
                c_tb.Text = "0"
            End If

            and_tb.Text = Val(temp_tb.Text) And Val(a_tb.Text)
            or_tb.Text = Val(temp_tb.Text) Or Val(a_tb.Text)
            xor_tb.Text = Val(temp_tb.Text) Xor Val(a_tb.Text)

            If (Val(a_tb.Text) * 2) > 255 Then
                rol_tb.Text = (Val(a_tb.Text) * 2) - 256
                asl = True
            Else
                rol_tb.Text = Val(a_tb.Text) * 2
                asl = False
            End If

            ror_tb.Text = Val(a_tb.Text) \ 2

            tc_tb.Text = 0
        End If

    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        timer()
    End Sub

    Sub CT()
        reset()
        If PCINC = True Then
            If pc_tb.Text = "65535" Then
            Else
                pc_tb.Text = Val(pc_tb.Text) + 1
            End If

            PCINC = False
            PCINCE = False
        End If

        If SPINC = True Then
            If sp_tb.Text = 0 Then
                sp_tb.Text = "255"
            Else
                sp_tb.Text = Val(sp_tb.Text) + 1
            End If
            SPINC = False
            SPINCE = False
        End If

        If SPDEC = True Then
            If sp_tb.Text = 255 Then
                sp_tb.Text = "0"
            Else
                sp_tb.Text = Val(sp_tb.Text) - 1
            End If
            SPDEC = False
            SPDECE = False
        End If

        If CTH = True Then

            If i = ctick Then

                Select Case Val(opt_tb.Text)
                    Case 1
                        ABO = True
                        AI = True
                    Case 2
                        ABO = True
                        AI = True
                    Case 3
                        ALO = True
                        AI = True
                    Case 4
                        AO = True
                        ABI = True
                    Case 5
                        AO = True
                        ABI = True
                    Case 6
                        ABO = True
                        XI = True
                    Case 7
                        ABO = True
                        XI = True
                    Case 8
                        ALO = True
                        XI = True
                    Case 9
                        XO = True
                        ABI = True
                    Case 10
                        XO = True
                        ABI = True
                    Case 11
                        ABO = True
                        YI = True
                    Case 12
                        ABO = True
                        YI = True
                    Case 13
                        ALO = True
                        YI = True
                    Case 14
                        YO = True
                        ABI = True
                    Case 15
                        YO = True
                        ABI = True
                    Case 16
                        AO = True
                        XI = True
                    Case 17
                        AO = True
                        YI = True
                    Case 18
                        XO = True
                        AI = True
                    Case 19
                        XO = True
                        YI = True
                    Case 20
                        YO = True
                        AI = True
                    Case 21
                        YO = True
                        XI = True
                    Case 22
                        AO = True
                        TPI = True
                    Case 23
                        ZERO = True
                        ABI = True
                    Case 24
                        ZERO = True
                        ABI = True
                    Case 25
                        SPO = True
                        XI = True
                    Case 26
                        XO = True
                        SPI = True
                    Case 27
                        AO = True
                        SI = True
                    Case 28
                        XO = True
                        SI = True
                    Case 29
                        YO = True
                        SI = True
                    Case 30
                        SO = True
                        AI = True
                    Case 31
                        SO = True
                        XI = True
                    Case 32
                        SO = True
                        YI = True
                    Case 33
                        RST = True
                    Case 34
                        CFS = True
                    Case 35
                        CFC = True


                    Case 42
                        INC = True
                        AI = True
                    Case 43
                        DEC = True
                        AI = True

                    Case 44
                        ABO = True
                        CPI = True
                        If cmp_tb.Text = a_tb.Text Then
                            eq = True
                        Else
                            eq = False
                        End If

                    Case 45
                        ABO = True
                        CPI = True
                        If cmp_tb.Text = a_tb.Text Then
                            eq = True
                        Else
                            eq = False
                        End If

                    Case 50
                        If c_tb.Text = "0" Then
                            PCI = True
                        End If
                    Case 51
                        If c_tb.Text = "1" Then
                            PCI = True
                        End If
                    Case 52
                        If eq = True Then
                            eq_tb.Text = "1"
                        Else
                            eq_tb.Text = "0"
                        End If
                        If eq_tb.Text = "1" Then
                            PCI = True
                            eq_tb.Text = "0"
                            cmp_tb.Text = "0"
                        End If
                    Case 53
                        If eq_tb.Text = "0" Then
                            PCI = True
                            eq_tb.Text = "0"
                        End If
                    Case 54
                        PCI = True

                    Case 60
                        ROL = True
                        AI = True
                    Case 61
                        ROR = True
                        AI = True

                    Case 62
                        HALT = True

                    Case Else
                        reset()

                End Select
                CTH = False
                i = 0

            Else
                If i = 0 Then
                    ah_tb.Text = ""
                    PCINCE = True
                    PCO = True
                    ALI = True
                    i += 1

                ElseIf i = 1 Then
                    If opt_tb.Text = 50 Then
                        If c_tb.Text = "0" Then
                            PCINCE = True
                        End If
                    ElseIf opt_tb.Text = 51 Then
                        If c_tb.Text = "1" Then
                            PCINCE = True
                        End If
                    ElseIf opt_tb.Text = 52 Then
                        If eq = False Then
                            PCINCE = True
                        End If

                    ElseIf opt_tb.Text = 53 Then
                        If eq = True Then
                            PCINCE = True
                        End If

                    ElseIf opt_tb.Text = 54 Then
                    Else
                        PCINCE = True
                    End If
                    PCO = True
                    AHI = True
                    i += 1
                End If
            End If

        Else

            PCO = True
            OPTI = True
            OPTE = True
            CTH = True


            PCINCE = True
        End If


    End Sub

    Sub AT()
        '----------------------------------------
        If ROR = True Then
            db_tb.Text = ror_tb.Text
        End If
        '----------------------------------------
        If ROL = True Then
            db_tb.Text = rol_tb.Text
        End If
        '----------------------------------------
        If ZERO = True Then
            db_tb.Text = "0"
        End If
        '----------------------------------------
        If ONE = True Then
            db_tb.Text = "1"
        End If
        '----------------------------------------
        If ALO = True Then
            db_tb.Text = al_tb.Text
        End If
        '----------------------------------------
        If SO = True Then
            db_tb.Text = ram(Val(sp_tb.Text) + 255)
            SPDEC = True
        End If
        '----------------------------------------
        If SPO = True Then
            db_tb.Text = sp_tb.Text
        End If
        '----------------------------------------
        If PCO = True Then
            db_tb.Text = ram(Val(pc_tb.Text))
        End If
        '----------------------------------------
        If INC = True Then
            db_tb.Text = add_tb.Text
        End If
        '----------------------------------------
        If DEC = True Then
            db_tb.Text = sub_tb.Text
        End If
        '----------------------------------------
        If ABO = True Then
            db_tb.Text = ram(Val(ab_tb.Text))
        End If
        '----------------------------------------
        If AO = True Then
            db_tb.Text = a_tb.Text
        End If
        '----------------------------------------
        If XO = True Then
            db_tb.Text = x_tb.Text
        End If
        '----------------------------------------
        If YO = True Then
            db_tb.Text = y_tb.Text
        End If
        '----------------------------------------
        If AI = True Then
            a_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If XI = True Then
            x_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If YI = True Then
            y_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If ABI = True Then
            ram(Val(ab_tb.Text)) = Val(db_tb.Text)
        End If
        '----------------------------------------
        If CFS = True Then
            c_tb.Text = 1
        End If
        '----------------------------------------
        If ZFS = True Then
            z_tb.Text = 1
        End If
        '----------------------------------------
        If CFC = True Then
            c_tb.Text = 0
        End If
        '----------------------------------------
        If ZFC = True Then
            z_tb.Text = 0
        End If
        '----------------------------------------
        If PCINCE = True Then
            PCINC = True
        End If
        '----------------------------------------
        If ABINC = True Then
            ab_tb.Text = Val(ab_tb.Text) + 1
        End If
        '----------------------------------------
        If RST = True Then
            reset_mem()
        End If
        '----------------------------------------
        If HALT = True Then
            Timer1.Enabled = False
        End If
        '----------------------------------------
        If AHI = True Then
            ah_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If ALI = True Then
            al_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If OPTI = True Then
            opt_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If TPI = True Then
            temp_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If SPI = True Then
            sp_tb.Text = db_tb.Text
        End If
        '----------------------------------------
        If SPINCE = True Then
            SPINC = True
        End If
        '----------------------------------------
        If SPDECE = True Then
            SPDEC = True
        End If
        '----------------------------------------
        If SI = True Then
            ram(Val(sp_tb.Text) + 256) = db_tb.Text
            SPINCE = True
        End If
        '----------------------------------------
        If PCI = True Then
            pc_tb.Text = Val(ab_tb.Text)
        End If
        '----------------------------------------
        If CPI = True Then
            cmp_tb.Text = Val(db_tb.Text)
        End If
        '----------------------------------------



    End Sub

    Sub IRQ()

    End Sub

    Sub init()

    End Sub


    Private Sub db_tb_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles db_tb.TextChanged

    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ram(32768) = 250
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        OpenFileDialog1.ShowDialog()
        If OpenFileDialog1.FileName = "" Then

        Else

            Dim temprom() As Byte
            temprom = My.Computer.FileSystem.ReadAllBytes(OpenFileDialog1.FileName)
            Dim i As Integer = 0
            Dim len As Integer

            If temprom.Length > 32768 Then
                MsgBox("Memory Overloaded")
            Else
                len = temprom.Length

                Do Until i >= len
                    ram(i + 32768) = temprom(i)
                    i += 1
                Loop

                reset_mem()

                Timer1.Enabled = CheckBox1.Checked
            End If

            
        End If
    End Sub



    Function Tick_EEPROM(ByVal inp As Byte) As Byte
        Dim tmp As Byte
        Select Case inp
            Case 1
                tmp = 1
            Case 2
                tmp = 2
            Case 3
                tmp = 1
            Case 4
                tmp = 1
            Case 5
                tmp = 2
            Case 6
                tmp = 1
            Case 7
                tmp = 2
            Case 8
                tmp = 1
            Case 9
                tmp = 1
            Case 10
                tmp = 2
            Case 11
                tmp = 1
            Case 12
                tmp = 2
            Case 13
                tmp = 1
            Case 14
                tmp = 1
            Case 15
                tmp = 2
            Case 16
                tmp = 0
            Case 17
                tmp = 0
            Case 18
                tmp = 0
            Case 19
                tmp = 0
            Case 20
                tmp = 0
            Case 21
                tmp = 0
            Case 22
                tmp = 0
            Case 23
                tmp = 1
            Case 24
                tmp = 2
            Case 25
                tmp = 0
            Case 26
                tmp = 0
            Case 27
                tmp = 0
            Case 28
                tmp = 0
            Case 29
                tmp = 0
            Case 30
                tmp = 0
            Case 31
                tmp = 0
            Case 32
                tmp = 0

            Case 33
                tmp = 0
            Case 34
                tmp = 0
            Case 35
                tmp = 0
            Case 36
                tmp = 1
            Case 37
                tmp = 2
            Case 38
                tmp = 1
            Case 39
                tmp = 2
            Case 40
                tmp = 1
            Case 41
                tmp = 2
            Case 42
                tmp = 0
            Case 43
                tmp = 0
            Case 44
                tmp = 1
            Case 45
                tmp = 2

            Case 50
                tmp = 2
            Case 51
                tmp = 2
            Case 52
                tmp = 2
            Case 53
                tmp = 2
            Case 54
                tmp = 2

            Case 60
                tmp = 0
            Case 61
                tmp = 0
            Case 62
                tmp = 0

            Case Else

        End Select
        Return tmp
    End Function


    Private Sub al_tb_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles al_tb.TextChanged
        Dim ah As String
        Dim al As String

        If Val(ah_tb.Text) <= 15 Then
            ah = "0" & Hex(Val(ah_tb.Text))
        Else
            ah = Hex(Val(ah_tb.Text))
        End If

        If Val(al_tb.Text) <= 15 Then
            al = "0" & Hex(Val(al_tb.Text))
        Else
            al = Hex(Val(al_tb.Text))
        End If


        ab_tb.Text = Convert.ToInt32(ah & al, 16)
    End Sub

    Private Sub ah_tb_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ah_tb.TextChanged
        Dim ah As String
        Dim al As String

        If Val(ah_tb.Text) <= 15 Then
            ah = "0" & Hex(Val(ah_tb.Text))
        Else
            ah = Hex(Val(ah_tb.Text))
        End If

        If Val(al_tb.Text) <= 15 Then
            al = "0" & Hex(Val(al_tb.Text))
        Else
            al = Hex(Val(al_tb.Text))
        End If


        ab_tb.Text = Convert.ToInt32(ah & al, 16)
    End Sub

    Sub reset_mem()
        tc_tb.Text = "0"
        db_tb.Text = "0"
        al_tb.Text = "0"
        ah_tb.Text = "0"
        a_tb.Text = "0"
        x_tb.Text = "0"
        y_tb.Text = "0"
        temp_tb.Text = "0"
        pc_tb.Text = "32768"
        tick = 0
    End Sub
    Sub reset()
        ABO = False
        ABI = False
        AO = False
        XO = False
        YO = False
        AI = False
        XI = False
        YI = False
        INC = False
        DEC = False
        CFS = False
        ZFS = False
        CFC = False
        ZFC = False
        ABINC = False
        RST = False
        HALT = False
        PCO = False
        AHI = False
        ALI = False
        OPTI = False
        OPTE = False
        ALO = False
        TPI = False
        ZERO = False
        ONE = False
        SPO = False
        SPI = False
        SO = False
        SI = False
        PCI = False
        CPI = False
        ROR = False
        ROL = False
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        db_tb.Text = ram(Val(TextBox1.Text))
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        timer()
    End Sub

    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox1.CheckedChanged

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        If Timer1.Enabled = True Then
            Timer1.Enabled = False
        Else
            Timer1.Enabled = True
        End If
    End Sub


End Class
