﻿'Imports System.Text.RegularExpressions

Imports System.Text.RegularExpressions

Public Class Form1


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If (TextBox4.Text.Length = 0 And Regex.IsMatch(TextBox4.Text.Trim, "\A-{0,1}[0-9.]*\Z")) Then
            MessageBox.Show("Enter message into Chaos input box")
            Return
        End If

        key.KeyF(TextBox4.Text)
        SboxClass.FillSBOXES()
        If (TextBox1.Text.Length > 0) Then
            Dim _mesageBin = readSplit(TextBox1.Text)
            _mesageBin = Encrypt(_mesageBin)
            TextBox2.Text = BtoC(_mesageBin)



            _mesageBin = Decrypt(TextBox2.Text)
            TextBox3.Text = BtoC(_mesageBin)
        Else
            MessageBox.Show("Enter message into Input message box")
        End If




    End Sub

    Function Encrypt(_mesageBin As String)

        MessageBox.Show("Your Message is:" & vbLf & BtoC(_mesageBin))

        Dim Tab_P_Box = Numbers_Generate(32, 200, "TableIP", True)
        Dim spliterParts As String = ""
        Dim Stack As String = ""
        Dim left
        Dim right

        For j = 1 To _mesageBin.Length Step 64

            spliterParts = Mid(_mesageBin, j, 64)

            left = Mid(spliterParts, 1, 32)
            right = Mid(spliterParts, 33, 32)

            For i = 1 To 16

                '1 Expand
                right = splitit(right)

                '2 Xor
                right = XorOperation(right, key.Key(i - 1), 48)
                right = splitit(right)

                '3 sbox
                right = SboxClass.RealProcess(right, i)


                '4 P_Boxes
                right = Ip_Procces_Encryption(Tab_P_Box, right)

                left = XorOperation(left, right, 32)

                spliterParts = right & left
                left = Mid(spliterParts, 1, 32)
                right = Mid(spliterParts, 33, 32)

            Next
            Stack &= spliterParts
        Next

        MessageBox.Show("Your Encrypted Message is:" & vbLf & BtoC(Stack))

        Return Stack
    End Function

    Function Decrypt(_mesageBin As String)
        Dim message = readSplit(_mesageBin)
        Dim Tab_P_Box = Numbers_Generate(32, 200, "TableIP", True)
        Dim spliterParts As String = ""
        Dim decrypter As String = ""
        Dim left
        Dim right

        For j = 1 To message.Length Step 64

            spliterParts = Mid(message, j, 64)

            left = Mid(spliterParts, 1, 32)
            right = Mid(spliterParts, 33, 32)

            For i = 16 To 1 Step -1
                'Decrypt
                spliterParts = right & left
                left = Mid(spliterParts, 1, 32)
                right = Mid(spliterParts, 33, 32)

                left = XorOperation(left, right, 32)
                right = INV_Order(Tab_P_Box, right)
                right = SboxClass.RealProcess(right, i, True)

                right = splitit(right)
                right = XorOperation(right, key.Key(i - 1), 48)
                right = splitit(right)



                '     RichTextBox1.Text += BtoC(Left & Right) & vbLf
            Next
            decrypter &= left & right
        Next

        MessageBox.Show("Your Decrypted Message is:" & vbLf & BtoC(decrypter))
        Return decrypter
    End Function

    Function readSplit(input As String)

        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""
        Dim MyOrgString = input

        While MyOrgString.Length Mod 8 <> 0 OrElse MyOrgString.Length = 0
            MyOrgString += " "
        End While

        For i = 1 To MyOrgString.Length()
            spliter = Mid(MyOrgString, i, 1)
            AscValue = Asc(spliter)
            stored += DtoB(AscValue, 8)
        Next


        Return stored
    End Function


    Function DtoB(val As Integer, LenthOfReturnValue As Integer)
        Dim stack As String = ""

        While val <> 0
            If val Mod 2 = 0 Then
                stack = "0" + stack
            Else
                stack = "1" + stack
            End If
            val = val \ 2
        End While
        While stack.Length < LenthOfReturnValue
            stack = "0" + stack
        End While

        Return stack
    End Function

    Function BtoC(val As String)

        Dim ArrString(val.Length) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(8) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To val.Length Step 8
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(val, i, 8)
            For j = 1 To 8
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To 8
                StackInteger += Math.Pow(2, 8 - j) * splitbit(j)
            Next
            DtoC += Chr(StackInteger)
        Next


        Return DtoC
    End Function

    Function splitit(EncryptedBinary As String)

        Dim Concate As String = ""

        If EncryptedBinary.Length = 32 Then

            Dim ExpandRight(48) As String

            Dim tabExpand() = {0, 1, 2, 3, 2, 3, 4, 5, 6, 7, 6, 7, 8, 9,
                          10, 11, 10, 11, 12, 13, 14, 15, 14, 15, 16,
                          17, 18, 19, 18, 19, 20, 21, 22, 23, 22, 23,
                          24, 25, 26, 27, 26, 27, 28, 29, 30, 31, 30, 31}

            For i = 0 To tabExpand.Length - 1
                ExpandRight(i) = EncryptedBinary(tabExpand(i))
            Next

            Concate = Join(ExpandRight, "")
        ElseIf EncryptedBinary.Length = 48 Then

            Dim ExpandRight(32) As String

            Dim InvExpand = {0, 1, 2, 3, 6, 7, 8, 9, 12, 13, 14, 15,
                 18, 19, 20, 21, 24, 25, 26, 27, 30, 31,
                 32, 33, 36, 37, 38, 39, 42, 43, 44, 45}


            For i = 0 To InvExpand.Length - 1
                ExpandRight(i) = EncryptedBinary(InvExpand(i))
            Next
            Concate = Join(ExpandRight, "")
        End If


        Return Concate
    End Function



    Function XorOperation(First As String, Second As String, IndexValue As Integer)


        Dim XorResult(IndexValue) As String
        For i = 0 To IndexValue - 1
            XorResult(i) = Convert.ToString(Convert.ToInt32(First(i)) Xor Convert.ToInt32(Second(i)))
        Next
        Return Join(XorResult, "")
    End Function

    Function SplitAs8(str As String, TableSBox(,) As Integer)

        Dim Chunks(7) As String
        Dim chunksReplace(7) As String
        Dim counter As Integer = 0


        Dim spliterRow As String = ""
        Dim spliterColumn As String = ""

        Dim rows(15) As String
        Dim columns(15) As String

        Dim spliterRowInt As Integer = 0
        Dim spliterColumnInt As Integer = 0

        For i = 1 To str.Length Step 4

            Chunks(counter) = Mid(str, i, 4)

            spliterRow = Mid(Chunks(counter), 1, 2)
            spliterRowInt = JustConvertToD(spliterRow)
            spliterColumn = Mid(Chunks(counter), 3, 2)
            spliterColumnInt = JustConvertToD(spliterColumn)

            rows(counter) = spliterRowInt
            columns(counter) = spliterColumnInt

            chunksReplace(counter) = TableSBox(spliterColumnInt, spliterRowInt)
            counter += 1


        Next

        Dim FinalSide As String = ""
        For i = 0 To chunksReplace.Length - 1
            FinalSide += DtoB(chunksReplace(i), 4)
        Next

        Return FinalSide
    End Function



    Function D_SplitSBOX(EChunks As String, TableSBox(,) As Integer)
        Dim Chunks = SplitString(EChunks, 4)

        Dim equal_ChunkDecimal As Integer

        Dim DChunks(7) As String
        Dim counter As Integer = 0


        Dim spliterRow As String = ""
        Dim spliterColumn As String = ""

        Dim rows(15) As String
        Dim columns(15) As String

        Dim spliterRowInt As Integer = 0
        Dim spliterColumnInt As Integer = 0

        For i = 0 To Chunks.Length - 1
            equal_ChunkDecimal = JustConvertToD(Chunks(i))
            For r = 0 To 3
                For c = 0 To 3
                    If equal_ChunkDecimal = TableSBox(r, c) Then
                        spliterRowInt = r
                        spliterColumnInt = c

                        spliterRow = DtoB(spliterRowInt, 2)
                        spliterColumn = DtoB(spliterColumnInt, 2)
                        DChunks(i) = spliterColumn & spliterRow
                    End If
                Next
            Next
        Next

        Return Join(DChunks, "")
    End Function

    Function SplitString(TheString As String, StringLen As Integer) As String()
        Dim ArrCount As Integer  'as it is declared locally, it will automatically reset to 0 when this is called again
        Dim I As Long  'we are going to use it.. so declare it (with local scope to avoid breaking other code)
        Dim TempArray() As String
        ReDim TempArray((Len(TheString) - 1) \ StringLen)
        For I = 1 To Len(TheString) Step StringLen
            TempArray(ArrCount) = Mid$(TheString, I, StringLen)
            ArrCount = ArrCount + 1
        Next
        SplitString = TempArray   'actually return the value
    End Function

    Function JustConvertToD(input As String)
        Dim stackInteger As Integer
        For i = 1 To input.Length
            stackInteger += Convert.ToInt32(Mid(input, i, 1)) * Math.Pow(2, input.Length - i)
        Next
        Return stackInteger

    End Function

    Function Numbers_Generate(IndexPlusOneArray As Integer, startFrom As Integer, Addres As String, Optional TakeModIndexOfArray As Boolean = False, Optional twoArray As Boolean = False)
        'IP_Table_Generate

        Dim reader As String = MyFile()
        Dim E_table(IndexPlusOneArray - 1) As String
        Dim counter As Integer = 0
        For i = startFrom To reader.Length Step 7



            If TakeModIndexOfArray Then

                If Not E_table.Contains(JustConvertToD(Mid(reader, i, 7)) Mod IndexPlusOneArray) Then
                    E_table(counter) = JustConvertToD(Mid(reader, i, 7)) Mod IndexPlusOneArray
                    counter += 1
                Else
                    GoTo skip
                End If
            Else

                If Not E_table.Contains(JustConvertToD(Mid(reader, i, 7))) Then
                    E_table(counter) = JustConvertToD(Mid(reader, i, 7))
                    counter += 1
                Else
                    GoTo skip
                End If

            End If


            If counter = IndexPlusOneArray Then
                Exit For
            End If
skip:
        Next


        If twoArray Then
            Dim TwoDArray(3, 3) As Integer
            Dim c As Integer = 0
            Dim k = TwoDArray.GetLength(0)
            Dim m = TwoDArray.GetLength(1)
            For i = 0 To TwoDArray.GetLength(0) - 1
                For j = 0 To TwoDArray.GetLength(1) - 1
                    TwoDArray(i, j) = Convert.ToInt32(E_table(c))
                    c += 1
                Next
            Next
            'E_table is my Encryption Table
            Return TwoDArray
        Else
            Return E_table
        End If

    End Function

    Function Ip_Procces_Encryption(E_table() As String, str As String)
        Dim E_Procces(str.Length) As String
        For i = 1 To str.Length
            E_Procces(E_table(i - 1)) = Mid(str, i, 1)
        Next
        Return Join(E_Procces, "")
    End Function

    Function INV_Order(table() As String, EncryptedBits As String)
        Dim inv_Table(EncryptedBits.Length)
        For i = 0 To table.Length - 1
            inv_Table(table(i)) = i

        Next

        Dim EncryptedBits_char() As Char = EncryptedBits.ToCharArray
        Dim DecryptBits(EncryptedBits.Length) As String
        Dim team
        For i = 0 To EncryptedBits.Length - 1
            team = EncryptedBits(i)
            DecryptBits(inv_Table(i)) = team

        Next


        Return Join(DecryptBits, "")
    End Function

    Function MyFile()
        Dim reader As String = System.IO.File.ReadAllText("_file22222 - Copy.txt")
        Return reader
    End Function

    Function shifting(MyString As String, NumberOfShinftingLoop As Integer)
        Dim EditOnMyString(MyString.Length) As String

        For i = 0 To MyString.Length - 1
            EditOnMyString(i) = MyString(i)
        Next


        Dim TheFirstBit As String = ""

        For i = 1 To NumberOfShinftingLoop
            TheFirstBit = EditOnMyString(0)

            For j = 1 To EditOnMyString.Length - 1
                EditOnMyString(j - 1) = EditOnMyString(j)
            Next
            EditOnMyString(EditOnMyString.Length - 1) = TheFirstBit
        Next
        Return Join(EditOnMyString, "")
    End Function


End Class

Class SboxClass
    Public Shared sbox1(,) As Integer

    Public Shared sbox2(,) As Integer

    Public Shared sbox3(,) As Integer

    Public Shared sbox4(,) As Integer

    Public Shared sbox5(,) As Integer

    Public Shared sbox6(,) As Integer

    Public Shared sbox7(,) As Integer

    Public Shared sbox8(,) As Integer

    Public Shared Function FillSBOXES()
        Dim SboxLength As Integer = 16
        sbox1 = Form1.Numbers_Generate(SboxLength, 700, "sbox1", True, True)
        sbox2 = Form1.Numbers_Generate(SboxLength, 1000, "sbox2", True, True)
        sbox3 = Form1.Numbers_Generate(SboxLength, 13000, "sbox3", True, True)
        sbox4 = Form1.Numbers_Generate(SboxLength, 16000, "sbox4", True, True)
        sbox5 = Form1.Numbers_Generate(SboxLength, 19000, "sbox5", True, True)
        sbox6 = Form1.Numbers_Generate(SboxLength, 22000, "sbox6", True, True)
        sbox7 = Form1.Numbers_Generate(SboxLength, 25000, "sbox7", True, True)
        sbox8 = Form1.Numbers_Generate(SboxLength, 28000, "sbox8", True, True)
        Return 0
    End Function

    Public Shared Function RealProcess(str As String, RoundNumber As Integer, Optional DecryptMode As Boolean = False)
        Dim str32 = Form1.splitit(str)
        str32 = str
        Dim strSboxProccess = ""
        RoundNumber = RoundNumber Mod 8
        If Not DecryptMode Then

            Select Case (RoundNumber)
                Case 0
                    strSboxProccess = Form1.SplitAs8(str32, sbox1)
                Case 1
                    strSboxProccess = Form1.SplitAs8(str32, sbox2)
                Case 2
                    strSboxProccess = Form1.SplitAs8(str32, sbox3)
                Case 3
                    strSboxProccess = Form1.SplitAs8(str32, sbox4)
                Case 4
                    strSboxProccess = Form1.SplitAs8(str32, sbox5)
                Case 5
                    strSboxProccess = Form1.SplitAs8(str32, sbox6)
                Case 6
                    strSboxProccess = Form1.SplitAs8(str32, sbox7)
                Case 7
                    strSboxProccess = Form1.SplitAs8(str32, sbox8)
            End Select

        Else

            Select Case (RoundNumber)
                Case 0
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox1)
                Case 1
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox2)
                Case 2
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox3)
                Case 3
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox4)
                Case 4
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox5)
                Case 5
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox6)
                Case 6
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox7)
                Case 7
                    strSboxProccess = Form1.D_SplitSBOX(str32, sbox8)
            End Select
        End If

        Return strSboxProccess
    End Function
End Class

Class key
    Public Shared Key(15) As String
    Public Shared ChaosKey(15) As String
    Public Shared Function KeyF(chaosInput As String)
        Dim MessageKey = Form1.readSplit(Form1.TextBox5.Text)
        ChaosProccess(chaosInput)

        Dim PC1 = {57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18,
                    10, 2, 59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36,
                    63, 55, 47, 39, 31, 23, 15, 7, 62, 54, 46, 38, 30, 22,
                    14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 28, 20, 12, 4}

        Dim PC2 = {14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10,
                    23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2,
                    41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
                    44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32}

        Dim arrShiftRound = Split("1 2 2 2 2 2 2 2 1 2 2 1 2 2 2 1", " ")
        Dim ArrayKey(15) As String
        Dim arrShiftRoundINT = Array.ConvertAll(arrShiftRound, Function(str) Int32.Parse(str))

        Dim left As String = Mid(MessageKey, 2, 28)
        Dim right As String = Mid(MessageKey, 33, 28)

        For i = 0 To 15

            left = Form1.shifting(left, arrShiftRoundINT(i))
            right = Form1.shifting(right, arrShiftRoundINT(i))

            MessageKey = left + right

            left = Mid(MessageKey, 1, 28)
            right = Mid(MessageKey, 29, 28)


            Key(i) = Mid(left, 4, left.Length - 4) & Mid(left, 4, left.Length - 4)
            Key(i) = Form1.shifting(Key(i), ChaosKey(i))
        Next
        Return 0
    End Function

    Shared Function ChaosProccess(InputR As String)
        Dim ChaosTable = Chaos_Table(InputR, 0.48)
        Dim random = New Random()
        For index = 0 To ChaosKey.Length - 1
            ChaosKey(0) = Mid(ChaosTable, random.Next(1, ChaosTable.Length - 1), 1)
        Next
        Return 0
    End Function

    Shared Function Chaos_Table(InputKeyR As String, InputKeyX As String) As String
        Dim x(1000) As Double
        Dim r As Double

        x(0) = ChaosAsc(InputKeyX) Mod 0.31
        r = ChaosAsc(InputKeyR) Mod 3.73
        If r = 0 Then
            r += 0.39
        ElseIf x(0) = 0 Then
            x(0) = 0.43
        End If
        For i = 0 To x.Length - 2

            If Not x.Contains(x(i) * r * (1 - x(i)) Mod 1) Then
                x(i + 1) = x(i) * r * (1 - x(i)) Mod 1
                r = r + 0.5 Mod 4
            End If
        Next
        Dim StrArray As String() = Array.ConvertAll(x, Function(y) y.ToString)
        Return Join(StrArray, "").Replace("0.", "").Replace("00", "0").Replace("00", "0")

    End Function

    Shared Function ChaosAsc(Key As String)
        Dim FullAsc As Integer = 0
        For i = 1 To Key.Length
            FullAsc += Asc(Mid(Key, i, 1))
        Next
        Return FullAsc
    End Function
End Class