'Imports System.Text.RegularExpressions

Public Class Form1


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim _mesageBin = readSplit()
        _mesageBin = Encrypt(_mesageBin)
        _mesageBin = Decrypt(_mesageBin)
       
      
    End Sub

    Function Encrypt(_mesageBin As String)

        key.KeyF()

        Dim Tab_P_Box = Numbers_Generate(32, 200, "TableIP", True)
        SboxClass.FillSBOXES()
        Dim Left = Mid(_mesageBin, 1, 32)
        Dim Right = Mid(_mesageBin, 33, 32)

        MessageBox.Show("Your Message is:" & vbLf & BtoC(_mesageBin))
        For i = 1 To 16

            '1 Expand
            Right = splitit(Right)

            '2 Xor
            Right = XorOperation(Right, key.Key(i - 1), 48)
            Right = splitit(Right)

            '3 sbox
            Right = SboxClass.RealProcess(Right, i)


            '4 P_Boxes
            Right = Ip_Procces_Encryption(Tab_P_Box, Right)

            Left = XorOperation(Left, Right, 32)

            _mesageBin = Right & Left
            Left = Mid(_mesageBin, 1, 32)
            Right = Mid(_mesageBin, 33, 32)

        Next
        MessageBox.Show("Your Encrypted Message is:" & vbLf & BtoC(_mesageBin))

        Return _mesageBin
    End Function

    Function Decrypt(_mesageBin As String)
        Dim Tab_P_Box = Numbers_Generate(32, 200, "TableIP", True)
        Dim Left = Mid(_mesageBin, 1, 32)
        Dim Right = Mid(_mesageBin, 33, 32)

        For i = 16 To 1 Step -1
            'Decrypt
            _mesageBin = Right & Left
            Left = Mid(_mesageBin, 1, 32)
            Right = Mid(_mesageBin, 33, 32)

            Left = XorOperation(Left, Right, 32)
            Right = INV_Order(Tab_P_Box, Right)
            Right = SboxClass.RealProcess(Right, i, True)

            Right = splitit(Right)
            Right = XorOperation(Right, key.Key(i - 1), 48)
            Right = splitit(Right)



            '     RichTextBox1.Text += BtoC(Left & Right) & vbLf
        Next
        _mesageBin = Left & Right
        MessageBox.Show("Your Decrypted Message is:" & vbLf & BtoC(_mesageBin))
        Return _mesageBin
    End Function

    Function readSplit()

        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""
        Dim MyOrgString = InputBox("Input Your message")

        While MyOrgString.Length < 8
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
        Dim reader As String = System.IO.File.ReadAllText("_file22222.txt")
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
    Public Shared Function KeyF()
        Dim MessageKey = Form1.readSplit()

        Dim PC1 = {57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18,
                    10, 2, 59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36,
                    63, 55, 47, 39, 31, 23, 15, 7, 62, 54, 46, 38, 30, 22,
                    14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 28, 20, 12, 4}

        Dim PC2 = {14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10,
                    23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2,
                    41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
                    44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32}

        Dim arrShiftRound = Split("1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1", " ")
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
        Next
        Return 0
    End Function
End Class