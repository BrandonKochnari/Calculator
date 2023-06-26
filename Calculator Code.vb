Public Class Calculator

    Dim NUMB1, NUMB2 As Double
    Dim Result As Double
    Dim Plus, Minus, Division, Multiplication, Equals As Boolean
    Dim Decimun As Boolean
    Dim Negative As Boolean
    Dim lastClickSymbol As Boolean
    Dim Blort, Bolob As String
    Dim history(20, 1)() As Integer
    Dim nums1(9), nums2(9) As Integer
    Dim reverseNums(10) As Integer
    Dim decNums(10) As Integer
    Dim i As Integer = 1
    Dim histC As Integer = 0
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Plus = False
        Minus = False
        Multiplication = False
        Division = False
        Blort = ""
        Negative = False
        Equals = True
        nums1 = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        nums2 = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        For index = 0 To 20
            history(index, 0) = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
            history(index, 1) = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        Next
        histC += 1
    End Sub

    Private Sub Btn9_Click(sender As Object, e As EventArgs) Handles Btn9.Click
        If Negative = True Then
            Call equate(-9)
        Else
            Call equate(9)
        End If
    End Sub

    Private Sub Btn8_Click(sender As Object, e As EventArgs) Handles Btn8.Click
        If Negative = True Then
            Call equate(-8)
        Else
            Call equate(8)
        End If
    End Sub

    Private Sub Btn7_Click(sender As Object, e As EventArgs) Handles Btn7.Click
        If Negative = True Then
            Call equate(-7)
        Else
            Call equate(7)
        End If
    End Sub

    Private Sub Btn6_Click(sender As Object, e As EventArgs) Handles Btn6.Click
        If Negative = True Then
            Call equate(-6)
        Else
            Call equate(6)
        End If
    End Sub

    Private Sub Btn5_Click(sender As Object, e As EventArgs) Handles Btn5.Click
        If Negative = True Then
            Call equate(-5)
        Else
            Call equate(5)
        End If
    End Sub

    Private Sub Btn4_Click(sender As Object, e As EventArgs) Handles Btn4.Click
        If Negative = True Then
            Call equate(-4)
        Else
            Call equate(4)
        End If
    End Sub

    Private Sub Btn3_Click(sender As Object, e As EventArgs) Handles Btn3.Click
        If Negative = True Then
            Call equate(-3)
        Else
            Call equate(3)
        End If

    End Sub

    Private Sub Btn2_Click(sender As Object, e As EventArgs) Handles Btn2.Click
        If Negative = True Then
            Call equate(-2)
        Else
            Call equate(2)
        End If
    End Sub

    Private Sub Btn1_Click(sender As Object, e As EventArgs) Handles Btn1.Click
        If Negative = True Then
            Call equate(-1)
        Else
            Call equate(1)
        End If
    End Sub

    Private Sub BTN0_Click(sender As Object, e As EventArgs) Handles BTN0.Click
        If Negative = True Then
            Call equate(0)
        Else
            Call equate(0)
        End If
    End Sub

    Private Sub BTN_Clear_Click(sender As Object, e As EventArgs) Handles BTN_Clear.Click
        NUMB1 = 0
        NUMB2 = 0
        Plus = False
        Minus = False
        Multiplication = False
        Division = False
        Answer.Text = ""
        Blort = ""
        Bolob = ""
        Negative = False
        Decimun = False
        Equals = True
        BTN_Decimun.Enabled = True
        Result = 0
        i = 1
        nums1 = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        nums2 = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        decNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        reverseNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    End Sub
    Private Sub BTN_DELETE_Click(sender As Object, e As EventArgs) Handles BTN_DELETE.Click
        If Equals Then
            If Plus = False And Minus = False And Multiplication = False And Division = False And (Blort.Length > 0) Then
                Blort = Blort.Remove(Blort.Length - 1)
                history(histC - 2, 0).CopyTo(nums1, 0)
                history(histC - 2, 1).CopyTo(nums2, 0)
                histC -= 1
            Else
                If checkLastSymbol() Then
                    Blort = Blort.Remove(Blort.Length - 3)
                    Multiplication = False
                    Division = False
                    Minus = False
                    Plus = False
                Else
                    If (Bolob.Length > 0) Then
                        Bolob = Bolob.Remove(Bolob.Length - 1)
                        history(histC - 2, 0).CopyTo(nums1, 0)
                        history(histC - 2, 1).CopyTo(nums2, 0)
                        histC -= 1
                    End If
                End If
            End If
        End If
        Answer.Text = Blort + Bolob
    End Sub

    Private Sub BtnPlus_Click(sender As Object, e As EventArgs) Handles BtnPlus.Click
        If Not Plus And Not Minus And Not Division And Not Multiplication Then
            Blort += " + "
            Plus = True
        Else
            If Not checkLastSymbol() Then
                Minus = False
                Division = False
                Multiplication = False
                Plus = True
                NUMB1 = Result
                Blort = Blort + Bolob + " + "
                Bolob = ""
            End If
        End If
            Negative = False
        Decimun = False
        decNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        reverseNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        lastClickSymbol = True
        i = 1
        Answer.Text = Blort + Bolob
    End Sub
    Private Sub BTN_Minus_Click(sender As Object, e As EventArgs) Handles BTN_Minus.Click
        If Not Minus And Not Plus And Not Division And Not Multiplication Then
            Blort += " - "
            Minus = True
            i = 1
        Else
            If Not checkLastSymbol() Then
                Minus = False
                Division = False
                Multiplication = False
                Plus = True
                NUMB1 = Result
                Blort = Blort + Bolob + " + "
                Bolob = ""
            End If
        End If
        Answer.Text = Blort + Bolob
        Negative = False
        Decimun = False
        decNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        reverseNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        lastClickSymbol = True
        i = 1
    End Sub

    Private Sub BTN_Multiplication_Click(sender As Object, e As EventArgs) Handles BTN_Multiplication.Click
        If Not Multiplication And Not Minus And Not Division And Not Plus Then
            Blort += " * "
            Multiplication = True
            i = 1
        Else
            If Not checkLastSymbol() Then
                Minus = False
                Division = False
                Multiplication = False
                Plus = True
                NUMB1 = Result
                Blort = Blort + Bolob + " + "
                Bolob = ""
            End If
        End If
        Answer.Text = Blort + Bolob
        Negative = False
        Decimun = False
        decNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        reverseNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        lastClickSymbol = True
    End Sub

    Private Sub BTN_Divison_Click(sender As Object, e As EventArgs) Handles BTN_Divison.Click
        If Not Multiplication And Not Minus And Not Division And Not Plus Then
            Blort += " / "
            Division = True
            i = 1
        Else
            If Not checkLastSymbol() Then
                Minus = False
                Division = False
                Multiplication = False
                Plus = True
                NUMB1 = Result
                Blort = Blort + Bolob + " + "
                Bolob = ""
            End If
        End If
        Answer.Text = Blort + Bolob
        Negative = False
        Decimun = False
        decNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        reverseNums = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        lastClickSymbol = True
    End Sub

    Private Sub BTN_Equals_Click(sender As Object, e As EventArgs) Handles BTN_Equals.Click
        If Equals = True Then
            Answer.Text += " = " & Result.ToString
            Equals = False
        End If
    End Sub
    Private Sub equate(num As Integer)
        lastClickSymbol = False
        If Plus = True Then
            If Decimun = False Then
                insertNum(num, nums2, NUMB2, Bolob)
                Result = NUMB1 + NUMB2
            Else
                insertDecimun(num, NUMB2, Bolob, i)
                Result = NUMB1 + NUMB2
            End If

        ElseIf Minus = True Then
            If Decimun = False Then
                insertNum(num, nums2, NUMB2, Bolob)
                Result = NUMB1 - NUMB2
            Else
                insertDecimun(num, NUMB2, Bolob, i)
                Result = NUMB1 - NUMB2
            End If
        ElseIf Multiplication = True Then
            If Decimun = False Then
                insertNum(num, nums2, NUMB2, Bolob)
                Result = NUMB1 * NUMB2
            Else
                insertDecimun(num, NUMB2, Bolob, i)
                Result = NUMB1 * NUMB2
            End If
        ElseIf Division = True Then
            If Decimun = False Then
                insertNum(num, nums2, NUMB2, Bolob)
                Result = NUMB1 / NUMB2
            Else
                insertDecimun(num, NUMB2, Bolob, i)
                Result = NUMB1 / NUMB2
            End If
        Else
            If Decimun = False Then
                insertNum(num, nums1, NUMB1, Blort)
            Else
                insertDecimun(num, NUMB1, Blort, i)
            End If
        End If
        nums1.CopyTo(history(histC, 0), 0)
        nums2.CopyTo(history(histC, 1), 0)
        histC += 1
        Answer.Text = Blort + Bolob
    End Sub

    Private Sub BTN_Negative_Click(sender As Object, e As EventArgs) Handles BTN_Negative.Click
        Negative = True
    End Sub
    Private Function reverse(nums() As Integer)
        Dim MultiNum As Integer = 0
        For Index = 0 To (nums.Length - 1)
            reverseNums(Index) = nums(nums.Length - 1 - Index)
        Next
        For Index = 0 To (nums.Length - 1)
            MultiNum += Math.Pow(10, Index) * reverseNums(Index)
        Next Index
        Return MultiNum

    End Function
    Private Sub insertNum(num As Double, nums() As Integer, ByRef position As Double, ByRef text As String)
        For Index = 1 To nums.Length - 1
            nums(Index - 1) = nums(Index)
        Next Index
        nums(nums.Length - 1) = num
        position = reverse(nums)
        text = position.ToString()
    End Sub


    Private Sub BTN_Decimun_Click(sender As Object, e As EventArgs) Handles BTN_Decimun.Click
        Decimun = True
        Answer.Text += "."
        BTN_Decimun.Enabled = False
    End Sub
    Private Sub insertDecimun(num As Integer, ByRef position As Double, ByRef text As String, ByRef index As Integer)
        decNums(index) = num
        position += Math.Pow(10, -index) * num
        text = position.ToString()
        index += 1
    End Sub

    Private Function checkLastSymbol()
        If Answer.Text(Answer.Text.Length - 2) = "+" Or
                Answer.Text(Answer.Text.Length - 2) = "-" Or
                Answer.Text(Answer.Text.Length - 2) = "*" Or
                Answer.Text(Answer.Text.Length - 2) = "/" Then
            Return True
        End If
        Return False
    End Function
End Class

