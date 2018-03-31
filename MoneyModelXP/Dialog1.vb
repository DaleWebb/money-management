Imports System.Windows.Forms

Public Class Dialog1
    Dim UserName As String
    Dim RowCount As Integer

    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OK_Button.Click

        If (ComboBox1.Text = "") Then MessageBox.Show("Please enter your Username", "Error") Else Form1.Show()
        If Not (ComboBox1.Text = "") Then Me.Hide()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Cancel_Button.Click
        Me.Close()
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If (ComboBox1.Text = "") Or (TextBox2.Text = "") Then
            MessageBox.Show("Please enter your desired Username and Password for the account you wish to create", "Error")
        Else
            DataGridView1.Rows.Add(ComboBox1.Text, TextBox2.Text)

        End If


    End Sub
End Class
