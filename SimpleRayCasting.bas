' Copyright (c) 2017, William Breathitt Gray
' All rights reserved.
' 
' Redistribution and use in source and binary forms, with or without modification,
' are permitted provided that the following conditions are met:
' 
'   Redistributions of source code must retain the above copyright notice, this
'   list of conditions and the following disclaimer.
' 
'   Redistributions in binary form must reproduce the above copyright notice, this
'   list of conditions and the following disclaimer in the documentation and/or
'   other materials provided with the distribution.
' 
' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
' ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
' DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
' ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
' (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
' LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
' ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
' (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
' SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Declare Function checkCollision (ByVal x_pos As Integer, ByVal y_pos As Integer) As UInteger
Declare Sub castRay (ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double, ByVal proj_col As UInteger, ByVal persp_ang As Double)
Declare Sub drawView (ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal face_dir As Double)
Declare Function getBlockColor(ByVal block_type As UInteger) As UInteger
Declare Function getHoriDist(ByRef slice_color As UInteger, ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double) As Double
Declare Function getVertDist(ByRef slice_color As UInteger, ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double) As Double

Const scrn_width As UInteger = 640
Const scrn_height As UInteger = 480
Const block_size As UInteger = 64
Const deg2rad As Double = 0.0174532925199433
Const turn_ang As Double = 10
Const move_speed As Integer = 10
Const map_width As UInteger = 10
Const map_edge As UInteger = block_size * map_width
Const view_angle As Double = 30

Dim Shared map(map_width - 1, map_width - 1) As UInteger = { { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, _
                                                             { 1, 0, 1, 0, 0, 0, 0, 0, 0, 1 }, _
                                                             { 1, 0, 1, 1, 0, 1, 1, 1, 0, 1 }, _
                                                             { 1, 0, 1, 1, 0, 1, 1, 1, 1, 1 }, _
                                                             { 1, 0, 0, 0, 0, 0, 0, 0, 0, 2 }, _
                                                             { 1, 0, 1, 1, 1, 1, 1, 0, 1, 1 }, _
                                                             { 1, 0, 1, 0, 0, 0, 1, 0, 1, 1 }, _
                                                             { 1, 0, 1, 0, 3, 0, 0, 0, 0, 1 }, _
                                                             { 1, 0, 1, 0, 0, 0, 1, 1, 0, 1 }, _
                                                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } }

ScreenRes scrn_width, scrn_height, 16

Dim offset_x As Integer = 96
Dim offset_y As Integer = 96
Dim face_dir As Double = 270

Do
        drawView(offset_x, offset_y, face_dir)

        Do
                Dim k As String = InKey$
                Select Case k
                        Case "a"
                                face_dir += turn_ang
                                If face_dir >= 360 Then
                                        face_dir -= 360
                                End If
                                Exit Do
                        Case "d"
                                face_dir -= turn_ang
                                If face_dir < 0 Then
                                        face_dir += 360
                                End If
                                Exit Do
                        Case "w"
                                Dim new_offset_x As Integer = offset_x + Fix(Cos(face_dir * deg2rad) * move_speed)
                                Dim new_offset_y As Integer = offset_y - Fix(Sin(face_dir * deg2rad) * move_speed)
                                If checkCollision(new_offset_x, new_offset_y) = 0 Then
                                        offset_x = new_offset_x
                                        offset_y = new_offset_y
                                End If
                                Exit Do
                        Case "s"
                                Dim new_offset_x As Integer = offset_x - Fix(Cos(face_dir * deg2rad) * move_speed)
                                Dim new_offset_y As Integer = offset_y + Fix(Sin(face_dir * deg2rad) * move_speed)
                                If checkCollision(new_offset_x, new_offset_y) = 0 Then
                                        offset_x = new_offset_x
                                        offset_y = new_offset_y
                                End If
                                Exit Do
                        Case "x"
                                End
                End Select
        Loop 
Loop

End

Function checkCollision (ByVal x_pos As Integer, ByVal y_pos As Integer) As UInteger
        If x_pos < 0 Or y_pos < 0 Then
                Return 1
        End If

        Dim x_index As UInteger = Fix(x_pos / block_size)
        Dim y_index As UInteger = Fix(y_pos / block_size)

        Return map(y_index, x_index)
End Function

Sub castRay (ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double, ByVal proj_col As UInteger, ByVal persp_ang As Double)
        Dim hori_color As UInteger
        Dim hori_dist As Double = getHoriDist(hori_color, x_pos, y_pos, proj_ang)
        Dim vert_color As UInteger
        Dim vert_dist As Double = getVertDist(vert_color, x_pos, y_pos, proj_ang)
        If hori_dist < 0 And vert_dist < 0 Then
                Exit Sub
        End If

        Dim dist As Double
        Dim slice_color As UInteger
        If vert_dist < 0 Or (hori_dist >= 0 And hori_dist < vert_dist) Then
                dist = hori_dist
                slice_color = hori_color
        Else
                dist = vert_dist
                slice_color = vert_color
        End If

        Const proj_plane_dist As Double = (scrn_width / 2) / Tan(view_angle * deg2rad)
        Const proj_height_factor As Double = block_size * proj_plane_dist
        Dim dist_corr As Double = dist * Cos(persp_ang * deg2rad)
        Dim slice_height As UInteger = Fix(proj_height_factor / dist_corr)
        If slice_height = 0 Then
                Exit Sub
        Elseif slice_height > scrn_height Then
                slice_height = scrn_height
        End If
        Dim slice_offset As UInteger = Fix((scrn_height - slice_height) / 2)

        Line (proj_col, slice_offset)-(proj_col, slice_offset + slice_height), slice_color
End Sub

Sub drawView (ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal face_dir As Double)
        Const scrn_end_x As UInteger = scrn_width - 1
        Const horizon As UInteger = Fix(scrn_height/2) - 1
        Const ceiling_color As UInteger = &H888888
        Const floor_color As UInteger = &H444444

        Line (0, 0)-(scrn_end_x, horizon), &H888888, BF
        Line (0, horizon + 1)-(scrn_end_x, scrn_height - 1), &H444444, BF

        Const halfview As UInteger = Fix(scrn_width/2)
        Const proj_ang_step As Double = view_angle / halfview
        For proj_col As UInteger = 0 to halfview-1
                Dim proj_ang As Double = view_angle - proj_col * proj_ang_step

                Dim left_proj_ang As Double = face_dir + proj_ang
                If left_proj_ang >= 360 Then
                        left_proj_ang -= 360
                End If
                Dim right_proj_ang As Double = face_dir - proj_ang
                If right_proj_ang < 0 Then
                        right_proj_ang += 360
                End If

                castRay(x_pos, y_pos, left_proj_ang, proj_col, proj_ang)
                castRay(x_pos, y_pos, right_proj_ang, scrn_end_x - proj_col, proj_ang)
        Next proj_col
End Sub

Function getBlockColor(ByVal block_type As UInteger) As UInteger
        Select Case As Const block_type
                Case 1
                        Return &HFF0000
                Case 2
                        Return &H00FF00
                Case 3
                        Return &H0000FF
        End Select

        Return 0
End Function

Function getHoriDist(ByRef slice_color As UInteger, ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double) As Double
        Dim check_pos_y As Integer = Fix(y_pos / block_size) * block_size
        Dim check_pos_y_delta As Double
        If proj_ang < 180 Then
                check_pos_y -= 1
                check_pos_y_delta = -block_size
        Else
                check_pos_y += block_size
                check_pos_y_delta = block_size
        End If

        While check_pos_y >=0 And check_pos_y < map_edge
                Dim check_pos_x As Double = x_pos + (y_pos - check_pos_y) / Tan(proj_ang * deg2rad)
                If check_pos_x < 0 Or check_pos_x >= map_edge Then
                        Return -1
                End If

                Dim x_index As UInteger = Fix(check_pos_x / block_size)
                Dim y_index As UInteger = Fix(check_pos_y / block_size)
                Dim block_type As UInteger = map(y_index, x_index)
                If block_type Then
                        slice_color = getBlockColor(block_type)

                        Dim pos_x_delta As Double = check_pos_x - x_pos
                        Dim pos_y_delta As Double = check_pos_y - y_pos
                        Return Sqr(pos_x_delta * pos_x_delta + pos_y_delta * pos_y_delta)
                End If

                check_pos_y += check_pos_y_delta
        WEnd

        Return -1
End Function

Function getVertDist(ByRef slice_color As UInteger, ByVal x_pos As Integer, ByVal y_pos As Integer, ByVal proj_ang As Double) As Double
        Dim check_pos_x As Integer = Fix(x_pos / block_size) * block_size
        Dim check_pos_x_delta As Double
        If proj_ang < 90 Or proj_ang >= 270 Then
                check_pos_x += block_size
                check_pos_x_delta = block_size
        Else
                check_pos_x -= 1
                check_pos_x_delta = -block_size
        End If

        While check_pos_x >=0 And check_pos_x < map_edge
                Dim check_pos_y As Double = y_pos - (check_pos_x - x_pos) * Tan(proj_ang * deg2rad)
                If check_pos_y < 0 Or check_pos_y >= map_edge Then
                        Return -1
                End If

                Dim x_index As UInteger = Fix(check_pos_x / block_size)
                Dim y_index As UInteger = Fix(check_pos_y / block_size)
                Dim block_type As UInteger = map(y_index, x_index)
                If block_type Then
                        slice_color = getBlockColor(block_type) And &HAAAAAA

                        Dim pos_x_delta As Double = check_pos_x - x_pos
                        Dim pos_y_delta As Double = check_pos_y - y_pos
                        Return Sqr(pos_x_delta * pos_x_delta + pos_y_delta * pos_y_delta)
                End If

                check_pos_x += check_pos_x_delta
        WEnd

        Return -1
End Function
