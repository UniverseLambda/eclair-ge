; Code taken from the Wikipedia page for BlitzBasic (but fix some issues)

; Those two will be treated as `AppTile("Binary Clock")` and `Graphics(150,80,16,3)`
AppTitle "Binary Clock"
Graphics 150,80,16,3

;create a timer that means the main loop will be
;executed twice a second
secondtimer=CreateTimer(2)

Repeat
 	Hour = Left(CurrentTime$(),2)
 	Minute = Mid(CurrentTime$(),3,2)
 	Second = Right(CurrentTime$(),2)

 	If Hour >= 12 Then PM = 1
 	If Hour > 12 Then Hour = Hour - 12
 	If Hour = 0 Then Hour = 12

 	;should do this otherwise the PM dot will be
 	;left up once the clock rolls past midnight!
 	Cls

 	Color(0,255,0) ;make the text green for the PM part
 	If PM = 1 Then Text 5,5,"PM"
 	;set the text colour back to white for the rest
 	Color(255,255,255)

 	For bit=0 To 5
        xpos=20*(6-bit)
        binaryMask=2^bit

        ;do hours
        If (bit<4)
            If (hour And binaryMask)
                Text xpos,5,"1"
            Else
                Text xpos,5,"0"
            EndIf
        EndIf

        ;do the minutes
        If (minute And binaryMask)
            Text xpos,25,"1"
        Else
            Text xpos,25,"0"
        EndIf

        ;do the seconds
        If (second And binaryMask)
 			Text xpos,45,"1"
        Else
 			Text xpos,45,"0"
        EndIf
    Next

    ;make the text red for the decimal time
    Color(255,0,0)
    Text 5,65,"Decimal: " + CurrentTime$()
    ;set the text back to white for the rest
    Color(255,255,255)

    ;will wait half a second
    WaitTimer(secondTimer)

Forever
