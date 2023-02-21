AutoItSetOption('MouseCoordMode', 0)

;It's important to go through the SonarTRX program manually to set everything the way you like it.  
;This is particularly important in the menus that set folders for opening and saving data



For $a = 79 To 79  ;70, 74, 75 are problems
If $a = 38 Then $c = 3
If $a = 39 Then $c = 3
If $a = 40 Then $c = 7
If $a = 41 Then $c = 3
If $a = 42 Then $c = 6
If $a = 43 Then $c = 1
If $a = 44 Then $c = 3
If $a = 45 Then $c = 7
If $a = 46 Then $c = 3
If $a = 47 Then $c = 7
If $a = 48 Then $c = 3
If $a = 49 Then $c = 7
If $a = 50 Then $c = 4
If $a = 51 Then $c = 7
If $a = 52 Then $c = 3
If $a = 53 Then $c = 7
If $a = 54 Then $c = 3
If $a = 55 Then $c = 7
If $a = 56 Then $c = 3
If $a = 57 Then $c = 7
If $a = 58 Then $c = 3
If $a = 59 Then $c = 7
If $a = 60 Then $c = 3
If $a = 61 Then $c = 3
If $a = 62 Then $c = 9
If $a = 63 Then $c = 3
If $a = 64 Then $c = 7
If $a = 65 Then $c = 9
If $a = 66 Then $c = 7
If $a = 67 Then $c = 8
If $a = 68 Then $c = 7
If $a = 69 Then $c = 7
If $a = 70 Then $c = 7
If $a = 71 Then $c = 6
If $a = 72 Then $c = 0
If $a = 73 Then $c = 7
If $a = 74 Then $c = 7
If $a = 75 Then $c = 7
If $a = 76 Then $c = 7
If $a = 77 Then $c = 7
If $a = 78 Then $c = 7
If $a = 79 Then $c = 6
If $a = 80 Then $c = 7
If $a = 81 Then $c = 7
If $a = 82 Then $c = 10

;For $b = 1 To $c
run("C:\Program Files\SonarTRX\SonarTRX (x64)\SonarTRX.exe")

;Open the Import button
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'','WindowsForms10.BUTTON.app.0.141b42a_r6_ad18');Send("{ENTER}")

;Select and write in file to import and push open
WinWaitActive("SonarTRX - Please select a sonar file to import, or multiple files for batch processing.")
Send("{SHIFTDOWN}r{SHIFTUP}000" &$a& ".{SHIFTDOWN}dat{SHIFTUP}")
ControlClick("SonarTRX - Please select a sonar file to import, or multiple files for batch processing.","",'Button1')
;Data Import Screen automatically opens, Select the Start Processing button and click it
WinWaitActive("SonarTRX - Data import","")
ControlClick("SonarTRX - Data import",'Start Processing','WindowsForms10.BUTTON.app.0.141b42a_r6_ad118')

;click the 2. View and edit button
;WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
;ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'2. View and edit','WindowsForms10.BUTTON.app.0.141b42a_r6_ad12')

;Select the appropriate clip
;WinWaitActive("SonarTRX - Data view and edit")
;MouseClick("primary",312,350)
;MouseClick("primary",312,355+12*$b);361-373, each is 14 pixels wide

;Select the SRC /TVG Tab
;WinWaitActive("SonarTRX - Data view and edit")
;MouseClick('primary', 200, 55, 1, 0)
;Select the Start Processing button and click
;WinWaitActive("SonarTRX - Data view and edit")
;ControlClick("SonarTRX - Data view and edit",'Start Processing','WindowsForms10.BUTTON.app.0.141b42a_r6_ad128')
;Select OK on Notification button
;WinWaitActive("SonarTRX Notification")
;ControlClick("SonarTRX Notification",'','Button1')
;Select the Close button and click
;WinWaitActive("SonarTRX - Data view and edit")
;ControlClick("SonarTRX - Data view and edit",'Close','WindowsForms10.BUTTON.app.0.141b42a_r6_ad13')

;Select Original (with water column)
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
MouseClick("primary",605,80)
MouseClick("primary",605,95)

For $b = 1 To $c
	;Select the 3. Create mosaic button and click
	WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
	ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'3. Create mosaic','WindowsForms10.BUTTON.app.0.141b42a_r6_ad16')
	WinWaitActive("SonarTRX - Mosaic creation")
	;Click on the File name edit field and write file name
	ControlClick("SonarTRX - Mosaic creation", "", "WindowsForms10.EDIT.app.0.141b42a_r6_ad113")
	Send("^a")
	Send("SonarTRX-R000" &$a& "-" &$b)
	;Select the correct clip images to create
	ControlClick("SonarTRX - Mosaic creation",'','WindowsForms10.BUTTON.app.0.141b42a_r6_ad115')
	MouseClick("primary",600,336)
	MouseClick("primary",600,336+14*$b)
	ControlClick("SonarTRX - Mosaic creation",'','WindowsForms10.BUTTON.app.0.141b42a_r6_ad18')
	ControlClick("SonarTRX - Mosaic creation",'','WindowsForms10.BUTTON.app.0.141b42a_r6_ad119')
	;Click Yes on SonarTRX Notification Window
	WinWaitActive("SonarTRX Notification")
	ControlClick("SonarTRX Notification", "OK", "Button1")
	;Click on Start Processing button
	ControlClick("SonarTRX - Mosaic creation", "Start Processing", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad119")
	;Click Yes on SonarTRX Notification Window
	WinWaitActive("SonarTRX Notification")
	ControlClick("SonarTRX Notification", "Yes", "Button1")
	;Processing Occurs, Click on OK Button
	WinWaitActive("SonarTRX - Mosaic Creation...")
	ControlClick("SonarTRX - Mosaic Creation...", "OK", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad12")

Next

;Close SonarTRX
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
MouseClick('primary', 605, 13, 1, 0)
Next