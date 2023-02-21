AutoItSetOption('MouseCoordMode', 0)

#comments-start
For $a = 84 To 99

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
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'2. View and edit','WindowsForms10.BUTTON.app.0.141b42a_r6_ad12')
;Select the SRC /TVG Tab
WinWaitActive("SonarTRX - Data view and edit")
MouseClick('primary', 200, 55, 1, 0)
;Select the Start Processing button and click
WinWaitActive("SonarTRX - Data view and edit")
ControlClick("SonarTRX - Data view and edit",'Start Processing','WindowsForms10.BUTTON.app.0.141b42a_r6_ad128')
;Select OK on Notification button
WinWaitActive("SonarTRX Notification")
ControlClick("SonarTRX Notification",'','Button1')
;Select the Close button and click
WinWaitActive("SonarTRX - Data view and edit")
ControlClick("SonarTRX - Data view and edit",'Close','WindowsForms10.BUTTON.app.0.141b42a_r6_ad13')

;Select the 3. Create mosaic button and click
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'3. Create mosaic','WindowsForms10.BUTTON.app.0.141b42a_r6_ad16')

WinWaitActive("SonarTRX - Mosaic creation")
;Click on the File name edit field and write file name
ControlClick("SonarTRX - Mosaic creation", "", "WindowsForms10.EDIT.app.0.141b42a_r6_ad113")
Send("^a")
;Send("{SHIFTDOWN}S{SHIFTUP}onar{SHIFTDOWN}TRX-R{SHIFTUP}00084")
Send("SonarTRX-R000" &$a)
;Click on Start Processing button
ControlClick("SonarTRX - Mosaic creation", "Start Processing", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad119")
;Click Yes on SonarTRX Notification Window
WinWaitActive("SonarTRX Notification")
ControlClick("SonarTRX Notification", "Yes", "Button1")
;Processing Occurs, Click on OK Button
WinWaitActive("SonarTRX - Mosaic Creation...")
ControlClick("SonarTRX - Mosaic Creation...", "OK", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad12")

;Close SonarTRX
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
MouseClick('primary', 605, 13, 1, 0)

Next
#comments-end

;missing channel 2 & 3 112,129,181,412
;totally missing R00151, R00389, R00440
For $a = 441 To 457

run("C:\Program Files\SonarTRX\SonarTRX (x64)\SonarTRX.exe")

;Open the Import button
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'','WindowsForms10.BUTTON.app.0.141b42a_r6_ad18');Send("{ENTER}")

;Select and write in file to import and push open
WinWaitActive("SonarTRX - Please select a sonar file to import, or multiple files for batch processing.")
Send("{SHIFTDOWN}r{SHIFTUP}00" &$a& ".{SHIFTDOWN}dat{SHIFTUP}")
ControlClick("SonarTRX - Please select a sonar file to import, or multiple files for batch processing.","",'Button1')
;Data Import Screen automatically opens, Select the Start Processing button and click it
WinWaitActive("SonarTRX - Data import","")
ControlClick("SonarTRX - Data import",'Start Processing','WindowsForms10.BUTTON.app.0.141b42a_r6_ad118')

;click the 2. View and edit button
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'2. View and edit','WindowsForms10.BUTTON.app.0.141b42a_r6_ad12')

For $b = 1 To 3
	;Select the appropriate clip
	WinWaitActive("SonarTRX - Data view and edit")
	ControlCommand("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'','WindowsForms10.COMBOBOX.app.0.141b42a_r6_ad1',"SetCurrentSelection", 2)






Next

;Select the SRC /TVG Tab
WinWaitActive("SonarTRX - Data view and edit")
MouseClick('primary', 200, 55, 1, 0)
;Select the Start Processing button and click
WinWaitActive("SonarTRX - Data view and edit")
ControlClick("SonarTRX - Data view and edit",'Start Processing','WindowsForms10.BUTTON.app.0.141b42a_r6_ad128')
;Select OK on Notification button
WinWaitActive("SonarTRX Notification")
ControlClick("SonarTRX Notification",'','Button1')
;Select the Close button and click
WinWaitActive("SonarTRX - Data view and edit")
ControlClick("SonarTRX - Data view and edit",'Close','WindowsForms10.BUTTON.app.0.141b42a_r6_ad13')

;Select the 3. Create mosaic button and click
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
ControlClick("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)",'3. Create mosaic','WindowsForms10.BUTTON.app.0.141b42a_r6_ad16')

WinWaitActive("SonarTRX - Mosaic creation")
;Click on the File name edit field and write file name
ControlClick("SonarTRX - Mosaic creation", "", "WindowsForms10.EDIT.app.0.141b42a_r6_ad113")
Send("^a")
;Send("{SHIFTDOWN}S{SHIFTUP}onar{SHIFTDOWN}TRX-R{SHIFTUP}00084")
Send("SonarTRX-R00" &$a)
;Click on Start Processing button
ControlClick("SonarTRX - Mosaic creation", "Start Processing", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad119")
;Click Yes on SonarTRX Notification Window
WinWaitActive("SonarTRX Notification")
ControlClick("SonarTRX Notification", "Yes", "Button1")
;Processing Occurs, Click on OK Button
WinWaitActive("SonarTRX - Mosaic Creation...")
ControlClick("SonarTRX - Mosaic Creation...", "OK", "WindowsForms10.BUTTON.app.0.141b42a_r6_ad12")

;Close SonarTRX
WinWaitActive("SonarTRX Pro w/PlusPack (Copyright LEI 2006-2020)")
MouseClick('primary', 605, 13, 1, 0)

Next

