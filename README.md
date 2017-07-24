# User Manual

1.  Type `source("runSuprathresholdTest.R")` into the console to begin the procedure.
2.	A pop-up window will appear, asking if you want to run a practice test. Enter the patient's age and eye to be tested before clicking yes. If no practice test is required, you can click no straight away.
3.	After the practice test, a menu will appear prompting you to enter patient and test information for the main test.
4.	You will then be prompted to start the main test. Live test information is provided during the test, which includes FP and FN rates, test duration, number of presentations and % of test complete. The display also shows where each stimulus is presented and will tell you if the observer responded yes or no to that stimulus. Locations shown in red indicate unfinished locations. Locations shown in blue indicate finished locations. The numeric values indicate the intensity of the next stimulus to be presented, or in the case of terminated locations, the previous stimulus that was presented.
5.	There is also the option to pause the test at any time. When paused, you have three options: 1) resume the test 2) terminate the test or 3) delete the last 1-5 presentation responses if the observer made a known false response.
6.	If a FP response is made, you will hear a double beep.
7.	Upon completion of the test, You will be asked if you want to save the test or not. If you elect to save the test, you will then be prompted to enter any final comments. If there are no further comments to add, just leave the space blank and press submit.


**Note:** The code for this test has been adapted from Luke Chong's ZEST procedure: https://github.com/lxchong/Work-for-Wall. If you are also using Luke's code, the test files for this suprathreshold test should be stored in a subfolder relative to Luke's test files.