compare - calling the compare function will tell the user if their working repository is up to date with the remote repo.

uncommitted - This function will show the user any uncommitted changes between their local working repository and the remote repository. The user is then asked if they would like to view the changes.
		If yes, Changes will appear in a different color from the pieces of the file which exist both locally and remotely.

todo - This function checks the current directory and any subdirectories for the tag TODO and writes any lines which contain this to a todo.log in the directory which the user runs the script.
	The user is then asked if they would like to view the log, if so, the log file is opened using vim.

checkHaskell - This function checks the current directory and any subdirectories for .hs files which contain errors, it then places the file name and the error into an error.log 
		in the directory which the user runs the script. The user is then asked if they would like to see the error log, if so, the log file is opened using vim.

search - This function first asks the user if they wish to see the file name, or the line containing their search pattern, the user is then asked to provide a search pattern.
		If the user chooses to view the names of the files, they are listed. If instead, the user wishes to view the individual lines containing their pattern, then the 
		names of the files and the lines containing the pattern are listed instead.

findFile - This function prompts the user for the name of a file. It then searches the current directory and any subdirectories for the file specified, if none are found the user is told the 
		file does not exist, otherwise the path of the file is provided to the user.

deleteOld - Reference: https://stackoverflow.com/questions/11426529/reading-output-of-command-into-array-in-bash 
		This was used as I was having trouble using a loop to correctly input values into my array. This function shows the user all files which have not been accessed within the last 30 days.
		It then asks the user if they would like to delete a file by providing a line number (0 based), if the user chooses to delete a file it is then removed.

checkPython - Checks the current directory and any subdirectories for .py files which contain errors, it then places the file name and the error found into a pythonError.log file
		in the directory which the user runs the script. The user is then informed whether or not errors have been found. If errors have been found the user will be asked if they would like
		to view the log.

removeLogs - The user is asked if they would like to remove any .log files found in the subdirectories as well as the current directory or just the current directory. Based on the users decision
		any .log files within the range will be deleted.

changePermission - The user provides a file, the current permissions for the file are then displayed. The user is asked if they would like to change the permissions of the file, if they would like
			to change permissions, the user then provides the changes they would like to make and the permissions are changed if possible.
