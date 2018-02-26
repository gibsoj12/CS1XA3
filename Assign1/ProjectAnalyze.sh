#!/bin/bash

# Do nothing if no arguments are given to the script

if [$# -eq 0 ]
then
	echo "Insufficient arguments, requires one argument."
else

	# If compare is called: 

	compare(){
		
		$(git fetch origin)
		local=$(git rev-parse master)
		remote=$(git rev-parse origin/master)
		
		#Check that these return the same.

		if [ $local = $remote ]
		then
			echo "Local repo up to date with remote."
		else
			echo "Local repo not up to date with remote."
			read -p "Would you like to attempt to update? (Y/N):"  varDecision

		#Ask the user if they would like to update.

			if [ "$varDecision" = "Y" ]
			then
				git pull
			fi
		fi

		}
	uncommitted(){
	
		#Redirect the differences to the log.

		git diff > changes.log
		read -p "Differences have been logged, would you like to view them? (Y/N):" varChoice
		if [ "$varChoice" = "Y" ]
		then
			vim changes.log
		fi
			}
		
	todo(){

		#Take all lines containing the tag todo and write them to the log

		grep -rh --exclude={*.log,ProjectAnalyze.sh} "#TODO" * > todo.log

		#Ask the user if they wish to view the todo log

		read -p "All TODO tags have been logged, would you like to view them? (Y/N):" varChoice	
		if [ "$varChoice" = "Y" ]
		then
			vim todo.log
		fi
			}
	
	checkHaskell(){

			#Find all .hs files, execute an error check, send all error messages to the error log
			
			find . -type f -name "*.hs" -exec ghc -fno-code "{}" \; 2>> error.log

			if [ "$(wc -l error.log)" -eq 0 ]
			then
				echo "No errors have been found."
			else

				read -p "Errors have been logged, would you like to view them? (Y/N):" varChoice
				if [ "$varChoice" = "Y" ]
				then
					vim error.log
				fi
			fi					
			}

	search(){
		#Ask the user if they are searching for a file name, or the line within the file.

		read -p "Would you like to view file names, or lines containing your search parameter? (file/line):" wantedType
		read -p "Enter the pattern you would like to search for." pattern
		
		if [ "$wantedType" = "file" ]
		then
			findings=$(grep -rl -e ${pattern})
			echo "$findings"
		
		elif [ "$wantedType" = "line" ]
		then
			findings=$(grep -r -e ${pattern})
			echo "$findings"

		else

			echo "Unrecognized arguments, expected one of the following: file, line"
		fi		
		}
	findFile(){
			
			#Find location of a file
			read -p "Please enter the name of the file you are looking for." fileName
			if [ $(find . -name "$fileName" -type f | wc -l) -gt 0 ]
			then
				echo "The location of your file is: " echo "$(readlink -f ${fileName})"
			else
				echo " "$fileName" does not exist."
			fi	
			}

	deleteOld(){
		
		#List all files which have not been accessed within the last 30 days
		unused=$(find . -atime +30 -type f)
		echo "$unused"

		#Create an array where each element is a file path

		mapfile -t old_files < <(find . -atime +30 -type f)

		#Ask user if they would like to delete a file

		read -p "Is there a file you would like to delete? (Y/N):" wantDelete
		if [ "$wantDelete" = "Y" ]
		then	
			read -p "Specify the line which the file you would like to delete appears (beginning with 0). " fileDelete
			read -p "You have selected: ${old_files["$fileDelete"]} is this correct? (Y/N): "
			if [ "$choice" = "Y" ]
			then
				rm ${old_files["$fileDelete"]}

			elif [ "$choice" = "N" ]
			then
				echo "Please retry."
			fi
		else
			echo "No files will be deleted."

		fi

		}

	removeLogs(){	#Ask user if they would like to remove log files in current directory as well as subdirectory, adjust depth accordingly.

			read -p "Would you like to delete all log files within subdirectories as well? (Y/N): " decision
			if [ "$decision" = "Y" ]
			then
				#Removes log files in current directory as well as any subdirectories.				

				find . -type f -name "*.log" -exec rm {} \;
			else

				#removes any .log files in the current directory, does not go into subdirectories.
			
				find . -maxdepth 1 -type f -name "*.log" -exec rm "{}" \;
			fi

			}

	checkPython(){	#Similar to the haskell check, finds all errors associated with files having a .py extension.

			find . -type f -name "*.py" -exec -fno-code "{}" \; | 2>> pythonError.log

			if [ "$(wc -l pythonError.log)" -eq 0 ]
			then
				echo "No errors have been found."
			else
				read -p "Errors have been found, would you like to view the error log? (Y/N): " decision
				if ["$decision" = "Y" ]
				then
					cat pythonError.log
				fi
			fi
			
			}

	if [ "$1" = "compare" ]
	then
		
		compare
 	
	elif [ "$1" = "uncommitted" ]
	then
		
		uncommitted

	elif [ "$1" = "todo" ]
	then
		todo

	elif [ "$1" = "checkHaskell" ]
	then
		checkHaskell
	
	elif [ "$1" = "search" ]
	then	
		search
	
	elif [ "$1" = "findFile" ]
	then
		findFile
	
	elif [ "$1" = "deleteOld" ]
	then
		deleteOld
	
	elif [ "$1" = "removeLogs" ]
	then
		removeLogs
	
	elif [ "$1" = "checkPython" ]
	then
		checkPython

	else
		echo "Command not found, expected one of the following:"
		echo "compare"
		echo "uncommitted"
		echo "todo"
		echo "checkHaskell"
		echo "search"
		echo "findFile"
		echo "deleteOld"
		echo "removeLogs"
		echo "checkPython"
	fi
fi
