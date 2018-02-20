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
			cat changes.log
		fi
			}
		
	todo(){

		#Take all lines containing the tag todo and write them to the log

		todoStuff=$(grep -r "#TODO")
		todoStuff > todo.log

		#Ask the user if they wish to view the todo log

		read -p "All TODO tags have been logged, would you like to view them? (Y/N):" varChoice	
		if [ "$varChoice" = "Y" ]
		then
			cat todo.log
		fi
			}
	
	checkHaskell(){

						

			}

	search(){
		#Ask the user if they are searching for a file name, or the line within the file.

		read -p "Would you like to view file names, or lines containing your search parameter? (file/line):" wantedType
		read -p "Where would you like to search?" startPoint
		read -p "Enter the pattern you would like to search for." pattern
		
		if [ "$wantedType" = "file" ]
		then
			findings=$(find "$startPoint" -type f -print0 | xargs grep -l "$pattern")
			echo "$findings"
		
		elif [ "$wantedType" = "line" ]
		then
			findings=$(find "$startPoint" -type -print0 | xargs grep "$pattern")

		else

			echo "Unrecognized arguments, expected one of the following: file, line"
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
		
		search

	else
		echo "Command not found, expected one of the following:"
		echo "compare"
		echo "uncommitted"
		echo "todo"
		echo "checkHaskell"
	fi
