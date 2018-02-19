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
			echo "Would you like to attempt to update? (Y/N)"
			read varDecision

		#Ask the user if they would like to update.

			if [ "$varDecision" = "Y" ]
			then
				git pull
			fi
		fi

		}
	uncommitted(){

		git diff > changes.log		
		echo "Differences have been logged, would you like to view them? (Y/N):"
		read varChoice
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

		echo "All TODO tags have been logged, would you like to view them? (Y/N):"
		read varChoice	
		if [ "$varChoice" = "Y" ]
		then
			cat todo.log
		fi
			}
	
	checkHaskell(){

						

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
	else
		echo "Command not found, expected one of the following:"
		echo "compare"
		echo "uncommitted"
		echo "todo"
		echo "checkHaskell"
	fi
