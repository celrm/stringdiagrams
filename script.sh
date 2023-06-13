#!/bin/bash

# Define the file to monitor
file_to_monitor="./src/BrickDiagrams.hs"

# Define the command to execute when the file is modified
command_to_execute="cabal run stringdiagrams -- -o diagram.svg -w 400"

# Function to monitor the file
monitor_file() {
    while true; do
        inotifywait -e modify "$file_to_monitor"  >/dev/null 2>&1 # Wait for the file to be modified
        $command_to_execute  # Execute the command
    done
}

# Call the function to start monitoring
monitor_file
