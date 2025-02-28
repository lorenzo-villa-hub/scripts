#!/bin/bash

# Find all occurrences of "job.sh" in subdirectories
find . -type f -name "job.sh" | while read -r script; do
    # Get the directory of the script
    script_dir=$(dirname "$script")
    
    # Move to the directory
    echo "Entering directory: $script_dir"
    cd "$script_dir" || { echo "Failed to enter $script_dir"; exit 1; }
    
    # Run the script
    echo "Executing $script"
    sbatch job.sh
    
    # Return to the original directory
    cd - > /dev/null
done

