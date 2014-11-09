function prompt_jobs -d "Print the current number of jobs"
	set result (jobs)

	# Check if there are any jobs running
	if [ (count $result) != 0 ]
		# Print the number of jobs running
		set_color d70000
		echo -n " $color("(count $result)")$white"
		set_color fff
	end
end
