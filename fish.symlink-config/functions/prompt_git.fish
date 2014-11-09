function prompt_git -d "Print the git status of a project"
  set branchName ''
  set s ''

  # Check if the current directory is in a Git repository.
  if git rev-parse --is-inside-work-tree > /dev/null ^&1
		# Check if the current directory is in .git before running git checks
    if [ (git rev-parse --is-inside-git-dir ^ /dev/null) = 'false' ]
			# Ensure the index is up to date
			git update-index --really-refresh -q > /dev/null ^&1

      # Check for uncommitted changes in the index
			if not git diff --quiet --ignore-submodules --cached
	set s "$s+"
			end

			# Check for unstaged changes
			if not git diff-files --quiet --ignore-submodules --
	set s "$s!"
			end

			set files (git ls-files --others --exclude-standard)

			# Check for untracked files.
			if [ (count $files) != 0 ]
	set s "$s?"
			end

			# Check for stashed files.
			if git rev-parse --verify refs/stash > /dev/null ^&1
	set s "$s\$"
			end
    end

		# Get the short symbolic ref
		# If HEAD isn’t a symbolic ref, get the short SHA for the latest commit
		# otherwise, just give up.
		set branchName (git symbolic-ref --quiet --short HEAD ^ /dev/null; or \
			git rev-parse --short HEAD ^ /dev/null; or \
			echo '(unknown)')

    if [ -n $s ]
      set s " [$s]"
    end

    echo -n " on "
    set_color 6c6b9c
    echo -n "$branchName"
    set_color 0f9ff2
    echo -n "$s"
    set_color fff
  end
end
