if [ $USER = 'root' ]
	set userStyle f00
else
	set userStyle de7123
end

if [ $SSH_TTY ]
	set hostStyle f00
else
	set hostStyle c49d1b
end

function fish_prompt
	set_color $userStyle -o
	echo -n (whoami)
	set_color fff
	echo -n ' at '
	set_color $hostStyle
	echo -n (hostname)
	set_color fff
	echo -n ' in '
	set_color 677a27
	prompt_pwd
	set_color fff
	prompt_git
	prompt_jobs
	echo ' '
	echo '$ '
end
