# Disable fish greeting
set --erase fish_greeting

# For identifying operating system
set -u OS (uname -s)

# Configure fish git prompt
#set __fish_git_prompt_showdirtystate 'true'
#set __fish_git_prompt_showuntrackedfiles 'true'
#set __fish_git_prompt_color_branch yellow
#set __fish_git_prompt_color_dirtystate red
#set __fish_git_prompt_color_untrackedfiles red

set custom "$HOME/.config/fish/custom"

# Load up all exports and aliases
source "$custom/exports.fish"
source "$custom/aliases.fish"

if [ -e "$custom/local.fish" ]
	# Execute personal/local settings
	source "$custom/local.fish"
end

if [ -e "$HOME/.dir_colors" ]
	# Make the 'ls' command pretty (solarized dark theme by default)
	eval (dircolors -c "$HOME/.dir_colors" | sed 's/>&\/dev\/null$//')
end

if [ -e "$HOME/.autojump/etc/profile.d/autojump.fish" ]
	# Load autojump if it's available to the user
	source "$HOME/.autojump/etc/profile.d/autojump.fish"
end

# Disable flow control (SSH)
stty -ixon -ixoff

# Check if the generated_completions folder is older than ten days
set result (find $fish_complete_path[-1] -type d -mtime +10 ^&1 | tr -d '\n')

if [ -n "$result" ]
	# If that's the case, update completions
	fish_update_completions
end

# Completions for aliases
complete --command g --wraps git