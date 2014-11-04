function c -d "Pipe input to clipboard (new lines are trimmed)"
	cat | tr -d '\n' | pbcopy
end
