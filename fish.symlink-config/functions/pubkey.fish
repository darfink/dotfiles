function pubkey -d 'Copy your public key to the pasteboard'
	set -l path "$HOME/.ssh/id_rsa.pub"

	if not test -e $path
		echo 'No public key exists'
	else
		cat $path | pbcopy
		echo 'Public key copied to pasteboard'
	end
end