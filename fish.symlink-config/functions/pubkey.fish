function pubkey -d 'Copy your public key to the pasteboard'
	cat $HOME/.ssh/id_rsa.pub | pbcopy
	echo 'Public key copied to pasteboard'
end
