function flush -d "Flush the DNS cache"
	if [ $OS = 'Darwin' ]
		dscacheutil -flushcache; and killall -HUP mDNSResponder
	else
		sudo service dns-clean restart
		sudo service networking force-reload
		sudo service nscd restart
		sudo service dnsmasq restart
	end
end
